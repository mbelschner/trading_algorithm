# =============================================================================
# Walk-Forward Backtester mit Plugin-Strategien, Parallel Processing (furrr)
# und Excel-Output (openxlsx).
#
# Verwendung:
#   1) CONFIG-Block unten anpassen
#   2) Script starten (z.B. source() in RStudio oder Rscript backtest_runner.R)
#
# Erwartete CSV-Spalten in price_data/: time, open, high, low, close, volume
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(furrr)
  library(openxlsx)
  library(TTR)
})

# -----------------------------------------------------------------------------
# Pfade
# -----------------------------------------------------------------------------

SCRIPT_DIR     <- file.path(getwd(), "r", "multi_backtest_runner")
STRATEGIES_DIR <- file.path(SCRIPT_DIR, "strategies")
PRICE_DATA_DIR <- file.path(getwd(), "price_data")
OUTPUT_DIR     <- file.path(SCRIPT_DIR, "results")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

IS_SPLIT <- 0.70


# =============================================================================
# >>> HIER EINSTELLEN <<<  (NULL = "alle nehmen")
# =============================================================================
CONFIG <- list(
  # Vektor von CSV-Dateinamen (mit .csv) aus price_data/.
  # NULL = alle .csv im Ordner verwenden.
  # Beispiel: c("GOLD_MINUTE_5.csv", "US500_MINUTE_5.csv", "DE40_MINUTE_15.csv")
  instruments = c("GOLD_MINUTE_5.csv"),
  
  # Vektor von Strategie-Dateinamen (ohne .R) aus strategies/.
  # NULL = alle Strategien laden.
  # Beispiel: c("rsi_mean_reversion", "donchian_breakout")
  strategies = c("ichimoku_triple", "cmo_vhf_stc", "tokyo_gap_range", "gap_fade", "rsi_mean_reversion"),
  
  # Anzahl Worker. NULL = parallel::detectCores() - 1.
  workers = NULL,
  
  # Transaktionskosten pro Side in Basispunkten (Round-Trip = 2x).
  # Capital.com grob: Gold ~3-5bp, US500 ~2-3bp, Brent ~5-8bp.
  cost_bps = 1.0,
  
  # Output-Pfad fuer Excel. NULL = automatisch in results/ mit Timestamp.
  output = NULL
)
# =============================================================================


# -----------------------------------------------------------------------------
# Strategy Loader
# -----------------------------------------------------------------------------
# Jede .R-Datei wird in ein eigenes Environment gesourct, damit Variablen-
# namen sich nicht ueberschreiben. Wir packen NAME / PARAM_GRID / die Funktion
# zusammen mit dem Pfad in eine Liste.
load_strategies <- function(only = NULL) {
  files <- list.files(STRATEGIES_DIR, pattern = "\\.R$", full.names = TRUE)
  files <- files[!grepl("^_", basename(files))]
  
  if (!is.null(only)) {
    stems <- tools::file_path_sans_ext(basename(files))
    files <- files[stems %in% only]
  }
  
  out <- list()
  for (f in files) {
    env <- new.env(parent = baseenv())
    sys.source(f, envir = env)
    if (!exists("generate_signals", envir = env) ||
        !exists("PARAM_GRID",      envir = env)) {
      message(sprintf("[WARN] %s hat kein gueltiges Interface - uebersprungen.",
                      basename(f)))
      next
    }
    name <- if (exists("NAME", envir = env)) env$NAME else tools::file_path_sans_ext(basename(f))
    out[[name]] <- list(
      name = name,
      path = f,
      param_grid = env$PARAM_GRID
    )
  }
  out
}

# Cartesian product aus param_grid -> list of named lists
expand_grid_list <- function(grid) {
  if (length(grid) == 0) return(list(list()))
  df <- do.call(expand.grid, c(grid, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}


# -----------------------------------------------------------------------------
# Data Loading
# -----------------------------------------------------------------------------
load_price_data <- function(filepath) {
  dt <- data.table::fread(filepath)
  setnames(dt, old = c("time", "open", "high", "low", "close", "volume"),
           new = c("Timestamp", "Open", "High", "Low", "Close", "Volume"),
           skip_absent = TRUE)
  dt[, Timestamp := as.POSIXct(Timestamp, tz = "UTC")]
  setorder(dt, Timestamp)
  dt <- unique(dt, by = "Timestamp", fromLast = TRUE)
  dt
}

infer_bars_per_year <- function(df) {
  if (nrow(df) < 2) return(252L)
  deltas <- as.numeric(diff(df$Timestamp), units = "mins")
  minutes <- max(median(deltas, na.rm = TRUE), 0.1)
  if (minutes >= 60 * 20) return(252L)  # daily oder groesser
  bars_per_day <- (60 * 24) / minutes    # 24h Markt
  as.integer(bars_per_day * 252)
}


# -----------------------------------------------------------------------------
# Metrik-Berechnung
# -----------------------------------------------------------------------------
# cost_bps: Kosten pro Side in Basispunkten. Werden bei jeder Position-
# Aenderung abgezogen.
compute_metrics <- function(df, bars_per_year, cost_bps = 0) {
  empty <- list(sharpe = 0, max_dd = 0, num_trades = 0L,
                win_rate = 0, profit_factor = 0, total_return = 0,
                exposure = 0)
  if (!"Position" %in% names(df) || nrow(df) == 0) return(empty)
  
  pos <- ifelse(is.na(df$Position), 0, df$Position)
  market_ret <- c(NA_real_, diff(log(df$Close)))
  
  # Position der vorigen Bar bestimmt Return der aktuellen (kein Look-ahead)
  pos_lag <- data.table::shift(pos, 1, fill = 0)
  strat_ret <- pos_lag * market_ret
  
  # Transaktionskosten bei Positionswechsel
  pos_change <- abs(c(NA_real_, diff(pos)))
  cost <- ifelse(is.na(pos_change), 0, pos_change) * (cost_bps / 10000)
  strat_ret <- strat_ret - cost
  
  ok <- !is.na(strat_ret)
  strat_ret <- strat_ret[ok]
  pos_lag_a <- pos_lag[ok]
  
  if (length(strat_ret) == 0) return(empty)
  
  mean_r <- mean(strat_ret)
  sd_r   <- sd(strat_ret)
  sharpe <- if (sd_r > 0) (mean_r / sd_r) * sqrt(bars_per_year) else 0
  
  cum     <- exp(cumsum(strat_ret))
  cummax_ <- cummax(cum)
  max_dd  <- min((cum - cummax_) / cummax_)
  total_return <- exp(sum(strat_ret)) - 1
  
  # Trade-Ebene: Gruppen wo pos_lag konstant bleibt
  trade_id <- cumsum(c(TRUE, diff(pos_lag_a) != 0))
  trade_rets <- tapply(strat_ret, trade_id, sum)
  trade_pos  <- tapply(pos_lag_a, trade_id, function(x) x[1])
  active <- trade_rets[trade_pos != 0]
  
  num_trades <- length(active)
  if (num_trades > 0) {
    win_rate <- sum(active > 0) / num_trades
    gp <- sum(active[active > 0])
    gl <- abs(sum(active[active < 0]))
    profit_factor <- if (gl > 0) gp / gl else Inf
  } else {
    win_rate <- 0; profit_factor <- 0
  }
  
  exposure <- mean(pos_lag_a != 0)
  
  list(sharpe = sharpe, max_dd = max_dd, num_trades = as.integer(num_trades),
       win_rate = win_rate, profit_factor = profit_factor,
       total_return = total_return, exposure = exposure)
}

apply_filters <- function(is_m, os_m) {
  f <- list(
    F1_OS_DD_above_neg35pct  = os_m$max_dd     > -0.35,
    F2_OS_Sharpe_gt_0.5      = os_m$sharpe     >  0.5,
    F3_OS_Sharpe_lt_2.5      = os_m$sharpe     <  2.5,
    F4_OS_le_IS_x1.3         = os_m$sharpe     <= is_m$sharpe * 1.30,
    F5_OS_Trades_ge_30       = os_m$num_trades >= 30L,
    F6_IS_Sharpe_positive    = is_m$sharpe     >  0
  )
  f$PASSED_ALL <- all(unlist(f))
  f
}


# -----------------------------------------------------------------------------
# Worker - laeuft im Subprozess via furrr
# -----------------------------------------------------------------------------
# Strategy-Modul wird im Worker neu gesourct, weil Funktions-Closures aus
# einem ad-hoc Environment nicht zuverlaessig ueber Prozessgrenzen serialisiert
# werden (gleiches Pattern wie in der Python-Variante).
run_single_backtest <- function(task) {
  res <- tryCatch({
    env <- new.env(parent = globalenv())
    sys.source(task$strategy_path, envir = env)
    
    df <- load_price_data(task$data_path)
    bpy <- infer_bars_per_year(df)
    
    split_idx <- floor(nrow(df) * IS_SPLIT)
    is_df <- df[1:split_idx]
    os_df <- df[(split_idx + 1):nrow(df)]
    
    is_sig <- do.call(env$generate_signals, c(list(df = is_df), task$params))
    os_sig <- do.call(env$generate_signals, c(list(df = os_df), task$params))
    
    is_m <- compute_metrics(is_sig, bpy, cost_bps = task$cost_bps)
    os_m <- compute_metrics(os_sig, bpy, cost_bps = task$cost_bps)
    filt <- apply_filters(is_m, os_m)
    
    # Flat row fuer Excel
    params_str <- paste(names(task$params),
                        sapply(task$params, function(x) as.character(x)),
                        sep = "=", collapse = ", ")
    param_cols <- setNames(
      lapply(task$params, function(x) x),
      paste0("Param_", names(task$params))
    )
    
    c(list(
      Strategy   = task$strategy_name,
      Instrument = task$instrument,
      Params     = params_str
    ),
    param_cols,
    list(
      Bars_per_Year   = bpy,
      Bars_IS         = nrow(is_df),
      Bars_OS         = nrow(os_df),
      IS_Sharpe       = round(is_m$sharpe, 3),
      IS_MaxDD        = round(is_m$max_dd, 4),
      IS_TotalReturn  = round(is_m$total_return, 4),
      IS_WinRate      = round(is_m$win_rate, 4),
      IS_ProfitFactor = if (is.finite(is_m$profit_factor)) round(is_m$profit_factor, 3) else NA_real_,
      IS_Trades       = is_m$num_trades,
      IS_Exposure     = round(is_m$exposure, 3),
      OS_Sharpe       = round(os_m$sharpe, 3),
      OS_MaxDD        = round(os_m$max_dd, 4),
      OS_TotalReturn  = round(os_m$total_return, 4),
      OS_WinRate      = round(os_m$win_rate, 4),
      OS_ProfitFactor = if (is.finite(os_m$profit_factor)) round(os_m$profit_factor, 3) else NA_real_,
      OS_Trades       = os_m$num_trades,
      OS_Exposure     = round(os_m$exposure, 3)
    ),
    filt,
    list(Error = NA_character_))
  }, error = function(e) {
    list(
      Strategy   = task$strategy_name,
      Instrument = task$instrument,
      Params     = paste(names(task$params),
                         sapply(task$params, as.character),
                         sep = "=", collapse = ", "),
      PASSED_ALL = FALSE,
      Error      = paste0(class(e)[1], ": ", conditionMessage(e))
    )
  })
  res
}



# Robuster rbind: Tasks koennen unterschiedliche Param-Spalten haben
results_to_dt <- function(results) {
  all_cols <- unique(unlist(lapply(results, names)))
  rows <- lapply(results, function(r) {
    missing <- setdiff(all_cols, names(r))
    for (m in missing) r[[m]] <- NA
    r[all_cols]
  })
  rbindlist(lapply(rows, as.data.table), fill = TRUE, use.names = TRUE)
}

write_excel <- function(results_dt, output_path) {
  passed <- results_dt[PASSED_ALL == TRUE]
  if ("OS_Sharpe" %in% names(passed) && nrow(passed) > 0) {
    setorder(passed, -OS_Sharpe)
  }
  
  summary_dt <- results_dt[, .(
    Runs             = .N,
    Passed           = sum(PASSED_ALL == TRUE, na.rm = TRUE),
    Best_OS_Sharpe   = suppressWarnings(max(OS_Sharpe, na.rm = TRUE)),
    Median_OS_Sharpe = median(OS_Sharpe, na.rm = TRUE),
    Median_IS_Sharpe = median(IS_Sharpe, na.rm = TRUE)
  ), by = .(Strategy, Instrument)]
  summary_dt[, Pass_Rate := round(Passed / Runs, 3)]
  
  wb <- createWorkbook()
  addWorksheet(wb, "All_Runs")
  addWorksheet(wb, "Passed_Filter")
  addWorksheet(wb, "Summary")
  
  writeData(wb, "All_Runs",      results_dt, headerStyle = createStyle(textDecoration = "bold"))
  writeData(wb, "Passed_Filter", passed,     headerStyle = createStyle(textDecoration = "bold"))
  writeData(wb, "Summary",       summary_dt, headerStyle = createStyle(textDecoration = "bold"))
  
  # PASSED_ALL einfaerben in All_Runs
  if ("PASSED_ALL" %in% names(results_dt)) {
    col_idx <- which(names(results_dt) == "PASSED_ALL")
    green <- createStyle(bgFill = "#C6EFCE")
    red   <- createStyle(bgFill = "#FFC7CE")
    conditionalFormatting(wb, "All_Runs", cols = col_idx,
                          rows = 2:(nrow(results_dt) + 1),
                          rule = "==TRUE",  style = green)
    conditionalFormatting(wb, "All_Runs", cols = col_idx,
                          rows = 2:(nrow(results_dt) + 1),
                          rule = "==FALSE", style = red)
  }
  
  setColWidths(wb, "All_Runs",      cols = seq_along(results_dt), widths = "auto")
  setColWidths(wb, "Passed_Filter", cols = seq_along(results_dt), widths = "auto")
  setColWidths(wb, "Summary",       cols = seq_along(summary_dt), widths = "auto")
  
  saveWorkbook(wb, output_path, overwrite = TRUE)
}


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
main <- function(cfg = CONFIG) {
  workers <- if (is.null(cfg$workers)) max(1L, parallel::detectCores() - 1L) else cfg$workers
  
  # Strategien laden
  strategies <- load_strategies(cfg$strategies)
  if (length(strategies) == 0) { message("Keine Strategien gefunden."); return(invisible()) }
  message("Geladene Strategien: ", paste(names(strategies), collapse = ", "))
  
  # Instrumente
  if (!is.null(cfg$instruments)) {
    files <- file.path(PRICE_DATA_DIR, cfg$instruments)
  } else {
    files <- list.files(PRICE_DATA_DIR, pattern = "\\.csv$", full.names = TRUE)
  }
  files <- files[file.exists(files)]
  if (length(files) == 0) {
    message("Keine CSV-Dateien gefunden in ", PRICE_DATA_DIR); return(invisible())
  }
  message("Instrumente: ", paste(basename(files), collapse = ", "))
  
  # Task-Liste aufbauen
  tasks <- list()
  for (s in strategies) {
    grid_combos <- expand_grid_list(s$param_grid)
    for (params in grid_combos) {
      for (f in files) {
        tasks[[length(tasks) + 1]] <- list(
          strategy_name = s$name,
          strategy_path = s$path,
          instrument    = tools::file_path_sans_ext(basename(f)),
          data_path     = f,
          params        = params,
          cost_bps      = cfg$cost_bps
        )
      }
    }
  }
  
  message(sprintf("Gesamt Tasks: %d  |  Worker: %d  |  Kosten: %.2f bp/side",
                  length(tasks), workers, cfg$cost_bps))
  
  # Parallel ausfuehren
  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)
  
  t0 <- Sys.time()
  results <- future_map(tasks, run_single_backtest,
                        .progress = TRUE,
                        .options = furrr_options(seed = TRUE))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  results_dt <- results_to_dt(results)
  
  # --- Excel Export (optional) ---
  if (isTRUE(cfg$export_excel)) {
    out <- if (!is.null(cfg$output)) cfg$output else
      file.path(OUTPUT_DIR, sprintf("backtest_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")))
    write_excel(results_dt, out)
    message(sprintf("[OK] Excel geschrieben: %s", out))
  }
  
  passed_n <- sum(results_dt$PASSED_ALL == TRUE, na.rm = TRUE)
  message(sprintf("Passed Filter: %d/%d  |  Laufzeit: %.1fs",
                  passed_n, nrow(results_dt), elapsed))
  
  results_dt   # sichtbar zurueckgeben -> landet in backtest_results
}


# Auto-Run: Ergebnis als 'backtest_results' im Global Environment speichern,
# damit es nach source() direkt in RStudio verfuegbar ist.
if (!interactive() ||
    identical(environmentName(topenv(parent.frame())), "R_GlobalEnv")) {
  backtest_results <- main()
}

