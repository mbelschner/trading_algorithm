# =============================================================================
# Walk-Forward Backtester — REALISTISCHE FASSUNG
#
# Baut auf backtest_runner_optimized.R auf (Chunk-Loading + RDS-Cache + Race-Fix)
# und ergaenzt drei Dinge, die ueberoptimistische Ergebnisse verhindern:
#
#   1) PER-INSTRUMENT-SPREADS  (SPREAD_BP)  statt globalem cost_bps = 1.0
#   2) NEXT-OPEN-EXECUTION      execution = "next_open"
#        Signal @ Close(t) -> Fill @ Open(t+1).  strat_ret[i] = pos[i-2]*(logO[i]-logO[i-1])
#        (Mode "close" bleibt bit-identisch zur bisherigen Fassung.)
#   3) ALL-YEARS-PROFITABLE     F7 in apply_filters: jedes (volle) Jahr > 0
#
# Erwartete CSV-Spalten: time, open, high, low, close, volume
# Strategie-Interface unveraendert: generate_signals(df, ...), PARAM_GRID, NAME
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
PRICE_DATA_DIR <- file.path(getwd(), "price_data", "ctrader_data")
OUTPUT_DIR     <- file.path(SCRIPT_DIR, "results")
CACHE_DIR      <- file.path(SCRIPT_DIR, "cache")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR,  showWarnings = FALSE, recursive = TRUE)

IS_SPLIT <- 0.70


# =============================================================================
# >>> PER-INSTRUMENT-SPREADS  (bp PRO SIDE) <<<
# PLATZHALTER — gegen echte Pepperstone/Capital.com-Screenshots kalibrieren!
# Round-Trip = 2x dieser Wert; ein Flip +1->-1 kostet volle 2 Sides.
# =============================================================================

#spreads für capital.com, Wochenende

SPREAD_BP <- c(
  USDJPY    = 2.79,
  AUDJPY    = 12.02,
  EURNZD    = 14.65,
  CN50      = 3.16,
  J225      = 0.70,
  OIL_BRENT = 1.25,
  DE40      = 0.30,
  GOLD      = 0.60,
  EURUSD    = 2.18,
  EU50      = 1.19,
  US500     = 1.00,
  SILVER    = 3.86,
  COPPER    = 1.17    # NEU
)

#Pepperstone

SPREAD_BP <- c(
  USDJPY    = 1.4,
  AUDJPY    = 5.7,
  EURNZD    = 2.93,
  OIL_BRENT = 2.34,
  DE40      = 0.18,
  GOLD      = 0.22,
  EURUSD    = 1.57,
  EURGBP    = 2.31,
  AUDUSD    = 1.71,
  EURJPY    = 2.4,
  USDCAD    = 0.95,
  US500     = 0.27,
  SILVER    = 4.01,
  COPPER    = 1.57    # NEU
)

# Instrument-Symbol aus Datei-Stem ableiten:
#   "OIL_BRENT_MINUTE_5" -> "OIL_BRENT"
#   "GOLD_DAY"           -> "GOLD"
#   "GOLD_HOUR_4"        -> "GOLD"
#   "GOLD_HOUR"          -> "GOLD"
#   "DXY_MINUTE_15"      -> "DXY"
instrument_symbol <- function(stem) {
  sub("_(MINUTE|HOUR|DAY|WEEK).*$", "", stem, ignore.case = FALSE)
}

# SPREAD_BP als Liste damit [[name]] bei fehlendem Key NULL liefert (kein Fehler)
SPREAD_BP <- as.list(SPREAD_BP)


# =============================================================================
# >>> HIER EINSTELLEN <<<
# =============================================================================
CONFIG <- list(
  instruments = NULL,    # NULL = alle .csv in price_data/ ansonsten schreiben wie: GOLD_MINUTE_5.csv
  strategies  = c("rsi_mean_reversion", "bollinger_zscore_mean_reversion"),
  workers     = NULL,
  
  # --- Kostenmodell ---
  use_spread_map = TRUE, # TRUE = SPREAD_BP pro Instrument; FALSE = globaler cost_bps
  cost_bps       = 1.0,  # Fallback, falls use_spread_map = FALSE
  spread_default = 5.0,  # bp/Side, falls Symbol nicht in SPREAD_BP
  
  # --- Ausfuehrung ---
  execution = "next_open",   # "next_open" (realistisch) oder "close" (alt)
  
  # --- All-Years-Filter ---
  use_all_years_filter = FALSE,  # FALSE = F7 wird ignoriert (immer TRUE), Breakdown trotzdem ausgegeben
  min_year_bars = 2000L,        # Jahre mit weniger Bars gelten als Teiljahr -> ignoriert
  
  use_cache    = TRUE,
  export_excel = TRUE,
  output       = NULL
)
# =============================================================================


# -----------------------------------------------------------------------------
# Strategy Loader / Grid
# -----------------------------------------------------------------------------
load_strategies <- function(only = NULL) {
  files <- list.files(STRATEGIES_DIR, pattern = "\\.R$", full.names = TRUE)
  files <- files[!grepl("^_", basename(files))]
  if (!is.null(only)) {
    stems <- tools::file_path_sans_ext(basename(files))
    files <- files[stems %in% only]
  }
  out <- list()
  for (f in files) {
    env <- new.env(parent = baseenv()); sys.source(f, envir = env)
    if (!exists("generate_signals", envir = env) || !exists("PARAM_GRID", envir = env)) {
      message(sprintf("[WARN] %s ohne gueltiges Interface - uebersprungen.", basename(f))); next
    }
    name <- if (exists("NAME", envir = env)) env$NAME else tools::file_path_sans_ext(basename(f))
    out[[name]] <- list(name = name, path = f, param_grid = env$PARAM_GRID)
  }
  out
}

expand_grid_list <- function(grid) {
  if (length(grid) == 0) return(list(list()))
  df <- do.call(expand.grid, c(grid, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}


# -----------------------------------------------------------------------------
# Data Loading (+ atomischer RDS-Cache)
# -----------------------------------------------------------------------------
load_price_data <- function(filepath) {
  dt <- data.table::fread(filepath)
  setnames(dt, old = c("time","open","high","low","close","volume"),
           new = c("Timestamp","Open","High","Low","Close","Volume"), skip_absent = TRUE)
  dt[, Timestamp := as.POSIXct(Timestamp, tz = "UTC")]
  setorder(dt, Timestamp)
  unique(dt, by = "Timestamp", fromLast = TRUE)
}

load_price_data_cached <- function(filepath, use_cache = TRUE) {
  if (!use_cache) return(load_price_data(filepath))
  cache <- file.path(CACHE_DIR, paste0(tools::file_path_sans_ext(basename(filepath)), ".rds"))
  if (file.exists(cache) && file.mtime(cache) >= file.mtime(filepath)) return(readRDS(cache))
  dt  <- load_price_data(filepath)
  tmp <- paste0(cache, ".tmp.", Sys.getpid())
  saveRDS(dt, tmp, compress = FALSE)
  file.rename(tmp, cache)               # atomarer Swap -> keine halben Reads
  dt
}

infer_bars_per_year <- function(df) {
  if (nrow(df) < 2) return(252L)
  deltas <- as.numeric(diff(df$Timestamp), units = "mins")
  minutes <- max(median(deltas, na.rm = TRUE), 0.1)
  if (minutes >= 60 * 20) return(252L)
  as.integer(((60 * 24) / minutes) * 252)
}


# -----------------------------------------------------------------------------
# Return-Stream je nach Execution-Mode (eine zentrale Stelle)
#   close:     pos_eff = shift(pos,1), rets = diff(log Close)   [= alte Fassung]
#   next_open: pos_eff = shift(pos,2), rets = diff(log Open)    [realistisch]
# Kosten: abs(diff(pos)) * cost_bps/10000  (Total korrekt; Flip = 2 Sides)
# -----------------------------------------------------------------------------
.strat_returns <- function(df, cost_bps, execution) {
  pos <- ifelse(is.na(df$Position), 0, df$Position)
  if (execution == "next_open") {
    rets    <- c(NA_real_, diff(log(df$Open)))
    pos_eff <- data.table::shift(pos, 2L, fill = 0)
  } else {
    rets    <- c(NA_real_, diff(log(df$Close)))
    pos_eff <- data.table::shift(pos, 1L, fill = 0)
  }
  strat_ret  <- pos_eff * rets
  pos_change <- abs(c(NA_real_, diff(pos)))
  cost       <- ifelse(is.na(pos_change), 0, pos_change) * (cost_bps / 10000)
  list(strat_ret = strat_ret - cost, pos_eff = pos_eff)
}


# -----------------------------------------------------------------------------
# Metriken
# -----------------------------------------------------------------------------
compute_metrics <- function(df, bars_per_year, cost_bps = 0, execution = "next_open") {
  empty <- list(sharpe = 0, sortino = 0, calmar = 0, max_dd = 0,
                num_trades = 0L, win_rate = 0, profit_factor = 0,
                total_return = 0, cagr = 0, exposure = 0)
  if (!"Position" %in% names(df) || nrow(df) == 0) return(empty)
  
  sr <- .strat_returns(df, cost_bps, execution)
  strat_ret <- sr$strat_ret; pos_eff <- sr$pos_eff
  
  ok <- !is.na(strat_ret); strat_ret <- strat_ret[ok]; pos_eff_a <- pos_eff[ok]
  if (length(strat_ret) == 0) return(empty)
  
  mean_r <- mean(strat_ret); sd_r <- sd(strat_ret)
  
  # --- Sharpe (deskriptiv, bleibt in Tabelle) ---
  sharpe <- if (sd_r > 0) (mean_r / sd_r) * sqrt(bars_per_year) else 0
  
  # --- Sortino: nur Downside-Deviation im Nenner ---
  # Downside relativ zu 0 (kein Mindest-Return-Target). Quadratischer Mittelwert
  # der negativen Returns; Upside-Vol wird NICHT bestraft.
  downside <- strat_ret[strat_ret < 0]
  dd_dev <- if (length(downside) > 0) sqrt(mean(downside^2)) else 0
  sortino <- if (dd_dev > 0) (mean_r / dd_dev) * sqrt(bars_per_year) else 0
  
  # --- Equity / Drawdown ---
  cum <- exp(cumsum(strat_ret)); cummax_ <- cummax(cum)
  max_dd <- min((cum - cummax_) / cummax_)
  total_return <- exp(sum(strat_ret)) - 1
  
  # --- CAGR (annualisiert) + Calmar = CAGR / |MaxDD| ---
  n_bars <- length(strat_ret)
  years  <- n_bars / bars_per_year
  cagr   <- if (years > 0) (1 + total_return)^(1 / years) - 1 else 0
  calmar <- if (max_dd < 0) cagr / abs(max_dd) else if (cagr > 0) Inf else 0
  
  # --- Trades / Win-Rate / Profit Factor ---
  trade_id   <- cumsum(c(TRUE, diff(pos_eff_a) != 0))
  trade_rets <- tapply(strat_ret, trade_id, sum)
  trade_pos  <- tapply(pos_eff_a, trade_id, function(x) x[1])
  active     <- trade_rets[trade_pos != 0]
  
  num_trades <- length(active)
  if (num_trades > 0) {
    win_rate <- sum(active > 0) / num_trades
    gp <- sum(active[active > 0]); gl <- abs(sum(active[active < 0]))
    profit_factor <- if (gl > 0) gp / gl else Inf
  } else { win_rate <- 0; profit_factor <- 0 }
  
  list(sharpe = sharpe, sortino = sortino, calmar = calmar, max_dd = max_dd,
       num_trades = as.integer(num_trades), win_rate = win_rate,
       profit_factor = profit_factor, total_return = total_return,
       cagr = cagr, exposure = mean(pos_eff_a != 0))
}


# Jahres-Returns auf dem GESAMT-Sample (fuer All-Years-Filter)
compute_yearly <- function(df, cost_bps, execution) {
  sr <- .strat_returns(df, cost_bps, execution)
  yr <- as.integer(format(df$Timestamp, "%Y"))
  ok <- !is.na(sr$strat_ret)
  tab <- data.table(year = yr[ok], r = sr$strat_ret[ok])
  tab[, .(ret = exp(sum(r)) - 1, bars = .N), by = year][order(year)]
}


# -----------------------------------------------------------------------------
# Filter (F7 = All-Years-Profitable, optional via use_f7)
# -----------------------------------------------------------------------------
apply_filters <- function(is_m, os_m, years_all_positive, use_f7 = TRUE) {
  os_pf_ok <- is.finite(os_m$profit_factor) && os_m$profit_factor > 1.2 ||
    is.infinite(os_m$profit_factor)
  is_pf_ok <- is.finite(is_m$profit_factor) && is_m$profit_factor > 1.0 ||
    is.infinite(is_m$profit_factor)
  calmar_ok <- is.finite(os_m$calmar) && os_m$calmar > 0.5 ||
    is.infinite(os_m$calmar)
  
  f <- list(
    F1_OS_MaxDD_above_neg15 = os_m$max_dd  > -0.15,
    F2_OS_PF_gt_1.2         = os_pf_ok,
    F3_OS_Sortino_gt_1.0    = os_m$sortino > 1.0,
    F4_OS_Calmar_gt_0.5     = calmar_ok,
    F5_IS_PF_gt_1.0         = is_pf_ok,
    F6_OS_WinRate_band      = os_m$win_rate > 0.45 && os_m$win_rate < 0.85,
    F7_AllYearsProfitable   = if (use_f7) isTRUE(years_all_positive) else TRUE
  )
  f$PASSED_ALL <- all(unlist(f))
  f
}



# -----------------------------------------------------------------------------
# Row-Builder
# -----------------------------------------------------------------------------
build_result_row <- function(strategy_name, instrument, symbol, params, bpy,
                             n_is, n_os, is_m, os_m, filt,
                             cost_bps, execution, yearly) {
  params_str <- paste(names(params), sapply(params, as.character), sep = "=", collapse = ", ")
  param_cols <- setNames(lapply(params, function(x) x), paste0("Param_", names(params)))
  
  brk        <- paste(sprintf("%d:%+.1f%%", yearly$year, yearly$ret * 100), collapse = " ")
  worst_year <- if (nrow(yearly) > 0) round(min(yearly$ret), 4) else NA_real_
  
  c(list(Strategy = strategy_name, Instrument = instrument, Symbol = symbol, Params = params_str),
    param_cols,
    list(
      Cost_bps_side   = cost_bps,
      Execution       = execution,
      Bars_per_Year   = bpy, Bars_IS = n_is, Bars_OS = n_os,
      # --- IS (deskriptiv) ---
      IS_Sharpe       = round(is_m$sharpe, 3),
      IS_Sortino      = round(is_m$sortino, 3),
      IS_ProfitFactor = if (is.finite(is_m$profit_factor)) round(is_m$profit_factor, 3) else NA_real_,
      IS_Trades       = is_m$num_trades,
      # --- OS (Filter-relevant + deskriptiv) ---
      OS_Sharpe       = round(os_m$sharpe, 3),          # nur noch deskriptiv
      OS_Sortino      = round(os_m$sortino, 3),         # Filter F3
      OS_Calmar       = if (is.finite(os_m$calmar)) round(os_m$calmar, 3) else NA_real_,  # Filter F4
      OS_CAGR         = round(os_m$cagr, 4),
      OS_MaxDD        = round(os_m$max_dd, 4),          # Filter F1
      OS_TotalReturn  = round(os_m$total_return, 4),
      OS_WinRate      = round(os_m$win_rate, 4),        # Filter F6
      OS_ProfitFactor = if (is.finite(os_m$profit_factor)) round(os_m$profit_factor, 3) else NA_real_,  # Filter F2
      OS_Trades       = os_m$num_trades,
      OS_Exposure     = round(os_m$exposure, 3),
      Worst_Year_Ret  = worst_year,
      Yearly_Breakdown= brk
    ),
    filt,
    list(Error = NA_character_))
}


error_result_row <- function(strategy_name, instrument, params, e) {
  list(Strategy = strategy_name, Instrument = instrument,
       Params = paste(names(params), sapply(params, as.character), sep = "=", collapse = ", "),
       PASSED_ALL = FALSE, Error = paste0(class(e)[1], ": ", conditionMessage(e)))
}


# -----------------------------------------------------------------------------
# Worker: ganzer (Strategie x Instrument)-Chunk
# -----------------------------------------------------------------------------
run_chunk <- function(chunk) {
  env <- new.env(parent = globalenv()); sys.source(chunk$strategy_path, envir = env)
  
  df  <- load_price_data_cached(chunk$data_path, use_cache = chunk$use_cache)
  bpy <- infer_bars_per_year(df)
  split_idx <- floor(nrow(df) * IS_SPLIT)
  is_df <- df[1:split_idx]; os_df <- df[(split_idx + 1):nrow(df)]
  n_is <- nrow(is_df); n_os <- nrow(os_df)
  
  lapply(chunk$param_combos, function(params) {
    tryCatch({
      sig_full <- do.call(env$generate_signals, c(list(df = df),    params))
      sig_is   <- do.call(env$generate_signals, c(list(df = is_df), params))
      sig_os   <- do.call(env$generate_signals, c(list(df = os_df), params))
      
      is_m <- compute_metrics(sig_is, bpy, chunk$cost_bps, chunk$execution)
      os_m <- compute_metrics(sig_os, bpy, chunk$cost_bps, chunk$execution)
      
      yearly  <- compute_yearly(sig_full, chunk$cost_bps, chunk$execution)
      counted <- yearly[bars >= chunk$min_year_bars]
      years_ok <- nrow(counted) > 0 && all(counted$ret > 0)
      
      filt <- apply_filters(is_m, os_m, years_ok, use_f7 = chunk$use_all_years_filter)
      build_result_row(chunk$strategy_name, chunk$instrument, chunk$symbol, params,
                       bpy, n_is, n_os, is_m, os_m, filt,
                       chunk$cost_bps, chunk$execution, yearly)
    }, error = function(e) error_result_row(chunk$strategy_name, chunk$instrument, params, e))
  })
}


# -----------------------------------------------------------------------------
# Aggregation + Excel
# -----------------------------------------------------------------------------
results_to_dt <- function(results) {
  all_cols <- unique(unlist(lapply(results, names)))
  rows <- lapply(results, function(r) {
    for (m in setdiff(all_cols, names(r))) r[[m]] <- NA
    r[all_cols]
  })
  rbindlist(lapply(rows, as.data.table), fill = TRUE, use.names = TRUE)
}

write_excel <- function(results_dt, output_path) {
  passed <- results_dt[PASSED_ALL == TRUE]
  if ("OS_Sharpe" %in% names(passed) && nrow(passed) > 0) setorder(passed, -OS_Sharpe)
  
  summary_dt <- results_dt[, .(
    Runs           = .N,
    Passed         = sum(PASSED_ALL == TRUE, na.rm = TRUE),
    Passed_F7      = sum(F7_AllYearsProfitable == TRUE, na.rm = TRUE),
    Best_OS_Sharpe = suppressWarnings(max(OS_Sharpe, na.rm = TRUE)),
    Median_OS_PF   = median(OS_ProfitFactor, na.rm = TRUE)
  ), by = .(Strategy, Instrument)]
  summary_dt[, Pass_Rate := round(Passed / Runs, 3)]
  
  wb <- createWorkbook()
  addWorksheet(wb, "All_Runs"); addWorksheet(wb, "Passed_Filter"); addWorksheet(wb, "Summary")
  hdr <- createStyle(textDecoration = "bold")
  writeData(wb, "All_Runs", results_dt, headerStyle = hdr)
  writeData(wb, "Passed_Filter", passed, headerStyle = hdr)
  writeData(wb, "Summary", summary_dt, headerStyle = hdr)
  
  if ("PASSED_ALL" %in% names(results_dt)) {
    ci <- which(names(results_dt) == "PASSED_ALL")
    conditionalFormatting(wb, "All_Runs", cols = ci, rows = 2:(nrow(results_dt) + 1),
                          rule = "==TRUE",  style = createStyle(bgFill = "#C6EFCE"))
    conditionalFormatting(wb, "All_Runs", cols = ci, rows = 2:(nrow(results_dt) + 1),
                          rule = "==FALSE", style = createStyle(bgFill = "#FFC7CE"))
  }
  setColWidths(wb, "All_Runs", cols = seq_along(results_dt), widths = "auto")
  setColWidths(wb, "Passed_Filter", cols = seq_along(results_dt), widths = "auto")
  setColWidths(wb, "Summary", cols = seq_along(summary_dt), widths = "auto")
  saveWorkbook(wb, output_path, overwrite = TRUE)
}


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
main <- function(cfg = CONFIG) {
  workers <- if (is.null(cfg$workers)) max(1L, parallel::detectCores() - 1L) else cfg$workers
  
  strategies <- load_strategies(cfg$strategies)
  if (length(strategies) == 0) { message("Keine Strategien gefunden."); return(invisible()) }
  message("Geladene Strategien: ", paste(names(strategies), collapse = ", "))
  
  files <- if (!is.null(cfg$instruments)) file.path(PRICE_DATA_DIR, cfg$instruments)
  else list.files(PRICE_DATA_DIR, pattern = "\\.csv$", full.names = TRUE)
  files <- files[file.exists(files)]
  if (length(files) == 0) { message("Keine CSVs in ", PRICE_DATA_DIR); return(invisible()) }
  message("Instrumente: ", paste(basename(files), collapse = ", "))
  message(sprintf("Execution: %s  |  Spread-Map: %s", cfg$execution,
                  if (isTRUE(cfg$use_spread_map)) "AN" else paste0("AUS (", cfg$cost_bps, " bp)")))
  
  # Cache vorwaermen (verhindert konkurrierende Schreibzugriffe der Worker)
  if (isTRUE(cfg$use_cache)) {
    for (f in unique(files)) load_price_data_cached(f, use_cache = TRUE)
    message("Cache vorgewaermt: ", length(unique(files)), " Instrument(e)")
  }
  
  # Chunks bauen — Kosten pro Instrument aufloesen
  chunks <- list()
  for (s in strategies) {
    grid_combos <- expand_grid_list(s$param_grid)
    for (f in files) {
      stem <- tools::file_path_sans_ext(basename(f))
      sym  <- instrument_symbol(stem)
      cb <- if (isTRUE(cfg$use_spread_map)) {
        v <- SPREAD_BP[[sym]]
        if (is.null(v)) { message(sprintf("[WARN] kein Spread fuer %s -> default %.1f bp", sym, cfg$spread_default)); cfg$spread_default }
        else v
      } else cfg$cost_bps
      chunks[[length(chunks) + 1]] <- list(
        strategy_name = s$name, strategy_path = s$path,
        instrument = stem, symbol = sym, data_path = f,
        param_combos = grid_combos,
        cost_bps = cb, execution = cfg$execution,
        min_year_bars = cfg$min_year_bars, use_cache = isTRUE(cfg$use_cache),
        use_all_years_filter = isTRUE(cfg$use_all_years_filter)
      )
    }
  }
  n_runs <- sum(vapply(chunks, function(c) length(c$param_combos), integer(1)))
  message(sprintf("Chunks: %d  |  Runs: %d  |  Worker: %d", length(chunks), n_runs, workers))
  
  plan(multisession, workers = workers); on.exit(plan(sequential), add = TRUE)
  
  t0 <- Sys.time()
  chunk_results <- future_map(chunks, run_chunk, .progress = TRUE,
                              .options = furrr_options(seed = TRUE))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  results_dt <- results_to_dt(unlist(chunk_results, recursive = FALSE))
  
  if (isTRUE(cfg$export_excel)) {
    out <- if (!is.null(cfg$output)) cfg$output else
      file.path(OUTPUT_DIR, sprintf("backtest_realistic_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")))
    write_excel(results_dt, out); message(sprintf("[OK] Excel: %s", out))
  }
  
  passed_n <- sum(results_dt$PASSED_ALL == TRUE, na.rm = TRUE)
  message(sprintf("Passed (alle F1-F7): %d/%d  |  Laufzeit: %.1fs", passed_n, nrow(results_dt), elapsed))
  results_dt
}

if (isTRUE(getOption("BTR_AUTORUN", FALSE))) { backtest_results <- main() }