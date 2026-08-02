# =============================================================================
# main_pipeline.R  —  Steuer-Script
# -----------------------------------------------------------------------------
# Ein Ort fuer den ganzen Ablauf:
#   1) run_backtest()        -> Backtest-Tabelle als data.table (KEIN Excel)
#   2) show_table()          -> kompakte Konsolen-Ansicht
#   3) prepare_survivors()   -> Survivors + Params als Liste rekonstruiert
#   4) run_validation()      -> MCPT + DSR pro Survivor (bpy korrekt pro Instrument!)
#   5) check_survivor_trades()-> Trade-Liste (full) + Konsistenz-Check vs .strat_returns
#
# VORAUSSETZUNG — EINMALIGE MINI-AENDERUNG im Runner:
#   Ersetze ganz unten in backtest_runner_realistic.R den Auto-Run-Block durch:
#     if (isTRUE(getOption("BTR_AUTORUN", FALSE))) { backtest_results <- main() }
#   -> dann startet das Sourcen KEINEN Backtest mehr; dieses Script steuert alles.
#
# Benoetigt Paket 'moments' (fuer DSR):  install.packages("moments")
# =============================================================================

# -----------------------------------------------------------------------------
# 0) Quellen laden  (Pfade ggf. anpassen)
# -----------------------------------------------------------------------------
options(BTR_AUTORUN = FALSE)   # Sicherheitsnetz, falls Edit im Runner vergessen wurde

.here <- function(...) file.path(getwd(), ...)

source(.here("r", "multi_backtest_runner", "backtest_runner_realistic.R"))
source(.here("r", "multi_backtest_runner", "mcpt_dsr_module.R"))
source(.here("r", "multi_backtest_runner", "trade_export_glue.R"))


# =============================================================================
# >>> FILTER-AUSWAHL (Survivor-Definition) — HIER STEUERN <<<
# -----------------------------------------------------------------------------
# ACTIVE_FILTERS bestimmt, WELCHE Filter darueber entscheiden, ob eine
# Konfiguration als Survivor gilt (PASSED_ALL == TRUE).
#
#   - ALLE Filter F1-F7 werden im Runner IMMER berechnet und in jeder
#     Ergebniszeile ausgegeben (Diagnose), egal ob aktiv oder nicht.
#   - Nur die hier gelisteten Filter gehen in PASSED_ALL ein.
#   - Leerer Vektor c() -> PASSED_ALL ist fuer alle Zeilen TRUE (Rohscan).
#
# Verfuegbare Filter-IDs:
#   "F1" = OS MaxDD        > -15%
#   "F2" = OS ProfitFactor > 1.2
#   "F3" = OS Sortino      > 1.0
#   "F4" = OS Calmar       > 0.5
#   "F5" = IS ProfitFactor > 1.0
#   "F6" = OS WinRate in (0.30, 0.85)
#   "F7" = All-Years-Profitable (jedes volle Jahr > 0; volles Jahr = Bars >= min_year_bars)
#
# Beispiele:
#   ACTIVE_FILTERS <- c("F1","F2","F3","F4","F5","F6","F7")  # streng (Default)
#   ACTIVE_FILTERS <- c("F2","F7")                            # nur PF + All-Years
#   ACTIVE_FILTERS <- c("F7")                                 # nur All-Years
#   ACTIVE_FILTERS <- c()                                     # kein Filter (Rohscan)
# =============================================================================
ACTIVE_FILTERS <- c("F1", "F2", "F5", "F6")
# =============================================================================


# -----------------------------------------------------------------------------
# 1) Backtest -> data.table (ohne Excel)
# -----------------------------------------------------------------------------
# strategies     : character vector von Strategie-Stems (ohne .R), z.B.
#                  c("rsi_mean_reversion", "bollinger_zscore_mean_reversion")
#                  NULL = alle .R-Dateien in STRATEGIES_DIR
# instruments    : character vector von CSV-Dateinamen, z.B.
#                  c("GOLD_MINUTE_5.csv", "EURUSD_MINUTE_5.csv")
#                  NULL = alle .csv in PRICE_DATA_DIR
# active_filters : Survivor-Filterset (siehe ACTIVE_FILTERS oben). Ueberschreibt
#                  die Runner-CONFIG. Pro Aufruf einzeln ueberschreibbar.
# ...            : weitere CONFIG-Ueberschreibungen als named args, z.B.
#                  execution="close", min_year_bars=1500L
run_backtest <- function(strategies = NULL, instruments = NULL,
                         active_filters = ACTIVE_FILTERS, cfg = CONFIG, ...) {
  cfg$export_excel <- FALSE
  if (!is.null(strategies))  cfg$strategies  <- strategies
  if (!is.null(instruments)) cfg$instruments <- instruments
  # active_filters IMMER setzen (auch leer): c()/character(0) -> Rohscan.
  # missing() unterscheidet "nicht uebergeben" von "bewusst leer".
  if (!missing(active_filters)) {
    cfg$active_filters <- if (is.null(active_filters)) character(0) else active_filters
  } else if (!is.null(active_filters)) {
    cfg$active_filters <- active_filters
  }
  dots <- list(...)
  for (nm in names(dots)) cfg[[nm]] <- dots[[nm]]
  main(cfg)
}


# -----------------------------------------------------------------------------
# 2) Kompakte Tabellen-Ansicht
# -----------------------------------------------------------------------------
show_table <- function(results_dt, n = 30, only_passed = FALSE) {
  dt <- data.table::copy(results_dt)
  if (only_passed) dt <- dt[PASSED_ALL == TRUE]
  if ("OS_Sharpe" %in% names(dt)) data.table::setorder(dt, -OS_Sharpe)
  cols <- intersect(c("Strategy", "Instrument", "Params",
                      "OS_Sharpe", "OS_ProfitFactor", "OS_WinRate",
                      "OS_Trades", "OS_MaxDD", "Worst_Year_Ret",
                      "Active_Filters", "PASSED_ALL"),
                    names(dt))
  print(head(dt[, ..cols], n))
  invisible(dt[, ..cols])
}


# -----------------------------------------------------------------------------
# Helfer: Param_*-Spalten einer Zeile -> named list (fuer generate_signals)
# -----------------------------------------------------------------------------
.row_to_params <- function(row) {
  pcols <- grep("^Param_", names(row), value = TRUE)
  if (length(pcols) == 0) return(list())
  vals <- as.list(row[, ..pcols])
  keep <- !vapply(vals, function(x) length(x) == 0 || all(is.na(x)), logical(1))
  vals <- vals[keep]
  stats::setNames(vals, sub("^Param_", "", names(vals)))
}

# Helfer: Strategie-NAME -> Datei-Stem (fuer sys.source des Plugins)
.strategy_stem_map <- function(cfg = CONFIG) {
  strs <- load_strategies(cfg$strategies)
  stats::setNames(
    lapply(strs, function(x) tools::file_path_sans_ext(basename(x$path))),
    names(strs)
  )
}


# -----------------------------------------------------------------------------
# 3) Survivors aufbereiten (PASSED_ALL == TRUE, Params als Liste)
# -----------------------------------------------------------------------------
prepare_survivors <- function(results_dt, max_n = NULL) {
  surv <- results_dt[PASSED_ALL == TRUE]
  if (nrow(surv) == 0) {
    message("Keine Survivors (PASSED_ALL == TRUE).")
    return(surv)
  }
  data.table::setorder(surv, -OS_Sharpe)
  if (!is.null(max_n)) surv <- head(surv, max_n)
  
  if ("Params" %in% names(surv)) data.table::setnames(surv, "Params", "Params_str")
  plist <- lapply(seq_len(nrow(surv)), function(i) .row_to_params(surv[i]))
  surv[, Params := plist]
  if ("Cost_bps_side" %in% names(surv)) surv[, Cost_bps := Cost_bps_side]
  
  message(sprintf("Survivors: %d  (Top OS_Sharpe = %.3f)",
                  nrow(surv), max(surv$OS_Sharpe, na.rm = TRUE)))
  surv[]
}


# -----------------------------------------------------------------------------
# 4) MCPT + DSR pro Survivor
#    - bars_per_year wird PRO INSTRUMENT inferiert (korrekte Annualisierung)
#    - MCPT auf dem OS-Segment (passend zur beobachteten OS_Sharpe)
# -----------------------------------------------------------------------------
run_validation <- function(survivors, results_dt, cfg = CONFIG,
                           n_perm = 500, seed = 42) {
  if (nrow(survivors) == 0) { message("Nichts zu validieren."); return(invisible()) }
  strat_map <- .strategy_stem_map(cfg)
  out <- vector("list", nrow(survivors))
  
  for (i in seq_len(nrow(survivors))) {
    s          <- survivors[i]
    stem_strat <- strat_map[[s$Strategy]]
    inst_stem  <- s$Instrument
    params     <- s$Params[[1]]
    cost       <- if (!is.null(s$Cost_bps)) s$Cost_bps else cfg$cost_bps
    exec       <- cfg$execution
    
    cat(sprintf("\n[%d/%d] %s | %s  (cost %.2f bp, %s)\n  Params: %s\n",
                i, nrow(survivors), s$Strategy, inst_stem, cost, exec,
                paste(names(params), unlist(params), sep = "=", collapse = ", ")))
    
    csv <- file.path(PRICE_DATA_DIR, paste0(inst_stem, ".csv"))
    df  <- load_price_data_cached(csv, use_cache = isTRUE(cfg$use_cache))
    bpy <- infer_bars_per_year(df)
    split_idx <- floor(nrow(df) * IS_SPLIT)
    os_df <- df[(split_idx + 1):nrow(df)]
    
    strat <- .load_strategy_env(stem_strat)
    
    # MCPT (Bar-Permutation) auf OS-Segment, korrekte bpy
    mcpt <- mcpt_strategy(
      df_os = os_df, generate_signals_fn = strat$generate_signals,
      params = params, observed_sharpe = s$OS_Sharpe,
      bars_per_year = bpy, cost_bps = cost, execution = exec,
      n_perm = n_perm, seed = seed, verbose = TRUE
    )
    
    # DSR ueber den Trial-Pool dieses Instruments
    trial_pool <- results_dt[Instrument == inst_stem, OS_Sharpe]
    df_sig <- do.call(strat$generate_signals, c(list(df = os_df), params))
    sr     <- .strat_returns(df_sig, cost, exec)
    rets   <- sr$strat_ret[!is.na(sr$strat_ret)]
    dsr    <- deflated_sharpe_ratio(s$OS_Sharpe, trial_pool, rets, length(trial_pool))
    
    cat(sprintf("  MCPT p = %.4f (null q95 %.3f) -> %s   |   DSR = %.4f -> %s\n",
                mcpt$p_value, mcpt$null_q95,
                if (mcpt$significant_at_95) "PASS" else "FAIL",
                dsr$dsr, if (isTRUE(dsr$significant_at_95)) "PASS" else "FAIL"))
    
    out[[i]] <- data.table::data.table(
      Strategy     = s$Strategy,
      Instrument   = inst_stem,
      Params       = s$Params_str,
      OS_Sharpe    = s$OS_Sharpe,
      Bars_per_Yr  = bpy,
      MCPT_p       = round(mcpt$p_value, 4),
      MCPT_null_q95= round(mcpt$null_q95, 3),
      MCPT_pass    = mcpt$significant_at_95,
      DSR          = round(dsr$dsr, 4),
      DSR_pass     = isTRUE(dsr$significant_at_95),
      FINAL_PASS   = mcpt$significant_at_95 && isTRUE(dsr$significant_at_95)
    )
  }
  
  res <- data.table::rbindlist(out)
  cat("\n================== VALIDIERUNG ==================\n")
  print(res)
  cat(sprintf("FINAL_PASS: %d/%d\n", sum(res$FINAL_PASS), nrow(res)))
  res
}


# -----------------------------------------------------------------------------
# 5) Trade-Listen exportieren + Konsistenz pruefen (full series fuer cTrader)
# -----------------------------------------------------------------------------
check_survivor_trades <- function(survivors, cfg = CONFIG, segment = "full") {
  if (nrow(survivors) == 0) { message("Keine Survivors."); return(invisible()) }
  strat_map <- .strategy_stem_map(cfg)
  res <- vector("list", nrow(survivors))
  
  for (i in seq_len(nrow(survivors))) {
    s <- survivors[i]
    r <- export_trades_runner(
      strategy_stem  = strat_map[[s$Strategy]],
      instrument_csv = paste0(s$Instrument, ".csv"),
      params         = s$Params[[1]],
      segment        = segment,
      cfg            = cfg
    )
    res[[i]] <- data.table::data.table(
      Strategy   = s$Strategy,
      Instrument = s$Instrument,
      Trades     = nrow(r$trades),
      Open_End   = if (nrow(r$trades) > 0) sum(r$trades$still_open) else 0L,
      File       = r$file
    )
  }
  out <- data.table::rbindlist(res)
  cat("\n================== TRADE-LISTEN ==================\n")
  print(out)
  out
}


# -----------------------------------------------------------------------------
# 6) Komplett-Durchlauf in einem Aufruf
# -----------------------------------------------------------------------------
# active_filters wird hier explizit durchgereicht -> die Pipeline ist die
# alleinige Steuerstelle fuer die Survivor-Definition.
pipeline <- function(cfg = CONFIG, n_perm = 500, max_survivors = 10,
                     show_n = 30, active_filters = ACTIVE_FILTERS) {
  message("=== 1) Backtest ===")
  message(sprintf("    Aktive Filter (Survivor): %s",
                  if (length(active_filters) > 0) paste(active_filters, collapse = ", ") else "KEINE (Rohscan)"))
  results <- run_backtest(active_filters = active_filters, cfg = cfg)
  
  message("\n=== 2) Tabelle (Top nach OS_Sharpe) ===")
  show_table(results, n = show_n)
  
  message("\n=== 3) Survivors ===")
  survivors <- prepare_survivors(results, max_n = max_survivors)
  if (nrow(survivors) == 0) {
    message("Keine Survivors -> Pipeline endet hier. ",
            "Tipp: ACTIVE_FILTERS (oben) lockern oder Param-Grid erweitern.")
    return(invisible(list(results = results, survivors = survivors)))
  }
  
  message("\n=== 4) MCPT + DSR ===")
  validation <- run_validation(survivors, results, cfg, n_perm = n_perm)
  
  message("\n=== 5) Trade-Listen + Konsistenz ===")
  trades <- check_survivor_trades(survivors, cfg, segment = "full")
  
  invisible(list(
    results    = results,
    survivors  = survivors,
    validation = validation,
    trades     = trades
  ))
}


# =============================================================================
# >>> STEP-BY-STEP (manuell ausfuehren, nicht beim Sourcen) <<<
# =============================================================================
# # Alles auf einmal (nutzt ACTIVE_FILTERS von oben):
# pl <- pipeline(n_perm = 500, max_survivors = 10)
#
# # Filterset NUR fuer diesen Lauf abweichend (ohne ACTIVE_FILTERS oben zu aendern):
# pl <- pipeline(n_perm = 500, active_filters = c("F2","F7"))
#
# # ODER einzeln, zum Iterieren:

message(paste0("Backtest started ", Sys.time(), sep = " "))

future::plan(future::sequential)
results   <- run_backtest(
  strategies  = NULL,
  instruments = c("GOLD_MINUTE_5.csv", "SILVER_MINUTE_5.csv"),
  active_filters = ACTIVE_FILTERS)          # 1)  <- Filter kommen von oben

message(paste0("Backtest ended ", Sys.time(), sep = " "))

write.csv(results, file.path(OUTPUT_DIR, paste0("runner_results", Sys.Date(), ".csv")))

show_table(results, n = 40)                 # 2)

show_table(results, only_passed = TRUE)     # nur Survivors

survivors <- prepare_survivors(results, max_n = 8)   # 3)

val       <- run_validation(survivors, results, n_perm = 1000)  # 4)

trd       <- check_survivor_trades(survivors)        # 5)

# Filterset ad-hoc fuer einen einzelnen Scan ueberschreiben:
# results <- run_backtest(strategies = c("keltner_squeeze_breakout"),
#                         active_filters = c("F7"))   # nur All-Years

# Eine konkrete Strategie gezielt pruefen (ohne den ganzen Scan):
# export_trades_runner("rsi_mean_reversion", "OIL_BRENT_MINUTE_15.csv",
#                      list(period=10L, lower=20, upper=80, exit_mid=TRUE), "full")