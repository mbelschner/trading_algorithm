# =============================================================================
# trade_export_glue.R
# -----------------------------------------------------------------------------
# Duenner Wrapper, der die generischen extract_trades()/check_trade_consistency()
# (aus mcpt_dsr_with_trade_export.R) an DEINEN realistischen Walk-Forward-Runner
# anbindet. Nutzt direkt:
#   - SPREAD_BP, instrument_symbol(), load_price_data_cached(), CONFIG$execution,
#     STRATEGIES_DIR, PRICE_DATA_DIR  aus backtest_runner_realistic.R
#   - extract_trades(), check_trade_consistency()  aus mcpt_dsr_with_trade_export.R
#
# LADE-REIHENFOLGE (wichtig, sonst fehlen .strat_returns etc.):
#   source("backtest_runner_realistic.R")
#   source("mcpt_dsr_with_trade_export.R")
#   source("trade_export_glue.R")
#
# Konventionen sind verifiziert konsistent zu .strat_returns:
#   - signal_col = "Position"  (dein Return-Engine nutzt df$Position)
#   - execution  = CONFIG$execution  (next_open: Entry Open[t+1], Exit Open[u+2])
#   - Kosten     = SPREAD_BP[symbol] pro Side; Round-Trip = 2 Sides
# =============================================================================

# -----------------------------------------------------------------------------
# Kosten pro Instrument aufloesen — exakt wie im Runner (Chunk-Bau)
# -----------------------------------------------------------------------------
.resolve_cost_bps <- function(symbol, cfg = CONFIG) {
  if (!isTRUE(cfg$use_spread_map)) return(cfg$cost_bps)
  v <- SPREAD_BP[[symbol]]
  if (is.null(v)) {
    message(sprintf("[WARN] kein Spread fuer %s -> default %.1f bp",
                    symbol, cfg$spread_default))
    return(cfg$spread_default)
  }
  v
}

# -----------------------------------------------------------------------------
# Strategie laden wie im Runner (sys.source in eigene Umgebung)
# -----------------------------------------------------------------------------
.load_strategy_env <- function(strategy_stem) {
  path <- file.path(STRATEGIES_DIR, paste0(strategy_stem, ".R"))
  if (!file.exists(path)) stop("Strategie nicht gefunden: ", path)
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  if (!exists("generate_signals", envir = env))
    stop("generate_signals fehlt in ", path)
  env
}

# -----------------------------------------------------------------------------
# HAUPTFUNKTION: Trade-Liste fuer EIN (Strategie x Instrument) exportieren
# -----------------------------------------------------------------------------
# strategy_stem : Dateiname ohne .R, z.B. "rsi_mean_reversion"
# instrument_csv: CSV-Dateiname in PRICE_DATA_DIR, z.B. "OIL_BRENT_MINUTE_15.csv"
# params        : named list, z.B. list(period = 10L, lower = 20, upper = 80)
# segment       : "full" (fuer Live-Reconciliation), "os" oder "is"
#                 -> nutze "os", wenn die Trade-Zahl exakt zur OS_Sharpe-Zeile passen soll
# out_dir       : Zielordner
# -----------------------------------------------------------------------------
export_trades_runner <- function(strategy_stem, instrument_csv, params,
                                 segment = "full",
                                 cfg = CONFIG,
                                 out_dir = file.path(OUTPUT_DIR, "trade_lists")) {
  
  filepath <- file.path(PRICE_DATA_DIR, instrument_csv)
  if (!file.exists(filepath)) stop("CSV nicht gefunden: ", filepath)
  
  stem   <- tools::file_path_sans_ext(instrument_csv)
  symbol <- instrument_symbol(stem)
  cost   <- .resolve_cost_bps(symbol, cfg)
  exec   <- cfg$execution
  
  df_all <- load_price_data_cached(filepath, use_cache = isTRUE(cfg$use_cache))
  
  # Segment waehlen — identisch zur IS/OS-Aufteilung im Runner
  if (segment == "full") {
    df_use <- df_all
  } else {
    split_idx <- floor(nrow(df_all) * IS_SPLIT)
    if (segment == "is")      df_use <- df_all[1:split_idx]
    else if (segment == "os") df_use <- df_all[(split_idx + 1):nrow(df_all)]
    else stop("segment muss 'full', 'is' oder 'os' sein.")
  }
  
  strat  <- .load_strategy_env(strategy_stem)
  df_sig <- do.call(strat$generate_signals, c(list(df = df_use), params))
  
  if (!"Position" %in% names(df_sig))
    stop("generate_signals hat keine 'Position'-Spalte erzeugt.")
  
  trades <- extract_trades(
    df_sig,
    cost_bps   = cost,
    execution  = exec,
    signal_col = "Position",          # << an .strat_returns angeglichen
    instrument = stem,
    strategy   = strategy_stem
  )
  
  # Konsistenz gegen .strat_returns (sollte gruen sein, da Konventionen passen)
  check_trade_consistency(trades, df_sig, cost, exec)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  fname <- file.path(out_dir,
                     sprintf("trades_%s_%s_%s.csv", strategy_stem, stem, segment))
  data.table::fwrite(trades, fname)
  
  n_open <- if (nrow(trades) > 0) sum(trades$still_open) else 0L
  message(sprintf("[Export] %s | %s (%s): %d Trades -> %s  (offen: %d, Kosten %.2f bp/Side)",
                  strategy_stem, symbol, segment, nrow(trades), fname, n_open, cost))
  
  invisible(list(trades = trades, file = fname, cost_bps = cost, execution = exec))
}

# =============================================================================
# BEISPIEL-AUFRUFE
# =============================================================================
# # Brent RSI, ganze Reihe (fuer cTrader-Live-Reconciliation):
# res <- export_trades_runner(
#   strategy_stem  = "rsi_mean_reversion",
#   instrument_csv = "OIL_BRENT_MINUTE_15.csv",
#   params         = list(period = 10L, lower = 20, upper = 80, exit_mid = TRUE),
#   segment        = "full"
# )
# head(res$trades)
#
# # Nur OS-Segment (Trade-Zahl matcht dann die OS_Sharpe-Zeile im Excel):
# export_trades_runner(
#   strategy_stem  = "rsi_mean_reversion",
#   instrument_csv = "OIL_BRENT_MINUTE_15.csv",
#   params         = list(period = 10L, lower = 20, upper = 80, exit_mid = TRUE),
#   segment        = "os"
# )