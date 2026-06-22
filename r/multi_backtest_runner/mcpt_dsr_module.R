# ============================================================================
# Monte Carlo Permutation Test (MCPT) + Deflated Sharpe Ratio (DSR)
# + TRADE-LIST EXPORT (fuer cTrader-Reconciliation)
# ============================================================================
# Drop-in-Modul fuer den Backtest-Runner. Erwartet, dass folgende Funktionen
# bereits geladen sind:
#   - generate_signals(df, ...)   aus dem jeweiligen Strategie-Plugin
#   - compute_metrics(df, bars_per_year, cost_bps, execution)
#   - .strat_returns(df, cost_bps, execution)
#
# Drei Bausteine:
#   1) MCPT auf Bar-Ebene (unveraendert)
#   2) Deflated Sharpe Ratio (unveraendert)
#   3) NEU: extract_trades() + Export, der pro Survivor eine Trade-CSV schreibt.
#      Diese CSV ist die Referenz-Wahrheit, gegen die der cTrader-cBot
#      reconciled wird (Trade-fuer-Trade-Vergleich, < 5% Toleranz).
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(moments)   # fuer skewness / kurtosis; falls nicht installiert: install.packages("moments")
})

# ----------------------------------------------------------------------------
# 1. Bar-Permutation  (UNVERAENDERT)
# ----------------------------------------------------------------------------
permute_bars <- function(df) {
  n <- nrow(df)
  log_ret <- c(0, log(df$Close[-1] / df$Close[-n]))
  o_off <- df$Open  - df$Close
  h_off <- df$High  - df$Close
  l_off <- df$Low   - df$Close
  
  idx <- sample(seq_len(n))
  new_log_ret <- log_ret[idx]
  new_close   <- df$Close[1] * exp(cumsum(new_log_ret))
  
  data.table(
    Timestamp = df$Timestamp,
    Open  = new_close + o_off[idx],
    High  = new_close + h_off[idx],
    Low   = new_close + l_off[idx],
    Close = new_close,
    Volume = df$Volume[idx]
  )
}

# ----------------------------------------------------------------------------
# 2. MCPT fuer eine einzelne Strategie/Konfiguration  (UNVERAENDERT)
# ----------------------------------------------------------------------------
mcpt_strategy <- function(df_os, generate_signals_fn, params,
                          observed_sharpe, bars_per_year,
                          cost_bps = 0, execution = "next_open",
                          n_perm = 1000, seed = 42, verbose = TRUE) {
  set.seed(seed)
  null_sharpes <- numeric(n_perm)
  
  t0 <- Sys.time()
  for (k in seq_len(n_perm)) {
    df_perm <- permute_bars(df_os)
    df_sig  <- do.call(generate_signals_fn, c(list(df = df_perm), params))
    m       <- compute_metrics(df_sig, bars_per_year, cost_bps, execution)
    null_sharpes[k] <- m$sharpe
    if (verbose && k %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      eta     <- elapsed / k * (n_perm - k)
      cat(sprintf("  Permutation %4d/%d  |  elapsed %5.0fs  ETA %5.0fs\n",
                  k, n_perm, elapsed, eta))
    }
  }
  
  p_value <- mean(null_sharpes >= observed_sharpe)
  list(
    observed_sharpe = observed_sharpe,
    null_distribution = null_sharpes,
    p_value = p_value,
    null_mean = mean(null_sharpes),
    null_sd   = sd(null_sharpes),
    null_q95  = unname(quantile(null_sharpes, 0.95)),
    null_q99  = unname(quantile(null_sharpes, 0.99)),
    n_perm = n_perm,
    significant_at_95 = p_value < 0.05,
    significant_at_99 = p_value < 0.01
  )
}

# ----------------------------------------------------------------------------
# 3. Deflated Sharpe Ratio  (UNVERAENDERT)
# ----------------------------------------------------------------------------
deflated_sharpe_ratio <- function(observed_sr, all_trial_sharpes, returns, n_trials) {
  T_ <- length(returns)
  sr_var <- var(all_trial_sharpes, na.rm = TRUE)
  if (is.na(sr_var) || sr_var <= 0) {
    warning("Varianz der Trial-Sharpes <= 0; DSR nicht berechenbar.")
    return(list(dsr = NA_real_, expected_max_sr_h0 = NA_real_))
  }
  
  skew_r <- moments::skewness(returns, na.rm = TRUE)
  kurt_r <- moments::kurtosis(returns, na.rm = TRUE)
  
  gamma <- 0.5772156649015329
  e_max <- sqrt(sr_var) * (
    (1 - gamma) * qnorm(1 - 1 / n_trials) +
      gamma       * qnorm(1 - 1 / (n_trials * exp(1)))
  )
  
  num <- (observed_sr - e_max) * sqrt(T_ - 1)
  den <- sqrt(1 - skew_r * observed_sr + (kurt_r - 1) / 4 * observed_sr^2)
  dsr <- pnorm(num / den)
  
  list(
    observed_sr = observed_sr,
    expected_max_sr_h0 = e_max,
    n_trials = n_trials,
    sr_variance_across_trials = sr_var,
    return_skewness = skew_r,
    return_kurtosis = kurt_r,
    dsr = dsr,
    significant_at_95 = dsr > 0.95,
    significant_at_99 = dsr > 0.99
  )
}

# ============================================================================
# 3b. NEU: TRADE-LIST EXTRAKTION  (fuer cTrader-Reconciliation)
# ============================================================================
# Wandelt die per-Bar-Signal-Serie in DISKRETE Trades um, mit exakt der
# gleichen Execution-Konvention wie .strat_returns. Das Ergebnis ist die
# Referenz-Wahrheit fuer den Vergleich mit dem cTrader-cBot.
#
# Konventionen (muessen zu deinem .strat_returns passen!):
#   execution == "next_open":
#       - Signal entsteht am Close von Bar t (aus Daten bis inkl. Close[t]).
#       - Fill erfolgt am Open der NAECHSTEN Bar (Open[t+1]).
#       => effektive Position in Bar i ist signal[i-1]; Transitions fuellen
#          zum Open[i].
#   execution == "close":
#       - Fill am Close der Signal-Bar.
#       => effektive Position in Bar i ist signal[i]; Transitions fuellen
#          zum Close[i].
#
# Kosten: cost_bps = Half-Spread pro Seite in Basispunkten. Round-Trip-Kosten
#   pro Trade = 2 * cost_bps / 1e4 (Entry + Exit), abgezogen vom Brutto-Return.
#
# Signal-Spalte: auto-detect ("Signal" oder "Position"), sonst per signal_col.
# Werte muessen in {-1, 0, +1} liegen (oder beliebig, Sign wird genutzt).
# ----------------------------------------------------------------------------
extract_trades <- function(df_sig, cost_bps = 0, execution = "next_open",
                           signal_col = NULL, instrument = NA_character_,
                           strategy = NA_character_) {
  
  df <- as.data.table(df_sig)
  n  <- nrow(df)
  
  # --- Signal-Spalte finden ---
  if (is.null(signal_col)) {
    cand <- intersect(c("Signal", "Position", "signal", "position", "pos"),
                      names(df))
    if (length(cand) == 0L) {
      stop("extract_trades: keine Signal-/Position-Spalte gefunden. ",
           "Bitte signal_col explizit setzen. Vorhandene Spalten: ",
           paste(names(df), collapse = ", "))
    }
    signal_col <- cand[1]
  }
  sig <- df[[signal_col]]
  sig[is.na(sig)] <- 0
  sig <- sign(sig)                 # auf {-1,0,1} normieren
  
  # --- effektive Position + Fill-Preise je Bar gemaess Execution ---
  if (execution == "next_open") {
    eff_pos    <- c(0, sig[-n])     # signal[i-1]
    fill_price <- df$Open           # Transitions fuellen zum Open[i]
  } else if (execution == "close") {
    eff_pos    <- sig               # signal[i]
    fill_price <- df$Close          # Transitions fuellen zum Close[i]
  } else {
    stop("extract_trades: execution muss 'next_open' oder 'close' sein.")
  }
  
  ts <- df$Timestamp
  # Timestamp robust als UTC-String (fuer Vergleich mit cTrader-Bars)
  fmt_ts <- function(x) {
    if (inherits(x, "POSIXct")) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    else as.character(x)
  }
  
  rt_cost <- 2 * cost_bps / 1e4     # Round-Trip-Kostenanteil (Bruchteil)
  
  trades <- list()
  tid    <- 0L
  cur    <- 0                       # aktuelle effektive Position
  e_idx  <- NA_integer_
  
  flush_trade <- function(entry_i, exit_i, dir, exit_price, still_open) {
    tid <<- tid + 1L
    entry_price <- fill_price[entry_i]
    gross <- dir * (exit_price / entry_price - 1)
    net   <- gross - rt_cost
    trades[[tid]] <<- data.table(
      trade_id    = tid,
      strategy    = strategy,
      instrument  = instrument,
      direction   = if (dir > 0) "LONG" else "SHORT",
      entry_time  = fmt_ts(ts[entry_i]),
      entry_price = entry_price,
      exit_time   = fmt_ts(ts[exit_i]),
      exit_price  = exit_price,
      bars_held   = exit_i - entry_i,
      gross_return = gross,
      cost_return  = rt_cost,
      net_return   = net,
      still_open   = still_open
    )
  }
  
  for (i in seq_len(n)) {
    target <- eff_pos[i]
    if (target != cur) {
      # bestehende Position schliessen (Fill zum aktuellen Bar-Preis)
      if (cur != 0 && !is.na(e_idx)) {
        flush_trade(e_idx, i, cur, fill_price[i], still_open = FALSE)
      }
      # neue Position eroeffnen
      if (target != 0) {
        e_idx <- i
      } else {
        e_idx <- NA_integer_
      }
      cur <- target
    }
  }
  
  # offene Position am Datenende -> Mark-to-Market zum letzten Close, markieren
  if (cur != 0 && !is.na(e_idx)) {
    flush_trade(e_idx, n, cur, df$Close[n], still_open = TRUE)
  }
  
  if (length(trades) == 0L) {
    return(data.table(
      trade_id = integer(), strategy = character(), instrument = character(),
      direction = character(), entry_time = character(), entry_price = numeric(),
      exit_time = character(), exit_price = numeric(), bars_held = integer(),
      gross_return = numeric(), cost_return = numeric(), net_return = numeric(),
      still_open = logical(), cum_equity = numeric()
    ))
  }
  
  dt <- rbindlist(trades)
  dt[, cum_equity := cumprod(1 + net_return)]   # kompoundierte Equity (Start = 1)
  dt[]
}

# ----------------------------------------------------------------------------
# 3c. NEU: Konsistenz-Check Trade-Liste  <->  .strat_returns
# ----------------------------------------------------------------------------
# Prueft, ob die kompoundierte Summe der Trade-Net-Returns ungefaehr dem
# Total-Return aus .strat_returns entspricht. Weicht es ab, stimmt die
# Execution-Konvention zwischen extract_trades und deinem .strat_returns
# NICHT ueberein -> Reconciliation waere wertlos. Dann laut warnen.
# ----------------------------------------------------------------------------
check_trade_consistency <- function(trades_dt, df_sig, cost_bps, execution,
                                    tol = 0.02) {
  ok <- tryCatch({
    sr <- .strat_returns(df_sig, cost_bps, execution)
    ret <- sr$strat_ret
    ret <- ret[!is.na(ret)]
    total_strat <- prod(1 + ret) - 1
    total_trades <- if (nrow(trades_dt) > 0)
      tail(trades_dt$cum_equity, 1) - 1 else 0
    diff <- abs(total_trades - total_strat)
    rel  <- diff / max(1e-9, abs(total_strat))
    cat(sprintf("  [Check] Total-Return  .strat_returns=%.4f  Trade-Liste=%.4f  (rel.Diff %.1f%%)\n",
                total_strat, total_trades, 100 * rel))
    if (rel > tol) {
      warning(sprintf(
        "Trade-Liste weicht um %.1f%% von .strat_returns ab! ",
        100 * rel),
        "Execution-Konvention in extract_trades stimmt vermutlich nicht mit ",
        ".strat_returns ueberein. Reconciliation mit cTrader ist erst gueltig, ",
        "wenn diese < ", 100*tol, "%% ist.")
      FALSE
    } else TRUE
  }, error = function(e) {
    cat("  [Check] .strat_returns nicht verfuegbar/fehlgeschlagen:",
        conditionMessage(e), "- ueberspringe Konsistenz-Check.\n")
    NA
  })
  invisible(ok)
}

# ----------------------------------------------------------------------------
# 3d. NEU: Export der Trade-Liste fuer EINEN Survivor
# ----------------------------------------------------------------------------
export_trade_list <- function(df_os, generate_signals_fn, params,
                              strategy, instrument,
                              cost_bps = 0, execution = "next_open",
                              out_dir = "trade_lists", signal_col = NULL,
                              run_check = TRUE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df_sig <- do.call(generate_signals_fn, c(list(df = df_os), params))
  
  trades <- extract_trades(
    df_sig, cost_bps = cost_bps, execution = execution,
    signal_col = signal_col, instrument = instrument, strategy = strategy
  )
  
  if (run_check) check_trade_consistency(trades, df_sig, cost_bps, execution)
  
  fname <- file.path(
    out_dir,
    sprintf("trades_%s_%s.csv", strategy, instrument)
  )
  fwrite(trades, fname)
  
  n_open <- if (nrow(trades) > 0) sum(trades$still_open) else 0
  cat(sprintf("  [Export] %d Trades -> %s  (davon %d offen am Ende)\n",
              nrow(trades), fname, n_open))
  
  list(trades = trades, file = fname)
}

# =============================================================================
# HINWEIS: Der fruehere Orchestrator run_mcpt_on_survivors() wurde entfernt.
# Er ist durch run_validation() in main_pipeline.R ersetzt (korrekte
# bars_per_year pro Instrument). Dieses Modul enthaelt jetzt NUR noch
# Funktionsdefinitionen -> beim source() wird nichts ausgefuehrt.
# Verfuegbare Funktionen:
#   permute_bars(), mcpt_strategy(), deflated_sharpe_ratio(),
#   extract_trades(), check_trade_consistency(), export_trade_list()
# =============================================================================