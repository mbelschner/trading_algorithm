# ============================================================
# 02_backtest_lean.R
# Lean ML Trading Pipeline — Gold 15min
#
# Architecture: Timing Model (XGBoost) + Rule-Based Direction
#   - XGBoost predicts: "Is now a good time to trade?" (binary)
#   - 4 direction filters compared: EMA, Supertrend, Donchian, Session Momentum
#   - Walk-Forward: 12m train | 1m gap | 3m test | 3m step
#
# Output: filter × threshold comparison table, equity curve,
#         feature importance, trade CSV
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(xgboost)
  library(TTR)
  library(ggplot2)
  library(lubridate)
})

# ===== 1. CONFIGURATION =====================================

CFG <- list(
  input_file  = "price_data/GOLD_MINUTE_15.csv",
  output_dir  = "backtest_results",

  # Label: |forward return over N bars| > label_atr_mult * ATR(14) → 1, else 0
  # [Fix 1] Dynamic ATR-based threshold replaces static spread/price ratio.
  label_horizon  = 16,     # 12 bars = 3 hours on 15-min data
  label_atr_mult = 2,    # [Fix 1] Label=1 only if move > 1x ATR. Target: 30-50% positive

  # Walk-Forward windows
  train_months = 12,
  test_months  = 3,
  step_months  = 3,
  gap_months   = 1,        # Data embargo between train end and test start

  # XGBoost — sensible defaults, no tuning
  xgb_params = list(
    objective        = "binary:logistic",
    max_depth        = 4,
    eta              = 0.05,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 10,
    eval_metric      = "logloss"
  ),
  xgb_nrounds = 200,

  # Trade simulation
  pred_threshold    = 0.60,          # [Fix 4] Default threshold (changed from 0.55)
  pred_thresholds   = c(0.55, 0.60, 0.65),  # [Fix 4] All thresholds compared
  max_trades_day    = 2,             # Max entries per calendar day per filter
  take_profit_atr   = 2.5,           # [Fix 3] Take Profit in ATR multiples from entry
  trailing_stop_atr = 1.5,           # [Fix 3] Trailing Stop (reduced from 2.0 → R:R ~1.7:1)
  spread_points     = 0.5,           # Cost deducted per trade (round-trip)
  eod_hour_cet      = 19,            # Force-close all positions at this CET hour

  # Trading sessions (CET): 1=Asia 00-08, 2=EU 08-14, 3=US 14-19
  active_sessions = c(1L, 2L, 3L),       # Only trade Asia and US

  # Indicator periods
  ema_n      = 50,
  atr_n      = 14,
  rsi_n      = 14,
  bb_n       = 20,  bb_sd = 2,
  donchian_n = 20,
  st_atr_n   = 10,  st_mult = 3.0,
  vol_sma_n  = 20,  vol_spike_mult = 2.0
)

dir.create(CFG$output_dir, showWarnings = FALSE, recursive = TRUE)

# ===== 2. HELPER FUNCTIONS ==================================

# Supertrend direction: +1 uptrend, -1 downtrend.
# Band only moves in the favorable direction; flip on close crossing active band.
calc_supertrend_dir <- function(high, low, close, n, mult) {
  atr_v <- TTR::ATR(cbind(high, low, close), n = n)[, "atr"]
  hl2   <- (high + low) / 2
  nb    <- length(close)

  trend    <- integer(nb)
  sup_line <- numeric(nb)

  trend[1]    <- 1L
  sup_line[1] <- hl2[1] - mult * ifelse(is.na(atr_v[1]), 0, atr_v[1])

  for (i in 2:nb) {
    if (is.na(atr_v[i]) || is.na(close[i])) {
      trend[i] <- trend[i - 1]; sup_line[i] <- sup_line[i - 1]; next
    }
    lower <- hl2[i] - mult * atr_v[i]
    upper <- hl2[i] + mult * atr_v[i]

    if (trend[i - 1] == 1L) {
      sup_line[i] <- max(lower, sup_line[i - 1])
      if (close[i] < sup_line[i]) {
        trend[i] <- -1L; sup_line[i] <- upper
      } else {
        trend[i] <- 1L
      }
    } else {
      sup_line[i] <- min(upper, sup_line[i - 1])
      if (close[i] > sup_line[i]) {
        trend[i] <- 1L; sup_line[i] <- lower
      } else {
        trend[i] <- -1L
      }
    }
  }
  trend
}


# Bar-by-bar trade simulation for one direction filter.
#
# [Fix 2] Trailing stop updated using PREVIOUS bar's close (cl[i-1]),
#         not the current bar's close — eliminates look-ahead bias.
# [Fix 3] Take Profit checked BEFORE trailing stop each bar.
#
# Exit priority order per bar:
#   1. TP hit  (hi[i] >= tp_level for Long / lo[i] <= tp_level for Short)
#   2. Trail stop hit  (using stop level from previous bar's close)
#   3. EOD forced close  (hour_cet >= eod_hour_cet)
#
# Returns data.table of individual trades.
simulate_trades <- function(dt, dir_col, window_id, cfg) {
  setorder(dt, timestamp)
  n <- nrow(dt)
  if (n == 0L) return(data.table())

  # Pre-extract vectors (avoids repeated row indexing in loop)
  ts       <- dt$timestamp
  dates    <- dt$date_cet
  hours    <- dt$hour_cet
  cl       <- dt$close
  hi       <- dt$high
  lo       <- dt$low
  atr      <- dt$atr14
  ml_sig   <- dt$ml_signal
  dirs     <- dt[[dir_col]]
  sessions <- dt$f_session

  trades <- vector("list", 1000L)
  tc     <- 0L

  # Position state
  in_pos   <- FALSE
  ep       <- NA_real_   # entry price
  dir      <- NA_integer_
  entry_i  <- NA_integer_
  tstop    <- NA_real_   # current trailing stop level
  tp_level <- NA_real_   # fixed take profit level set at entry  [Fix 3]

  # Daily entry counter
  cur_date <- as.Date(NA)
  n_today  <- 0L

  for (i in seq_len(n)) {

    # ---- Manage open position ----
    if (in_pos) {

      # [Fix 2] Update trailing stop using PREVIOUS bar's close, not current
      if (i > 1L && !is.na(cl[i - 1L]) && !is.na(atr[i - 1L])) {
        if (dir == 1L) {
          tstop <- max(tstop, cl[i - 1L] - cfg$trailing_stop_atr * atr[i - 1L])
        } else {
          tstop <- min(tstop, cl[i - 1L] + cfg$trailing_stop_atr * atr[i - 1L])
        }
      }

      # [Fix 3] Evaluate exit conditions (TP has highest priority)
      if (dir == 1L) {
        tp_hit    <- !is.na(hi[i]) && hi[i] >= tp_level
        trail_hit <- !is.na(lo[i]) && lo[i] <= tstop
      } else {
        tp_hit    <- !is.na(lo[i]) && lo[i] <= tp_level
        trail_hit <- !is.na(hi[i]) && hi[i] >= tstop
      }
      eod_close <- hours[i] >= cfg$eod_hour_cet

      if (tp_hit || trail_hit || eod_close) {
        # Priority: TP > Trail > EOD
        if (tp_hit) {
          xp <- tp_level; xr <- "tp"
        } else if (trail_hit) {
          xp <- tstop;    xr <- "trail"
        } else {
          xp <- cl[i];    xr <- "eod"
        }

        pnl <- if (dir == 1L) xp - ep - cfg$spread_points
               else            ep - xp - cfg$spread_points

        tc <- tc + 1L
        trades[[tc]] <- list(
          window = window_id, filter = dir_col,
          entry_ts = ts[entry_i], exit_ts = ts[i],
          dir = if (dir == 1L) "Long" else "Short",
          entry = ep, exit = xp,
          reason = xr, pnl_pts = pnl, pnl_pct = pnl / ep
        )
        in_pos <- FALSE
      }
    }

    # ---- Entry logic (active sessions only) ----
    if (!in_pos && sessions[i] %in% cfg$active_sessions) {

      if (is.na(cur_date) || dates[i] != cur_date) {
        cur_date <- dates[i]; n_today <- 0L
      }

      if (n_today < cfg$max_trades_day &&
          !is.na(ml_sig[i]) && ml_sig[i] == 1L &&
          !is.na(dirs[i])   && dirs[i]   != 0L) {

        in_pos  <- TRUE
        ep      <- cl[i]
        dir     <- dirs[i]
        entry_i <- i
        n_today <- n_today + 1L

        atr_i <- if (!is.na(atr[i])) atr[i] else median(atr, na.rm = TRUE)

        # [Fix 2] Initial stop uses entry bar's ATR/close (correct — this is at entry time)
        tstop    <- if (dir == 1L) ep - cfg$trailing_stop_atr * atr_i
                    else            ep + cfg$trailing_stop_atr * atr_i

        # [Fix 3] Fixed TP level set once at entry
        tp_level <- if (dir == 1L) ep + cfg$take_profit_atr * atr_i
                    else            ep - cfg$take_profit_atr * atr_i
      }
    }
  }

  # Close any position still open at end of test window
  if (in_pos) {
    pnl <- if (dir == 1L) cl[n] - ep - cfg$spread_points
           else            ep - cl[n] - cfg$spread_points
    tc <- tc + 1L
    trades[[tc]] <- list(
      window = window_id, filter = dir_col,
      entry_ts = ts[entry_i], exit_ts = ts[n],
      dir = if (dir == 1L) "Long" else "Short",
      entry = ep, exit = cl[n],
      reason = "end_of_data", pnl_pts = pnl, pnl_pct = pnl / ep
    )
  }

  if (tc == 0L) return(data.table())
  rbindlist(trades[1:tc])
}


# ===== 3. LOAD DATA =========================================

cat("=== Loading Data ===\n")

dt <- fread(CFG$input_file)
setnames(dt, tolower(names(dt)))
if ("time"     %in% names(dt)) setnames(dt, "time",     "timestamp")
if ("datetime" %in% names(dt)) setnames(dt, "datetime", "timestamp")

dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
setorder(dt, timestamp)

dt[, ts_cet   := with_tz(timestamp, "Europe/Berlin")]
dt[, hour_cet := hour(ts_cet)]
dt[, wday_num := wday(ts_cet, week_start = 1)]
dt[, date_cet := as.Date(ts_cet)]

cat(sprintf("  Rows: %s | Range: %s to %s\n",
  format(nrow(dt), big.mark = ","),
  format(min(dt$timestamp)), format(max(dt$timestamp))))


# ===== 4. FEATURE ENGINEERING ===============================

cat("=== Feature Engineering ===\n")

n_dt <- nrow(dt)
hlc  <- cbind(dt$high, dt$low, dt$close)

atr_raw <- TTR::ATR(hlc, n = CFG$atr_n)[, "atr"]
dt[, atr14 := atr_raw]

dt[, f_atr_norm := atr14 / close]

vol_sma <- TTR::SMA(dt$volume, n = CFG$vol_sma_n)
dt[, f_vol_ratio := volume / vol_sma]

dt[, f_rsi := TTR::RSI(close, n = CFG$rsi_n)]

macd_res <- TTR::MACD(dt$close, nFast = 12, nSlow = 26, nSig = 9)
dt[, f_macd_hist := macd_res[, "macd"] - macd_res[, "signal"]]

dt[, f_hour := hour_cet]
dt[, f_wday := wday_num]

dt[, f_session := fcase(
  hour_cet >= 0  & hour_cet < 8,  1L,
  hour_cet >= 8  & hour_cet < 14, 2L,
  hour_cet >= 14 & hour_cet < 19, 3L,
  default = 0L
)]

bb_res <- TTR::BBands(dt$close, n = CFG$bb_n, sd = CFG$bb_sd)
dt[, f_bb_width := (bb_res[, "up"] - bb_res[, "dn"]) / bb_res[, "mavg"]]

ema50 <- TTR::EMA(dt$close, n = CFG$ema_n)
dt[, f_ema_dist := (close - ema50) / close]

dt[, f_ret4  := (close / shift(close, 4L))  - 1]
dt[, f_ret16 := (close / shift(close, 16L)) - 1]

dt[, f_vol_spike := as.integer(!is.na(vol_sma) & volume > CFG$vol_spike_mult * vol_sma)]

dt[, f_hl_range := (high - low) / close]

{
  spike_idx <- which(dt$f_vol_spike == 1L)
  hrs_spike <- rep(NA_real_, n_dt)
  if (length(spike_idx) > 0) {
    bar_pos <- seq_len(n_dt)
    fi      <- findInterval(bar_pos, spike_idx)
    valid   <- fi > 0L
    hrs_spike[valid] <- (bar_pos[valid] - spike_idx[fi[valid]]) * 15 / 60
  }
  dt[, f_hrs_since_spike := hrs_spike]
}

dt[, sess_grp   := rleid(paste(date_cet, f_session))]
dt[, sess_start := close[1L], by = sess_grp]
dt[, f_sess_mom := (close - sess_start) / sess_start]
dt[, c("sess_grp", "sess_start") := NULL]

cat(sprintf("  15 features computed on %s rows\n", format(n_dt, big.mark = ",")))


# ===== 5. DIRECTION FILTERS =================================

cat("=== Direction Filters ===\n")

dt[, dir_ema := fifelse(!is.na(ema50) & close > ema50, 1L, -1L)]

cat("  Supertrend... ")
dt[, dir_supertrend := calc_supertrend_dir(
  dt$high, dt$low, dt$close, CFG$st_atr_n, CFG$st_mult
)]
cat("done\n")

don_high <- TTR::runMax(dt$high, n = CFG$donchian_n)
don_low  <- TTR::runMin(dt$low,  n = CFG$donchian_n)
dt[, dir_donchian := fifelse(close > (don_high + don_low) / 2, 1L, -1L)]

dt[, dir_sess_mom := fifelse(!is.na(f_sess_mom) & f_sess_mom > 0, 1L, -1L)]


# ===== 6. LABEL CREATION ====================================

cat("=== Labels ===\n")

# [Fix 1] Dynamic threshold: label = 1 if |forward return| > label_atr_mult * ATR(14).
# This replaces the static spread/median-price ratio which caused ~85% positive labels.
# Target: 30-50% positive labels so XGBoost has a meaningful filtering problem.
dt[, fwd_close := shift(close, CFG$label_horizon, type = "lead")]
dt[, label     := as.integer(
  !is.na(fwd_close) & !is.na(atr14) &
  abs(fwd_close - close) > CFG$label_atr_mult * atr14
)]
dt[, fwd_close := NULL]

label1_pct <- mean(dt$label, na.rm = TRUE) * 100
cat(sprintf("  ATR mult: %.1fx | Horizon: %d bars (%.0fh) | Label=1: %.1f%%\n",
  CFG$label_atr_mult, CFG$label_horizon, CFG$label_horizon * 15 / 60, label1_pct))

if (label1_pct > 60)
  warning(sprintf(
    "Label=1 is %.1f%% — too high for meaningful filtering! Increase label_atr_mult (currently %.1f).",
    label1_pct, CFG$label_atr_mult))

if (label1_pct < 20)
  warning(sprintf(
    "Label=1 is only %.1f%% — too rare, model will struggle. Decrease label_atr_mult (currently %.1f).",
    label1_pct, CFG$label_atr_mult))


# ===== 7. MODEL DATASET =====================================

FEATURES    <- c("f_atr_norm", "f_vol_ratio", "f_rsi", "f_macd_hist",
                 "f_hour", "f_wday", "f_session", "f_bb_width", "f_ema_dist",
                 "f_ret4", "f_ret16", "f_vol_spike", "f_hl_range",
                 "f_hrs_since_spike", "f_sess_mom")

DIR_FILTERS <- c("dir_ema", "dir_supertrend", "dir_donchian", "dir_sess_mom")

req_cols <- c("timestamp", "date_cet", "hour_cet",
              "open", "high", "low", "close", "volume",
              "atr14", "label", "f_session",
              DIR_FILTERS, FEATURES)

dt_m <- dt[complete.cases(dt[, ..req_cols])]
dt_m[, ts_month := as.Date(floor_date(timestamp, "month"))]

cat(sprintf("  Clean rows for modelling: %s / %s (%.1f%%)\n",
  format(nrow(dt_m), big.mark = ","), format(n_dt, big.mark = ","),
  nrow(dt_m) / n_dt * 100))


# ===== 8. WALK-FORWARD BACKTEST =============================

cat("\n=== Walk-Forward Backtest ===\n")
cat(sprintf("  Train: %dm | Gap: %dm | Test: %dm | Step: %dm\n",
  CFG$train_months, CFG$gap_months, CFG$test_months, CFG$step_months))
cat(sprintf("  Thresholds compared: %s\n", paste(CFG$pred_thresholds, collapse = ", ")))

months_vec <- sort(unique(dt_m$ts_month))
M          <- length(months_vec)

all_trades    <- list()
feat_imp_last <- NULL
wf_i          <- 0L
mi            <- 1L

repeat {
  t_start <- mi
  t_end   <- mi + CFG$train_months - 1L
  v_start <- t_end  + CFG$gap_months  + 1L
  v_end   <- v_start + CFG$test_months - 1L

  if (v_end > M) break
  wf_i <- wf_i + 1L

  m_train <- months_vec[t_start:t_end]
  m_test  <- months_vec[v_start:v_end]

  dtr <- dt_m[ts_month %in% m_train]
  dte <- dt_m[ts_month %in% m_test]

  cat(sprintf("  W%02d | Train %s–%s (%d rows) | Test %s–%s (%d rows)\n",
    wf_i,
    format(min(m_train), "%Y-%m"), format(max(m_train), "%Y-%m"), nrow(dtr),
    format(min(m_test),  "%Y-%m"), format(max(m_test),  "%Y-%m"), nrow(dte)))

  if (nrow(dtr) < 200L || nrow(dte) < 50L) {
    mi <- mi + CFG$step_months; next
  }

  # --- Train XGBoost timing model (once per window) ---
  dtrain <- xgb.DMatrix(as.matrix(dtr[, ..FEATURES]), label = dtr$label)
  model  <- xgb.train(
    params  = CFG$xgb_params,
    data    = dtrain,
    nrounds = CFG$xgb_nrounds,
    verbose = 0
  )

  # --- Predict probabilities (once per window) ---
  dte[, ml_prob := predict(model, xgb.DMatrix(as.matrix(dte[, ..FEATURES])))]

  feat_imp_last <- xgb.importance(feature_names = FEATURES, model = model)

  # --- [Fix 4] Apply each threshold to same predictions, simulate all filters ---
  for (thr in CFG$pred_thresholds) {
    dte[, ml_signal := as.integer(ml_prob > thr)]

    for (dc in DIR_FILTERS) {
      tr <- simulate_trades(dte, dc, wf_i, CFG)
      if (nrow(tr) > 0) {
        tr[, threshold := thr]          # tag with threshold for comparison
        all_trades[[length(all_trades) + 1L]] <- tr
      }
    }
  }

  mi <- mi + CFG$step_months
}

if (length(all_trades) == 0L) stop("No trades generated. Adjust pred_thresholds or check data.")
trades <- rbindlist(all_trades, fill = TRUE)

cat(sprintf("\n  Total trades: %d | Windows: %d\n", nrow(trades), wf_i))


# ===== 9. PERFORMANCE METRICS ===============================

cat("\n=== Results ===\n")

# [Fix 4] Group by both filter AND threshold (4 filters × 3 thresholds = 12 rows)
perf <- trades[, {
  wins <- pnl_pts > 0
  n_tr <- .N
  cum  <- cumsum(pnl_pts)
  dd   <- cummax(cum) - cum
  list(
    n_trades    = n_tr,
    win_rate    = round(sum(wins) / n_tr * 100, 1),
    avg_pnl_pts = round(mean(pnl_pts), 3),
    tot_pnl_pts = round(sum(pnl_pts), 1),
    sharpe      = round(mean(pnl_pct) / sd(pnl_pct) * sqrt(200), 3),
    profit_fac  = round(sum(pnl_pts[wins]) / abs(sum(pnl_pts[!wins]) + 1e-9), 3),
    max_dd_pts  = round(max(dd), 1)
  )
}, by = .(filter, threshold)]

setorder(perf, -sharpe)

# Console table — 12 rows (4 filters × 3 thresholds)
cat("\n")
cat(sprintf("%-16s  Thr   Trades WinRate AvgPnL TotPnL  Sharpe ProfFac  MaxDD\n",
  "Filter"))
cat(strrep("-", 72), "\n")
perf[, cat(sprintf(
  "%-16s  %.2f  %6d  %5.1f%%  %6.3f  %6.1f  %6.3f  %6.3f  %6.1f\n",
  filter, threshold, n_trades, win_rate, avg_pnl_pts,
  tot_pnl_pts, sharpe, profit_fac, max_dd_pts
))]

best_row    <- perf[1]
best_filter <- best_row$filter
best_thresh <- best_row$threshold
cat(sprintf("\nBest combination: %s @ threshold %.2f (Sharpe %.3f)\n",
  best_filter, best_thresh, best_row$sharpe))


# ===== 10. PLOTS ============================================

cat("\n=== Generating Plots ===\n")

# --- Plot 1: Equity curve for best filter + threshold ---
best_trades <- trades[filter == best_filter & threshold == best_thresh][order(entry_ts)]
best_trades[, cum_pnl := cumsum(pnl_pts)]
best_trades[, trade_n := .I]

p_equity <- ggplot(best_trades, aes(x = trade_n, y = cum_pnl)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title    = sprintf("Equity Curve — %s @ thr %.2f",
                       gsub("dir_", "", best_filter), best_thresh),
    subtitle = sprintf("Sharpe: %.3f | Trades: %d | Win Rate: %.1f%% | PF: %.3f",
      best_row$sharpe, best_row$n_trades, best_row$win_rate, best_row$profit_fac),
    x = "Trade #", y = "Cumulative PnL (points)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(CFG$output_dir, "lean_equity_curve.png"),
       p_equity, width = 10, height = 5, dpi = 150)

# --- Plot 2: Filter × threshold comparison (12 bars) ---
perf_plot <- copy(perf)
perf_plot[, label := sprintf("%s @ %.2f", gsub("dir_", "", filter), threshold)]

p_compare <- ggplot(perf_plot, aes(x = reorder(label, sharpe), y = sharpe,
                                    fill = sharpe > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(
    label = sprintf("%.3f  n=%d  WR%.0f%%", sharpe, n_trades, win_rate),
    hjust = ifelse(sharpe >= 0, -0.05, 1.05)
  ), size = 2.8) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
  labs(title = "Direction Filter × Threshold Comparison",
       subtitle = sprintf("TP: %.1fx ATR | Trail: %.1fx ATR | Label ATR mult: %.1f",
                          CFG$take_profit_atr, CFG$trailing_stop_atr, CFG$label_atr_mult),
       x = NULL, y = "Sharpe Ratio") +
  theme_minimal(base_size = 11) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3)))

ggsave(file.path(CFG$output_dir, "lean_filter_comparison.png"),
       p_compare, width = 9, height = 6, dpi = 150)

# --- Plot 3: Feature importance (last walk-forward window) ---
if (!is.null(feat_imp_last) && nrow(feat_imp_last) > 0) {
  fi <- head(feat_imp_last[order(-Gain)], 15)
  fi[, Feature := factor(Feature, levels = rev(Feature))]

  p_fi <- ggplot(fi, aes(x = Feature, y = Gain)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "XGBoost Feature Importance (Gain)",
         subtitle = "From last walk-forward window",
         x = NULL, y = "Gain") +
    theme_minimal(base_size = 12)

  ggsave(file.path(CFG$output_dir, "lean_feature_importance.png"),
         p_fi, width = 8, height = 6, dpi = 150)
}


# ===== 11. CSV EXPORT =======================================

trade_out <- trades[, .(
  timestamp_entry  = entry_ts,
  timestamp_exit   = exit_ts,
  direction        = dir,
  direction_filter = filter,
  threshold        = threshold,
  wf_window        = window,
  entry_price      = round(entry, 4),
  exit_price       = round(exit, 4),
  exit_reason      = reason,
  pnl_points       = round(pnl_pts, 4),
  pnl_pct          = round(pnl_pct * 100, 5)
)]

trade_file <- file.path(CFG$output_dir, "lean_trades.csv")
fwrite(trade_out, trade_file)

cat(sprintf("\nExports:\n"))
cat(sprintf("  Trades CSV:        %s  (%d rows)\n", trade_file, nrow(trade_out)))
cat(sprintf("  Equity curve:      %s\n", file.path(CFG$output_dir, "lean_equity_curve.png")))
cat(sprintf("  Filter comparison: %s\n", file.path(CFG$output_dir, "lean_filter_comparison.png")))
cat(sprintf("  Feature importance:%s\n", file.path(CFG$output_dir, "lean_feature_importance.png")))
cat("\n=== DONE ===\n")
