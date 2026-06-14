# =============================================================================
# Silver Asia Range Breakout v4 — Vektorisierte Version
#
# Identische Logik zu asia_breakout.R, aber 4 R-Schleifen durch
# data.table-Operationen ersetzt:
#
#   Loop 1 (per-day Asia range) → dt[in_asia, .(max/min), by=bar_date]
#   Loop 2 (bars_since counter) → gidx - nafill(cross_gidx, "locf"), by=bar_date
#   Loop 3 (traded_today)       → cummax(entered), by=bar_date
#   Loop 4 (position LOCF)      → nafill(sig, "locf")
#
# Äquivalenz geprüft via r/multi_backtest_runner/tests/verify_equivalence.R
# =============================================================================

NAME <- "SilverAsiaBreakoutV4"

PARAM_GRID <- list(
  asia_start_hour  = c(0),
  asia_end_hour    = c(6),
  vol_lookback     = c(15, 25),
  vol_threshold    = c(0.8, 1.1),
  kc_length        = c(36, 54),
  kc_mult          = c(1.1, 1.5),
  ma_fast_len      = c(72, 120),
  ma_slow_len      = c(180, 300),
  min_range        = c(0.0, 0.4),
  breakout_window  = c(8, 16),
  min_kc_dist_pct  = c(0.10, 0.25),
  rsi_length       = c(12, 18),
  div_lookback     = c(18, 28),
  max_dtd_pct      = c(0.0, 2.5),
  atr_length       = c(14),
  sl_atr_mult      = c(2.5),
  min_sl_pct       = c(1.5),
  tp_pct           = c(1.5),
  trail_atr_mult   = c(3.0)
)
# Total: 2^12 = 4,096 Kombinationen


# =============================================================================
# Helpers  (identisch mit asia_breakout.R)
# =============================================================================
.ema <- function(x, n) as.numeric(TTR::EMA(x, n = n))
.rsi <- function(x, n) as.numeric(TTR::RSI(x, n = n))

.atr <- function(high, low, close, n) {
  as.numeric(TTR::ATR(cbind(high, low, close), n = n)[, "atr"])
}

.keltner <- function(high, low, close, n, mult) {
  basis <- .ema(close, n)
  atr_v <- .atr(high, low, close, n)
  list(upper = basis + mult * atr_v, lower = basis - mult * atr_v)
}

.rsi_divergence <- function(high, low, rsi_v, lookback) {
  price_high_lb <- as.numeric(data.table::frollapply(high,  lookback, max, fill = NA, align = "right"))
  price_low_lb  <- as.numeric(data.table::frollapply(low,   lookback, min, fill = NA, align = "right"))
  rsi_high_lb   <- as.numeric(data.table::frollapply(rsi_v, lookback, max, fill = NA, align = "right"))
  rsi_low_lb    <- as.numeric(data.table::frollapply(rsi_v, lookback, min, fill = NA, align = "right"))

  bearish_div <- high >= price_high_lb * 0.999 & rsi_v < rsi_high_lb - 5
  bullish_div <- low  <= price_low_lb  * 1.001 & rsi_v > rsi_low_lb  + 5

  bearish_div[is.na(bearish_div)] <- FALSE
  bullish_div[is.na(bullish_div)] <- FALSE
  list(bearish = bearish_div, bullish = bullish_div)
}


# =============================================================================
# MAIN SIGNAL FUNCTION
# =============================================================================
generate_signals <- function(
    df,

    asia_start_hour  = 0,
    asia_end_hour    = 6,

    vol_lookback     = 20,
    vol_threshold    = 1.0,
    kc_length        = 45,
    kc_mult          = 1.3,
    ma_fast_len      = 108,
    ma_slow_len      = 252,
    min_range        = 0.0,
    breakout_window  = 12,
    min_kc_dist_pct  = 0.2,
    rsi_length       = 14,
    div_lookback     = 20,
    max_dtd_pct      = 3.0,

    atr_length       = 14,
    sl_atr_mult      = 2.5,
    min_sl_pct       = 1.5,
    tp_pct           = 1.5,
    trail_atr_mult   = 3.0
) {
  out <- data.table::copy(df)

  if (!inherits(out$Timestamp, "POSIXct"))
    out[, Timestamp := as.POSIXct(Timestamp)]

  ts_vienna <- lubridate::with_tz(out$Timestamp, "Europe/Vienna")
  bar_hour  <- as.integer(format(ts_vienna, "%H"))
  bar_date  <- as.Date(ts_vienna)

  N    <- nrow(out)
  gidx <- seq_len(N)

  # ── 1. Indikatoren (identisch mit Original) ────────────────────────────────
  vol_sma   <- as.numeric(TTR::SMA(out$Volume, n = vol_lookback))
  vol_ratio <- ifelse(is.na(vol_sma) | vol_sma == 0, 0, out$Volume / vol_sma)

  kc            <- .keltner(out$High, out$Low, out$Close, kc_length, kc_mult)
  kc_upper      <- kc$upper
  kc_lower      <- kc$lower
  kc_dist_long  <- (out$Close - kc_upper) / out$Close * 100
  kc_dist_short <- (kc_lower - out$Close) / out$Close * 100

  ma_fast <- .ema(out$Close, ma_fast_len)
  ma_slow <- .ema(out$Close, ma_slow_len)
  ma_bull <- ma_fast > ma_slow
  ma_bear <- ma_fast < ma_slow

  rsi_v       <- .rsi(out$Close, rsi_length)
  div         <- .rsi_divergence(out$High, out$Low, rsi_v, div_lookback)
  bearish_div <- div$bearish
  bullish_div <- div$bullish

  # ── 2. Asia-Range pro Tag (LOOP 1 → data.table by=bar_date) ───────────────
  in_asia <- bar_hour >= asia_start_hour & bar_hour < asia_end_hour

  asia_agg <- data.table(
    bar_date = bar_date,
    High     = out$High,
    Low      = out$Low,
    Close    = out$Close,
    in_asia  = in_asia
  )[in_asia == TRUE, .(
    asia_h = max(High,  na.rm = TRUE),
    asia_l = min(Low,   na.rm = TRUE),
    asia_c = Close[.N]             # letzter Close der Asia-Session
  ), by = bar_date]

  setorder(asia_agg, bar_date)

  # DTD: Vergleich mit Asia-Close des Vortags (shift innerhalb asia_agg)
  asia_agg[, prev_asia_c := data.table::shift(asia_c, 1, type = "lag")]
  asia_agg[, dtd_chg := data.table::fifelse(
    !is.na(prev_asia_c) & prev_asia_c > 0,
    abs(asia_c - prev_asia_c) / prev_asia_c * 100,
    NA_real_
  )]
  asia_agg[, dtd_ok := (max_dtd_pct == 0) | is.na(dtd_chg) | (dtd_chg <= max_dtd_pct)]

  # Join zurück auf Barebene (LOOP 2 Zuweisung → right join)
  bar_dt <- data.table(bar_date = bar_date, bar_hour = bar_hour)
  bar_dt <- asia_agg[bar_dt, on = "bar_date"]   # all bars preserved

  after_asia <- bar_dt$bar_hour >= asia_end_hour

  asia_high_vec <- data.table::fifelse(after_asia & !is.na(bar_dt$asia_h),
                                       bar_dt$asia_h, NA_real_)
  asia_low_vec  <- data.table::fifelse(after_asia & !is.na(bar_dt$asia_l),
                                       bar_dt$asia_l, NA_real_)
  range_set_vec <- after_asia & !is.na(bar_dt$asia_h)

  dtd_ok_vec <- data.table::fifelse(after_asia & !is.na(bar_dt$dtd_ok),
                                    bar_dt$dtd_ok, TRUE)

  range_size  <- asia_high_vec - asia_low_vec
  range_valid <- range_set_vec & (min_range == 0 |
                                    (!is.na(range_size) & range_size >= min_range))

  # ── 3. Breakout-Erkennung (identisch mit Original) ────────────────────────
  close_lag1     <- data.table::shift(out$Close,    1, type = "lag")
  asia_high_lag1 <- data.table::shift(asia_high_vec, 1, type = "lag")
  asia_low_lag1  <- data.table::shift(asia_low_vec,  1, type = "lag")

  cross_above <- !is.na(close_lag1) & !is.na(asia_high_vec) &
    close_lag1 <= asia_high_lag1 & out$Close > asia_high_vec & range_valid
  cross_below <- !is.na(close_lag1) & !is.na(asia_low_vec) &
    close_lag1 >= asia_low_lag1  & out$Close < asia_low_vec  & range_valid

  cross_above[is.na(cross_above)] <- FALSE
  cross_below[is.na(cross_below)] <- FALSE

  # ── 4. bars_since (LOOP 2 → nafill LOCF by=bar_date) ─────────────────────
  # Für jeden Bar: globaler Index des letzten Crossovers im selben Tag.
  # Kein Tag-Übertrag da nafill by=bar_date.
  cross_gidx_long  <- data.table::fifelse(cross_above, gidx, NA_integer_)
  cross_gidx_short <- data.table::fifelse(cross_below, gidx, NA_integer_)

  bs_dt <- data.table(
    bar_date         = bar_date,
    gidx             = gidx,
    cross_gidx_long  = cross_gidx_long,
    cross_gidx_short = cross_gidx_short
  )
  bs_dt[, last_cross_long  := data.table::nafill(cross_gidx_long,  "locf"), by = bar_date]
  bs_dt[, last_cross_short := data.table::nafill(cross_gidx_short, "locf"), by = bar_date]

  bars_since_long_bo  <- data.table::fifelse(
    is.na(bs_dt$last_cross_long),  999L,
    pmin(gidx - bs_dt$last_cross_long,  999L))
  bars_since_short_bo <- data.table::fifelse(
    is.na(bs_dt$last_cross_short), 999L,
    pmin(gidx - bs_dt$last_cross_short, 999L))

  # ── 5. Breakout-Fenster ───────────────────────────────────────────────────
  in_long_window  <- bars_since_long_bo  <= breakout_window &
    !is.na(out$Close) & out$Close > asia_high_vec
  in_short_window <- bars_since_short_bo <= breakout_window &
    !is.na(out$Close) & out$Close < asia_low_vec

  in_long_window[is.na(in_long_window)]   <- FALSE
  in_short_window[is.na(in_short_window)] <- FALSE

  # ── 6. Entry-Bedingungen (identisch mit Original) ─────────────────────────
  kc_dist_ok_long  <- !is.na(kc_dist_long)  & kc_dist_long  >= min_kc_dist_pct
  kc_dist_ok_short <- !is.na(kc_dist_short) & kc_dist_short >= min_kc_dist_pct

  enter_long_raw <- (
    in_long_window &
    !is.na(vol_ratio) & vol_ratio >= vol_threshold &
    !is.na(kc_upper)  & out$Close > kc_upper       &
    !is.na(ma_bull)   & ma_bull                    &
    kc_dist_ok_long                                 &
    !bearish_div                                    &
    dtd_ok_vec
  )
  enter_short_raw <- (
    in_short_window &
    !is.na(vol_ratio) & vol_ratio >= vol_threshold &
    !is.na(kc_lower)  & out$Close < kc_lower       &
    !is.na(ma_bear)   & ma_bear                    &
    kc_dist_ok_short                                &
    !bullish_div                                    &
    dtd_ok_vec
  )
  enter_long_raw[is.na(enter_long_raw)]   <- FALSE
  enter_short_raw[is.na(enter_short_raw)] <- FALSE

  # ── 7. Ein Trade pro Tag (LOOP 3 → cummax by=bar_date) ───────────────────
  # cummax der unkontrollierten Entry-Signale ergibt traded_today
  entered_raw <- as.integer(enter_long_raw | enter_short_raw)

  tt_dt <- data.table(bar_date = bar_date, entered = entered_raw)
  tt_dt[, traded_today := cummax(entered), by = bar_date]
  traded_today_v <- as.logical(tt_dt$traded_today)

  can_trade <- !data.table::shift(traded_today_v, 1, fill = FALSE) |
    (bar_date != data.table::shift(bar_date, 1, fill = bar_date[1L]))

  enter_long_raw[!can_trade]  <- FALSE
  enter_short_raw[!can_trade] <- FALSE

  # ── 8. No-Look-Ahead-Shift (identisch mit Original) ───────────────────────
  enter_long_shifted  <- data.table::shift(enter_long_raw,  1, fill = FALSE, type = "lag")
  enter_short_shifted <- data.table::shift(enter_short_raw, 1, fill = FALSE, type = "lag")

  # ── 9. Position via LOCF (LOOP 4 → nafill) ───────────────────────────────
  # Long überschreibt Short beim gleichen Bar (identisch mit Original: else-if long-first)
  sig <- rep(NA_integer_, N)
  sig[enter_short_shifted] <- -1L
  sig[enter_long_shifted]  <-  1L      # long gewinnt bei Gleichstand

  sig <- data.table::nafill(sig, "locf")
  sig[is.na(sig)] <- 0L                # führende NAs = flat

  out[, Position := as.integer(sig)]
  out
}
