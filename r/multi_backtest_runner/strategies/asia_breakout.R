# =============================================================================
# Silver Asia Range Breakout v4 — R Plugin
# Converted from PineScript v6 | Designed for 5-min Silver CFD data
#
# Strategy Logic:
#   1. Define Asia session range (default 00:00–06:00 Vienna time)
#   2. Wait for breakout of Asia High/Low after session closes
#   3. Confirm with: KC (15min EMA±ATR), 1H MA cross, Volume ratio, KC distance, RSI divergence, DTD filter
#   4. Hold position until counter-signal or new day (no ATR-stop simulation)
#
# NOTE — Filters approximated on the base timeframe (5min) since multi-TF
#   request.security() calls cannot be replicated without pre-computed HTF data:
#   - KC: computed on 5min bars (PineScript used 15min) → set kc_length=45 to approximate
#   - MA: computed on 5min bars (PineScript used 1H)    → set ma_fast_len=108, ma_slow_len=252 to approximate
#   - RSI divergence: rolling lookback window, same logic
#   - Pyramid / trailing / BE logic: NOT simulated (signal-only framework)
#   - ATR stops: noted in comments, not enforced
#   - Time exit (21:00): noted in comments, not enforced
# =============================================================================

NAME <- "SilverAsiaBreakoutV4"

PARAM_GRID <- list(
  # Session (hours in local/Vienna time) — fixed, not searched
  asia_start_hour  = c(0),
  asia_end_hour    = c(6),
  
  # Volume filter
  # vol_lookback: short (15) vs. medium (25) memory for volume baseline
  # vol_threshold: permissive (0.8) vs. strict (1.1) relative volume gate
  vol_lookback     = c(15, 25),
  vol_threshold    = c(0.8, 1.1),
  
  # Keltner Channel (computed on 5min base TF)
  # kc_length: 36 ≈ 12min, 54 ≈ 18min — straddles the intended 15min KC
  # kc_mult:   tight (1.1) vs. wide (1.5) band — controls filter strictness
  kc_length        = c(36, 54),
  kc_mult          = c(1.1, 1.5),
  
  # Moving averages (computed on 5min base TF)
  # Pairs maintain ~2.5x fast/slow ratio (economic: typical trend-confirmation ratio)
  # 72→180: faster (≈6H / 15H on 5min), 120→300: slower (≈10H / 25H on 5min)
  ma_fast_len      = c(72, 120),
  ma_slow_len      = c(180, 300),
  
  # Asia range / breakout window
  # min_range: 0 = no size filter, 0.4 = meaningful range required (reduces noise days)
  # breakout_window: 8 bars (40min) vs. 16 bars (80min) chase tolerance
  min_range        = c(0.0, 0.4),
  breakout_window  = c(8, 16),
  
  # KC distance filter
  # 0.10% = minimal extension beyond band, 0.25% = meaningful breakout confirmation
  min_kc_dist_pct  = c(0.10, 0.25),
  
  # RSI divergence
  # rsi_length:  12 (more reactive) vs. 18 (smoother)
  # div_lookback: 18 bars (~90min lookback) vs. 28 bars (~140min)
  rsi_length       = c(12, 18),
  div_lookback     = c(18, 28),
  
  # Day-to-Day Asia close filter
  # 0.0 = disabled (trade all days), 2.5 = skip high-volatility carry-over days
  max_dtd_pct      = c(0.0, 2.5),
  
  # Risk — fixed, not enforced in signal layer (backtester applies externally)
  atr_length       = c(14),
  sl_atr_mult      = c(2.5),
  min_sl_pct       = c(1.5),
  tp_pct           = c(1.5),
  trail_atr_mult   = c(3.0)
)
# Total combinations: 2^12 = 4,096


# =============================================================================
# HELPER: EMA (exponential moving average) using TTR
# =============================================================================
.ema <- function(x, n) {
  as.numeric(TTR::EMA(x, n = n))
}

# =============================================================================
# HELPER: RSI using TTR
# =============================================================================
.rsi <- function(x, n) {
  as.numeric(TTR::RSI(x, n = n))
}

# =============================================================================
# HELPER: ATR using TTR (requires HLC matrix)
# =============================================================================
.atr <- function(high, low, close, n) {
  hlc <- cbind(high, low, close)
  as.numeric(TTR::ATR(hlc, n = n)[, "atr"])
}

# =============================================================================
# HELPER: Keltner Channel
#   Upper = EMA(close, n) + mult * ATR(n)
#   Lower = EMA(close, n) - mult * ATR(n)
# =============================================================================
.keltner <- function(high, low, close, n, mult) {
  basis <- .ema(close, n)
  atr_v <- .atr(high, low, close, n)
  list(
    upper = basis + mult * atr_v,
    lower = basis - mult * atr_v
  )
}

# =============================================================================
# HELPER: Detect bullish/bearish RSI divergence
#   Bearish div: price at rolling high, RSI below its rolling high by > 5
#   Bullish div: price at rolling low,  RSI above its rolling low  by > 5
# =============================================================================
.rsi_divergence <- function(high, low, rsi_v, lookback) {
  n <- length(rsi_v)
  
  price_high_lb <- as.numeric(data.table::frollapply(high, lookback, max, fill = NA, align = "right"))
  price_low_lb  <- as.numeric(data.table::frollapply(low,  lookback, min, fill = NA, align = "right"))
  rsi_high_lb   <- as.numeric(data.table::frollapply(rsi_v, lookback, max, fill = NA, align = "right"))
  rsi_low_lb    <- as.numeric(data.table::frollapply(rsi_v, lookback, min, fill = NA, align = "right"))
  
  bearish_div <- high >= price_high_lb * 0.999 & rsi_v < rsi_high_lb - 5
  bullish_div <- low  <= price_low_lb  * 1.001 & rsi_v > rsi_low_lb  + 5
  
  # Replace NA with FALSE
  bearish_div[is.na(bearish_div)] <- FALSE
  bullish_div[is.na(bullish_div)] <- FALSE
  
  list(bearish = bearish_div, bullish = bullish_div)
}


# =============================================================================
# MAIN SIGNAL FUNCTION
# =============================================================================
generate_signals <- function(
    df,
    
    # Session
    asia_start_hour  = 0,
    asia_end_hour    = 6,
    
    # Entry filters
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
    
    # Risk (not enforced, comments only)
    atr_length       = 14,
    sl_atr_mult      = 2.5,
    min_sl_pct       = 1.5,
    tp_pct           = 1.5,
    trail_atr_mult   = 3.0
) {
  # ---------------------------------------------------------------------------
  # 0. Setup
  # ---------------------------------------------------------------------------
  out <- data.table::copy(df)
  
  # Ensure Timestamp is POSIXct; convert to Vienna time for session logic
  if (!inherits(out$Timestamp, "POSIXct")) {
    out[, Timestamp := as.POSIXct(Timestamp)]
  }
  ts_vienna <- lubridate::with_tz(out$Timestamp, "Europe/Vienna")
  
  bar_hour  <- as.integer(format(ts_vienna, "%H"))
  bar_date  <- as.Date(ts_vienna)
  
  N <- nrow(out)
  
  # ---------------------------------------------------------------------------
  # 1. Indicator computation (no look-ahead; results used with shift below)
  # ---------------------------------------------------------------------------
  
  # Volume ratio
  vol_sma   <- as.numeric(TTR::SMA(out$Volume, n = vol_lookback))
  vol_ratio <- ifelse(is.na(vol_sma) | vol_sma == 0, 0, out$Volume / vol_sma)
  
  # Keltner Channel (approximated on base TF)
  kc        <- .keltner(out$High, out$Low, out$Close, kc_length, kc_mult)
  kc_upper  <- kc$upper
  kc_lower  <- kc$lower
  
  # KC distance % beyond band
  kc_dist_long  <- (out$Close - kc_upper) / out$Close * 100
  kc_dist_short <- (kc_lower - out$Close) / out$Close * 100
  
  # Moving averages (1H approximated on base TF)
  ma_fast   <- .ema(out$Close, ma_fast_len)
  ma_slow   <- .ema(out$Close, ma_slow_len)
  ma_bull   <- ma_fast > ma_slow    # TRUE = bullish regime
  ma_bear   <- ma_fast < ma_slow    # TRUE = bearish regime
  
  # RSI + divergence
  rsi_v     <- .rsi(out$Close, rsi_length)
  div       <- .rsi_divergence(out$High, out$Low, rsi_v, div_lookback)
  bearish_div <- div$bearish
  bullish_div <- div$bullish
  
  # ATR (for reference — SL/TP/trail not enforced in signal layer)
  # atr_v  <- .atr(out$High, out$Low, out$Close, atr_length)
  # sl_atr_dist <- pmax(atr_v * sl_atr_mult, out$Close * min_sl_pct / 100)
  
  # ---------------------------------------------------------------------------
  # 2. Asia session range — computed per day, no look-ahead
  #    asia_high / asia_low are set on the BAR AFTER the Asia session closes.
  #    They become available (look-ahead free) from the first non-Asia bar
  #    of the same day onwards.
  # ---------------------------------------------------------------------------
  asia_high_vec <- rep(NA_real_, N)
  asia_low_vec  <- rep(NA_real_, N)
  range_set_vec <- rep(FALSE, N)
  
  # DTD tracking
  dtd_ok_vec    <- rep(TRUE, N)
  
  unique_days <- unique(bar_date)
  
  # Build per-day Asia range lookup
  day_asia_high     <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))
  day_asia_low      <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))
  day_asia_close    <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))  # last Asia bar close
  
  in_asia_mask <- bar_hour >= asia_start_hour & bar_hour < asia_end_hour
  
  for (d in as.character(unique_days)) {
    idx <- which(as.character(bar_date) == d & in_asia_mask)
    if (length(idx) > 0) {
      day_asia_high[d]  <- max(out$High[idx],  na.rm = TRUE)
      day_asia_low[d]   <- min(out$Low[idx],   na.rm = TRUE)
      day_asia_close[d] <- out$Close[max(idx)]
    }
  }
  
  # Assign per-bar: range available once Asia session has closed
  prev_day_close <- NA_real_
  
  for (i in seq_len(N)) {
    d <- as.character(bar_date[i])
    h <- bar_hour[i]
    
    # After Asia session and range exists
    if (h >= asia_end_hour && !is.na(day_asia_high[d])) {
      asia_high_vec[i] <- day_asia_high[d]
      asia_low_vec[i]  <- day_asia_low[d]
      range_set_vec[i] <- TRUE
    }
    
    # DTD check: use previous day's Asia close
    if (h >= asia_end_hour) {
      today_close <- day_asia_close[d]
      if (!is.na(prev_day_close) && prev_day_close > 0 && !is.na(today_close)) {
        dtd_chg <- abs(today_close - prev_day_close) / prev_day_close * 100
        dtd_ok_vec[i] <- (max_dtd_pct == 0) || (dtd_chg <= max_dtd_pct)
      }
    }
    
    # Update prev_day_close at first bar of a new day (last Asia close of previous day)
    if (i > 1 && bar_date[i] != bar_date[i - 1]) {
      prev_d <- as.character(bar_date[i - 1])
      if (!is.na(day_asia_close[prev_d])) {
        prev_day_close <- day_asia_close[prev_d]
      }
    }
  }
  
  range_size <- asia_high_vec - asia_low_vec
  range_valid <- range_set_vec & (min_range == 0 | (!is.na(range_size) & range_size >= min_range))
  
  # ---------------------------------------------------------------------------
  # 3. Breakout detection — crossover of Asia High/Low
  #    Using lag-1 close to detect crossover without look-ahead
  # ---------------------------------------------------------------------------
  close_lag1     <- data.table::shift(out$Close, 1, type = "lag")
  asia_high_lag1 <- data.table::shift(asia_high_vec, 1, type = "lag")
  asia_low_lag1  <- data.table::shift(asia_low_vec,  1, type = "lag")
  
  # Crossover: prev close was below/above, current close is above/below
  cross_above <- !is.na(close_lag1) & !is.na(asia_high_vec) &
    close_lag1 <= asia_high_lag1 & out$Close > asia_high_vec & range_valid
  cross_below <- !is.na(close_lag1) & !is.na(asia_low_vec)  &
    close_lag1 >= asia_low_lag1  & out$Close < asia_low_vec  & range_valid
  
  # Track bars since breakout, reset per day
  # NOTE: bars_since is computed on current bar; signal uses bar[i-1] via shift later
  bars_since_long_bo  <- rep(999L, N)
  bars_since_short_bo <- rep(999L, N)
  
  for (i in seq_len(N)) {
    if (cross_above[i]) {
      bars_since_long_bo[i] <- 0L
    } else if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      bars_since_long_bo[i] <- min(bars_since_long_bo[i - 1] + 1L, 999L)
    }
    if (cross_below[i]) {
      bars_since_short_bo[i] <- 0L
    } else if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      bars_since_short_bo[i] <- min(bars_since_short_bo[i - 1] + 1L, 999L)
    }
    # Reset at start of new day
    if (i > 1 && bar_date[i] != bar_date[i - 1]) {
      bars_since_long_bo[i]  <- 999L
      bars_since_short_bo[i] <- 999L
    }
  }
  
  # Window: still within breakout_window bars AND price still beyond level
  in_long_window  <- bars_since_long_bo  <= breakout_window & out$Close > asia_high_vec
  in_short_window <- bars_since_short_bo <= breakout_window & out$Close < asia_low_vec
  
  # Replace NA
  in_long_window[is.na(in_long_window)]   <- FALSE
  in_short_window[is.na(in_short_window)] <- FALSE
  
  # ---------------------------------------------------------------------------
  # 4. All filter conditions (evaluated on CURRENT bar indicator values)
  #    These are then SHIFTED by 1 so Position applies to NEXT bar
  # ---------------------------------------------------------------------------
  kc_dist_ok_long  <- !is.na(kc_dist_long)  & kc_dist_long  >= min_kc_dist_pct
  kc_dist_ok_short <- !is.na(kc_dist_short) & kc_dist_short >= min_kc_dist_pct
  
  no_bearish_div <- !bearish_div
  no_bullish_div <- !bullish_div
  
  enter_long_raw <- (
    in_long_window                          &  # within breakout window, price above Asia High
      !is.na(vol_ratio)  & vol_ratio >= vol_threshold  &  # volume confirmation
      !is.na(kc_upper)   & out$Close > kc_upper        &  # price above KC upper
      !is.na(ma_bull)    & ma_bull                     &  # 1H MA bullish
      kc_dist_ok_long                         &  # minimum distance beyond KC
      no_bearish_div                          &  # no RSI bearish divergence
      dtd_ok_vec                                 # day-to-day Asia change within limit
  )
  
  enter_short_raw <- (
    in_short_window                         &
      !is.na(vol_ratio)  & vol_ratio >= vol_threshold  &
      !is.na(kc_lower)   & out$Close < kc_lower        &
      !is.na(ma_bear)    & ma_bear                     &
      kc_dist_ok_short                        &
      no_bullish_div                          &
      dtd_ok_vec
  )
  
  # Replace any NA
  enter_long_raw[is.na(enter_long_raw)]   <- FALSE
  enter_short_raw[is.na(enter_short_raw)] <- FALSE
  
  # ---------------------------------------------------------------------------
  # 5. One trade per day constraint
  #    Once a trade is entered on a given day, suppress further entries
  # ---------------------------------------------------------------------------
  traded_today <- rep(FALSE, N)
  for (i in seq_len(N)) {
    if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      traded_today[i] <- traded_today[i - 1]
    }
    if ((enter_long_raw[i] || enter_short_raw[i]) && !traded_today[i]) {
      traded_today[i] <- TRUE
    }
  }
  
  can_trade <- !data.table::shift(traded_today, 1, fill = FALSE) |
    (bar_date != data.table::shift(bar_date, 1, fill = bar_date[1]))
  
  enter_long_raw[!can_trade]  <- FALSE
  enter_short_raw[!can_trade] <- FALSE
  
  # ---------------------------------------------------------------------------
  # 6. NO LOOK-AHEAD SHIFT
  #    Signal computed on bar[i] → Position for bar[i+1]
  #    We shift raw entry signals forward by 1 bar.
  # ---------------------------------------------------------------------------
  enter_long_shifted  <- data.table::shift(enter_long_raw,  1, fill = FALSE, type = "lag")
  enter_short_shifted <- data.table::shift(enter_short_raw, 1, fill = FALSE, type = "lag")
  
  # ---------------------------------------------------------------------------
  # 7. Build Position vector: hold until counter-signal
  #    1 = long, -1 = short, 0 = flat
  #    NOTE: In live trading, exits are via:
  #      - ATR-based SL (sl_atr_mult * ATR, min min_sl_pct%)
  #      - TP at tp_pct% from Entry 1
  #      - Time exit at 21:00 Vienna time
  #      - Session pyramid at 15:45 + ATR trailing
  #    These are NOT simulated here; the backtester should apply them externally.
  # ---------------------------------------------------------------------------
  position <- integer(N)
  
  current_pos <- 0L
  for (i in seq_len(N)) {
    if (enter_long_shifted[i] && current_pos != 1L) {
      current_pos <- 1L
    } else if (enter_short_shifted[i] && current_pos != -1L) {
      current_pos <- -1L
    }
    # Optional: flat at start of Asia session (position would be closed by time exit anyway)
    # Uncomment to force flat during Asia session:
    # if (bar_hour[i] >= asia_start_hour && bar_hour[i] < asia_end_hour) {
    #   current_pos <- 0L
    # }
    position[i] <- current_pos
  }
  
  out[, Position := position]
  
  # ---------------------------------------------------------------------------
  # 8. Diagnostic columns (optional, can be dropped in production)
  # ---------------------------------------------------------------------------
  # out[, `:=`(
  #   AsiaHigh     = asia_high_vec,
  #   AsiaLow      = asia_low_vec,
  #   RangeValid   = range_valid,
  #   VolRatio     = vol_ratio,
  #   KC_Upper     = kc_upper,
  #   KC_Lower     = kc_lower,
  #   MA_Bull      = ma_bull,
  #   MA_Bear      = ma_bear,
  #   BearishDiv   = bearish_div,
  #   BullishDiv   = bullish_div,
  #   EnterLongRaw = enter_long_raw,
  #   EnterShortRaw= enter_short_raw,
  #   DTD_OK       = dtd_ok_vec
  # )]
  
  out
}