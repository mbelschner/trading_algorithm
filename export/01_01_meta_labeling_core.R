# =============================================================================
# META-LABELING CORE FUNCTIONS
# =============================================================================
#
# Core functions for meta-labeling pipeline:
# - Technical indicator calculation
# - Primary signal generation (Side: Long/Short)
# - Dynamic triple barrier labeling with intraday volatility adjustment
# - Meta-label creation (TP=1, SL/Timeout=0)
#
# =============================================================================

library(data.table)
library(TTR)

# =============================================================================
# TECHNICAL INDICATORS
# =============================================================================

#' Calculate Chande Momentum Oscillator (CMO)
#' @param x Price vector
#' @param n Period
#' @return CMO values
calculate_cmo <- function(x, n = 14) {
  delta <- diff(x)
  delta <- c(NA, delta)

  gains <- ifelse(delta > 0, delta, 0)
  losses <- ifelse(delta < 0, abs(delta), 0)

  sum_gains <- runSum(gains, n)
  sum_losses <- runSum(losses, n)

  cmo <- 100 * (sum_gains - sum_losses) / (sum_gains + sum_losses)
  return(cmo)
}

#' Calculate Vertical Horizontal Filter (VHF)
#' @param close Close prices
#' @param n Period
#' @return VHF values (higher = trending, lower = ranging)
calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  numerator <- abs(highest - lowest)

  delta <- abs(diff(close))
  delta <- c(NA, delta)
  denominator <- runSum(delta, n)

  vhf <- numerator / denominator
  return(vhf)
}

#' Calculate Schaff Trend Cycle (STC)
#' @param close Close prices
#' @param n_fast Fast period
#' @param n_slow Slow period
#' @param n_cycle Cycle period
#' @param n_smooth Smoothing period
#' @return STC values (0-100)
calculate_stc <- function(close, n_fast = 23, n_slow = 50, n_cycle = 10, n_smooth = 3) {
  # MACD-like calculation
  ema_fast <- EMA(close, n = n_fast)
  ema_slow <- EMA(close, n = n_slow)
  macd_line <- ema_fast - ema_slow

  # First Stochastic of MACD
  macd_low <- runMin(macd_line, n_cycle)
  macd_high <- runMax(macd_line, n_cycle)
  stoch1 <- ifelse(macd_high - macd_low > 0,
                   100 * (macd_line - macd_low) / (macd_high - macd_low),
                   50)
  pf <- EMA(stoch1, n = n_smooth)

  # Second Stochastic
  pf_low <- runMin(pf, n_cycle)
  pf_high <- runMax(pf, n_cycle)
  stoch2 <- ifelse(pf_high - pf_low > 0,
                   100 * (pf - pf_low) / (pf_high - pf_low),
                   50)
  stc <- EMA(stoch2, n = n_smooth)

  return(stc)
}

#' Calculate Supertrend Indicator
#' @param high High prices
#' @param low Low prices
#' @param close Close prices
#' @param n ATR period
#' @param mult ATR multiplier
#' @return List with supertrend line and direction
calculate_supertrend <- function(high, low, close, n = 10, mult = 3.0) {
  atr <- ATR(cbind(high, low, close), n = n)[, "atr"]

  hl2 <- (high + low) / 2
  upper_band <- hl2 + mult * atr
  lower_band <- hl2 - mult * atr

  supertrend <- rep(NA_real_, length(close))
  direction <- rep(NA_integer_, length(close))

  for (i in (n + 1):length(close)) {
    if (is.na(supertrend[i - 1])) {
      supertrend[i] <- lower_band[i]
      direction[i] <- 1L
    } else if (supertrend[i - 1] == lower_band[i - 1]) {
      # Was in uptrend
      if (close[i] > lower_band[i]) {
        supertrend[i] <- max(lower_band[i], lower_band[i - 1], na.rm = TRUE)
        direction[i] <- 1L
      } else {
        supertrend[i] <- upper_band[i]
        direction[i] <- -1L
      }
    } else {
      # Was in downtrend
      if (close[i] < upper_band[i]) {
        supertrend[i] <- min(upper_band[i], upper_band[i - 1], na.rm = TRUE)
        direction[i] <- -1L
      } else {
        supertrend[i] <- lower_band[i]
        direction[i] <- 1L
      }
    }
  }

  return(list(supertrend = supertrend, direction = direction))
}

#' Calculate Ichimoku Cloud components
#' @param high High prices
#' @param low Low prices
#' @param close Close prices
#' @param n_tenkan Tenkan-sen period (9)
#' @param n_kijun Kijun-sen period (26)
#' @param n_senkou Senkou Span B period (52)
#' @return data.table with Ichimoku components
calculate_ichimoku <- function(high, low, close, n_tenkan = 9, n_kijun = 26, n_senkou = 52) {
  # Tenkan-sen (Conversion Line)
  tenkan <- (runMax(high, n_tenkan) + runMin(low, n_tenkan)) / 2

  # Kijun-sen (Base Line)
  kijun <- (runMax(high, n_kijun) + runMin(low, n_kijun)) / 2

  # Senkou Span A (Leading Span A) - shifted forward 26 periods
  senkou_a <- (tenkan + kijun) / 2

  # Senkou Span B (Leading Span B) - shifted forward 26 periods
  senkou_b <- (runMax(high, n_senkou) + runMin(low, n_senkou)) / 2

  # Chikou Span (Lagging Span) - close shifted back 26 periods
  # For signal generation, we compare current close to cloud

  return(data.table(
    tenkan = tenkan,
    kijun = kijun,
    senkou_a = senkou_a,
    senkou_b = senkou_b
  ))
}


#' Calculate all required technical indicators
#'
#' Calculates indicators for ALL available signal strategies.
#'
#' @param dt Price data.table with OHLCV columns
#' @param atr_period ATR calculation period
#' @param ema_fast Fast EMA period
#' @param ema_slow Slow EMA period
#' @param rsi_period RSI period
#' @return data.table with added indicator columns
calculate_technical_indicators <- function(
    dt,
    atr_period = 12,
    ema_fast = 50,
    ema_slow = 200,
    rsi_period = 14
) {

  dt <- copy(dt)

  # Ensure column names are lowercase
  setnames(dt, tolower(names(dt)))

  # Rename 'time' to 'datetime' if needed (some data sources use 'time')
  if ("time" %in% names(dt) && !"datetime" %in% names(dt)) {
    setnames(dt, "time", "datetime")
  }

  # Ensure datetime is POSIXct
  if (!inherits(dt$datetime, "POSIXct")) {
    dt[, datetime := as.POSIXct(datetime)]
  }

  cat("Calculating technical indicators...\n")

  # ===== BASIC INDICATORS =====

  # ATR
  atr_result <- ATR(HLC = cbind(dt$high, dt$low, dt$close), n = atr_period)
  dt[, atr := atr_result[, "atr"]]
  dt[, atr_pct := atr / close * 100]

  # EMAs (multiple periods for alignment strategy)
  dt[, ema_20 := EMA(close, n = 20)]
  dt[, ema_fast := EMA(close, n = ema_fast)]   # Default 50
  dt[, ema_slow := EMA(close, n = ema_slow)]   # Default 200

  # RSI
  dt[, rsi := RSI(close, n = rsi_period)]

  # MACD
  macd_result <- MACD(dt$close, nFast = 12, nSlow = 26, nSig = 9)
  dt[, macd := macd_result[, "macd"]]
  dt[, macd_signal := macd_result[, "signal"]]
  dt[, macd_hist := macd - macd_signal]

  # Volume Average (for volume filter strategies)
  if ("volume" %in% names(dt)) {
    dt[, volume_avg := SMA(volume, n = 20)]
    dt[, volume_ratio := volume / volume_avg]
  }

  # ===== ADX & DIRECTIONAL INDICATORS =====

  adx_result <- ADX(HLC = cbind(dt$high, dt$low, dt$close), n = 14)
  dt[, adx := adx_result[, "ADX"]]
  dt[, di_plus := adx_result[, "DIp"]]
  dt[, di_minus := adx_result[, "DIn"]]

  # ===== CMO-VHF-STC INDICATORS =====

  dt[, cmo := calculate_cmo(close, n = 14)]
  dt[, vhf := calculate_vhf(close, n = 28)]
  dt[, stc := calculate_stc(close, n_fast = 23, n_slow = 50, n_cycle = 10, n_smooth = 3)]

  # ===== SUPERTREND =====

  supertrend_result <- calculate_supertrend(dt$high, dt$low, dt$close, n = 10, mult = 3.0)
  dt[, supertrend := supertrend_result$supertrend]
  dt[, supertrend_dir := supertrend_result$direction]

  # ===== ICHIMOKU =====

  ichimoku_result <- calculate_ichimoku(dt$high, dt$low, dt$close)
  dt[, tenkan := ichimoku_result$tenkan]
  dt[, kijun := ichimoku_result$kijun]
  dt[, senkou_a := ichimoku_result$senkou_a]
  dt[, senkou_b := ichimoku_result$senkou_b]

  # Cloud top/bottom for easy comparison

  dt[, cloud_top := pmax(senkou_a, senkou_b, na.rm = TRUE)]
  dt[, cloud_bottom := pmin(senkou_a, senkou_b, na.rm = TRUE)]
  dt[, cloud_bullish := senkou_a > senkou_b]

  # ===== TIME-BASED FEATURES =====

  dt[, hour := hour(datetime)]
  dt[, date := as.Date(datetime)]

  # Session classification (UTC times for Gold)
  dt[, session := fcase(
    hour >= 1 & hour < 8, "asia",
    hour >= 8 & hour < 13, "london",
    hour >= 13 & hour < 17, "overlap",
    hour >= 17 & hour < 22, "ny",
    default = "closed"
  )]

  cat(sprintf("  Indicators calculated: %d columns added\n",
              length(setdiff(names(dt), c("datetime", "open", "high", "low", "close", "volume")))))

  return(dt)
}


# =============================================================================
# PRIMARY SIGNAL GENERATION
# =============================================================================

#' List all available primary signal strategies
#' @return Named vector of strategy descriptions
list_primary_signal_strategies <- function() {
  strategies <- c(
    "ema_cross" = "EMA Crossover (50/200) - Classic trend following",
    "cmo_vhf_stc" = "CMO-VHF-STC - Momentum + trend filter + early signals",
    "adx_di" = "ADX + DI Crossover - Strong trend filter",
    "ichimoku" = "Ichimoku Cloud Breakout - Multi-timeframe perspective",
    "supertrend" = "Supertrend + RSI - ATR-based with momentum",
    "macd_volume" = "MACD + Volume - Institutional approach",
    "ema_alignment" = "Multi-EMA Alignment - High conviction pullback",
    "rsi_breakout" = "RSI Breakout - Mean reversion",
    "macd" = "MACD Crossover - Simple momentum"
  )
  return(strategies)
}


#' Generate primary trading signals (Side: Long/Short)
#'
#' The primary signal determines the DIRECTION of the trade.
#' Available strategies:
#' - ema_cross: Classic EMA 50/200 crossover
#' - cmo_vhf_stc: CMO momentum + VHF trend filter + STC early signals
#' - adx_di: ADX trend strength + DI crossover
#' - ichimoku: Ichimoku Cloud breakout + Tenkan/Kijun cross
#' - supertrend: Supertrend flip + RSI confirmation
#' - macd_volume: MACD crossover + volume filter
#' - ema_alignment: Multi-EMA alignment + pullback entry
#' - rsi_breakout: RSI oversold/overbought breakout
#' - macd: Simple MACD crossover
#'
#' @param dt data.table with technical indicators
#' @param method Signal generation method
#' @param params List of strategy-specific parameters
#' @return data.table with primary_signal column (-1, 0, 1)
generate_primary_signals <- function(
    dt,
    method = "ema_cross",
    params = list()
) {

  dt <- copy(dt)

  # Default parameters
  default_params <- list(
    # EMA Cross
    ema_fast_col = "ema_fast",
    ema_slow_col = "ema_slow",
    # RSI
    rsi_overbought = 70,
    rsi_oversold = 30,
    # CMO-VHF-STC
    vhf_threshold = 0.35,
    cmo_threshold = 20,
    stc_long_cross = 25,
    stc_short_cross = 75,
    # ADX
    adx_threshold = 25,
    # Volume
    volume_mult = 1.2,
    # Signal validity
    signal_validity_bars = 5
  )

  # Merge with user params
  p <- modifyList(default_params, params)

  cat(sprintf("Generating primary signals: %s\n", method))

  # =========================================================================
  # STRATEGY 1: EMA CROSSOVER (Original)
  # =========================================================================
  if (method == "ema_cross") {

    dt[, ema_state := fifelse(get(p$ema_fast_col) > get(p$ema_slow_col), 1L, -1L)]
    dt[, ema_state_prev := shift(ema_state, 1)]
    dt[, is_crossover := ema_state != ema_state_prev & !is.na(ema_state_prev)]

    dt[, primary_signal := NA_integer_]
    dt[is_crossover == TRUE, primary_signal := ema_state]

    # Extend signal validity for a few bars
    dt[, bars_since_cross := {
      cross_idx <- which(is_crossover)
      result <- rep(NA_integer_, .N)
      for (idx in cross_idx) {
        signal_val <- ema_state[idx]
        for (j in idx:min(idx + p$signal_validity_bars - 1, .N)) {
          if (is.na(result[j])) result[j] <- j - idx
        }
      }
      result
    }]

    # Fill forward signal
    dt[, signal_to_fill := primary_signal]
    dt[, signal_to_fill := nafill(signal_to_fill, type = "locf")]
    dt[!is.na(bars_since_cross) & bars_since_cross <= p$signal_validity_bars,
       primary_signal := signal_to_fill]

    dt[, c("ema_state", "ema_state_prev", "is_crossover", "bars_since_cross", "signal_to_fill") := NULL]

  # =========================================================================
  # STRATEGY 2: CMO-VHF-STC (User's main system)
  # =========================================================================
  } else if (method == "cmo_vhf_stc") {

    # Check required columns
    if (!all(c("cmo", "vhf", "stc") %in% names(dt))) {
      stop("CMO-VHF-STC strategy requires cmo, vhf, stc columns")
    }

    dt[, stc_prev := shift(stc, 1)]
    dt[, close_prev := shift(close, 1)]

    dt[, primary_signal := fcase(
      # LONG: VHF trending + CMO bullish + STC crosses 25 upward + price confirmation
      vhf > p$vhf_threshold &
        cmo > p$cmo_threshold &
        stc_prev < p$stc_long_cross & stc >= p$stc_long_cross &
        close > close_prev, 1L,

      # SHORT: VHF trending + CMO bearish + STC crosses 75 downward + price confirmation
      vhf > p$vhf_threshold &
        cmo < -p$cmo_threshold &
        stc_prev > p$stc_short_cross & stc <= p$stc_short_cross &
        close < close_prev, -1L,

      default = NA_integer_
    )]

    dt[, c("stc_prev", "close_prev") := NULL]

  # =========================================================================
  # STRATEGY 3: ADX + DI CROSSOVER
  # =========================================================================
  } else if (method == "adx_di") {

    if (!all(c("adx", "di_plus", "di_minus") %in% names(dt))) {
      stop("ADX-DI strategy requires adx, di_plus, di_minus columns")
    }

    dt[, di_plus_prev := shift(di_plus, 1)]
    dt[, di_minus_prev := shift(di_minus, 1)]
    dt[, adx_prev := shift(adx, 1)]

    dt[, primary_signal := fcase(
      # LONG: ADX strong + +DI crosses above -DI
      adx > p$adx_threshold &
        di_plus_prev < di_minus_prev & di_plus >= di_minus, 1L,

      # SHORT: ADX strong + -DI crosses above +DI
      adx > p$adx_threshold &
        di_minus_prev < di_plus_prev & di_minus >= di_plus, -1L,

      default = NA_integer_
    )]

    dt[, c("di_plus_prev", "di_minus_prev", "adx_prev") := NULL]

  # =========================================================================
  # STRATEGY 4: ICHIMOKU CLOUD BREAKOUT
  # =========================================================================
  } else if (method == "ichimoku") {

    if (!all(c("tenkan", "kijun", "cloud_top", "cloud_bottom", "cloud_bullish") %in% names(dt))) {
      stop("Ichimoku strategy requires tenkan, kijun, cloud_top, cloud_bottom, cloud_bullish columns")
    }

    dt[, tenkan_prev := shift(tenkan, 1)]
    dt[, kijun_prev := shift(kijun, 1)]
    dt[, close_prev := shift(close, 1)]

    dt[, primary_signal := fcase(
      # LONG: Price breaks above cloud + cloud bullish + Tenkan crosses above Kijun
      close > cloud_top &
        close_prev <= cloud_top &
        cloud_bullish == TRUE &
        tenkan > kijun &
        tenkan_prev <= kijun_prev, 1L,

      # SHORT: Price breaks below cloud + cloud bearish + Tenkan crosses below Kijun
      close < cloud_bottom &
        close_prev >= cloud_bottom &
        cloud_bullish == FALSE &
        tenkan < kijun &
        tenkan_prev >= kijun_prev, -1L,

      default = NA_integer_
    )]

    # Relaxed version: just cloud breakout with Tenkan/Kijun confirmation
    dt[is.na(primary_signal), primary_signal := fcase(
      close > cloud_top & tenkan > kijun, 1L,
      close < cloud_bottom & tenkan < kijun, -1L,
      default = NA_integer_
    )]

    # Keep only on state changes
    dt[, signal_state := primary_signal]
    dt[, signal_state := nafill(signal_state, type = "locf")]
    dt[, signal_state_prev := shift(signal_state, 1)]
    dt[signal_state == signal_state_prev, primary_signal := NA_integer_]

    dt[, c("tenkan_prev", "kijun_prev", "close_prev", "signal_state", "signal_state_prev") := NULL]

  # =========================================================================
  # STRATEGY 5: SUPERTREND + RSI
  # =========================================================================
  } else if (method == "supertrend") {

    if (!all(c("supertrend", "supertrend_dir", "rsi") %in% names(dt))) {
      stop("Supertrend strategy requires supertrend, supertrend_dir, rsi columns")
    }

    dt[, st_dir_prev := shift(supertrend_dir, 1)]

    dt[, primary_signal := fcase(
      # LONG: Supertrend flips bullish + RSI > 50
      st_dir_prev == -1L & supertrend_dir == 1L & rsi > 50, 1L,

      # SHORT: Supertrend flips bearish + RSI < 50
      st_dir_prev == 1L & supertrend_dir == -1L & rsi < 50, -1L,

      default = NA_integer_
    )]

    dt[, st_dir_prev := NULL]

  # =========================================================================
  # STRATEGY 6: MACD + VOLUME
  # =========================================================================
  } else if (method == "macd_volume") {

    if (!all(c("macd", "macd_signal", "macd_hist") %in% names(dt))) {
      stop("MACD-Volume strategy requires macd, macd_signal, macd_hist columns")
    }

    dt[, macd_prev := shift(macd, 1)]
    dt[, macd_signal_prev := shift(macd_signal, 1)]
    dt[, macd_hist_prev := shift(macd_hist, 1)]
    dt[, macd_hist_prev2 := shift(macd_hist, 2)]

    # Volume filter (if available)
    has_volume <- "volume_ratio" %in% names(dt)

    if (has_volume) {
      dt[, primary_signal := fcase(
        # LONG: MACD crosses signal + histogram rising 2 bars + volume above average
        macd_prev < macd_signal_prev & macd >= macd_signal &
          macd_hist > macd_hist_prev & macd_hist_prev > macd_hist_prev2 &
          volume_ratio > p$volume_mult, 1L,

        # SHORT: MACD crosses below signal + histogram falling 2 bars + volume above average
        macd_prev > macd_signal_prev & macd <= macd_signal &
          macd_hist < macd_hist_prev & macd_hist_prev < macd_hist_prev2 &
          volume_ratio > p$volume_mult, -1L,

        default = NA_integer_
      )]
    } else {
      # Without volume filter
      dt[, primary_signal := fcase(
        macd_prev < macd_signal_prev & macd >= macd_signal &
          macd_hist > macd_hist_prev & macd_hist_prev > macd_hist_prev2, 1L,

        macd_prev > macd_signal_prev & macd <= macd_signal &
          macd_hist < macd_hist_prev & macd_hist_prev < macd_hist_prev2, -1L,

        default = NA_integer_
      )]
    }

    dt[, c("macd_prev", "macd_signal_prev", "macd_hist_prev", "macd_hist_prev2") := NULL]

  # =========================================================================
  # STRATEGY 7: MULTI-EMA ALIGNMENT + PULLBACK
  # =========================================================================
  } else if (method == "ema_alignment") {

    if (!all(c("ema_20", "ema_fast", "ema_slow", "rsi") %in% names(dt))) {
      stop("EMA Alignment strategy requires ema_20, ema_fast, ema_slow, rsi columns")
    }

    dt[, close_prev := shift(close, 1)]
    dt[, high_prev := shift(high, 1)]
    dt[, low_prev := shift(low, 1)]

    # Check alignment
    dt[, bullish_aligned := ema_20 > ema_fast & ema_fast > ema_slow]
    dt[, bearish_aligned := ema_20 < ema_fast & ema_fast < ema_slow]

    # Pullback detection: price touched EMA 20 recently
    dt[, touched_ema20_long := low <= ema_20 * 1.002 & close > ema_20]
    dt[, touched_ema20_short := high >= ema_20 * 0.998 & close < ema_20]

    # Rolling check: was there a pullback in last 5 bars?
    dt[, pullback_long := frollapply(touched_ema20_long, 5, any)]
    dt[, pullback_short := frollapply(touched_ema20_short, 5, any)]

    dt[, primary_signal := fcase(
      # LONG: All EMAs aligned bullish + pullback to EMA20 + RSI > 40 + break of previous high
      bullish_aligned == TRUE &
        pullback_long == TRUE &
        rsi > 40 &
        close > high_prev, 1L,

      # SHORT: All EMAs aligned bearish + pullback to EMA20 + RSI < 60 + break of previous low
      bearish_aligned == TRUE &
        pullback_short == TRUE &
        rsi < 60 &
        close < low_prev, -1L,

      default = NA_integer_
    )]

    dt[, c("close_prev", "high_prev", "low_prev", "bullish_aligned", "bearish_aligned",
           "touched_ema20_long", "touched_ema20_short", "pullback_long", "pullback_short") := NULL]

  # =========================================================================
  # STRATEGY 8: RSI BREAKOUT (Original)
  # =========================================================================
  } else if (method == "rsi_breakout") {

    dt[, rsi_prev := shift(rsi, 1)]

    dt[, primary_signal := fcase(
      rsi_prev < p$rsi_oversold & rsi >= p$rsi_oversold, 1L,
      rsi_prev > p$rsi_overbought & rsi <= p$rsi_overbought, -1L,
      default = NA_integer_
    )]

    dt[, rsi_prev := NULL]

  # =========================================================================
  # STRATEGY 9: SIMPLE MACD (Original)
  # =========================================================================
  } else if (method == "macd") {

    dt[, macd_prev := shift(macd, 1)]
    dt[, macd_signal_prev := shift(macd_signal, 1)]

    dt[, primary_signal := fcase(
      macd_prev < macd_signal_prev & macd >= macd_signal, 1L,
      macd_prev > macd_signal_prev & macd <= macd_signal, -1L,
      default = NA_integer_
    )]

    dt[, c("macd_prev", "macd_signal_prev") := NULL]

  } else {
    available <- paste(names(list_primary_signal_strategies()), collapse = ", ")
    stop(sprintf("Unknown primary signal method: %s\nAvailable: %s", method, available))
  }

  # Count signals
  n_long <- sum(dt$primary_signal == 1, na.rm = TRUE)
  n_short <- sum(dt$primary_signal == -1, na.rm = TRUE)
  n_total <- n_long + n_short

  cat(sprintf("  Signals generated: %d total (Long=%d, Short=%d)\n",
              n_total, n_long, n_short))

  if (n_total == 0) {
    warning("No signals generated! Check strategy parameters or data quality.")
  }

  return(dt)
}


# =============================================================================
# DYNAMIC TRIPLE BARRIER LABELING
# =============================================================================

#' Get session-based volatility multiplier
#'
#' @param hour Hour of day (0-23)
#' @param session_vol_multipliers List of multipliers by session
#' @return Volatility multiplier
get_session_volatility_multiplier <- function(hour, session_vol_multipliers) {
  session <- fcase(
    hour >= 1 & hour < 8, "asia",
    hour >= 8 & hour < 13, "london",
    hour >= 13 & hour < 17, "overlap",
    hour >= 17 & hour < 22, "ny",
    default = "default"
  )
  return(session_vol_multipliers[[session]] %||% 1.0)
}


#' Apply dynamic triple barrier labeling with meta-labels
#'
#' Key differences from standard triple barrier:
#' - Uses PRIMARY SIGNAL to determine direction (not symmetric)
#' - TP/SL barriers are direction-aware
#' - Meta-label = 1 if TP hit, 0 if SL or timeout
#' - Barriers dynamically scaled by intraday volatility
#'
#' @param dt data.table with prices, indicators, and primary_signal
#' @param atr_mult_tp ATR multiplier for take profit
#' @param atr_mult_sl ATR multiplier for stop loss
#' @param max_horizon Maximum bars to hold
#' @param session_start Session start hour
#' @param session_end Session end hour
#' @param session_vol_multipliers Volatility multipliers by session
#' @param min_barrier_distance Minimum barrier distance (spread protection)
#' @param neutral_threshold ATR multiplier for neutral classification
#' @return data.table with meta-labels and barrier info
apply_dynamic_triple_barrier <- function(
    dt,
    atr_mult_tp = 2.5,
    atr_mult_sl = 2.0,
    max_horizon = 16,
    session_start = 1,
    session_end = 22,
    session_vol_multipliers = list(asia = 0.8, london = 1.2, overlap = 1.4, ny = 1.1, default = 1.0),
    min_barrier_distance = 0.0005,
    neutral_threshold = 1.5
) {

  dt <- copy(dt)
  n <- nrow(dt)

  cat(sprintf("Applying dynamic triple barrier to %s observations...\n",
              format(n, big.mark = ",")))

  # Session filter
  dt[, in_session := hour >= session_start & hour < session_end]

  # Bars until session end
  dt[, bars_until_session_end := {
    session_end_time <- as.POSIXct(paste(date, sprintf("%02d:00:00", session_end)))
    pmax(0, as.numeric(difftime(session_end_time, datetime, units = "mins")) / 15)
  }]

  # Pre-allocate result vectors
  meta_label <- rep(NA_integer_, n)
  barrier_touched <- rep(NA_character_, n)
  bars_to_exit <- rep(NA_integer_, n)
  realized_return <- rep(NA_real_, n)
  tp_distance <- rep(NA_real_, n)
  sl_distance <- rep(NA_real_, n)

  # Extract vectors for speed
  close_vec <- dt$close
  high_vec <- dt$high
  low_vec <- dt$low
  open_vec <- dt$open
  atr_vec <- dt$atr
  hour_vec <- dt$hour
  signal_vec <- dt$primary_signal
  in_session_vec <- dt$in_session
  bars_until_end_vec <- dt$bars_until_session_end

  # Valid indices: has signal, in session, valid ATR, enough time
  valid_mask <- !is.na(signal_vec) & signal_vec != 0 &
                in_session_vec & !is.na(atr_vec) & atr_vec > 0 &
                bars_until_end_vec >= 4

  valid_indices <- which(valid_mask)
  n_valid <- length(valid_indices)

  cat(sprintf("Processing %d valid signal observations...\n", n_valid))

  # Progress bar
  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = n_valid,
    clear = FALSE
  )

  for (idx in seq_along(valid_indices)) {
    pb$tick()

    i <- valid_indices[idx]
    entry_price <- close_vec[i]
    current_atr <- atr_vec[i]
    direction <- signal_vec[i]  # 1 = long, -1 = short
    current_hour <- hour_vec[i]

    # Dynamic volatility multiplier based on session
    vol_mult <- get_session_volatility_multiplier(current_hour, session_vol_multipliers)

    # Calculate barrier distances with volatility adjustment
    tp_dist <- max(atr_mult_tp * current_atr * vol_mult, min_barrier_distance)
    sl_dist <- max(atr_mult_sl * current_atr * vol_mult, min_barrier_distance)

    tp_distance[i] <- tp_dist
    sl_distance[i] <- sl_dist

    # Direction-aware barriers
    if (direction == 1) {  # Long
      tp_price <- entry_price + tp_dist
      sl_price <- entry_price - sl_dist
    } else {  # Short
      tp_price <- entry_price - tp_dist
      sl_price <- entry_price + sl_dist
    }

    # Effective horizon (session-bounded)
    eff_horizon <- min(max_horizon, floor(bars_until_end_vec[i]) - 1)
    if (eff_horizon < 1) next

    # Look for barrier hits
    end_idx <- min(i + eff_horizon, n)
    hit_tp <- FALSE
    hit_sl <- FALSE
    exit_bar <- NA
    exit_price <- NA

    for (j in 1:eff_horizon) {
      check_idx <- i + j
      if (check_idx > n) break

      bar_high <- high_vec[check_idx]
      bar_low <- low_vec[check_idx]
      bar_close <- close_vec[check_idx]

      # Check barriers based on direction
      if (direction == 1) {  # Long
        check_tp <- bar_high >= tp_price
        check_sl <- bar_low <= sl_price
      } else {  # Short
        check_tp <- bar_low <= tp_price
        check_sl <- bar_high >= sl_price
      }

      # Handle same-bar TP/SL hits conservatively (assume SL first)
      if (check_tp && check_sl) {
        hit_sl <- TRUE
        exit_bar <- j
        exit_price <- sl_price
        break
      } else if (check_sl) {
        hit_sl <- TRUE
        exit_bar <- j
        exit_price <- sl_price
        break
      } else if (check_tp) {
        hit_tp <- TRUE
        exit_bar <- j
        exit_price <- tp_price
        break
      }

      # Session end check
      if (!in_session_vec[check_idx]) {
        exit_bar <- j
        exit_price <- bar_close
        break
      }
    }

    # Handle timeout (vertical barrier)
    if (is.na(exit_bar)) {
      exit_bar <- eff_horizon
      exit_price <- close_vec[min(i + eff_horizon, n)]
    }

    # Calculate realized return
    if (direction == 1) {
      ret <- (exit_price - entry_price) / entry_price
    } else {
      ret <- (entry_price - exit_price) / entry_price
    }

    # Assign meta-label
    # 1 = TP hit (success), 0 = SL hit or timeout (failure)
    if (hit_tp) {
      meta_label[i] <- 1L
      barrier_touched[i] <- "take_profit"
    } else if (hit_sl) {
      meta_label[i] <- 0L
      barrier_touched[i] <- "stop_loss"
    } else {
      # Timeout - classify based on return magnitude
      neutral_dist <- neutral_threshold * current_atr / entry_price
      if (abs(ret) < neutral_dist) {
        meta_label[i] <- 0L  # Too small move = failure
        barrier_touched[i] <- "timeout_neutral"
      } else if (ret > 0) {
        meta_label[i] <- 1L  # Positive timeout = success
        barrier_touched[i] <- "timeout_positive"
      } else {
        meta_label[i] <- 0L  # Negative timeout = failure
        barrier_touched[i] <- "timeout_negative"
      }
    }

    bars_to_exit[i] <- exit_bar
    realized_return[i] <- ret
  }

  # Add results to data.table
  dt[, `:=`(
    meta_label = meta_label,
    barrier_touched = barrier_touched,
    bars_to_exit = bars_to_exit,
    realized_return = realized_return,
    tp_distance = tp_distance,
    sl_distance = sl_distance
  )]

  # Calculate adjusted return (after spread + slippage)
  spread_cost <- 0.00013  # Typical Gold spread
  slippage_cost <- 0.0001  # ~1 pip slippage
  dt[, realized_return_adj := realized_return - spread_cost - slippage_cost]

  # Filter to labeled observations only
  dt_labeled <- dt[!is.na(meta_label)]

  # Statistics
  cat(sprintf("\n=== LABELING RESULTS ===\n"))
  cat(sprintf("Total labeled: %d\n", nrow(dt_labeled)))
  cat(sprintf("Meta-label 1 (TP): %d (%.1f%%)\n",
              sum(dt_labeled$meta_label == 1),
              mean(dt_labeled$meta_label == 1) * 100))
  cat(sprintf("Meta-label 0 (SL/TO): %d (%.1f%%)\n",
              sum(dt_labeled$meta_label == 0),
              mean(dt_labeled$meta_label == 0) * 100))

  cat("\nBarrier touch distribution:\n")
  print(table(dt_labeled$barrier_touched))

  cat(sprintf("\nMean holding period: %.1f bars\n", mean(dt_labeled$bars_to_exit)))
  cat(sprintf("Mean realized return: %.4f%%\n", mean(dt_labeled$realized_return) * 100))
  cat(sprintf("Mean adjusted return: %.4f%%\n", mean(dt_labeled$realized_return_adj) * 100))

  return(dt_labeled)
}


# =============================================================================
# LEGACY LABEL CONVERSION (for compatibility)
# =============================================================================

#' Convert meta-labels to traditional labels (-1, 0, 1)
#'
#' For backward compatibility with scripts expecting traditional labels.
#'
#' @param dt data.table with meta_label and primary_signal
#' @return data.table with added 'label' column
convert_to_traditional_labels <- function(dt) {
  dt <- copy(dt)

  # Traditional label = primary_signal * meta_label
  # If meta_label = 1 (success), keep the direction
  # If meta_label = 0 (failure), label = 0 (neutral)
  dt[, label := fifelse(meta_label == 1, primary_signal, 0L)]

  cat("Converted to traditional labels:\n")
  print(table(dt$label))

 return(dt)
}


cat("\n=== META-LABELING CORE MODULE LOADED ===\n")
cat("Functions:\n")
cat("  - calculate_technical_indicators()\n")
cat("  - generate_primary_signals()\n")
cat("  - apply_dynamic_triple_barrier()\n")
cat("  - convert_to_traditional_labels()\n\n")
