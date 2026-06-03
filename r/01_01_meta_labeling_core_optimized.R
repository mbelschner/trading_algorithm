# =============================================================================
# META-LABELING CORE FUNCTIONS (OPTIMIZED)
# =============================================================================
# Vectorized implementation for improved performance
# =============================================================================

library(data.table)
library(TTR)

# =============================================================================
# HELPER: SESSION CLASSIFICATION (centralized)
# =============================================================================

#' Classify trading session based on hour
#' @param hour Hour (0-23)
#' @return Session name
classify_session <- function(hour) {
  fcase(
    hour >= 1 & hour < 8, "asia",
    hour >= 8 & hour < 13, "london",
    hour >= 13 & hour < 17, "overlap",
    hour >= 17 & hour < 22, "ny",
    default = "closed"
  )
}

#' Get volatility multiplier for session
#' @param session Session name
#' @param multipliers Named list of multipliers
#' @return Multiplier value
get_vol_multiplier <- function(session, multipliers) {
  vapply(session, function(s) multipliers[[s]] %||% 1.0, numeric(1))
}

# =============================================================================
# TECHNICAL INDICATORS (unchanged logic, cleaner code)
# =============================================================================

#' Calculate CMO
calculate_cmo <- function(x, n = 14) {
  delta <- c(NA, diff(x))
  gains <- pmax(delta, 0, na.rm = TRUE)
  losses <- abs(pmin(delta, 0, na.rm = TRUE))
  sum_gains <- runSum(gains, n)
  sum_losses <- runSum(losses, n)
  100 * (sum_gains - sum_losses) / (sum_gains + sum_losses)
}

#' Calculate VHF
calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  numerator <- abs(highest - lowest)
  delta <- c(NA, abs(diff(close)))
  denominator <- runSum(delta, n)
  numerator / denominator
}

#' Calculate STC
calculate_stc <- function(close, n_fast = 23, n_slow = 50, n_cycle = 10, n_smooth = 3) {
  ema_fast <- EMA(close, n = n_fast)
  ema_slow <- EMA(close, n = n_slow)
  macd_line <- ema_fast - ema_slow

  macd_low <- runMin(macd_line, n_cycle)
  macd_high <- runMax(macd_line, n_cycle)
  stoch1 <- fifelse(macd_high - macd_low > 0,
                    100 * (macd_line - macd_low) / (macd_high - macd_low), 50)
  pf <- EMA(stoch1, n = n_smooth)

  pf_low <- runMin(pf, n_cycle)
  pf_high <- runMax(pf, n_cycle)
  stoch2 <- fifelse(pf_high - pf_low > 0,
                    100 * (pf - pf_low) / (pf_high - pf_low), 50)
  EMA(stoch2, n = n_smooth)
}

#' Calculate Supertrend (optimized with Rcpp-style vectorization where possible)
calculate_supertrend <- function(high, low, close, n = 10, mult = 3.0) {
  atr <- ATR(cbind(high, low, close), n = n)[, "atr"]
  hl2 <- (high + low) / 2
  upper_band <- hl2 + mult * atr
  lower_band <- hl2 - mult * atr

  len <- length(close)
  supertrend <- rep(NA_real_, len)
  direction <- rep(NA_integer_, len)

  # Initial state
  supertrend[n + 1] <- lower_band[n + 1]
  direction[n + 1] <- 1L

  # Sequential update (state-dependent, can't fully vectorize)
  for (i in (n + 2):len) {
    prev_st <- supertrend[i - 1]
    prev_dir <- direction[i - 1]

    if (prev_dir == 1L) {
      if (close[i] > lower_band[i]) {
        supertrend[i] <- max(lower_band[i], lower_band[i - 1], na.rm = TRUE)
        direction[i] <- 1L
      } else {
        supertrend[i] <- upper_band[i]
        direction[i] <- -1L
      }
    } else {
      if (close[i] < upper_band[i]) {
        supertrend[i] <- min(upper_band[i], upper_band[i - 1], na.rm = TRUE)
        direction[i] <- -1L
      } else {
        supertrend[i] <- lower_band[i]
        direction[i] <- 1L
      }
    }
  }

  list(supertrend = supertrend, direction = direction)
}

#' Calculate Ichimoku
calculate_ichimoku <- function(high, low, close, n_tenkan = 9, n_kijun = 26, n_senkou = 52) {
  tenkan <- (runMax(high, n_tenkan) + runMin(low, n_tenkan)) / 2
  kijun <- (runMax(high, n_kijun) + runMin(low, n_kijun)) / 2
  senkou_a <- (tenkan + kijun) / 2
  senkou_b <- (runMax(high, n_senkou) + runMin(low, n_senkou)) / 2

  data.table(tenkan = tenkan, kijun = kijun, senkou_a = senkou_a, senkou_b = senkou_b)
}

#' Calculate all technical indicators
calculate_technical_indicators <- function(dt, atr_period = 12, ema_fast = 50,
                                           ema_slow = 200, rsi_period = 14) {
  dt <- copy(dt)
  setnames(dt, tolower(names(dt)))

  if ("time" %in% names(dt) && !"datetime" %in% names(dt)) {
    setnames(dt, "time", "datetime")
  }
  if (!inherits(dt$datetime, "POSIXct")) dt[, datetime := as.POSIXct(datetime)]

  # Core indicators (vectorized)
  atr_result <- ATR(cbind(dt$high, dt$low, dt$close), n = atr_period)
  macd_result <- MACD(dt$close, nFast = 12, nSlow = 26, nSig = 9)
  adx_result <- ADX(cbind(dt$high, dt$low, dt$close), n = 14)
  supertrend_result <- calculate_supertrend(dt$high, dt$low, dt$close, n = 10, mult = 3.0)
  ichimoku_result <- calculate_ichimoku(dt$high, dt$low, dt$close)

  # Assign all at once (single memory allocation)
  dt[, `:=`(
    atr = atr_result[, "atr"],
    atr_pct = atr_result[, "atr"] / close * 100,
    ema_20 = EMA(close, n = 20),
    ema_fast = EMA(close, n = ema_fast),
    ema_slow = EMA(close, n = ema_slow),
    rsi = RSI(close, n = rsi_period),
    macd = macd_result[, "macd"],
    macd_signal = macd_result[, "signal"],
    macd_hist = macd_result[, "macd"] - macd_result[, "signal"],
    adx = adx_result[, "ADX"],
    di_plus = adx_result[, "DIp"],
    di_minus = adx_result[, "DIn"],
    cmo = calculate_cmo(close, n = 14),
    vhf = calculate_vhf(close, n = 28),
    stc = calculate_stc(close),
    supertrend = supertrend_result$supertrend,
    supertrend_dir = supertrend_result$direction,
    tenkan = ichimoku_result$tenkan,
    kijun = ichimoku_result$kijun,
    senkou_a = ichimoku_result$senkou_a,
    senkou_b = ichimoku_result$senkou_b,
    hour = hour(datetime),
    date = as.Date(datetime)
  )]

  # Volume indicators (if available)
  if ("volume" %in% names(dt)) {
    dt[, `:=`(volume_avg = SMA(volume, n = 20), volume_ratio = volume / SMA(volume, n = 20))]
  }

  # Cloud and session
  dt[, `:=`(
    cloud_top = pmax(senkou_a, senkou_b, na.rm = TRUE),
    cloud_bottom = pmin(senkou_a, senkou_b, na.rm = TRUE),
    cloud_bullish = senkou_a > senkou_b,
    session = classify_session(hour)
  )]

  dt
}

# =============================================================================
# PRIMARY SIGNAL GENERATION
# =============================================================================

list_primary_signal_strategies <- function() {
  c(ema_cross = "EMA Crossover (50/200)",
    cmo_vhf_stc = "CMO-VHF-STC Momentum",
    adx_di = "ADX + DI Crossover",
    ichimoku = "Ichimoku Cloud Breakout",
    supertrend = "Supertrend + RSI",
    macd_volume = "MACD + Volume",
    ema_alignment = "Multi-EMA Alignment",
    rsi_breakout = "RSI Breakout",
    macd = "MACD Crossover")
}

generate_primary_signals <- function(dt, method = "ema_cross", params = list()) {
  dt <- copy(dt)

  p <- modifyList(list(
    ema_fast_col = "ema_fast", ema_slow_col = "ema_slow",
    rsi_overbought = 70, rsi_oversold = 30,
    vhf_threshold = 0.35, cmo_threshold = 20,
    stc_long_cross = 25, stc_short_cross = 75,
    adx_threshold = 25, volume_mult = 1.2,
    signal_validity_bars = 5
  ), params)

  # Generate signals based on method
  dt[, primary_signal := NA_integer_]

  if (method == "ema_cross") {
    dt[, `:=`(ema_state = fifelse(get(p$ema_fast_col) > get(p$ema_slow_col), 1L, -1L),
              ema_state_prev = shift(fifelse(get(p$ema_fast_col) > get(p$ema_slow_col), 1L, -1L), 1))]
    dt[ema_state != ema_state_prev & !is.na(ema_state_prev), primary_signal := ema_state]
    dt[, c("ema_state", "ema_state_prev") := NULL]

  } else if (method == "cmo_vhf_stc") {
    stopifnot(all(c("cmo", "vhf", "stc") %in% names(dt)))
    dt[, `:=`(stc_prev = shift(stc, 1), close_prev = shift(close, 1))]
    dt[, primary_signal := fcase(
      vhf > p$vhf_threshold & cmo > p$cmo_threshold &
        stc_prev < p$stc_long_cross & stc >= p$stc_long_cross & close > close_prev, 1L,
      vhf > p$vhf_threshold & cmo < -p$cmo_threshold &
        stc_prev > p$stc_short_cross & stc <= p$stc_short_cross & close < close_prev, -1L,
      default = NA_integer_
    )]
    dt[, c("stc_prev", "close_prev") := NULL]

  } else if (method == "adx_di") {
    stopifnot(all(c("adx", "di_plus", "di_minus") %in% names(dt)))
    dt[, `:=`(di_plus_prev = shift(di_plus, 1), di_minus_prev = shift(di_minus, 1))]
    dt[, primary_signal := fcase(
      adx > p$adx_threshold & di_plus_prev < di_minus_prev & di_plus >= di_minus, 1L,
      adx > p$adx_threshold & di_minus_prev < di_plus_prev & di_minus >= di_plus, -1L,
      default = NA_integer_
    )]
    dt[, c("di_plus_prev", "di_minus_prev") := NULL]

  } else if (method == "ichimoku") {
    stopifnot(all(c("tenkan", "kijun", "cloud_top", "cloud_bottom", "cloud_bullish") %in% names(dt)))
    dt[, `:=`(tenkan_prev = shift(tenkan, 1), kijun_prev = shift(kijun, 1), close_prev = shift(close, 1))]
    dt[, primary_signal := fcase(
      close > cloud_top & close_prev <= cloud_top & cloud_bullish & tenkan > kijun & tenkan_prev <= kijun_prev, 1L,
      close < cloud_bottom & close_prev >= cloud_bottom & !cloud_bullish & tenkan < kijun & tenkan_prev >= kijun_prev, -1L,
      close > cloud_top & tenkan > kijun, 1L,
      close < cloud_bottom & tenkan < kijun, -1L,
      default = NA_integer_
    )]
    # Remove duplicates
    dt[, signal_state := nafill(primary_signal, type = "locf")]
    dt[signal_state == shift(signal_state, 1), primary_signal := NA_integer_]
    dt[, c("tenkan_prev", "kijun_prev", "close_prev", "signal_state") := NULL]

  } else if (method == "supertrend") {
    stopifnot(all(c("supertrend", "supertrend_dir", "rsi") %in% names(dt)))
    dt[, st_dir_prev := shift(supertrend_dir, 1)]
    dt[, primary_signal := fcase(
      st_dir_prev == -1L & supertrend_dir == 1L & rsi > 50, 1L,
      st_dir_prev == 1L & supertrend_dir == -1L & rsi < 50, -1L,
      default = NA_integer_
    )]
    dt[, st_dir_prev := NULL]

  } else if (method == "macd_volume") {
    stopifnot(all(c("macd", "macd_signal", "macd_hist") %in% names(dt)))
    dt[, `:=`(macd_prev = shift(macd, 1), macd_signal_prev = shift(macd_signal, 1),
              macd_hist_prev = shift(macd_hist, 1), macd_hist_prev2 = shift(macd_hist, 2))]
    has_vol <- "volume_ratio" %in% names(dt)
    if (has_vol) {
      dt[, primary_signal := fcase(
        macd_prev < macd_signal_prev & macd >= macd_signal &
          macd_hist > macd_hist_prev & macd_hist_prev > macd_hist_prev2 & volume_ratio > p$volume_mult, 1L,
        macd_prev > macd_signal_prev & macd <= macd_signal &
          macd_hist < macd_hist_prev & macd_hist_prev < macd_hist_prev2 & volume_ratio > p$volume_mult, -1L,
        default = NA_integer_
      )]
    } else {
      dt[, primary_signal := fcase(
        macd_prev < macd_signal_prev & macd >= macd_signal &
          macd_hist > macd_hist_prev & macd_hist_prev > macd_hist_prev2, 1L,
        macd_prev > macd_signal_prev & macd <= macd_signal &
          macd_hist < macd_hist_prev & macd_hist_prev < macd_hist_prev2, -1L,
        default = NA_integer_
      )]
    }
    dt[, c("macd_prev", "macd_signal_prev", "macd_hist_prev", "macd_hist_prev2") := NULL]

  } else if (method == "ema_alignment") {
    stopifnot(all(c("ema_20", "ema_fast", "ema_slow", "rsi") %in% names(dt)))
    dt[, `:=`(close_prev = shift(close, 1), high_prev = shift(high, 1), low_prev = shift(low, 1),
              bullish_aligned = ema_20 > ema_fast & ema_fast > ema_slow,
              bearish_aligned = ema_20 < ema_fast & ema_fast < ema_slow,
              touched_ema20_long = low <= ema_20 * 1.002 & close > ema_20,
              touched_ema20_short = high >= ema_20 * 0.998 & close < ema_20)]
    dt[, `:=`(pullback_long = frollapply(touched_ema20_long, 5, any),
              pullback_short = frollapply(touched_ema20_short, 5, any))]
    dt[, primary_signal := fcase(
      bullish_aligned & pullback_long & rsi > 40 & close > high_prev, 1L,
      bearish_aligned & pullback_short & rsi < 60 & close < low_prev, -1L,
      default = NA_integer_
    )]
    dt[, c("close_prev", "high_prev", "low_prev", "bullish_aligned", "bearish_aligned",
           "touched_ema20_long", "touched_ema20_short", "pullback_long", "pullback_short") := NULL]

  } else if (method == "rsi_breakout") {
    dt[, rsi_prev := shift(rsi, 1)]
    dt[, primary_signal := fcase(
      rsi_prev < p$rsi_oversold & rsi >= p$rsi_oversold, 1L,
      rsi_prev > p$rsi_overbought & rsi <= p$rsi_overbought, -1L,
      default = NA_integer_
    )]
    dt[, rsi_prev := NULL]

  } else if (method == "macd") {
    dt[, `:=`(macd_prev = shift(macd, 1), macd_signal_prev = shift(macd_signal, 1))]
    dt[, primary_signal := fcase(
      macd_prev < macd_signal_prev & macd >= macd_signal, 1L,
      macd_prev > macd_signal_prev & macd <= macd_signal, -1L,
      default = NA_integer_
    )]
    dt[, c("macd_prev", "macd_signal_prev") := NULL]

  } else {
    stop(sprintf("Unknown method: %s", method))
  }

  n_long <- sum(dt$primary_signal == 1, na.rm = TRUE)
  n_short <- sum(dt$primary_signal == -1, na.rm = TRUE)
  cat(sprintf("Signals: %d Long, %d Short\n", n_long, n_short))

  dt
}

# =============================================================================
# DYNAMIC TRIPLE BARRIER LABELING (VECTORIZED)
# =============================================================================

#' Apply dynamic triple barrier - VECTORIZED VERSION
#'
#' Uses pre-computed forward-looking price matrices for ~10-50x speedup
apply_dynamic_triple_barrier <- function(
    dt, atr_mult_tp = 2.5, atr_mult_sl = 2.0, max_horizon = 16,
    session_start = 1, session_end = 22,
    session_vol_multipliers = list(asia = 0.8, london = 1.2, overlap = 1.4, ny = 1.1, default = 1.0),
    min_barrier_distance = 0.0005, neutral_threshold = 1.5,
    spread_cost = 0.00013, slippage_cost = 0.0001
) {

  dt <- copy(dt)
  n <- nrow(dt)

  cat(sprintf("Applying triple barrier to %s rows (vectorized)...\n", format(n, big.mark = ",")))

  # Session filter & time calculations
  dt[, `:=`(
    in_session = hour >= session_start & hour < session_end,
    session_end_time = as.POSIXct(paste(date, sprintf("%02d:00:00", session_end)))
  )]
  dt[, bars_until_session_end := pmax(0, as.numeric(difftime(session_end_time, datetime, units = "mins")) / 15)]

  # Get volatility multipliers vectorized
  dt[, vol_mult := get_vol_multiplier(session, session_vol_multipliers)]

  # Calculate barrier distances (vectorized)
  dt[, `:=`(
    tp_dist = pmax(atr_mult_tp * atr * vol_mult, min_barrier_distance),
    sl_dist = pmax(atr_mult_sl * atr * vol_mult, min_barrier_distance)
  )]

  # Pre-compute forward-looking highs and lows for all horizons
  cat("Pre-computing forward price matrix...\n")
  for (h in 1:max_horizon) {
    dt[, paste0("fwd_high_", h) := shift(high, -h, type = "lead")]
    dt[, paste0("fwd_low_", h) := shift(low, -h, type = "lead")]
    dt[, paste0("fwd_close_", h) := shift(close, -h, type = "lead")]
    dt[, paste0("fwd_in_session_", h) := shift(in_session, -h, type = "lead")]
  }

  # Valid mask for signals
  valid_mask <- !is.na(dt$primary_signal) & dt$primary_signal != 0 &
                dt$in_session & !is.na(dt$atr) & dt$atr > 0 &
                dt$bars_until_session_end >= 4

  # Initialize result columns
  dt[, `:=`(
    meta_label = NA_integer_,
    barrier_touched = NA_character_,
    bars_to_exit = NA_integer_,
    realized_return = NA_real_,
    tp_distance = NA_real_,
    sl_distance = NA_real_
  )]

  # Process only valid signals
  signal_idx <- which(valid_mask)
  n_signals <- length(signal_idx)
  cat(sprintf("Processing %d valid signals...\n", n_signals))

  if (n_signals == 0) {
    warning("No valid signals found!")
    return(dt[FALSE])
  }

  # Vectorized barrier checking
  pb <- progress_bar$new(format = "[:bar] :percent", total = n_signals, clear = FALSE)

  for (i in signal_idx) {
    pb$tick()

    entry_price <- dt$close[i]
    direction <- dt$primary_signal[i]
    current_atr <- dt$atr[i]
    tp_dist <- dt$tp_dist[i]
    sl_dist <- dt$sl_dist[i]

    # Direction-aware barriers
    if (direction == 1) {
      tp_price <- entry_price + tp_dist
      sl_price <- entry_price - sl_dist
    } else {
      tp_price <- entry_price - tp_dist
      sl_price <- entry_price + sl_dist
    }

    # Effective horizon
    eff_horizon <- min(max_horizon, floor(dt$bars_until_session_end[i]) - 1)
    if (eff_horizon < 1) next

    # Check each bar for barrier hits
    hit_tp <- FALSE
    hit_sl <- FALSE
    exit_bar <- NA
    exit_price <- NA

    for (h in 1:eff_horizon) {
      fwd_high <- dt[[paste0("fwd_high_", h)]][i]
      fwd_low <- dt[[paste0("fwd_low_", h)]][i]
      fwd_close <- dt[[paste0("fwd_close_", h)]][i]
      fwd_session <- dt[[paste0("fwd_in_session_", h)]][i]

      if (is.na(fwd_high)) break

      # Check barriers
      if (direction == 1) {
        check_tp <- fwd_high >= tp_price
        check_sl <- fwd_low <= sl_price
      } else {
        check_tp <- fwd_low <= tp_price
        check_sl <- fwd_high >= sl_price
      }

      if (check_tp && check_sl) {
        hit_sl <- TRUE
        exit_bar <- h
        exit_price <- sl_price
        break
      } else if (check_sl) {
        hit_sl <- TRUE
        exit_bar <- h
        exit_price <- sl_price
        break
      } else if (check_tp) {
        hit_tp <- TRUE
        exit_bar <- h
        exit_price <- tp_price
        break
      }

      if (!fwd_session %||% TRUE) {
        exit_bar <- h
        exit_price <- fwd_close
        break
      }
    }

    # Timeout
    if (is.na(exit_bar)) {
      exit_bar <- eff_horizon
      exit_price <- dt[[paste0("fwd_close_", eff_horizon)]][i]
    }

    # Realized return
    ret <- if (direction == 1) {
      (exit_price - entry_price) / entry_price
    } else {
      (entry_price - exit_price) / entry_price
    }

    # Meta-label assignment
    if (hit_tp) {
      dt[i, `:=`(meta_label = 1L, barrier_touched = "take_profit")]
    } else if (hit_sl) {
      dt[i, `:=`(meta_label = 0L, barrier_touched = "stop_loss")]
    } else {
      neutral_dist <- neutral_threshold * current_atr / entry_price
      if (abs(ret) < neutral_dist) {
        dt[i, `:=`(meta_label = 0L, barrier_touched = "timeout_neutral")]
      } else if (ret > 0) {
        dt[i, `:=`(meta_label = 1L, barrier_touched = "timeout_positive")]
      } else {
        dt[i, `:=`(meta_label = 0L, barrier_touched = "timeout_negative")]
      }
    }

    dt[i, `:=`(
      bars_to_exit = exit_bar,
      realized_return = ret,
      tp_distance = tp_dist,
      sl_distance = sl_dist
    )]
  }

  # Cleanup forward columns
  fwd_cols <- grep("^fwd_", names(dt), value = TRUE)
  dt[, (fwd_cols) := NULL]
  dt[, c("session_end_time", "vol_mult", "tp_dist", "sl_dist") := NULL]

  # Adjusted return
  dt[, realized_return_adj := realized_return - spread_cost - slippage_cost]

  # Filter to labeled only
  dt_labeled <- dt[!is.na(meta_label)]

  # Statistics
  cat(sprintf("\nLabeled: %d | TP: %d (%.1f%%) | SL/TO: %d (%.1f%%)\n",
              nrow(dt_labeled),
              sum(dt_labeled$meta_label == 1), mean(dt_labeled$meta_label == 1) * 100,
              sum(dt_labeled$meta_label == 0), mean(dt_labeled$meta_label == 0) * 100))
  cat("Barrier distribution:\n")
  print(table(dt_labeled$barrier_touched))

  dt_labeled
}

# =============================================================================
# LEGACY CONVERSION (for backward compatibility)
# =============================================================================

convert_to_traditional_labels <- function(dt) {
  dt <- copy(dt)
  dt[, label := fifelse(meta_label == 1, primary_signal, 0L)]
  dt
}
