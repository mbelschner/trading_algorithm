# Technical Indicator Calculation Module
# Berechnet EMA, RSI, ADX, ATR, DPO, Keltner, Bollinger, McGinley, Ichimoku,
# Aroon, Momentum, Choppiness, Donchian, Parabolic SAR, Volume Ratio, VHF, ROC

#' Calculate all technical indicators
#'
#' @param dt data.table with OHLCV data (open, high, low, close, volume)
#' @param ema_periods Vector of EMA periods (default: c(9, 21, 50, 100, 200))
#' @param rsi_periods Vector of RSI periods (default: c(14, 28))
#' @param atr_periods Vector of ATR periods (default: c(14, 28))
#' @param adx_periods Vector of ADX periods (default: c(14))
#' @param bb_periods Vector of Bollinger Band periods (default: c(20))
#' @param kc_periods Vector of Keltner Channel periods (default: c(20))
#' @param verbose Print progress messages
#'
#' @return data.table with all original columns plus indicator columns
calculate_all_indicators <- function(
    dt,
    ema_periods = c(9, 21, 50, 100, 200),
    rsi_periods = c(14, 28),
    atr_periods = c(14, 28),
    adx_periods = c(14),
    bb_periods = c(20),
    kc_periods = c(20),
    verbose = TRUE
) {

  if (verbose) cat("Berechne technische Indikatoren...\n")

  # Kopiere Datensatz
  dt_ind <- copy(dt)

  # Validiere erforderliche Spalten
  required_cols <- c("open", "high", "low", "close", "volume")
  missing_cols <- setdiff(required_cols, names(dt_ind))
  if (length(missing_cols) > 0) {
    stop("Fehlende Spalten: ", paste(missing_cols, collapse = ", "))
  }

  # Basis-Preis-Features
  dt_ind[, `:=`(
    typical_price = (high + low + close) / 3,
    hl_range = high - low,
    oc_range = abs(close - open),
    body_pct = abs(close - open) / (high - low + 1e-10)
  )]

  # ===== 1. EMA (Exponential Moving Average) =====
  if (verbose) cat("  - EMA (", length(ema_periods), " Perioden)\n", sep = "")
  for (period in ema_periods) {
    col_name <- paste0("ema_", period)
    dt_ind[, (col_name) := EMA(close, n = period)]

    # EMA Slope (Momentum)
    slope_col <- paste0("ema_", period, "_slope")
    dt_ind[, (slope_col) := c(NA, diff(get(col_name)))]

    # Distance to EMA
    dist_col <- paste0("dist_ema_", period)
    dt_ind[, (dist_col) := (close - get(col_name)) / get(col_name)]
  }

  # EMA Crossovers (wichtige Signale)
  if (9 %in% ema_periods && 21 %in% ema_periods) {
    dt_ind[, ema_9_21_cross := sign(ema_9 - ema_21)]
  }
  if (21 %in% ema_periods && 50 %in% ema_periods) {
    dt_ind[, ema_21_50_cross := sign(ema_21 - ema_50)]
  }

  # ===== 2. RSI (Relative Strength Index) =====
  if (verbose) cat("  - RSI (", length(rsi_periods), " Perioden)\n", sep = "")
  for (period in rsi_periods) {
    col_name <- paste0("rsi_", period)
    dt_ind[, (col_name) := RSI(close, n = period)]

    # RSI Momentum
    rsi_slope_col <- paste0("rsi_", period, "_slope")
    dt_ind[, (rsi_slope_col) := c(NA, diff(get(col_name)))]
  }

  # ===== 3. ATR (Average True Range) =====
  if (verbose) cat("  - ATR (", length(atr_periods), " Perioden)\n", sep = "")
  for (period in atr_periods) {
    col_name <- paste0("atr_", period)
    atr_values <- with(dt_ind, ATR(cbind(high, low, close), n = period)[, "atr"])
    dt_ind[, (col_name) := atr_values]

    # ATR % (normalisiert)
    atr_pct_col <- paste0("atr_", period, "_pct")
    dt_ind[, (atr_pct_col) := get(col_name) / close]
  }

  # ===== 4. ADX (Average Directional Index) =====
  if (verbose) cat("  - ADX (", length(adx_periods), " Perioden)\n", sep = "")
  for (period in adx_periods) {
    adx_result <- with(dt_ind, ADX(cbind(high, low, close), n = period))
    dt_ind[, (paste0("adx_", period)) := adx_result[, "ADX"]]
    dt_ind[, (paste0("di_plus_", period)) := adx_result[, "DIp"]]
    dt_ind[, (paste0("di_minus_", period)) := adx_result[, "DIn"]]

    # DI Difference (Trend Stärke)
    dt_ind[, (paste0("di_diff_", period)) :=
             get(paste0("di_plus_", period)) - get(paste0("di_minus_", period))]
  }

  # ===== 5. DPO (Detrended Price Oscillator) =====
  # HINWEIS: Standard DPO hat Look-Ahead Bias (zentrierter MA)
  # Verwende stattdessen: Close - SMA (kein Offset)
  if (verbose) cat("  - DPO (ohne Look-Ahead Bias)\n")
  dpo_period <- 20
  dt_ind[, dpo_20 := close - SMA(close, n = dpo_period)]

  # ===== 6. Keltner Channel =====
  if (verbose) cat("  - Keltner Channel\n")
  for (period in kc_periods) {
    # Keltner = EMA ± (2 * ATR)
    ema_col <- paste0("ema_", period)
    atr_col <- paste0("atr_", period)

    if (!ema_col %in% names(dt_ind)) {
      dt_ind[, (ema_col) := EMA(close, n = period)]
    }
    if (!atr_col %in% names(dt_ind)) {
      atr_values <- with(dt_ind, ATR(cbind(high, low, close), n = period)[, "atr"])
      dt_ind[, (atr_col) := atr_values]
    }

    dt_ind[, (paste0("kc_upper_", period)) := get(ema_col) + 2 * get(atr_col)]
    dt_ind[, (paste0("kc_lower_", period)) := get(ema_col) - 2 * get(atr_col)]
    dt_ind[, (paste0("kc_mid_", period)) := get(ema_col)]

    # Position innerhalb Keltner Channel
    dt_ind[, (paste0("kc_position_", period)) :=
             (close - get(paste0("kc_lower_", period))) /
             (get(paste0("kc_upper_", period)) - get(paste0("kc_lower_", period)) + 1e-10)]
  }

  # ===== 7. Bollinger Bands =====
  if (verbose) cat("  - Bollinger Bands\n")
  for (period in bb_periods) {
    bb_result <- with(dt_ind, BBands(cbind(high, low, close), n = period, sd = 2))
    dt_ind[, (paste0("bb_upper_", period)) := bb_result[, "up"]]
    dt_ind[, (paste0("bb_lower_", period)) := bb_result[, "dn"]]
    dt_ind[, (paste0("bb_mid_", period)) := bb_result[, "mavg"]]
    dt_ind[, (paste0("bb_pct_", period)) := bb_result[, "pctB"]]

    # Bandwidth (Volatilität)
    dt_ind[, (paste0("bb_bandwidth_", period)) :=
             (get(paste0("bb_upper_", period)) - get(paste0("bb_lower_", period))) /
             get(paste0("bb_mid_", period))]
  }

  # ===== 8. McGinley Dynamic =====
  if (verbose) cat("  - McGinley Dynamic\n")
  calculate_mcginley <- function(price, period = 10, k = 0.6) {
    mg <- numeric(length(price))
    mg[1] <- price[1]
    for (i in 2:length(price)) {
      mg[i] <- mg[i-1] + (price[i] - mg[i-1]) / (k * period * (price[i]/mg[i-1])^4)
    }
    return(mg)
  }

  dt_ind[, mcginley_10 := calculate_mcginley(close, period = 10)]
  dt_ind[, mcginley_20 := calculate_mcginley(close, period = 20)]

  # ===== 9. Ichimoku Cloud =====
  if (verbose) cat("  - Ichimoku Cloud\n")
  # Tenkan-sen (Conversion Line): (9-period high + 9-period low)/2
  dt_ind[, tenkan_sen := (runmax(high, 9) + runmin(low, 9)) / 2]

  # Kijun-sen (Base Line): (26-period high + 26-period low)/2
  dt_ind[, kijun_sen := (runmax(high, 26) + runmin(low, 26)) / 2]

  # Senkou Span A (Leading Span A): (Tenkan + Kijun)/2, shifted 26 forward
  dt_ind[, senkou_span_a := data.table::shift((tenkan_sen + kijun_sen) / 2, n = -26)]

  # Senkou Span B (Leading Span B): (52-period high + 52-period low)/2, shifted 26
  dt_ind[, senkou_span_b := data.table::shift((runmax(high, 52) + runmin(low, 52)) / 2, n = -26)]

  # Chikou Span (Lagging Span): Close shifted 26 backward
  dt_ind[, chikou_span := data.table::shift(close, n = 26)]

  # Cloud Position
  dt_ind[, ichimoku_position := fifelse(
    close > pmax(senkou_span_a, senkou_span_b, na.rm = TRUE), 1,  # Above cloud
    fifelse(close < pmin(senkou_span_a, senkou_span_b, na.rm = TRUE), -1, 0)  # Below cloud
  )]

  # ===== 10. Aroon Indicator =====
  if (verbose) cat("  - Aroon\n")
  aroon_result <- with(dt_ind, aroon(cbind(high, low), n = 25))
  dt_ind[, aroon_up := aroon_result[, "aroonUp"]]
  dt_ind[, aroon_down := aroon_result[, "aroonDn"]]
  dt_ind[, aroon_oscillator := aroon_result[, "oscillator"]]

  # ===== 11. Momentum =====
  if (verbose) cat("  - Momentum\n")
  dt_ind[, momentum_5 := momentum(close, n = 5)]
  dt_ind[, momentum_10 := momentum(close, n = 10)]
  dt_ind[, momentum_20 := momentum(close, n = 20)]

  # ROC (Rate of Change) - verwandt mit Momentum
  dt_ind[, roc_5 := ROC(close, n = 5)]
  dt_ind[, roc_10 := ROC(close, n = 10)]
  dt_ind[, roc_20 := ROC(close, n = 20)]

  # ===== 12. Choppiness Index =====
  if (verbose) cat("  - Choppiness Index\n")
  calculate_choppiness <- function(high, low, close, period = 14) {
    atr_sum <- runSum(ATR(cbind(high, low, close), n = 1)[, "atr"], n = period)
    high_low_diff <- runmax(high, period) - runmin(low, period)
    chop <- 100 * log10(atr_sum / (high_low_diff + 1e-10)) / log10(period)
    return(chop)
  }

  chop_values <- with(dt_ind, calculate_choppiness(high, low, close, period = 14))
  dt_ind[, choppiness_14 := chop_values]

  # ===== 13. Donchian Channel =====
  if (verbose) cat("  - Donchian Channel\n")
  dt_ind[, donchian_upper_20 := runmax(high, 20)]
  dt_ind[, donchian_lower_20 := runmin(low, 20)]
  dt_ind[, donchian_mid_20 := (donchian_upper_20 + donchian_lower_20) / 2]
  dt_ind[, donchian_position_20 :=
           (close - donchian_lower_20) / (donchian_upper_20 - donchian_lower_20 + 1e-10)]

  # ===== 14. Parabolic SAR =====
  if (verbose) cat("  - Parabolic SAR\n")
  sar_result <- with(dt_ind, SAR(cbind(high, low), accel = c(0.02, 0.2)))
  dt_ind[, sar := sar_result]
  dt_ind[, sar_signal := fifelse(close > sar, 1, -1)]  # 1 = bullish, -1 = bearish

  # ===== 15. Volume Indicators =====
  if (verbose) cat("  - Volume Indicators\n")

  # Volume MA
  dt_ind[, volume_ma_20 := SMA(volume, n = 20)]
  dt_ind[, volume_ratio := volume / (volume_ma_20 + 1e-10)]

  # On Balance Volume (OBV)
  dt_ind[, close_prev := data.table::shift(close, 1)]
  dt_ind[, obv_change := fifelse(is.na(close_prev), 0,
                                  fifelse(close > close_prev, volume, -volume))]
  dt_ind[, obv := cumsum(obv_change)]
  dt_ind[, c("close_prev", "obv_change") := NULL]

  # Volume Price Trend (VPT)
  dt_ind[, close_prev := data.table::shift(close, 1)]
  dt_ind[, vpt_change := fifelse(is.na(close_prev), 0,
                                  volume * (close - close_prev) / (close_prev + 1e-10))]
  dt_ind[, vpt := cumsum(vpt_change)]
  dt_ind[, c("close_prev", "vpt_change") := NULL]

  # ===== 16. VHF (Vertical Horizontal Filter) =====
  if (verbose) cat("  - VHF (Vertical Horizontal Filter)\n")
  calculate_vhf <- function(close, low, period = 28) {
    highest <- runmax(close, period)
    lowest <- runmin(low, period)
    price_changes <- abs(c(NA, diff(close)))
    sum_changes <- runSum(price_changes, period)
    vhf <- (highest - lowest) / (sum_changes + 1e-10)
    return(vhf)
  }

  dt_ind[, vhf_28 := calculate_vhf(close, low, period = 28)]

  # ===== 17. Stochastic Oscillator =====
  if (verbose) cat("  - Stochastic Oscillator\n")
  stoch_result <- with(dt_ind, stoch(cbind(high, low, close), nFastK = 14, nFastD = 3, nSlowD = 3))
  dt_ind[, stoch_k := stoch_result[, "fastK"]]
  dt_ind[, stoch_d := stoch_result[, "fastD"]]
  dt_ind[, stoch_k_d_diff := stoch_k - stoch_d]

  # ===== 18. Williams %R =====
  if (verbose) cat("  - Williams %R\n")
  wpr_values <- with(dt_ind, WPR(cbind(high, low, close), n = 14))
  dt_ind[, williams_r_14 := wpr_values]

  # ===== 19. CCI (Commodity Channel Index) =====
  if (verbose) cat("  - CCI\n")
  cci_values <- with(dt_ind, CCI(cbind(high, low, close), n = 20))
  dt_ind[, cci_20 := cci_values]

  # ===== 20. CMO (Chande Momentum Oscillator) =====
  if (verbose) cat("  - CMO\n")
  dt_ind[, cmo_14 := CMO(close, n = 14)]

  if (verbose) {
    cat(sprintf("✓ Indikatoren berechnet: %d Features erstellt\n",
                ncol(dt_ind) - ncol(dt)))
  }

  return(dt_ind)
}


#' Helper: Running Max
runmax <- function(x, n) {
  rollapply(x, width = n, FUN = max, align = "right", fill = NA, partial = TRUE)
}

#' Helper: Running Min
runmin <- function(x, n) {
  rollapply(x, width = n, FUN = min, align = "right", fill = NA, partial = TRUE)
}

#' Helper: Running Sum
runSum <- function(x, n) {
  rollapply(x, width = n, FUN = sum, align = "right", fill = NA, partial = TRUE)
}
