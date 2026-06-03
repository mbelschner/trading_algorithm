# Feature Engineering Module
# Erstellt Lags, 1. und 2. Ableitungen, stündliche Aggregate, Rolling Statistics

#' Engineer features from technical indicators
#'
#' @param dt data.table with technical indicators
#' @param lag_periods Vector of lag periods (default: c(1, 2, 3, 5, 10))
#' @param derivative_orders Vector of derivative orders (1, 2) (default: c(1, 2))
#' @param hourly_aggregation Create 1-hour aggregated features (default: TRUE)
#' @param rolling_windows Vector of rolling window sizes (default: c(10, 20, 50))
#' @param interaction_features Create interaction features (default: FALSE)
#' @param verbose Print progress messages
#'
#' @return data.table with engineered features
engineer_features <- function(
    dt,
    lag_periods = c(1, 2, 3, 5, 10),
    derivative_orders = c(1, 2),
    hourly_aggregation = TRUE,
    rolling_windows = c(10, 20, 50),
    interaction_features = FALSE,
    verbose = TRUE
) {

  if (verbose) cat("\n=== Feature Engineering ===\n")

  dt_feat <- copy(dt)

  # Identifiziere numerische Feature-Spalten (exkludiere Meta-Daten)
  exclude_cols <- c("datetime", "label", "barrier_hit", "t1", "return",
                    "sample_weight", "open", "high", "low", "close", "volume")

  # Hole alle numerischen Spalten außer excluded
  all_cols <- names(dt_feat)
  feature_cols <- setdiff(all_cols, exclude_cols)

  # Filtere nur numerische Spalten
  numeric_cols <- feature_cols[sapply(dt_feat[, ..feature_cols], is.numeric)]

  if (verbose) {
    cat(sprintf("Basis-Features: %d\n", length(numeric_cols)))
  }

  # ===== 1. Lag Features =====
  if (length(lag_periods) > 0) {
    if (verbose) cat(sprintf("Erstelle Lag Features (%d Perioden)...\n", length(lag_periods)))

    # Wähle wichtige Features für Lags (nicht alle, sonst zu viele Features)
    important_for_lags <- grep(
      "^(ema_|rsi_|atr_|adx_|bb_pct_|kc_position_|momentum_|roc_|volume_ratio|aroon_oscillator|stoch_k|williams_r|cci)",
      numeric_cols,
      value = TRUE
    )

    pb <- progress_bar$new(
      format = "  [:bar] :percent eta: :eta",
      total = length(important_for_lags) * length(lag_periods),
      clear = FALSE
    )

    for (col in important_for_lags) {
      for (lag in lag_periods) {
        lag_col_name <- paste0(col, "_lag", lag)
        dt_feat[, (lag_col_name) := data.table::shift(get(col), n = lag)]
        pb$tick()
      }
    }

    if (verbose) cat(sprintf("  ✓ %d Lag Features erstellt\n",
                            length(important_for_lags) * length(lag_periods)))
  }

  # ===== 2. Derivative Features (1. und 2. Ableitung) =====
  if (length(derivative_orders) > 0) {
    if (verbose) cat(sprintf("Erstelle Ableitungen (Order: %s)...\n",
                            paste(derivative_orders, collapse = ", ")))

    # Wähle Features für Ableitungen
    important_for_derivatives <- grep(
      "^(ema_|rsi_|atr_|bb_pct_|momentum_|roc_|volume_ratio|adx_|mcginley_|obv|vpt)",
      numeric_cols,
      value = TRUE
    )

    # Exkludiere bereits existierende Slope-Features
    important_for_derivatives <- grep("_slope$", important_for_derivatives,
                                     value = TRUE, invert = TRUE)

    pb <- progress_bar$new(
      format = "  [:bar] :percent eta: :eta",
      total = length(important_for_derivatives) * length(derivative_orders),
      clear = FALSE
    )

    for (col in important_for_derivatives) {
      # 1. Ableitung (Diff)
      if (1 %in% derivative_orders) {
        deriv1_col <- paste0(col, "_d1")
        dt_feat[, (deriv1_col) := c(NA, diff(get(col)))]
        pb$tick()
      }

      # 2. Ableitung (Diff of Diff)
      if (2 %in% derivative_orders) {
        deriv1_col <- paste0(col, "_d1")
        deriv2_col <- paste0(col, "_d2")

        # Falls d1 noch nicht existiert, erstelle es
        if (!deriv1_col %in% names(dt_feat)) {
          dt_feat[, (deriv1_col) := c(NA, diff(get(col)))]
        }

        dt_feat[, (deriv2_col) := c(NA, diff(get(deriv1_col)))]
        pb$tick()
      }
    }

    if (verbose) cat(sprintf("  ✓ %d Ableitungs-Features erstellt\n",
                            length(important_for_derivatives) * length(derivative_orders)))
  }

  # ===== 3. Hourly Aggregation (1h Intervall) =====
  if (hourly_aggregation) {
    if (verbose) cat("Erstelle 1-Stunden Aggregate...\n")

    # Annahme: datetime ist POSIXct oder kann konvertiert werden
    if (!inherits(dt_feat$datetime, "POSIXct")) {
      dt_feat[, datetime := as.POSIXct(datetime)]
    }

    # Erstelle Hour-Marker
    dt_feat[, hour_mark := floor_date(datetime, unit = "hour")]

    # Wähle Features für Aggregation
    agg_features <- grep(
      "^(close|volume|atr_|rsi_|adx_|bb_bandwidth_|volume_ratio)",
      numeric_cols,
      value = TRUE
    )

    # Berechne stündliche Statistiken
    hourly_stats <- dt_feat[, .(
      hour_close_mean = mean(close, na.rm = TRUE),
      hour_close_sd = sd(close, na.rm = TRUE),
      hour_volume_sum = sum(volume, na.rm = TRUE),
      hour_volume_mean = mean(volume, na.rm = TRUE),
      hour_high = max(high, na.rm = TRUE),
      hour_low = min(low, na.rm = TRUE),
      hour_range = max(high, na.rm = TRUE) - min(low, na.rm = TRUE)
    ), by = hour_mark]

    # Merge zurück
    dt_feat <- merge(dt_feat, hourly_stats, by = "hour_mark", all.x = TRUE)

    # Entferne hour_mark
    dt_feat[, hour_mark := NULL]

    if (verbose) cat(sprintf("  ✓ %d stündliche Aggregate erstellt\n", 7))
  }

  # ===== 4. Rolling Statistics =====
  if (length(rolling_windows) > 0) {
    if (verbose) cat(sprintf("Erstelle Rolling Statistics (%d Windows)...\n",
                            length(rolling_windows)))

    # Wähle Features für Rolling Stats
    important_for_rolling <- c("close", "volume", "atr_14", "rsi_14",
                               "bb_bandwidth_20", "volume_ratio")
    important_for_rolling <- intersect(important_for_rolling, names(dt_feat))

    pb <- progress_bar$new(
      format = "  [:bar] :percent eta: :eta",
      total = length(important_for_rolling) * length(rolling_windows) * 3,
      clear = FALSE
    )

    for (col in important_for_rolling) {
      for (window in rolling_windows) {
        # Rolling Mean
        mean_col <- paste0(col, "_roll_mean_", window)
        dt_feat[, (mean_col) := frollmean(get(col), n = window, align = "right")]
        pb$tick()

        # Rolling SD
        sd_col <- paste0(col, "_roll_sd_", window)
        dt_feat[, (sd_col) := frollapply(get(col), n = window,
                                         FUN = sd, align = "right", na.rm = TRUE)]
        pb$tick()

        # Z-Score (Distanz zu Rolling Mean)
        zscore_col <- paste0(col, "_zscore_", window)
        dt_feat[, (zscore_col) := (get(col) - get(mean_col)) / (get(sd_col) + 1e-10)]
        pb$tick()
      }
    }

    if (verbose) cat(sprintf("  ✓ %d Rolling Statistics erstellt\n",
                            length(important_for_rolling) * length(rolling_windows) * 3))
  }

  # ===== 5. Time-based Features =====
  if (verbose) cat("Erstelle zeitbasierte Features...\n")

  if (!inherits(dt_feat$datetime, "POSIXct")) {
    dt_feat[, datetime := as.POSIXct(datetime)]
  }

  dt_feat[, `:=`(
    hour = hour(datetime),
    day_of_week = wday(datetime),
    day_of_month = mday(datetime),
    month = month(datetime)
  )]

  # Session Indicator (Trading Session)
  # Annahme: Session ist 2-21 Uhr (wie in Labeling)
  dt_feat[, session := fifelse(hour >= 2 & hour <= 21, 1, 0)]

  # Sine/Cosine Encoding für zyklische Features
  dt_feat[, `:=`(
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24),
    day_of_week_sin = sin(2 * pi * day_of_week / 7),
    day_of_week_cos = cos(2 * pi * day_of_week / 7),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )]

  if (verbose) cat("  ✓ 12 zeitbasierte Features erstellt\n")

  # ===== 6. Interaction Features (Optional, nach Feature Selection) =====
  if (interaction_features) {
    if (verbose) cat("Erstelle Interaktions-Features...\n")

    # Beispiel: RSI * Volume Ratio
    if ("rsi_14" %in% names(dt_feat) && "volume_ratio" %in% names(dt_feat)) {
      dt_feat[, rsi_volume_interaction := rsi_14 * volume_ratio]
    }

    # EMA Cross * ADX (Trend Stärke wenn Cross)
    if ("ema_9_21_cross" %in% names(dt_feat) && "adx_14" %in% names(dt_feat)) {
      dt_feat[, ema_cross_adx := ema_9_21_cross * adx_14]
    }

    # BB Position * ATR (Volatility Breakout)
    if ("bb_pct_20" %in% names(dt_feat) && "atr_14_pct" %in% names(dt_feat)) {
      dt_feat[, bb_atr_interaction := bb_pct_20 * atr_14_pct]
    }

    if (verbose) cat("  ✓ Interaktions-Features erstellt\n")
  }

  # ===== Summary =====
  if (verbose) {
    n_original <- ncol(dt)
    n_engineered <- ncol(dt_feat)
    cat(sprintf("\n✓ Feature Engineering abgeschlossen:\n"))
    cat(sprintf("  Original Features: %d\n", n_original))
    cat(sprintf("  Engineered Features: %d\n", n_engineered))
    cat(sprintf("  Neue Features: %d\n", n_engineered - n_original))
  }

  return(dt_feat)
}


#' Helper: Floor datetime to nearest unit
floor_date <- function(x, unit = "hour") {
  if (unit == "hour") {
    return(as.POSIXct(format(x, "%Y-%m-%d %H:00:00")))
  }
  stop("Nur 'hour' unit implementiert")
}
