# =============================================================================
# META-LABELING FÜR EXTREMA-BASIERTE SIGNALE
# =============================================================================
#
# Implementierung nach Lopez de Prado: "Advances in Financial Machine Learning"
#
# META-LABELING KONZEPT:
# - Primary Model: Erzeugt Signale (Side: Long/Short) basierend auf Extrema
# - Secondary Model (Meta-Labeling): Entscheidet ob das Signal profitabel ist (0/1)
#
# WORKFLOW:
# 1. Erkenne lokale Extrema (Minima für Long, Maxima für Short)
# 2. Warte auf Confirmation (Look-Ahead Bias vermeiden)
# 3. Generiere Primary Signal (Side: 1 = Long, -1 = Short)
# 4. Erstelle Meta-Label (0 = Skip Trade, 1 = Take Trade)
#
# =============================================================================

library(data.table)
library(TTR)

# =============================================================================
# 1. EXTREMA DETECTION MIT CONFIRMATION
# =============================================================================

#' Erkenne lokale Minima/Maxima mit verschiedenen Methoden
#'
#' @param prices data.table mit OHLC-Daten
#' @param lookback_bars Anzahl der Bars links/rechts für Extrema-Suche
#' @param confirmation_method "bars", "derivative", oder "both"
#' @param confirmation_bars Anzahl Bars für Confirmation (wenn method = "bars")
#' @param use_rsi Verwende RSI für zusätzliche Extrema-Bestätigung
#' @param rsi_period RSI Period
#' @param rsi_oversold RSI-Schwelle für Oversold (Minima)
#' @param rsi_overbought RSI-Schwelle für Overbought (Maxima)
#'
detect_local_extrema <- function(
    prices,
    lookback_bars = 5,
    confirmation_method = "bars",  # "bars", "derivative", "both"
    confirmation_bars = 2,
    use_rsi = TRUE,
    rsi_period = 14,
    rsi_oversold = 30,
    rsi_overbought = 70
) {

  cat("=== EXTREMA DETECTION ===\n")
  cat("Lookback Bars:", lookback_bars, "\n")
  cat("Confirmation Method:", confirmation_method, "\n")
  cat("Confirmation Bars:", confirmation_bars, "\n")
  cat("Use RSI:", use_rsi, "\n\n")

  dt <- copy(prices)
  n <- nrow(dt)

  # -------------------------------------------------------------------------
  # Berechne Indikatoren
  # -------------------------------------------------------------------------

  # RSI für Extrema-Filter
  if(use_rsi) {
    dt[, rsi := RSI(close, n = rsi_period)]
  }

  # EMA für Trend-Ableitungen
  dt[, ema_fast := EMA(close, n = 5)]
  dt[, ema_slow := EMA(close, n = 10)]

  # Erste und zweite Ableitung des EMAs (Momentum & Acceleration)
  dt[, ema_velocity := c(NA, diff(ema_fast))]
  dt[, ema_acceleration := c(NA, diff(ema_velocity))]

  # -------------------------------------------------------------------------
  # Erkenne Kandidaten für Extrema (basierend auf lokalen Highs/Lows)
  # -------------------------------------------------------------------------

  dt[, is_local_min_candidate := FALSE]
  dt[, is_local_max_candidate := FALSE]

  for(i in (lookback_bars + 1):(n - lookback_bars)) {

    current_low <- dt$low[i]
    current_high <- dt$high[i]

    # Prüfe ob lokales Minimum
    left_window <- dt$low[(i - lookback_bars):(i - 1)]
    right_window <- dt$low[(i + 1):(i + lookback_bars)]

    if(all(current_low <= left_window) && all(current_low <= right_window)) {
      dt$is_local_min_candidate[i] <- TRUE
    }

    # Prüfe ob lokales Maximum
    left_window <- dt$high[(i - lookback_bars):(i - 1)]
    right_window <- dt$high[(i + 1):(i + lookback_bars)]

    if(all(current_high >= left_window) && all(current_high >= right_window)) {
      dt$is_local_max_candidate[i] <- TRUE
    }
  }

  cat("Lokale Minima (Kandidaten):", sum(dt$is_local_min_candidate), "\n")
  cat("Lokale Maxima (Kandidaten):", sum(dt$is_local_max_candidate), "\n")

  # -------------------------------------------------------------------------
  # RSI Filter (Optional)
  # -------------------------------------------------------------------------

  if(use_rsi) {
    dt[is_local_min_candidate == TRUE & rsi > rsi_oversold, is_local_min_candidate := FALSE]
    dt[is_local_max_candidate == TRUE & rsi < rsi_overbought, is_local_max_candidate := FALSE]

    cat("Nach RSI-Filter:\n")
    cat("  Lokale Minima:", sum(dt$is_local_min_candidate), "\n")
    cat("  Lokale Maxima:", sum(dt$is_local_max_candidate), "\n")
  }

  # -------------------------------------------------------------------------
  # CONFIRMATION LOGIC (Look-Ahead Bias vermeiden)
  # -------------------------------------------------------------------------

  dt[, extrema_confirmed := FALSE]
  dt[, extrema_type := NA_character_]  # "min" oder "max"
  dt[, confirmation_bar := NA_integer_]  # An welcher Bar wurde confirmed

  # Finde alle Kandidaten-Indizes
  min_candidates <- which(dt$is_local_min_candidate)
  max_candidates <- which(dt$is_local_max_candidate)

  cat("\n=== CONFIRMATION PHASE ===\n")

  # MINIMA CONFIRMATION
  for(idx in min_candidates) {

    if(idx + confirmation_bars > n) next

    extrema_price <- dt$low[idx]

    # METHOD 1: Bar-basierte Confirmation
    if(confirmation_method %in% c("bars", "both")) {

      # Prüfe ob die nächsten N Bars höher schließen
      future_closes <- dt$close[(idx + 1):(idx + confirmation_bars)]

      # Confirmation: Mindestens 1 Bar schließt höher als Extremum
      if(any(future_closes > extrema_price)) {
        dt$extrema_confirmed[idx] <- TRUE
        dt$extrema_type[idx] <- "min"
        dt$confirmation_bar[idx] <- idx + which(future_closes > extrema_price)[1]
      }
    }

    # METHOD 2: Derivative-basierte Confirmation
    if(confirmation_method %in% c("derivative", "both")) {

      # Prüfe ob Velocity und Acceleration positiv werden (Trendwende nach oben)
      future_velocity <- dt$ema_velocity[(idx + 1):(idx + confirmation_bars)]
      future_accel <- dt$ema_acceleration[(idx + 1):(idx + confirmation_bars)]

      # Confirmation: Velocity wird positiv UND Acceleration wird positiv
      velocity_positive <- which(future_velocity > 0)
      accel_positive <- which(future_accel > 0)

      if(length(velocity_positive) > 0 && length(accel_positive) > 0) {

        if(confirmation_method == "both") {
          # Beide Bedingungen müssen erfüllt sein
          if(dt$extrema_confirmed[idx]) {  # bars war schon TRUE
            dt$confirmation_bar[idx] <- idx + min(velocity_positive[1], accel_positive[1])
          }
        } else {
          dt$extrema_confirmed[idx] <- TRUE
          dt$extrema_type[idx] <- "min"
          dt$confirmation_bar[idx] <- idx + min(velocity_positive[1], accel_positive[1])
        }
      }
    }
  }

  # MAXIMA CONFIRMATION
  for(idx in max_candidates) {

    if(idx + confirmation_bars > n) next

    extrema_price <- dt$high[idx]

    # METHOD 1: Bar-basierte Confirmation
    if(confirmation_method %in% c("bars", "both")) {

      future_closes <- dt$close[(idx + 1):(idx + confirmation_bars)]

      # Confirmation: Mindestens 1 Bar schließt tiefer als Extremum
      if(any(future_closes < extrema_price)) {
        dt$extrema_confirmed[idx] <- TRUE
        dt$extrema_type[idx] <- "max"
        dt$confirmation_bar[idx] <- idx + which(future_closes < extrema_price)[1]
      }
    }

    # METHOD 2: Derivative-basierte Confirmation
    if(confirmation_method %in% c("derivative", "both")) {

      future_velocity <- dt$ema_velocity[(idx + 1):(idx + confirmation_bars)]
      future_accel <- dt$ema_acceleration[(idx + 1):(idx + confirmation_bars)]

      # Confirmation: Velocity wird negativ UND Acceleration wird negativ
      velocity_negative <- which(future_velocity < 0)
      accel_negative <- which(future_accel < 0)

      if(length(velocity_negative) > 0 && length(accel_negative) > 0) {

        if(confirmation_method == "both") {
          if(dt$extrema_confirmed[idx]) {
            dt$confirmation_bar[idx] <- idx + min(velocity_negative[1], accel_negative[1])
          }
        } else {
          dt$extrema_confirmed[idx] <- TRUE
          dt$extrema_type[idx] <- "max"
          dt$confirmation_bar[idx] <- idx + min(velocity_negative[1], accel_negative[1])
        }
      }
    }
  }

  cat("Confirmed Extrema:\n")
  cat("  Minima:", sum(dt$extrema_confirmed & dt$extrema_type == "min", na.rm = TRUE), "\n")
  cat("  Maxima:", sum(dt$extrema_confirmed & dt$extrema_type == "max", na.rm = TRUE), "\n\n")

  return(dt)
}


# =============================================================================
# 2. PRIMARY SIGNAL GENERATION
# =============================================================================

#' Generiere Primary Signals (Side) basierend auf Extrema
#'
#' Primary Signal = Side der Trade (1 = Long, -1 = Short, 0 = kein Signal)
#'
generate_primary_signals <- function(
    extrema_data,
    atr_period = 14
) {

  cat("=== PRIMARY SIGNAL GENERATION ===\n")

  dt <- copy(extrema_data)

  # Berechne ATR
  if(!"atr" %in% names(dt)) {
    atr_result <- ATR(HLC = cbind(dt$high, dt$low, dt$close), n = atr_period)
    dt[, atr := atr_result[, "atr"]]
  }

  # Initialize Signal
  dt[, primary_signal := 0L]
  dt[, signal_bar := NA_integer_]  # Bar an der das Signal aktiv wird

  # Signals werden an der CONFIRMATION BAR generiert (nicht am Extremum selbst!)
  confirmed_idx <- which(dt$extrema_confirmed)

  for(idx in confirmed_idx) {

    extrema_t <- dt$extrema_type[idx]
    confirm_bar <- dt$confirmation_bar[idx]

    if(is.na(confirm_bar)) next

    # Signal wird an der Confirmation Bar aktiv
    if(extrema_t == "min") {
      dt$primary_signal[confirm_bar] <- 1L  # Long Signal
      dt$signal_bar[confirm_bar] <- confirm_bar

    } else if(extrema_t == "max") {
      dt$primary_signal[confirm_bar] <- -1L  # Short Signal
      dt$signal_bar[confirm_bar] <- confirm_bar
    }
  }

  n_long <- sum(dt$primary_signal == 1)
  n_short <- sum(dt$primary_signal == -1)

  cat("Primary Signals:\n")
  cat("  Long (1):", n_long, "\n")
  cat("  Short (-1):", n_short, "\n")
  cat("  Total:", n_long + n_short, "\n\n")

  return(dt)
}


# =============================================================================
# 3. META-LABELING
# =============================================================================

#' Erstelle Meta-Labels für Primary Signals
#'
#' Meta-Label = Binäres Label (0 oder 1), das angibt ob das Primary Signal
#' profitabel war (1) oder nicht (0)
#'
#' @param signal_data data.table mit Primary Signals
#' @param atr_mult_profit ATR Multiplier für Profit Target
#' @param atr_mult_stop ATR Multiplier für Stop Loss (optional)
#' @param max_holding_bars Maximale Holding Period
#' @param use_stop_loss Verwende Stop Loss? (TRUE/FALSE)
#'
create_meta_labels <- function(
    signal_data,
    atr_mult_profit = 2.0,
    atr_mult_stop = 1.5,
    max_holding_bars = 20,
    use_stop_loss = TRUE
) {

  cat("=== META-LABELING ===\n")
  cat("ATR Mult Profit:", atr_mult_profit, "\n")
  cat("ATR Mult Stop:", atr_mult_stop, "\n")
  cat("Max Holding Bars:", max_holding_bars, "\n")
  cat("Use Stop Loss:", use_stop_loss, "\n\n")

  dt <- copy(signal_data)
  n <- nrow(dt)

  # Initialize Meta-Label columns
  dt[, meta_label := NA_integer_]
  dt[, exit_reason := NA_character_]
  dt[, bars_held := NA_integer_]
  dt[, realized_pnl := NA_real_]
  dt[, profit_target := NA_real_]
  dt[, stop_loss := NA_real_]

  # Finde alle Signal-Bars
  signal_indices <- which(dt$primary_signal != 0)

  cat("Labeling", length(signal_indices), "signals...\n")

  for(idx in signal_indices) {

    side <- dt$primary_signal[idx]
    entry_price <- dt$close[idx]
    current_atr <- dt$atr[idx]

    if(is.na(current_atr) || current_atr == 0) next

    # Define Profit Target & Stop Loss
    if(side == 1) {  # Long
      profit_target <- entry_price + (atr_mult_profit * current_atr)
      stop_loss <- entry_price - (atr_mult_stop * current_atr)

    } else {  # Short
      profit_target <- entry_price - (atr_mult_profit * current_atr)
      stop_loss <- entry_price + (atr_mult_stop * current_atr)
    }

    dt$profit_target[idx] <- profit_target
    dt$stop_loss[idx] <- stop_loss

    # Scan future bars
    end_idx <- min(idx + max_holding_bars, n)

    for(j in (idx + 1):end_idx) {

      bars_elapsed <- j - idx

      # Check Profit Target
      if(side == 1) {  # Long
        if(dt$high[j] >= profit_target) {
          dt$meta_label[idx] <- 1L
          dt$exit_reason[idx] <- "profit_target"
          dt$bars_held[idx] <- bars_elapsed
          dt$realized_pnl[idx] <- (profit_target - entry_price) / entry_price
          break
        }

        # Check Stop Loss
        if(use_stop_loss && dt$low[j] <= stop_loss) {
          dt$meta_label[idx] <- 0L
          dt$exit_reason[idx] <- "stop_loss"
          dt$bars_held[idx] <- bars_elapsed
          dt$realized_pnl[idx] <- (stop_loss - entry_price) / entry_price
          break
        }

      } else {  # Short
        if(dt$low[j] <= profit_target) {
          dt$meta_label[idx] <- 1L
          dt$exit_reason[idx] <- "profit_target"
          dt$bars_held[idx] <- bars_elapsed
          dt$realized_pnl[idx] <- (entry_price - profit_target) / entry_price
          break
        }

        if(use_stop_loss && dt$high[j] >= stop_loss) {
          dt$meta_label[idx] <- 0L
          dt$exit_reason[idx] <- "stop_loss"
          dt$bars_held[idx] <- bars_elapsed
          dt$realized_pnl[idx] <- (entry_price - stop_loss) / entry_price
          break
        }
      }

      # Max Holding Period erreicht
      if(j == end_idx) {
        final_return <- (dt$close[j] - entry_price) / entry_price * side

        # Meta-Label basierend auf finalem Return
        if(final_return > 0) {
          dt$meta_label[idx] <- 1L
          dt$exit_reason[idx] <- "max_holding_positive"
        } else {
          dt$meta_label[idx] <- 0L
          dt$exit_reason[idx] <- "max_holding_negative"
        }
        dt$bars_held[idx] <- bars_elapsed
        dt$realized_pnl[idx] <- final_return
      }
    }
  }

  # Filter nur die Zeilen mit Signals
  result_dt <- dt[primary_signal != 0]

  cat("\n=== META-LABEL STATISTIKEN ===\n")
  cat("Total Signals:", nrow(result_dt), "\n\n")

  cat("Meta-Label Verteilung:\n")
  print(table(result_dt$meta_label, useNA = "ifany"))
  cat("\nProzentual:\n")
  print(round(prop.table(table(result_dt$meta_label)) * 100, 1))

  cat("\nExit Reasons:\n")
  print(table(result_dt$exit_reason))

  cat("\nDurchschnittliche Holding Period:\n")
  cat("  Mean:", round(mean(result_dt$bars_held, na.rm = TRUE), 1), "bars\n")
  cat("  Median:", median(result_dt$bars_held, na.rm = TRUE), "bars\n")

  cat("\nRealized PnL:\n")
  cat("  Mean:", round(mean(result_dt$realized_pnl, na.rm = TRUE) * 100, 2), "%\n")
  cat("  Median:", round(median(result_dt$realized_pnl, na.rm = TRUE) * 100, 2), "%\n")

  cat("\nSignal Performance by Side:\n")
  print(result_dt[, .(
    count = .N,
    success_rate = round(mean(meta_label, na.rm = TRUE) * 100, 1),
    avg_pnl = round(mean(realized_pnl, na.rm = TRUE) * 100, 2)
  ), by = primary_signal])

  return(result_dt)
}


# =============================================================================
# 4. MAIN WRAPPER FUNCTION
# =============================================================================

#' Complete Meta-Labeling Pipeline
#'
generate_meta_labeled_signals <- function(
    prices,
    # Extrema Detection
    lookback_bars = 5,
    confirmation_method = "bars",
    confirmation_bars = 2,
    use_rsi = TRUE,
    rsi_period = 14,
    rsi_oversold = 30,
    rsi_overbought = 70,
    # Signal Generation
    atr_period = 14,
    # Meta-Labeling
    atr_mult_profit = 2.0,
    atr_mult_stop = 1.5,
    max_holding_bars = 20,
    use_stop_loss = TRUE
) {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║         META-LABELING PIPELINE (Lopez de Prado)               ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  # Step 1: Detect Extrema
  extrema_data <- detect_local_extrema(
    prices = prices,
    lookback_bars = lookback_bars,
    confirmation_method = confirmation_method,
    confirmation_bars = confirmation_bars,
    use_rsi = use_rsi,
    rsi_period = rsi_period,
    rsi_oversold = rsi_oversold,
    rsi_overbought = rsi_overbought
  )

  # Step 2: Generate Primary Signals
  signal_data <- generate_primary_signals(
    extrema_data = extrema_data,
    atr_period = atr_period
  )

  # Step 3: Create Meta-Labels
  meta_labeled <- create_meta_labels(
    signal_data = signal_data,
    atr_mult_profit = atr_mult_profit,
    atr_mult_stop = atr_mult_stop,
    max_holding_bars = max_holding_bars,
    use_stop_loss = use_stop_loss
  )

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                    PIPELINE COMPLETED                          ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  return(list(
    meta_labeled = meta_labeled,
    full_data = signal_data
  ))
}


cat("\n=== META-LABELING SCRIPT LOADED ===\n")
cat("Hauptfunktion:\n")
cat("  - generate_meta_labeled_signals()  # Complete Pipeline\n\n")
cat("Einzelne Funktionen:\n")
cat("  - detect_local_extrema()           # Extrema Detection + Confirmation\n")
cat("  - generate_primary_signals()       # Primary Signal (Side)\n")
cat("  - create_meta_labels()             # Meta-Label (Size/Quality)\n\n")
