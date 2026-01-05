# =============================================================================
# META-LABELING FÜR EXTREMA-BASIERTE SIGNALE - OPTIMIZED VERSION
# =============================================================================
#
# PERFORMANCE OPTIMIERUNGEN:
# - Vektorisierte Operationen statt for-Loops
# - RcppRoll für schnelle Rolling Window Operationen
# - data.table Syntax für effiziente Berechnungen
# - Parallele Verarbeitung für Meta-Labeling (optional)
#
# =============================================================================

library(data.table)
library(TTR)
library(RcppRoll)  # Schnelle Rolling Window Funktionen

shift <- data.table::shift

# =============================================================================
# 1. EXTREMA DETECTION MIT VEKTORISIERUNG
# =============================================================================

#' Erkenne lokale Minima/Maxima mit vektorisierten Operationen
#'
detect_local_extrema <- function(
    prices,
    lookback_bars = 5,
    confirmation_method = "bars",
    confirmation_bars = 2,
    use_rsi = TRUE,
    rsi_period = 14,
    rsi_oversold = 30,
    rsi_overbought = 70
) {

  cat("=== EXTREMA DETECTION (OPTIMIZED) ===\n")
  cat("Lookback Bars:", lookback_bars, "\n")
  cat("Confirmation Method:", confirmation_method, "\n")
  cat("Confirmation Bars:", confirmation_bars, "\n")
  cat("Use RSI:", use_rsi, "\n\n")

  dt <- copy(prices)
  setDT(dt)
  n <- nrow(dt)

  # -------------------------------------------------------------------------
  # Berechne Indikatoren
  # -------------------------------------------------------------------------

  if(use_rsi) {
    dt[, rsi := RSI(close, n = rsi_period)]
  }

  dt[, ema_fast := EMA(close, n = 5)]
  dt[, ema_slow := EMA(close, n = 10)]
  dt[, ema_velocity := c(NA, diff(ema_fast))]
  dt[, ema_acceleration := c(NA, diff(ema_velocity))]

  # -------------------------------------------------------------------------
  # VEKTORISIERTE Extrema Detection mit RcppRoll
  # -------------------------------------------------------------------------

  # Berechne Rolling Min/Max für lookback window
  dt[, roll_min_left := roll_minl(low, n = lookback_bars + 1, fill = NA)]
  dt[, roll_min_right := roll_minr(data.table::shift(low, n = -1), n = lookback_bars + 1, fill = NA)]
  dt[, roll_max_left := roll_maxl(high, n = lookback_bars + 1, fill = NA)]
  dt[, roll_max_right := roll_maxr(data.table::shift(high, n = -1), n = lookback_bars + 1, fill = NA)]

  # Vektorisierte Extrema Detection
  dt[, is_local_min_candidate := (low <= roll_min_left) & (low <= roll_min_right)]
  dt[, is_local_max_candidate := (high >= roll_max_left) & (high >= roll_max_right)]

  # Erste und letzte lookback_bars auf FALSE setzen
  dt[c(1:lookback_bars, (n - lookback_bars + 1):n),
     `:=`(is_local_min_candidate = FALSE, is_local_max_candidate = FALSE)]

  cat("Lokale Minima (Kandidaten):", sum(dt$is_local_min_candidate, na.rm = TRUE), "\n")
  cat("Lokale Maxima (Kandidaten):", sum(dt$is_local_max_candidate, na.rm = TRUE), "\n")

  # -------------------------------------------------------------------------
  # RSI Filter (Vektorisiert)
  # -------------------------------------------------------------------------

  if(use_rsi) {
    dt[is_local_min_candidate == TRUE & rsi > rsi_oversold, is_local_min_candidate := FALSE]
    dt[is_local_max_candidate == TRUE & rsi < rsi_overbought, is_local_max_candidate := FALSE]

    cat("Nach RSI-Filter:\n")
    cat("  Lokale Minima:", sum(dt$is_local_min_candidate, na.rm = TRUE), "\n")
    cat("  Lokale Maxima:", sum(dt$is_local_max_candidate, na.rm = TRUE), "\n")
  }

  # -------------------------------------------------------------------------
  # CONFIRMATION LOGIC (Optimiert)
  # -------------------------------------------------------------------------

  dt[, extrema_confirmed := FALSE]
  dt[, extrema_type := NA_character_]
  dt[, confirmation_bar := NA_integer_]

  # Finde Kandidaten-Indizes
  min_candidates <- which(dt$is_local_min_candidate)
  max_candidates <- which(dt$is_local_max_candidate)

  cat("\n=== CONFIRMATION PHASE (OPTIMIZED) ===\n")

  # MINIMA CONFIRMATION (optimiert mit vektorisierten Vergleichen)
  if(length(min_candidates) > 0) {
    for(idx in min_candidates) {
      if(idx + confirmation_bars > n) next

      extrema_price <- dt$low[idx]
      future_idx <- (idx + 1):(idx + confirmation_bars)

      # METHOD 1: Bar-basierte Confirmation
      if(confirmation_method %in% c("bars", "both")) {
        future_closes <- dt$close[future_idx]
        higher_closes <- which(future_closes > extrema_price)

        if(length(higher_closes) > 0) {
          dt$extrema_confirmed[idx] <- TRUE
          dt$extrema_type[idx] <- "min"
          dt$confirmation_bar[idx] <- idx + higher_closes[1]
        }
      }

      # METHOD 2: Derivative-basierte Confirmation
      if(confirmation_method %in% c("derivative", "both")) {
        future_velocity <- dt$ema_velocity[future_idx]
        future_accel <- dt$ema_acceleration[future_idx]

        velocity_positive <- which(future_velocity > 0)
        accel_positive <- which(future_accel > 0)

        if(length(velocity_positive) > 0 && length(accel_positive) > 0) {
          if(confirmation_method == "both") {
            if(dt$extrema_confirmed[idx]) {
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
  }

  # MAXIMA CONFIRMATION
  if(length(max_candidates) > 0) {
    for(idx in max_candidates) {
      if(idx + confirmation_bars > n) next

      extrema_price <- dt$high[idx]
      future_idx <- (idx + 1):(idx + confirmation_bars)

      # METHOD 1: Bar-basierte Confirmation
      if(confirmation_method %in% c("bars", "both")) {
        future_closes <- dt$close[future_idx]
        lower_closes <- which(future_closes < extrema_price)

        if(length(lower_closes) > 0) {
          dt$extrema_confirmed[idx] <- TRUE
          dt$extrema_type[idx] <- "max"
          dt$confirmation_bar[idx] <- idx + lower_closes[1]
        }
      }

      # METHOD 2: Derivative-basierte Confirmation
      if(confirmation_method %in% c("derivative", "both")) {
        future_velocity <- dt$ema_velocity[future_idx]
        future_accel <- dt$ema_acceleration[future_idx]

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
  }

  cat("Confirmed Extrema:\n")
  cat("  Minima:", sum(dt$extrema_confirmed & dt$extrema_type == "min", na.rm = TRUE), "\n")
  cat("  Maxima:", sum(dt$extrema_confirmed & dt$extrema_type == "max", na.rm = TRUE), "\n\n")

  # Cleanup temporäre Spalten
  dt[, c("roll_min_left", "roll_min_right", "roll_max_left", "roll_max_right") := NULL]

  return(dt)
}


# =============================================================================
# 2. PRIMARY SIGNAL GENERATION (Vektorisiert)
# =============================================================================

generate_primary_signals <- function(
    extrema_data,
    atr_period = 14
) {

  cat("=== PRIMARY SIGNAL GENERATION (OPTIMIZED) ===\n")

  dt <- copy(extrema_data)
  setDT(dt)

  # Berechne ATR
  if(!"atr" %in% names(dt)) {
    atr_result <- ATR(HLC = cbind(dt$high, dt$low, dt$close), n = atr_period)
    dt[, atr := atr_result[, "atr"]]
  }

  # Initialize Signal
  dt[, primary_signal := 0L]
  dt[, signal_bar := NA_integer_]

  # VEKTORISIERTE Signal Generation
  confirmed_idx <- which(dt$extrema_confirmed)

  if(length(confirmed_idx) > 0) {
    # Erstelle Mapping von Extrema-Index zu Confirmation-Bar
    mapping_dt <- data.table(
      extrema_idx = confirmed_idx,
      confirmation_bar = dt$confirmation_bar[confirmed_idx],
      extrema_type = dt$extrema_type[confirmed_idx]
    )
    mapping_dt <- mapping_dt[!is.na(confirmation_bar)]

    # Setze Signals an Confirmation Bars
    for(i in 1:nrow(mapping_dt)) {
      confirm_bar <- mapping_dt$confirmation_bar[i]
      extrema_t <- mapping_dt$extrema_type[i]

      if(extrema_t == "min") {
        dt$primary_signal[confirm_bar] <- 1L  # Long
      } else if(extrema_t == "max") {
        dt$primary_signal[confirm_bar] <- -1L  # Short
      }
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
# 3. META-LABELING (Hochoptimiert)
# =============================================================================

#' Erstelle Meta-Labels mit optimierter Loop-Logik
#'
create_meta_labels <- function(
    signal_data,
    atr_mult_profit = 2.0,
    atr_mult_stop = 1.5,
    max_holding_bars = 20,
    use_stop_loss = TRUE,
    parallel = FALSE,
    n_cores = 4
) {

  cat("=== META-LABELING (OPTIMIZED) ===\n")
  cat("ATR Mult Profit:", atr_mult_profit, "\n")
  cat("ATR Mult Stop:", atr_mult_stop, "\n")
  cat("Max Holding Bars:", max_holding_bars, "\n")
  cat("Use Stop Loss:", use_stop_loss, "\n")
  cat("Parallel:", parallel, "\n\n")

  dt <- copy(signal_data)
  setDT(dt)
  n <- nrow(dt)

  # Initialize Meta-Label columns
  dt[, `:=`(
    meta_label = NA_integer_,
    exit_reason = NA_character_,
    bars_held = NA_integer_,
    realized_pnl = NA_real_,
    profit_target = NA_real_,
    stop_loss = NA_real_
  )]

  # Finde alle Signal-Bars
  signal_indices <- which(dt$primary_signal != 0)
  n_signals <- length(signal_indices)

  cat("Labeling", n_signals, "signals...\n")

  if(n_signals == 0) {
    return(dt[primary_signal != 0])
  }

  # -------------------------------------------------------------------------
  # FUNKTION für einzelnes Signal-Labeling
  # -------------------------------------------------------------------------

  label_single_signal <- function(idx, dt_ref, n_ref) {
    side <- dt_ref$primary_signal[idx]
    entry_price <- dt_ref$close[idx]
    current_atr <- dt_ref$atr[idx]

    if(is.na(current_atr) || current_atr == 0) {
      return(list(
        idx = idx,
        meta_label = NA_integer_,
        exit_reason = NA_character_,
        bars_held = NA_integer_,
        realized_pnl = NA_real_,
        profit_target = NA_real_,
        stop_loss = NA_real_
      ))
    }

    # Define Profit Target & Stop Loss
    if(side == 1) {  # Long
      profit_target <- entry_price + (atr_mult_profit * current_atr)
      stop_loss_val <- entry_price - (atr_mult_stop * current_atr)
    } else {  # Short
      profit_target <- entry_price - (atr_mult_profit * current_atr)
      stop_loss_val <- entry_price + (atr_mult_stop * current_atr)
    }

    # Scan future bars
    end_idx <- min(idx + max_holding_bars, n_ref)
    future_idx <- (idx + 1):end_idx

    # Vektorisierte Vergleiche
    future_high <- dt_ref$high[future_idx]
    future_low <- dt_ref$low[future_idx]
    future_close <- dt_ref$close[future_idx]

    meta_label <- NA_integer_
    exit_reason <- NA_character_
    bars_held <- NA_integer_
    realized_pnl <- NA_real_

    if(side == 1) {  # Long
      # Profit Target Check
      profit_hits <- which(future_high >= profit_target)

      # Stop Loss Check
      if(use_stop_loss) {
        stop_hits <- which(future_low <= stop_loss_val)
      } else {
        stop_hits <- integer(0)
      }

      # Finde ersten Hit
      if(length(profit_hits) > 0 && (length(stop_hits) == 0 || profit_hits[1] < stop_hits[1])) {
        # Profit Target zuerst
        meta_label <- 1L
        exit_reason <- "profit_target"
        bars_held <- profit_hits[1]
        realized_pnl <- (profit_target - entry_price) / entry_price

      } else if(length(stop_hits) > 0) {
        # Stop Loss zuerst
        meta_label <- 0L
        exit_reason <- "stop_loss"
        bars_held <- stop_hits[1]
        realized_pnl <- (stop_loss_val - entry_price) / entry_price

      } else {
        # Max Holding Period
        final_return <- (future_close[length(future_close)] - entry_price) / entry_price
        meta_label <- if(final_return > 0) 1L else 0L
        exit_reason <- if(final_return > 0) "max_holding_positive" else "max_holding_negative"
        bars_held <- length(future_idx)
        realized_pnl <- final_return
      }

    } else {  # Short
      # Profit Target Check
      profit_hits <- which(future_low <= profit_target)

      # Stop Loss Check
      if(use_stop_loss) {
        stop_hits <- which(future_high >= stop_loss_val)
      } else {
        stop_hits <- integer(0)
      }

      # Finde ersten Hit
      if(length(profit_hits) > 0 && (length(stop_hits) == 0 || profit_hits[1] < stop_hits[1])) {
        # Profit Target zuerst
        meta_label <- 1L
        exit_reason <- "profit_target"
        bars_held <- profit_hits[1]
        realized_pnl <- (entry_price - profit_target) / entry_price

      } else if(length(stop_hits) > 0) {
        # Stop Loss zuerst
        meta_label <- 0L
        exit_reason <- "stop_loss"
        bars_held <- stop_hits[1]
        realized_pnl <- (entry_price - stop_loss_val) / entry_price

      } else {
        # Max Holding Period
        final_return <- (entry_price - future_close[length(future_close)]) / entry_price
        meta_label <- if(final_return > 0) 1L else 0L
        exit_reason <- if(final_return > 0) "max_holding_positive" else "max_holding_negative"
        bars_held <- length(future_idx)
        realized_pnl <- final_return
      }
    }

    return(list(
      idx = idx,
      meta_label = meta_label,
      exit_reason = exit_reason,
      bars_held = bars_held,
      realized_pnl = realized_pnl,
      profit_target = profit_target,
      stop_loss = stop_loss_val
    ))
  }

  # -------------------------------------------------------------------------
  # PARALLEL oder SEQUENTIAL Processing
  # -------------------------------------------------------------------------

  if(parallel && n_signals > 100) {
    cat("Using parallel processing with", n_cores, "cores...\n")

    library(parallel)
    cl <- makeCluster(n_cores)
    clusterExport(cl, c("dt", "n", "atr_mult_profit", "atr_mult_stop",
                        "max_holding_bars", "use_stop_loss"), envir = environment())

    results <- parLapply(cl, signal_indices, label_single_signal,
                         dt_ref = dt, n_ref = n)
    stopCluster(cl)

  } else {
    # Sequential Processing (schneller für kleine Datensätze)
    results <- lapply(signal_indices, label_single_signal,
                     dt_ref = dt, n_ref = n)
  }

  # -------------------------------------------------------------------------
  # Ergebnisse zurückschreiben
  # -------------------------------------------------------------------------

  for(res in results) {
    idx <- res$idx
    dt$meta_label[idx] <- res$meta_label
    dt$exit_reason[idx] <- res$exit_reason
    dt$bars_held[idx] <- res$bars_held
    dt$realized_pnl[idx] <- res$realized_pnl
    dt$profit_target[idx] <- res$profit_target
    dt$stop_loss[idx] <- res$stop_loss
  }

  # Filter nur die Zeilen mit Signals
  result_dt <- dt[primary_signal != 0]

  # -------------------------------------------------------------------------
  # Statistiken
  # -------------------------------------------------------------------------

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

#' Complete Meta-Labeling Pipeline (Optimized)
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
    use_stop_loss = TRUE,
    # Performance
    parallel = FALSE,
    n_cores = 4
) {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║      META-LABELING PIPELINE (OPTIMIZED VERSION)                ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  start_time <- Sys.time()

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
    use_stop_loss = use_stop_loss,
    parallel = parallel,
    n_cores = n_cores
  )

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                    PIPELINE COMPLETED                          ║\n")
  cat(sprintf("║  Total Runtime: %.2f seconds                              ║\n", elapsed))
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  return(list(
    meta_labeled = meta_labeled,
    full_data = signal_data,
    runtime = elapsed
  ))
}


cat("\n=== META-LABELING SCRIPT LOADED (OPTIMIZED) ===\n")
cat("Hauptfunktion:\n")
cat("  - generate_meta_labeled_signals()  # Complete Pipeline\n")
cat("  - Parameter: parallel=TRUE, n_cores=4 für Parallelisierung\n\n")
cat("Performance-Optimierungen:\n")
cat("  - Vektorisierte Extrema Detection mit RcppRoll\n")
cat("  - Optimierte Meta-Labeling Loops\n")
cat("  - Optional: Parallele Verarbeitung\n\n")
