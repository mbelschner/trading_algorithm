# =============================================================================
# TRIPLE BARRIER LABELING FÜR INTRADAY METALS TRADING
# Optimiert für Gold/Silber/Kupfer auf 15-Minuten Daten (8:00-19:00 Session)
# =============================================================================
# 
# Basiert auf: López de Prado - "Advances in Financial Machine Learning" (2018)
# 
# Wichtige Konzepte:
# - Symmetrische Barrieren: Weil wir die Side (Long/Short) noch NICHT kennen
# - ATR-basierte dynamische Schwellen: Passt sich Volatilität an
# - Session-bounded: Verhindert Overnight-Exposure
# - Vertical Barrier: Maximale Haltedauer
#
# =============================================================================

library(data.table)
library(TTR)
library(ggplot2)

# =============================================================================
# HAUPTFUNKTION: TRIPLE BARRIER LABELING
# =============================================================================

create_triple_barrier_labels <- function(
    prices,                    # xts/data.frame mit OHLC Daten
    atr_period = 14,           # Periode für ATR-Berechnung
    atr_mult_barrier = 1.5,    # ATR-Multiplikator für Barrieren (SYMMETRISCH)
    max_horizon_bars = 24,     # Maximale Haltedauer in Bars (24 = 6h bei 15min)
    session_start = 2,         # Session Start (Stunde)
    session_end = 20,          # Session Ende (Stunde)
    min_atr = 0.0001,          # Minimum ATR (filtert illiquide Perioden)
    vertical_label_mode = "sign"  # "sign" = Return-Vorzeichen, "zero" = neutral
) {
  
  cat("=== TRIPLE BARRIER LABELING ===\n")
  cat("ATR Period:", atr_period, "\n")
  cat("ATR Multiplier:", atr_mult_barrier, "(symmetrisch)\n")
  cat("Max Horizon:", max_horizon_bars, "bars\n")
  cat("Session:", session_start, ":00 -", session_end, ":00\n\n")
  
  # -------------------------------------------------------------------------
  # 1. DATEN VORBEREITEN
  # -------------------------------------------------------------------------
  
  # Konvertiere zu data.table
  if(inherits(prices, "xts")) {
    dt <- data.table(
      datetime = index(prices),
      open = as.numeric(prices[, grep("open", names(prices), ignore.case = TRUE)]),
      high = as.numeric(prices[, grep("high", names(prices), ignore.case = TRUE)]),
      low = as.numeric(prices[, grep("low", names(prices), ignore.case = TRUE)]),
      close = as.numeric(prices[, grep("close", names(prices), ignore.case = TRUE)])
    )
  } else {
    dt <- as.data.table(prices)
    setnames(dt, tolower(names(dt)))
  }
  
  # Sicherstellen dass datetime korrekt ist
  if(!inherits(dt$datetime, "POSIXct")) {
    dt[, datetime := as.POSIXct(datetime)]
  }
  
  # Stunde extrahieren für Session-Filter
  dt[, hour := hour(datetime)]
  dt[, date := as.Date(datetime)]
  
  cat("Gesamte Bars geladen:", nrow(dt), "\n")
  
  # -------------------------------------------------------------------------
  # 2. ATR BERECHNEN (Dynamische Volatilität)
  # -------------------------------------------------------------------------
  
  # ATR = Average True Range
  # True Range = max(High-Low, |High-PrevClose|, |Low-PrevClose|)
  atr_result <- ATR(HLC = cbind(dt$high, dt$low, dt$close), n = atr_period)
  dt[, atr := atr_result[, "atr"]]
  
  # Prozentuale ATR (für Vergleichbarkeit)
  dt[, atr_pct := atr / close * 100]
  
  cat("ATR berechnet. Median ATR%:", round(median(dt$atr_pct, na.rm = TRUE), 4), "%\n")
  
  # -------------------------------------------------------------------------
  # 3. SESSION FILTER
  # -------------------------------------------------------------------------
  
  # Nur Bars innerhalb der Trading-Session
  dt[, in_session := hour >= session_start & hour < session_end]
  
  # Berechne verbleibende Bars bis Session-Ende für jeden Zeitpunkt
  dt[, bars_until_session_end := {
    session_end_time <- as.POSIXct(paste(date, sprintf("%02d:00:00", session_end)))
    pmax(0, as.numeric(difftime(session_end_time, datetime, units = "mins")) / 15)
  }]
  
  cat("Bars in Session:", sum(dt$in_session), "\n")
  cat("Bars außerhalb Session:", sum(!dt$in_session), "\n\n")
  
  # -------------------------------------------------------------------------
  # 4. TRIPLE BARRIER LABELING
  # -------------------------------------------------------------------------
  
  # Initialisiere Label-Spalten
  dt[, `:=`(
    label = NA_integer_,           # -1 (Short), 0 (Neutral), +1 (Long)
    barrier_touched = NA_character_, # "upper", "lower", "vertical"
    bars_to_exit = NA_integer_,    # Wie viele Bars bis Exit
    realized_return = NA_real_,    # Realisierter Return
    upper_barrier = NA_real_,      # Obere Barriere (für Debugging)
    lower_barrier = NA_real_,      # Untere Barriere (für Debugging)
    effective_horizon = NA_integer_ # Tatsächlich verwendeter Horizont
  )]
  
  # Nur Bars labeln die:
  # 1. In der Session sind
  # 2. Genug ATR haben
  # 3. Genug Bars bis Session-Ende haben
  valid_indices <- which(
    dt$in_session & 
      !is.na(dt$atr) & 
      dt$atr > min_atr &
      dt$bars_until_session_end >= 4  # Mindestens 1h bis Session-Ende
  )
  
  cat("Labeling", length(valid_indices), "gültige Observations...\n")
  
  # Progress tracking
  n_total <- length(valid_indices)
  progress_step <- max(1, floor(n_total / 10))
  
  for(idx in seq_along(valid_indices)) {
    
    i <- valid_indices[idx]
    
    # Progress
    if(idx %% progress_step == 0) {
      cat(sprintf("  Progress: %d%% (%d/%d)\n", 
                  round(idx/n_total*100), idx, n_total))
    }
    
    entry_price <- dt$close[i]
    current_atr <- dt$atr[i]
    
    # SYMMETRISCHE Barrieren
    upper_barrier <- entry_price + (atr_mult_barrier * current_atr)
    lower_barrier <- entry_price - (atr_mult_barrier * current_atr)
    
    dt$upper_barrier[i] <- upper_barrier
    dt$lower_barrier[i] <- lower_barrier
    
    # Effektiver Horizont: Minimum aus max_horizon und verbleibende Session-Bars
    effective_horizon <- min(
      max_horizon_bars, 
      floor(dt$bars_until_session_end[i]) - 1
    )
    dt$effective_horizon[i] <- effective_horizon
    
    if(effective_horizon < 1) next
    
    # -----------------------------------------------------------------------
    # Suche erste Barrier-Berührung
    # -----------------------------------------------------------------------
    
    for(j in (i+1):min(i + effective_horizon, nrow(dt))) {
      
      bars_elapsed <- j - i
      
      # Prüfe ob wir noch in der Session sind
      if(!dt$in_session[j]) {
        # Session beendet - behandle wie Vertical Barrier
        final_return <- (dt$close[j-1] - entry_price) / entry_price
        
        if(vertical_label_mode == "zero") {
          dt$label[i] <- 0L
        } else {
          dt$label[i] <- as.integer(sign(final_return))
        }
        
        dt$barrier_touched[i] <- "session_end"
        dt$bars_to_exit[i] <- bars_elapsed - 1
        dt$realized_return[i] <- final_return
        break
      }
      
      # Upper Barrier Check (Preis erreicht obere Grenze)
      hit_upper <- dt$high[j] >= upper_barrier
      
      # Lower Barrier Check (Preis erreicht untere Grenze)
      hit_lower <- dt$low[j] <= lower_barrier
      
      # Beide Barrieren im selben Bar?
      if(hit_upper && hit_lower) {
        # Annahme: Preis war näher an einer der Barrieren
        # Verwende Open-Preis als Indikator
        dist_to_upper <- upper_barrier - dt$open[j]
        dist_to_lower <- dt$open[j] - lower_barrier
        
        if(dist_to_upper < dist_to_lower) {
          dt$label[i] <- 1L
          dt$barrier_touched[i] <- "upper_first"
          dt$realized_return[i] <- (upper_barrier - entry_price) / entry_price
        } else {
          dt$label[i] <- -1L
          dt$barrier_touched[i] <- "lower_first"
          dt$realized_return[i] <- (entry_price - lower_barrier) / entry_price
        }
        dt$bars_to_exit[i] <- bars_elapsed
        break
      }
      
      # Nur Upper Barrier
      if(hit_upper) {
        dt$label[i] <- 1L
        dt$barrier_touched[i] <- "upper"
        dt$bars_to_exit[i] <- bars_elapsed
        dt$realized_return[i] <- (upper_barrier - entry_price) / entry_price
        break
      }
      
      # Nur Lower Barrier
      if(hit_lower) {
        dt$label[i] <- -1L
        dt$barrier_touched[i] <- "lower"
        dt$bars_to_exit[i] <- bars_elapsed
        dt$realized_return[i] <- (entry_price - lower_barrier) / entry_price
        break
      }
      
      # Vertical Barrier (Zeit abgelaufen)
      if(bars_elapsed >= effective_horizon) {
        final_return <- (dt$close[j] - entry_price) / entry_price
        
        if(vertical_label_mode == "zero") {
          dt$label[i] <- 0L
        } else {
          # Label basiert auf Return-Vorzeichen
          # Kleine Returns (< 0.1 * ATR) als neutral
          if(abs(final_return) < 0.1 * current_atr / entry_price) {
            dt$label[i] <- 0L
          } else {
            dt$label[i] <- as.integer(sign(final_return))
          }
        }
        
        dt$barrier_touched[i] <- "vertical"
        dt$bars_to_exit[i] <- bars_elapsed
        dt$realized_return[i] <- final_return
        break
      }
    }
  }
  
  cat("\nLabeling abgeschlossen.\n")
  
  # -------------------------------------------------------------------------
  # 5. ERGEBNIS ZUSAMMENFASSEN
  # -------------------------------------------------------------------------
  
  labeled_dt <- dt[!is.na(label)]
  
  cat("\n=== LABEL STATISTIKEN ===\n")
  cat("Total labeled:", nrow(labeled_dt), "\n")
  cat("\nLabel-Verteilung:\n")
  print(table(labeled_dt$label))
  cat("\nProzentual:\n")
  print(round(prop.table(table(labeled_dt$label)) * 100, 1))
  
  cat("\nBarrier Touch Distribution:\n")
  print(table(labeled_dt$barrier_touched))
  
  cat("\nDurchschnittliche Bars bis Exit:\n")
  print(labeled_dt[, .(
    mean_bars = round(mean(bars_to_exit), 1),
    median_bars = median(bars_to_exit),
    min_bars = min(bars_to_exit),
    max_bars = max(bars_to_exit)
  ), by = barrier_touched])
  
  cat("\nRealisierte Returns (%):\n")
  print(labeled_dt[, .(
    mean_return = round(mean(realized_return) * 100, 3),
    median_return = round(median(realized_return) * 100, 3),
    std_return = round(sd(realized_return) * 100, 3)
  ), by = label])
  
  return(labeled_dt)
}


# =============================================================================
# HILFSFUNKTION: LABEL QUALITÄT PRÜFEN
# =============================================================================

analyze_label_quality <- function(labeled_data) {
  
  cat("\n=== LABEL QUALITÄTSANALYSE ===\n")
  
  dt <- copy(labeled_data)
  
  # 1. Class Balance
  label_dist <- table(dt$label)
  total <- sum(label_dist)
  
  cat("\n1. CLASS BALANCE:\n")
  for(l in names(label_dist)) {
    pct <- label_dist[l] / total * 100
    balance_status <- ifelse(pct > 20 & pct < 40, "✓ OK", "⚠ Unbalanced")
    cat(sprintf("   Label %s: %d (%.1f%%) %s\n", l, label_dist[l], pct, balance_status))
  }
  
  # 2. Temporal Distribution
  cat("\n2. ZEITLICHE VERTEILUNG:\n")
  dt[, hour := hour(datetime)]
  hourly_dist <- dt[, .N, by = .(hour, label)]
  hourly_wide <- dcast(hourly_dist, hour ~ label, value.var = "N", fill = 0)
  print(hourly_wide)
  
  # 3. Autocorrelation Check (Label-Clustering)
  cat("\n3. LABEL AUTOKORRELATION:\n")
  label_numeric <- as.numeric(dt$label)
  if(length(label_numeric) > 50) {
    acf_1 <- cor(label_numeric[-length(label_numeric)], 
                 label_numeric[-1], 
                 use = "complete.obs")
    cat(sprintf("   Lag-1 Autokorrelation: %.3f\n", acf_1))
    if(abs(acf_1) > 0.3) {
      cat("   ⚠ WARNUNG: Hohe Autokorrelation - Labels sind geclustert!\n")
      cat("   → Verwende Sample Weights oder erhöhe Mindestabstand zwischen Samples\n")
    } else {
      cat("   ✓ OK - Labels sind nicht stark autokorreliert\n")
    }
  }
  
  # 4. Return Distribution per Label
  cat("\n4. RETURN-VERTEILUNG PRO LABEL:\n")
  return_stats <- dt[, .(
    count = .N,
    mean_ret = mean(realized_return) * 100,
    std_ret = sd(realized_return) * 100,
    sharpe = mean(realized_return) / sd(realized_return) * sqrt(252 * 26)
  ), by = label]
  print(return_stats)
  
  # 5. Holding Period Analysis
  cat("\n5. HALTEDAUER-ANALYSE:\n")
  hold_stats <- dt[, .(
    mean_bars = mean(bars_to_exit),
    median_bars = median(bars_to_exit),
    pct_quick = sum(bars_to_exit <= 4) / .N * 100,  # < 1h
    pct_slow = sum(bars_to_exit >= 20) / .N * 100   # > 5h
  ), by = label]
  print(hold_stats)
  
  return(invisible(NULL))
}


# =============================================================================
# VISUALISIERUNG
# =============================================================================

plot_label_distribution <- function(labeled_data) {
  
  dt <- copy(labeled_data)
  dt[, hour := hour(datetime)]
  dt[, label_factor := factor(label, levels = c(-1, 0, 1), 
                              labels = c("Short (-1)", "Neutral (0)", "Long (+1)"))]
  
  # Plot 1: Label Distribution
  p1 <- ggplot(dt, aes(x = label_factor, fill = label_factor)) +
    geom_bar() +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C", 
                                 "Neutral (0)" = "#95A5A6", 
                                 "Long (+1)" = "#27AE60")) +
    labs(title = "Label-Verteilung",
         subtitle = "Triple Barrier Labeling",
         x = "Label", y = "Anzahl") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Plot 2: Labels über Zeit (Stunde)
  hourly <- dt[, .N, by = .(hour, label_factor)]
  
  p2 <- ggplot(hourly, aes(x = hour, y = N, fill = label_factor)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C", 
                                 "Neutral (0)" = "#95A5A6", 
                                 "Long (+1)" = "#27AE60")) +
    labs(title = "Labels nach Tageszeit",
         x = "Stunde", y = "Anzahl", fill = "Label") +
    theme_minimal()
  
  # Plot 3: Barrier Touch Distribution
  p3 <- ggplot(dt, aes(x = barrier_touched, fill = label_factor)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C", 
                                 "Neutral (0)" = "#95A5A6", 
                                 "Long (+1)" = "#27AE60")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Labels pro Barrier-Typ",
         x = "Barrier", y = "Anteil", fill = "Label") +
    theme_minimal()
  
  # Plot 4: Holding Period Distribution
  p4 <- ggplot(dt, aes(x = bars_to_exit, fill = label_factor)) +
    geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C", 
                                 "Neutral (0)" = "#95A5A6", 
                                 "Long (+1)" = "#27AE60")) +
    labs(title = "Haltedauer-Verteilung",
         subtitle = "Bars bis zum Exit",
         x = "Bars bis Exit", y = "Anzahl", fill = "Label") +
    theme_minimal()
  
  # Kombiniere Plots
  if(requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  } else {
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }
}


# =============================================================================
# BEISPIEL: DATEN LADEN UND LABELN
# =============================================================================

# Beispiel-Verwendung (auskommentiert):
#
# # 1. Daten laden
# gold <- fread("path/to/gold_15min.csv")
# 
# # 2. Labels erstellen
# labeled_gold <- create_triple_barrier_labels(
#   prices = gold,
#   atr_period = 14,
#   atr_mult_barrier = 1.5,
#   max_horizon_bars = 24,
#   session_start = 8,
#   session_end = 19
# )
# 
# # 3. Qualität prüfen
# analyze_label_quality(labeled_gold)
# 
# # 4. Visualisieren
# plot_label_distribution(labeled_gold)
# 
# # 5. Speichern
# fwrite(labeled_gold, "gold_15min_labeled.csv")


# =============================================================================
# PARAMETER-OPTIMIERUNG GRID
# =============================================================================

optimize_labeling_parameters <- function(prices, 
                                         atr_periods = c(8, 10, 14, 20),
                                         atr_mults = c(1.0, 1.5, 2.0),
                                         max_horizons = c(12, 16, 24, 32)) {
  
  cat("=== PARAMETER OPTIMIERUNG ===\n")
  cat("Teste", length(atr_periods) * length(atr_mults) * length(max_horizons), 
      "Kombinationen...\n\n")
  
  results <- data.table()
  
  for(atr_p in atr_periods) {
    for(atr_m in atr_mults) {
      for(max_h in max_horizons) {
        
        # Labels erstellen (leise)
        sink("/dev/null")
        labeled <- tryCatch({
          create_triple_barrier_labels(
            prices = prices,
            atr_period = atr_p,
            atr_mult_barrier = atr_m,
            max_horizon_bars = max_h
          )
        }, error = function(e) NULL)
        sink()
        
        if(is.null(labeled)) next
        
        # Metriken berechnen
        label_counts <- table(labeled$label)
        total <- sum(label_counts)
        
        # Class Balance Score (0 = perfekt balanced, 1 = total unbalanced)
        expected <- total / length(label_counts)
        balance_score <- sum(abs(label_counts - expected)) / (2 * total)
        
        # Vertical Barrier Ratio (niedriger = besser)
        vertical_ratio <- sum(labeled$barrier_touched == "vertical") / nrow(labeled)
        
        # Average Holding Period
        avg_hold <- mean(labeled$bars_to_exit)
        
        results <- rbind(results, data.table(
          atr_period = atr_p,
          atr_mult = atr_m,
          max_horizon = max_h,
          n_samples = nrow(labeled),
          pct_long = round(label_counts["1"] / total * 100, 1),
          pct_short = round(label_counts["-1"] / total * 100, 1),
          pct_neutral = round(ifelse("0" %in% names(label_counts), 
                                     label_counts["0"] / total * 100, 0), 1),
          balance_score = round(balance_score, 3),
          vertical_ratio = round(vertical_ratio, 3),
          avg_hold_bars = round(avg_hold, 1)
        ))
      }
    }
  }
  
  # Sortiere nach Balance Score
  results <- results[order(balance_score)]
  
  cat("Top 5 Parameter-Kombinationen (nach Balance):\n")
  print(head(results, 10))
  
  return(results)
}


cat("\n=== TRIPLE BARRIER LABELING SCRIPT GELADEN ===\n")
cat("Verfügbare Funktionen:\n")
cat("  - create_triple_barrier_labels(prices, ...)\n")
cat("  - analyze_label_quality(labeled_data)\n")
cat("  - plot_label_distribution(labeled_data)\n")
cat("  - optimize_labeling_parameters(prices, ...)\n")
cat("\nBeispiel:\n")
cat("  labeled <- create_triple_barrier_labels(gold_15min)\n")
cat("  analyze_label_quality(labeled)\n")