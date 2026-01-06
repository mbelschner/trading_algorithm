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
# HINWEIS: Die Hauptfunktion create_triple_barrier_labels() ist jetzt in
# 01_triple_barrier_labeling_optimized.R (viel schneller!)
# Diese Datei enthält nur noch Helper-Funktionen und Analyse-Tools
# =============================================================================


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
# NEUTRAL THRESHOLD TESTING
# =============================================================================

test_neutral_thresholds <- function(labeled_data,
                                    thresholds = c(0.05, 0.1, 0.15, 0.2, 0.25)) {

  cat("\n=== NEUTRAL THRESHOLD TESTING ===\n")
  cat("Teste", length(thresholds), "verschiedene Schwellenwerte für Neutral-Labels...\n\n")

  results <- rbindlist(lapply(thresholds, function(thr) {

    # Re-label mit neuem Threshold
    labeled_copy <- copy(labeled_data)

    # Finde Vertical Barrier Cases
    vertical_cases <- labeled_copy$barrier_touched == "vertical"

    # Berechne Threshold in absoluten Preiseinheiten (thr * ATR)
    # Wenn abs(realized_return) < threshold * ATR / entry_price -> Label = 0
    labeled_copy[vertical_cases &
                 abs(realized_return) < thr * atr / close,
                 label := 0L]

    # Berechne Label-Verteilung
    label_counts <- table(labeled_copy$label)
    total <- sum(label_counts)

    pct_neutral <- label_counts["0"] / total * 100
    pct_long <- label_counts["1"] / total * 100
    pct_short <- label_counts["-1"] / total * 100

    # Balance = Differenz zwischen Long/Short (0 = perfekt balanced)
    balance <- abs(pct_long - pct_short)

    data.table(
      threshold = thr,
      pct_neutral = round(pct_neutral, 2),
      pct_long = round(pct_long, 2),
      pct_short = round(pct_short, 2),
      balance = round(balance, 2),
      n_neutral = as.numeric(label_counts["0"]),
      n_long = as.numeric(label_counts["1"]),
      n_short = as.numeric(label_counts["-1"])
    )
  }))

  cat("=== ERGEBNISSE ===\n")
  print(results[, .(threshold, pct_neutral, pct_long, pct_short, balance)])

  cat("\n=== INTERPRETATION ===\n")
  cat("threshold: ATR-Multiplikator für Neutral-Schwelle\n")
  cat("  -> Label wird 0 wenn abs(return) < threshold * ATR\n")
  cat("pct_neutral: % der Labels die Neutral (0) sind\n")
  cat("balance: Differenz zwischen Long/Short % (niedriger = besser)\n\n")

  # Finde besten Threshold (höchste Neutral % bei akzeptabler Balance)
  # Akzeptable Balance: < 5%
  acceptable <- results[balance < 5]

  if(nrow(acceptable) > 0) {
    best_idx <- which.max(acceptable$pct_neutral)
    best <- acceptable[best_idx]
    cat("=== EMPFEHLUNG ===\n")
    cat(sprintf("Bester Threshold: %.2f (Balance: %.2f%%, Neutral: %.2f%%)\n",
                best$threshold, best$balance, best$pct_neutral))
  } else {
    cat("WARNUNG: Kein Threshold mit Balance < 5% gefunden.\n")
    cat("Erwäge kleinere Thresholds oder asymmetrische Barrieren.\n")
  }

  return(results)
}


# =============================================================================
# SAMPLE WEIGHTS: ÜBERLAPPENDE LABELS
# =============================================================================

calculate_sample_weights <- function(labeled_data) {

  cat("\n=== BERECHNE SAMPLE WEIGHTS ===\n")

  dt <- copy(labeled_data)

  # Berechne Entry und Exit Zeiten (bars_to_exit * 15 Minuten)
  dt[, entry_time := datetime]
  dt[, exit_time := datetime + bars_to_exit * 15 * 60]

  cat("Berechne überlappende Labels (optimierter Algorithmus)...\n")

  # OPTIMIERTER ALGORITHMUS: O(n log n) statt O(n²)
  #
  # Alte Methode: Für jedes Label alle anderen Labels durchgehen
  #   - 1,000 Labels   = ~1,000,000 Vergleiche
  #   - 6,000 Labels   = ~36,000,000 Vergleiche (36x mehr!)
  #   - 100,000 Labels = ~10,000,000,000 Vergleiche
  #
  # Neue Methode: Event-basiertes Counting
  #   - Erstelle Events für Entry (+1) und Exit (-1)
  #   - Sortiere Events (O(n log n))
  #   - Ein Durchgang durch Events (O(n))
  #   - Gesamt: O(n log n) - viel schneller!
  #
  # Speedup: Bei 6,000 Labels: ~200x schneller!
  # Verwende Interval Overlap mit Event-basiertem Counting

  n <- nrow(dt)
  dt[, idx := 1:.N]  # Original Index

  # Erstelle Events: +1 für Entry, -1 für Exit
  events <- rbindlist(list(
    dt[, .(time = entry_time, type = 1L, idx = idx)],   # Entry event
    dt[, .(time = exit_time, type = -1L, idx = idx)]    # Exit event
  ))

  # Sortiere Events nach Zeit (bei Gleichstand: Entry vor Exit)
  setorder(events, time, -type)

  # Zähle aktive Intervals für jeden Zeitpunkt
  active_count <- 0
  concurrent_map <- integer(n)

  # Tracking welche Intervals gerade aktiv sind
  for(i in 1:nrow(events)) {
    if(events$type[i] == 1L) {
      # Entry: Zähle aktuelle aktive Intervals (inklusive sich selbst)
      active_count <- active_count + 1L
      concurrent_map[events$idx[i]] <- active_count
    } else {
      # Exit: Reduziere Count
      active_count <- active_count - 1L
    }
  }

  dt[, n_concurrent := concurrent_map[idx]]
  dt[, idx := NULL]

  # Sample Weight = 1 / Anzahl überlappender Labels
  dt[, sample_weight := 1.0 / n_concurrent]

  cat(sprintf("Sample Weights berechnet für %d Labels.\n", nrow(dt)))

  return(dt)
}


analyze_sample_weights <- function(labeled_data) {

  cat("\n=== SAMPLE WEIGHTS ANALYSE ===\n")

  dt <- copy(labeled_data)

  cat("\n1. CONCURRENT LABELS STATISTIK:\n")
  cat(sprintf("   Durchschnittliche Anzahl überlappender Labels: %.2f\n",
              mean(dt$n_concurrent)))
  cat(sprintf("   Median überlappende Labels: %.0f\n",
              median(dt$n_concurrent)))
  cat(sprintf("   Max überlappende Labels: %.0f\n",
              max(dt$n_concurrent)))
  cat(sprintf("   Min überlappende Labels: %.0f\n",
              min(dt$n_concurrent)))

  cat("\n2. SAMPLE WEIGHTS STATISTIK:\n")
  cat(sprintf("   Durchschnittliches Sample Weight: %.4f\n",
              mean(dt$sample_weight)))
  cat(sprintf("   Median Sample Weight: %.4f\n",
              median(dt$sample_weight)))
  cat(sprintf("   Min Sample Weight: %.4f\n",
              min(dt$sample_weight)))
  cat(sprintf("   Max Sample Weight: %.4f\n",
              max(dt$sample_weight)))

  cat("\n3. VERTEILUNG DER CONCURRENT LABELS:\n")
  concurrent_table <- table(dt$n_concurrent)
  concurrent_df <- data.frame(
    n_concurrent = as.numeric(names(concurrent_table)),
    count = as.numeric(concurrent_table),
    pct = round(as.numeric(concurrent_table) / sum(concurrent_table) * 100, 1)
  )
  print(head(concurrent_df, 10))

  cat("\n4. EFFECTIVE SAMPLE SIZE:\n")
  effective_n <- sum(dt$sample_weight)
  cat(sprintf("   Original Samples: %d\n", nrow(dt)))
  cat(sprintf("   Effective Samples (gewichtet): %.1f\n", effective_n))
  cat(sprintf("   Reduktion: %.1f%%\n",
              (1 - effective_n/nrow(dt)) * 100))

  cat("\n5. WEIGHTS PRO LABEL:\n")
  weight_by_label <- dt[, .(
    count = .N,
    mean_weight = round(mean(sample_weight), 4),
    median_weight = round(median(sample_weight), 4),
    sum_weight = round(sum(sample_weight), 1),
    effective_pct = round(sum(sample_weight) / sum(dt$sample_weight) * 100, 1)
  ), by = label]
  print(weight_by_label)

  return(invisible(NULL))
}


plot_sample_weights <- function(labeled_data) {

  dt <- copy(labeled_data)
  dt[, hour := hour(datetime)]
  dt[, label_factor := factor(label, levels = c(-1, 0, 1),
                              labels = c("Short (-1)", "Neutral (0)", "Long (+1)"))]

  # Plot 1: Verteilung der Concurrent Labels
  p1 <- ggplot(dt, aes(x = n_concurrent)) +
    geom_histogram(binwidth = 1, fill = "#3498DB", color = "white") +
    labs(title = "Verteilung der überlappenden Labels",
         subtitle = "Wie viele Labels sind gleichzeitig aktiv?",
         x = "Anzahl überlappender Labels",
         y = "Häufigkeit") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 2: Sample Weights Verteilung
  p2 <- ggplot(dt, aes(x = sample_weight)) +
    geom_histogram(bins = 50, fill = "#E74C3C", color = "white") +
    labs(title = "Verteilung der Sample Weights",
         subtitle = "Weight = 1 / Anzahl überlappender Labels",
         x = "Sample Weight",
         y = "Häufigkeit") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 3: Concurrent Labels über Zeit
  hourly_concurrent <- dt[, .(
    mean_concurrent = mean(n_concurrent),
    median_concurrent = median(n_concurrent)
  ), by = hour]

  p3 <- ggplot(hourly_concurrent, aes(x = hour)) +
    geom_line(aes(y = mean_concurrent, color = "Durchschnitt"), size = 1) +
    geom_line(aes(y = median_concurrent, color = "Median"), size = 1) +
    scale_color_manual(values = c("Durchschnitt" = "#3498DB", "Median" = "#E67E22")) +
    labs(title = "Überlappende Labels nach Tageszeit",
         x = "Stunde",
         y = "Anzahl überlappender Labels",
         color = "") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

  # Plot 4: Sample Weights pro Label
  p4 <- ggplot(dt, aes(x = label_factor, y = sample_weight, fill = label_factor)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C",
                                  "Neutral (0)" = "#95A5A6",
                                  "Long (+1)" = "#27AE60")) +
    labs(title = "Sample Weights pro Label",
         x = "Label",
         y = "Sample Weight") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "none")

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
                                         max_horizons = c(12, 16, 24, 32),
                                         sort_by = "uniqueness") {  # "uniqueness", "balance", or "combined"

  cat("=== PARAMETER OPTIMIERUNG ===\n")
  cat("Teste", length(atr_periods) * length(atr_mults) * length(max_horizons),
      "Kombinationen...\n")
  cat("Sortierung nach:", sort_by, "\n\n")

  results <- data.table()

  for(atr_p in atr_periods) {
    for(atr_m in atr_mults) {
      for(max_h in max_horizons) {

        cat(sprintf("Testing: ATR=%d, Mult=%.1f, Horizon=%d...\n", atr_p, atr_m, max_h))

        # Labels erstellen (unterdrücke Output mit capture.output)
        labeled <- tryCatch({
          invisible(capture.output({
            result <- create_triple_barrier_labels(
              prices = prices,
              atr_period = atr_p,
              atr_mult_barrier = atr_m,
              max_horizon_bars = max_h
            )
          }))
          result
        }, error = function(e) {
          cat("  ✗ Fehler:", e$message, "\n")
          NULL
        })

        if(is.null(labeled)) {
          cat("  ✗ Fehler beim Labeling\n")
          next
        }

        # WICHTIG: Berechne Sample Weights (Uniqueness)
        labeled_weighted <- suppressMessages(calculate_sample_weights(labeled))

        # Metriken berechnen
        label_counts <- table(labeled$label)
        total <- sum(label_counts)

        # 1. Class Balance Score (0 = perfekt balanced, 1 = total unbalanced)
        expected <- total / length(label_counts)
        balance_score <- sum(abs(label_counts - expected)) / (2 * total)

        # 2. Uniqueness Score (höher = besser)
        # = Durchschnittliches Sample Weight = 1 / durchschnittliche Anzahl überlappender Labels
        avg_uniqueness <- mean(labeled_weighted$sample_weight)

        # 3. Vertical Barrier Ratio (niedriger = besser)
        vertical_ratio <- sum(labeled$barrier_touched == "vertical") / nrow(labeled)

        # 4. Average Holding Period
        avg_hold <- mean(labeled$bars_to_exit)

        # 5. Concurrent Labels Statistics
        mean_concurrent <- mean(labeled_weighted$n_concurrent)
        median_concurrent <- median(labeled_weighted$n_concurrent)
        max_concurrent <- max(labeled_weighted$n_concurrent)

        cat(sprintf("  ✓ Samples=%d, Uniqueness=%.3f, Balance=%.3f, Concurrent=%.1f\n",
                    nrow(labeled), avg_uniqueness, balance_score, mean_concurrent))

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
          avg_uniqueness = round(avg_uniqueness, 4),
          mean_concurrent = round(mean_concurrent, 1),
          median_concurrent = median_concurrent,
          max_concurrent = max_concurrent,
          vertical_ratio = round(vertical_ratio, 3),
          avg_hold_bars = round(avg_hold, 1)
        ))
      }
    }
  }

  # Sortiere nach gewähltem Kriterium
  if(sort_by == "uniqueness") {
    # Sortiere nach Uniqueness (höher = besser), dann Balance
    results <- results[order(-avg_uniqueness, balance_score)]
    cat("\n✓ Sortiert nach: Uniqueness (höher = besser), dann Balance\n")

  } else if(sort_by == "balance") {
    # Sortiere nach Balance (niedriger = besser), dann Uniqueness
    results <- results[order(balance_score, -avg_uniqueness)]
    cat("\n✓ Sortiert nach: Balance (niedriger = besser), dann Uniqueness\n")

  } else if(sort_by == "combined") {
    # Kombinierter Score: 70% Uniqueness + 30% Balance
    # Normalisiere beide auf 0-1 Skala
    results[, uniqueness_norm := (avg_uniqueness - min(avg_uniqueness)) /
              (max(avg_uniqueness) - min(avg_uniqueness))]
    results[, balance_norm := 1 - ((balance_score - min(balance_score)) /
              (max(balance_score) - min(balance_score)))]  # Invertiert (niedrig = gut)

    results[, combined_score := 0.7 * uniqueness_norm + 0.3 * balance_norm]
    results <- results[order(-combined_score)]
    cat("\n✓ Sortiert nach: Kombinierter Score (70% Uniqueness + 30% Balance)\n")
  }

  cat("\n=== TOP 10 PARAMETER-KOMBINATIONEN ===\n")
  if(sort_by == "combined") {
    print(results[, .(atr_period, atr_mult, max_horizon, n_samples,
                      avg_uniqueness, balance_score, mean_concurrent,
                      vertical_ratio, combined_score)][1:min(10, nrow(results))])
  } else {
    print(results[, .(atr_period, atr_mult, max_horizon, n_samples,
                      avg_uniqueness, balance_score, mean_concurrent,
                      vertical_ratio, avg_hold_bars)][1:min(10, nrow(results))])
  }

  return(results)
}


# =============================================================================
# HORIZON IMPACT ANALYSE
# =============================================================================

test_horizon_impact <- function(prices,
                                horizons = c(8, 12, 16, 20, 24, 28, 32),
                                atr_period = 14,
                                atr_mult_barrier = 1.5,
                                session_start = 8,
                                session_end = 21) {

  cat("=== HORIZON IMPACT ANALYSE ===\n")
  cat("Teste", length(horizons), "verschiedene Max Horizons...\n\n")

  results <- rbindlist(lapply(horizons, function(h) {

    cat(sprintf("Testing max_horizon = %d bars...\n", h))

    # Erstelle Labels (unterdrücke Output)
    labeled <- suppressMessages(create_triple_barrier_labels(
      prices = prices,
      atr_period = atr_period,
      atr_mult_barrier = atr_mult_barrier,
      max_horizon_bars = h,
      session_start = session_start,
      session_end = session_end
    ))

    # Berechne Concurrent Labels (verwende optimierte Funktion)
    labeled_weighted <- calculate_sample_weights(labeled)
    labeled <- labeled_weighted

    # Label-Verteilung
    label_dist <- table(labeled$label)

    # Statistiken
    data.table(
      max_horizon = h,
      n_samples = nrow(labeled),
      mean_concurrent = round(mean(labeled$n_concurrent), 2),
      median_concurrent = median(labeled$n_concurrent),
      max_concurrent = max(labeled$n_concurrent),
      min_concurrent = min(labeled$n_concurrent),
      mean_holding_bars = round(mean(labeled$bars_to_exit), 2),
      pct_long = round(label_dist["1"] / sum(label_dist) * 100, 1),
      pct_short = round(label_dist["-1"] / sum(label_dist) * 100, 1),
      pct_neutral = round(ifelse("0" %in% names(label_dist),
                                  label_dist["0"] / sum(label_dist) * 100, 0), 1),
      pct_vertical = round(mean(labeled$barrier_touched == "vertical") * 100, 1),
      avg_uniqueness = round(mean(1 / labeled$n_concurrent), 4)
    )
  }))

  cat("\n=== ERGEBNISSE ===\n")
  print(results)

  return(results)
}


plot_horizon_impact <- function(horizon_results) {

  dt <- copy(horizon_results)

  # Plot 1: Concurrent Labels vs Horizon
  p1 <- ggplot(dt, aes(x = max_horizon)) +
    geom_line(aes(y = mean_concurrent, color = "Mean Concurrent"), size = 1.2) +
    geom_line(aes(y = median_concurrent, color = "Median Concurrent"), size = 1.2) +
    geom_point(aes(y = mean_concurrent, color = "Mean Concurrent"), size = 2) +
    geom_point(aes(y = median_concurrent, color = "Median Concurrent"), size = 2) +
    scale_color_manual(values = c("Mean Concurrent" = "#3498DB",
                                   "Median Concurrent" = "#E67E22")) +
    labs(title = "Überlappende Labels vs Max Horizon",
         x = "Max Horizon (bars)",
         y = "Anzahl überlappender Labels",
         color = "") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

  # Plot 2: Uniqueness Score (1/n_concurrent)
  p2 <- ggplot(dt, aes(x = max_horizon, y = avg_uniqueness)) +
    geom_line(color = "#E74C3C", size = 1.2) +
    geom_point(color = "#E74C3C", size = 3) +
    labs(title = "Average Sample Uniqueness vs Max Horizon",
         subtitle = "Höher = Weniger Overlap",
         x = "Max Horizon (bars)",
         y = "Avg Uniqueness (1/n_concurrent)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 3: Label Distribution
  dt_long <- melt(dt, id.vars = "max_horizon",
                  measure.vars = c("pct_long", "pct_short", "pct_neutral"),
                  variable.name = "label_type", value.name = "percentage")

  p3 <- ggplot(dt_long, aes(x = max_horizon, y = percentage, fill = label_type)) +
    geom_area(alpha = 0.7, position = "stack") +
    scale_fill_manual(values = c("pct_long" = "#27AE60",
                                  "pct_short" = "#E74C3C",
                                  "pct_neutral" = "#95A5A6"),
                      labels = c("Long", "Short", "Neutral")) +
    labs(title = "Label-Verteilung vs Max Horizon",
         x = "Max Horizon (bars)",
         y = "Prozent",
         fill = "Label") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

  # Plot 4: Holding Period
  p4 <- ggplot(dt, aes(x = max_horizon, y = mean_holding_bars)) +
    geom_line(color = "#9B59B6", size = 1.2) +
    geom_point(color = "#9B59B6", size = 3) +
    geom_hline(yintercept = dt$max_horizon, linetype = "dashed", alpha = 0.3) +
    labs(title = "Durchschnittliche Haltedauer vs Max Horizon",
         x = "Max Horizon (bars)",
         y = "Mean Holding Period (bars)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

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
# HOLDING PERIOD ANALYSE
# =============================================================================

analyze_holding_period <- function(labeled_data, max_horizon) {

  cat("\n=== HOLDING PERIOD ANALYSE ===\n")
  cat(sprintf("Max Horizon: %d bars\n\n", max_horizon))

  dt <- copy(labeled_data)

  # 1. Grundstatistiken
  cat("1. GRUNDSTATISTIKEN:\n")
  cat(sprintf("   Durchschnittliche Haltedauer: %.2f bars (%.1f min)\n",
              mean(dt$bars_to_exit), mean(dt$bars_to_exit) * 15))
  cat(sprintf("   Median Haltedauer: %.0f bars (%.0f min)\n",
              median(dt$bars_to_exit), median(dt$bars_to_exit) * 15))
  cat(sprintf("   Std Dev: %.2f bars\n", sd(dt$bars_to_exit)))
  cat(sprintf("   Min: %d bars, Max: %d bars\n",
              min(dt$bars_to_exit), max(dt$bars_to_exit)))

  # 2. Verteilung nach Barrier Type
  cat("\n2. HALTEDAUER NACH BARRIER TYPE:\n")
  hold_by_barrier <- dt[, .(
    count = .N,
    mean_bars = round(mean(bars_to_exit), 2),
    median_bars = median(bars_to_exit),
    pct_of_max = round(mean(bars_to_exit) / max_horizon * 100, 1)
  ), by = barrier_touched]
  print(hold_by_barrier)

  # 3. Verteilung nach Label
  cat("\n3. HALTEDAUER NACH LABEL:\n")
  hold_by_label <- dt[, .(
    count = .N,
    mean_bars = round(mean(bars_to_exit), 2),
    median_bars = median(bars_to_exit),
    q25 = quantile(bars_to_exit, 0.25),
    q75 = quantile(bars_to_exit, 0.75)
  ), by = label]
  print(hold_by_label)

  # 4. Kategorisierung
  cat("\n4. KATEGORISIERUNG DER HALTEDAUER:\n")
  dt[, hold_category := fcase(
    bars_to_exit <= 4, "Sehr kurz (<=1h)",
    bars_to_exit <= 8, "Kurz (1-2h)",
    bars_to_exit <= 16, "Mittel (2-4h)",
    bars_to_exit <= 24, "Lang (4-6h)",
    default = "Sehr lang (>6h)"
  )]

  category_dist <- dt[, .N, by = hold_category][order(-N)]
  category_dist[, pct := round(N / sum(N) * 100, 1)]
  print(category_dist)

  # 5. Utilization Rate
  cat("\n5. HORIZON UTILIZATION:\n")
  cat(sprintf("   Durchschnittliche Utilization: %.1f%%\n",
              mean(dt$bars_to_exit) / max_horizon * 100))
  cat(sprintf("   Anzahl Trades die Max Horizon erreichen: %d (%.1f%%)\n",
              sum(dt$bars_to_exit >= max_horizon * 0.9),
              sum(dt$bars_to_exit >= max_horizon * 0.9) / nrow(dt) * 100))

  return(invisible(dt))
}


plot_holding_period_analysis <- function(labeled_data, max_horizon) {

  dt <- copy(labeled_data)
  dt[, label_factor := factor(label, levels = c(-1, 0, 1),
                              labels = c("Short (-1)", "Neutral (0)", "Long (+1)"))]

  # Plot 1: Distribution of Holding Periods
  p1 <- ggplot(dt, aes(x = bars_to_exit)) +
    geom_histogram(binwidth = 1, fill = "#3498DB", color = "white") +
    geom_vline(xintercept = max_horizon, color = "#E74C3C",
               linetype = "dashed", size = 1) +
    annotate("text", x = max_horizon, y = Inf,
             label = sprintf("Max Horizon = %d", max_horizon),
             vjust = 2, hjust = -0.1, color = "#E74C3C") +
    labs(title = "Verteilung der Haltedauern",
         x = "Bars bis Exit",
         y = "Häufigkeit") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Plot 2: By Label
  p2 <- ggplot(dt, aes(x = label_factor, y = bars_to_exit, fill = label_factor)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, alpha = 0.5) +
    scale_fill_manual(values = c("Short (-1)" = "#E74C3C",
                                  "Neutral (0)" = "#95A5A6",
                                  "Long (+1)" = "#27AE60")) +
    labs(title = "Haltedauer nach Label",
         x = "Label",
         y = "Bars bis Exit") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "none")

  # Plot 3: By Barrier Type
  p3 <- ggplot(dt, aes(x = barrier_touched, y = bars_to_exit, fill = barrier_touched)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_brewer(palette = "Set3") +
    labs(title = "Haltedauer nach Barrier Type",
         x = "Barrier Type",
         y = "Bars bis Exit") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))

  # Plot 4: Cumulative Distribution
  dt_sorted <- dt[order(bars_to_exit)]
  dt_sorted[, cumulative_pct := (1:.N) / .N * 100]

  p4 <- ggplot(dt_sorted, aes(x = bars_to_exit, y = cumulative_pct)) +
    geom_line(color = "#9B59B6", size = 1.2) +
    geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = median(dt$bars_to_exit),
               linetype = "dashed", alpha = 0.3) +
    labs(title = "Kumulative Verteilung der Haltedauer",
         x = "Bars bis Exit",
         y = "Kumulative % der Trades") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

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
# EXIT-BASED LABEL FILTERING (REMOVE OVERLAPS)
# =============================================================================
# Goal: Create non-overlapping labels by greedy selection
# Method: Select first label, wait until its exit, then select next label
# Benefits: Eliminates autocorrelation, creates truly independent samples

filter_labels_exit_based <- function(labeled_data,
                                     datetime_col = "datetime",
                                     label_col = "label",
                                     bars_to_exit_col = "bars_to_exit") {

  cat("\n=== EXIT-BASED LABEL FILTERING ===\n")
  cat(sprintf("[%s] Starting filtering process...\n", Sys.time()))

  # Validate inputs
  if(!datetime_col %in% names(labeled_data)) {
    stop(sprintf("Column '%s' not found in data", datetime_col))
  }
  if(!bars_to_exit_col %in% names(labeled_data)) {
    stop(sprintf("Column '%s' not found in data", bars_to_exit_col))
  }

  dt <- copy(labeled_data)
  n_original <- nrow(dt)

  # Sort by datetime (ascending)
  setorderv(dt, datetime_col)

  # Calculate exit times
  dt[, entry_time := get(datetime_col)]
  dt[, exit_time := get(datetime_col) + get(bars_to_exit_col) * 15 * 60]

  cat(sprintf("Original labels: %s\n", format(n_original, big.mark = ",")))
  cat("Applying greedy selection (first label, wait for exit, next label)...\n")

  # Greedy selection algorithm
  selected_indices <- integer()
  current_exit_time <- as.POSIXct("1900-01-01", tz = "UTC")

  for(i in 1:nrow(dt)) {
    entry <- dt$entry_time[i]

    # Can we enter? Only if current time >= last exit time
    if(entry >= current_exit_time) {
      selected_indices <- c(selected_indices, i)
      current_exit_time <- dt$exit_time[i]
    }
  }

  # Filter to selected labels only
  dt_filtered <- dt[selected_indices]
  n_filtered <- nrow(dt_filtered)

  # Remove temporary columns
  dt_filtered[, entry_time := NULL]
  dt_filtered[, exit_time := NULL]

  # Statistics
  reduction_pct <- (1 - n_filtered/n_original) * 100

  cat(sprintf("\n=== FILTERING RESULTS ===\n"))
  cat(sprintf("Original labels: %s\n", format(n_original, big.mark = ",")))
  cat(sprintf("Filtered labels: %s\n", format(n_filtered, big.mark = ",")))
  cat(sprintf("Reduction: %.1f%%\n", reduction_pct))
  cat(sprintf("Retention: %.1f%%\n", 100 - reduction_pct))

  # Label distribution before/after
  cat("\n=== LABEL DISTRIBUTION ===\n")
  cat("BEFORE filtering:\n")
  dist_before <- table(labeled_data[[label_col]])
  for(lbl in names(dist_before)) {
    pct <- dist_before[lbl] / n_original * 100
    cat(sprintf("  Label %s: %d (%.1f%%)\n", lbl, dist_before[lbl], pct))
  }

  cat("\nAFTER filtering:\n")
  dist_after <- table(dt_filtered[[label_col]])
  for(lbl in names(dist_after)) {
    pct <- dist_after[lbl] / n_filtered * 100
    cat(sprintf("  Label %s: %d (%.1f%%)\n", lbl, dist_after[lbl], pct))
  }

  cat(sprintf("\n[%s] Filtering complete!\n", Sys.time()))

  return(dt_filtered)
}


# =============================================================================
# AGGRESSIVE NEUTRAL RELABELING
# =============================================================================
# Re-labels samples to neutral based on multiple criteria:
# - Vertical/session exits
# - Small realized returns
# - Low sample weights (high overlap)

relabel_to_neutral <- function(labeled_data,
                                return_threshold = 0.0015,  # 0.15% - CONFIGURABLE
                                weight_threshold = 0.7,
                                preserve_original = TRUE) {

  dt <- copy(labeled_data)

  # Preserve original labels
  if(preserve_original) dt[, label_original := label]

  # Count before
  n_before_relabel <- sum(dt$label != 0)

  # Apply aggressive neutral relabeling (ANY condition triggers)
  dt[barrier_touched %in% c("vertical", "session_end") |
     abs(realized_return) < return_threshold |
     sample_weight < weight_threshold,
     label := 0L]

  # Count after & statistics
  n_after_relabel <- sum(dt$label != 0)
  n_relabeled <- n_before_relabel - n_after_relabel

  cat(sprintf("\n=== AGGRESSIVE NEUTRAL RELABELING ===\n"))
  cat(sprintf("Non-neutral before: %s\n", format(n_before_relabel, big.mark = ",")))
  cat(sprintf("Non-neutral after:  %s\n", format(n_after_relabel, big.mark = ",")))
  cat(sprintf("Relabeled to neutral: %s (%.1f%%)\n",
              format(n_relabeled, big.mark = ","),
              n_relabeled / n_before_relabel * 100))

  # Breakdown by criterion
  if(preserve_original) {
    dt_relabeled <- dt[label_original != 0 & label == 0]
    n_vertical <- sum(dt_relabeled$barrier_touched %in% c("vertical", "session_end"))
    n_small_return <- sum(abs(dt_relabeled$realized_return) < return_threshold)
    n_low_weight <- sum(dt_relabeled$sample_weight < weight_threshold)

    cat("\nRelabeling reasons (may overlap):\n")
    cat(sprintf("  Vertical/Session exits:  %s (%.1f%%)\n",
                format(n_vertical, big.mark = ","),
                n_vertical / n_relabeled * 100))
    cat(sprintf("  Small returns (< %.4f%%): %s (%.1f%%)\n",
                return_threshold * 100,
                format(n_small_return, big.mark = ","),
                n_small_return / n_relabeled * 100))
    cat(sprintf("  Low weights (< %.2f):     %s (%.1f%%)\n",
                weight_threshold,
                format(n_low_weight, big.mark = ","),
                n_low_weight / n_relabeled * 100))
  }

  # Validation warning
  pct_neutral <- sum(dt$label == 0) / nrow(dt) * 100
  if(pct_neutral > 70) {
    cat(sprintf("\n⚠ WARNING: %.1f%% of labels are now neutral - may be too aggressive!\n", pct_neutral))
  }

  return(dt)
}


# =============================================================================
# QUALITY METRICS REPORTING
# =============================================================================
# Standardized function to print quality metrics for labeled datasets

print_quality_metrics <- function(labeled_data, version_name) {
  cat(sprintf("\n--- %s VERSION QUALITY METRICS ---\n", version_name))
  cat(sprintf("Total labels: %s\n", format(nrow(labeled_data), big.mark = ",")))
  cat(sprintf("Mean sample weight: %.4f\n", mean(labeled_data$sample_weight)))
  cat(sprintf("Mean concurrent: %.2f\n", mean(labeled_data$n_concurrent)))

  # ACF
  labels_num <- as.numeric(labeled_data$label)
  acf_result <- acf(labels_num, lag.max = 10, plot = FALSE)

  # Handle NaN/NA in ACF (happens when all labels are the same)
  if(is.na(acf_result$acf[2]) || is.nan(acf_result$acf[2])) {
    cat("ACF Lag-1: N/A (constant series)\n")
  } else {
    cat(sprintf("ACF Lag-1: %.4f", acf_result$acf[2]))
    if(abs(acf_result$acf[2]) < 0.2) {
      cat(" ✅ (Target: < 0.2)\n")
    } else {
      cat(" ⚠ (Target: < 0.2)\n")
    }
  }

  cat("\nLabel distribution:\n")
  print(table(labeled_data$label))
  cat(sprintf("Percentages: Short=%.1f%%, Long=%.1f%%, Neutral=%.1f%%\n",
              prop.table(table(labeled_data$label))["-1"] * 100,
              prop.table(table(labeled_data$label))["1"] * 100,
              ifelse("0" %in% names(table(labeled_data$label)),
                     prop.table(table(labeled_data$label))["0"] * 100, 0)))

  return(invisible(acf_result))
}


# =============================================================================
# AUTOCORRELATION ANALYSIS & COMPARISON
# =============================================================================
# Analyzes autocorrelation before/after filtering and creates comparison plots

analyze_label_autocorrelation <- function(labeled_before,
                                         labeled_after = NULL,
                                         max_lag = 20,
                                         label_col = "label",
                                         output_path = "plots/label_quality",
                                         save_plot = TRUE) {

  cat("\n=== LABEL AUTOCORRELATION ANALYSIS ===\n")

  # Convert labels to numeric
  labels_before <- as.numeric(labeled_before[[label_col]])

  # Remove NAs
  labels_before <- na.omit(labels_before)

  if(length(labels_before) < max_lag + 1) {
    stop("Not enough labels for ACF analysis")
  }

  # Calculate ACF
  acf_before <- acf(labels_before, lag.max = max_lag, plot = FALSE)
  acf_vals_before <- as.numeric(acf_before$acf[-1])  # Remove lag 0

  # Create results data.table
  results <- data.table(
    lag = 1:max_lag,
    acf_before = acf_vals_before
  )

  # Statistics
  cat("\n=== ACF STATISTICS (BEFORE) ===\n")
  cat(sprintf("ACF Lag-1:  %.4f\n", acf_vals_before[1]))
  cat(sprintf("ACF Lag-5:  %.4f\n", acf_vals_before[5]))
  cat(sprintf("ACF Lag-10: %.4f\n", acf_vals_before[10]))
  cat(sprintf("Mean ACF (Lag 1-10): %.4f\n", mean(acf_vals_before[1:10])))

  # If we have "after" data, compare
  if(!is.null(labeled_after)) {
    labels_after <- as.numeric(labeled_after[[label_col]])
    labels_after <- na.omit(labels_after)

    if(length(labels_after) >= max_lag + 1) {
      acf_after <- acf(labels_after, lag.max = max_lag, plot = FALSE)
      acf_vals_after <- as.numeric(acf_after$acf[-1])
      results[, acf_after := acf_vals_after]

      cat("\n=== ACF STATISTICS (AFTER) ===\n")
      cat(sprintf("ACF Lag-1:  %.4f\n", acf_vals_after[1]))
      cat(sprintf("ACF Lag-5:  %.4f\n", acf_vals_after[5]))
      cat(sprintf("ACF Lag-10: %.4f\n", acf_vals_after[10]))
      cat(sprintf("Mean ACF (Lag 1-10): %.4f\n", mean(acf_vals_after[1:10])))

      cat("\n=== IMPROVEMENT ===\n")
      cat(sprintf("Lag-1 reduction:  %.4f -> %.4f (%.1f%%)\n",
                  acf_vals_before[1], acf_vals_after[1],
                  (1 - acf_vals_after[1]/acf_vals_before[1]) * 100))
      cat(sprintf("Mean ACF reduction: %.4f -> %.4f (%.1f%%)\n",
                  mean(acf_vals_before[1:10]), mean(acf_vals_after[1:10]),
                  (1 - mean(acf_vals_after[1:10])/mean(acf_vals_before[1:10])) * 100))
    }
  }

  # Create plot
  if(save_plot && !is.null(labeled_after) && "acf_after" %in% names(results)) {

    # Create output directory if needed
    if(!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
      cat(sprintf("\nCreated directory: %s\n", output_path))
    }

    # Reshape for plotting
    results_long <- melt(results, id.vars = "lag",
                        measure.vars = c("acf_before", "acf_after"),
                        variable.name = "dataset", value.name = "acf")

    results_long[, dataset := ifelse(dataset == "acf_before",
                                     "Before Filtering", "After Filtering")]

    # Significance bounds (95% confidence)
    n_before <- length(labels_before)
    n_after <- length(labels_after)
    ci_before <- qnorm(0.975) / sqrt(n_before)
    ci_after <- qnorm(0.975) / sqrt(n_after)

    # Create plot
    p <- ggplot(results_long, aes(x = lag, y = acf, color = dataset, group = dataset)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_hline(yintercept = c(ci_before, -ci_before),
                linetype = "dashed", color = "#E74C3C", alpha = 0.5) +
      geom_hline(yintercept = c(ci_after, -ci_after),
                linetype = "dashed", color = "#27AE60", alpha = 0.5) +
      scale_color_manual(values = c("Before Filtering" = "#E74C3C",
                                    "After Filtering" = "#27AE60")) +
      labs(title = "Label Autocorrelation: Before vs After Exit-Based Filtering",
           subtitle = sprintf("Dashed lines = 95%% confidence bounds (n_before=%s, n_after=%s)",
                             format(n_before, big.mark = ","),
                             format(n_after, big.mark = ",")),
           x = "Lag",
           y = "Autocorrelation",
           color = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10),
            legend.position = "bottom")

    # Save plot
    output_file <- file.path(output_path, "acf_comparison.png")
    ggsave(output_file, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("\nPlot saved: %s\n", output_file))

    
  }

  return(results)
}


cat("\n=== TRIPLE BARRIER LABELING SCRIPT GELADEN ===\n")
cat("Verfügbare Funktionen:\n")
cat("  - create_triple_barrier_labels(prices, ...)\n")
cat("  - analyze_label_quality(labeled_data)\n")
cat("  - test_neutral_thresholds(labeled_data, thresholds)\n")
cat("  - plot_label_distribution(labeled_data)\n")
cat("  - calculate_sample_weights(labeled_data)\n")
cat("  - analyze_sample_weights(labeled_data)\n")
cat("  - plot_sample_weights(labeled_data)\n")
cat("  - test_horizon_impact(prices, horizons, ...)\n")
cat("  - plot_horizon_impact(horizon_results)\n")
cat("  - analyze_holding_period(labeled_data, max_horizon)\n")
cat("  - plot_holding_period_analysis(labeled_data, max_horizon)\n")
cat("  - optimize_labeling_parameters(prices, ...)\n")
cat("  - filter_labels_exit_based(labeled_data) [NEW]\n")
cat("  - analyze_label_autocorrelation(labeled_before, labeled_after) [NEW]\n")
cat("\nBeispiel:\n")
cat("  labeled <- create_triple_barrier_labels(gold_15min)\n")
cat("  threshold_test <- test_neutral_thresholds(labeled, thresholds = c(0.05, 0.1, 0.15))\n")
cat("  labeled_weighted <- calculate_sample_weights(labeled)\n")
cat("  analyze_sample_weights(labeled_weighted)\n")
cat("  horizon_test <- test_horizon_impact(gold_15min, horizons = c(8, 12, 16, 20, 24))\n")
cat("  plot_horizon_impact(horizon_test)\n")
cat("  labeled_clean <- filter_labels_exit_based(labeled_weighted)\n")
cat("  acf_analysis <- analyze_label_autocorrelation(labeled_weighted, labeled_clean)\n")