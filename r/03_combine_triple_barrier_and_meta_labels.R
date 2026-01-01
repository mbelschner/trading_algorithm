# =============================================================================
# KOMBINIERTE LABELS: TRIPLE BARRIER + META-LABELING
# =============================================================================
#
# Dieses Skript kombiniert beide Labeling-Ansätze in einer einzigen Datei:
#
# 1. TRIPLE BARRIER LABELS (Primary Labels)
#    - Liefert Labels für ALLE Bars (Long/Short/Neutral)
#    - Basiert auf symmetrischen Profit/Loss Barriers
#    - Sample Weights für überlappende Labels
#
# 2. META-LABELS (Secondary Labels)
#    - Liefert Labels nur für EXTREMA-SIGNALE (0/1 - Trade oder Skip)
#    - Basiert auf lokalen Minima/Maxima
#    - Filtert schlechte Trades aus
#
# 3. KOMBINIERTE LABELS
#    - Merged beide Datensätze
#    - Erlaubt Vergleich und Kombination beider Ansätze
#    - Nützlich für Ensemble-Modelle
#
# =============================================================================

library(data.table)

#' Kombiniere Triple Barrier Labels und Meta-Labels
#'
#' @param triple_barrier_labeled data.table mit Triple Barrier Labels
#' @param meta_labeled data.table mit Meta-Labels (nur Signal-Bars)
#' @param prices Original Price Data (optional, für zusätzliche Features)
#'
#' @return data.table mit kombinierten Labels
#'
combine_labels <- function(
    triple_barrier_labeled,
    meta_labeled,
    prices = NULL
) {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║         KOMBINIERE TRIPLE BARRIER + META-LABELS               ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  # Erstelle Kopien
  tb <- copy(triple_barrier_labeled)
  ml <- copy(meta_labeled)

  setDT(tb)
  setDT(ml)

  cat("Input Datasets:\n")
  cat(sprintf("  Triple Barrier Labels: %s Zeilen\n", format(nrow(tb), big.mark = ",")))
  cat(sprintf("  Meta-Labels:           %s Zeilen (nur Signale)\n", format(nrow(ml), big.mark = ",")))

  # -------------------------------------------------------------------------
  # Stelle sicher, dass beide Datasets datetime haben
  # -------------------------------------------------------------------------

  if(!"datetime" %in% names(tb)) {
    if("time" %in% names(tb)) {
      setnames(tb, "time", "datetime")
    } else {
      stop("Triple Barrier Dataset benötigt 'datetime' oder 'time' Spalte")
    }
  }

  if(!"datetime" %in% names(ml)) {
    if("time" %in% names(ml)) {
      setnames(ml, "time", "datetime")
    } else {
      stop("Meta-Label Dataset benötigt 'datetime' oder 'time' Spalte")
    }
  }

  # -------------------------------------------------------------------------
  # Prefix für Meta-Label Spalten (um Konflikte zu vermeiden)
  # -------------------------------------------------------------------------

  ml_cols <- setdiff(names(ml), c("datetime", "open", "high", "low", "close"))

  # Rename Meta-Label Spalten mit "ml_" Prefix
  setnames(ml, ml_cols, paste0("ml_", ml_cols))

  cat("\nMeta-Label Spalten (mit 'ml_' Prefix):\n")
  cat(paste("  -", paste0("ml_", ml_cols), collapse = "\n"), "\n")

  # -------------------------------------------------------------------------
  # Merge: Left Join auf Triple Barrier (alle Bars behalten)
  # -------------------------------------------------------------------------

  cat("\nMerge Strategie: LEFT JOIN (Triple Barrier behält alle Bars)\n")

  combined <- merge(
    tb,
    ml,
    by = "datetime",
    all.x = TRUE,
    suffixes = c("_tb", "_ml")
  )

  cat(sprintf("Kombinierte Daten: %s Zeilen\n", format(nrow(combined), big.mark = ",")))

  # -------------------------------------------------------------------------
  # Erstelle Indikator-Spalte: Hat diese Bar ein Meta-Signal?
  # -------------------------------------------------------------------------

  combined[, has_meta_signal := !is.na(ml_primary_signal)]

  cat(sprintf("\nBars mit Meta-Signal: %s (%.2f%%)\n",
              sum(combined$has_meta_signal),
              sum(combined$has_meta_signal) / nrow(combined) * 100))

  # -------------------------------------------------------------------------
  # Analyse: Übereinstimmung zwischen Triple Barrier und Meta-Labels
  # -------------------------------------------------------------------------

  cat("\n=== LABEL ÜBEREINSTIMMUNG ===\n\n")

  # Nur Bars mit Meta-Signal analysieren
  signal_bars <- combined[has_meta_signal == TRUE]

  if(nrow(signal_bars) > 0) {

    cat("Triple Barrier Label Verteilung (nur Meta-Signal Bars):\n")
    print(table(signal_bars$label, useNA = "ifany"))

    cat("\nMeta-Label Verteilung:\n")
    print(table(signal_bars$ml_meta_label, useNA = "ifany"))

    # Side Agreement: Stimmen Triple Barrier und Meta Primary Signal überein?
    cat("\n--- Side Agreement ---\n")

    signal_bars[, side_agreement := case_when(
      ml_primary_signal == 1 & label == 1 ~ "both_long",
      ml_primary_signal == -1 & label == -1 ~ "both_short",
      ml_primary_signal == 1 & label == -1 ~ "conflict_long_vs_short",
      ml_primary_signal == -1 & label == 1 ~ "conflict_short_vs_long",
      ml_primary_signal != 0 & label == 0 ~ "meta_signal_tb_neutral",
      TRUE ~ "other"
    )]

    cat("\nSide Agreement:\n")
    print(table(signal_bars$side_agreement))

    # Agreement Rate
    agreement_rate <- sum(signal_bars$side_agreement %in% c("both_long", "both_short")) / nrow(signal_bars) * 100
    cat(sprintf("\nSide Agreement Rate: %.1f%%\n", agreement_rate))

    # Quality Agreement: Stimmen die Profitability-Labels überein?
    cat("\n--- Quality Agreement ---\n")

    # Triple Barrier: 1 = Long/Short (profitabel), 0 = Neutral (nicht profitabel)
    # Meta-Label: 1 = Trade (profitabel), 0 = Skip (nicht profitabel)

    signal_bars[, quality_agreement := case_when(
      (label != 0) & ml_meta_label == 1 ~ "both_profitable",
      (label == 0) & ml_meta_label == 0 ~ "both_unprofitable",
      (label != 0) & ml_meta_label == 0 ~ "tb_profitable_ml_skip",
      (label == 0) & ml_meta_label == 1 ~ "tb_neutral_ml_trade",
      TRUE ~ "unknown"
    )]

    cat("\nQuality Agreement:\n")
    print(table(signal_bars$quality_agreement))

    quality_agreement_rate <- sum(signal_bars$quality_agreement %in%
                                   c("both_profitable", "both_unprofitable")) / nrow(signal_bars) * 100
    cat(sprintf("\nQuality Agreement Rate: %.1f%%\n", quality_agreement_rate))

    # Speichere Agreement Columns im combined Dataset
    combined[signal_bars, side_agreement := i.side_agreement, on = "datetime"]
    combined[signal_bars, quality_agreement := i.quality_agreement, on = "datetime"]
  }

  # -------------------------------------------------------------------------
  # Erstelle kombinierte Features für ML
  # -------------------------------------------------------------------------

  cat("\n=== ERSTELLE KOMBINIERTE FEATURES ===\n")

  # Feature 1: Ensemble Label (beide müssen zustimmen)
  combined[, ensemble_label := case_when(
    has_meta_signal & (label == 1) & (ml_meta_label == 1) & (ml_primary_signal == 1) ~ 1L,    # Beide sagen Long
    has_meta_signal & (label == -1) & (ml_meta_label == 1) & (ml_primary_signal == -1) ~ -1L, # Beide sagen Short
    TRUE ~ 0L  # Sonst Neutral
  )]

  cat("Ensemble Label Verteilung:\n")
  print(table(combined$ensemble_label))
  cat("\n")

  # Feature 2: Signal Strength (wie viele Modelle stimmen überein?)
  combined[, signal_strength := 0]
  combined[label != 0, signal_strength := signal_strength + 1]  # Triple Barrier sagt Trade
  combined[has_meta_signal & ml_meta_label == 1, signal_strength := signal_strength + 1]  # Meta sagt Trade

  cat("Signal Strength Verteilung:\n")
  print(table(combined$signal_strength))
  cat("  0 = Beide neutral\n")
  cat("  1 = Nur ein Modell signalisiert\n")
  cat("  2 = Beide Modelle signalisieren Trade\n\n")

  # Feature 3: Confidence Score (gewichteter Score basierend auf beiden Modellen)
  combined[, confidence_score := 0.0]

  # Triple Barrier Confidence (basierend auf Sample Weight)
  if("uniqueness" %in% names(combined)) {
    combined[label != 0, confidence_score := confidence_score + uniqueness]
  } else {
    combined[label != 0, confidence_score := confidence_score + 0.5]
  }

  # Meta-Label Confidence (binär: 0.5 wenn signalisiert)
  combined[has_meta_signal & ml_meta_label == 1, confidence_score := confidence_score + 0.5]

  cat(sprintf("Confidence Score Range: %.3f - %.3f\n",
              min(combined$confidence_score, na.rm = TRUE),
              max(combined$confidence_score, na.rm = TRUE)))

  # -------------------------------------------------------------------------
  # Statistiken
  # -------------------------------------------------------------------------

  cat("\n=== FINALE STATISTIKEN ===\n\n")

  cat("Kombinierte Dataset Struktur:\n")
  cat(sprintf("  Total Bars:              %s\n", format(nrow(combined), big.mark = ",")))
  cat(sprintf("  Bars mit Meta-Signal:    %s (%.2f%%)\n",
              format(sum(combined$has_meta_signal), big.mark = ","),
              sum(combined$has_meta_signal) / nrow(combined) * 100))
  cat(sprintf("  Ensemble Long Signals:   %s\n", sum(combined$ensemble_label == 1)))
  cat(sprintf("  Ensemble Short Signals:  %s\n", sum(combined$ensemble_label == -1)))
  cat(sprintf("  High Confidence Signals: %s (strength = 2)\n", sum(combined$signal_strength == 2)))

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                    KOMBINATION ABGESCHLOSSEN                   ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  return(combined)
}


#' Analysiere kombinierte Labels
#'
analyze_combined_labels <- function(combined_data) {

  cat("\n=== DETAILLIERTE ANALYSE KOMBINIERTER LABELS ===\n\n")

  dt <- copy(combined_data)
  setDT(dt)

  # -------------------------------------------------------------------------
  # 1. Performance nach Signal Strength
  # -------------------------------------------------------------------------

  cat("--- Performance nach Signal Strength ---\n\n")

  if("ret" %in% names(dt)) {
    strength_stats <- dt[, .(
      count = .N,
      mean_ret = mean(ret, na.rm = TRUE),
      median_ret = median(ret, na.rm = TRUE),
      win_rate = sum(ret > 0, na.rm = TRUE) / sum(!is.na(ret)) * 100,
      sharpe = ifelse(sd(ret, na.rm = TRUE) > 0,
                     mean(ret, na.rm = TRUE) / sd(ret, na.rm = TRUE),
                     NA)
    ), by = signal_strength]

    print(strength_stats)
  }

  # -------------------------------------------------------------------------
  # 2. Ensemble vs. Individual Performance
  # -------------------------------------------------------------------------

  cat("\n--- Ensemble vs. Individual Labels ---\n\n")

  if("ret" %in% names(dt)) {

    performance_comparison <- rbind(
      dt[label != 0, .(
        strategy = "Triple Barrier Only",
        n_signals = .N,
        mean_ret = mean(ret, na.rm = TRUE),
        win_rate = sum(ret > 0, na.rm = TRUE) / .N * 100
      )],
      dt[has_meta_signal & ml_meta_label == 1, .(
        strategy = "Meta-Label Only",
        n_signals = .N,
        mean_ret = mean(ret, na.rm = TRUE),
        win_rate = sum(ret > 0, na.rm = TRUE) / .N * 100
      )],
      dt[ensemble_label != 0, .(
        strategy = "Ensemble (Both Agree)",
        n_signals = .N,
        mean_ret = mean(ret, na.rm = TRUE),
        win_rate = sum(ret > 0, na.rm = TRUE) / .N * 100
      )]
    )

    print(performance_comparison)
  }

  # -------------------------------------------------------------------------
  # 3. Confusion Matrix: Triple Barrier vs. Meta-Label
  # -------------------------------------------------------------------------

  cat("\n--- Confusion Matrix ---\n\n")

  signal_bars <- dt[has_meta_signal == TRUE]

  if(nrow(signal_bars) > 0) {

    # Profitable = 1, Unprofitable = 0
    signal_bars[, tb_profitable := as.integer(label != 0)]
    signal_bars[, ml_profitable := ml_meta_label]

    confusion <- table(
      TB = signal_bars$tb_profitable,
      ML = signal_bars$ml_profitable
    )

    cat("Triple Barrier (TB) vs. Meta-Label (ML):\n")
    cat("(1 = Profitable, 0 = Unprofitable)\n\n")
    print(confusion)

    # Cohen's Kappa
    if(nrow(signal_bars) > 0) {
      obs_agreement <- sum(diag(confusion)) / sum(confusion)
      expected_agreement <- sum(rowSums(confusion) * colSums(confusion)) / sum(confusion)^2
      kappa <- (obs_agreement - expected_agreement) / (1 - expected_agreement)

      cat(sprintf("\nCohen's Kappa: %.3f\n", kappa))
      cat("Interpretation:\n")
      cat("  < 0.00: No agreement\n")
      cat("  0.00-0.20: Slight agreement\n")
      cat("  0.21-0.40: Fair agreement\n")
      cat("  0.41-0.60: Moderate agreement\n")
      cat("  0.61-0.80: Substantial agreement\n")
      cat("  0.81-1.00: Almost perfect agreement\n")
    }
  }

  # -------------------------------------------------------------------------
  # 4. Feature Importance: Welche Features sind am wichtigsten?
  # -------------------------------------------------------------------------

  cat("\n--- Feature Correlation mit Returns ---\n\n")

  if("ret" %in% names(dt)) {

    features <- c("signal_strength", "confidence_score")

    if("uniqueness" %in% names(dt)) {
      features <- c(features, "uniqueness")
    }

    if("ml_realized_pnl" %in% names(dt)) {
      features <- c(features, "ml_realized_pnl")
    }

    correlations <- sapply(features, function(feat) {
      if(feat %in% names(dt)) {
        cor(dt[[feat]], dt$ret, use = "complete.obs")
      } else {
        NA
      }
    })

    correlation_df <- data.table(
      Feature = features,
      Correlation = correlations
    )
    correlation_df <- correlation_df[!is.na(Correlation)]
    correlation_df <- correlation_df[order(-abs(Correlation))]

    print(correlation_df)
  }

  cat("\n")
}


#' Speichere kombinierte Labels
#'
save_combined_labels <- function(
    combined_data,
    output_path,
    epic,
    interval
) {

  output_filename <- file.path(
    output_path,
    paste0(epic, "_", interval, "_combined_labels.csv")
  )

  fwrite(combined_data, output_filename)

  cat(sprintf("\nKombinierte Labels gespeichert in:\n  %s\n", output_filename))
  cat(sprintf("Größe: %.2f MB\n", file.size(output_filename) / 1024^2))

  return(output_filename)
}


#' Erstelle Visualisierung der kombinierten Labels
#'
plot_combined_labels <- function(
    combined_data,
    output_path,
    epic,
    interval
) {

  library(ggplot2)

  dt <- copy(combined_data)
  setDT(dt)

  # -------------------------------------------------------------------------
  # Plot 1: Signal Overlap Venn Diagram (als Bar Chart)
  # -------------------------------------------------------------------------

  signal_types <- dt[, .(count = .N), by = .(
    has_triple_barrier = label != 0,
    has_meta_signal = has_meta_signal & ml_meta_label == 1
  )]

  signal_types[, category := case_when(
    has_triple_barrier & has_meta_signal ~ "Both",
    has_triple_barrier & !has_meta_signal ~ "Triple Barrier Only",
    !has_triple_barrier & has_meta_signal ~ "Meta-Label Only",
    TRUE ~ "None"
  )]

  p1 <- ggplot(signal_types[category != "None"], aes(x = category, y = count, fill = category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::comma(count)), vjust = -0.5) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Signal Overlap: Triple Barrier vs. Meta-Label",
      subtitle = paste(epic, interval),
      x = NULL,
      y = "Anzahl Signale"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none"
    )

  # -------------------------------------------------------------------------
  # Plot 2: Performance nach Signal Strength
  # -------------------------------------------------------------------------

  if("ret" %in% names(dt)) {
    performance_by_strength <- dt[, .(
      mean_ret = mean(ret, na.rm = TRUE) * 100,
      se = sd(ret, na.rm = TRUE) / sqrt(.N) * 100,
      count = .N
    ), by = signal_strength]

    p2 <- ggplot(performance_by_strength, aes(x = factor(signal_strength), y = mean_ret)) +
      geom_col(aes(fill = factor(signal_strength))) +
      geom_errorbar(aes(ymin = mean_ret - se, ymax = mean_ret + se), width = 0.2) +
      geom_text(aes(label = sprintf("n=%s", scales::comma(count))),
                vjust = ifelse(performance_by_strength$mean_ret > 0, -1.5, 1.5)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Performance nach Signal Strength",
        subtitle = "Mean Return mit Standard Error",
        x = "Signal Strength (Anzahl zustimmender Modelle)",
        y = "Mean Return (%)",
        fill = "Strength"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "none"
      )
  } else {
    p2 <- NULL
  }

  # -------------------------------------------------------------------------
  # Plot 3: Ensemble Label Distribution Over Time
  # -------------------------------------------------------------------------

  if("datetime" %in% names(dt)) {
    # Sample für Performance (nur jeden 10. Datenpunkt bei großen Datasets)
    plot_sample <- if(nrow(dt) > 10000) {
      dt[seq(1, nrow(dt), by = 10)]
    } else {
      dt
    }

    plot_sample[, label_text := case_when(
      ensemble_label == 1 ~ "Long",
      ensemble_label == -1 ~ "Short",
      TRUE ~ "Neutral"
    )]

    p3 <- ggplot(plot_sample, aes(x = datetime, y = close)) +
      geom_line(color = "gray50", alpha = 0.6) +
      geom_point(data = plot_sample[ensemble_label != 0],
                aes(color = label_text), size = 2, alpha = 0.7) +
      scale_color_manual(values = c("Long" = "green", "Short" = "red")) +
      labs(
        title = "Ensemble Signals Over Time",
        subtitle = paste(epic, interval, "- Nur Bars wo beide Modelle zustimmen"),
        x = "Datum",
        y = "Preis",
        color = "Signal"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
      )
  } else {
    p3 <- NULL
  }

  # -------------------------------------------------------------------------
  # Speichere Plots
  # -------------------------------------------------------------------------

  ggsave(
    file.path(output_path, paste0(epic, "_", interval, "_combined_signal_overlap.png")),
    p1, width = 10, height = 6, dpi = 300
  )

  if(!is.null(p2)) {
    ggsave(
      file.path(output_path, paste0(epic, "_", interval, "_combined_performance_by_strength.png")),
      p2, width = 10, height = 6, dpi = 300
    )
  }

  if(!is.null(p3)) {
    ggsave(
      file.path(output_path, paste0(epic, "_", interval, "_combined_signals_timeline.png")),
      p3, width = 12, height = 6, dpi = 300
    )
  }

  cat("\nPlots gespeichert in:\n")
  cat(sprintf("  - %s\n", file.path(output_path, paste0(epic, "_", interval, "_combined_signal_overlap.png"))))
  if(!is.null(p2)) cat(sprintf("  - %s\n", file.path(output_path, paste0(epic, "_", interval, "_combined_performance_by_strength.png"))))
  if(!is.null(p3)) cat(sprintf("  - %s\n", file.path(output_path, paste0(epic, "_", interval, "_combined_signals_timeline.png"))))
}


cat("\n=== KOMBINIERTE LABEL SCRIPT GELADEN ===\n")
cat("Hauptfunktionen:\n")
cat("  - combine_labels()               # Kombiniere beide Labeling-Ansätze\n")
cat("  - analyze_combined_labels()      # Detaillierte Analyse\n")
cat("  - save_combined_labels()         # Speichere kombinierte Labels\n")
cat("  - plot_combined_labels()         # Visualisierungen\n\n")
