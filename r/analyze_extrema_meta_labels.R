# =============================================================================
# ANALYSE: EXTREMA META-LABELS
# =============================================================================
#
# Vergleicht Extrema-basierte Meta-Labels mit Triple Barrier Labels
# und liefert detaillierte Performance-Metriken
#
# =============================================================================

library(data.table)
library(ggplot2)

# =============================================================================
# HAUPTANALYSE-FUNKTION
# =============================================================================

#' Analysiere Extrema Meta-Labels
#'
#' @param meta_labeled Meta-labeled Datensatz (nur Signale)
#' @param triple_barrier Triple Barrier Labels (alle Bars)
#'
analyze_extrema_performance <- function(
    meta_labeled,
    triple_barrier = NULL
) {

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║              EXTREMA META-LABEL ANALYSE                        ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  # -------------------------------------------------------------------------
  # 1. BASIC STATISTICS
  # -------------------------------------------------------------------------

  cat("1. SIGNAL OVERVIEW\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  total_signals <- nrow(meta_labeled)
  long_signals <- sum(meta_labeled$primary_signal == 1)
  short_signals <- sum(meta_labeled$primary_signal == -1)

  cat(sprintf("Total Signals:         %6d\n", total_signals))
  cat(sprintf("  Long Signals (1):    %6d (%.1f%%)\n",
              long_signals, long_signals / total_signals * 100))
  cat(sprintf("  Short Signals (-1):  %6d (%.1f%%)\n\n",
              short_signals, short_signals / total_signals * 100))

  # -------------------------------------------------------------------------
  # 2. META-LABEL DISTRIBUTION
  # -------------------------------------------------------------------------

  cat("2. META-LABEL DISTRIBUTION\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  meta_table <- table(meta_labeled$meta_label)
  cat("Meta-Label Counts:\n")
  print(meta_table)
  cat("\nProportions:\n")
  print(round(prop.table(meta_table) * 100, 1))

  success_rate <- mean(meta_labeled$meta_label, na.rm = TRUE) * 100
  cat(sprintf("\nOverall Success Rate: %.1f%%\n\n", success_rate))

  # -------------------------------------------------------------------------
  # 3. PERFORMANCE BY SIDE
  # -------------------------------------------------------------------------

  cat("3. PERFORMANCE BY SIDE\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  perf_by_side <- meta_labeled[, .(
    Signals = .N,
    Success_Rate = round(mean(meta_label, na.rm = TRUE) * 100, 1),
    Avg_PnL = round(mean(realized_pnl, na.rm = TRUE) * 100, 2),
    Median_PnL = round(median(realized_pnl, na.rm = TRUE) * 100, 2),
    Avg_Bars_Held = round(mean(bars_held, na.rm = TRUE), 1),
    Max_PnL = round(max(realized_pnl, na.rm = TRUE) * 100, 2),
    Min_PnL = round(min(realized_pnl, na.rm = TRUE) * 100, 2)
  ), by = .(Side = primary_signal)]

  print(perf_by_side)
  cat("\n")

  # -------------------------------------------------------------------------
  # 4. EXIT REASON ANALYSIS
  # -------------------------------------------------------------------------

  cat("4. EXIT REASON DISTRIBUTION\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  exit_stats <- meta_labeled[, .(
    Count = .N,
    Pct = round(.N / nrow(meta_labeled) * 100, 1),
    Avg_Bars = round(mean(bars_held, na.rm = TRUE), 1),
    Avg_PnL = round(mean(realized_pnl, na.rm = TRUE) * 100, 2)
  ), by = exit_reason]

  exit_stats <- exit_stats[order(-Count)]
  print(exit_stats)
  cat("\n")

  # -------------------------------------------------------------------------
  # 5. EXTREMA TYPE ANALYSIS
  # -------------------------------------------------------------------------

  cat("5. EXTREMA TYPE PERFORMANCE\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  # Join mit full_data um extrema_type zu bekommen
  if("extrema_type" %in% names(meta_labeled)) {

    extrema_perf <- meta_labeled[!is.na(extrema_type), .(
      Signals = .N,
      Success_Rate = round(mean(meta_label, na.rm = TRUE) * 100, 1),
      Avg_PnL = round(mean(realized_pnl, na.rm = TRUE) * 100, 2)
    ), by = extrema_type]

    print(extrema_perf)
  } else {
    cat("(Extrema Type nicht verfügbar im Dataset)\n")
  }
  cat("\n")

  # -------------------------------------------------------------------------
  # 6. TEMPORAL ANALYSIS
  # -------------------------------------------------------------------------

  cat("6. PERFORMANCE BY TIME OF DAY\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  meta_labeled[, hour := hour(datetime)]

  hour_perf <- meta_labeled[, .(
    Signals = .N,
    Success_Rate = round(mean(meta_label, na.rm = TRUE) * 100, 1),
    Avg_PnL = round(mean(realized_pnl, na.rm = TRUE) * 100, 2)
  ), by = hour][order(hour)]

  # Zeige nur Stunden mit mind. 5 Signalen
  hour_perf_filtered <- hour_perf[Signals >= 5]
  print(hour_perf_filtered)
  cat("\n")

  # -------------------------------------------------------------------------
  # 7. HOLDING PERIOD ANALYSIS
  # -------------------------------------------------------------------------

  cat("7. HOLDING PERIOD ANALYSIS\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  cat(sprintf("Mean Holding Period:   %.1f bars\n",
              mean(meta_labeled$bars_held, na.rm = TRUE)))
  cat(sprintf("Median Holding Period: %d bars\n",
              median(meta_labeled$bars_held, na.rm = TRUE)))
  cat(sprintf("Min Holding Period:    %d bars\n",
              min(meta_labeled$bars_held, na.rm = TRUE)))
  cat(sprintf("Max Holding Period:    %d bars\n\n",
              max(meta_labeled$bars_held, na.rm = TRUE)))

  # Holding Period Distribution
  holding_dist <- meta_labeled[, .(
    Count = .N,
    Success_Rate = round(mean(meta_label, na.rm = TRUE) * 100, 1)
  ), by = bars_held][order(bars_held)]

  cat("Top 5 Most Common Holding Periods:\n")
  print(head(holding_dist[order(-Count)], 5))
  cat("\n")

  # -------------------------------------------------------------------------
  # 8. PNL DISTRIBUTION
  # -------------------------------------------------------------------------

  cat("8. PNL DISTRIBUTION\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  cat(sprintf("Mean PnL:    %6.2f%%\n",
              mean(meta_labeled$realized_pnl, na.rm = TRUE) * 100))
  cat(sprintf("Median PnL:  %6.2f%%\n",
              median(meta_labeled$realized_pnl, na.rm = TRUE) * 100))
  cat(sprintf("Std Dev:     %6.2f%%\n",
              sd(meta_labeled$realized_pnl, na.rm = TRUE) * 100))
  cat(sprintf("Best Trade:  %6.2f%%\n",
              max(meta_labeled$realized_pnl, na.rm = TRUE) * 100))
  cat(sprintf("Worst Trade: %6.2f%%\n\n",
              min(meta_labeled$realized_pnl, na.rm = TRUE) * 100))

  # Quantiles
  pnl_quantiles <- quantile(meta_labeled$realized_pnl, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  cat("PnL Quantiles:\n")
  print(round(pnl_quantiles * 100, 2))
  cat("\n")

  # -------------------------------------------------------------------------
  # 9. RISK METRICS
  # -------------------------------------------------------------------------

  cat("9. RISK METRICS\n")
  cat("─────────────────────────────────────────────────────────────────\n")

  win_rate <- mean(meta_labeled$meta_label, na.rm = TRUE)
  avg_win <- mean(meta_labeled[meta_label == 1, realized_pnl], na.rm = TRUE)
  avg_loss <- mean(meta_labeled[meta_label == 0, realized_pnl], na.rm = TRUE)

  expectancy <- (win_rate * avg_win) + ((1 - win_rate) * avg_loss)
  profit_factor <- ifelse(avg_loss != 0, abs(avg_win / avg_loss), NA)

  cat(sprintf("Win Rate:         %.1f%%\n", win_rate * 100))
  cat(sprintf("Average Win:      %.2f%%\n", avg_win * 100))
  cat(sprintf("Average Loss:     %.2f%%\n", avg_loss * 100))
  cat(sprintf("Expectancy:       %.2f%%\n", expectancy * 100))
  cat(sprintf("Profit Factor:    %.2f\n\n", profit_factor))

  # -------------------------------------------------------------------------
  # 10. COMPARISON WITH TRIPLE BARRIER (if provided)
  # -------------------------------------------------------------------------

  if(!is.null(triple_barrier)) {

    cat("10. COMPARISON: EXTREMA vs. TRIPLE BARRIER\n")
    cat("─────────────────────────────────────────────────────────────────\n")

    # Merge Meta-Labels mit Triple Barrier
    comparison_dt <- merge(
      meta_labeled[, .(datetime, primary_signal, meta_label,
                      realized_pnl_meta = realized_pnl)],
      triple_barrier[, .(datetime, label, realized_return)],
      by = "datetime",
      all.x = TRUE
    )

    # Agreement Analysis
    cat("\nSignal Direction Agreement:\n")

    comparison_dt[, direction_match := (
      (primary_signal == 1 & label == 1) |
      (primary_signal == -1 & label == -1)
    )]

    agreement_rate <- mean(comparison_dt$direction_match, na.rm = TRUE) * 100
    cat(sprintf("  Agreement Rate: %.1f%%\n", agreement_rate))

    # Performance when signals agree
    agree_perf <- comparison_dt[direction_match == TRUE, .(
      Count = .N,
      Meta_Success = round(mean(meta_label, na.rm = TRUE) * 100, 1),
      Avg_PnL_Meta = round(mean(realized_pnl_meta, na.rm = TRUE) * 100, 2),
      Avg_PnL_TB = round(mean(realized_return, na.rm = TRUE) * 100, 2)
    )]

    cat("\nWhen Signals Agree:\n")
    print(agree_perf)

    # Performance when signals disagree
    disagree_perf <- comparison_dt[direction_match == FALSE, .(
      Count = .N,
      Meta_Success = round(mean(meta_label, na.rm = TRUE) * 100, 1),
      Avg_PnL_Meta = round(mean(realized_pnl_meta, na.rm = TRUE) * 100, 2),
      Avg_PnL_TB = round(mean(realized_return, na.rm = TRUE) * 100, 2)
    )]

    cat("\nWhen Signals Disagree:\n")
    print(disagree_perf)
    cat("\n")
  }

  # -------------------------------------------------------------------------
  # RETURN SUMMARY
  # -------------------------------------------------------------------------

  summary_stats <- list(
    total_signals = total_signals,
    long_signals = long_signals,
    short_signals = short_signals,
    overall_success_rate = success_rate,
    mean_pnl = mean(meta_labeled$realized_pnl, na.rm = TRUE) * 100,
    median_pnl = median(meta_labeled$realized_pnl, na.rm = TRUE) * 100,
    mean_holding_bars = mean(meta_labeled$bars_held, na.rm = TRUE),
    win_rate = win_rate * 100,
    expectancy = expectancy * 100,
    profit_factor = profit_factor,
    performance_by_side = perf_by_side,
    exit_reason_stats = exit_stats
  )

  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                  ANALYSIS COMPLETED                            ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")

  return(invisible(summary_stats))
}


# =============================================================================
# VISUALISIERUNGEN
# =============================================================================

#' Erstelle Visualisierungen für Extrema Meta-Labels
#'
plot_extrema_analysis <- function(meta_labeled, output_path = "labelled_data") {

  cat("\n=== ERSTELLE VISUALISIERUNGEN ===\n")

  meta_labeled[, date := as.Date(datetime)]
  meta_labeled[, hour := hour(datetime)]

  # -------------------------------------------------------------------------
  # Plot 1: Success Rate Over Time
  # -------------------------------------------------------------------------

  daily_stats <- meta_labeled[, .(
    signals = .N,
    success_rate = mean(meta_label, na.rm = TRUE) * 100,
    avg_pnl = mean(realized_pnl, na.rm = TRUE) * 100
  ), by = date]

  p1 <- ggplot(daily_stats[signals >= 3], aes(x = date, y = success_rate)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(size = signals), color = "steelblue", alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "red",
                linetype = "dashed", alpha = 0.2) +
    geom_hline(yintercept = 50, linetype = "dotted", color = "gray50") +
    scale_size_continuous(range = c(1, 5)) +
    labs(
      title = "Extrema Signal Success Rate Over Time",
      subtitle = "Size = Number of Signals per Day (min. 3 signals shown)",
      x = "Date",
      y = "Success Rate (%)",
      size = "Daily Signals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  ggsave(
    filename = file.path(output_path, "extrema_success_rate_over_time.png"),
    plot = p1,
    width = 14,
    height = 7,
    dpi = 300
  )
  cat("✓ Plot gespeichert: extrema_success_rate_over_time.png\n")

  # -------------------------------------------------------------------------
  # Plot 2: PnL Distribution by Meta-Label
  # -------------------------------------------------------------------------

  p2 <- ggplot(meta_labeled, aes(x = realized_pnl * 100,
                                  fill = factor(meta_label))) +
    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(
      values = c("0" = "firebrick", "1" = "forestgreen"),
      labels = c("Failed (0)", "Success (1)")
    ) +
    facet_wrap(~factor(primary_signal,
                       labels = c("-1" = "Short Signals", "1" = "Long Signals")),
               ncol = 1) +
    labs(
      title = "PnL Distribution by Meta-Label and Signal Side",
      x = "Realized PnL (%)",
      y = "Count",
      fill = "Meta-Label"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  ggsave(
    filename = file.path(output_path, "extrema_pnl_distribution.png"),
    plot = p2,
    width = 12,
    height = 8,
    dpi = 300
  )
  cat("✓ Plot gespeichert: extrema_pnl_distribution.png\n")

  # -------------------------------------------------------------------------
  # Plot 3: Exit Reasons by Side
  # -------------------------------------------------------------------------

  p3 <- ggplot(meta_labeled, aes(x = exit_reason, fill = factor(meta_label))) +
    geom_bar(position = "stack") +
    facet_wrap(~factor(primary_signal,
                       labels = c("-1" = "Short", "1" = "Long"))) +
    scale_fill_manual(
      values = c("0" = "firebrick", "1" = "forestgreen"),
      labels = c("Failed", "Success")
    ) +
    labs(
      title = "Exit Reasons by Signal Side and Outcome",
      x = "Exit Reason",
      y = "Count",
      fill = "Meta-Label"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  ggsave(
    filename = file.path(output_path, "extrema_exit_reasons.png"),
    plot = p3,
    width = 12,
    height = 7,
    dpi = 300
  )
  cat("✓ Plot gespeichert: extrema_exit_reasons.png\n")

  # -------------------------------------------------------------------------
  # Plot 4: Performance by Hour of Day
  # -------------------------------------------------------------------------

  hourly_stats <- meta_labeled[, .(
    signals = .N,
    success_rate = mean(meta_label, na.rm = TRUE) * 100,
    avg_pnl = mean(realized_pnl, na.rm = TRUE) * 100
  ), by = hour][signals >= 5]  # Min 5 signals per hour

  p4 <- ggplot(hourly_stats, aes(x = factor(hour))) +
    geom_col(aes(y = signals), fill = "lightblue", alpha = 0.6) +
    geom_line(aes(y = success_rate * max(signals) / 100, group = 1),
              color = "red", linewidth = 1.2) +
    geom_point(aes(y = success_rate * max(signals) / 100),
               color = "red", size = 3) +
    scale_y_continuous(
      name = "Number of Signals",
      sec.axis = sec_axis(~. * 100 / max(hourly_stats$signals),
                         name = "Success Rate (%)")
    ) +
    labs(
      title = "Signal Performance by Hour of Day",
      subtitle = "Bars = Signal Count, Line = Success Rate",
      x = "Hour of Day"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title.y.right = element_text(color = "red"),
      axis.text.y.right = element_text(color = "red")
    )

  ggsave(
    filename = file.path(output_path, "extrema_hourly_performance.png"),
    plot = p4,
    width = 12,
    height = 6,
    dpi = 300
  )
  cat("✓ Plot gespeichert: extrema_hourly_performance.png\n")

  cat("\n✓ Alle Visualisierungen erstellt in:", output_path, "\n\n")
}


cat("\n=== EXTREMA ANALYSE SCRIPT GELADEN ===\n")
cat("Funktionen:\n")
cat("  - analyze_extrema_performance()  # Detaillierte Analyse\n")
cat("  - plot_extrema_analysis()        # Visualisierungen\n\n")
