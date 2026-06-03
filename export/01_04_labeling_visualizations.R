# =============================================================================
# LABELING VISUALIZATIONS
# =============================================================================
#
# Comprehensive visualization suite for meta-labeling analysis:
# - Label Density by Hour
# - Cumulative Edge Plot
# - Barrier Width vs Profitability Heatmap
# - Sample Weight Distribution
# - Meta-Label Performance
#
# =============================================================================

library(data.table)
library(ggplot2)
library(gridExtra)
library(scales)
library(viridis)

# =============================================================================
# 1. LABEL DENSITY BY HOUR
# =============================================================================

#' Plot label density distribution by hour of day
#'
#' Shows when trading signals occur and their distribution throughout the day.
#'
#' @param dt data.table with labels
#' @param output_path Directory to save plot (NULL = don't save)
#' @param show_plot Display plot interactively?
#' @return ggplot object
plot_label_density_by_hour <- function(dt, output_path = NULL, show_plot = TRUE) {

  dt <- copy(dt)
  dt[, hour := hour(datetime)]

  # Session labels
  dt[, session := fcase(
    hour >= 1 & hour < 8, "Asia",
    hour >= 8 & hour < 13, "London",
    hour >= 13 & hour < 17, "Overlap",
    hour >= 17 & hour < 22, "NY",
    default = "Closed"
  )]

  # Prepare data for plotting
  if ("meta_label" %in% names(dt)) {
    dt[, label_type := fifelse(meta_label == 1, "TP (Success)", "SL/TO (Failure)")]
    hourly_counts <- dt[, .N, by = .(hour, label_type)]
  } else if ("primary_signal" %in% names(dt)) {
    dt[, label_type := fifelse(primary_signal == 1, "Long", "Short")]
    hourly_counts <- dt[, .N, by = .(hour, label_type)]
  } else {
    dt[, label_type := as.character(label)]
    hourly_counts <- dt[, .N, by = .(hour, label_type)]
  }

  # Session background data
  sessions <- data.table(
    session = c("Asia", "London", "Overlap", "NY"),
    xmin = c(1, 8, 13, 17),
    xmax = c(8, 13, 17, 22),
    color = c("#E8F5E9", "#E3F2FD", "#FFF3E0", "#FCE4EC")
  )

  # Create plot
  p <- ggplot(hourly_counts, aes(x = hour, y = N, fill = label_type)) +
    # Session backgrounds
    geom_rect(data = sessions,
              aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf, fill = NULL),
              fill = sessions$color, alpha = 0.5, inherit.aes = FALSE) +
    # Bars
    geom_bar(stat = "identity", position = "dodge", width = 0.8, alpha = 0.9) +
    # Session labels
    geom_text(data = sessions,
              aes(x = (xmin + xmax) / 2, y = Inf, label = session),
              vjust = 1.5, size = 3, fontface = "bold", inherit.aes = FALSE) +
    scale_fill_manual(values = c("TP (Success)" = "#27AE60", "SL/TO (Failure)" = "#E74C3C",
                                 "Long" = "#27AE60", "Short" = "#E74C3C",
                                 "1" = "#27AE60", "-1" = "#E74C3C", "0" = "#95A5A6")) +
    scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
    labs(
      title = "Label Density by Hour of Day",
      subtitle = "Distribution of trading signals across trading sessions",
      x = "Hour (UTC)",
      y = "Number of Labels",
      fill = "Label Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  if (show_plot) print(p)

  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "label_density_by_hour.png"),
           p, width = 12, height = 6, dpi = 300)
    cat("Saved: label_density_by_hour.png\n")
  }

  return(invisible(p))
}


# =============================================================================
# 2. CUMULATIVE EDGE PLOT
# =============================================================================

#' Plot cumulative edge (returns) over time
#'
#' Shows the cumulative return of following the labels after costs.
#'
#' @param dt data.table with returns
#' @param spread Spread cost per trade
#' @param slippage_pips Slippage in pips
#' @param output_path Directory to save plot
#' @param show_plot Display plot?
#' @return ggplot object
plot_cumulative_edge <- function(
    dt,
    spread = 0.00013,
    slippage_pips = 1.0,
    output_path = NULL,
    show_plot = TRUE
) {

  dt <- copy(dt)
  setorder(dt, datetime)

  # Calculate costs
  slippage <- slippage_pips * 0.0001
  total_cost <- spread + slippage

  # Returns
  dt[, return_gross := realized_return]
  dt[, return_net := realized_return - total_cost]

  # Cumulative returns
  dt[, cum_return_gross := cumsum(return_gross)]
  dt[, cum_return_net := cumsum(return_net)]

  # Calculate weighted cumulative (if sample_weight exists)
  if ("sample_weight" %in% names(dt)) {
    dt[, weighted_return := return_net * sample_weight]
    dt[, cum_return_weighted := cumsum(weighted_return)]
  }

  # Reshape for plotting
  plot_data <- melt(
    dt[, .(datetime, Gross = cum_return_gross, Net = cum_return_net)],
    id.vars = "datetime",
    variable.name = "type",
    value.name = "cumulative_return"
  )

  # Create plot
  p <- ggplot(plot_data, aes(x = datetime, y = cumulative_return * 100, color = type)) +
    geom_line(size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("Gross" = "#3498DB", "Net" = "#E74C3C")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Cumulative Edge Plot",
      subtitle = sprintf("Returns after spread (%.2f pips) + slippage (%.1f pips)",
                         spread * 10000, slippage_pips),
      x = "Date",
      y = "Cumulative Return (%)",
      color = "Return Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )

  # Add annotations
  final_gross <- tail(dt$cum_return_gross, 1) * 100
  final_net <- tail(dt$cum_return_net, 1) * 100

  p <- p +
    annotate("text", x = max(dt$datetime), y = final_gross,
             label = sprintf("Gross: %.1f%%", final_gross),
             hjust = 1.1, vjust = -0.5, color = "#3498DB", size = 3.5) +
    annotate("text", x = max(dt$datetime), y = final_net,
             label = sprintf("Net: %.1f%%", final_net),
             hjust = 1.1, vjust = 1.5, color = "#E74C3C", size = 3.5)

  if (show_plot) print(p)

  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "cumulative_edge_plot.png"),
           p, width = 12, height = 6, dpi = 300)
    cat("Saved: cumulative_edge_plot.png\n")
  }

  return(invisible(p))
}


# =============================================================================
# 3. BARRIER WIDTH VS PROFITABILITY HEATMAP
# =============================================================================

#' Heatmap of barrier width vs profitability
#'
#' Shows how different TP/SL distances affect profitability.
#'
#' @param dt data.table with barrier distances and returns
#' @param output_path Directory to save plot
#' @param show_plot Display plot?
#' @return ggplot object
plot_barrier_profitability_heatmap <- function(dt, output_path = NULL, show_plot = TRUE) {

  dt <- copy(dt)

  # Check required columns
  if (!all(c("tp_distance", "sl_distance", "realized_return_adj") %in% names(dt))) {
    warning("Required columns not found for heatmap")
    return(NULL)
  }

  # Bin barrier distances
  dt[, tp_bin := cut(tp_distance * 10000, breaks = c(0, 20, 40, 60, 80, 100, Inf),
                     labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100+"))]
  dt[, sl_bin := cut(sl_distance * 10000, breaks = c(0, 20, 40, 60, 80, 100, Inf),
                     labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100+"))]

  # Aggregate profitability by bins
  heatmap_data <- dt[!is.na(tp_bin) & !is.na(sl_bin), .(
    mean_return = mean(realized_return_adj, na.rm = TRUE) * 100,
    win_rate = mean(realized_return_adj > 0, na.rm = TRUE) * 100,
    n = .N
  ), by = .(tp_bin, sl_bin)]

  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = tp_bin, y = sl_bin, fill = mean_return)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.2f%%\n(n=%d)", mean_return, n)),
              color = "white", size = 3) +
    scale_fill_gradient2(
      low = "#E74C3C",
      mid = "#F1C40F",
      high = "#27AE60",
      midpoint = 0,
      name = "Mean Return (%)"
    ) +
    labs(
      title = "Barrier Width vs Profitability",
      subtitle = "Mean adjusted return by TP and SL distance (in pips)",
      x = "Take Profit Distance (pips)",
      y = "Stop Loss Distance (pips)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  if (show_plot) print(p)

  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "barrier_profitability_heatmap.png"),
           p, width = 10, height = 8, dpi = 300)
    cat("Saved: barrier_profitability_heatmap.png\n")
  }

  return(invisible(p))
}


# =============================================================================
# 4. SAMPLE WEIGHT DISTRIBUTION
# =============================================================================

#' Plot sample weight distribution
#'
#' @param dt data.table with sample_weight column
#' @param output_path Directory to save plot
#' @param show_plot Display plot?
#' @return ggplot object
plot_sample_weight_distribution <- function(dt, output_path = NULL, show_plot = TRUE) {

  if (!"sample_weight" %in% names(dt)) {
    warning("sample_weight column not found")
    return(NULL)
  }

  # Create multiple plots
  p1 <- ggplot(dt, aes(x = sample_weight)) +
    geom_histogram(binwidth = 0.05, fill = "#3498DB", color = "white", alpha = 0.8) +
    geom_vline(xintercept = mean(dt$sample_weight), color = "#E74C3C",
               linetype = "dashed", size = 1) +
    annotate("text", x = mean(dt$sample_weight), y = Inf,
             label = sprintf("Mean: %.3f", mean(dt$sample_weight)),
             vjust = 2, hjust = -0.1, color = "#E74C3C") +
    labs(
      title = "Sample Weight Distribution",
      x = "Sample Weight (1 / n_concurrent)",
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Concurrent labels distribution
  p2 <- ggplot(dt, aes(x = n_concurrent)) +
    geom_histogram(binwidth = 1, fill = "#E67E22", color = "white", alpha = 0.8) +
    labs(
      title = "Concurrent Labels Distribution",
      x = "Number of Overlapping Labels",
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # By hour
  dt[, hour := hour(datetime)]
  hourly_weights <- dt[, .(
    mean_weight = mean(sample_weight),
    mean_concurrent = mean(n_concurrent)
  ), by = hour]

  p3 <- ggplot(hourly_weights, aes(x = hour)) +
    geom_line(aes(y = mean_concurrent), color = "#3498DB", size = 1) +
    geom_point(aes(y = mean_concurrent), color = "#3498DB", size = 2) +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    labs(
      title = "Mean Concurrent Labels by Hour",
      x = "Hour (UTC)",
      y = "Mean Concurrent Labels"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))

  # Effective sample size over time
  dt[, date := as.Date(datetime)]
  daily_ess <- dt[, .(
    n_samples = .N,
    effective_n = sum(sample_weight)
  ), by = date]

  p4 <- ggplot(daily_ess, aes(x = date)) +
    geom_line(aes(y = n_samples, color = "Original"), size = 0.8) +
    geom_line(aes(y = effective_n, color = "Effective"), size = 0.8) +
    scale_color_manual(values = c("Original" = "#95A5A6", "Effective" = "#27AE60")) +
    labs(
      title = "Daily Sample Size (Original vs Effective)",
      x = "Date",
      y = "Number of Samples",
      color = ""
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

  # Combine plots
  combined <- grid.arrange(p1, p2, p3, p4, ncol = 2)

  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "sample_weight_distribution.png"),
           combined, width = 12, height = 10, dpi = 300)
    cat("Saved: sample_weight_distribution.png\n")
  }

  return(invisible(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)))
}


# =============================================================================
# 5. META-LABEL PERFORMANCE
# =============================================================================

#' Plot meta-label performance analysis
#'
#' @param dt data.table with meta_label and returns
#' @param output_path Directory to save plot
#' @param show_plot Display plot?
#' @return ggplot object
plot_meta_label_performance <- function(dt, output_path = NULL, show_plot = TRUE) {

  if (!"meta_label" %in% names(dt)) {
    warning("meta_label column not found")
    return(NULL)
  }

  dt <- copy(dt)
  dt[, label_type := fifelse(meta_label == 1, "TP (Success)", "SL/Timeout (Failure)")]

  # Return distribution by label
  p1 <- ggplot(dt, aes(x = realized_return_adj * 100, fill = label_type)) +
    geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_fill_manual(values = c("TP (Success)" = "#27AE60", "SL/Timeout (Failure)" = "#E74C3C")) +
    labs(
      title = "Return Distribution by Meta-Label",
      x = "Adjusted Return (%)",
      y = "Count",
      fill = "Meta-Label"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

  # Holding period by label
  p2 <- ggplot(dt, aes(x = label_type, y = bars_to_exit, fill = label_type)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
    scale_fill_manual(values = c("TP (Success)" = "#27AE60", "SL/Timeout (Failure)" = "#E74C3C")) +
    labs(
      title = "Holding Period by Meta-Label",
      x = "",
      y = "Bars to Exit",
      fill = ""
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), legend.position = "none")

  # Win rate by barrier touched
  barrier_stats <- dt[, .(
    n = .N,
    win_rate = mean(realized_return_adj > 0) * 100,
    mean_return = mean(realized_return_adj) * 100
  ), by = barrier_touched]

  p3 <- ggplot(barrier_stats, aes(x = reorder(barrier_touched, -win_rate), y = win_rate, fill = win_rate)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", win_rate, n)), vjust = -0.3, size = 3) +
    scale_fill_gradient(low = "#E74C3C", high = "#27AE60") +
    labs(
      title = "Win Rate by Exit Type",
      x = "Barrier Touched",
      y = "Win Rate (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  # Cumulative returns by direction
  if ("primary_signal" %in% names(dt)) {
    setorder(dt, datetime)
    dt[primary_signal == 1, cum_return_long := cumsum(realized_return_adj)]
    dt[primary_signal == -1, cum_return_short := cumsum(realized_return_adj)]

    cum_data <- melt(
      dt[, .(datetime,
             Long = nafill(cum_return_long, type = "locf"),
             Short = nafill(cum_return_short, type = "locf"))],
      id.vars = "datetime",
      variable.name = "direction",
      value.name = "cumulative"
    )
    cum_data <- cum_data[!is.na(cumulative)]

    p4 <- ggplot(cum_data, aes(x = datetime, y = cumulative * 100, color = direction)) +
      geom_line(size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("Long" = "#27AE60", "Short" = "#E74C3C")) +
      labs(
        title = "Cumulative Returns by Direction",
        x = "Date",
        y = "Cumulative Return (%)",
        color = "Direction"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  } else {
    p4 <- ggplot() + theme_void()
  }

  # Combine plots
  combined <- grid.arrange(p1, p2, p3, p4, ncol = 2)

  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "meta_label_performance.png"),
           combined, width = 14, height = 12, dpi = 300)
    cat("Saved: meta_label_performance.png\n")
  }

  return(invisible(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)))
}


# =============================================================================
# 6. PRICE CHART WITH SIGNALS AND META-LABELS
# =============================================================================

#' Plot price chart with primary signals and meta-label outcomes
#'
#' Creates a detailed chart showing:
#' - Price line/candlestick
#' - Entry signals (arrows: up=Long, down=Short)
#' - Color-coded by outcome (Green=TP hit, Red=SL/Timeout)
#' - Optional: TP/SL barrier lines
#'
#' @param dt data.table with price data, primary_signal, meta_label
#' @param n_weeks Number of example weeks to plot (default 2)
#' @param output_path Directory to save plots
#' @param show_barriers Show TP/SL barrier lines?
#' @param show_plot Display plot?
#' @return List of ggplot objects
plot_price_with_signals <- function(
    dt,
    n_weeks = 2,
    output_path = NULL,
    show_barriers = TRUE,
    show_plot = TRUE
) {

  dt <- copy(dt)
  setorder(dt, datetime)

  # Ensure required columns exist
  required_cols <- c("datetime", "close", "primary_signal", "meta_label")
  if (!all(required_cols %in% names(dt))) {
    missing <- setdiff(required_cols, names(dt))
    warning(sprintf("Missing columns: %s", paste(missing, collapse = ", ")))
    return(NULL)
  }

  # Add week identifier
  dt[, week := floor(as.numeric(difftime(datetime, min(datetime), units = "weeks")))]
  dt[, week_start := min(datetime), by = week]

  # Find weeks with most signals
  signals_per_week <- dt[!is.na(primary_signal) & primary_signal != 0,
                         .(n_signals = .N,
                           n_tp = sum(meta_label == 1, na.rm = TRUE),
                           n_sl = sum(meta_label == 0, na.rm = TRUE)),
                         by = .(week, week_start)]
  setorder(signals_per_week, -n_signals)

  # Select top n weeks
  top_weeks <- head(signals_per_week, n_weeks)
  cat(sprintf("\nSelected %d weeks with most signals:\n", n_weeks))
  print(top_weeks[, .(week_start, n_signals, n_tp, n_sl)])

  plots <- list()

  for (i in 1:nrow(top_weeks)) {
    week_num <- top_weeks$week[i]
    week_data <- dt[week == week_num]

    # Get signals only
    signals <- week_data[!is.na(primary_signal) & primary_signal != 0]

    # Create outcome labels
    signals[, outcome := fcase(
      meta_label == 1, "TP Hit (Success)",
      meta_label == 0, "SL/Timeout (Failure)",
      default = "Unknown"
    )]

    # Calculate signal positions for arrows
    price_range <- max(week_data$high, na.rm = TRUE) - min(week_data$low, na.rm = TRUE)
    arrow_offset <- price_range * 0.02

    signals[, arrow_y := fifelse(
      primary_signal == 1,
      low - arrow_offset,   # Long: arrow below candle
      high + arrow_offset   # Short: arrow above candle
    )]

    # Base price chart
    p <- ggplot(week_data, aes(x = datetime)) +
      # Price line
      geom_line(aes(y = close), color = "gray30", size = 0.5, alpha = 0.8)

    # Add high/low range as ribbon
    p <- p + geom_ribbon(aes(ymin = low, ymax = high), fill = "steelblue", alpha = 0.15)

    # Add EMA lines if available
    if ("ema_fast" %in% names(week_data)) {
      p <- p + geom_line(aes(y = ema_fast), color = "#3498DB", size = 0.7, linetype = "solid", alpha = 0.8)
    }
    if ("ema_slow" %in% names(week_data)) {
      p <- p + geom_line(aes(y = ema_slow), color = "#E67E22", size = 0.7, linetype = "solid", alpha = 0.8)
    }

    # Add TP/SL barrier lines if requested
    if (show_barriers && "tp_distance" %in% names(signals) && nrow(signals) > 0) {
      barrier_data <- signals[, .(
        datetime_start = datetime,
        datetime_end = datetime + bars_to_exit * 15 * 60,
        entry_price = close,
        tp_price = fifelse(primary_signal == 1,
                           close + tp_distance,
                           close - tp_distance),
        sl_price = fifelse(primary_signal == 1,
                           close - sl_distance,
                           close + sl_distance),
        outcome = outcome
      )]

      # TP lines (green, dashed)
      for (j in 1:nrow(barrier_data)) {
        p <- p + annotate("segment",
                          x = barrier_data$datetime_start[j],
                          xend = barrier_data$datetime_end[j],
                          y = barrier_data$tp_price[j],
                          yend = barrier_data$tp_price[j],
                          color = "#27AE60", linetype = "dashed", size = 0.4, alpha = 0.6)
        # SL lines (red, dashed)
        p <- p + annotate("segment",
                          x = barrier_data$datetime_start[j],
                          xend = barrier_data$datetime_end[j],
                          y = barrier_data$sl_price[j],
                          yend = barrier_data$sl_price[j],
                          color = "#E74C3C", linetype = "dashed", size = 0.4, alpha = 0.6)
      }
    }

    # Add signal arrows
    if (nrow(signals) > 0) {
      # Long signals (upward arrows)
      long_signals <- signals[primary_signal == 1]
      if (nrow(long_signals) > 0) {
        p <- p + geom_point(
          data = long_signals,
          aes(x = datetime, y = arrow_y, color = outcome),
          shape = 24,  # Triangle pointing up
          size = 3,
          fill = NA,
          stroke = 1.5
        )
      }

      # Short signals (downward arrows)
      short_signals <- signals[primary_signal == -1]
      if (nrow(short_signals) > 0) {
        p <- p + geom_point(
          data = short_signals,
          aes(x = datetime, y = arrow_y, color = outcome),
          shape = 25,  # Triangle pointing down
          size = 3,
          fill = NA,
          stroke = 1.5
        )
      }
    }

    # Color scale for outcomes
    p <- p + scale_color_manual(
      values = c("TP Hit (Success)" = "#27AE60",
                 "SL/Timeout (Failure)" = "#E74C3C",
                 "Unknown" = "#95A5A6"),
      name = "Outcome"
    )

    # Format x-axis
    p <- p + scale_x_datetime(
      date_labels = "%a %d.%m\n%H:%M",
      date_breaks = "1 day"
    )

    # Labels and theme
    week_start_str <- format(min(week_data$datetime), "%Y-%m-%d")
    week_end_str <- format(max(week_data$datetime), "%Y-%m-%d")
    n_long <- sum(signals$primary_signal == 1)
    n_short <- sum(signals$primary_signal == -1)
    n_tp <- sum(signals$meta_label == 1, na.rm = TRUE)
    n_sl <- sum(signals$meta_label == 0, na.rm = TRUE)

    p <- p + labs(
      title = sprintf("Price Chart with Signals - Week %d", i),
      subtitle = sprintf("%s to %s | Signals: %d Long, %d Short | Outcomes: %d TP, %d SL",
                         week_start_str, week_end_str, n_long, n_short, n_tp, n_sl),
      x = "Date/Time",
      y = "Price",
      caption = "Triangle Up = Long Entry | Triangle Down = Short Entry | Green = TP Hit | Red = SL/Timeout"
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        plot.caption = element_text(size = 8, color = "gray50"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8)
      )

    if (show_plot) print(p)

    plots[[paste0("week_", i)]] <- p

    # Save individual plot
    if (!is.null(output_path)) {
      filename <- sprintf("price_signals_week_%d_%s.png", i, gsub("-", "", week_start_str))
      ggsave(file.path(output_path, filename), p, width = 16, height = 8, dpi = 300)
      cat(sprintf("Saved: %s\n", filename))
    }
  }

  # Create combined plot with all weeks
  if (n_weeks > 1 && length(plots) > 1) {
    combined <- do.call(grid.arrange, c(plots, ncol = 1))

    if (!is.null(output_path)) {
      ggsave(file.path(output_path, "price_signals_combined.png"),
             combined, width = 16, height = 8 * n_weeks, dpi = 300)
      cat("Saved: price_signals_combined.png\n")
    }
  }

  return(invisible(plots))
}


# =============================================================================
# 7. COMPREHENSIVE REPORT
# =============================================================================

#' Generate all visualizations at once
#'
#' @param dt data.table with all label data
#' @param output_path Output directory
#' @param spread Spread cost
#' @param slippage_pips Slippage in pips
generate_all_visualizations <- function(
    dt,
    output_path,
    spread = 0.00013,
    slippage_pips = 1.0
) {

  cat("\n=== GENERATING ALL VISUALIZATIONS ===\n")

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  # Generate each plot
  plot_label_density_by_hour(dt, output_path, show_plot = FALSE)
  plot_cumulative_edge(dt, spread, slippage_pips, output_path, show_plot = FALSE)
  plot_barrier_profitability_heatmap(dt, output_path, show_plot = FALSE)
  plot_sample_weight_distribution(dt, output_path, show_plot = FALSE)
  plot_meta_label_performance(dt, output_path, show_plot = FALSE)
  plot_price_with_signals(dt, n_weeks = 2, output_path, show_barriers = TRUE, show_plot = FALSE)

  cat(sprintf("\nAll visualizations saved to: %s\n", output_path))
}


cat("\n=== LABELING VISUALIZATIONS MODULE LOADED ===\n")
cat("Functions:\n")
cat("  - plot_label_density_by_hour()\n")
cat("  - plot_cumulative_edge()\n")
cat("  - plot_barrier_profitability_heatmap()\n")
cat("  - plot_sample_weight_distribution()\n")
cat("  - plot_meta_label_performance()\n")
cat("  - plot_price_with_signals()        [NEW]\n")
cat("  - generate_all_visualizations()\n\n")
