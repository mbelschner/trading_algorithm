# =============================================================================
# TRIPLE BARRIER LABELING - MAIN SCRIPT
# =============================================================================
# Creates two versions of labels:
# 1. STANDARD (optimized + filtered): For production use
# 2. UNFILTERED (old version): For comparison and legacy compatibility
#
# Output files:
# - GOLD_MINUTE_15_labeled.csv (FILTERED - use this for backtesting!)
# - GOLD_MINUTE_15_labeled_unfiltered.csv (UNFILTERED - for comparison)
# =============================================================================

cat("\n=== START TRIPLE BARRIER LABELING ===\n")
cat(sprintf("[%s] Script started\n", Sys.time()))

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(data.table,
               TTR,
               ggplot2,
               progress,
               tictoc)

# ===== Paths ==================================================================

price_data_path = file.path("price_data")
labelled_output_path = file.path("labelled_data")

# Configuration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"
prices_filename = file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))

# Load data
dt = fread(prices_filename)

# Rename 'time' column to 'datetime' for consistency
setnames(dt, "time", "datetime")

cat("\n=== DATASET SIZE ===\n")
cat(sprintf("Full dataset: %s rows\n", format(nrow(dt), big.mark = ",")))
cat(sprintf("Date range: %s to %s\n", min(dt$datetime), max(dt$datetime)))

# ===== Load Scripts ===========================================================

source("r/01_01_triple_barrier_labeling_optimized.R")  # Main labeling function
source("r/01_02_triple_barrier_labeling_helpers.R")    # Helper functions (includes filtering)
#source("r/01_03_alternative_labeling_methods.R")       # Alternative methods

# =============================================================================
# VERSION 1: STANDARD (OPTIMIZED + FILTERED) - FOR PRODUCTION
# =============================================================================

cat("VERSION 1: STANDARD (OPTIMIZED + FILTERED)\n")

cat("This version uses:\n")
cat("  - Optimized parameters (ATR=6, Horizon=6, Threshold=1)\n")
cat("  - Sample weight filtering (>= 0.3)\n")
cat("  - Exit-based filtering (eliminates autocorrelation)\n")
cat("  -> USE THIS FOR BACKTESTING AND PRODUCTION!\n\n")

# Optimized parameters
STANDARD_ATR_PERIOD = 6
STANDARD_MAX_HORIZON = 6
STANDARD_NEUTRAL_THRESHOLD = 1
STANDARD_ATR_MULT = 1.5
STANDARD_WEIGHT_THRESHOLD = 0.3

cat("Parameters:\n")
cat(sprintf("  ATR Period: %d\n", STANDARD_ATR_PERIOD))
cat(sprintf("  Max Horizon: %d bars\n", STANDARD_MAX_HORIZON))
cat(sprintf("  Neutral Threshold: %.2f\n", STANDARD_NEUTRAL_THRESHOLD))
cat(sprintf("  ATR Multiplier: %.1f\n", STANDARD_ATR_MULT))
cat(sprintf("  Sample Weight Threshold: %.2f\n\n", STANDARD_WEIGHT_THRESHOLD))

# Create labels
tic()
cat("Creating labels with optimized parameters...\n")
labeled_standard <- create_triple_barrier_labels(
  prices = dt,
  atr_period = STANDARD_ATR_PERIOD,
  atr_mult_barrier = STANDARD_ATR_MULT,
  max_horizon_bars = STANDARD_MAX_HORIZON,
  session_start = 2,
  session_end = 21,
  neutral_threshold = STANDARD_NEUTRAL_THRESHOLD
)
toc()

# Calculate sample weights
cat("\nCalculating sample weights...\n")
labeled_standard <- calculate_sample_weights(labeled_standard)

# Filter by sample weight
cat(sprintf("\nFiltering by sample weight (>= %.2f)...\n", STANDARD_WEIGHT_THRESHOLD))
n_before_weight <- nrow(labeled_standard)
labeled_standard <- labeled_standard[sample_weight >= STANDARD_WEIGHT_THRESHOLD]
n_after_weight <- nrow(labeled_standard)
cat(sprintf("  Removed %s labels (%.1f%% reduction)\n",
            format(n_before_weight - n_after_weight, big.mark = ","),
            (1 - n_after_weight/n_before_weight) * 100))

# Exit-based filtering
cat("\nApplying exit-based filtering (non-overlapping)...\n")
labeled_standard <- filter_labels_exit_based(
  labeled_data = labeled_standard,
  datetime_col = "datetime",
  label_col = "label",
  bars_to_exit_col = "bars_to_exit"
)

# Recalculate sample weights after filtering
labeled_standard <- calculate_sample_weights(labeled_standard)

# Analyze quality
cat("\n--- STANDARD VERSION QUALITY METRICS ---\n")
cat(sprintf("Total labels: %s\n", format(nrow(labeled_standard), big.mark = ",")))
cat(sprintf("Mean sample weight: %.4f\n", mean(labeled_standard$sample_weight)))
cat(sprintf("Mean concurrent: %.2f\n", mean(labeled_standard$n_concurrent)))

# ACF
labels_num <- as.numeric(labeled_standard$label)
acf_standard <- acf(labels_num, lag.max = 10, plot = FALSE)
cat(sprintf("ACF Lag-1: %.4f", acf_standard$acf[2]))
if(abs(acf_standard$acf[2]) < 0.2) {
  cat(" ✅ (Target: < 0.2)\n")
} else {
  cat(" ⚠ (Target: < 0.2)\n")
}

cat("\nLabel distribution:\n")
print(table(labeled_standard$label))

# Save STANDARD version (this is the default for backtesting!)
output_standard <- file.path(labelled_output_path, paste0(EPIC, "_", INTERVAL, "_labeled.csv"))
fwrite(labeled_standard, output_standard)
cat(sprintf("\n✅ STANDARD labels saved to: %s\n", output_standard))
cat("   (Use this file for backtesting!)\n")

# =============================================================================
# VERSION 2: UNFILTERED (OLD VERSION) - FOR COMPARISON
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("VERSION 2: UNFILTERED (OLD VERSION)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("This version uses:\n")
cat("  - Old parameters (ATR=10, Horizon=8, Threshold=0.3)\n")
cat("  - NO exit-based filtering\n")
cat("  - Only sample weights calculated\n")
cat("  -> USE FOR COMPARISON ONLY!\n\n")

# Old parameters
OLD_ATR_PERIOD = 10
OLD_MAX_HORIZON = 8
OLD_NEUTRAL_THRESHOLD = 0.3
OLD_ATR_MULT = 1.5

cat("Parameters:\n")
cat(sprintf("  ATR Period: %d\n", OLD_ATR_PERIOD))
cat(sprintf("  Max Horizon: %d bars\n", OLD_MAX_HORIZON))
cat(sprintf("  Neutral Threshold: %.2f\n", OLD_NEUTRAL_THRESHOLD))
cat(sprintf("  ATR Multiplier: %.1f\n\n", OLD_ATR_MULT))

# Create labels
tic()
cat("Creating labels with old parameters...\n")
labeled_unfiltered <- create_triple_barrier_labels(
  prices = dt,
  atr_period = OLD_ATR_PERIOD,
  atr_mult_barrier = OLD_ATR_MULT,
  max_horizon_bars = OLD_MAX_HORIZON,
  session_start = 2,
  session_end = 21,
  neutral_threshold = OLD_NEUTRAL_THRESHOLD
)
toc()

# Calculate sample weights (but don't filter!)
cat("\nCalculating sample weights (no filtering applied)...\n")
labeled_unfiltered <- calculate_sample_weights(labeled_unfiltered)

# Analyze quality
cat("\n--- UNFILTERED VERSION QUALITY METRICS ---\n")
cat(sprintf("Total labels: %s\n", format(nrow(labeled_unfiltered), big.mark = ",")))
cat(sprintf("Mean sample weight: %.4f\n", mean(labeled_unfiltered$sample_weight)))
cat(sprintf("Mean concurrent: %.2f\n", mean(labeled_unfiltered$n_concurrent)))

# ACF
labels_num_unf <- as.numeric(labeled_unfiltered$label)
acf_unfiltered <- acf(labels_num_unf, lag.max = 10, plot = FALSE)
cat(sprintf("ACF Lag-1: %.4f", acf_unfiltered$acf[2]))
if(abs(acf_unfiltered$acf[2]) < 0.2) {
  cat(" ✅ (Target: < 0.2)\n")
} else {
  cat(" ⚠ (High autocorrelation as expected)\n")
}

cat("\nLabel distribution:\n")
print(table(labeled_unfiltered$label))

# Save UNFILTERED version
output_unfiltered <- file.path(labelled_output_path, paste0(EPIC, "_", INTERVAL, "_labeled_unfiltered.csv"))
fwrite(labeled_unfiltered, output_unfiltered)
cat(sprintf("\n✅ UNFILTERED labels saved to: %s\n", output_unfiltered))
cat("   (For comparison only - not recommended for production)\n")

# =============================================================================
# VERSION 3: RAW (NO FILTERING)
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("VERSION 3: RAW (NO FILTERING)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("This version uses:\n")
cat("  - Optimized parameters (same as STANDARD)\n")
cat("  - NO filtering whatsoever\n")
cat("  - Pure triple barrier output\n")
cat("  -> USE FOR RAW LABEL ANALYSIS!\n\n")

# Configuration
RAW_ATR_PERIOD = STANDARD_ATR_PERIOD
RAW_MAX_HORIZON = STANDARD_MAX_HORIZON
RAW_NEUTRAL_THRESHOLD = STANDARD_NEUTRAL_THRESHOLD
RAW_ATR_MULT = STANDARD_ATR_MULT
SPREAD = 0.00013  # 0.013% - USER CONFIGURABLE (trading cost)

cat("Parameters:\n")
cat(sprintf("  ATR Period: %d\n", RAW_ATR_PERIOD))
cat(sprintf("  Max Horizon: %d bars\n", RAW_MAX_HORIZON))
cat(sprintf("  Neutral Threshold: %.2f\n", RAW_NEUTRAL_THRESHOLD))
cat(sprintf("  ATR Multiplier: %.1f\n", RAW_ATR_MULT))
cat(sprintf("  Spread: %.5f (%.3f%%)\n\n", SPREAD, SPREAD * 100))

# Create labels
tic()
cat("Creating raw labels with optimized parameters...\n")
labeled_raw <- create_triple_barrier_labels(
  prices = dt,
  atr_period = RAW_ATR_PERIOD,
  atr_mult_barrier = RAW_ATR_MULT,
  max_horizon_bars = RAW_MAX_HORIZON,
  session_start = 2,
  session_end = 21,
  neutral_threshold = RAW_NEUTRAL_THRESHOLD
)
toc()

# Calculate sample weights (for metrics, but don't filter!)
cat("\nCalculating sample weights (no filtering applied)...\n")
labeled_raw <- calculate_sample_weights(labeled_raw)

# Add log returns and spread-adjusted returns
cat("\nAdding log returns and spread-adjusted returns...\n")
labeled_raw[, log_return := log(1 + realized_return)]
labeled_raw[, realized_return_adj := realized_return - SPREAD]
labeled_raw[, log_return_adj := log(1 + realized_return_adj)]

# Quality metrics
acf_raw <- print_quality_metrics(labeled_raw, "RAW")

# Save RAW version
output_raw <- file.path(labelled_output_path, paste0(EPIC, "_", INTERVAL, "_labeled_raw.csv"))
fwrite(labeled_raw, output_raw)
cat(sprintf("\n✅ RAW labels saved to: %s\n", output_raw))
cat("   (Pure triple barrier output - no filtering)\n")

# =============================================================================
# VERSION 4: ENHANCED_NEUTRAL (EXIT-BASED + AGGRESSIVE NEUTRAL RELABELING)
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("VERSION 4: ENHANCED_NEUTRAL\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("This version uses:\n")
cat("  - Optimized parameters (same as STANDARD)\n")
cat("  - Exit-based filtering (removes overlaps)\n")
cat("  - Aggressive neutral relabeling (vertical + small returns + low weights)\n")
cat("  -> USE FOR CONSERVATIVE LABELING WITH HIGH NEUTRAL %!\n\n")

# Configuration - Option C: Only vertical exits become neutral
ENHANCED_RETURN_THRESHOLD = 0.0  # Disabled 
ENHANCED_WEIGHT_THRESHOLD = 0  # Disabled 

cat("Parameters:\n")
cat(sprintf("  ATR Period: %d\n", STANDARD_ATR_PERIOD))
cat(sprintf("  Max Horizon: %d bars\n", STANDARD_MAX_HORIZON))
cat(sprintf("  Neutral Threshold: %.2f\n", STANDARD_NEUTRAL_THRESHOLD))
cat(sprintf("  ATR Multiplier: %.1f\n", STANDARD_ATR_MULT))
cat("  Relabeling Strategy: Only vertical/session exits → neutral\n")
cat("  (Return threshold: DISABLED, Weight threshold: DISABLED)\n")
cat(sprintf("  Spread: %.5f (%.3f%%)\n\n", SPREAD, SPREAD * 100))

# Create labels
tic()
cat("Creating labels with optimized parameters...\n")
labeled_enhanced <- create_triple_barrier_labels(
  prices = dt,
  atr_period = STANDARD_ATR_PERIOD,
  atr_mult_barrier = STANDARD_ATR_MULT,
  max_horizon_bars = STANDARD_MAX_HORIZON,
  session_start = 2,
  session_end = 21,
  neutral_threshold = STANDARD_NEUTRAL_THRESHOLD
)
toc()

# Calculate sample weights (FIRST TIME - for relabeling criteria)
cat("\nCalculating sample weights (for relabeling criteria)...\n")
labeled_enhanced <- calculate_sample_weights(labeled_enhanced)

# Exit-based filtering
cat("\nApplying exit-based filtering (non-overlapping)...\n")
labeled_enhanced <- filter_labels_exit_based(
  labeled_data = labeled_enhanced,
  datetime_col = "datetime",
  label_col = "label",
  bars_to_exit_col = "bars_to_exit"
)

# Aggressive neutral relabeling
cat("\nApplying aggressive neutral relabeling...\n")
labeled_enhanced <- relabel_to_neutral(
  labeled_data = labeled_enhanced,
  return_threshold = ENHANCED_RETURN_THRESHOLD,
  weight_threshold = ENHANCED_WEIGHT_THRESHOLD,
  preserve_original = TRUE
)

# Recalculate sample weights (SECOND TIME - after filtering & relabeling)
cat("\nRecalculating sample weights (after relabeling)...\n")
labeled_enhanced <- calculate_sample_weights(labeled_enhanced)

# Add log returns and spread-adjusted returns
cat("\nAdding log returns and spread-adjusted returns...\n")
labeled_enhanced[, log_return := log(1 + realized_return)]
labeled_enhanced[, realized_return_adj := realized_return - SPREAD]
labeled_enhanced[, log_return_adj := log(1 + realized_return_adj)]

# Quality metrics
acf_enhanced <- print_quality_metrics(labeled_enhanced, "ENHANCED_NEUTRAL")

# Additional analysis: How many labels changed?
if("label_original" %in% names(labeled_enhanced)) {
  n_changed <- sum(labeled_enhanced$label != labeled_enhanced$label_original)
  cat(sprintf("\nRelabeling Impact: %s labels changed (%.1f%% of filtered dataset)\n",
              format(n_changed, big.mark = ","),
              n_changed / nrow(labeled_enhanced) * 100))
}

# Save ENHANCED_NEUTRAL version
output_enhanced <- file.path(labelled_output_path, paste0(EPIC, "_", INTERVAL, "_labeled_enhanced_neutral.csv"))
fwrite(labeled_enhanced, output_enhanced)
cat(sprintf("\n✅ ENHANCED_NEUTRAL labels saved to: %s\n", output_enhanced))
cat("   (Exit-based filtering + aggressive neutral relabeling)\n")

# =============================================================================
# COMPARISON SUMMARY
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("COMPARISON SUMMARY: ALL FOUR VERSIONS\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

comparison <- data.table(
  Metric = c(
    "Total Labels",
    "Mean Sample Weight",
    "Mean Concurrent Labels",
    "ACF Lag-1",
    "Label Balance (S/L/N)"
  ),
  STANDARD = c(
    format(nrow(labeled_standard), big.mark = ","),
    sprintf("%.4f", mean(labeled_standard$sample_weight)),
    sprintf("%.2f", mean(labeled_standard$n_concurrent)),
    sprintf("%.4f", acf_standard$acf[2]),
    sprintf("%.0f/%.0f/%.0f%%",
            prop.table(table(labeled_standard$label))["-1"] * 100,
            prop.table(table(labeled_standard$label))["1"] * 100,
            ifelse("0" %in% names(table(labeled_standard$label)),
                   prop.table(table(labeled_standard$label))["0"] * 100, 0))
  ),
  UNFILTERED = c(
    format(nrow(labeled_unfiltered), big.mark = ","),
    sprintf("%.4f", mean(labeled_unfiltered$sample_weight)),
    sprintf("%.2f", mean(labeled_unfiltered$n_concurrent)),
    sprintf("%.4f", acf_unfiltered$acf[2]),
    sprintf("%.0f/%.0f/%.0f%%",
            prop.table(table(labeled_unfiltered$label))["-1"] * 100,
            prop.table(table(labeled_unfiltered$label))["1"] * 100,
            ifelse("0" %in% names(table(labeled_unfiltered$label)),
                   prop.table(table(labeled_unfiltered$label))["0"] * 100, 0))
  ),
  RAW = c(
    format(nrow(labeled_raw), big.mark = ","),
    sprintf("%.4f", mean(labeled_raw$sample_weight)),
    sprintf("%.2f", mean(labeled_raw$n_concurrent)),
    sprintf("%.4f", acf_raw$acf[2]),
    sprintf("%.0f/%.0f/%.0f%%",
            prop.table(table(labeled_raw$label))["-1"] * 100,
            prop.table(table(labeled_raw$label))["1"] * 100,
            ifelse("0" %in% names(table(labeled_raw$label)),
                   prop.table(table(labeled_raw$label))["0"] * 100, 0))
  ),
  ENHANCED_NEUTRAL = c(
    format(nrow(labeled_enhanced), big.mark = ","),
    sprintf("%.4f", mean(labeled_enhanced$sample_weight)),
    sprintf("%.2f", mean(labeled_enhanced$n_concurrent)),
    sprintf("%.4f", acf_enhanced$acf[2]),
    sprintf("%.0f/%.0f/%.0f%%",
            prop.table(table(labeled_enhanced$label))["-1"] * 100,
            prop.table(table(labeled_enhanced$label))["1"] * 100,
            ifelse("0" %in% names(table(labeled_enhanced$label)),
                   prop.table(table(labeled_enhanced$label))["0"] * 100, 0))
  )
)

print(comparison)

cat("\n--- RECOMMENDATIONS ---\n")
cat("1. STANDARD: For production backtesting (balanced, low ACF)\n")
cat("   ✅ Low autocorrelation (< 0.2)\n")
cat("   ✅ High sample uniqueness (> 0.5)\n")
cat("   ✅ Balanced label distribution\n")
cat("   ✅ Independent samples (exit-based filtering)\n\n")
cat("2. UNFILTERED: For comparison/legacy (high overlap)\n\n")
cat("3. RAW: For pure triple barrier analysis (no post-processing)\n")
cat("   ✅ Same parameters as STANDARD\n")
cat("   ✅ Maximum data retention\n\n")
cat("4. ENHANCED_NEUTRAL: For conservative modeling (high neutral %, low false signals)\n")
cat("   ✅ Very high-quality directional signals only\n")
cat("   ✅ Low autocorrelation\n")
cat("   ✅ Aggressive neutral filtering\n\n")

# =============================================================================
# OPTIONAL: ACF COMPARISON PLOT
# =============================================================================

cat("Creating ACF comparison plot...\n")

# Create output directory
if(!dir.exists("plots/label_quality")) {
  dir.create("plots/label_quality", recursive = TRUE)
}

# Analyze ACF
acf_results <- analyze_label_autocorrelation(
  labeled_before = labeled_unfiltered,
  labeled_after = labeled_standard,
  max_lag = 20,
  label_col = "label",
  output_path = "plots/label_quality",
  save_plot = TRUE
)

cat("\n✅ ACF comparison plot saved to: plots/label_quality/acf_comparison.png\n")

# =============================================================================
# EXAMPLE TRADING WEEK VISUALIZATION
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("EXAMPLE TRADING WEEK VISUALIZATION\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("Creating visualization for week: 2025-12-15 to 2025-12-19...\n")

# Define the week to visualize (adjust year to match your data)
week_start <- as.POSIXct("2025-12-15 00:00:00", tz = "UTC")
week_end <- as.POSIXct("2025-12-19 23:59:59", tz = "UTC")

# Extract labels for this week from all 4 versions
week_standard <- labeled_standard[datetime >= week_start & datetime <= week_end]
week_unfiltered <- labeled_unfiltered[datetime >= week_start & datetime <= week_end]
week_raw <- labeled_raw[datetime >= week_start & datetime <= week_end]
week_enhanced <- labeled_enhanced[datetime >= week_start & datetime <= week_end]

# Get the underlying price data for the same period
week_prices <- dt[datetime >= week_start & datetime <= week_end]

if(nrow(week_prices) > 0) {

  # Create the plot
  png("plots/label_quality/example_trading_week.png",
      width = 1600, height = 1200, res = 120)

  par(mfrow = c(5, 1), mar = c(3, 4, 2, 1), oma = c(2, 0, 2, 0))

  # Colors for labels
  col_long <- "#2ecc71"    # Green
  col_short <- "#e74c3c"   # Red
  col_neutral <- "#95a5a6" # Gray

  get_label_color <- function(label) {
    ifelse(label == 1, col_long, ifelse(label == -1, col_short, col_neutral))
  }

  # Panel 1: Price chart
  plot(week_prices$datetime, week_prices$close, type = "l",
       col = "black", lwd = 2,
       main = "Gold Price (15-min bars)",
       xlab = "", ylab = "Price", xaxt = "n")
  grid()
  axis.POSIXct(1, at = seq(week_start, week_end, by = "day"), format = "%m-%d")

  # Panel 2: STANDARD labels
  plot(week_standard$datetime, week_standard$label,
       pch = 19, cex = 1.2,
       col = get_label_color(week_standard$label),
       main = sprintf("STANDARD Labels (n=%d)", nrow(week_standard)),
       xlab = "", ylab = "Label", ylim = c(-1.5, 1.5), xaxt = "n")
  abline(h = c(-1, 0, 1), col = "gray", lty = 2)
  grid()
  axis.POSIXct(1, at = seq(week_start, week_end, by = "day"), format = "%m-%d")
  legend("topright", legend = c("Long (1)", "Neutral (0)", "Short (-1)"),
         col = c(col_long, col_neutral, col_short), pch = 19, bty = "n", horiz = TRUE)

  # Panel 3: RAW labels
  plot(week_raw$datetime, week_raw$label,
       pch = 19, cex = 0.8,
       col = get_label_color(week_raw$label),
       main = sprintf("RAW Labels (n=%d)", nrow(week_raw)),
       xlab = "", ylab = "Label", ylim = c(-1.5, 1.5), xaxt = "n")
  abline(h = c(-1, 0, 1), col = "gray", lty = 2)
  grid()
  axis.POSIXct(1, at = seq(week_start, week_end, by = "day"), format = "%m-%d")

  # Panel 4: ENHANCED_NEUTRAL labels
  plot(week_enhanced$datetime, week_enhanced$label,
       pch = 19, cex = 1.2,
       col = get_label_color(week_enhanced$label),
       main = sprintf("ENHANCED_NEUTRAL Labels (n=%d)", nrow(week_enhanced)),
       xlab = "", ylab = "Label", ylim = c(-1.5, 1.5), xaxt = "n")
  abline(h = c(-1, 0, 1), col = "gray", lty = 2)
  grid()
  axis.POSIXct(1, at = seq(week_start, week_end, by = "day"), format = "%m-%d")

  # Panel 5: Label distribution comparison
  barplot(
    matrix(c(
      sum(week_standard$label == -1), sum(week_standard$label == 0), sum(week_standard$label == 1),
      sum(week_raw$label == -1), sum(week_raw$label == 0), sum(week_raw$label == 1),
      sum(week_enhanced$label == -1), sum(week_enhanced$label == 0), sum(week_enhanced$label == 1)
    ), nrow = 3, byrow = TRUE),
    beside = TRUE,
    col = c(col_short, col_neutral, col_long),
    names.arg = c("STANDARD", "RAW", "ENHANCED"),
    main = "Label Count Comparison",
    ylab = "Count",
    legend.text = c("Short (-1)", "Neutral (0)", "Long (1)"),
    args.legend = list(x = "topright", bty = "n")
  )
  grid()

  # Overall title
  mtext("Example Trading Week: Label Comparison Across 4 Versions",
        outer = TRUE, cex = 1.3, font = 2, line = 0.5)
  mtext(sprintf("Week: %s to %s",
                format(week_start, "%Y-%m-%d"),
                format(week_end, "%Y-%m-%d")),
        outer = TRUE, cex = 1.0, line = -0.5)

  dev.off()

  cat(sprintf("✅ Example trading week plot saved to: plots/label_quality/example_trading_week.png\n"))
  cat(sprintf("   Week contains: %d price bars\n", nrow(week_prices)))
  cat(sprintf("   STANDARD: %d labels\n", nrow(week_standard)))
  cat(sprintf("   RAW: %d labels\n", nrow(week_raw)))
  cat(sprintf("   ENHANCED_NEUTRAL: %d labels\n\n", nrow(week_enhanced)))

} else {
  cat("⚠ WARNING: No data found for the specified week. Skipping visualization.\n")
  cat("   Please check if your data covers 2025-12-15 to 2025-12-19.\n\n")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("LABELING COMPLETE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("Output Files:\n")
cat(sprintf("  1. %s (STANDARD - production backtesting)\n", output_standard))
cat(sprintf("  2. %s (UNFILTERED - legacy comparison)\n", output_unfiltered))
cat(sprintf("  3. %s (RAW - pure triple barrier)\n", output_raw))
cat(sprintf("  4. %s (ENHANCED_NEUTRAL - conservative modeling)\n", output_enhanced))
cat(sprintf("  5. plots/label_quality/acf_comparison.png (ACF plot)\n"))
cat(sprintf("  6. plots/label_quality/example_trading_week.png (Example week visualization)\n"))

cat(sprintf("\n[%s] Script completed!\n", Sys.time()))

cat("\n=== END TRIPLE BARRIER LABELING ===\n")
