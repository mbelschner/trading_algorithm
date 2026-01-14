# ============================================================================
# PnL SIMULATION - LONG/SHORT MODELS
# ============================================================================
#
# PURPOSE:
# - Load RAW price data for 2025 (test period)
# - Calculate ALL features from scratch (same as training)
# - Load trained Long and Short models
# - Generate predictions with probabilities
# - Combine signals: Long prob → +1, Short prob → -1, else 0
# - Simulate PnL using log returns and bars_to_exit
# - Calculate performance metrics: Sharpe, Max Drawdown, Win Rate, etc.
#
# ============================================================================

cat("\n=== PnL SIMULATION - LONG/SHORT MODELS ===\n")

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  xgboost,         # Load models
  ggplot2,         # Visualization
  TTR,             # Technical indicators
  zoo,             # Time series
  progress,        # Progress bars
  tictoc,          # Timing
  jsonlite         # Read JSON files
)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
models_path <- file.path(backtest_output_path, "models")
pnl_output_path <- file.path(backtest_output_path, "pnl_simulation")

# Create output folder
if (!dir.exists(pnl_output_path)) {
  dir.create(pnl_output_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
LABEL_VERSION <- "enhanced_neutral"  # Must match the version used in training
TEST_YEAR <- 2025

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Test Year: %d\n", TEST_YEAR))

# ===== STEP 1: LOAD RAW PRICE DATA ===========================================

cat("\n=== STEP 1: LOAD RAW PRICE DATA ===\n")

prices_file <- file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))
cat(sprintf("Loading: %s\n", prices_file))

dt_prices <- fread(prices_file)
setDT(dt_prices)

# Rename 'time' to 'datetime' if necessary
if ("time" %in% names(dt_prices)) {
  setnames(dt_prices, "time", "datetime")
}

# Convert datetime to POSIXct if character
if (is.character(dt_prices$datetime)) {
  dt_prices[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("✓ Price data loaded: %s rows\n", format(nrow(dt_prices), big.mark = ",")))
cat(sprintf("  Time range: %s to %s\n",
            min(dt_prices$datetime), max(dt_prices$datetime)))

# ===== STEP 2: LOAD LABELS (FOR 2025 ONLY) ==================================

cat("\n=== STEP 2: LOAD LABELS ===\n")

label_file_map <- list(
  "enhanced_neutral" = paste0(EPIC, "_", INTERVAL, "_labeled_enhanced_neutral.csv"),
  "raw" = paste0(EPIC, "_", INTERVAL, "_labeled_raw.csv"),
  "standard" = paste0(EPIC, "_", INTERVAL, "_labeled.csv"),
  "unfiltered" = paste0(EPIC, "_", INTERVAL, "_labeled_unfiltered.csv")
)

labels_file <- file.path(labelled_data_path, label_file_map[[LABEL_VERSION]])
cat(sprintf("Loading labels: %s\n", labels_file))

dt_labels <- fread(labels_file)
setDT(dt_labels)

# Convert datetime if needed
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

# Filter for 2025
dt_labels[, year := as.integer(format(datetime, "%Y"))]
dt_labels <- dt_labels[year == TEST_YEAR]

cat(sprintf("✓ Labels loaded for %d: %s rows\n",
            TEST_YEAR, format(nrow(dt_labels), big.mark = ",")))
cat("\n  Label distribution:\n")
print(table(dt_labels$label))

# ===== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA =========================

cat("\n=== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA ===\n")

# Load pipeline modules
cat("\nLoading pipeline modules...\n")
source("r/02_01_indicator_calculation.R")
cat("✓ Indicator Calculation loaded\n")
source("r/02_01b_additional_markets.R")
cat("✓ Additional Markets module loaded\n")
source("r/02_02_feature_engineering.R")
cat("✓ Feature Engineering loaded\n")

# Calculate indicators
cat("\n=== CALCULATING INDICATORS ===\n")
tic()
dt_indicators <- calculate_all_indicators(
  dt = copy(dt_prices),
  ema_periods = c(9, 21, 50, 100),
  rsi_periods = c(14, 28),
  atr_periods = c(14, 28),
  adx_periods = c(14),
  bb_periods = c(20),
  kc_periods = c(20),
  verbose = TRUE
)
toc()

cat(sprintf("Features after indicators: %d columns\n", ncol(dt_indicators)))

# Feature engineering
cat("\n=== FEATURE ENGINEERING ===\n")
tic()
dt_features_all <- engineer_features(
  dt = dt_indicators,
  lag_periods = c(1, 2, 3, 5, 10),
  derivative_orders = c(1, 2),
  hourly_aggregation = TRUE,
  rolling_windows = c(10, 20, 50),
  interaction_features = TRUE,
  verbose = TRUE
)
toc()

cat(sprintf("Features after engineering: %d columns\n", ncol(dt_features_all)))

# Load additional markets (if configured)
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)

if (!is.null(ADDITIONAL_MARKETS) && length(ADDITIONAL_MARKETS) > 0) {
  cat("\n=== LOADING ADDITIONAL MARKETS ===\n")

  dt_additional_markets <- load_additional_markets(
    markets = ADDITIONAL_MARKETS,
    interval = INTERVAL,
    price_data_path = price_data_path,
    lag_periods = ADDITIONAL_MARKETS_LAG_PERIODS,
    verbose = TRUE
  )

  # Merge with main features
  if (!is.null(dt_additional_markets)) {
    dt_features_all <- merge_additional_markets(
      dt_main = dt_features_all,
      dt_additional = dt_additional_markets,
      verbose = TRUE
    )
  }
}

# Remove NA rows (from lags/rolling windows)
n_before_na <- nrow(dt_features_all)
dt_features_all <- na.omit(dt_features_all)
n_after_na <- nrow(dt_features_all)

cat(sprintf("\nRows after NA removal: %s (-%s)\n",
            format(n_after_na, big.mark = ","),
            format(n_before_na - n_after_na, big.mark = ",")))

# ===== STEP 4: MERGE LABELS WITH FEATURES ====================================

cat("\n=== STEP 4: MERGE LABELS WITH FEATURES ===\n")

# Select label columns to merge
label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                          "bars_to_exit", "realized_return", "n_concurrent",
                          "realized_return_adj", "log_return")

available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))

# Merge (inner join - only keep rows with both features AND labels)
dt_test <- merge(
  dt_features_all,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE  # Inner join
)

cat(sprintf("✓ Merged dataset: %s rows\n", format(nrow(dt_test), big.mark = ",")))

if (nrow(dt_test) == 0) {
  stop("ERROR: No data after merging features and labels!")
}

cat("\nLabel distribution after merge:\n")
print(table(dt_test$label))

# ===== STEP 5: LOAD TRAINED MODELS ===========================================

cat("\n=== STEP 5: LOAD TRAINED MODELS ===\n")

# Long model path (use JSON format - more robust)
model_long_file <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".json")
)

# Short model path (use JSON format - more robust)
model_short_file <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".json")
)

# Check if models exist
if (!file.exists(model_long_file)) {
  stop(sprintf("ERROR: Long model not found at: %s", model_long_file))
}
if (!file.exists(model_short_file)) {
  stop(sprintf("ERROR: Short model not found at: %s", model_short_file))
}

# Load models from JSON
cat(sprintf("Loading Long model from: %s\n", basename(model_long_file)))
model_long <- xgb.load(model_long_file)
cat("✓ Long model loaded\n")

cat(sprintf("Loading Short model from: %s\n", basename(model_short_file)))
model_short <- xgb.load(model_short_file)
cat("✓ Short model loaded\n")

# Extract feature names from JSON files
cat("\nExtracting feature names from JSON...\n")

# Read JSON to get feature names for Long model
model_long_json_data <- fromJSON(model_long_file)
features_long <- model_long_json_data$learner$feature_names

# Read JSON to get feature names for Short model
model_short_json_data <- fromJSON(model_short_file)
features_short <- model_short_json_data$learner$feature_names

cat(sprintf("✓ Long model features extracted: %d features\n", length(features_long)))
cat(sprintf("✓ Short model features extracted: %d features\n", length(features_short)))

# ===== STEP 6: GENERATE PREDICTIONS ==========================================

cat("\n=== STEP 6: GENERATE PREDICTIONS ===\n")

# --- Long Model Predictions ---
cat("\nGenerating Long model predictions...\n")

# Check if all required features are available
missing_features_long <- setdiff(features_long, names(dt_test))
if (length(missing_features_long) > 0) {
  stop(sprintf("ERROR: Missing features for Long model: %s",
               paste(missing_features_long, collapse = ", ")))
}

# Create matrix
X_test_long <- as.matrix(dt_test[, ..features_long])

# Predict probabilities
dtest_long <- xgb.DMatrix(data = X_test_long)
pred_prob_long <- predict(model_long, dtest_long)

dt_test[, pred_prob_long := pred_prob_long]

cat(sprintf("✓ Long predictions generated\n"))
cat(sprintf("  Probability range: [%.4f, %.4f]\n",
            min(pred_prob_long, na.rm = TRUE), max(pred_prob_long, na.rm = TRUE)))
cat(sprintf("  Mean: %.4f, Median: %.4f\n",
            mean(pred_prob_long, na.rm = TRUE), median(pred_prob_long, na.rm = TRUE)))

# --- Short Model Predictions ---
cat("\nGenerating Short model predictions...\n")

# Check if all required features are available
missing_features_short <- setdiff(features_short, names(dt_test))
if (length(missing_features_short) > 0) {
  stop(sprintf("ERROR: Missing features for Short model: %s",
               paste(missing_features_short, collapse = ", ")))
}

# Create matrix
X_test_short <- as.matrix(dt_test[, ..features_short])

# Predict probabilities
dtest_short <- xgb.DMatrix(data = X_test_short)
pred_prob_short <- predict(model_short, dtest_short)

dt_test[, pred_prob_short := pred_prob_short]

cat(sprintf("✓ Short predictions generated\n"))
cat(sprintf("  Probability range: [%.4f, %.4f]\n",
            min(pred_prob_short, na.rm = TRUE), max(pred_prob_short, na.rm = TRUE)))
cat(sprintf("  Mean: %.4f, Median: %.4f\n",
            mean(pred_prob_short, na.rm = TRUE), median(pred_prob_short, na.rm = TRUE)))

# ===== STEP 7: COMBINE SIGNALS ===============================================

cat("\n=== STEP 7: COMBINE LONG AND SHORT SIGNALS ===\n")

# Define threshold for binary classification (default 0.5)
THRESHOLD <- 0.5

cat(sprintf("Using threshold: %.2f\n", THRESHOLD))

# Create binary signals
dt_test[, signal_long := fifelse(pred_prob_long > THRESHOLD, 1, 0)]
dt_test[, signal_short := fifelse(pred_prob_short > THRESHOLD, 1, 0)]

# Combined signal: Long=+1, Short=-1, Neutral=0
# Priority: If both signal (should be rare), use the one with higher probability
dt_test[, signal := fcase(
  signal_long == 1 & signal_short == 0, 1L,   # Long
  signal_short == 1 & signal_long == 0, -1L,  # Short
  signal_long == 1 & signal_short == 1 & pred_prob_long > pred_prob_short, 1L,  # Both → Long wins
  signal_long == 1 & signal_short == 1 & pred_prob_short >= pred_prob_long, -1L, # Both → Short wins
  default = 0L  # Neutral
)]

# Signal distribution
cat("\nSignal distribution:\n")
print(table(dt_test$signal))

n_long <- sum(dt_test$signal == 1, na.rm = TRUE)
n_short <- sum(dt_test$signal == -1, na.rm = TRUE)
n_neutral <- sum(dt_test$signal == 0, na.rm = TRUE)
n_both <- sum(dt_test$signal_long == 1 & dt_test$signal_short == 1, na.rm = TRUE)

cat(sprintf("\n  Long signals:    %s (%.2f%%)\n",
            format(n_long, big.mark = ","),
            100 * n_long / nrow(dt_test)))
cat(sprintf("  Short signals:   %s (%.2f%%)\n",
            format(n_short, big.mark = ","),
            100 * n_short / nrow(dt_test)))
cat(sprintf("  Neutral signals: %s (%.2f%%)\n",
            format(n_neutral, big.mark = ","),
            100 * n_neutral / nrow(dt_test)))
cat(sprintf("  Both signals:    %s (resolved by probability)\n",
            format(n_both, big.mark = ",")))

# ===== STEP 8: CALCULATE PnL =================================================

cat("\n=== STEP 8: CALCULATE PnL ===\n")

# We use the log_return and bars_to_exit from the labels
# Strategy:
# - Entry: At current bar
# - Exit: After 'bars_to_exit' bars
# - Return: We already have log_return from triple-barrier labeling

# Sort by datetime
setorder(dt_test, datetime)

# Add row index for forward-looking calculation
dt_test[, idx := .I]

# Calculate exit price for each trade using forward bars
dt_test[, exit_idx := idx + bars_to_exit]
dt_test[, entry_price := close]

# Get exit price by looking up future close
dt_test[, exit_price := dt_test$close[pmin(exit_idx, nrow(dt_test))]]

# Calculate log return
dt_test[, log_return_calculated := log(exit_price / entry_price)]

# PnL = signal * log_return
# Long (signal=1): Profit if price goes up (positive log return)
# Short (signal=-1): Profit if price goes down (negative log return → positive PnL)
dt_test[, pnl := signal * log_return_calculated]

# Only calculate PnL for non-neutral signals
dt_test[signal == 0, pnl := 0]

cat("✓ PnL calculated\n")

# ===== STEP 9: CALCULATE PERFORMANCE METRICS =================================

cat("\n=== STEP 9: PERFORMANCE METRICS ===\n")

# Filter for trades only (non-zero signals)
dt_trades <- dt_test[signal != 0]

n_trades <- nrow(dt_trades)
n_long_trades <- sum(dt_trades$signal == 1, na.rm = TRUE)
n_short_trades <- sum(dt_trades$signal == -1, na.rm = TRUE)

cat(sprintf("\nTotal trades: %s\n", format(n_trades, big.mark = ",")))
cat(sprintf("  Long trades:  %s (%.2f%%)\n",
            format(n_long_trades, big.mark = ","),
            100 * n_long_trades / n_trades))
cat(sprintf("  Short trades: %s (%.2f%%)\n",
            format(n_short_trades, big.mark = ","),
            100 * n_short_trades / n_trades))

# --- Overall Performance ---
cat("\n=== OVERALL PERFORMANCE ===\n")

cumulative_pnl <- sum(dt_trades$pnl, na.rm = TRUE)
mean_pnl <- mean(dt_trades$pnl, na.rm = TRUE)
sd_pnl <- sd(dt_trades$pnl, na.rm = TRUE)
sharpe_ratio <- if (sd_pnl > 0) mean_pnl / sd_pnl else 0

# Win rate
n_winning_trades <- sum(dt_trades$pnl > 0, na.rm = TRUE)
n_losing_trades <- sum(dt_trades$pnl < 0, na.rm = TRUE)
n_breakeven_trades <- sum(dt_trades$pnl == 0, na.rm = TRUE)
win_rate <- n_winning_trades / n_trades

# Average win/loss
avg_win <- mean(dt_trades[pnl > 0]$pnl, na.rm = TRUE)
avg_loss <- mean(dt_trades[pnl < 0]$pnl, na.rm = TRUE)
profit_factor <- if (!is.na(avg_loss) && avg_loss != 0) {
  abs(avg_win / avg_loss)
} else {
  NA
}

cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
            cumulative_pnl, cumulative_pnl * 100))
cat(sprintf("Mean PnL per trade:    %.6f (%.4f%%)\n",
            mean_pnl, mean_pnl * 100))
cat(sprintf("Std Dev PnL:           %.6f\n", sd_pnl))
cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_ratio))
cat(sprintf("\nWin Rate:              %.2f%% (%d / %d)\n",
            win_rate * 100, n_winning_trades, n_trades))
cat(sprintf("Losing Rate:           %.2f%% (%d / %d)\n",
            (n_losing_trades / n_trades) * 100, n_losing_trades, n_trades))
cat(sprintf("Breakeven:             %d trades\n", n_breakeven_trades))
cat(sprintf("\nAverage Win:           %.6f (%.4f%%)\n",
            avg_win, avg_win * 100))
cat(sprintf("Average Loss:          %.6f (%.4f%%)\n",
            avg_loss, avg_loss * 100))
cat(sprintf("Profit Factor:         %.4f\n", profit_factor))

# --- Long Trades Performance ---
cat("\n=== LONG TRADES PERFORMANCE ===\n")

dt_long_trades <- dt_trades[signal == 1]
if (nrow(dt_long_trades) > 0) {
  cumulative_pnl_long <- sum(dt_long_trades$pnl, na.rm = TRUE)
  mean_pnl_long <- mean(dt_long_trades$pnl, na.rm = TRUE)
  sd_pnl_long <- sd(dt_long_trades$pnl, na.rm = TRUE)
  sharpe_long <- if (sd_pnl_long > 0) mean_pnl_long / sd_pnl_long else 0
  win_rate_long <- sum(dt_long_trades$pnl > 0, na.rm = TRUE) / nrow(dt_long_trades)

  cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
              cumulative_pnl_long, cumulative_pnl_long * 100))
  cat(sprintf("Mean PnL per trade:    %.6f (%.4f%%)\n",
              mean_pnl_long, mean_pnl_long * 100))
  cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_long))
  cat(sprintf("Win Rate:              %.2f%%\n", win_rate_long * 100))
} else {
  cat("No long trades found.\n")
}

# --- Short Trades Performance ---
cat("\n=== SHORT TRADES PERFORMANCE ===\n")

dt_short_trades <- dt_trades[signal == -1]
if (nrow(dt_short_trades) > 0) {
  cumulative_pnl_short <- sum(dt_short_trades$pnl, na.rm = TRUE)
  mean_pnl_short <- mean(dt_short_trades$pnl, na.rm = TRUE)
  sd_pnl_short <- sd(dt_short_trades$pnl, na.rm = TRUE)
  sharpe_short <- if (sd_pnl_short > 0) mean_pnl_short / sd_pnl_short else 0
  win_rate_short <- sum(dt_short_trades$pnl > 0, na.rm = TRUE) / nrow(dt_short_trades)

  cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
              cumulative_pnl_short, cumulative_pnl_short * 100))
  cat(sprintf("Mean PnL per trade:    %.6f (%.4f%%)\n",
              mean_pnl_short, mean_pnl_short * 100))
  cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_short))
  cat(sprintf("Win Rate:              %.2f%%\n", win_rate_short * 100))
} else {
  cat("No short trades found.\n")
}

# --- Drawdown Analysis ---
cat("\n=== DRAWDOWN ANALYSIS ===\n")

# Calculate cumulative PnL over time
dt_test[, cumulative_pnl := cumsum(pnl)]

# Calculate running maximum
dt_test[, running_max := cummax(cumulative_pnl)]

# Drawdown = current cumulative PnL - running maximum
dt_test[, drawdown := cumulative_pnl - running_max]

max_drawdown <- min(dt_test$drawdown, na.rm = TRUE)

cat(sprintf("Max Drawdown:          %.6f (%.2f%%)\n",
            max_drawdown, max_drawdown * 100))

# ===== STEP 10: SAVE RESULTS =================================================

cat("\n=== STEP 10: SAVE RESULTS ===\n")

# Save full test set with predictions and PnL
output_file <- file.path(
  pnl_output_path,
  paste0(EPIC, "_", INTERVAL, "_pnl_simulation_", LABEL_VERSION, ".csv")
)

# Select relevant columns
output_cols <- c(
  "datetime", "close", "signal", "pred_prob_long", "pred_prob_short",
  "bars_to_exit", "entry_price", "exit_price", "log_return_calculated",
  "pnl", "cumulative_pnl", "drawdown"
)

# Check which columns exist
available_output_cols <- intersect(output_cols, names(dt_test))

fwrite(dt_test[, ..available_output_cols], output_file)
cat(sprintf("✓ Results saved: %s\n", output_file))

# Save summary metrics
summary_file <- file.path(
  pnl_output_path,
  paste0(EPIC, "_", INTERVAL, "_pnl_summary_", LABEL_VERSION, ".txt")
)

sink(summary_file)
cat("=== PnL SIMULATION SUMMARY ===\n\n")
cat(sprintf("Epic: %s\n", EPIC))
cat(sprintf("Interval: %s\n", INTERVAL))
cat(sprintf("Label Version: %s\n", LABEL_VERSION))
cat(sprintf("Test Year: %d\n\n", TEST_YEAR))

cat(sprintf("Total Bars: %s\n", format(nrow(dt_test), big.mark = ",")))
cat(sprintf("Total Trades: %s\n", format(n_trades, big.mark = ",")))
cat(sprintf("  Long Trades:  %s (%.2f%%)\n",
            format(n_long_trades, big.mark = ","),
            100 * n_long_trades / n_trades))
cat(sprintf("  Short Trades: %s (%.2f%%)\n\n",
            format(n_short_trades, big.mark = ","),
            100 * n_short_trades / n_trades))

cat("=== OVERALL PERFORMANCE ===\n")
cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
            cumulative_pnl, cumulative_pnl * 100))
cat(sprintf("Mean PnL per trade:    %.6f (%.4f%%)\n",
            mean_pnl, mean_pnl * 100))
cat(sprintf("Std Dev PnL:           %.6f\n", sd_pnl))
cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_ratio))
cat(sprintf("Win Rate:              %.2f%%\n", win_rate * 100))
cat(sprintf("Average Win:           %.6f (%.4f%%)\n",
            avg_win, avg_win * 100))
cat(sprintf("Average Loss:          %.6f (%.4f%%)\n",
            avg_loss, avg_loss * 100))
cat(sprintf("Profit Factor:         %.4f\n", profit_factor))
cat(sprintf("Max Drawdown:          %.6f (%.2f%%)\n\n",
            max_drawdown, max_drawdown * 100))

if (nrow(dt_long_trades) > 0) {
  cat("=== LONG TRADES ===\n")
  cat(sprintf("Trades: %s\n", format(nrow(dt_long_trades), big.mark = ",")))
  cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
              cumulative_pnl_long, cumulative_pnl_long * 100))
  cat(sprintf("Mean PnL:              %.6f (%.4f%%)\n",
              mean_pnl_long, mean_pnl_long * 100))
  cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_long))
  cat(sprintf("Win Rate:              %.2f%%\n\n", win_rate_long * 100))
}

if (nrow(dt_short_trades) > 0) {
  cat("=== SHORT TRADES ===\n")
  cat(sprintf("Trades: %s\n", format(nrow(dt_short_trades), big.mark = ",")))
  cat(sprintf("Cumulative PnL:        %.6f (%.2f%%)\n",
              cumulative_pnl_short, cumulative_pnl_short * 100))
  cat(sprintf("Mean PnL:              %.6f (%.4f%%)\n",
              mean_pnl_short, mean_pnl_short * 100))
  cat(sprintf("Sharpe Ratio:          %.4f\n", sharpe_short))
  cat(sprintf("Win Rate:              %.2f%%\n\n", win_rate_short * 100))
}

sink()

cat(sprintf("✓ Summary saved: %s\n", summary_file))

# ===== STEP 11: VISUALIZATIONS ===============================================

cat("\n=== STEP 11: CREATE VISUALIZATIONS ===\n")

# --- Cumulative PnL Plot ---
cat("Creating cumulative PnL plot...\n")

p_cumulative <- ggplot(dt_test, aes(x = datetime, y = cumulative_pnl)) +
  geom_line(color = "blue", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = sprintf("Cumulative PnL - %s %s (%d)", EPIC, INTERVAL, TEST_YEAR),
    x = "Date",
    y = "Cumulative PnL (Log Returns)",
    subtitle = sprintf("Sharpe: %.2f | Win Rate: %.1f%% | Max DD: %.2f%%",
                       sharpe_ratio, win_rate * 100, max_drawdown * 100)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

plot_file_cumulative <- file.path(
  pnl_output_path,
  paste0(EPIC, "_", INTERVAL, "_cumulative_pnl_", LABEL_VERSION, ".png")
)

ggsave(plot_file_cumulative, p_cumulative, width = 12, height = 6, dpi = 300)
cat(sprintf("✓ Cumulative PnL plot saved: %s\n", plot_file_cumulative))

# --- Drawdown Plot ---
cat("Creating drawdown plot...\n")

p_drawdown <- ggplot(dt_test, aes(x = datetime, y = drawdown)) +
  geom_line(color = "red", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = sprintf("Drawdown - %s %s (%d)", EPIC, INTERVAL, TEST_YEAR),
    x = "Date",
    y = "Drawdown (Log Returns)",
    subtitle = sprintf("Max Drawdown: %.2f%%", max_drawdown * 100)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

plot_file_drawdown <- file.path(
  pnl_output_path,
  paste0(EPIC, "_", INTERVAL, "_drawdown_", LABEL_VERSION, ".png")
)

ggsave(plot_file_drawdown, p_drawdown, width = 12, height = 6, dpi = 300)
cat(sprintf("✓ Drawdown plot saved: %s\n", plot_file_drawdown))

# --- PnL Distribution ---
if (nrow(dt_trades) > 0) {
  cat("Creating PnL distribution plot...\n")

  p_pnl_dist <- ggplot(dt_trades, aes(x = pnl)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = mean_pnl, linetype = "solid", color = "green", size = 1) +
    labs(
      title = sprintf("PnL Distribution - %s %s (%d)", EPIC, INTERVAL, TEST_YEAR),
      x = "PnL per Trade (Log Returns)",
      y = "Frequency",
      subtitle = sprintf("Mean: %.4f%% | Std: %.4f%%",
                         mean_pnl * 100, sd_pnl * 100)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )

  plot_file_dist <- file.path(
    pnl_output_path,
    paste0(EPIC, "_", INTERVAL, "_pnl_distribution_", LABEL_VERSION, ".png")
  )

  ggsave(plot_file_dist, p_pnl_dist, width = 10, height = 6, dpi = 300)
  cat(sprintf("✓ PnL distribution plot saved: %s\n", plot_file_dist))
}

# ===== COMPLETE ==============================================================

cat("\n=== PnL SIMULATION COMPLETE ===\n")
cat(sprintf("\nOutput files saved to: %s\n", pnl_output_path))
cat("  - Full results CSV with predictions and PnL\n")
cat("  - Summary text file with metrics\n")
cat("  - Cumulative PnL plot\n")
cat("  - Drawdown plot\n")
if (nrow(dt_trades) > 0) {
  cat("  - PnL distribution plot\n")
}
