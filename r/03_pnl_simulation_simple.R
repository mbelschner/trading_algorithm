# ============================================================================
# PnL SIMULATION - SIMPLE VERSION (Uses cached features)
# ============================================================================
#
# PURPOSE:
# - Use EXISTING cached features from backtest
# - Load trained Long and Short models
# - Generate predictions with probabilities on 2025 data
# - Combine signals and calculate PnL
# - No feature recalculation = fast execution
#
# ============================================================================

cat("\n=== PnL SIMULATION - SIMPLE VERSION ===\n")

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  xgboost,         # Load models
  ggplot2,         # Visualization
  jsonlite         # Read JSON files
)

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
LABEL_VERSION <- "enhanced_neutral"
TEST_YEAR <- 2025

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Test Year: %d\n", TEST_YEAR))

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")
models_path <- file.path(backtest_output_path, "models")
pnl_output_path <- file.path(backtest_output_path, "pnl_simulation")

# Create output folder
if (!dir.exists(pnl_output_path)) {
  dir.create(pnl_output_path, recursive = TRUE)
}

# ===== STEP 1: LOAD MODELS ===================================================

cat("\n=== STEP 1: LOAD TRAINED MODELS ===\n")

model_long_file <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".rds")
)

model_short_file <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".rds")
)

# Check if models exist
if (!file.exists(model_long_file)) {
  stop(sprintf("ERROR: Long model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_long_file))
}
if (!file.exists(model_short_file)) {
  stop(sprintf("ERROR: Short model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_short_file))
}

# Load models (try JSON first, then XGB, then RDS)
cat("Loading Long model...\n")

# Try JSON format first (most robust)
model_long_file_json <- sub("\\.rds$", ".json", model_long_file)
model_long <- xgb.load(model_long_file_json)
model_long_file_xgb <- sub("\\.rds$", ".xgb", model_long_file)
model_long_xgb <- xgb.load(model_long_file_xgb)


cat("\nLoading Short model...\n")

# Try JSON format first (most robust)
model_short_file_json <- sub("\\.rds$", ".json", model_short_file)
model_short <- xgb.load(model_short_file_json)
model_short_file_xgb <- sub("\\.rds$", ".xgb", model_short_file)
model_short <- xgb.load(model_short_file_xgb)

# Extract feature names from JSON files
cat("\nExtracting feature names from JSON...\n")

# Read JSON to get feature names for Long model
model_long_json_data <- fromJSON(model_long_file_json)
features_long <- model_long_json_data$learner$feature_names

# Read JSON to get feature names for Short model
model_short_json_data <- fromJSON(model_short_file_json)
features_short <- model_short_json_data$learner$feature_names

cat(sprintf("✓ Long model features extracted: %d features\n", length(features_long)))
cat(sprintf("✓ Short model features extracted: %d features\n", length(features_short)))

# ===== STEP 2: LOAD CACHED FEATURES ==========================================

cat("\n=== STEP 2: LOAD CACHED FEATURES ===\n")

features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (!file.exists(features_cache_file)) {
  stop(sprintf("ERROR: Features cache not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", features_cache_file))
}

cat(sprintf("Loading features from cache...\n"))
dt_features <- fread(features_cache_file)
setDT(dt_features)

# Convert datetime
if (is.character(dt_features$datetime)) {
  dt_features[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("✓ Features loaded: %s rows, %d columns\n",
            format(nrow(dt_features), big.mark = ","),
            ncol(dt_features)))

# ===== STEP 3: LOAD LABELS ===================================================

cat("\n=== STEP 3: LOAD LABELS ===\n")

label_file_map <- list(
  "enhanced_neutral" = paste0(EPIC, "_", INTERVAL, "_labeled_enhanced_neutral.csv"),
  "raw" = paste0(EPIC, "_", INTERVAL, "_labeled_raw.csv"),
  "standard" = paste0(EPIC, "_", INTERVAL, "_labeled.csv"),
  "unfiltered" = paste0(EPIC, "_", INTERVAL, "_labeled_unfiltered.csv")
)

labels_file <- file.path(labelled_data_path, label_file_map[[LABEL_VERSION]])

if (!file.exists(labels_file)) {
  stop(sprintf("ERROR: Labels not found!\nPath: %s", labels_file))
}

cat(sprintf("Loading labels...\n"))
dt_labels <- fread(labels_file)
setDT(dt_labels)

# Convert datetime
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("✓ Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

# ===== STEP 4: MERGE AND FILTER FOR 2025 ====================================

cat("\n=== STEP 4: MERGE FEATURES AND LABELS ===\n")

# Select label columns to merge
label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                          "bars_to_exit", "realized_return", "n_concurrent",
                          "realized_return_adj", "log_return")

available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))

cat(sprintf("Merging features with labels...\n"))

# Merge
dt_merged <- merge(
  dt_features,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE
)

cat(sprintf("✓ Merged: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# Add year column and filter for 2025
dt_merged[, year := as.integer(format(datetime, "%Y"))]
dt_test <- dt_merged[year == TEST_YEAR]

cat(sprintf("✓ Filtered for %d: %s rows\n",
            TEST_YEAR, format(nrow(dt_test), big.mark = ",")))

if (nrow(dt_test) == 0) {
  stop(sprintf("ERROR: No data for year %d!", TEST_YEAR))
}

cat("\nLabel distribution:\n")
print(table(dt_test$label))

# ===== STEP 5: VERIFY FEATURES ===============================================

cat("\n=== STEP 5: VERIFY REQUIRED FEATURES ===\n")

# Check Long model features
missing_long <- setdiff(features_long, names(dt_test))
if (length(missing_long) > 0) {
  cat(sprintf("ERROR: Missing %d features for Long model!\n", length(missing_long)))
  cat("First 10 missing features:\n")
  print(head(missing_long, 10))
  stop("Cannot continue - features missing!")
}
cat(sprintf("✓ All Long model features present (%d)\n", length(features_long)))

# Check Short model features
missing_short <- setdiff(features_short, names(dt_test))
if (length(missing_short) > 0) {
  cat(sprintf("ERROR: Missing %d features for Short model!\n", length(missing_short)))
  cat("First 10 missing features:\n")
  print(head(missing_short, 10))
  stop("Cannot continue - features missing!")
}
cat(sprintf("✓ All Short model features present (%d)\n", length(features_short)))

# ===== STEP 6: GENERATE PREDICTIONS ==========================================

cat("\n=== STEP 6: GENERATE PREDICTIONS ===\n")

# Long model
cat("Predicting with Long model...\n")
X_test_long <- as.matrix(dt_test[, ..features_long])
dtest_long <- xgb.DMatrix(data = X_test_long)
pred_prob_long <- predict(model_long, dtest_long)
dt_test[, pred_prob_long := pred_prob_long]

cat(sprintf("✓ Long predictions: [%.4f, %.4f] (mean: %.4f)\n",
            min(pred_prob_long, na.rm = TRUE),
            max(pred_prob_long, na.rm = TRUE),
            mean(pred_prob_long, na.rm = TRUE)))

# Short model
cat("Predicting with Short model...\n")
X_test_short <- as.matrix(dt_test[, ..features_short])
dtest_short <- xgb.DMatrix(data = X_test_short)
pred_prob_short <- predict(model_short, dtest_short)
dt_test[, pred_prob_short := pred_prob_short]

cat(sprintf("✓ Short predictions: [%.4f, %.4f] (mean: %.4f)\n",
            min(pred_prob_short, na.rm = TRUE),
            max(pred_prob_short, na.rm = TRUE),
            mean(pred_prob_short, na.rm = TRUE)))

# ===== STEP 7: COMBINE SIGNALS ===============================================

cat("\n=== STEP 7: COMBINE SIGNALS ===\n")

THRESHOLD <- 0.5

dt_test[, signal_long := fifelse(pred_prob_long > THRESHOLD, 1, 0)]
dt_test[, signal_short := fifelse(pred_prob_short > THRESHOLD, 1, 0)]

dt_test[, signal := fcase(
  signal_long == 1 & signal_short == 0, 1L,
  signal_short == 1 & signal_long == 0, -1L,
  signal_long == 1 & signal_short == 1 & pred_prob_long > pred_prob_short, 1L,
  signal_long == 1 & signal_short == 1 & pred_prob_short >= pred_prob_long, -1L,
  default = 0L
)]

cat("\nSignal distribution:\n")
print(table(dt_test$signal))

n_long <- sum(dt_test$signal == 1, na.rm = TRUE)
n_short <- sum(dt_test$signal == -1, na.rm = TRUE)
n_neutral <- sum(dt_test$signal == 0, na.rm = TRUE)

cat(sprintf("\n  Long:    %s (%.2f%%)\n", format(n_long, big.mark = ","), 100 * n_long / nrow(dt_test)))
cat(sprintf("  Short:   %s (%.2f%%)\n", format(n_short, big.mark = ","), 100 * n_short / nrow(dt_test)))
cat(sprintf("  Neutral: %s (%.2f%%)\n", format(n_neutral, big.mark = ","), 100 * n_neutral / nrow(dt_test)))

# ===== STEP 8: CALCULATE PnL =================================================

cat("\n=== STEP 8: CALCULATE PnL ===\n")

setorder(dt_test, datetime)
dt_test[, idx := .I]

# Calculate exit price
dt_test[, exit_idx := idx + bars_to_exit]
dt_test[, entry_price := close]
dt_test[, exit_price := dt_test$close[pmin(exit_idx, nrow(dt_test))]]

# Calculate log return
dt_test[, log_return_calculated := log(exit_price / entry_price)]

# PnL
dt_test[, pnl := signal * log_return_calculated]
dt_test[signal == 0, pnl := 0]

cat("✓ PnL calculated\n")

# ===== STEP 9: METRICS =======================================================

cat("\n=== STEP 9: PERFORMANCE METRICS ===\n")

dt_trades <- dt_test[signal != 0]
n_trades <- nrow(dt_trades)

if (n_trades == 0) {
  cat("\n⚠ WARNING: NO TRADES GENERATED!\n")
  cat("This means all predictions were below threshold 0.5\n")
  cat("\nTo fix this, you have two options:\n")
  cat("1. Lower the threshold (e.g., THRESHOLD <- 0.3)\n")
  cat("2. Use adaptive thresholds (percentile-based)\n")
  stop("Cannot calculate metrics - no trades!")
}

n_long_trades <- sum(dt_trades$signal == 1, na.rm = TRUE)
n_short_trades <- sum(dt_trades$signal == -1, na.rm = TRUE)

cat(sprintf("Total trades: %s\n", format(n_trades, big.mark = ",")))
cat(sprintf("  Long:  %s\n", format(n_long_trades, big.mark = ",")))
cat(sprintf("  Short: %s\n", format(n_short_trades, big.mark = ",")))

cumulative_pnl <- sum(dt_trades$pnl, na.rm = TRUE)
mean_pnl <- mean(dt_trades$pnl, na.rm = TRUE)
sd_pnl <- sd(dt_trades$pnl, na.rm = TRUE)
sharpe_ratio <- if (sd_pnl > 0) mean_pnl / sd_pnl else 0

n_winning <- sum(dt_trades$pnl > 0, na.rm = TRUE)
win_rate <- n_winning / n_trades

cat(sprintf("\nCumulative PnL: %.4f (%.2f%%)\n", cumulative_pnl, cumulative_pnl * 100))
cat(sprintf("Mean PnL:       %.4f (%.2f%%)\n", mean_pnl, mean_pnl * 100))
cat(sprintf("Sharpe Ratio:   %.4f\n", sharpe_ratio))
cat(sprintf("Win Rate:       %.2f%%\n", win_rate * 100))

# Drawdown
dt_test[, cumulative_pnl := cumsum(pnl)]
dt_test[, running_max := cummax(cumulative_pnl)]
dt_test[, drawdown := cumulative_pnl - running_max]
max_drawdown <- min(dt_test$drawdown, na.rm = TRUE)

cat(sprintf("Max Drawdown:   %.4f (%.2f%%)\n", max_drawdown, max_drawdown * 100))

# ===== STEP 10: SAVE =========================================================

cat("\n=== STEP 10: SAVE RESULTS ===\n")

output_file <- file.path(
  pnl_output_path,
  paste0(EPIC, "_", INTERVAL, "_pnl_simulation_", LABEL_VERSION, ".csv")
)

output_cols <- c("datetime", "close", "signal", "pred_prob_long", "pred_prob_short",
                 "bars_to_exit", "entry_price", "exit_price", "log_return_calculated",
                 "pnl", "cumulative_pnl", "drawdown")
available_output_cols <- intersect(output_cols, names(dt_test))

fwrite(dt_test[, ..available_output_cols], output_file)
cat(sprintf("✓ Saved: %s\n", output_file))

# ===== COMPLETE ==============================================================

cat("\n=== PnL SIMULATION COMPLETE ===\n\n")
cat("SUMMARY:\n")
cat(sprintf("  Trades: %s\n", format(n_trades, big.mark = ",")))
cat(sprintf("  Cumulative PnL: %.2f%%\n", cumulative_pnl * 100))
cat(sprintf("  Sharpe Ratio: %.2f\n", sharpe_ratio))
cat(sprintf("  Win Rate: %.1f%%\n", win_rate * 100))
cat(sprintf("  Max Drawdown: %.2f%%\n", max_drawdown * 100))
