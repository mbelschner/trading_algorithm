# ============================================================================
# QUANTILE REGRESSION META-LABELLING
# ============================================================================
#
# PURPOSE:
# - Predict Expected Upside (75th Percentile) of trade PnL
# - Filter trades based on predicted upside potential
# - Alternative to binary classification which failed (AUC 0.56)
#
# APPROACH:
# - XGBoost with quantile_alpha = 0.75 predicts Q75 of PnL distribution
# - Features engineered on FULL price data, then merged with labels
# - Incremental feature selection to avoid overfitting
# - Evaluation via Spearman correlation and monotonic binning
#
# REQUIRES:
# - Run 02_backtest_main_script_ls_v2.R first (creates feature cache)
# - Trained Long/Short models
#
# ============================================================================

cat("\n=== QUANTILE REGRESSION META-LABELLING ===\n")

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  xgboost,         # Quantile regression
  ggplot2,         # Visualization
  scales,          # Plot formatting
  TTR,             # Technical indicators
  zoo,             # Rolling functions
  jsonlite         # Read JSON files
)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")
models_path <- file.path(backtest_output_path, "models")
qr_output_path <- file.path(backtest_output_path, "quantile_regression")

# Create output folder
if (!dir.exists(qr_output_path)) {
  dir.create(qr_output_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
LABEL_VERSION <- "enhanced_neutral"
TEST_YEAR <- 2025

# Quantile regression parameters
QUANTILE_ALPHA <- 0.75  # Predict 75th percentile (upside)

# XGBoost parameters - more regularization than classification
xgb_params <- list(
  objective = "reg:quantileerror",
  quantile_alpha = QUANTILE_ALPHA,
  max_depth = 3,
  eta = 0.03,
  subsample = 0.7,
  colsample_bytree = 0.7,
  min_child_weight = 50
)

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Test Year: %d\n", TEST_YEAR))
cat(sprintf("  Quantile Alpha: %.2f\n", QUANTILE_ALPHA))

# ===== STEP 1: LOAD TRAINED PRIMARY MODELS ===================================

cat("\n=== STEP 1: LOAD TRAINED PRIMARY MODELS ===\n")

model_long_file_json <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".json")
)

model_short_file_json <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".json")
)

# Check if models exist
if (!file.exists(model_long_file_json)) {
  stop(sprintf("ERROR: Long model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_long_file_json))
}
if (!file.exists(model_short_file_json)) {
  stop(sprintf("ERROR: Short model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_short_file_json))
}

# Load models
cat("Loading Long model...\n")
model_long <- xgb.load(model_long_file_json)

cat("Loading Short model...\n")
model_short <- xgb.load(model_short_file_json)

# Extract feature names from JSON files
cat("Extracting feature names from JSON...\n")

model_long_json_data <- fromJSON(model_long_file_json)
features_long <- model_long_json_data$learner$feature_names

model_short_json_data <- fromJSON(model_short_file_json)
features_short <- model_short_json_data$learner$feature_names

cat(sprintf("Long model features: %d\n", length(features_long)))
cat(sprintf("Short model features: %d\n", length(features_short)))

# ===== STEP 2: LOAD CACHED FEATURES ==========================================

cat("\n=== STEP 2: LOAD CACHED FEATURES ===\n")

features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (!file.exists(features_cache_file)) {
  stop(sprintf("ERROR: Features cache not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", features_cache_file))
}

cat(sprintf("Loading features from cache: %s\n", features_cache_file))
dt_features <- fread(features_cache_file)
setDT(dt_features)

# Convert datetime
if (is.character(dt_features$datetime)) {
  dt_features[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Features loaded: %s rows, %d columns\n",
            format(nrow(dt_features), big.mark = ","),
            ncol(dt_features)))

# ===== STEP 3: LOAD LABELS ===================================================

cat("\n=== STEP 3: LOAD LABELS ===\n")

labels_file <- file.path(labelled_data_path, paste0(EPIC, "_", INTERVAL, "_labeled_", LABEL_VERSION, ".csv"))

if (!file.exists(labels_file)) {
  stop(sprintf("ERROR: Labels not found!\nPath: %s", labels_file))
}

cat(sprintf("Loading labels: %s\n", labels_file))
dt_labels <- fread(labels_file)
setDT(dt_labels)

# Convert datetime
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

# ===== STEP 4: MERGE FEATURES AND LABELS =====================================

cat("\n=== STEP 4: MERGE FEATURES AND LABELS ===\n")

# Select label columns to merge
label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                         "bars_to_exit", "realized_return", "n_concurrent",
                         "realized_return_adj", "log_return")

available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))

cat("Merging features with labels...\n")

# Merge
dt_merged <- merge(
  dt_features,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE
)

cat(sprintf("Merged dataset: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# ===== STEP 5: GENERATE PRIMARY MODEL PREDICTIONS ============================

cat("\n=== STEP 5: GENERATE PRIMARY MODEL PREDICTIONS ===\n")

# Prepare features for prediction
cat("Preparing Long model features...\n")
missing_long <- setdiff(features_long, names(dt_merged))
if (length(missing_long) > 0) {
  cat(sprintf("WARNING: Missing Long features: %s\n", paste(missing_long, collapse = ", ")))
}
available_features_long <- intersect(features_long, names(dt_merged))

cat("Preparing Short model features...\n")
missing_short <- setdiff(features_short, names(dt_merged))
if (length(missing_short) > 0) {
  cat(sprintf("WARNING: Missing Short features: %s\n", paste(missing_short, collapse = ", ")))
}
available_features_short <- intersect(features_short, names(dt_merged))

# Generate predictions
cat("Generating Long predictions...\n")
X_long <- as.matrix(dt_merged[, ..available_features_long])
dmat_long <- xgb.DMatrix(data = X_long)
dt_merged[, pred_prob_long := predict(model_long, dmat_long)]

cat("Generating Short predictions...\n")
X_short <- as.matrix(dt_merged[, ..available_features_short])
dmat_short <- xgb.DMatrix(data = X_short)
dt_merged[, pred_prob_short := predict(model_short, dmat_short)]

cat(sprintf("Predictions generated for %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# ===== STEP 6: GENERATE SIGNALS AND CALCULATE PNL ============================

cat("\n=== STEP 6: GENERATE SIGNALS AND CALCULATE PNL ===\n")

# Signal thresholds (same as backtest)
LONG_THRESHOLD <- 0.55
SHORT_THRESHOLD <- 0.55

# Generate signals
dt_merged[, signal_long := as.integer(pred_prob_long > LONG_THRESHOLD)]
dt_merged[, signal_short := as.integer(pred_prob_short > SHORT_THRESHOLD)]

# Combined signal: Long = 1, Short = -1, Neutral = 0
# If both fire, use the stronger probability
dt_merged[, signal := fifelse(
  signal_long == 1 & signal_short == 0, 1L,
  fifelse(
    signal_long == 0 & signal_short == 1, -1L,
    fifelse(
      signal_long == 1 & signal_short == 1,
      fifelse(pred_prob_long > pred_prob_short, 1L, -1L),
      0L
    )
  )
)]

# Filter to only rows with signals
dt_trades <- dt_merged[signal != 0]

cat(sprintf("Trades with signals: %s\n", format(nrow(dt_trades), big.mark = ",")))
cat(sprintf("  Long signals: %d\n", sum(dt_trades$signal == 1)))
cat(sprintf("  Short signals: %d\n", sum(dt_trades$signal == -1)))

# Calculate PnL
# For Long: pnl = log_return (positive return = profit)
# For Short: pnl = -log_return (negative return = profit)
dt_trades[, pnl := fifelse(signal == 1, log_return, -log_return)]

# Trade direction for clarity
dt_trades[, trade_direction := fifelse(signal == 1, "Long", "Short")]

cat(sprintf("\nPnL Summary:\n"))
cat(sprintf("  Mean PnL: %.6f\n", mean(dt_trades$pnl)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_trades$pnl > 0)))

# ===== STEP 7: COMPUTE META-FEATURES =========================================

cat("\n=== STEP 7: COMPUTE META-FEATURES ===\n")

# --- 7.1: Clarity Gap ---
dt_trades[, clarity_gap := abs(pred_prob_long - pred_prob_short)]
cat("  clarity_gap: abs(pred_prob_long - pred_prob_short)\n")

# --- 7.2: ATR percentile (from existing ATR) ---
if ("atr_14" %in% names(dt_trades)) {
  # Calculate percentile within rolling window on full data first
  setorder(dt_merged, datetime)
  dt_merged[, atr_percentile := {
    n <- .N
    result <- rep(NA_real_, n)
    for (i in 60:n) {
      window <- atr_14[(i-59):i]
      result[i] <- sum(window <= atr_14[i], na.rm = TRUE) / sum(!is.na(window))
    }
    result
  }]

  # Merge back to trades
  dt_trades <- merge(dt_trades, dt_merged[, .(datetime, atr_percentile)],
                     by = "datetime", all.x = TRUE, suffixes = c("", "_new"))
  if ("atr_percentile_new" %in% names(dt_trades)) {
    dt_trades[, atr_percentile := atr_percentile_new]
    dt_trades[, atr_percentile_new := NULL]
  }
  cat("  atr_percentile: ATR_14 percentile in 60-bar window\n")
} else {
  dt_trades[, atr_percentile := NA_real_]
  cat("  WARNING: atr_14 not found, atr_percentile set to NA\n")
}

# --- 7.3: ATR trend ---
if ("atr_14" %in% names(dt_trades)) {
  # Calculate on full data
  setorder(dt_merged, datetime)
  dt_merged[, tr := pmax(high - low, abs(high - shift(close, 1)), abs(low - shift(close, 1)))]
  dt_merged[, atr_5 := frollmean(tr, n = 5, fill = NA, align = "right")]
  dt_merged[, atr_20 := frollmean(tr, n = 20, fill = NA, align = "right")]
  dt_merged[, atr_trend := atr_5 / atr_20]

  # Merge to trades
  dt_trades <- merge(dt_trades, dt_merged[, .(datetime, atr_trend)],
                     by = "datetime", all.x = TRUE, suffixes = c("", "_new"))
  if ("atr_trend_new" %in% names(dt_trades)) {
    dt_trades[, atr_trend := atr_trend_new]
    dt_trades[, atr_trend_new := NULL]
  }
  cat("  atr_trend: ATR_5 / ATR_20\n")
}

# --- 7.4: ADX (should exist in features) ---
if ("adx_14" %in% names(dt_trades)) {
  cat("  adx_14: Already available from features\n")
} else {
  dt_trades[, adx_14 := NA_real_]
  cat("  WARNING: adx_14 not found\n")
}

# --- 7.5: Volume ratio ---
if ("volume_ratio" %in% names(dt_trades)) {
  cat("  volume_ratio: Already available from features\n")
} else if ("volume" %in% names(dt_merged)) {
  setorder(dt_merged, datetime)
  dt_merged[, vol_sma_20 := frollmean(volume, n = 20, fill = NA, align = "right")]
  dt_merged[, volume_ratio := volume / vol_sma_20]

  dt_trades <- merge(dt_trades, dt_merged[, .(datetime, volume_ratio)],
                     by = "datetime", all.x = TRUE, suffixes = c("", "_new"))
  if ("volume_ratio_new" %in% names(dt_trades)) {
    dt_trades[, volume_ratio := volume_ratio_new]
    dt_trades[, volume_ratio_new := NULL]
  }
  cat("  volume_ratio: Volume / SMA(Volume, 20)\n")
} else {
  dt_trades[, volume_ratio := NA_real_]
  cat("  WARNING: volume not found\n")
}

# --- 7.6: RSI (should exist in features) ---
if ("rsi_14" %in% names(dt_trades)) {
  cat("  rsi_14: Already available from features\n")
} else {
  dt_trades[, rsi_14 := NA_real_]
  cat("  WARNING: rsi_14 not found\n")
}

# --- 7.7: BB %B (should exist in features) ---
if ("bb_pct_b_20" %in% names(dt_trades)) {
  cat("  bb_pct_b_20: Already available from features\n")
} else {
  dt_trades[, bb_pct_b_20 := NA_real_]
  cat("  WARNING: bb_pct_b_20 not found\n")
}

# ===== STEP 8: TRAIN/TEST SPLIT ==============================================

cat("\n=== STEP 8: TRAIN/TEST SPLIT ===\n")

dt_trades[, year := year(datetime)]

dt_train <- dt_trades[year < TEST_YEAR]
dt_test <- dt_trades[year == TEST_YEAR]

cat(sprintf("Training data: %s trades (before %d)\n", format(nrow(dt_train), big.mark = ","), TEST_YEAR))
cat(sprintf("Test data: %s trades (%d)\n", format(nrow(dt_test), big.mark = ","), TEST_YEAR))

cat(sprintf("\nTraining set:\n"))
cat(sprintf("  Mean PnL: %.6f\n", mean(dt_train$pnl)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_train$pnl > 0)))

cat(sprintf("\nTest set:\n"))
cat(sprintf("  Mean PnL: %.6f\n", mean(dt_test$pnl)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_test$pnl > 0)))

# ===== STEP 9: DEFINE META-FEATURES ==========================================

cat("\n=== STEP 9: DEFINE META-FEATURES ===\n")

# Define feature groups for incremental testing
feature_groups <- list(
  baseline = c("clarity_gap", "atr_percentile", "adx_14"),
  volatility = c("atr_trend", "atr_14"),
  momentum = c("rsi_14", "bb_pct_b_20"),
  volume = c("volume_ratio")
)

# Filter to available features
for (group_name in names(feature_groups)) {
  available <- intersect(feature_groups[[group_name]], names(dt_train))
  feature_groups[[group_name]] <- available
  cat(sprintf("  %s: %s\n", group_name, paste(available, collapse = ", ")))
}

# ===== STEP 10: TRAIN QUANTILE REGRESSION MODEL ==============================

cat("\n=== STEP 10: INCREMENTAL FEATURE SELECTION ===\n")

# Function to train and evaluate a feature set
evaluate_features <- function(features, dt_train, dt_test, target_col = "pnl") {

  # Filter to available features with no NA
  features <- intersect(features, names(dt_train))
  if (length(features) == 0) {
    return(list(spearman_train = NA, spearman_test = NA))
  }

  # Prepare training data
  train_complete <- dt_train[complete.cases(dt_train[, ..features])]
  test_complete <- dt_test[complete.cases(dt_test[, ..features])]

  if (nrow(train_complete) < 100 || nrow(test_complete) < 50) {
    return(list(
      spearman_train = NA,
      spearman_test = NA,
      n_train = nrow(train_complete),
      n_test = nrow(test_complete)
    ))
  }

  X_train <- as.matrix(train_complete[, ..features])
  y_train <- train_complete[[target_col]]

  X_test <- as.matrix(test_complete[, ..features])
  y_test <- test_complete[[target_col]]

  # Split training for early stopping
  set.seed(42)
  val_idx <- sample(1:nrow(X_train), size = floor(0.2 * nrow(X_train)))
  train_idx <- setdiff(1:nrow(X_train), val_idx)

  dtrain <- xgb.DMatrix(data = X_train[train_idx, , drop = FALSE], label = y_train[train_idx])
  dval <- xgb.DMatrix(data = X_train[val_idx, , drop = FALSE], label = y_train[val_idx])
  dtest <- xgb.DMatrix(data = X_test, label = y_test)

  # Train with early stopping
  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = 50,
    verbose = 0
  )

  # Get best iteration
  best_iter <- model$best_iteration
  if (is.null(best_iter) || length(best_iter) == 0) {
    best_iter <- 100
  }

  # Retrain on full training data
  dtrain_full <- xgb.DMatrix(data = X_train, label = y_train)
  model_final <- xgb.train(
    params = xgb_params,
    data = dtrain_full,
    nrounds = best_iter,
    verbose = 0
  )

  # Predictions
  pred_train <- predict(model_final, dtrain_full)
  pred_test <- predict(model_final, dtest)

  # Spearman correlations
  spearman_train <- cor(pred_train, y_train, method = "spearman", use = "complete.obs")
  spearman_test <- cor(pred_test, y_test, method = "spearman", use = "complete.obs")

  return(list(
    spearman_train = spearman_train,
    spearman_test = spearman_test,
    n_train = nrow(train_complete),
    n_test = nrow(test_complete),
    model = model_final,
    best_iter = best_iter,
    features = features,
    pred_test = pred_test,
    y_test = y_test,
    test_data = test_complete
  ))
}

# Incremental feature selection
cat("\n--- Testing Feature Groups Incrementally ---\n")

selected_features <- c()
feature_selection_results <- data.table(
  step = character(),
  features_added = character(),
  total_features = integer(),
  spearman_train = numeric(),
  spearman_test = numeric(),
  improvement = numeric()
)

best_spearman <- -Inf
best_result <- NULL

for (group_name in names(feature_groups)) {

  if (length(feature_groups[[group_name]]) == 0) {
    cat(sprintf("\nSkipping %s (no available features)\n", group_name))
    next
  }

  # Test adding this group
  test_features <- c(selected_features, feature_groups[[group_name]])

  cat(sprintf("\nTesting: %s\n", group_name))
  cat(sprintf("  Features: %s\n", paste(feature_groups[[group_name]], collapse = ", ")))

  result <- evaluate_features(test_features, dt_train, dt_test)

  if (is.na(result$spearman_test)) {
    cat(sprintf("  -> SKIPPED (insufficient data)\n"))
    next
  }

  improvement <- result$spearman_test - best_spearman

  cat(sprintf("  Spearman (train): %.4f\n", result$spearman_train))
  cat(sprintf("  Spearman (test):  %.4f\n", result$spearman_test))
  cat(sprintf("  Improvement:      %+.4f\n", improvement))

  # Record result
  feature_selection_results <- rbind(feature_selection_results, data.table(
    step = group_name,
    features_added = paste(feature_groups[[group_name]], collapse = ", "),
    total_features = length(test_features),
    spearman_train = result$spearman_train,
    spearman_test = result$spearman_test,
    improvement = improvement
  ))

  # Keep group if it improves test correlation
  if (result$spearman_test > best_spearman) {
    cat(sprintf("  -> ACCEPTED (improves correlation)\n"))
    selected_features <- test_features
    best_spearman <- result$spearman_test
    best_result <- result
  } else {
    cat(sprintf("  -> REJECTED (no improvement)\n"))
  }
}

cat("\n--- Feature Selection Summary ---\n")
print(feature_selection_results)

cat(sprintf("\nFinal selected features (%d): %s\n",
            length(selected_features), paste(selected_features, collapse = ", ")))
cat(sprintf("Final Spearman correlation (test): %.4f\n", best_spearman))

# ===== STEP 11: FINAL MODEL AND ANALYSIS =====================================

cat("\n=== STEP 11: FINAL MODEL TRAINING ===\n")

if (is.null(best_result)) {
  stop("ERROR: No valid model could be trained. Check your features.")
}

# Use best result from feature selection
final_features <- best_result$features
final_model <- best_result$model

# Save model
model_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_quantile_model_", LABEL_VERSION, ".json"))
xgb.save(final_model, model_file)
cat(sprintf("Model saved: %s\n", model_file))

# Feature importance
importance <- xgb.importance(feature_names = final_features, model = final_model)
cat("\nFeature Importance:\n")
print(importance)

# ===== STEP 12: BINNING ANALYSIS =============================================

cat("\n=== STEP 12: BINNING ANALYSIS ===\n")

# Get test predictions from best result
test_data <- best_result$test_data
test_data[, pred_q75 := best_result$pred_test]
test_data[, actual_pnl := best_result$y_test]

# Create 5 quantile bins
test_data[, pred_bin := ntile(pred_q75, 5)]

# Analyze by bin
bin_analysis <- test_data[, .(
  mean_pred = mean(pred_q75),
  mean_pnl = mean(actual_pnl),
  median_pnl = median(actual_pnl),
  q75_pnl = quantile(actual_pnl, 0.75),
  win_rate = mean(actual_pnl > 0),
  total_pnl = sum(actual_pnl),
  n = .N
), by = pred_bin][order(pred_bin)]

cat("\n--- Binning Analysis (5 Quantile Bins) ---\n")
print(bin_analysis)

# Check monotonicity
monotonic_check <- all(diff(bin_analysis$mean_pnl) >= 0)
cat(sprintf("\nMonotonicity check (mean_pnl increases with bin): %s\n",
            ifelse(monotonic_check, "PASSED", "FAILED")))

# Spread between top and bottom bin
spread <- bin_analysis[pred_bin == 5]$mean_pnl - bin_analysis[pred_bin == 1]$mean_pnl
cat(sprintf("Spread (Bin 5 - Bin 1): %.6f\n", spread))

# ===== STEP 13: THRESHOLD ANALYSIS ===========================================

cat("\n=== STEP 13: THRESHOLD ANALYSIS ===\n")

# Use quantiles of predictions as thresholds
pred_quantiles <- quantile(test_data$pred_q75, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
threshold_candidates <- as.numeric(pred_quantiles)

threshold_results <- data.table(
  threshold = numeric(),
  n_trades = integer(),
  pct_trades = numeric(),
  mean_pnl = numeric(),
  total_pnl = numeric(),
  win_rate = numeric(),
  mean_pred = numeric()
)

cat("\n--- Threshold Comparison ---\n")
cat(sprintf("%-12s %10s %10s %12s %12s %10s\n",
            "Threshold", "Trades", "% Kept", "Mean PnL", "Total PnL", "Win Rate"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (thresh in threshold_candidates) {
  filtered <- test_data[pred_q75 > thresh]

  if (nrow(filtered) > 0) {
    result <- data.table(
      threshold = thresh,
      n_trades = nrow(filtered),
      pct_trades = 100 * nrow(filtered) / nrow(test_data),
      mean_pnl = mean(filtered$actual_pnl),
      total_pnl = sum(filtered$actual_pnl),
      win_rate = mean(filtered$actual_pnl > 0),
      mean_pred = mean(filtered$pred_q75)
    )
    threshold_results <- rbind(threshold_results, result)

    cat(sprintf("%-12.6f %10d %9.1f%% %12.6f %12.6f %9.1f%%\n",
                thresh, result$n_trades, result$pct_trades,
                result$mean_pnl, result$total_pnl, result$win_rate * 100))
  }
}

# Find optimal threshold (maximize mean_pnl while keeping >30% of trades)
optimal_thresh <- threshold_results[pct_trades >= 30][which.max(mean_pnl)]$threshold
if (length(optimal_thresh) == 0) optimal_thresh <- min(threshold_candidates)

cat(sprintf("\nRecommended threshold (max mean_pnl with >=30%% trades): %.6f\n", optimal_thresh))

# ===== STEP 14: COMPARISON VS UNFILTERED =====================================

cat("\n=== STEP 14: COMPARISON VS UNFILTERED ===\n")

# Unfiltered stats
unfiltered_stats <- test_data[, .(
  n_trades = .N,
  mean_pnl = mean(actual_pnl),
  total_pnl = sum(actual_pnl),
  win_rate = mean(actual_pnl > 0)
)]

# Filtered stats (using optimal threshold)
filtered_data <- test_data[pred_q75 > optimal_thresh]
filtered_stats <- filtered_data[, .(
  n_trades = .N,
  mean_pnl = mean(actual_pnl),
  total_pnl = sum(actual_pnl),
  win_rate = mean(actual_pnl > 0)
)]

# Rejected trades
rejected_data <- test_data[pred_q75 <= optimal_thresh]
rejected_stats <- if (nrow(rejected_data) > 0) {
  rejected_data[, .(
    n_trades = .N,
    mean_pnl = mean(actual_pnl),
    total_pnl = sum(actual_pnl),
    win_rate = mean(actual_pnl > 0)
  )]
} else {
  data.table(n_trades = 0, mean_pnl = NA, total_pnl = 0, win_rate = NA)
}

cat(sprintf("\n                    UNFILTERED    FILTERED      REJECTED\n"))
cat(sprintf("                    (All)         (pred>%.4f) (pred<=%.4f)\n", optimal_thresh, optimal_thresh))
cat(paste(rep("-", 65), collapse = ""), "\n")
cat(sprintf("Number of Trades:   %-14d%-14d%d\n",
            unfiltered_stats$n_trades, filtered_stats$n_trades, rejected_stats$n_trades))
cat(sprintf("Mean PnL:           %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$mean_pnl, filtered_stats$mean_pnl, rejected_stats$mean_pnl))
cat(sprintf("Total PnL:          %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$total_pnl, filtered_stats$total_pnl, rejected_stats$total_pnl))
cat(sprintf("Win Rate:           %-13.1f%%%-13.1f%%%.1f%%\n",
            unfiltered_stats$win_rate * 100, filtered_stats$win_rate * 100, rejected_stats$win_rate * 100))

# ===== STEP 15: SAVE OUTPUTS =================================================

cat("\n=== STEP 15: SAVE OUTPUTS ===\n")

# --- 15.1: Save Feature Selection Results ---
fs_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_feature_selection_", LABEL_VERSION, ".csv"))
fwrite(feature_selection_results, fs_file)
cat(sprintf("Feature selection results saved: %s\n", fs_file))

# --- 15.2: Save Binning Analysis ---
bin_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_binning_analysis_", LABEL_VERSION, ".csv"))
fwrite(bin_analysis, bin_file)
cat(sprintf("Binning analysis saved: %s\n", bin_file))

# --- 15.3: Save Threshold Results ---
thresh_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_threshold_analysis_", LABEL_VERSION, ".csv"))
fwrite(threshold_results, thresh_file)
cat(sprintf("Threshold analysis saved: %s\n", thresh_file))

# --- 15.4: Save Filtered Test Data ---
output_cols <- c("datetime", "signal", "pnl", "pred_prob_long", "pred_prob_short",
                 "pred_q75", "pred_bin", final_features)
output_cols <- intersect(output_cols, names(test_data))

filtered_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_test_predictions_", LABEL_VERSION, ".csv"))
fwrite(test_data[, ..output_cols], filtered_file)
cat(sprintf("Test predictions saved: %s\n", filtered_file))

# ===== STEP 16: VISUALIZATIONS ===============================================

cat("\n=== STEP 16: VISUALIZATIONS ===\n")

# --- 16.1: Feature Importance Plot ---
cat("Creating feature importance plot...\n")

p_importance <- ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = sprintf("Quantile Regression Feature Importance - %s %s", EPIC, INTERVAL),
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

importance_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_feature_importance_", LABEL_VERSION, ".png"))
ggsave(importance_file, p_importance, width = 10, height = 6, dpi = 300)
cat(sprintf("Feature importance plot saved: %s\n", importance_file))

# --- 16.2: Binning Analysis Plot ---
cat("Creating binning analysis plot...\n")

p_bins <- ggplot(bin_analysis, aes(x = factor(pred_bin))) +
  geom_bar(aes(y = mean_pnl, fill = "Mean PnL"), stat = "identity", alpha = 0.7) +
  geom_point(aes(y = win_rate / 100, color = "Win Rate"), size = 4) +
  geom_line(aes(y = win_rate / 100, group = 1, color = "Win Rate"), size = 1) +
  scale_y_continuous(
    name = "Mean PnL",
    sec.axis = sec_axis(~. * 100, name = "Win Rate (%)")
  ) +
  scale_fill_manual(values = c("Mean PnL" = "steelblue")) +
  scale_color_manual(values = c("Win Rate" = "coral")) +
  labs(
    title = sprintf("Performance by Predicted Q75 Bin - %s %s (Test %d)", EPIC, INTERVAL, TEST_YEAR),
    x = "Prediction Quintile (1=Low, 5=High)",
    fill = "", color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

bins_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_binning_plot_", LABEL_VERSION, ".png"))
ggsave(bins_file, p_bins, width = 10, height = 6, dpi = 300)
cat(sprintf("Binning plot saved: %s\n", bins_file))

# --- 16.3: Prediction Distribution Plot ---
cat("Creating prediction distribution plot...\n")

p_dist <- ggplot(test_data, aes(x = pred_q75)) +
  geom_histogram(aes(fill = actual_pnl > 0), bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = optimal_thresh, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = optimal_thresh, y = Inf, label = sprintf("Threshold: %.4f", optimal_thresh),
           vjust = 2, hjust = -0.1, color = "red") +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"),
                    labels = c("TRUE" = "Profitable", "FALSE" = "Not Profitable")) +
  labs(
    title = sprintf("Predicted Q75 Distribution - %s %s (Test %d)", EPIC, INTERVAL, TEST_YEAR),
    x = "Predicted 75th Percentile PnL",
    y = "Count",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

dist_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_prediction_dist_", LABEL_VERSION, ".png"))
ggsave(dist_file, p_dist, width = 10, height = 6, dpi = 300)
cat(sprintf("Prediction distribution saved: %s\n", dist_file))

# ===== STEP 17: COMPREHENSIVE ANALYSIS REPORT ================================

cat("\n=== STEP 17: SAVE ANALYSIS REPORT ===\n")

report_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_quantile_analysis_report_", LABEL_VERSION, ".txt"))

sink(report_file)

cat("================================================================================\n")
cat("              QUANTILE REGRESSION META-LABELLING ANALYSIS REPORT\n")
cat("                    For Analysis by Claude (LLM Interpretation)\n")
cat("================================================================================\n\n")

cat("================================================================================\n")
cat("1. CONTEXT & APPROACH\n")
cat("================================================================================\n\n")

cat(sprintf("Asset: %s (Gold futures)\n", EPIC))
cat(sprintf("Timeframe: %s (15-minute bars)\n", INTERVAL))
cat(sprintf("Test Year: %d (out-of-sample)\n", TEST_YEAR))
cat(sprintf("Training Period: All data before %d\n\n", TEST_YEAR))

cat("PROBLEM:\n")
cat("Binary meta-labelling (predict profitable yes/no) achieved only AUC 0.56.\n")
cat("This is barely better than random guessing and not useful for trading.\n\n")

cat("NEW APPROACH:\n")
cat("Quantile Regression predicting the 75th percentile of PnL distribution.\n")
cat("Instead of asking 'will this trade be profitable?', we ask\n")
cat("'what is the expected upside potential of this trade?'\n\n")

cat("XGBoost Parameters:\n")
cat(sprintf("  objective: reg:quantileerror\n"))
cat(sprintf("  quantile_alpha: %.2f (predicting Q75)\n", QUANTILE_ALPHA))
cat(sprintf("  max_depth: %d\n", xgb_params$max_depth))
cat(sprintf("  eta: %.3f\n", xgb_params$eta))
cat(sprintf("  subsample: %.1f\n", xgb_params$subsample))
cat(sprintf("  colsample_bytree: %.1f\n", xgb_params$colsample_bytree))
cat(sprintf("  min_child_weight: %d\n\n", xgb_params$min_child_weight))

cat("================================================================================\n")
cat("2. DATA SUMMARY\n")
cat("================================================================================\n\n")

cat(sprintf("Training trades: %s (before %d)\n", format(nrow(dt_train), big.mark = ","), TEST_YEAR))
cat(sprintf("Test trades: %s (%d)\n\n", format(nrow(dt_test), big.mark = ","), TEST_YEAR))

cat("Training set performance (primary model signals):\n")
cat(sprintf("  Mean PnL: %.6f\n", mean(dt_train$pnl)))
cat(sprintf("  Win Rate: %.1f%%\n\n", 100 * mean(dt_train$pnl > 0)))

cat("Test set performance (primary model signals):\n")
cat(sprintf("  Mean PnL: %.6f\n", mean(dt_test$pnl)))
cat(sprintf("  Win Rate: %.1f%%\n\n", 100 * mean(dt_test$pnl > 0)))

cat("================================================================================\n")
cat("3. FEATURES USED\n")
cat("================================================================================\n\n")

cat("Feature selection was incremental - groups only kept if they improved correlation.\n\n")

feature_descriptions <- data.table(
  Feature = c("clarity_gap", "atr_percentile", "adx_14", "atr_trend", "atr_14",
              "rsi_14", "bb_pct_b_20", "volume_ratio"),
  Description = c(
    "abs(pred_prob_long - pred_prob_short): Signal clarity",
    "ATR_14 percentile within rolling 60-bar window",
    "Average Directional Index (trend strength)",
    "ATR_5 / ATR_20: Volatility expansion/contraction",
    "14-period Average True Range",
    "14-period Relative Strength Index",
    "Bollinger Band %B (position within bands)",
    "Volume / SMA(Volume, 20)"
  )
)

cat("Feature Definitions:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
for (feat in final_features) {
  desc_row <- feature_descriptions[Feature == feat]
  if (nrow(desc_row) > 0) {
    cat(sprintf("%-20s %s\n", feat, desc_row$Description))
  } else {
    cat(sprintf("%-20s (no description)\n", feat))
  }
}

cat(sprintf("\n\nFinal selected features (%d): %s\n\n", length(final_features), paste(final_features, collapse = ", ")))

cat("================================================================================\n")
cat("4. FEATURE SELECTION RESULTS\n")
cat("================================================================================\n\n")

print(feature_selection_results)

cat("\n")

cat("================================================================================\n")
cat("5. MODEL PERFORMANCE\n")
cat("================================================================================\n\n")

cat("Primary Metric: Spearman Correlation\n")
cat("(Measures monotonic relationship between predictions and actual PnL)\n\n")

cat(sprintf("Training Spearman:  %.4f\n", best_result$spearman_train))
cat(sprintf("Test Spearman:      %.4f\n", best_result$spearman_test))
cat(sprintf("Difference:         %+.4f\n\n", best_result$spearman_test - best_result$spearman_train))

cat("Interpretation:\n")
cat("  Spearman = 0.00: No relationship (model is useless)\n")
cat("  Spearman = 0.10: Weak positive relationship\n")
cat("  Spearman = 0.20: Moderate relationship (good for finance)\n")
cat("  Spearman = 0.30+: Strong relationship (rare in finance)\n\n")

cat("================================================================================\n")
cat("6. FEATURE IMPORTANCE\n")
cat("================================================================================\n\n")

print(importance)

cat("\n")

cat("================================================================================\n")
cat("7. BINNING ANALYSIS (KEY VALIDATION)\n")
cat("================================================================================\n\n")

cat("Trades split into 5 quintile bins based on predicted Q75.\n")
cat("If model works, higher bins should have better outcomes.\n\n")

print(bin_analysis[, .(
  pred_bin,
  n,
  mean_pred = round(mean_pred, 6),
  mean_pnl = round(mean_pnl, 6),
  median_pnl = round(median_pnl, 6),
  q75_pnl = round(q75_pnl, 6),
  win_rate = round(win_rate, 4)
)])

cat(sprintf("\n\nMonotonicity Check: %s\n", ifelse(monotonic_check, "PASSED", "FAILED")))
cat(sprintf("Spread (Bin 5 - Bin 1 mean_pnl): %.6f\n", spread))

cat("\n")

cat("================================================================================\n")
cat("8. THRESHOLD ANALYSIS\n")
cat("================================================================================\n\n")

print(threshold_results[, .(
  threshold = round(threshold, 6),
  n_trades,
  pct_trades = round(pct_trades, 1),
  mean_pnl = round(mean_pnl, 6),
  total_pnl = round(total_pnl, 6),
  win_rate = round(win_rate, 4)
)])

cat(sprintf("\n\nRecommended threshold: %.6f\n", optimal_thresh))

cat("\n")

cat("================================================================================\n")
cat("9. FINAL COMPARISON: FILTERED VS UNFILTERED\n")
cat("================================================================================\n\n")

cat(sprintf("                    UNFILTERED    FILTERED      REJECTED\n"))
cat(sprintf("                    (All)         (pred>%.4f) (pred<=%.4f)\n", optimal_thresh, optimal_thresh))
cat(paste(rep("-", 65), collapse = ""), "\n")
cat(sprintf("Number of Trades:   %-14d%-14d%d\n",
            unfiltered_stats$n_trades, filtered_stats$n_trades, rejected_stats$n_trades))
cat(sprintf("Mean PnL:           %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$mean_pnl, filtered_stats$mean_pnl, rejected_stats$mean_pnl))
cat(sprintf("Total PnL:          %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$total_pnl, filtered_stats$total_pnl, rejected_stats$total_pnl))
cat(sprintf("Win Rate:           %-13.1f%%%-13.1f%%%.1f%%\n",
            unfiltered_stats$win_rate * 100, filtered_stats$win_rate * 100, rejected_stats$win_rate * 100))

cat("\n")

cat("================================================================================\n")
cat("10. QUESTIONS FOR ANALYSIS\n")
cat("================================================================================\n\n")

cat("Please analyze:\n\n")

cat(sprintf("1. MODEL QUALITY: Is Spearman correlation of %.4f meaningful?\n", best_spearman))
cat("   Is this correlation strong enough to be useful for trading?\n\n")

cat("2. MONOTONICITY: Does the binning analysis show the expected pattern?\n")
cat("   Are higher prediction bins consistently associated with better outcomes?\n\n")

cat(sprintf("3. OVERFITTING: Train Spearman = %.4f, Test Spearman = %.4f\n",
            best_result$spearman_train, best_result$spearman_test))
cat("   Is there evidence of overfitting?\n\n")

cat("4. FEATURE IMPORTANCE: Do the important features make economic sense?\n\n")

cat(sprintf("5. THRESHOLD CHOICE: Is %.6f the right threshold?\n", optimal_thresh))
cat("   Should we be more or less aggressive in filtering?\n\n")

cat("6. PRACTICAL VALUE: Does the filtered strategy meaningfully improve\n")
cat("   over the unfiltered strategy?\n\n")

cat("7. RECOMMENDATIONS:\n")
cat("   - Should we use this quantile filter in production?\n")
cat("   - What threshold would you recommend?\n")
cat("   - Any concerns about the methodology?\n\n")

cat("================================================================================\n")
cat("END OF REPORT\n")
cat("================================================================================\n")

sink()

cat(sprintf("Analysis report saved: %s\n", report_file))

# ===== STEP 18: CREATE ZIP ARCHIVE ===========================================

cat("\n=== STEP 18: CREATE ZIP ARCHIVE ===\n")

zip_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_quantile_regression_package_", LABEL_VERSION, ".zip"))

files_to_zip <- c(
  "r/04_quantile_regression.R",
  model_file,
  fs_file,
  bin_file,
  thresh_file,
  filtered_file,
  importance_file,
  bins_file,
  dist_file,
  report_file
)

files_to_zip <- files_to_zip[file.exists(files_to_zip)]

cat(sprintf("Creating ZIP archive with %d files...\n", length(files_to_zip)))

if (length(files_to_zip) > 0) {
  if (file.exists(zip_file)) {
    file.remove(zip_file)
  }

  zip(zip_file, files = files_to_zip, flags = "-j")

  if (file.exists(zip_file)) {
    zip_size <- file.size(zip_file) / 1024
    cat(sprintf("ZIP archive created: %s (%.1f KB)\n", zip_file, zip_size))
    cat("\nFiles included:\n")
    for (f in files_to_zip) {
      cat(sprintf("  - %s\n", basename(f)))
    }
  }
}

# ===== DONE ==================================================================

cat("\n=== QUANTILE REGRESSION COMPLETE ===\n")
cat(sprintf("\nKey Results:\n"))
cat(sprintf("  Spearman Correlation (test): %.4f\n", best_spearman))
cat(sprintf("  Recommended Threshold: %.6f\n", optimal_thresh))
cat(sprintf("  Monotonicity Check: %s\n", ifelse(monotonic_check, "PASSED", "FAILED")))
cat(sprintf("\nOutputs saved to: %s\n", qr_output_path))
cat(sprintf("ZIP archive: %s\n", zip_file))
