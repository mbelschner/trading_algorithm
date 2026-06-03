# ============================================================================
# ML BACKTEST PIPELINE - META-LABELED DATA
# ============================================================================
#
# This script works with the NEW meta-labeling system:
# - primary_signal: Direction from primary signal strategy (1=Long, -1=Short)
# - meta_label: Target for ML model (1=TP hit/profitable, 0=SL/Timeout)
#
# The ML model learns to FILTER trades (meta-labeling approach):
# - Long model: Given a LONG signal, will it be profitable?
# - Short model: Given a SHORT signal, will it be profitable?
#
# This is fundamentally different from the old approach where the model
# predicted the direction. Here, direction is given by the primary signal,
# and the model predicts QUALITY (will it work?).
#
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("ML BACKTEST PIPELINE - META-LABELED DATA\n")
cat("============================================================================\n")
cat(sprintf("Started: %s\n", Sys.time()))

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  TTR,             # Technical indicators
  zoo,             # Time series
  ggplot2,         # Visualization
  progress,        # Progress bars
  tictoc,          # Timing
  parallel,        # Parallelization
  doParallel,      # Parallel backend
  foreach,         # Parallel loops
  xgboost,         # Gradient boosting
  ranger,          # Random Forest
  caret,           # ML framework
  Metrics,         # Evaluation metrics
  pROC             # ROC/AUC calculation
)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results", "meta_labeling")
features_cache_path <- file.path("feature_cache")

# Create output folders
for (path in c(backtest_output_path, features_cache_path)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

CONFIG <- list(
  # Data settings
  epic = "GOLD",
  interval = "MINUTE_15",

  # Train/Test split
  train_years = 2019:2024,
  test_year = 2025,

  # Feature selection
  force_recalculate_features = FALSE,
  top_features_stage1 = 50,  # XGBoost walk-forward
  top_features_final = 15,   # Final Boruta selection

  # Model settings
  use_sample_weights = TRUE,  # Use sample_weight from meta-labeling

  # Walk-forward windows for feature selection
  wf_windows = list(
    list(train_years = 2019:2020, val_year = 2021),
    list(train_years = 2019:2021, val_year = 2022),
    list(train_years = 2019:2022, val_year = 2023),
    list(train_years = 2019:2023, val_year = 2024)
  )
)

cat("\nConfiguration:\n")
cat(sprintf("  Epic: %s, Interval: %s\n", CONFIG$epic, CONFIG$interval))
cat(sprintf("  Train Period: %d-%d\n", min(CONFIG$train_years), max(CONFIG$train_years)))
cat(sprintf("  Test Period: %d\n", CONFIG$test_year))
cat(sprintf("  Use Sample Weights: %s\n", CONFIG$use_sample_weights))

# ===== STEP 1: LOAD META-LABELED DATA ========================================

cat("\n=== STEP 1: LOAD META-LABELED DATA ===\n")
tic()

# Meta-labeled file
labels_file <- file.path(
  labelled_data_path,
  sprintf("%s_%s_meta_labeled.csv", CONFIG$epic, CONFIG$interval)
)

if (!file.exists(labels_file)) {
  stop(sprintf("Meta-labeled data not found: %s\nRun 01_labelling_main_script.R first!", labels_file))
}

dt_labels <- fread(labels_file)
setDT(dt_labels)

# Convert datetime
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))
cat(sprintf("Time range: %s to %s\n",
            min(dt_labels$datetime), max(dt_labels$datetime)))
cat(sprintf("Columns: %s\n", paste(names(dt_labels), collapse = ", ")))

# Show distributions
cat("\nPrimary Signal Distribution:\n")
print(table(dt_labels$primary_signal, useNA = "ifany"))

cat("\nMeta-Label Distribution:\n")
print(table(dt_labels$meta_label, useNA = "ifany"))

# Filter to only rows with valid signals
dt_signals <- dt_labels[!is.na(primary_signal) & primary_signal != 0]
cat(sprintf("\nSignals for modeling: %s rows\n", format(nrow(dt_signals), big.mark = ",")))

toc()

# ===== STEP 2: LOAD PRICE DATA & CALCULATE FEATURES ==========================

cat("\n=== STEP 2: LOAD PRICE DATA & CALCULATE FEATURES ===\n")

# Load raw prices
prices_file <- file.path(price_data_path, paste0(CONFIG$epic, "_", CONFIG$interval, ".csv"))
dt_prices <- fread(prices_file)
setDT(dt_prices)

# Standardize column names
setnames(dt_prices, tolower(names(dt_prices)))
if ("time" %in% names(dt_prices)) {
  setnames(dt_prices, "time", "datetime")
}

if (is.character(dt_prices$datetime)) {
  dt_prices[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Price data loaded: %s rows\n", format(nrow(dt_prices), big.mark = ",")))

# Load pipeline modules
cat("\nLoading pipeline modules...\n")
source("r/02_01_indicator_calculation.R")
source("r/02_02_feature_engineering.R")
source("r/02_03_feature_selection.R")
source("r/02_04_purged_kfold_cv.R")
source("r/02_05_model_training.R")
source("r/02_06_backtest_evaluation.R")
cat("Pipeline modules loaded.\n")

# Check for cached features
features_cache_file <- file.path(
  features_cache_path,
  paste0(CONFIG$epic, "_", CONFIG$interval, "_features_all.csv")
)

if (file.exists(features_cache_file) && !CONFIG$force_recalculate_features) {

  cat("\nLoading cached features...\n")
  dt_features_all <- fread(features_cache_file)
  setDT(dt_features_all)

  if (is.character(dt_features_all$datetime)) {
    dt_features_all[, datetime := as.POSIXct(datetime, tz = "UTC")]
  }

  cat(sprintf("Features loaded: %s rows, %d columns\n",
              format(nrow(dt_features_all), big.mark = ","),
              ncol(dt_features_all)))

} else {

  cat("\nCalculating features (this may take a while)...\n")
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

  dt_features_all <- engineer_all_features(
    dt = dt_indicators,
    lag_periods = c(1, 4, 8, 16),
    rolling_windows = c(4, 12, 24, 48),
    verbose = TRUE
  )

  # Save cache
  fwrite(dt_features_all, features_cache_file)
  cat(sprintf("Features cached: %s\n", features_cache_file))

  toc()
}

# ===== STEP 3: MERGE FEATURES WITH LABELS ====================================

cat("\n=== STEP 3: MERGE FEATURES WITH LABELS ===\n")

# Label columns to keep
label_cols <- c("datetime", "primary_signal", "meta_label", "sample_weight",
                "barrier_touched", "bars_to_exit", "realized_return",
                "realized_return_adj", "tp_distance", "sl_distance",
                "n_concurrent", "session", "hour")
label_cols <- intersect(label_cols, names(dt_signals))

# Merge
dt_merged <- merge(
  dt_features_all,
  dt_signals[, ..label_cols],
  by = "datetime",
  all = FALSE
)

cat(sprintf("Merged dataset: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# Add year column
dt_merged[, year := as.integer(format(datetime, "%Y"))]

# ===== STEP 4: TRAIN/TEST SPLIT ==============================================

cat("\n=== STEP 4: TRAIN/TEST SPLIT ===\n")

dt_train <- dt_merged[year %in% CONFIG$train_years]
dt_test <- dt_merged[year == CONFIG$test_year]

cat(sprintf("Train set (%d-%d): %s rows\n",
            min(CONFIG$train_years), max(CONFIG$train_years),
            format(nrow(dt_train), big.mark = ",")))
cat(sprintf("Test set (%d): %s rows\n",
            CONFIG$test_year,
            format(nrow(dt_test), big.mark = ",")))

cat("\nTrain meta-label distribution:\n")
print(table(dt_train$meta_label))

cat("\nTest meta-label distribution:\n")
print(table(dt_test$meta_label))

# ===== STEP 5: SPLIT INTO LONG AND SHORT DATASETS ============================

cat("\n=== STEP 5: CREATE LONG AND SHORT DATASETS ===\n")

# ========== LONG DATASET ==========
cat("\n--- LONG DATASET ---\n")
cat("Samples where primary_signal = 1 (Long)\n")
cat("Target: meta_label (1=TP hit, 0=SL/Timeout)\n\n")

dt_train_long <- dt_train[primary_signal == 1]
dt_test_long <- dt_test[primary_signal == 1]

cat(sprintf("Train Long: %s rows\n", format(nrow(dt_train_long), big.mark = ",")))
cat("  Meta-label distribution:\n")
print(table(dt_train_long$meta_label))

cat(sprintf("\nTest Long: %s rows\n", format(nrow(dt_test_long), big.mark = ",")))
cat("  Meta-label distribution:\n")
print(table(dt_test_long$meta_label))

# ========== SHORT DATASET ==========
cat("\n--- SHORT DATASET ---\n")
cat("Samples where primary_signal = -1 (Short)\n")
cat("Target: meta_label (1=TP hit, 0=SL/Timeout)\n\n")

dt_train_short <- dt_train[primary_signal == -1]
dt_test_short <- dt_test[primary_signal == -1]

cat(sprintf("Train Short: %s rows\n", format(nrow(dt_train_short), big.mark = ",")))
cat("  Meta-label distribution:\n")
print(table(dt_train_short$meta_label))

cat(sprintf("\nTest Short: %s rows\n", format(nrow(dt_test_short), big.mark = ",")))
cat("  Meta-label distribution:\n")
print(table(dt_test_short$meta_label))

# ===== STEP 6: DEFINE FEATURE COLUMNS ========================================

cat("\n=== STEP 6: DEFINE FEATURE COLUMNS ===\n")

# Metadata columns (exclude from features)
meta_cols <- c("datetime", "year", "primary_signal", "meta_label", "sample_weight",
               "barrier_touched", "bars_to_exit", "realized_return", "realized_return_adj",
               "tp_distance", "sl_distance", "n_concurrent", "session", "hour",
               "open", "high", "low", "close", "volume", "in_session")

# Additional exclusions (lookahead bias, reserved for meta-labeling)
excluded_features <- c(
  "log_return", "realized_return", "realized_return_adj"
)

# Get all feature columns
all_feature_cols <- setdiff(names(dt_train_long), c(meta_cols, excluded_features))

# Remove any columns with NA patterns that might indicate issues
na_cols <- sapply(dt_train_long[, ..all_feature_cols], function(x) mean(is.na(x)) > 0.5)
if (any(na_cols)) {
  cat(sprintf("Removing %d columns with >50%% NA values\n", sum(na_cols)))
  all_feature_cols <- all_feature_cols[!na_cols]
}

cat(sprintf("Total feature columns: %d\n", length(all_feature_cols)))

# ============================================================================
# LONG MODEL PIPELINE
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("LONG MODEL PIPELINE (Meta-Label)\n")
cat("Objective: Predict if LONG trades will hit TP (profitable)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7a: FEATURE SELECTION (LONG) =====================================

cat("\n=== STEP 7a: FEATURE SELECTION (LONG) ===\n")
tic()

# Sample weights for class imbalance
if (CONFIG$use_sample_weights && "sample_weight" %in% names(dt_train_long)) {
  sample_weights_long <- dt_train_long$sample_weight
  cat("Using sample weights from meta-labeling\n")
} else {
  sample_weights_long <- NULL
}

# XGBoost feature importance
cat("\nRunning XGBoost feature importance...\n")

# Prepare data
X_train_long <- as.matrix(dt_train_long[, ..all_feature_cols])
y_train_long <- dt_train_long$meta_label

# Handle NA values
X_train_long[is.na(X_train_long)] <- 0

# XGBoost DMatrix
dtrain_long <- xgb.DMatrix(
  data = X_train_long,
  label = y_train_long,
  weight = sample_weights_long
)

# Calculate scale_pos_weight for class imbalance
n_pos <- sum(y_train_long == 1)
n_neg <- sum(y_train_long == 0)
scale_pos_weight_long <- n_neg / n_pos
cat(sprintf("Class imbalance: %d positive, %d negative, scale_pos_weight = %.2f\n",
            n_pos, n_neg, scale_pos_weight_long))

# XGBoost params
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight_long
)

# Train model for feature importance
xgb_model_long <- xgb.train(
  params = xgb_params,
  data = dtrain_long,
  nrounds = 100,
  verbose = 0
)

# Get feature importance
importance_long <- xgb.importance(
  feature_names = all_feature_cols,
  model = xgb_model_long
)

# Select top features
top_features_long <- head(importance_long$Feature, CONFIG$top_features_final)
cat(sprintf("\nTop %d features for LONG model:\n", length(top_features_long)))
print(head(importance_long, CONFIG$top_features_final))

toc()

# ===== STEP 8a: TRAIN FINAL MODEL (LONG) =====================================

cat("\n=== STEP 8a: TRAIN FINAL LONG MODEL ===\n")
tic()

# Prepare final training data
X_train_long_final <- as.matrix(dt_train_long[, ..top_features_long])
X_train_long_final[is.na(X_train_long_final)] <- 0

dtrain_long_final <- xgb.DMatrix(
  data = X_train_long_final,
  label = y_train_long,
  weight = sample_weights_long
)

# Cross-validation
cat("\nRunning 5-fold cross-validation...\n")
cv_long <- xgb.cv(
  params = xgb_params,
  data = dtrain_long_final,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 30,
  verbose = 0
)

best_nrounds_long <- cv_long$best_iteration
best_auc_long <- max(cv_long$evaluation_log$test_auc_mean)
cat(sprintf("Best iteration: %d, CV AUC: %.4f\n", best_nrounds_long, best_auc_long))

# Train final model
model_long <- xgb.train(
  params = xgb_params,
  data = dtrain_long_final,
  nrounds = best_nrounds_long,
  verbose = 0
)

toc()

# ===== STEP 9a: EVALUATE LONG MODEL ==========================================

cat("\n=== STEP 9a: EVALUATE LONG MODEL ===\n")

# Prepare test data
X_test_long <- as.matrix(dt_test_long[, ..top_features_long])
X_test_long[is.na(X_test_long)] <- 0

# Predictions
pred_train_long <- predict(model_long, dtrain_long_final)
pred_test_long <- predict(model_long, X_test_long)

# Calculate AUC
auc_train_long <- auc(roc(y_train_long, pred_train_long, quiet = TRUE))
auc_test_long <- auc(roc(dt_test_long$meta_label, pred_test_long, quiet = TRUE))

cat(sprintf("\nLONG Model Performance:\n"))
cat(sprintf("  Train AUC: %.4f\n", auc_train_long))
cat(sprintf("  Test AUC: %.4f\n", auc_test_long))

# Confusion matrix at threshold 0.5
pred_class_train_long <- ifelse(pred_train_long > 0.5, 1, 0)
pred_class_test_long <- ifelse(pred_test_long > 0.5, 1, 0)

cat("\nTrain Confusion Matrix:\n")
print(table(Predicted = pred_class_train_long, Actual = y_train_long))

cat("\nTest Confusion Matrix:\n")
print(table(Predicted = pred_class_test_long, Actual = dt_test_long$meta_label))

# Calculate precision, recall, F1
if (sum(pred_class_test_long == 1) > 0) {
  precision_long <- sum(pred_class_test_long == 1 & dt_test_long$meta_label == 1) /
    sum(pred_class_test_long == 1)
  recall_long <- sum(pred_class_test_long == 1 & dt_test_long$meta_label == 1) /
    sum(dt_test_long$meta_label == 1)
  f1_long <- 2 * precision_long * recall_long / (precision_long + recall_long)

  cat(sprintf("\nTest Metrics (threshold=0.5):\n"))
  cat(sprintf("  Precision: %.4f\n", precision_long))
  cat(sprintf("  Recall: %.4f\n", recall_long))
  cat(sprintf("  F1 Score: %.4f\n", f1_long))
}

# ============================================================================
# SHORT MODEL PIPELINE
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("SHORT MODEL PIPELINE (Meta-Label)\n")
cat("Objective: Predict if SHORT trades will hit TP (profitable)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7b: FEATURE SELECTION (SHORT) ====================================

cat("\n=== STEP 7b: FEATURE SELECTION (SHORT) ===\n")
tic()

# Sample weights
if (CONFIG$use_sample_weights && "sample_weight" %in% names(dt_train_short)) {
  sample_weights_short <- dt_train_short$sample_weight
} else {
  sample_weights_short <- NULL
}

# Prepare data
X_train_short <- as.matrix(dt_train_short[, ..all_feature_cols])
y_train_short <- dt_train_short$meta_label

X_train_short[is.na(X_train_short)] <- 0

dtrain_short <- xgb.DMatrix(
  data = X_train_short,
  label = y_train_short,
  weight = sample_weights_short
)

# Scale pos weight
n_pos_short <- sum(y_train_short == 1)
n_neg_short <- sum(y_train_short == 0)
scale_pos_weight_short <- n_neg_short / n_pos_short
cat(sprintf("Class imbalance: %d positive, %d negative, scale_pos_weight = %.2f\n",
            n_pos_short, n_neg_short, scale_pos_weight_short))

xgb_params_short <- xgb_params
xgb_params_short$scale_pos_weight <- scale_pos_weight_short

# Train for feature importance
xgb_model_short <- xgb.train(
  params = xgb_params_short,
  data = dtrain_short,
  nrounds = 100,
  verbose = 0
)

# Feature importance
importance_short <- xgb.importance(
  feature_names = all_feature_cols,
  model = xgb_model_short
)

top_features_short <- head(importance_short$Feature, CONFIG$top_features_final)
cat(sprintf("\nTop %d features for SHORT model:\n", length(top_features_short)))
print(head(importance_short, CONFIG$top_features_final))

toc()

# ===== STEP 8b: TRAIN FINAL MODEL (SHORT) ====================================

cat("\n=== STEP 8b: TRAIN FINAL SHORT MODEL ===\n")
tic()

X_train_short_final <- as.matrix(dt_train_short[, ..top_features_short])
X_train_short_final[is.na(X_train_short_final)] <- 0

dtrain_short_final <- xgb.DMatrix(
  data = X_train_short_final,
  label = y_train_short,
  weight = sample_weights_short
)

# Cross-validation
cat("\nRunning 5-fold cross-validation...\n")
cv_short <- xgb.cv(
  params = xgb_params_short,
  data = dtrain_short_final,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 30,
  verbose = 0
)

best_nrounds_short <- cv_short$best_iteration
best_auc_short <- max(cv_short$evaluation_log$test_auc_mean)
cat(sprintf("Best iteration: %d, CV AUC: %.4f\n", best_nrounds_short, best_auc_short))

# Train final model
model_short <- xgb.train(
  params = xgb_params_short,
  data = dtrain_short_final,
  nrounds = best_nrounds_short,
  verbose = 0
)

toc()

# ===== STEP 9b: EVALUATE SHORT MODEL =========================================

cat("\n=== STEP 9b: EVALUATE SHORT MODEL ===\n")

X_test_short <- as.matrix(dt_test_short[, ..top_features_short])
X_test_short[is.na(X_test_short)] <- 0

pred_train_short <- predict(model_short, dtrain_short_final)
pred_test_short <- predict(model_short, X_test_short)

auc_train_short <- auc(roc(y_train_short, pred_train_short, quiet = TRUE))
auc_test_short <- auc(roc(dt_test_short$meta_label, pred_test_short, quiet = TRUE))

cat(sprintf("\nSHORT Model Performance:\n"))
cat(sprintf("  Train AUC: %.4f\n", auc_train_short))
cat(sprintf("  Test AUC: %.4f\n", auc_test_short))

pred_class_train_short <- ifelse(pred_train_short > 0.5, 1, 0)
pred_class_test_short <- ifelse(pred_test_short > 0.5, 1, 0)

cat("\nTrain Confusion Matrix:\n")
print(table(Predicted = pred_class_train_short, Actual = y_train_short))

cat("\nTest Confusion Matrix:\n")
print(table(Predicted = pred_class_test_short, Actual = dt_test_short$meta_label))

if (sum(pred_class_test_short == 1) > 0) {
  precision_short <- sum(pred_class_test_short == 1 & dt_test_short$meta_label == 1) /
    sum(pred_class_test_short == 1)
  recall_short <- sum(pred_class_test_short == 1 & dt_test_short$meta_label == 1) /
    sum(dt_test_short$meta_label == 1)
  f1_short <- 2 * precision_short * recall_short / (precision_short + recall_short)

  cat(sprintf("\nTest Metrics (threshold=0.5):\n"))
  cat(sprintf("  Precision: %.4f\n", precision_short))
  cat(sprintf("  Recall: %.4f\n", recall_short))
  cat(sprintf("  F1 Score: %.4f\n", f1_short))
}

# ============================================================================
# STEP 10: PnL SIMULATION
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("PnL SIMULATION (TEST SET %d)\n", CONFIG$test_year)
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Add predictions to test data
dt_test_long[, pred_prob := pred_test_long]
dt_test_short[, pred_prob := pred_test_short]

# Combine for simulation
dt_test_all <- rbindlist(list(
  dt_test_long[, .(datetime, primary_signal, meta_label, realized_return_adj,
                   sample_weight, pred_prob, barrier_touched)],
  dt_test_short[, .(datetime, primary_signal, meta_label, realized_return_adj,
                    sample_weight, pred_prob, barrier_touched)]
))
setorder(dt_test_all, datetime)

# Simulate different threshold strategies
thresholds <- c(0.3, 0.4, 0.5, 0.6, 0.7)

cat("\n=== PnL SIMULATION BY THRESHOLD ===\n")
cat("Comparing: Take all signals vs ML-filtered signals\n\n")

results <- data.table()

for (thresh in thresholds) {
  # ML filtered: only take trades where pred_prob > threshold
  dt_filtered <- dt_test_all[pred_prob > thresh]

  if (nrow(dt_filtered) > 0) {
    # Calculate metrics
    n_trades <- nrow(dt_filtered)
    win_rate <- mean(dt_filtered$meta_label == 1) * 100
    total_return <- sum(dt_filtered$realized_return_adj, na.rm = TRUE) * 100
    avg_return <- mean(dt_filtered$realized_return_adj, na.rm = TRUE) * 100

    results <- rbind(results, data.table(
      threshold = thresh,
      n_trades = n_trades,
      win_rate = win_rate,
      total_return_pct = total_return,
      avg_return_pct = avg_return
    ))
  }
}

# Add baseline (all trades)
baseline_trades <- nrow(dt_test_all)
baseline_win_rate <- mean(dt_test_all$meta_label == 1, na.rm = TRUE) * 100
baseline_total <- sum(dt_test_all$realized_return_adj, na.rm = TRUE) * 100
baseline_avg <- mean(dt_test_all$realized_return_adj, na.rm = TRUE) * 100

results <- rbind(
  data.table(
    threshold = 0.0,
    n_trades = baseline_trades,
    win_rate = baseline_win_rate,
    total_return_pct = baseline_total,
    avg_return_pct = baseline_avg
  ),
  results
)

cat("Results by Threshold:\n")
print(results)

# ============================================================================
# STEP 11: SAVE MODELS AND RESULTS
# ============================================================================

cat("\n=== STEP 11: SAVE MODELS AND RESULTS ===\n")

# Save models
model_path <- file.path(backtest_output_path, "models")
if (!dir.exists(model_path)) dir.create(model_path, recursive = TRUE)

xgb.save(model_long, file.path(model_path, "model_long_meta.xgb"))
xgb.save(model_short, file.path(model_path, "model_short_meta.xgb"))

# Save feature lists
fwrite(data.table(feature = top_features_long),
       file.path(model_path, "features_long_meta.csv"))
fwrite(data.table(feature = top_features_short),
       file.path(model_path, "features_short_meta.csv"))

# Save results
fwrite(results, file.path(backtest_output_path, "pnl_simulation_results.csv"))

cat(sprintf("Models saved to: %s\n", model_path))

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat("============================================================================\n")
cat("BACKTEST COMPLETE - SUMMARY\n")
cat("============================================================================\n")

cat("\nLONG Model:\n")
cat(sprintf("  Features: %d\n", length(top_features_long)))
cat(sprintf("  Train AUC: %.4f\n", auc_train_long))
cat(sprintf("  Test AUC: %.4f\n", auc_test_long))

cat("\nSHORT Model:\n")
cat(sprintf("  Features: %d\n", length(top_features_short)))
cat(sprintf("  Train AUC: %.4f\n", auc_train_short))
cat(sprintf("  Test AUC: %.4f\n", auc_test_short))

cat("\nBest Threshold Strategy (by total return):\n")
best_result <- results[which.max(total_return_pct)]
print(best_result)

cat(sprintf("\nFinished: %s\n", Sys.time()))
cat("============================================================================\n")
