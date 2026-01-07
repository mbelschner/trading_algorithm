# ============================================================================
# ML BACKTEST PIPELINE - LONG/SHORT SPLIT (Version 2)
# ============================================================================
#
# KEY CHANGES:
# 1. Features calculated on ALL price data (not just labeled samples)
# 2. Enhanced Neutral labels as default (configurable for RAW/STANDARD/UNFILTERED)
# 3. Long model: Only label=1 (long) vs label=0 (neutral), SHORT LABELS FILTERED OUT
# 4. Short model: Only label=-1 (short) vs label=0 (neutral), LONG LABELS FILTERED OUT
# 5. Two-Stage Feature Selection:
#    - Stage 1: Walk-Forward XGBoost (4 windows) → Top 50 stable features
#    - Stage 2: Single Boruta run on full training period (2019-2024) → Top 15 final features
# 6. 2025 as out-of-sample test set
# 7. Confusion Matrix + Metrics for BOTH train and test sets
# 8. Excluded features: Returns, ATR, Hour/Session (reserved for meta-labeling)
#
# ============================================================================

cat("\n=== START ML BACKTEST PIPELINE (LONG/SHORT SPLIT V2) ===\n")

# ===== SETUP =================================================================

rm(list=ls())
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
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")

# Create output folders
if (!dir.exists(backtest_output_path)) {
  dir.create(backtest_output_path, recursive = TRUE)
}
if (!dir.exists(features_cache_path)) {
  dir.create(features_cache_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"

# Label version selection (configurable)
LABEL_VERSION <- "enhanced_neutral"  # Options: "enhanced_neutral", "raw", "standard", "unfiltered"

# Feature caching
FORCE_RECALCULATE_FEATURES <- FALSE  # Set to TRUE if feature selection logic changed

# Train/Test split years
TRAIN_YEARS <- 2019:2024  # Feature selection and training
TEST_YEAR <- 2025          # Out-of-sample test

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Train Period: %d-%d\n", min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Test Period: %d\n", TEST_YEAR))

# ===== STEP 1: LOAD RAW PRICE DATA ===========================================

cat("\n=== STEP 1: LOAD RAW PRICE DATA (ALL DATA) ===\n")

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
cat(sprintf("  Columns: %s\n", paste(names(dt_prices), collapse = ", ")))

# ===== STEP 2: LOAD LABELS ===================================================

cat("\n=== STEP 2: LOAD LABELS ===\n")

# Select label file based on configuration
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

cat(sprintf("✓ Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))
cat(sprintf("  Label columns: %s\n", paste(names(dt_labels), collapse = ", ")))

cat("\n  Label distribution:\n")
print(table(dt_labels$label))

# ===== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA =========================

cat("\n=== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA ===\n")

# Load pipeline modules
cat("\nLoading pipeline modules...\n")
source("r/02_01_indicator_calculation.R")
cat("✓ Indicator Calculation loaded\n")
source("r/02_02_feature_engineering.R")
cat("✓ Feature Engineering loaded\n")
source("r/02_03_feature_selection.R")
cat("✓ Feature Selection loaded\n")
source("r/02_04_purged_kfold_cv.R")
cat("✓ Purged K-Fold CV loaded\n")
source("r/02_05_model_training.R")
cat("✓ Model Training loaded\n")
source("r/02_06_backtest_evaluation.R")
cat("✓ Backtest Evaluation loaded\n")

# Check for cached features
features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (file.exists(features_cache_file) && !FORCE_RECALCULATE_FEATURES) {

  cat("\n=== LOADING CACHED FEATURES ===\n")
  cat(sprintf("Loading from cache: %s\n", features_cache_file))

  dt_features_all <- fread(features_cache_file)
  setDT(dt_features_all)

  cat(sprintf("✓ Features loaded: %s rows, %d columns\n",
              format(nrow(dt_features_all), big.mark = ","),
              ncol(dt_features_all)))

} else {

  cat("\n=== CALCULATING FEATURES (ON ALL PRICE DATA) ===\n")

  # Calculate indicators
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

  # Remove NA rows (from lags/rolling windows)
  n_before_na <- nrow(dt_features_all)
  dt_features_all <- na.omit(dt_features_all)
  n_after_na <- nrow(dt_features_all)

  cat(sprintf("Rows after NA removal: %s (-%s)\n",
              format(n_after_na, big.mark = ","),
              format(n_before_na - n_after_na, big.mark = ",")))

  # Save to cache
  cat("\n=== SAVING FEATURES TO CACHE ===\n")
  fwrite(dt_features_all, features_cache_file)
  cat(sprintf("✓ Features saved: %s\n", features_cache_file))
}

# ===== STEP 4: MERGE LABELS WITH FEATURES ====================================

cat("\n=== STEP 4: MERGE LABELS WITH FEATURES ===\n")

cat(sprintf("Features before merge: %s rows\n",
            format(nrow(dt_features_all), big.mark = ",")))
cat(sprintf("Labels before merge: %s rows\n",
            format(nrow(dt_labels), big.mark = ",")))

# Select label columns to merge
label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                          "bars_to_exit", "realized_return", "n_concurrent",
                          "realized_return_adj", "log_return")

# Check which columns are available
available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))
cat(sprintf("Available label columns: %s\n",
            paste(available_label_cols, collapse = ", ")))

# Merge (inner join - only keep rows with both features AND labels)
dt_merged <- merge(
  dt_features_all,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE  # Inner join
)

cat(sprintf("✓ Merged dataset: %s rows\n",
            format(nrow(dt_merged), big.mark = ",")))

# Verify label column exists
if (!"label" %in% names(dt_merged)) {
  stop("ERROR: Labels could not be merged!")
}

cat("\nLabel distribution after merge:\n")
print(table(dt_merged$label))

# Add year column for train/test split
dt_merged[, year := as.integer(format(datetime, "%Y"))]

# ===== STEP 5: SPLIT INTO TRAIN (2019-2024) AND TEST (2025) =================

cat("\n=== STEP 5: TRAIN/TEST SPLIT ===\n")

dt_train <- dt_merged[year %in% TRAIN_YEARS]
dt_test <- dt_merged[year == TEST_YEAR]

cat(sprintf("Train set (%d-%d): %s rows\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS),
            format(nrow(dt_train), big.mark = ",")))
cat(sprintf("Test set (%d): %s rows\n",
            TEST_YEAR,
            format(nrow(dt_test), big.mark = ",")))

cat("\nTrain label distribution:\n")
print(table(dt_train$label))

cat("\nTest label distribution:\n")
print(table(dt_test$label))

# ===== STEP 6: CREATE LONG AND SHORT DATASETS ================================

cat("\n=== STEP 6: CREATE LONG AND SHORT DATASETS ===\n")

# ========== LONG DATASET ==========
# Long=1 vs Neutral=0, FILTER OUT SHORT=-1
cat("\n--- LONG DATASET ---\n")

dt_train_long <- dt_train[label != -1]  # Remove short labels
dt_test_long <- dt_test[label != -1]    # Remove short labels

# Binary labels: Long=1, Neutral=0
dt_train_long[, label_binary := fifelse(label == 1, 1, 0)]
dt_test_long[, label_binary := fifelse(label == 1, 1, 0)]

cat(sprintf("Train Long: %s rows\n", format(nrow(dt_train_long), big.mark = ",")))
cat("  Distribution:\n")
print(table(dt_train_long$label_binary))

cat(sprintf("\nTest Long: %s rows\n", format(nrow(dt_test_long), big.mark = ",")))
cat("  Distribution:\n")
print(table(dt_test_long$label_binary))

# ========== SHORT DATASET ==========
# Short=1 vs Neutral=0, FILTER OUT LONG=1
cat("\n--- SHORT DATASET ---\n")

dt_train_short <- dt_train[label != 1]   # Remove long labels
dt_test_short <- dt_test[label != 1]     # Remove long labels

# Binary labels: Short=1, Neutral=0
dt_train_short[, label_binary := fifelse(label == -1, 1, 0)]
dt_test_short[, label_binary := fifelse(label == -1, 1, 0)]

cat(sprintf("Train Short: %s rows\n", format(nrow(dt_train_short), big.mark = ",")))
cat("  Distribution:\n")
print(table(dt_train_short$label_binary))

cat(sprintf("\nTest Short: %s rows\n", format(nrow(dt_test_short), big.mark = ",")))
cat("  Distribution:\n")
print(table(dt_test_short$label_binary))

cat("\n✓ Long/Short datasets created\n")
cat("  Long: label=1 (long) vs label=0 (neutral), shorts filtered out\n")
cat("  Short: label=1 (short) vs label=0 (neutral), longs filtered out\n")

# ============================================================================
# LONG MODEL PIPELINE
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("LONG MODEL PIPELINE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7a: WALK-FORWARD FEATURE SELECTION (LONG) ========================

cat("\n=== STEP 7a: TWO-STAGE FEATURE SELECTION (LONG MODEL) ===\n")
cat("Strategy: XGBoost Walk-Forward → Boruta Single Run\n")
cat("  Stage 1: XGBoost on 4 expanding windows → 50 stable features\n")
cat("  Stage 2: Boruta on full training period (2019-2024) → 15 final features\n\n")

# Define walk-forward windows
wf_windows <- list(
  list(train_years = 2019:2020, val_year = 2021),
  list(train_years = 2019:2021, val_year = 2022),
  list(train_years = 2019:2022, val_year = 2023),
  list(train_years = 2019:2023, val_year = 2024)
)

# Get all feature columns (exclude metadata and features with lookahead bias)
meta_cols <- c("datetime", "year", "label", "label_binary", "sample_weight",
               "barrier_touched", "bars_to_exit", "realized_return",
               "n_concurrent", "realized_return_adj", "log_return",
               "open", "high", "low", "close", "volume")

# Additional exclusions: return features (lookahead) and session/hour features (for meta-labeling)
excluded_features <- c(
  # Return features (lookahead bias)
  "log_return", "realized_return", "realized_return_adj",
   # Session/hour features (reserved for meta-labeling)
  "hour", "hour_sin", "hour_cos", "hour_open", "hour_high", "hour_low",
  "hour_close", "hour_volume", "hour_close_mean", "hour_close_sd",
  "session_london", "session_ny", "session_asia", "session_overlap"
)

all_feature_cols <- setdiff(names(dt_train_long), c(meta_cols, excluded_features))
cat(sprintf("Total features available: %d\n", length(all_feature_cols)))
cat(sprintf("Excluded features: %s\n", paste(intersect(names(dt_train_long), excluded_features), collapse = ", ")))

# --- STAGE 1: XGBoost Feature Selection (50 features) ---

cat("\n--- STAGE 1: XGBoost Walk-Forward Feature Selection ---\n")
cat("Target: 50 stable features across all windows\n\n")

xgb_feature_importance_list <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d → Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  # Split data for this window
  dt_wf_train <- dt_train_long[year %in% window$train_years]
  dt_wf_val <- dt_train_long[year == window$val_year]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))
  cat(sprintf("  Val:   %s rows\n", format(nrow(dt_wf_val), big.mark = ",")))

  # Run XGBoost feature selection
  tic()
  fs_result <- select_important_features(
    dt = dt_wf_train,
    target_col = "label_binary",
    weight_col = "sample_weight",
    feature_cols = all_feature_cols,  # Pass allowed features explicitly
    method = "xgboost",
    n_top_features = 50,
    cv_folds = 3,
    verbose = FALSE
  )
  toc()

  # Store feature importance
  xgb_feature_importance_list[[i]] <- fs_result$importance
  cat(sprintf("  ✓ Top 50 features selected\n"))
}

# Find features that appear in ALL windows (stable features)
cat("\n--- Identifying Stable Features (appear in ALL 4 windows) ---\n")

# Get top 50 features from each window
top_features_per_window <- lapply(xgb_feature_importance_list, function(imp) {
  head(imp$feature, 50)
})

# Count how often each feature appears
feature_counts <- table(unlist(top_features_per_window))
stable_features_xgb <- names(feature_counts[feature_counts == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_xgb)))

if (length(stable_features_xgb) < 50) {
  cat(sprintf("WARNING: Only %d stable features found. Taking top 50 by average rank...\n",
              length(stable_features_xgb)))

  # Fallback: Take top 50 by average rank across windows
  all_features <- unique(unlist(top_features_per_window))
  avg_ranks <- sapply(all_features, function(f) {
    ranks <- sapply(xgb_feature_importance_list, function(imp) {
      idx <- which(imp$feature == f)
      if (length(idx) == 0) return(999)  # Not in this window
      return(idx)
    })
    mean(ranks)
  })

  stable_features_xgb <- names(sort(avg_ranks)[1:50])
}

cat(sprintf("✓ Stage 1 complete: %d stable features selected\n", length(stable_features_xgb)))

# --- STAGE 2: Boruta Feature Selection (ONCE on full training period) ---

cat("\n--- STAGE 2: Boruta Feature Selection (Single Run) ---\n")
cat("Strategy: Run Boruta ONCE on full training period (2019-2024)\n")
cat(sprintf("Input: %d stable features from XGBoost stage\n", length(stable_features_xgb)))
cat("Target: 15 final features\n\n")

# Create reduced dataset with only XGBoost-selected features
required_cols_boruta <- c("datetime", "year", "label_binary", "sample_weight")
dt_train_long_reduced <- dt_train_long[, c(required_cols_boruta, stable_features_xgb), with = FALSE]

cat(sprintf("Running Boruta on full training set (%d-%d)...\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Training samples: %s\n", format(nrow(dt_train_long_reduced), big.mark = ",")))
cat("  This will take a few minutes...\n\n")

# Run Boruta ONCE on full training data (2019-2024)
tic()
fs_result <- select_important_features(
  dt = dt_train_long_reduced,
  target_col = "label_binary",
  weight_col = "sample_weight",
  feature_cols = stable_features_xgb,  # Pass XGBoost-selected features
  method = "boruta",
  n_top_features = 15,
  cv_folds = 1,
  verbose = TRUE  # Show progress for this long-running operation
)
toc()

# Extract top 15 features
stable_features_long <- fs_result$top_features

cat(sprintf("\n✓ Stage 2 complete: %d final features selected for LONG model\n",
            length(stable_features_long)))

# === SAFETY CHECK: Verify no excluded features made it through ===
cat("\n--- SAFETY CHECK: Verifying no excluded features ---\n")

forbidden_features_found <- intersect(stable_features_long, excluded_features)
if (length(forbidden_features_found) > 0) {
  cat("WARNING: Found forbidden features in final selection!\n")
  cat(sprintf("  Removing: %s\n", paste(forbidden_features_found, collapse = ", ")))
  stable_features_long <- setdiff(stable_features_long, excluded_features)
  cat(sprintf("  Final count after removal: %d features\n", length(stable_features_long)))
} else {
  cat("✓ No forbidden features detected\n")
}

cat("\n=== FINAL FEATURES FOR LONG MODEL ===\n")
cat(paste(stable_features_long, collapse = "\n"))
cat("\n")

# ===== STEP 8a: TRAIN FINAL LONG MODEL ======================================

cat("\n=== STEP 8a: TRAIN FINAL LONG MODEL ===\n")

# Prepare final datasets with selected features
final_cols <- c("datetime", "year", "label_binary", "sample_weight",
                stable_features_long)

dt_train_long_final <- dt_train_long[, ..final_cols]
dt_test_long_final <- dt_test_long[, ..final_cols]

cat(sprintf("Train set: %s rows, %d features\n",
            format(nrow(dt_train_long_final), big.mark = ","),
            length(stable_features_long)))
cat(sprintf("Test set:  %s rows, %d features\n",
            format(nrow(dt_test_long_final), big.mark = ","),
            length(stable_features_long)))

# Prepare matrices
X_train <- as.matrix(dt_train_long_final[, ..stable_features_long])
y_train <- dt_train_long_final$label_binary
w_train <- dt_train_long_final$sample_weight

X_test <- as.matrix(dt_test_long_final[, ..stable_features_long])
y_test <- dt_test_long_final$label_binary

# Create DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# XGBoost parameters (can be tuned later)
params_long <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 3
)

cat("\nTraining XGBoost model...\n")
tic()
model_long <- xgb.train(
  params = params_long,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)
toc()

cat("✓ Model trained\n")

# ===== STEP 9a: EVALUATE LONG MODEL ==========================================

cat("\n=== STEP 9a: EVALUATE LONG MODEL ===\n")

# --- Helper function for confusion matrix and metrics ---
evaluate_binary_model <- function(y_true, y_pred_prob, threshold = 0.5, set_name = "Unknown") {

  y_pred_class <- ifelse(y_pred_prob > threshold, 1, 0)

  cat(sprintf("\n--- %s SET PERFORMANCE ---\n", set_name))

  # Confusion Matrix
  conf_matrix <- table(Predicted = y_pred_class, Actual = y_true)
  print(conf_matrix)

  # Calculate metrics
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]

  accuracy <- (TP + TN) / sum(conf_matrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  specificity <- TN / (TN + FP)

  # AUC
  roc_obj <- pROC::roc(y_true, y_pred_prob, quiet = TRUE)
  auc_score <- pROC::auc(roc_obj)

  cat(sprintf("\nMetrics:\n"))
  cat(sprintf("  Accuracy:    %.4f\n", accuracy))
  cat(sprintf("  Precision:   %.4f (of predicted longs, how many correct?)\n", precision))
  cat(sprintf("  Recall:      %.4f (of actual longs, how many found?)\n", recall))
  cat(sprintf("  F1-Score:    %.4f\n", f1_score))
  cat(sprintf("  Specificity: %.4f (of actual neutrals, how many correct?)\n", specificity))
  cat(sprintf("  AUC:         %.4f (0.5=Random, 1.0=Perfect)\n", auc_score))

  # Baseline
  baseline_accuracy <- max(table(y_true)) / length(y_true)
  cat(sprintf("\nBaseline (always majority class): %.4f\n", baseline_accuracy))
  cat(sprintf("Improvement over baseline: %.2f%%\n",
              100 * (accuracy - baseline_accuracy) / baseline_accuracy))

  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    auc = auc_score,
    conf_matrix = conf_matrix
  ))
}

# --- Train Set Evaluation ---
y_pred_train <- predict(model_long, dtrain)
metrics_train_long <- evaluate_binary_model(y_train, y_pred_train, set_name = "TRAIN")

# --- Test Set Evaluation ---
y_pred_test <- predict(model_long, dtest)
metrics_test_long <- evaluate_binary_model(y_test, y_pred_test, set_name = "TEST")

# --- Feature Importance ---
cat("\n=== FEATURE IMPORTANCE (LONG MODEL) ===\n")
importance_long <- xgb.importance(
  feature_names = stable_features_long,
  model = model_long
)
cat("\nTop 10 features:\n")
print(head(importance_long, 10))

# ============================================================================
# SHORT MODEL PIPELINE
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("SHORT MODEL PIPELINE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7b: WALK-FORWARD FEATURE SELECTION (SHORT) =======================

cat("\n=== STEP 7b: TWO-STAGE FEATURE SELECTION (SHORT MODEL) ===\n")
cat("Strategy: XGBoost Walk-Forward → Boruta Single Run (same as LONG)\n\n")

# --- STAGE 1: XGBoost Feature Selection (50 features) ---

cat("--- STAGE 1: XGBoost Walk-Forward Feature Selection ---\n")
cat("Target: 50 stable features across all windows\n\n")

xgb_feature_importance_list_short <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d → Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  # Split data for this window
  dt_wf_train <- dt_train_short[year %in% window$train_years]
  dt_wf_val <- dt_train_short[year == window$val_year]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))
  cat(sprintf("  Val:   %s rows\n", format(nrow(dt_wf_val), big.mark = ",")))

  # Run XGBoost feature selection
  tic()
  fs_result <- select_important_features(
    dt = dt_wf_train,
    target_col = "label_binary",
    weight_col = "sample_weight",
    feature_cols = all_feature_cols,  # Pass allowed features explicitly
    method = "xgboost",
    n_top_features = 50,
    cv_folds = 3,
    verbose = FALSE
  )
  toc()

  xgb_feature_importance_list_short[[i]] <- fs_result$importance
  cat(sprintf("  ✓ Top 50 features selected\n"))
}

# Find stable features
cat("\n--- Identifying Stable Features (appear in ALL 4 windows) ---\n")

top_features_per_window_short <- lapply(xgb_feature_importance_list_short, function(imp) {
  head(imp$feature, 50)
})

feature_counts_short <- table(unlist(top_features_per_window_short))
stable_features_xgb_short <- names(feature_counts_short[feature_counts_short == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_xgb_short)))

if (length(stable_features_xgb_short) < 50) {
  cat(sprintf("WARNING: Only %d stable features found. Taking top 50 by average rank...\n",
              length(stable_features_xgb_short)))

  all_features_short <- unique(unlist(top_features_per_window_short))
  avg_ranks_short <- sapply(all_features_short, function(f) {
    ranks <- sapply(xgb_feature_importance_list_short, function(imp) {
      idx <- which(imp$feature == f)
      if (length(idx) == 0) return(999)
      return(idx)
    })
    mean(ranks)
  })

  stable_features_xgb_short <- names(sort(avg_ranks_short)[1:50])
}

cat(sprintf("✓ Stage 1 complete: %d stable features selected\n", length(stable_features_xgb_short)))

# --- STAGE 2: Boruta Feature Selection (ONCE on full training period) ---

cat("\n--- STAGE 2: Boruta Feature Selection (Single Run) ---\n")
cat("Strategy: Run Boruta ONCE on full training period (2019-2024)\n")
cat(sprintf("Input: %d stable features from XGBoost stage\n", length(stable_features_xgb_short)))
cat("Target: 15 final features\n\n")

# Create reduced dataset with only XGBoost-selected features
dt_train_short_reduced <- dt_train_short[, c(required_cols_boruta, stable_features_xgb_short), with = FALSE]

cat(sprintf("Running Boruta on full training set (%d-%d)...\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Training samples: %s\n", format(nrow(dt_train_short_reduced), big.mark = ",")))
cat("  This will take a few minutes...\n\n")

# Run Boruta ONCE on full training data (2019-2024)
tic()
fs_result <- select_important_features(
  dt = dt_train_short_reduced,
  target_col = "label_binary",
  weight_col = "sample_weight",
  feature_cols = stable_features_xgb_short,  # Pass XGBoost-selected features
  method = "boruta",
  n_top_features = 15,
  cv_folds = 1,
  verbose = TRUE  # Show progress for this long-running operation
)
toc()

# Extract top 15 features
stable_features_short <- fs_result$top_features

cat(sprintf("\n✓ Stage 2 complete: %d final features selected for SHORT model\n",
            length(stable_features_short)))

# === SAFETY CHECK: Verify no excluded features made it through ===
cat("\n--- SAFETY CHECK: Verifying no excluded features ---\n")

forbidden_features_found_short <- intersect(stable_features_short, excluded_features)
if (length(forbidden_features_found_short) > 0) {
  cat("WARNING: Found forbidden features in final selection!\n")
  cat(sprintf("  Removing: %s\n", paste(forbidden_features_found_short, collapse = ", ")))
  stable_features_short <- setdiff(stable_features_short, excluded_features)
  cat(sprintf("  Final count after removal: %d features\n", length(stable_features_short)))
} else {
  cat("✓ No forbidden features detected\n")
}

cat("\n=== FINAL FEATURES FOR SHORT MODEL ===\n")
cat(paste(stable_features_short, collapse = "\n"))
cat("\n")

# ===== STEP 8b: TRAIN FINAL SHORT MODEL =====================================

cat("\n=== STEP 8b: TRAIN FINAL SHORT MODEL ===\n")

final_cols_short <- c("datetime", "year", "label_binary", "sample_weight",
                      stable_features_short)

dt_train_short_final <- dt_train_short[, ..final_cols_short]
dt_test_short_final <- dt_test_short[, ..final_cols_short]

cat(sprintf("Train set: %s rows, %d features\n",
            format(nrow(dt_train_short_final), big.mark = ","),
            length(stable_features_short)))
cat(sprintf("Test set:  %s rows, %d features\n",
            format(nrow(dt_test_short_final), big.mark = ","),
            length(stable_features_short)))

# Prepare matrices
X_train_short <- as.matrix(dt_train_short_final[, ..stable_features_short])
y_train_short <- dt_train_short_final$label_binary
w_train_short <- dt_train_short_final$sample_weight

X_test_short <- as.matrix(dt_test_short_final[, ..stable_features_short])
y_test_short <- dt_test_short_final$label_binary

# Create DMatrix
dtrain_short <- xgb.DMatrix(data = X_train_short, label = y_train_short, weight = w_train_short)
dtest_short <- xgb.DMatrix(data = X_test_short, label = y_test_short)

# XGBoost parameters
params_short <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 3
)

cat("\nTraining XGBoost model...\n")
tic()
model_short <- xgb.train(
  params = params_short,
  data = dtrain_short,
  nrounds = 200,
  verbose = 0
)
toc()

cat("✓ Model trained\n")

# ===== STEP 9b: EVALUATE SHORT MODEL =========================================

cat("\n=== STEP 9b: EVALUATE SHORT MODEL ===\n")

# Train Set Evaluation
y_pred_train_short <- predict(model_short, dtrain_short)
metrics_train_short <- evaluate_binary_model(y_train_short, y_pred_train_short, set_name = "TRAIN")

# Test Set Evaluation
y_pred_test_short <- predict(model_short, dtest_short)
metrics_test_short <- evaluate_binary_model(y_test_short, y_pred_test_short, set_name = "TEST")

# Feature Importance
cat("\n=== FEATURE IMPORTANCE (SHORT MODEL) ===\n")
importance_short <- xgb.importance(
  feature_names = stable_features_short,
  model = model_short
)
cat("\nTop 10 features:\n")
print(head(importance_short, 10))

# ============================================================================
# SUMMARY & COMPARISON
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("FINAL SUMMARY: LONG vs SHORT MODELS\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("=== LONG MODEL ===\n")
cat(sprintf("Features: %d\n", length(stable_features_long)))
cat(sprintf("Train AUC: %.4f\n", metrics_train_long$auc))
cat(sprintf("Test AUC:  %.4f\n", metrics_test_long$auc))
cat(sprintf("Train F1:  %.4f\n", metrics_train_long$f1_score))
cat(sprintf("Test F1:   %.4f\n", metrics_test_long$f1_score))

cat("\n=== SHORT MODEL ===\n")
cat(sprintf("Features: %d\n", length(stable_features_short)))
cat(sprintf("Train AUC: %.4f\n", metrics_train_short$auc))
cat(sprintf("Test AUC:  %.4f\n", metrics_test_short$auc))
cat(sprintf("Train F1:  %.4f\n", metrics_train_short$f1_score))
cat(sprintf("Test F1:   %.4f\n", metrics_test_short$f1_score))

cat("\n=== LONG/SHORT PIPELINE V2 COMPLETE ===\n")
