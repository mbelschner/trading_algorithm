# ============================================================================
# ML BACKTEST PIPELINE - MULTI-CLASS (Version 2)
# ============================================================================
#
# KEY CHANGES FROM V1:
# 1. Features calculated on ALL price data (not just labeled samples)
# 2. Enhanced Neutral labels as default (configurable for RAW/STANDARD/UNFILTERED)
# 3. Multi-class classification: label = -1 (Short), 0 (Neutral), 1 (Long)
# 4. Walk-Forward Feature Selection on 2019-2024 (both XGBoost + Boruta)
# 5. 2025 as out-of-sample test set
# 6. Multi-class Confusion Matrix + Metrics for BOTH train and test sets
# 7. XGBoost multi:softprob objective for 3-class classification
#
# ============================================================================

cat("\n=== START ML BACKTEST PIPELINE (MULTI-CLASS V2) ===\n")

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

if (!dir.exists(backtest_output_path)) {
  dir.create(backtest_output_path, recursive = TRUE)
}
if (!dir.exists(features_cache_path)) {
  dir.create(features_cache_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"

# Label version selection
LABEL_VERSION <- "enhanced_neutral"  # Options: "enhanced_neutral", "raw", "standard", "unfiltered"

# Feature caching
FORCE_RECALCULATE_FEATURES <- FALSE

# Train/Test split years
TRAIN_YEARS <- 2019:2024
TEST_YEAR <- 2025

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

if ("time" %in% names(dt_prices)) {
  setnames(dt_prices, "time", "datetime")
}

if (is.character(dt_prices$datetime)) {
  dt_prices[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("✓ Price data loaded: %s rows\n", format(nrow(dt_prices), big.mark = ",")))
cat(sprintf("  Time range: %s to %s\n",
            min(dt_prices$datetime), max(dt_prices$datetime)))

# ===== STEP 2: LOAD LABELS ===================================================

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

if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("✓ Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

cat("\n  Label distribution:\n")
print(table(dt_labels$label))

# ===== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA =========================

cat("\n=== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA ===\n")

# Load pipeline modules
cat("\nLoading pipeline modules...\n")
source("r/02_01_indicator_calculation.R")
source("r/02_02_feature_engineering.R")
source("r/02_03_feature_selection.R")
source("r/02_04_purged_kfold_cv.R")
source("r/02_05_model_training.R")
source("r/02_06_backtest_evaluation.R")
cat("✓ All modules loaded\n")

# Check for cached features
features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (file.exists(features_cache_file) && !FORCE_RECALCULATE_FEATURES) {

  cat("\n=== LOADING CACHED FEATURES ===\n")
  dt_features_all <- fread(features_cache_file)
  setDT(dt_features_all)
  cat(sprintf("✓ Features loaded: %s rows, %d columns\n",
              format(nrow(dt_features_all), big.mark = ","),
              ncol(dt_features_all)))

} else {

  cat("\n=== CALCULATING FEATURES ===\n")

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

  tic()
  dt_features_all <- engineer_features(
    dt = dt_indicators,
    lag_periods = c(1, 2, 3, 5, 10),
    derivative_orders = c(1, 2),
    hourly_aggregation = TRUE,
    rolling_windows = c(10, 20, 50),
    interaction_features = FALSE,
    verbose = TRUE
  )
  toc()

  n_before_na <- nrow(dt_features_all)
  dt_features_all <- na.omit(dt_features_all)
  cat(sprintf("Rows after NA removal: %s (-%s)\n",
              format(nrow(dt_features_all), big.mark = ","),
              format(n_before_na - nrow(dt_features_all), big.mark = ",")))

  fwrite(dt_features_all, features_cache_file)
  cat(sprintf("✓ Features saved to cache\n"))
}

# ===== STEP 4: MERGE LABELS WITH FEATURES ====================================

cat("\n=== STEP 4: MERGE LABELS WITH FEATURES ===\n")

label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                          "bars_to_exit", "realized_return", "n_concurrent",
                          "realized_return_adj", "log_return")

available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))

dt_merged <- merge(
  dt_features_all,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE
)

cat(sprintf("✓ Merged dataset: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

if (!"label" %in% names(dt_merged)) {
  stop("ERROR: Labels could not be merged!")
}

cat("\nLabel distribution after merge:\n")
print(table(dt_merged$label))

# Add year column
dt_merged[, year := as.integer(format(datetime, "%Y"))]

# Convert labels to 0-indexed for XGBoost multi-class
# XGBoost expects: 0, 1, 2 for 3 classes
# Our labels: -1 (Short), 0 (Neutral), 1 (Long)
# Mapping: -1 → 0, 0 → 1, 1 → 2
dt_merged[, label_multiclass := as.integer(label + 1)]

cat("\nMulti-class label mapping (for XGBoost):\n")
cat("  -1 (Short)   → 0\n")
cat("   0 (Neutral) → 1\n")
cat("   1 (Long)    → 2\n")

cat("\nMulti-class label distribution:\n")
print(table(dt_merged$label_multiclass))

# ===== STEP 5: TRAIN/TEST SPLIT ==============================================

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

# ===== STEP 6: WALK-FORWARD FEATURE SELECTION ================================

cat("\n=== STEP 6: WALK-FORWARD FEATURE SELECTION ===\n")
cat("Strategy: Expanding Window Approach\n")
cat("  Window 1: 2019-2020 train → 2021 validate\n")
cat("  Window 2: 2019-2021 train → 2022 validate\n")
cat("  Window 3: 2019-2022 train → 2023 validate\n")
cat("  Window 4: 2019-2023 train → 2024 validate\n")
cat("→ Features important in ALL windows = stable!\n\n")

wf_windows <- list(
  list(train_years = 2019:2020, val_year = 2021),
  list(train_years = 2019:2021, val_year = 2022),
  list(train_years = 2019:2022, val_year = 2023),
  list(train_years = 2019:2023, val_year = 2024)
)

# Get feature columns
meta_cols <- c("datetime", "year", "label", "label_multiclass", "sample_weight",
               "barrier_touched", "bars_to_exit", "realized_return",
               "n_concurrent", "realized_return_adj", "log_return",
               "open", "high", "low", "close", "volume")

all_feature_cols <- setdiff(names(dt_train), meta_cols)
cat(sprintf("Total features available: %d\n", length(all_feature_cols)))

# NOTE: For multi-class XGBoost feature selection, we need to adapt the function
# For now, we'll use a workaround: convert to binary problem (directional vs neutral)
# This is a simplification - ideally we'd modify select_important_features() to support multi-class

cat("\n--- STAGE 1: XGBoost Feature Selection (50 features) ---\n")
cat("Note: Using binary approximation (directional vs neutral) for feature selection\n\n")

# Create binary target for feature selection: 0 = neutral, 1 = directional
dt_train[, label_binary_fs := fifelse(label == 0, 0, 1)]

xgb_feature_importance_list <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d → Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  dt_wf_train <- dt_train[year %in% window$train_years]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))

  tic()
  fs_result <- select_important_features(
    dt = dt_wf_train,
    target_col = "label_binary_fs",
    weight_col = "sample_weight",
    method = "xgboost",
    n_top_features = 50,
    cv_folds = 3,
    verbose = FALSE
  )
  toc()

  xgb_feature_importance_list[[i]] <- fs_result$importance
  cat(sprintf("  ✓ Top 50 features selected\n"))
}

# Find stable features
cat("\n--- Identifying Stable Features ---\n")

top_features_per_window <- lapply(xgb_feature_importance_list, function(imp) {
  head(imp$feature, 50)
})

feature_counts <- table(unlist(top_features_per_window))
stable_features_xgb <- names(feature_counts[feature_counts == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_xgb)))

if (length(stable_features_xgb) < 50) {
  cat(sprintf("WARNING: Only %d stable. Taking top 50 by average rank...\n",
              length(stable_features_xgb)))

  all_features <- unique(unlist(top_features_per_window))
  avg_ranks <- sapply(all_features, function(f) {
    ranks <- sapply(xgb_feature_importance_list, function(imp) {
      idx <- which(imp$feature == f)
      if (length(idx) == 0) return(999)
      return(idx)
    })
    mean(ranks)
  })

  stable_features_xgb <- names(sort(avg_ranks)[1:50])
}

cat(sprintf("✓ Stage 1 complete: %d features selected\n", length(stable_features_xgb)))

# --- STAGE 2: Boruta (15 features) ---

cat("\n--- STAGE 2: Boruta Feature Selection (15 features) ---\n")

required_cols_boruta <- c("datetime", "year", "label_binary_fs", "sample_weight")
dt_train_reduced <- dt_train[, c(required_cols_boruta, stable_features_xgb), with = FALSE]

boruta_feature_importance_list <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d\n",
              i, min(window$train_years), max(window$train_years)))

  dt_wf_train <- dt_train_reduced[year %in% window$train_years]
  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))

  tic()
  fs_result <- select_important_features(
    dt = dt_wf_train,
    target_col = "label_binary_fs",
    weight_col = "sample_weight",
    method = "boruta",
    n_top_features = 15,
    cv_folds = 1,
    verbose = FALSE
  )
  toc()

  boruta_feature_importance_list[[i]] <- fs_result$importance
  cat(sprintf("  ✓ Top 15 features selected\n"))
}

# Find stable features
cat("\n--- Identifying Stable Features ---\n")

top_features_per_window_boruta <- lapply(boruta_feature_importance_list, function(imp) {
  head(imp$feature, 15)
})

feature_counts_boruta <- table(unlist(top_features_per_window_boruta))
stable_features_final <- names(feature_counts_boruta[feature_counts_boruta == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_final)))

if (length(stable_features_final) < 15) {
  cat(sprintf("WARNING: Only %d stable. Taking top 15 by average rank...\n",
              length(stable_features_final)))

  all_features_boruta <- unique(unlist(top_features_per_window_boruta))
  avg_ranks_boruta <- sapply(all_features_boruta, function(f) {
    ranks <- sapply(boruta_feature_importance_list, function(imp) {
      idx <- which(imp$feature == f)
      if (length(idx) == 0) return(999)
      return(idx)
    })
    mean(ranks)
  })

  stable_features_final <- names(sort(avg_ranks_boruta)[1:15])
}

cat(sprintf("\n✓ Stage 2 complete: %d final features selected\n",
            length(stable_features_final)))

cat("\n=== FINAL FEATURES ===\n")
cat(paste(stable_features_final, collapse = "\n"))
cat("\n")

# ===== STEP 7: TRAIN FINAL MULTI-CLASS MODEL =================================

cat("\n=== STEP 7: TRAIN FINAL MULTI-CLASS MODEL ===\n")

final_cols <- c("datetime", "year", "label", "label_multiclass", "sample_weight",
                stable_features_final)

dt_train_final <- dt_train[, ..final_cols]
dt_test_final <- dt_test[, ..final_cols]

cat(sprintf("Train set: %s rows, %d features\n",
            format(nrow(dt_train_final), big.mark = ","),
            length(stable_features_final)))
cat(sprintf("Test set:  %s rows, %d features\n",
            format(nrow(dt_test_final), big.mark = ","),
            length(stable_features_final)))

# Prepare matrices
X_train <- as.matrix(dt_train_final[, ..stable_features_final])
y_train <- dt_train_final$label_multiclass
w_train <- dt_train_final$sample_weight

X_test <- as.matrix(dt_test_final[, ..stable_features_final])
y_test <- dt_test_final$label_multiclass

# Create DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# XGBoost parameters for multi-class
params_multiclass <- list(
  objective = "multi:softprob",
  num_class = 3,
  eval_metric = "mlogloss",
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 3
)

cat("\nTraining XGBoost multi-class model...\n")
tic()
model_multiclass <- xgb.train(
  params = params_multiclass,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)
toc()

cat("✓ Model trained\n")

# ===== STEP 8: EVALUATE MULTI-CLASS MODEL ====================================

cat("\n=== STEP 8: EVALUATE MULTI-CLASS MODEL ===\n")

# Helper function for multi-class evaluation
evaluate_multiclass_model <- function(y_true, y_pred_matrix, set_name = "Unknown") {

  # y_pred_matrix is n x 3 matrix of probabilities
  # Get predicted class (0, 1, 2)
  y_pred_class <- apply(y_pred_matrix, 1, which.max) - 1

  cat(sprintf("\n--- %s SET PERFORMANCE ---\n", set_name))

  # Confusion Matrix
  conf_matrix <- table(Predicted = y_pred_class, Actual = y_true)
  print(conf_matrix)

  # Overall accuracy
  accuracy <- sum(y_pred_class == y_true) / length(y_true)

  # Per-class metrics
  classes <- sort(unique(y_true))
  class_names <- c("Short", "Neutral", "Long")

  cat(sprintf("\nOverall Accuracy: %.4f\n", accuracy))
  cat("\nPer-Class Metrics:\n")

  metrics_list <- list()

  for (i in seq_along(classes)) {
    cls <- classes[i]
    cls_name <- class_names[i + 1]  # +1 because classes are 0,1,2

    # Binary classification for this class
    y_true_binary <- ifelse(y_true == cls, 1, 0)
    y_pred_binary <- ifelse(y_pred_class == cls, 1, 0)

    TP <- sum(y_true_binary == 1 & y_pred_binary == 1)
    TN <- sum(y_true_binary == 0 & y_pred_binary == 0)
    FP <- sum(y_true_binary == 0 & y_pred_binary == 1)
    FN <- sum(y_true_binary == 1 & y_pred_binary == 0)

    precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
    recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
    f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

    cat(sprintf("\n  %s (class %d):\n", cls_name, cls))
    cat(sprintf("    Precision: %.4f\n", precision))
    cat(sprintf("    Recall:    %.4f\n", recall))
    cat(sprintf("    F1-Score:  %.4f\n", f1))

    metrics_list[[cls_name]] <- list(
      precision = precision,
      recall = recall,
      f1_score = f1
    )
  }

  # Macro-averaged F1
  macro_f1 <- mean(sapply(metrics_list, function(x) x$f1_score))
  cat(sprintf("\nMacro-averaged F1: %.4f\n", macro_f1))

  # Baseline (always predict majority class)
  baseline_accuracy <- max(table(y_true)) / length(y_true)
  cat(sprintf("\nBaseline (majority class): %.4f\n", baseline_accuracy))
  cat(sprintf("Improvement: %.2f%%\n",
              100 * (accuracy - baseline_accuracy) / baseline_accuracy))

  return(list(
    accuracy = accuracy,
    macro_f1 = macro_f1,
    conf_matrix = conf_matrix,
    class_metrics = metrics_list
  ))
}

# Train Set Evaluation
y_pred_train_matrix <- predict(model_multiclass, dtrain, reshape = TRUE)
metrics_train <- evaluate_multiclass_model(y_train, y_pred_train_matrix, set_name = "TRAIN")

# Test Set Evaluation
y_pred_test_matrix <- predict(model_multiclass, dtest, reshape = TRUE)
metrics_test <- evaluate_multiclass_model(y_test, y_pred_test_matrix, set_name = "TEST")

# Feature Importance
cat("\n=== FEATURE IMPORTANCE ===\n")
importance_multiclass <- xgb.importance(
  feature_names = stable_features_final,
  model = model_multiclass
)
cat("\nTop 10 features:\n")
print(head(importance_multiclass, 10))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("FINAL SUMMARY: MULTI-CLASS MODEL\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat(sprintf("Features: %d\n", length(stable_features_final)))
cat(sprintf("Train Accuracy: %.4f\n", metrics_train$accuracy))
cat(sprintf("Test Accuracy:  %.4f\n", metrics_test$accuracy))
cat(sprintf("Train Macro-F1: %.4f\n", metrics_train$macro_f1))
cat(sprintf("Test Macro-F1:  %.4f\n", metrics_test$macro_f1))

cat("\n=== MULTI-CLASS PIPELINE V2 COMPLETE ===\n")
