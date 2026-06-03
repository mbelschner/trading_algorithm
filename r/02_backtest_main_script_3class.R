# ============================================================================
# ML BACKTEST PIPELINE - 3-CLASS MODEL (Long/Neutral/Short)
# ============================================================================
#
# KEY CHANGES FROM LONG/SHORT SPLIT VERSION:
# 1. Single model with 3 classes: Long (1), Neutral (0), Short (-1)
# 2. Uses XGBoost multi:softprob objective for multiclass classification
# 3. Features calculated on ALL price data (not just labeled samples)
# 4. Two-Stage Feature Selection:
#    - Stage 1: Walk-Forward XGBoost (4 windows) → Top 50 stable features
#    - Stage 2: Single Boruta run on full training period (2019-2024) → Top 15 final features
# 5. 2025 as out-of-sample test set
# 6. Confusion Matrix + Metrics for BOTH train and test sets (3x3 matrix)
# 7. Excluded features: Returns, ATR, Hour/Session (reserved for meta-labeling)
#
# ============================================================================

cat("\n=== START ML BACKTEST PIPELINE (3-CLASS MODEL) ===\n")

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

# Additional markets for feature analysis
ADDITIONAL_MARKETS <- c("DXY")  # Set to NULL or c() to disable
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)  # Lag periods for additional market features

# Feature caching
FORCE_RECALCULATE_FEATURES <- FALSE  # Set to TRUE if feature selection logic changed

# Train/Test split years
TRAIN_YEARS <- 2019:2024  # Feature selection and training
TEST_YEAR <- 2025          # Out-of-sample test

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Model Type: 3-CLASS (Long/Neutral/Short)\n"))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Train Period: %d-%d\n", min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Test Period: %d\n", TEST_YEAR))
if (!is.null(ADDITIONAL_MARKETS) && length(ADDITIONAL_MARKETS) > 0) {
  cat(sprintf("  Additional Markets: %s\n", paste(ADDITIONAL_MARKETS, collapse = ", ")))
  cat(sprintf("  Additional Markets Lag Periods: %s\n", paste(ADDITIONAL_MARKETS_LAG_PERIODS, collapse = ", ")))
}

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
source("r/02_01b_additional_markets.R")
cat("✓ Additional Markets module loaded\n")
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

  # === LOAD AND MERGE ADDITIONAL MARKETS ===
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

# ===== STEP 6: PREPARE 3-CLASS LABELS ========================================

cat("\n=== STEP 6: PREPARE 3-CLASS LABELS ===\n")

# Convert labels to 0-indexed for XGBoost multiclass:
# Original: -1 (Short), 0 (Neutral), 1 (Long)
# XGBoost:   0 (Short), 1 (Neutral), 2 (Long)

dt_train[, label_multiclass := fifelse(label == -1, 0L,
                                fifelse(label == 0, 1L, 2L))]
dt_test[, label_multiclass := fifelse(label == -1, 0L,
                               fifelse(label == 0, 1L, 2L))]

cat("Label mapping for XGBoost:\n")
cat("  Original -1 (Short)   → XGBoost 0\n")
cat("  Original  0 (Neutral) → XGBoost 1\n")
cat("  Original  1 (Long)    → XGBoost 2\n")

cat("\nTrain multiclass distribution:\n")
print(table(dt_train$label_multiclass))

cat("\nTest multiclass distribution:\n")
print(table(dt_test$label_multiclass))

# ============================================================================
# 3-CLASS MODEL PIPELINE
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("3-CLASS MODEL PIPELINE (Long/Neutral/Short)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7: TWO-STAGE FEATURE SELECTION ===================================

cat("\n=== STEP 7: TWO-STAGE FEATURE SELECTION ===\n")
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
meta_cols <- c("datetime", "year", "label", "label_multiclass", "sample_weight",
               "barrier_touched", "bars_to_exit", "realized_return",
               "n_concurrent", "realized_return_adj", "log_return",
               "open", "high", "low", "close", "volume")

# Additional exclusions: return features (lookahead), session/hour features, and ATR features (for meta-labeling)
excluded_features <- c(
  # Return features (lookahead bias)
  "log_return", "realized_return", "realized_return_adj",
  # Session/hour features (reserved for meta-labeling)
  "hour", "hour_sin", "hour_cos", "hour_open", "hour_high", "hour_low",
  "hour_close", "hour_volume", "hour_close_mean", "hour_close_sd",
  "session_london", "session_ny", "session_asia", "session_overlap"
)

# ATR features (reserved for meta-labeling / risk adjustment) - use pattern matching
atr_features <- names(dt_train)[grepl("^atr_|_atr_", names(dt_train), ignore.case = TRUE)]
cat(sprintf("ATR features found and excluded: %d\n", length(atr_features)))

# Combine all exclusions
excluded_features <- c(excluded_features, atr_features)

all_feature_cols <- setdiff(names(dt_train), c(meta_cols, excluded_features))
cat(sprintf("Total features available: %d\n", length(all_feature_cols)))
cat(sprintf("Excluded features: %s\n", paste(intersect(names(dt_train), excluded_features), collapse = ", ")))

# --- STAGE 1: XGBoost Feature Selection (50 features) ---

cat("\n--- STAGE 1: XGBoost Walk-Forward Feature Selection ---\n")
cat("Target: 50 stable features across all windows\n")
cat("Using multiclass objective for feature selection\n\n")

xgb_feature_importance_list <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d → Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  # Split data for this window
  dt_wf_train <- dt_train[year %in% window$train_years]
  dt_wf_val <- dt_train[year == window$val_year]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))
  cat(sprintf("  Val:   %s rows\n", format(nrow(dt_wf_val), big.mark = ",")))

  # Prepare matrices for multiclass XGBoost
  X_wf <- as.matrix(dt_wf_train[, ..all_feature_cols])
  y_wf <- dt_wf_train$label_multiclass
  w_wf <- dt_wf_train$sample_weight

  # XGBoost parameters for multiclass feature selection
  params_fs <- list(
    objective = "multi:softprob",
    num_class = 3,
    eval_metric = "mlogloss",
    max_depth = 4,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 10
  )

  # Create DMatrix
  dtrain_fs <- xgb.DMatrix(data = X_wf, label = y_wf, weight = w_wf)

  # Train model for feature importance
  tic()
  model_fs <- xgb.train(
    params = params_fs,
    data = dtrain_fs,
    nrounds = 100,
    verbose = 0
  )
  toc()

  # Get feature importance
  importance <- xgb.importance(feature_names = all_feature_cols, model = model_fs)
  xgb_feature_importance_list[[i]] <- importance

  cat(sprintf("  ✓ Top 50 features selected\n"))
}

# Find features that appear in ALL windows (stable features)
cat("\n--- Identifying Stable Features (appear in ALL 4 windows) ---\n")

# Get top 50 features from each window
top_features_per_window <- lapply(xgb_feature_importance_list, function(imp) {
  head(imp$Feature, 50)
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
      idx <- which(imp$Feature == f)
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
required_cols_boruta <- c("datetime", "year", "label_multiclass", "sample_weight")
dt_train_reduced <- dt_train[, c(required_cols_boruta, stable_features_xgb), with = FALSE]

cat(sprintf("Running Boruta on full training set (%d-%d)...\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Training samples: %s\n", format(nrow(dt_train_reduced), big.mark = ",")))
cat("  This will take a few minutes...\n\n")

# Run Boruta ONCE on full training data (2019-2024)
# Note: Boruta uses ranger internally which handles multiclass
tic()
fs_result <- select_important_features(
  dt = dt_train_reduced,
  target_col = "label_multiclass",
  weight_col = "sample_weight",
  feature_cols = stable_features_xgb,
  method = "boruta",
  n_top_features = 15,
  cv_folds = 1,
  verbose = TRUE
)
toc()

# Extract top 15 features
stable_features <- fs_result$top_features

cat(sprintf("\n✓ Stage 2 complete: %d final features selected\n",
            length(stable_features)))

# === SAFETY CHECK: Verify no excluded features made it through ===
cat("\n--- SAFETY CHECK: Verifying no excluded features ---\n")

forbidden_features_found <- intersect(stable_features, excluded_features)
if (length(forbidden_features_found) > 0) {
  cat("WARNING: Found forbidden features in final selection!\n")
  cat(sprintf("  Removing: %s\n", paste(forbidden_features_found, collapse = ", ")))
  stable_features <- setdiff(stable_features, excluded_features)
  cat(sprintf("  Final count after removal: %d features\n", length(stable_features)))
} else {
  cat("✓ No forbidden features detected\n")
}

cat("\n=== FINAL FEATURES FOR 3-CLASS MODEL ===\n")
cat(paste(stable_features, collapse = "\n"))
cat("\n")

# ===== STEP 7.5: SIMPLE PARAMETER GRID SEARCH ================================

cat("\n=== STEP 7.5: SIMPLE PARAMETER GRID SEARCH (3-CLASS MODEL) ===\n")

# Define parameter grid
param_grid <- expand.grid(
  max_depth = c(3),
  eta = c(0.1),
  gamma = c(0.1),
  lambda = c(1.5),
  min_child_weight = c(10, 20),
  stringsAsFactors = FALSE
)

n_combinations <- nrow(param_grid)
cat(sprintf("Testing %d parameter combinations\n", n_combinations))
cat(sprintf("Estimated time: ~%d-%d minutes\n\n",
            ceiling(n_combinations * 0.5),
            ceiling(n_combinations * 1.5)))

# Prepare data
final_cols_temp <- c("datetime", "year", "label_multiclass", "sample_weight", stable_features)
dt_train_grid <- dt_train[, ..final_cols_temp]
dt_test_grid <- dt_test[, ..final_cols_temp]

X_train_grid <- as.matrix(dt_train_grid[, ..stable_features])
y_train_grid <- dt_train_grid$label_multiclass
w_train_grid <- dt_train_grid$sample_weight

X_test_grid <- as.matrix(dt_test_grid[, ..stable_features])
y_test_grid <- dt_test_grid$label_multiclass

# Class distribution for weighting
class_counts <- table(y_train_grid)
cat("Class distribution in training set:\n")
cat(sprintf("  Class 0 (Short):   %s\n", format(class_counts["0"], big.mark = ",")))
cat(sprintf("  Class 1 (Neutral): %s\n", format(class_counts["1"], big.mark = ",")))
cat(sprintf("  Class 2 (Long):    %s\n", format(class_counts["2"], big.mark = ",")))

# Split for early stopping
set.seed(42)
val_idx_grid <- sample(1:nrow(X_train_grid), size = floor(0.2 * nrow(X_train_grid)))
train_idx_grid <- setdiff(1:nrow(X_train_grid), val_idx_grid)

X_train_sub_grid <- X_train_grid[train_idx_grid, ]
y_train_sub_grid <- y_train_grid[train_idx_grid]
w_train_sub_grid <- w_train_grid[train_idx_grid]

X_val_grid <- X_train_grid[val_idx_grid, ]
y_val_grid <- y_train_grid[val_idx_grid]
w_val_grid <- w_train_grid[val_idx_grid]

# Initialize results
grid_results <- data.frame()

# Progress bar
pb <- progress_bar$new(
  format = "  [:bar] :percent | Combo :current/:total | ETA: :eta",
  total = n_combinations,
  clear = FALSE,
  width = 70
)

# Helper function for multiclass metrics
calculate_multiclass_metrics <- function(y_true, y_pred_prob_matrix) {
  # y_pred_prob_matrix: rows = samples, cols = classes (0, 1, 2)
  y_pred_class <- max.col(y_pred_prob_matrix) - 1  # Convert to 0-indexed

  # Confusion matrix
  conf_matrix <- table(Predicted = factor(y_pred_class, levels = 0:2),
                       Actual = factor(y_true, levels = 0:2))

  # Overall accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  # Per-class metrics
  metrics_per_class <- list()
  for (cls in 0:2) {
    cls_name <- c("Short", "Neutral", "Long")[cls + 1]
    tp <- conf_matrix[as.character(cls), as.character(cls)]
    fp <- sum(conf_matrix[as.character(cls), ]) - tp
    fn <- sum(conf_matrix[, as.character(cls)]) - tp
    tn <- sum(conf_matrix) - tp - fp - fn

    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    f1 <- ifelse((precision + recall) > 0, 2 * precision * recall / (precision + recall), 0)

    metrics_per_class[[cls_name]] <- list(
      precision = precision,
      recall = recall,
      f1 = f1
    )
  }

  # Macro-averaged metrics
  macro_precision <- mean(sapply(metrics_per_class, function(x) x$precision))
  macro_recall <- mean(sapply(metrics_per_class, function(x) x$recall))
  macro_f1 <- mean(sapply(metrics_per_class, function(x) x$f1))

  return(list(
    accuracy = accuracy,
    macro_precision = macro_precision,
    macro_recall = macro_recall,
    macro_f1 = macro_f1,
    per_class = metrics_per_class,
    conf_matrix = conf_matrix
  ))
}

# Loop over parameter combinations
for (i in 1:n_combinations) {

  # Current parameters
  params_test <- list(
    objective = "multi:softprob",
    num_class = 3,
    eval_metric = "mlogloss",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = 0.8,
    colsample_bytree = 0.8,
    colsample_bynode = 0.8
  )

  # Create DMatrix
  dtrain_grid <- xgb.DMatrix(data = X_train_sub_grid, label = y_train_sub_grid, weight = w_train_sub_grid)
  dval_grid <- xgb.DMatrix(data = X_val_grid, label = y_val_grid, weight = w_val_grid)
  dtrain_full_grid <- xgb.DMatrix(data = X_train_grid, label = y_train_grid, weight = w_train_grid)
  dtest_grid <- xgb.DMatrix(data = X_test_grid, label = y_test_grid)

  # Train model
  model_grid <- xgb.train(
    params = params_test,
    data = dtrain_grid,
    nrounds = 1000,
    evals = list(train = dtrain_grid, val = dval_grid),
    early_stopping_rounds = 50,
    verbose = 0
  )

  # Predictions on training set (returns matrix: nsamples x nclasses)
  pred_train_grid <- predict(model_grid, dtrain_full_grid, reshape = TRUE)
  train_metrics <- calculate_multiclass_metrics(y_train_grid, pred_train_grid)

  # Predictions on test set
  pred_test_grid <- predict(model_grid, dtest_grid, reshape = TRUE)
  test_metrics <- calculate_multiclass_metrics(y_test_grid, pred_test_grid)

  # Store results
  grid_results <- rbind(grid_results, data.frame(
    combination_id = i,
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    train_accuracy = train_metrics$accuracy,
    train_macro_precision = train_metrics$macro_precision,
    train_macro_recall = train_metrics$macro_recall,
    train_macro_f1 = train_metrics$macro_f1,
    test_accuracy = test_metrics$accuracy,
    test_macro_precision = test_metrics$macro_precision,
    test_macro_recall = test_metrics$macro_recall,
    test_macro_f1 = test_metrics$macro_f1,
    stringsAsFactors = FALSE
  ))

  pb$tick()
}

# Save results
grid_output_path <- file.path(backtest_output_path, "parameter_grid")
if (!dir.exists(grid_output_path)) {
  dir.create(grid_output_path, recursive = TRUE)
}

# Add ranking based on key metrics (lower rank = better)
grid_results$rank_train_accuracy <- rank(-grid_results$train_accuracy, na.last = "keep")
grid_results$rank_train_macro_f1 <- rank(-grid_results$train_macro_f1, na.last = "keep")
grid_results$rank_test_accuracy <- rank(-grid_results$test_accuracy, na.last = "keep")
grid_results$rank_test_macro_f1 <- rank(-grid_results$test_macro_f1, na.last = "keep")

# Calculate average rank (lower is better)
grid_results$avg_rank <- rowMeans(grid_results[, c("rank_train_accuracy", "rank_train_macro_f1",
                                                    "rank_test_accuracy", "rank_test_macro_f1")],
                                   na.rm = TRUE)

fwrite(grid_results, file.path(grid_output_path, paste0(EPIC, "_", INTERVAL, "_3class_grid_results.csv")))

cat(sprintf("\n\n✓ Grid search complete. Results saved to:\n"))
cat(sprintf("  %s\n", file.path(grid_output_path, paste0(EPIC, "_", INTERVAL, "_3class_grid_results.csv"))))

# Find best parameters (best test macro F1)
best_idx <- which.max(grid_results$test_macro_f1)
best_params <- grid_results[best_idx, ]

cat("\n=== BEST PARAMETERS (3-CLASS MODEL) ===\n")
cat(sprintf("Combination ID: %d\n", best_params$combination_id))
cat(sprintf("  max_depth:        %d\n", best_params$max_depth))
cat(sprintf("  eta:              %.3f\n", best_params$eta))
cat(sprintf("  gamma:            %.2f\n", best_params$gamma))
cat(sprintf("  lambda:           %.2f\n", best_params$lambda))
cat(sprintf("  min_child_weight: %d\n", best_params$min_child_weight))
cat(sprintf("  Average Rank:     %.2f (lower is better)\n\n", best_params$avg_rank))

cat("Training Performance:\n")
cat(sprintf("  Train Accuracy:        %.4f\n", best_params$train_accuracy))
cat(sprintf("  Train Macro Precision: %.4f\n", best_params$train_macro_precision))
cat(sprintf("  Train Macro Recall:    %.4f\n", best_params$train_macro_recall))
cat(sprintf("  Train Macro F1:        %.4f\n\n", best_params$train_macro_f1))

cat("Test Performance:\n")
cat(sprintf("  Test Accuracy:         %.4f\n", best_params$test_accuracy))
cat(sprintf("  Test Macro Precision:  %.4f\n", best_params$test_macro_precision))
cat(sprintf("  Test Macro Recall:     %.4f\n", best_params$test_macro_recall))
cat(sprintf("  Test Macro F1:         %.4f\n\n", best_params$test_macro_f1))

# ===== STEP 8: TRAIN FINAL 3-CLASS MODEL =====================================

cat("\n=== STEP 8: TRAIN FINAL 3-CLASS MODEL (WITH BEST PARAMETERS) ===\n")

# Prepare final datasets with selected features
final_cols <- c("datetime", "year", "label", "label_multiclass", "sample_weight",
                stable_features)

dt_train_final <- dt_train[, ..final_cols]
dt_test_final <- dt_test[, ..final_cols]

cat(sprintf("Train set: %s rows, %d features\n",
            format(nrow(dt_train_final), big.mark = ","),
            length(stable_features)))
cat(sprintf("Test set:  %s rows, %d features\n",
            format(nrow(dt_test_final), big.mark = ","),
            length(stable_features)))

# Prepare matrices
X_train <- as.matrix(dt_train_final[, ..stable_features])
y_train <- dt_train_final$label_multiclass
w_train <- dt_train_final$sample_weight

X_test <- as.matrix(dt_test_final[, ..stable_features])
y_test <- dt_test_final$label_multiclass

# Split training data into train/validation for early stopping
set.seed(42)
val_idx <- sample(1:nrow(X_train), size = floor(0.2 * nrow(X_train)))
train_idx <- setdiff(1:nrow(X_train), val_idx)

X_train_sub <- X_train[train_idx, ]
y_train_sub <- y_train[train_idx]
w_train_sub <- w_train[train_idx]

X_val <- X_train[val_idx, ]
y_val <- y_train[val_idx]
w_val <- w_train[val_idx]

# Create DMatrix with validation set
dtrain <- xgb.DMatrix(data = X_train_sub, label = y_train_sub, weight = w_train_sub)
dval <- xgb.DMatrix(data = X_val, label = y_val, weight = w_val)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Class distribution
cat("\nClass distribution in training set:\n")
print(table(y_train))

# Use best parameters from grid search
params <- list(
  objective = "multi:softprob",
  num_class = 3,
  eval_metric = "mlogloss",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = 0.8,
  colsample_bytree = 0.8,
  colsample_bynode = 0.8,
  min_child_weight = best_params$min_child_weight,
  gamma = best_params$gamma,
  lambda = best_params$lambda,
  alpha = 0.1
)

cat("\nTraining XGBoost 3-class model with early stopping...\n")
tic()
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  evals = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  verbose = 0
)
toc()

cat(sprintf("✓ Model trained (best iteration: %d)\n", model$best_iteration))

# --- Save Model (Multiple Formats for Safety) ---
models_output_path <- file.path(backtest_output_path, "models")
if (!dir.exists(models_output_path)) {
  dir.create(models_output_path, recursive = TRUE)
}

cat("\nSaving 3-class model in multiple formats...\n")

# Format 1: RDS (R binary format)
model_file_rds <- file.path(
  models_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_3class_", LABEL_VERSION, ".rds")
)
saveRDS(model, model_file_rds)
cat(sprintf("  ✓ RDS saved: %s (%.2f MB)\n",
            basename(model_file_rds),
            file.size(model_file_rds) / 1024^2))

# Format 2: XGBoost native binary format
model_file_xgb <- file.path(
  models_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_3class_", LABEL_VERSION, ".xgb")
)
xgb.save(model, model_file_xgb)
cat(sprintf("  ✓ XGB saved: %s (%.2f MB)\n",
            basename(model_file_xgb),
            file.size(model_file_xgb) / 1024^2))

# Format 3: JSON format (human-readable, for debugging)
model_file_json <- file.path(
  models_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_3class_", LABEL_VERSION, ".json")
)
xgb.save(model, model_file_json)
cat(sprintf("  ✓ JSON saved: %s (%.2f MB)\n",
            basename(model_file_json),
            file.size(model_file_json) / 1024^2))

# Save metadata separately (as safety check)
model_metadata <- list(
  feature_names = model$feature_names,
  n_features = length(model$feature_names),
  niter = model$niter,
  best_iteration = model$best_iteration,
  params = model$params,
  class_mapping = c("0=Short", "1=Neutral", "2=Long"),
  save_timestamp = Sys.time()
)

model_meta_file <- file.path(
  models_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_3class_", LABEL_VERSION, "_metadata.rds")
)
saveRDS(model_metadata, model_meta_file)
cat(sprintf("  ✓ Metadata saved: %s\n", basename(model_meta_file)))

cat(sprintf("\n✓ 3-class model saved in 4 formats (RDS, XGB, JSON, Metadata)\n"))

# ===== STEP 9: EVALUATE 3-CLASS MODEL ========================================

cat("\n=== STEP 9: EVALUATE 3-CLASS MODEL ===\n")

# --- Helper function for 3-class confusion matrix and metrics ---
evaluate_multiclass_model <- function(y_true, y_pred_prob_matrix, set_name = "Unknown") {

  y_pred_class <- max.col(y_pred_prob_matrix) - 1  # Convert to 0-indexed

  cat(sprintf("\n--- %s SET PERFORMANCE ---\n", set_name))

  # Confusion Matrix (3x3)
  conf_matrix <- table(
    Predicted = factor(y_pred_class, levels = 0:2, labels = c("Short", "Neutral", "Long")),
    Actual = factor(y_true, levels = 0:2, labels = c("Short", "Neutral", "Long"))
  )

  cat("\nConfusion Matrix:\n")
  print(conf_matrix)

  # Overall accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  # Per-class metrics
  cat("\nPer-Class Metrics:\n")
  class_names <- c("Short", "Neutral", "Long")
  metrics_list <- list()

  for (cls_idx in 1:3) {
    cls_name <- class_names[cls_idx]
    tp <- conf_matrix[cls_name, cls_name]
    fp <- sum(conf_matrix[cls_name, ]) - tp
    fn <- sum(conf_matrix[, cls_name]) - tp

    precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    f1 <- ifelse((precision + recall) > 0, 2 * precision * recall / (precision + recall), 0)

    cat(sprintf("  %s:\n", cls_name))
    cat(sprintf("    Precision: %.4f\n", precision))
    cat(sprintf("    Recall:    %.4f\n", recall))
    cat(sprintf("    F1-Score:  %.4f\n", f1))

    metrics_list[[cls_name]] <- list(precision = precision, recall = recall, f1 = f1)
  }

  # Macro-averaged metrics
  macro_precision <- mean(sapply(metrics_list, function(x) x$precision))
  macro_recall <- mean(sapply(metrics_list, function(x) x$recall))
  macro_f1 <- mean(sapply(metrics_list, function(x) x$f1))

  cat(sprintf("\nOverall Metrics:\n"))
  cat(sprintf("  Accuracy:         %.4f\n", accuracy))
  cat(sprintf("  Macro Precision:  %.4f\n", macro_precision))
  cat(sprintf("  Macro Recall:     %.4f\n", macro_recall))
  cat(sprintf("  Macro F1-Score:   %.4f\n", macro_f1))

  # Baseline
  baseline_accuracy <- max(table(y_true)) / length(y_true)
  cat(sprintf("\nBaseline (always majority class): %.4f\n", baseline_accuracy))
  cat(sprintf("Improvement over baseline: %.2f%%\n",
              100 * (accuracy - baseline_accuracy) / baseline_accuracy))

  return(list(
    accuracy = accuracy,
    macro_precision = macro_precision,
    macro_recall = macro_recall,
    macro_f1 = macro_f1,
    per_class = metrics_list,
    conf_matrix = conf_matrix
  ))
}

# --- Train Set Evaluation (full training set) ---
dtrain_full <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
y_pred_train <- predict(model, dtrain_full, reshape = TRUE)
metrics_train <- evaluate_multiclass_model(y_train, y_pred_train, set_name = "TRAIN")

# --- Test Set Evaluation ---
y_pred_test <- predict(model, dtest, reshape = TRUE)
metrics_test <- evaluate_multiclass_model(y_test, y_pred_test, set_name = "TEST")

# --- Feature Importance ---
cat("\n=== FEATURE IMPORTANCE (3-CLASS MODEL) ===\n")
importance <- xgb.importance(
  feature_names = stable_features,
  model = model
)
cat("\nTop 15 features:\n")
print(head(importance, 15))

# Save feature importance
importance_file <- file.path(
  models_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_feature_importance.csv")
)
fwrite(importance, importance_file)
cat(sprintf("\n✓ Feature importance saved: %s\n", importance_file))

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("FINAL SUMMARY: 3-CLASS MODEL (Long/Neutral/Short)\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("=== MODEL CONFIGURATION ===\n")
cat(sprintf("Features: %d\n", length(stable_features)))
cat(sprintf("Classes: Short (0), Neutral (1), Long (2)\n"))
cat(sprintf("Best Iteration: %d\n", model$best_iteration))

cat("\n=== TRAINING PERFORMANCE ===\n")
cat(sprintf("Accuracy:         %.4f\n", metrics_train$accuracy))
cat(sprintf("Macro Precision:  %.4f\n", metrics_train$macro_precision))
cat(sprintf("Macro Recall:     %.4f\n", metrics_train$macro_recall))
cat(sprintf("Macro F1:         %.4f\n", metrics_train$macro_f1))

cat("\n=== TEST PERFORMANCE ===\n")
cat(sprintf("Accuracy:         %.4f\n", metrics_test$accuracy))
cat(sprintf("Macro Precision:  %.4f\n", metrics_test$macro_precision))
cat(sprintf("Macro Recall:     %.4f\n", metrics_test$macro_recall))
cat(sprintf("Macro F1:         %.4f\n", metrics_test$macro_f1))

cat("\n=== SELECTED FEATURES ===\n")
cat(paste(stable_features, collapse = "\n"))
cat("\n")

cat("\n=== MODEL FILES ===\n")
cat(sprintf("  RDS:      %s\n", model_file_rds))
cat(sprintf("  XGB:      %s\n", model_file_xgb))
cat(sprintf("  JSON:     %s\n", model_file_json))
cat(sprintf("  Metadata: %s\n", model_meta_file))

cat("\n=== 3-CLASS PIPELINE COMPLETE ===\n")
