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
# 9. Hyperparameter Grid Search with average rank selection
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
  ranger,          # Random Forest (for Boruta)
  Boruta,          # Feature selection
  caret,           # ML framework
  Metrics,         # Evaluation metrics
  pROC,            # ROC/AUC calculation
  jsonlite         # Save model with feature names
)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")
models_path <- file.path(backtest_output_path, "models")
grid_output_path <- file.path(backtest_output_path, "parameter_grid")

for (dir_path in c(backtest_output_path, features_cache_path, models_path, grid_output_path)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
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

# Additional markets for features (set to NULL to disable)
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Train Period: %d-%d\n", min(TRAIN_YEARS), max(TRAIN_YEARS)))
cat(sprintf("  Test Period: %d\n", TEST_YEAR))
cat(sprintf("  Additional Markets: %s\n",
            ifelse(is.null(ADDITIONAL_MARKETS), "None", paste(ADDITIONAL_MARKETS, collapse = ", "))))

# ===== STEP 1: LOAD RAW PRICE DATA ===========================================

cat("\n=== STEP 1: LOAD RAW PRICE DATA ===\n")

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

cat(sprintf("Price data loaded: %s rows\n", format(nrow(dt_prices), big.mark = ",")))
cat(sprintf("  Time range: %s to %s\n", min(dt_prices$datetime), max(dt_prices$datetime)))

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

cat(sprintf("Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

cat("\nLabel distribution:\n")
print(table(dt_labels$label))

# ===== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA ==========================

cat("\n=== STEP 3: CALCULATE FEATURES ON ALL PRICE DATA ===\n")

# Load pipeline modules
cat("\nLoading pipeline modules...\n")
source("r/02_01_indicator_calculation.R")
source("r/02_02_feature_engineering.R")
source("r/02_03_feature_selection.R")
source("r/02_04_purged_kfold_cv.R")
source("r/02_05_model_training.R")
source("r/02_06_backtest_evaluation.R")

# Load additional markets module if needed
if (!is.null(ADDITIONAL_MARKETS) && length(ADDITIONAL_MARKETS) > 0) {
  source("r/02_07_additional_markets.R")
}

cat("All modules loaded\n")

# Check for cached features
features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (file.exists(features_cache_file) && !FORCE_RECALCULATE_FEATURES) {

  cat("\n=== LOADING CACHED FEATURES ===\n")
  dt_features_all <- fread(features_cache_file)
  setDT(dt_features_all)

  if (is.character(dt_features_all$datetime)) {
    dt_features_all[, datetime := as.POSIXct(datetime, tz = "UTC")]
  }

  cat(sprintf("Features loaded: %s rows, %d columns\n",
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
  cat(sprintf("Features saved: %s\n", features_cache_file))
}

# ===== STEP 4: MERGE LABELS WITH FEATURES ====================================

cat("\n=== STEP 4: MERGE LABELS WITH FEATURES ===\n")

cat(sprintf("Features before merge: %s rows\n",
            format(nrow(dt_features_all), big.mark = ",")))

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

cat(sprintf("Merged dataset: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

if (!"label" %in% names(dt_merged)) {
  stop("ERROR: Labels could not be merged!")
}

cat("\nLabel distribution after merge:\n")
print(table(dt_merged$label))

# Add year column
dt_merged[, year := as.integer(format(datetime, "%Y"))]

# ===== STEP 5: CREATE LONG/SHORT DATASETS ====================================

cat("\n=== STEP 5: CREATE LONG/SHORT DATASETS ===\n")

# LONG dataset: Only Long (1) and Neutral (0) - EXCLUDE SHORTS
dt_long <- dt_merged[label != -1]
dt_long[, label_binary := as.integer(label == 1)]

cat(sprintf("\nLONG Dataset (Long vs Neutral):\n"))
cat(sprintf("  Total rows: %s\n", format(nrow(dt_long), big.mark = ",")))
cat(sprintf("  Label 0 (Neutral): %s\n", format(sum(dt_long$label_binary == 0), big.mark = ",")))
cat(sprintf("  Label 1 (Long):    %s\n", format(sum(dt_long$label_binary == 1), big.mark = ",")))

# SHORT dataset: Only Short (-1) and Neutral (0) - EXCLUDE LONGS
dt_short <- dt_merged[label != 1]
dt_short[, label_binary := as.integer(label == -1)]

cat(sprintf("\nSHORT Dataset (Short vs Neutral):\n"))
cat(sprintf("  Total rows: %s\n", format(nrow(dt_short), big.mark = ",")))
cat(sprintf("  Label 0 (Neutral): %s\n", format(sum(dt_short$label_binary == 0), big.mark = ",")))
cat(sprintf("  Label 1 (Short):   %s\n", format(sum(dt_short$label_binary == 1), big.mark = ",")))

# ===== STEP 6: TRAIN/TEST SPLIT ==============================================

cat("\n=== STEP 6: TRAIN/TEST SPLIT ===\n")

# LONG splits
dt_train_long <- dt_long[year %in% TRAIN_YEARS]
dt_test_long <- dt_long[year == TEST_YEAR]

cat(sprintf("\nLONG Model:\n"))
cat(sprintf("  Train (%d-%d): %s rows\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS),
            format(nrow(dt_train_long), big.mark = ",")))
cat(sprintf("  Test (%d):     %s rows\n",
            TEST_YEAR,
            format(nrow(dt_test_long), big.mark = ",")))

# SHORT splits
dt_train_short <- dt_short[year %in% TRAIN_YEARS]
dt_test_short <- dt_short[year == TEST_YEAR]

cat(sprintf("\nSHORT Model:\n"))
cat(sprintf("  Train (%d-%d): %s rows\n",
            min(TRAIN_YEARS), max(TRAIN_YEARS),
            format(nrow(dt_train_short), big.mark = ",")))
cat(sprintf("  Test (%d):     %s rows\n",
            TEST_YEAR,
            format(nrow(dt_test_short), big.mark = ",")))

# ============================================================================
#                              LONG MODEL
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("                              LONG MODEL\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7a: FEATURE SELECTION FOR LONG MODEL =============================

cat("\n=== STEP 7a: FEATURE SELECTION FOR LONG MODEL ===\n")

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

# Additional exclusions: return features (lookahead), session/hour features, and ATR features (for meta-labeling)
# ATR exclusion uses pattern matching below since there are many derived features
excluded_features <- c(
  # Return features (lookahead bias)
  "log_return", "realized_return", "realized_return_adj",
  # Session/hour features (reserved for meta-labeling)
  "hour", "hour_sin", "hour_cos", "hour_open", "hour_high", "hour_low",
  "hour_close", "hour_volume", "hour_close_mean", "hour_close_sd",
  "session_london", "session_ny", "session_asia", "session_overlap"
)

# ATR features (reserved for meta-labeling / risk adjustment) - use pattern matching
atr_features <- names(dt_train_long)[grepl("^atr_|_atr_", names(dt_train_long), ignore.case = TRUE)]
cat(sprintf("ATR features found and excluded: %d\n", length(atr_features)))

# Combine all exclusions
excluded_features <- c(excluded_features, atr_features)

all_feature_cols <- setdiff(names(dt_train_long), c(meta_cols, excluded_features))
cat(sprintf("Total features available: %d\n", length(all_feature_cols)))
cat(sprintf("Excluded features: %d\n", length(intersect(names(dt_train_long), excluded_features))))

# --- STAGE 1: XGBoost Feature Selection (50 features) ---

cat("\n--- STAGE 1: XGBoost Walk-Forward Feature Selection ---\n")
cat("Target: 50 stable features across all windows\n\n")

xgb_feature_importance_list <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d -> Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  # Split data for this window
  dt_wf_train <- dt_train_long[year %in% window$train_years]
  dt_wf_val <- dt_train_long[year == window$val_year]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))
  cat(sprintf("  Val:   %s rows\n", format(nrow(dt_wf_val), big.mark = ",")))

  # Prepare matrices
  X_wf_train <- as.matrix(dt_wf_train[, ..all_feature_cols])
  y_wf_train <- dt_wf_train$label_binary
  w_wf_train <- dt_wf_train$sample_weight

  # Train XGBoost for feature importance
  dtrain_wf <- xgb.DMatrix(data = X_wf_train, label = y_wf_train, weight = w_wf_train)

  params_fs <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  model_wf <- xgb.train(
    params = params_fs,
    data = dtrain_wf,
    nrounds = 100,
    verbose = 0
  )

  # Get feature importance
  importance <- xgb.importance(feature_names = all_feature_cols, model = model_wf)
  xgb_feature_importance_list[[i]] <- importance

  cat(sprintf("  Top 5 features: %s\n", paste(head(importance$Feature, 5), collapse = ", ")))
}

# Find stable features (appear in top 50 across all windows)
cat("\n--- Identifying Stable Features ---\n")

top_features_per_window <- lapply(xgb_feature_importance_list, function(imp) {
  head(imp$Feature, 50)
})

feature_counts <- table(unlist(top_features_per_window))
stable_features_xgb_long <- names(feature_counts[feature_counts == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_xgb_long)))

if (length(stable_features_xgb_long) < 50) {
  cat(sprintf("Taking top 50 by average rank...\n"))

  all_features <- unique(unlist(top_features_per_window))
  avg_ranks <- sapply(all_features, function(f) {
    ranks <- sapply(xgb_feature_importance_list, function(imp) {
      idx <- which(imp$Feature == f)
      if (length(idx) == 0) return(999)
      return(idx)
    })
    mean(ranks)
  })

  stable_features_xgb_long <- names(sort(avg_ranks)[1:min(50, length(avg_ranks))])
}

cat(sprintf("Stage 1 complete: %d features selected\n", length(stable_features_xgb_long)))

# --- STAGE 2: Boruta Feature Selection (15 features) ---

cat("\n--- STAGE 2: Boruta Feature Selection ---\n")
cat("Using full training period (2019-2024) with Stage 1 features\n")

# Prepare data for Boruta
X_boruta <- as.data.frame(dt_train_long[, ..stable_features_xgb_long])
y_boruta <- dt_train_long$label_binary

cat(sprintf("Running Boruta on %d samples with %d features...\n",
            nrow(X_boruta), ncol(X_boruta)))

set.seed(42)
boruta_result <- Boruta(
  x = X_boruta,
  y = as.factor(y_boruta),
  maxRuns = 100,
  doTrace = 0
)

# Get confirmed and tentative features
boruta_decision <- boruta_result$finalDecision
confirmed_features <- names(boruta_decision[boruta_decision == "Confirmed"])
tentative_features <- names(boruta_decision[boruta_decision == "Tentative"])

cat(sprintf("Boruta results: %d confirmed, %d tentative, %d rejected\n",
            length(confirmed_features), length(tentative_features),
            sum(boruta_decision == "Rejected")))

# Take top 15 by importance
boruta_importance <- attStats(boruta_result)
boruta_importance$feature <- rownames(boruta_importance)
boruta_importance <- as.data.table(boruta_importance)
setorder(boruta_importance, -meanImp)

stable_features_long <- head(boruta_importance$feature, 15)

cat(sprintf("\nFinal LONG model features (%d):\n", length(stable_features_long)))
cat(paste(stable_features_long, collapse = "\n"))
cat("\n")

# === SAFETY CHECK: Verify no excluded features made it through ===
cat("\n--- SAFETY CHECK: Verifying no excluded features ---\n")

forbidden_features_found <- intersect(stable_features_long, excluded_features)
if (length(forbidden_features_found) > 0) {
  cat(sprintf("WARNING: Removing %d forbidden features: %s\n",
              length(forbidden_features_found), paste(forbidden_features_found, collapse = ", ")))
  stable_features_long <- setdiff(stable_features_long, excluded_features)
}

# ===== STEP 8a: HYPERPARAMETER GRID SEARCH (LONG) ============================

cat("\n=== STEP 8a: HYPERPARAMETER GRID SEARCH (LONG MODEL) ===\n")

# Define parameter grid
param_grid <- expand.grid(
  max_depth = c(3, 4, 5, 6),
  eta = c(0.01, 0.03, 0.05),
  gamma = c(0, 0.1, 0.3),
  lambda = c(1, 3),
  min_child_weight = c(5, 10, 20),
  stringsAsFactors = FALSE
)

cat(sprintf("Total combinations to test: %d\n", nrow(param_grid)))

# Prepare data for grid search
required_cols_grid <- c("datetime", "year", "label_binary", "sample_weight", stable_features_long)
dt_train_long_grid <- dt_train_long[, ..required_cols_grid]
dt_test_long_grid <- dt_test_long[, ..required_cols_grid]

X_train_grid <- as.matrix(dt_train_long_grid[, ..stable_features_long])
y_train_grid <- dt_train_long_grid$label_binary
w_train_grid <- dt_train_long_grid$sample_weight

X_test_grid <- as.matrix(dt_test_long_grid[, ..stable_features_long])
y_test_grid <- dt_test_long_grid$label_binary

# Calculate scale_pos_weight
n_negative <- sum(y_train_grid == 0)
n_positive <- sum(y_train_grid == 1)
scale_pos_weight <- n_negative / (n_positive + 1e-10)

# Split training data for early stopping
set.seed(42)
val_idx_grid <- sample(1:nrow(X_train_grid), size = floor(0.2 * nrow(X_train_grid)))
train_idx_grid <- setdiff(1:nrow(X_train_grid), val_idx_grid)

X_train_sub_grid <- X_train_grid[train_idx_grid, ]
y_train_sub_grid <- y_train_grid[train_idx_grid]
w_train_sub_grid <- w_train_grid[train_idx_grid]

X_val_grid <- X_train_grid[val_idx_grid, ]
y_val_grid <- y_train_grid[val_idx_grid]
w_val_grid <- w_train_grid[val_idx_grid]

# Grid search
grid_results_long <- data.table()

cat("\nRunning grid search...\n")
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

for (i in 1:nrow(param_grid)) {
  setTxtProgressBar(pb, i)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight
  )

  # Create DMatrix
  dtrain_grid <- xgb.DMatrix(data = X_train_sub_grid, label = y_train_sub_grid, weight = w_train_sub_grid)
  dval_grid <- xgb.DMatrix(data = X_val_grid, label = y_val_grid, weight = w_val_grid)
  dtrain_full_grid <- xgb.DMatrix(data = X_train_grid, label = y_train_grid, weight = w_train_grid)
  dtest_grid <- xgb.DMatrix(data = X_test_grid, label = y_test_grid)

  # Train with early stopping
  model_grid <- tryCatch({
    xgb.train(
      params = params,
      data = dtrain_grid,
      nrounds = 500,
      evals = list(train = dtrain_grid, val = dval_grid),
      early_stopping_rounds = 30,
      verbose = 0
    )
  }, error = function(e) NULL)

  if (is.null(model_grid)) next

  best_iter <- model_grid$best_iteration
  if (is.null(best_iter) || length(best_iter) == 0) best_iter <- 100

  # Retrain on full training data
  model_full <- xgb.train(
    params = params,
    data = dtrain_full_grid,
    nrounds = best_iter,
    verbose = 0
  )

  # Predictions
  pred_train <- predict(model_full, dtrain_full_grid)
  pred_test <- predict(model_full, dtest_grid)

  # Metrics
  train_auc <- tryCatch(auc(roc(y_train_grid, pred_train, quiet = TRUE)), error = function(e) NA)
  test_auc <- tryCatch(auc(roc(y_test_grid, pred_test, quiet = TRUE)), error = function(e) NA)

  # Precision/Recall at 0.5 threshold
  pred_train_class <- as.integer(pred_train > 0.5)
  pred_test_class <- as.integer(pred_test > 0.5)

  train_precision <- sum(pred_train_class == 1 & y_train_grid == 1) / max(1, sum(pred_train_class == 1))
  train_recall <- sum(pred_train_class == 1 & y_train_grid == 1) / max(1, sum(y_train_grid == 1))
  train_f1 <- ifelse(train_precision + train_recall > 0,
                     2 * train_precision * train_recall / (train_precision + train_recall), 0)

  test_precision <- sum(pred_test_class == 1 & y_test_grid == 1) / max(1, sum(pred_test_class == 1))
  test_recall <- sum(pred_test_class == 1 & y_test_grid == 1) / max(1, sum(y_test_grid == 1))
  test_f1 <- ifelse(test_precision + test_recall > 0,
                    2 * test_precision * test_recall / (test_precision + test_recall), 0)

  # Store results
  grid_results_long <- rbind(grid_results_long, data.table(
    combination_id = i,
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    best_iteration = best_iter,
    train_auc = train_auc,
    test_auc = test_auc,
    train_precision = train_precision,
    train_recall = train_recall,
    train_f1 = train_f1,
    test_precision = test_precision,
    test_recall = test_recall,
    test_f1 = test_f1
  ))
}
close(pb)

# Calculate ranks for each metric (lower rank = better)
grid_results_long$rank_train_auc <- rank(-grid_results_long$train_auc, na.last = "keep")
grid_results_long$rank_train_precision <- rank(-grid_results_long$train_precision, na.last = "keep")
grid_results_long$rank_test_auc <- rank(-grid_results_long$test_auc, na.last = "keep")
grid_results_long$rank_test_precision <- rank(-grid_results_long$test_precision, na.last = "keep")

# Calculate average rank (lower is better)
grid_results_long$avg_rank <- rowMeans(grid_results_long[, c("rank_train_auc", "rank_train_precision",
                                                               "rank_test_auc", "rank_test_precision")],
                                        na.rm = TRUE)

fwrite(grid_results_long, file.path(grid_output_path, paste0(EPIC, "_", INTERVAL, "_long_grid_results.csv")))

# Find best parameters (lowest average rank)
best_idx_long <- which.min(grid_results_long$avg_rank)
best_params_long <- grid_results_long[best_idx_long, ]

cat("\n=== BEST PARAMETERS (LONG MODEL) ===\n")
cat(sprintf("Combination ID: %d\n", best_params_long$combination_id))
cat(sprintf("  max_depth:        %d\n", best_params_long$max_depth))
cat(sprintf("  eta:              %.3f\n", best_params_long$eta))
cat(sprintf("  gamma:            %.2f\n", best_params_long$gamma))
cat(sprintf("  lambda:           %.2f\n", best_params_long$lambda))
cat(sprintf("  min_child_weight: %d\n", best_params_long$min_child_weight))
cat(sprintf("  Average Rank:     %.2f (lower is better)\n\n", best_params_long$avg_rank))

cat("Individual Ranks:\n")
cat(sprintf("  Train AUC Rank:       %.0f\n", best_params_long$rank_train_auc))
cat(sprintf("  Train Precision Rank: %.0f\n", best_params_long$rank_train_precision))
cat(sprintf("  Test AUC Rank:        %.0f\n", best_params_long$rank_test_auc))
cat(sprintf("  Test Precision Rank:  %.0f\n\n", best_params_long$rank_test_precision))

cat("Training Performance:\n")
cat(sprintf("  Train AUC:       %.4f\n", best_params_long$train_auc))
cat(sprintf("  Train Precision: %.4f\n", best_params_long$train_precision))
cat(sprintf("  Train Recall:    %.4f\n", best_params_long$train_recall))
cat(sprintf("  Train F1:        %.4f\n\n", best_params_long$train_f1))

cat("Test Performance:\n")
cat(sprintf("  Test AUC:        %.4f\n", best_params_long$test_auc))
cat(sprintf("  Test Precision:  %.4f\n", best_params_long$test_precision))
cat(sprintf("  Test Recall:     %.4f\n", best_params_long$test_recall))
cat(sprintf("  Test F1:         %.4f\n\n", best_params_long$test_f1))

# ===== STEP 9a: TRAIN FINAL LONG MODEL ======================================

cat("\n=== STEP 9a: TRAIN FINAL LONG MODEL (WITH BEST PARAMETERS) ===\n")

# Prepare final datasets with selected features
final_cols <- c("datetime", "year", "label_binary", "sample_weight", stable_features_long)

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

# Calculate scale_pos_weight for class imbalance
n_negative <- sum(y_train == 0)
n_positive <- sum(y_train == 1)
scale_pos_weight <- n_negative / (n_positive + 1e-10)

cat(sprintf("\n  Class balance: Negative=%s, Positive=%s\n",
            format(n_negative, big.mark = ","),
            format(n_positive, big.mark = ",")))
cat(sprintf("  scale_pos_weight: %.2f\n", scale_pos_weight))

# XGBoost parameters with best hyperparameters
params_long <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_long$max_depth,
  eta = best_params_long$eta,
  gamma = best_params_long$gamma,
  lambda = best_params_long$lambda,
  min_child_weight = best_params_long$min_child_weight,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight
)

cat("\nTraining XGBoost LONG model with early stopping...\n")
tic()
model_long <- xgb.train(
  params = params_long,
  data = dtrain,
  nrounds = 1000,
  evals = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 100
)
toc()

# Retrain on full training data with best iteration
best_iter_long <- model_long$best_iteration
if (is.null(best_iter_long) || length(best_iter_long) == 0) {
  best_iter_long <- 200
}
cat(sprintf("\nBest iteration: %d\n", best_iter_long))

dtrain_full <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)

model_long_final <- xgb.train(
  params = params_long,
  data = dtrain_full,
  nrounds = best_iter_long,
  verbose = 0
)

# Save model with feature names
model_long_file <- file.path(models_path, paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".json"))
xgb.save(model_long_final, model_long_file)

# Also save feature names to JSON
model_metadata <- list(
  features = stable_features_long,
  best_iteration = best_iter_long,
  params = params_long
)
writeLines(toJSON(model_metadata, auto_unbox = TRUE, pretty = TRUE),
           gsub("\\.json$", "_metadata.json", model_long_file))

cat(sprintf("LONG model saved: %s\n", model_long_file))

# ===== STEP 10a: EVALUATE LONG MODEL =========================================

cat("\n=== STEP 10a: EVALUATE LONG MODEL ===\n")

# --- Helper function for confusion matrix and metrics ---
print_binary_metrics <- function(y_true, y_pred_prob, threshold = 0.5, set_name = "Unknown") {
  y_pred_class <- as.integer(y_pred_prob > threshold)

  cat(sprintf("\n--- %s SET PERFORMANCE (threshold=%.2f) ---\n", set_name, threshold))

  # Confusion Matrix
  conf_matrix <- table(Predicted = y_pred_class, Actual = y_true)
  print(conf_matrix)

  # Extract values
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[2, 1]
  FN <- conf_matrix[1, 2]

  accuracy <- (TP + TN) / sum(conf_matrix)
  precision <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
  recall <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

  # AUC
  roc_obj <- tryCatch(roc(y_true, y_pred_prob, quiet = TRUE), error = function(e) NULL)
  auc_val <- ifelse(!is.null(roc_obj), auc(roc_obj), NA)

  cat(sprintf("\n  Accuracy:  %.4f\n", accuracy))
  cat(sprintf("  Precision: %.4f\n", precision))
  cat(sprintf("  Recall:    %.4f\n", recall))
  cat(sprintf("  F1-Score:  %.4f\n", f1))
  cat(sprintf("  AUC:       %.4f\n", auc_val))

  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1,
    auc = auc_val,
    conf_matrix = conf_matrix
  ))
}

# Train Set Evaluation
dtrain_full <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
pred_train_long <- predict(model_long_final, dtrain_full)
metrics_train_long <- print_binary_metrics(y_train, pred_train_long, 0.5, "TRAIN (LONG)")

# Test Set Evaluation
pred_test_long <- predict(model_long_final, dtest)
metrics_test_long <- print_binary_metrics(y_test, pred_test_long, 0.5, "TEST (LONG)")

# Feature Importance
cat("\n=== LONG MODEL FEATURE IMPORTANCE ===\n")
importance_long <- xgb.importance(feature_names = stable_features_long, model = model_long_final)
print(importance_long)

# ============================================================================
#                              SHORT MODEL
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("                              SHORT MODEL\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# ===== STEP 7b: FEATURE SELECTION FOR SHORT MODEL ============================

cat("\n=== STEP 7b: FEATURE SELECTION FOR SHORT MODEL ===\n")

# Get all feature columns (same exclusions as LONG)
all_feature_cols_short <- setdiff(names(dt_train_short), c(meta_cols, excluded_features))
cat(sprintf("Total features available: %d\n", length(all_feature_cols_short)))

# --- STAGE 1: XGBoost Feature Selection ---

cat("\n--- STAGE 1: XGBoost Walk-Forward Feature Selection ---\n")

xgb_feature_importance_list_short <- list()

for (i in seq_along(wf_windows)) {
  window <- wf_windows[[i]]
  cat(sprintf("\nWindow %d: Train %d-%d -> Validate %d\n",
              i, min(window$train_years), max(window$train_years), window$val_year))

  dt_wf_train <- dt_train_short[year %in% window$train_years]
  dt_wf_val <- dt_train_short[year == window$val_year]

  cat(sprintf("  Train: %s rows\n", format(nrow(dt_wf_train), big.mark = ",")))
  cat(sprintf("  Val:   %s rows\n", format(nrow(dt_wf_val), big.mark = ",")))

  X_wf_train <- as.matrix(dt_wf_train[, ..all_feature_cols_short])
  y_wf_train <- dt_wf_train$label_binary
  w_wf_train <- dt_wf_train$sample_weight

  dtrain_wf <- xgb.DMatrix(data = X_wf_train, label = y_wf_train, weight = w_wf_train)

  params_fs <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  model_wf <- xgb.train(
    params = params_fs,
    data = dtrain_wf,
    nrounds = 100,
    verbose = 0
  )

  importance <- xgb.importance(feature_names = all_feature_cols_short, model = model_wf)
  xgb_feature_importance_list_short[[i]] <- importance

  cat(sprintf("  Top 5 features: %s\n", paste(head(importance$Feature, 5), collapse = ", ")))
}

# Find stable features
cat("\n--- Identifying Stable Features ---\n")

top_features_per_window_short <- lapply(xgb_feature_importance_list_short, function(imp) {
  head(imp$Feature, 50)
})

feature_counts_short <- table(unlist(top_features_per_window_short))
stable_features_xgb_short <- names(feature_counts_short[feature_counts_short == length(wf_windows)])

cat(sprintf("Stable features (in all %d windows): %d features\n",
            length(wf_windows), length(stable_features_xgb_short)))

if (length(stable_features_xgb_short) < 50) {
  cat(sprintf("Taking top 50 by average rank...\n"))

  all_features_short <- unique(unlist(top_features_per_window_short))
  avg_ranks_short <- sapply(all_features_short, function(f) {
    ranks <- sapply(xgb_feature_importance_list_short, function(imp) {
      idx <- which(imp$Feature == f)
      if (length(idx) == 0) return(999)
      return(idx)
    })
    mean(ranks)
  })

  stable_features_xgb_short <- names(sort(avg_ranks_short)[1:min(50, length(avg_ranks_short))])
}

cat(sprintf("Stage 1 complete: %d features selected\n", length(stable_features_xgb_short)))

# --- STAGE 2: Boruta Feature Selection ---

cat("\n--- STAGE 2: Boruta Feature Selection ---\n")

X_boruta_short <- as.data.frame(dt_train_short[, ..stable_features_xgb_short])
y_boruta_short <- dt_train_short$label_binary

cat(sprintf("Running Boruta on %d samples with %d features...\n",
            nrow(X_boruta_short), ncol(X_boruta_short)))

set.seed(42)
boruta_result_short <- Boruta(
  x = X_boruta_short,
  y = as.factor(y_boruta_short),
  maxRuns = 100,
  doTrace = 0
)

boruta_decision_short <- boruta_result_short$finalDecision
confirmed_features_short <- names(boruta_decision_short[boruta_decision_short == "Confirmed"])
tentative_features_short <- names(boruta_decision_short[boruta_decision_short == "Tentative"])

cat(sprintf("Boruta results: %d confirmed, %d tentative, %d rejected\n",
            length(confirmed_features_short), length(tentative_features_short),
            sum(boruta_decision_short == "Rejected")))

boruta_importance_short <- attStats(boruta_result_short)
boruta_importance_short$feature <- rownames(boruta_importance_short)
boruta_importance_short <- as.data.table(boruta_importance_short)
setorder(boruta_importance_short, -meanImp)

stable_features_short <- head(boruta_importance_short$feature, 15)

cat(sprintf("\nFinal SHORT model features (%d):\n", length(stable_features_short)))
cat(paste(stable_features_short, collapse = "\n"))
cat("\n")

# === SAFETY CHECK: Verify no excluded features made it through ===
cat("\n--- SAFETY CHECK: Verifying no excluded features ---\n")

forbidden_features_found_short <- intersect(stable_features_short, excluded_features)
if (length(forbidden_features_found_short) > 0) {
  cat(sprintf("WARNING: Removing %d forbidden features: %s\n",
              length(forbidden_features_found_short), paste(forbidden_features_found_short, collapse = ", ")))
  stable_features_short <- setdiff(stable_features_short, excluded_features)
}

# ===== STEP 8b: HYPERPARAMETER GRID SEARCH (SHORT) ===========================

cat("\n=== STEP 8b: HYPERPARAMETER GRID SEARCH (SHORT MODEL) ===\n")

# Prepare data for grid search
required_cols_grid_short <- c("datetime", "year", "label_binary", "sample_weight", stable_features_short)
dt_train_short_grid <- dt_train_short[, ..required_cols_grid_short]
dt_test_short_grid <- dt_test_short[, ..required_cols_grid_short]

X_train_grid_short <- as.matrix(dt_train_short_grid[, ..stable_features_short])
y_train_grid_short <- dt_train_short_grid$label_binary
w_train_grid_short <- dt_train_short_grid$sample_weight

X_test_grid_short <- as.matrix(dt_test_short_grid[, ..stable_features_short])
y_test_grid_short <- dt_test_short_grid$label_binary

# Calculate scale_pos_weight
n_negative_short <- sum(y_train_grid_short == 0)
n_positive_short <- sum(y_train_grid_short == 1)
scale_pos_weight_short <- n_negative_short / (n_positive_short + 1e-10)

# Split training data for early stopping
set.seed(42)
val_idx_grid_short <- sample(1:nrow(X_train_grid_short), size = floor(0.2 * nrow(X_train_grid_short)))
train_idx_grid_short <- setdiff(1:nrow(X_train_grid_short), val_idx_grid_short)

X_train_sub_grid_short <- X_train_grid_short[train_idx_grid_short, ]
y_train_sub_grid_short <- y_train_grid_short[train_idx_grid_short]
w_train_sub_grid_short <- w_train_grid_short[train_idx_grid_short]

X_val_grid_short <- X_train_grid_short[val_idx_grid_short, ]
y_val_grid_short <- y_train_grid_short[val_idx_grid_short]
w_val_grid_short <- w_train_grid_short[val_idx_grid_short]

# Grid search
grid_results_short <- data.table()

cat(sprintf("Total combinations to test: %d\n", nrow(param_grid)))
cat("\nRunning grid search...\n")
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

for (i in 1:nrow(param_grid)) {
  setTxtProgressBar(pb, i)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight_short
  )

  dtrain_grid_short <- xgb.DMatrix(data = X_train_sub_grid_short, label = y_train_sub_grid_short, weight = w_train_sub_grid_short)
  dval_grid_short <- xgb.DMatrix(data = X_val_grid_short, label = y_val_grid_short, weight = w_val_grid_short)
  dtrain_full_grid_short <- xgb.DMatrix(data = X_train_grid_short, label = y_train_grid_short, weight = w_train_grid_short)
  dtest_grid_short <- xgb.DMatrix(data = X_test_grid_short, label = y_test_grid_short)

  model_grid <- tryCatch({
    xgb.train(
      params = params,
      data = dtrain_grid_short,
      nrounds = 500,
      evals = list(train = dtrain_grid_short, val = dval_grid_short),
      early_stopping_rounds = 30,
      verbose = 0
    )
  }, error = function(e) NULL)

  if (is.null(model_grid)) next

  best_iter <- model_grid$best_iteration
  if (is.null(best_iter) || length(best_iter) == 0) best_iter <- 100

  model_full <- xgb.train(
    params = params,
    data = dtrain_full_grid_short,
    nrounds = best_iter,
    verbose = 0
  )

  pred_train <- predict(model_full, dtrain_full_grid_short)
  pred_test <- predict(model_full, dtest_grid_short)

  train_auc <- tryCatch(auc(roc(y_train_grid_short, pred_train, quiet = TRUE)), error = function(e) NA)
  test_auc <- tryCatch(auc(roc(y_test_grid_short, pred_test, quiet = TRUE)), error = function(e) NA)

  pred_train_class <- as.integer(pred_train > 0.5)
  pred_test_class <- as.integer(pred_test > 0.5)

  train_precision <- sum(pred_train_class == 1 & y_train_grid_short == 1) / max(1, sum(pred_train_class == 1))
  train_recall <- sum(pred_train_class == 1 & y_train_grid_short == 1) / max(1, sum(y_train_grid_short == 1))
  train_f1 <- ifelse(train_precision + train_recall > 0,
                     2 * train_precision * train_recall / (train_precision + train_recall), 0)

  test_precision <- sum(pred_test_class == 1 & y_test_grid_short == 1) / max(1, sum(pred_test_class == 1))
  test_recall <- sum(pred_test_class == 1 & y_test_grid_short == 1) / max(1, sum(y_test_grid_short == 1))
  test_f1 <- ifelse(test_precision + test_recall > 0,
                    2 * test_precision * test_recall / (test_precision + test_recall), 0)

  grid_results_short <- rbind(grid_results_short, data.table(
    combination_id = i,
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    gamma = param_grid$gamma[i],
    lambda = param_grid$lambda[i],
    min_child_weight = param_grid$min_child_weight[i],
    best_iteration = best_iter,
    train_auc = train_auc,
    test_auc = test_auc,
    train_precision = train_precision,
    train_recall = train_recall,
    train_f1 = train_f1,
    test_precision = test_precision,
    test_recall = test_recall,
    test_f1 = test_f1
  ))
}
close(pb)

# Calculate ranks
grid_results_short$rank_train_auc <- rank(-grid_results_short$train_auc, na.last = "keep")
grid_results_short$rank_train_precision <- rank(-grid_results_short$train_precision, na.last = "keep")
grid_results_short$rank_test_auc <- rank(-grid_results_short$test_auc, na.last = "keep")
grid_results_short$rank_test_precision <- rank(-grid_results_short$test_precision, na.last = "keep")

grid_results_short$avg_rank <- rowMeans(grid_results_short[, c("rank_train_auc", "rank_train_precision",
                                                                 "rank_test_auc", "rank_test_precision")],
                                         na.rm = TRUE)

# Save results
fwrite(grid_results_short, file.path(grid_output_path, paste0(EPIC, "_", INTERVAL, "_short_grid_results.csv")))

# Find best parameters
best_idx_short <- which.min(grid_results_short$avg_rank)
best_params_short <- grid_results_short[best_idx_short, ]

cat("\n=== BEST PARAMETERS (SHORT MODEL) ===\n")
cat(sprintf("Combination ID: %d\n", best_params_short$combination_id))
cat(sprintf("  max_depth:        %d\n", best_params_short$max_depth))
cat(sprintf("  eta:              %.3f\n", best_params_short$eta))
cat(sprintf("  gamma:            %.2f\n", best_params_short$gamma))
cat(sprintf("  lambda:           %.2f\n", best_params_short$lambda))
cat(sprintf("  min_child_weight: %d\n", best_params_short$min_child_weight))
cat(sprintf("  Average Rank:     %.2f (lower is better)\n\n", best_params_short$avg_rank))

cat("Individual Ranks:\n")
cat(sprintf("  Train AUC Rank:       %.0f\n", best_params_short$rank_train_auc))
cat(sprintf("  Train Precision Rank: %.0f\n", best_params_short$rank_train_precision))
cat(sprintf("  Test AUC Rank:        %.0f\n", best_params_short$rank_test_auc))
cat(sprintf("  Test Precision Rank:  %.0f\n\n", best_params_short$rank_test_precision))

cat("Training Performance:\n")
cat(sprintf("  Train AUC:       %.4f\n", best_params_short$train_auc))
cat(sprintf("  Train Precision: %.4f\n", best_params_short$train_precision))
cat(sprintf("  Train Recall:    %.4f\n", best_params_short$train_recall))
cat(sprintf("  Train F1:        %.4f\n\n", best_params_short$train_f1))

cat("Test Performance:\n")
cat(sprintf("  Test AUC:        %.4f\n", best_params_short$test_auc))
cat(sprintf("  Test Precision:  %.4f\n", best_params_short$test_precision))
cat(sprintf("  Test Recall:     %.4f\n", best_params_short$test_recall))
cat(sprintf("  Test F1:         %.4f\n\n", best_params_short$test_f1))

# ===== STEP 9b: TRAIN FINAL SHORT MODEL =====================================

cat("\n=== STEP 9b: TRAIN FINAL SHORT MODEL (WITH BEST PARAMETERS) ===\n")

final_cols_short <- c("datetime", "year", "label_binary", "sample_weight", stable_features_short)

dt_train_short_final <- dt_train_short[, ..final_cols_short]
dt_test_short_final <- dt_test_short[, ..final_cols_short]

cat(sprintf("Train set: %s rows, %d features\n",
            format(nrow(dt_train_short_final), big.mark = ","),
            length(stable_features_short)))
cat(sprintf("Test set:  %s rows, %d features\n",
            format(nrow(dt_test_short_final), big.mark = ","),
            length(stable_features_short)))

X_train_short <- as.matrix(dt_train_short_final[, ..stable_features_short])
y_train_short <- dt_train_short_final$label_binary
w_train_short <- dt_train_short_final$sample_weight

X_test_short <- as.matrix(dt_test_short_final[, ..stable_features_short])
y_test_short <- dt_test_short_final$label_binary

set.seed(42)
val_idx_short <- sample(1:nrow(X_train_short), size = floor(0.2 * nrow(X_train_short)))
train_idx_short <- setdiff(1:nrow(X_train_short), val_idx_short)

X_train_sub_short <- X_train_short[train_idx_short, ]
y_train_sub_short <- y_train_short[train_idx_short]
w_train_sub_short <- w_train_short[train_idx_short]

X_val_short <- X_train_short[val_idx_short, ]
y_val_short <- y_train_short[val_idx_short]
w_val_short <- w_train_short[val_idx_short]

dtrain_short <- xgb.DMatrix(data = X_train_sub_short, label = y_train_sub_short, weight = w_train_sub_short)
dval_short <- xgb.DMatrix(data = X_val_short, label = y_val_short, weight = w_val_short)
dtest_short <- xgb.DMatrix(data = X_test_short, label = y_test_short)

n_negative_short <- sum(y_train_short == 0)
n_positive_short <- sum(y_train_short == 1)
scale_pos_weight_short <- n_negative_short / (n_positive_short + 1e-10)

cat(sprintf("\n  Class balance: Negative=%s, Positive=%s\n",
            format(n_negative_short, big.mark = ","),
            format(n_positive_short, big.mark = ",")))
cat(sprintf("  scale_pos_weight: %.2f\n", scale_pos_weight_short))

params_short <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params_short$max_depth,
  eta = best_params_short$eta,
  gamma = best_params_short$gamma,
  lambda = best_params_short$lambda,
  min_child_weight = best_params_short$min_child_weight,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight_short
)

cat("\nTraining XGBoost SHORT model with early stopping...\n")
tic()
model_short <- xgb.train(
  params = params_short,
  data = dtrain_short,
  nrounds = 1000,
  evals = list(train = dtrain_short, val = dval_short),
  early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 100
)
toc()

best_iter_short <- model_short$best_iteration
if (is.null(best_iter_short) || length(best_iter_short) == 0) {
  best_iter_short <- 200
}
cat(sprintf("\nBest iteration: %d\n", best_iter_short))

dtrain_short_full <- xgb.DMatrix(data = X_train_short, label = y_train_short, weight = w_train_short)

model_short_final <- xgb.train(
  params = params_short,
  data = dtrain_short_full,
  nrounds = best_iter_short,
  verbose = 0
)

# Save model
model_short_file <- file.path(models_path, paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".json"))
xgb.save(model_short_final, model_short_file)

model_metadata_short <- list(
  features = stable_features_short,
  best_iteration = best_iter_short,
  params = params_short
)
writeLines(toJSON(model_metadata_short, auto_unbox = TRUE, pretty = TRUE),
           gsub("\\.json$", "_metadata.json", model_short_file))

cat(sprintf("SHORT model saved: %s\n", model_short_file))

# ===== STEP 10b: EVALUATE SHORT MODEL ========================================

cat("\n=== STEP 10b: EVALUATE SHORT MODEL ===\n")

# Train Set Evaluation
pred_train_short <- predict(model_short_final, dtrain_short_full)
metrics_train_short <- print_binary_metrics(y_train_short, pred_train_short, 0.5, "TRAIN (SHORT)")

# Test Set Evaluation
pred_test_short <- predict(model_short_final, dtest_short)
metrics_test_short <- print_binary_metrics(y_test_short, pred_test_short, 0.5, "TEST (SHORT)")

# Feature Importance
cat("\n=== SHORT MODEL FEATURE IMPORTANCE ===\n")
importance_short <- xgb.importance(feature_names = stable_features_short, model = model_short_final)
print(importance_short)

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("                           FINAL SUMMARY\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("LONG MODEL:\n")
cat(sprintf("  Features: %d\n", length(stable_features_long)))
cat(sprintf("  Train AUC: %.4f | Test AUC: %.4f\n", metrics_train_long$auc, metrics_test_long$auc))
cat(sprintf("  Train F1:  %.4f | Test F1:  %.4f\n", metrics_train_long$f1_score, metrics_test_long$f1_score))
cat(sprintf("  Model: %s\n\n", model_long_file))

cat("SHORT MODEL:\n")
cat(sprintf("  Features: %d\n", length(stable_features_short)))
cat(sprintf("  Train AUC: %.4f | Test AUC: %.4f\n", metrics_train_short$auc, metrics_test_short$auc))
cat(sprintf("  Train F1:  %.4f | Test F1:  %.4f\n", metrics_train_short$f1_score, metrics_test_short$f1_score))
cat(sprintf("  Model: %s\n\n", model_short_file))

cat("=== PIPELINE COMPLETE ===\n")
