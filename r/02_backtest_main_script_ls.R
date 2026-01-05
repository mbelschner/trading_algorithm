# Script for running Machine Learning Backtest Pipeline with Long/Short Split
# Splits labels into separate Long (label=1) and Short (label=-1) models
# Uses purged k-fold cross validation, feature importance (MDI/MDA/SFI),
# and tree-based models on triple barrier and meta-labels

cat("\n=== START ML BACKTEST PIPELINE (LONG/SHORT SPLIT) ===\n")

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(
  data.table,      # Fast data manipulation
  TTR,             # Technical indicators
  zoo,             # Time series (für rollapply)
  ggplot2,         # Visualization
  progress,        # Progress bars
  tictoc,          # Timing
  parallel,        # Parallelization
  doParallel,      # Parallel backend
  foreach,         # Parallel loops
  xgboost,         # Gradient boosting
  ranger,          # Random Forest
  caret,           # ML framework
  Metrics          # Evaluation metrics
)

# ===== Paths ==================================================================

labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")

# Erstelle Output-Ordner falls nicht vorhanden
if (!dir.exists(backtest_output_path)) {
  dir.create(backtest_output_path, recursive = TRUE)
}

# ===== Configuration ==========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"

# Lade gelabelte Daten (erstellt von 01_labelling_main_script.R)
# Option 1: Triple Barrier Labels
triple_barrier_file <- file.path(
  labelled_data_path,
  paste0(EPIC, "_", INTERVAL, "_labeled.csv")
)

# Option 2: Meta Labels
meta_labels_file <- file.path(
  labelled_data_path,
  paste0(EPIC, "_", INTERVAL, "_meta_labeled.csv")
)

# Option 3: Combined Labels
combined_labels_file <- file.path(
  labelled_data_path,
  paste0(EPIC, "_", INTERVAL, "_combined_labels.csv")
)

cat("\n=== LADE GELABELTE DATEN ===\n")

# Für diesen Test starten wir mit Triple Barrier Labels
dt <- fread(triple_barrier_file)
setDT(dt)  # Stelle sicher dass dt als data.table erkannt wird

cat(sprintf("Geladene Zeilen: %s\n", format(nrow(dt), big.mark = ",")))
cat(sprintf("Zeitraum: %s bis %s\n", min(dt$datetime), max(dt$datetime)))
cat(sprintf("Spalten: %s\n", paste(head(names(dt), 10), collapse = ", ")))

# Für schnellere Tests: Reduziere Datensatz (nur 2024-2025)
USE_SMALL_DATASET <- FALSE  # Auf FALSE setzen für vollständige Analyse

if (USE_SMALL_DATASET) {
  dt_small <- dt[datetime >= "2024-01-01" & datetime <= "2025-12-31"]

  cat(sprintf("\n=== VERWENDE REDUZIERTES DATASET FÜR TESTS ===\n"))
  cat(sprintf("Original: %s Zeilen\n", format(nrow(dt), big.mark = ",")))
  cat(sprintf("Reduziert: %s Zeilen\n", format(nrow(dt_small), big.mark = ",")))
  cat(sprintf("Faktor: %.1fx schneller\n\n", nrow(dt) / nrow(dt_small)))

  dt <- dt_small
  rm(dt_small)
}

# ===== Load Pipeline Modules ==================================================

cat("\n=== LADE PIPELINE MODULE ===\n")

source("r/02_01_indicator_calculation.R")
cat("✓ Indicator Calculation geladen\n")

source("r/02_02_feature_engineering.R")
cat("✓ Feature Engineering geladen\n")

source("r/02_03_feature_selection.R")
cat("✓ Feature Selection geladen\n")

source("r/02_04_purged_kfold_cv.R")
cat("✓ Purged K-Fold CV geladen\n")

source("r/02_05_model_training.R")
cat("✓ Model Training (MDI/MDA/SFI) geladen\n")

source("r/02_06_backtest_evaluation.R")
cat("✓ Backtest Evaluation geladen\n")

# ===== Step 1: Calculate Technical Indicators ================================

cat("\n=== STEP 1: BERECHNE TECHNISCHE INDIKATOREN ===\n")

tic()
dt_indicators <- calculate_all_indicators(
  dt = dt,
  # Periods für multi-timeframe Indikatoren
  ema_periods = c(9, 21, 50, 100),
  rsi_periods = c(14, 28),
  atr_periods = c(14, 28),
  adx_periods = c(14),
  bb_periods = c(20),
  kc_periods = c(20),
  # Weitere Parameter siehe 02_01_indicator_calculation.R
  verbose = TRUE
)
toc()

cat(sprintf("Features nach Indikator-Berechnung: %d\n", ncol(dt_indicators)))

# ===== Step 2: Feature Engineering (Lags, Derivatives) =======================

cat("\n=== STEP 2: FEATURE ENGINEERING ===\n")

tic()
dt_features <- engineer_features(
  dt = dt_indicators,
  lag_periods = c(1, 2, 3, 5, 10),           # Lags
  derivative_orders = c(1, 2),                # 1. und 2. Ableitung
  hourly_aggregation = TRUE,                  # 1h Aggregate
  rolling_windows = c(10, 20, 50),           # Rolling Stats
  interaction_features = FALSE,               # Erst nach Feature Selection
  verbose = TRUE
)
toc()

cat(sprintf("Features nach Engineering: %d\n", ncol(dt_features)))

# Entferne Zeilen mit NA (durch Lags/Rolling Windows entstanden)
n_before <- nrow(dt_features)
dt_features <- na.omit(dt_features)
n_after <- nrow(dt_features)
cat(sprintf("Zeilen nach NA-Entfernung: %s (-%s)\n",
            format(n_after, big.mark = ","),
            format(n_before - n_after, big.mark = ",")))

# ===== LONG/SHORT SPLIT =======================================================

cat("\n=== SPLIT IN LONG UND SHORT DATASETS ===\n")

# Original Label Distribution
cat("\nOriginal Label Distribution:\n")
print(table(dt_features$label))

# Erstelle Long Dataset: label=1 vs. label=0 (binary classification)
dt_long <- copy(dt_features)
dt_long[, label_binary := fifelse(label == 1, 1, 0)]  # Long=1, Rest=0
n_long_positive <- sum(dt_long$label_binary == 1)
n_long_total <- nrow(dt_long)

cat(sprintf("\nLONG Dataset erstellt:\n"))
cat(sprintf("  Positive (Long): %s (%.1f%%)\n",
            format(n_long_positive, big.mark = ","),
            100 * n_long_positive / n_long_total))
cat(sprintf("  Negative (Rest): %s (%.1f%%)\n",
            format(n_long_total - n_long_positive, big.mark = ","),
            100 * (n_long_total - n_long_positive) / n_long_total))

# Erstelle Short Dataset: label=-1 vs. label=0 (binary classification)
dt_short <- copy(dt_features)
dt_short[, label_binary := fifelse(label == -1, 1, 0)]  # Short=1, Rest=0
n_short_positive <- sum(dt_short$label_binary == 1)
n_short_total <- nrow(dt_short)

cat(sprintf("\nSHORT Dataset erstellt:\n"))
cat(sprintf("  Positive (Short): %s (%.1f%%)\n",
            format(n_short_positive, big.mark = ","),
            100 * n_short_positive / n_short_total))
cat(sprintf("  Negative (Rest): %s (%.1f%%)\n",
            format(n_short_total - n_short_positive, big.mark = ","),
            100 * (n_short_total - n_short_positive) / n_short_total))

# ===== LONG MODEL PIPELINE ====================================================

cat("\n\n")
cat("================================================================================\n")
cat("===                        LONG MODEL PIPELINE                              ===\n")
cat("================================================================================\n")

# ----- Feature Selection (Long) -----

cat("\n=== STEP 3a: FEATURE SELECTION FÜR LONG MODEL ===\n")

target_col <- "label_binary"
weight_col <- "sample_weight"

# Prüfe ob sample_weight vorhanden ist
use_sample_weights <- weight_col %in% names(dt_long)

if (!use_sample_weights) {
  cat("WARNUNG: Keine sample_weights gefunden. Verwende gleichgewichtete Samples.\n")
  dt_long[, sample_weight := 1.0]
}

# ===== STUFE 1: XGBoost (schnell) - ~380 Features → 50 Features =====
cat("\n--- STUFE 1: XGBoost Feature Selection (schnell) ---\n")

tic()
feature_selection_stage1_long <- select_important_features(
  dt = dt_long,
  target_col = target_col,
  weight_col = weight_col,
  method = "xgboost",           # Schnell und effizient
  n_top_features = 50,          # Reduziere auf 50
  cv_folds = 5,                 # 5-fold für Stabilität
  verbose = TRUE
)
toc()

top_features_stage1_long <- feature_selection_stage1_long$top_features
cat(sprintf("\n✓ Stufe 1 abgeschlossen: %d Features reduziert auf %d Features\n",
            length(feature_selection_stage1_long$importance$feature),
            length(top_features_stage1_long)))

# Erstelle reduzierten Datensatz für Stufe 2
required_cols_temp <- c("datetime", "label", target_col, weight_col,
                        "barrier_touched", "bars_to_exit", "realized_return")
dt_long_stage1 <- dt_long[, c(required_cols_temp, top_features_stage1_long), with = FALSE]

# ===== STUFE 2: Boruta (gründlich) - 50 Features → 10 Features =====
cat("\n--- STUFE 2: Boruta Feature Selection (gründlich) ---\n")
cat("HINWEIS: Boruta ist langsam, aber findet die wirklich relevanten Features!\n\n")

tic()
feature_selection_stage2_long <- select_important_features(
  dt = dt_long_stage1,
  target_col = target_col,
  weight_col = weight_col,
  method = "boruta",            # Gründlich und präzise
  n_top_features = 12,          # Finale Top 12
  cv_folds = 1,                 # Boruta braucht kein CV
  verbose = TRUE
)
toc()

top_features_long <- feature_selection_stage2_long$top_features
feature_importance_long <- feature_selection_stage2_long$importance

cat(sprintf("\n✓ Stufe 2 abgeschlossen: %d Features → %d Features\n",
            length(top_features_stage1_long), length(top_features_long)))

cat(sprintf("\n=== FINALE TOP  FEATURES FÜR LONG MODEL ===\n"))
print(head(feature_importance_long, 20))

# Erstelle finalen reduzierten Long-Datensatz
required_cols <- c("datetime", "label", target_col, weight_col,
                   "barrier_touched", "bars_to_exit", "realized_return")
feature_cols_long <- top_features_long

dt_long_reduced <- dt_long[, c(required_cols, feature_cols_long), with = FALSE]

cat(sprintf("\nReduzierter Long-Datensatz: %d Features + %d Meta-Spalten\n",
            length(feature_cols_long), length(required_cols)))

# ----- Purged K-Fold CV (Long) -----

cat("\n=== STEP 4a: PURGED K-FOLD CV FÜR LONG MODEL ===\n")

cv_splits_long <- create_purged_kfold_splits(
  dt = dt_long_reduced,
  n_splits = 5,
  pct_embargo = 0.01,
  verbose = TRUE
)

cat(sprintf("CV Splits erstellt: %d Folds\n", length(cv_splits_long)))

# ----- Model Training (Long) -----

cat("\n=== STEP 5a: MODEL TRAINING FÜR LONG MODEL ===\n")

hyperparameter_grid <- list(
  xgb = expand.grid(
    max_depth = c(6, 8, 10),
    eta = c(0.01, 0.05, 0.1),
    subsample = c(0.8, 1.0),
    colsample_bytree = c(0.6, 0.8),
    min_child_weight = c(1, 3),
    stringsAsFactors = FALSE
  ),
  rf = expand.grid(
    num.trees = c(500, 1000),
    mtry = c(3, 5, 7),
    max.depth = c(10, 15, 20),
    min.node.size = c(5, 10),
    stringsAsFactors = FALSE
  )
)

tic()
model_results_long <- train_and_evaluate_models(
  dt = dt_long_reduced,
  cv_splits = cv_splits_long,
  target_col = target_col,
  weight_col = weight_col,
  feature_cols = feature_cols_long,
  hyperparam_grid = hyperparameter_grid,
  models = "xgboost",  # Nur xgboost für Long Model
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)
toc()

cat("\n=== BESTE LONG MODELLE (CV Performance) ===\n")
print(model_results_long$best_models)

# ----- Test Set Evaluation (Long) -----

cat("\n=== STEP 5b: TEST SET EVALUATION FÜR LONG MODEL ===\n")

# Verwende letzten Fold als Test Set (Out-of-Time Test)
test_fold_idx <- length(cv_splits_long)
test_indices <- cv_splits_long[[test_fold_idx]]$test_idx
train_indices <- cv_splits_long[[test_fold_idx]]$train_idx

cat(sprintf("Train Set: %d Samples\n", length(train_indices)))
cat(sprintf("Test Set: %d Samples (Out-of-Time)\n", length(test_indices)))

# Trainiere finales Modell auf allen Train-Daten
X_train <- as.matrix(dt_long_reduced[train_indices, ..feature_cols_long])
y_train <- dt_long_reduced[train_indices][[target_col]]
w_train <- dt_long_reduced[train_indices][[weight_col]]

X_test <- as.matrix(dt_long_reduced[test_indices, ..feature_cols_long])
y_test <- dt_long_reduced[test_indices][[target_col]]

# XGBoost DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Trainiere mit besten Hyperparametern
best_params <- model_results_long$best_hyperparams
params_list <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight
)

final_model_long <- xgb.train(
  params = params_list,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)

# Predictions
y_pred_prob <- predict(final_model_long, dtest)
y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)

# Evaluation Metrics
cat("\n=== TEST SET PERFORMANCE (LONG MODEL) ===\n")

# Confusion Matrix
conf_matrix <- table(Predicted = y_pred_class, Actual = y_test)
print(conf_matrix)

# Metriken
accuracy <- sum(y_pred_class == y_test) / length(y_test)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# AUC
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(y_test, y_pred_prob, quiet = TRUE)
  auc_score <- pROC::auc(roc_obj)
} else {
  auc_score <- NA
}

cat(sprintf("\nAccuracy:  %.4f\n", accuracy))
cat(sprintf("Precision: %.4f (von allen vorhergesagten Longs, wie viele waren korrekt?)\n", precision))
cat(sprintf("Recall:    %.4f (von allen tatsächlichen Longs, wie viele haben wir gefunden?)\n", recall))
cat(sprintf("F1-Score:  %.4f (harmonisches Mittel von Precision & Recall)\n", f1_score))
if (!is.na(auc_score)) {
  cat(sprintf("AUC:       %.4f (0.5=Random, 1.0=Perfekt)\n", auc_score))
}

# Baseline Comparison
baseline_accuracy <- max(table(y_test)) / length(y_test)
cat(sprintf("\nBaseline (immer häufigste Klasse): %.4f\n", baseline_accuracy))
cat(sprintf("Improvement over Baseline: %.2f%%\n",
            100 * (accuracy - baseline_accuracy) / baseline_accuracy))

# ----- Feature Importance (Long Model) -----

cat("\n=== FEATURE IMPORTANCE (LONG MODEL) ===\n")

# Extrahiere Feature Importance aus finalem Modell
importance_matrix <- xgb.importance(
  feature_names = feature_cols_long,
  model = final_model_long
)

cat(sprintf("\nTop 10 wichtigste Features:\n"))
print(head(importance_matrix, 10))

# Plot Feature Importance
if (interactive()) {
  xgb.plot.importance(importance_matrix, top_n = 15)
}

# Zeige alle Features die im Modell verwendet werden
cat(sprintf("\n=== ALLE FEATURES IM LONG MODEL (%d Features) ===\n", length(feature_cols_long)))
cat(paste(feature_cols_long, collapse = "\n"))
cat("\n")


# ===== SHORT MODEL PIPELINE ===================================================

cat("\n\n")
cat("================================================================================\n")
cat("===                        SHORT MODEL PIPELINE                             ===\n")
cat("================================================================================\n")

# ----- Feature Selection (Short) -----

cat("\n=== STEP 3b: FEATURE SELECTION FÜR SHORT MODEL ===\n")

if (!use_sample_weights) {
  dt_short[, sample_weight := 1.0]
}

# ===== STUFE 1: XGBoost (schnell) - ~380 Features → 50 Features =====
cat("\n--- STUFE 1: XGBoost Feature Selection (schnell) ---\n")

tic()
feature_selection_stage1_short <- select_important_features(
  dt = dt_short,
  target_col = target_col,
  weight_col = weight_col,
  method = "xgboost",
  n_top_features = 50,
  cv_folds = 5,
  verbose = TRUE
)
toc()

top_features_stage1_short <- feature_selection_stage1_short$top_features
cat(sprintf("\n✓ Stufe 1 abgeschlossen: %d Features reduziert auf %d Features\n",
            length(feature_selection_stage1_short$importance$feature),
            length(top_features_stage1_short)))

# Erstelle reduzierten Datensatz für Stufe 2
dt_short_stage1 <- dt_short[, c(required_cols_temp, top_features_stage1_short), with = FALSE]

# ===== STUFE 2: Boruta (gründlich) - 50 Features → 10 Features =====
cat("\n--- STUFE 2: Boruta Feature Selection (gründlich) ---\n")
cat("HINWEIS: Boruta ist langsam, aber findet die wirklich relevanten Features!\n\n")

tic()
feature_selection_stage2_short <- select_important_features(
  dt = dt_short_stage1,
  target_col = target_col,
  weight_col = weight_col,
  method = "boruta",
  n_top_features = 12,
  cv_folds = 1,
  verbose = TRUE
)
toc()

top_features_short <- feature_selection_stage2_short$top_features
feature_importance_short <- feature_selection_stage2_short$importance

cat(sprintf("\n✓ Stufe 2 abgeschlossen: %d Features → %d Features\n",
            length(top_features_stage1_short), length(top_features_short)))

cat(sprintf("\n=== FINALE TOP FEATURES FÜR SHORT MODEL ===\n"))
print(head(feature_importance_short, 20))

# Erstelle finalen reduzierten Short-Datensatz
feature_cols_short <- top_features_short

dt_short_reduced <- dt_short[, c(required_cols, feature_cols_short), with = FALSE]

cat(sprintf("\nReduzierter Short-Datensatz: %d Features + %d Meta-Spalten\n",
            length(feature_cols_short), length(required_cols)))

# ----- Purged K-Fold CV (Short) -----

cat("\n=== STEP 4b: PURGED K-FOLD CV FÜR SHORT MODEL ===\n")

cv_splits_short <- create_purged_kfold_splits(
  dt = dt_short_reduced,
  n_splits = 5,
  pct_embargo = 0.01,
  verbose = TRUE
)

cat(sprintf("CV Splits erstellt: %d Folds\n", length(cv_splits_short)))

# ----- Model Training (Short) -----

cat("\n=== STEP 5b: MODEL TRAINING FÜR SHORT MODEL ===\n")

tic()
model_results_short <- train_and_evaluate_models(
  dt = dt_short_reduced,
  cv_splits = cv_splits_short,
  target_col = target_col,
  weight_col = weight_col,
  feature_cols = feature_cols_short,
  hyperparam_grid = hyperparameter_grid,
  models ="xgboost",,
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)
toc()

cat("\n=== BESTE SHORT MODELLE (CV Performance) ===\n")
print(model_results_short$best_models)

# ----- Test Set Evaluation (Short) -----

cat("\n=== STEP 5c: TEST SET EVALUATION FÜR SHORT MODEL ===\n")

# Verwende letzten Fold als Test Set
test_fold_idx <- length(cv_splits_short)
test_indices <- cv_splits_short[[test_fold_idx]]$test_idx
train_indices <- cv_splits_short[[test_fold_idx]]$train_idx

cat(sprintf("Train Set: %d Samples\n", length(train_indices)))
cat(sprintf("Test Set: %d Samples (Out-of-Time)\n", length(test_indices)))

# Trainiere finales Modell
X_train <- as.matrix(dt_short_reduced[train_indices, ..feature_cols_short])
y_train <- dt_short_reduced[train_indices][[target_col]]
w_train <- dt_short_reduced[train_indices][[weight_col]]

X_test <- as.matrix(dt_short_reduced[test_indices, ..feature_cols_short])
y_test <- dt_short_reduced[test_indices][[target_col]]

dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

best_params <- model_results_short$best_hyperparams
params_list <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight
)

final_model_short <- xgb.train(
  params = params_list,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)

# Predictions
y_pred_prob <- predict(final_model_short, dtest)
y_pred_class <- ifelse(y_pred_prob > 0.5, 1, 0)

# Evaluation
cat("\n=== TEST SET PERFORMANCE (SHORT MODEL) ===\n")

conf_matrix <- table(Predicted = y_pred_class, Actual = y_test)
print(conf_matrix)

accuracy <- sum(y_pred_class == y_test) / length(y_test)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(y_test, y_pred_prob, quiet = TRUE)
  auc_score <- pROC::auc(roc_obj)
} else {
  auc_score <- NA
}

cat(sprintf("\nAccuracy:  %.4f\n", accuracy))
cat(sprintf("Precision: %.4f (von allen vorhergesagten Shorts, wie viele waren korrekt?)\n", precision))
cat(sprintf("Recall:    %.4f (von allen tatsächlichen Shorts, wie viele haben wir gefunden?)\n", recall))
cat(sprintf("F1-Score:  %.4f\n", f1_score))
if (!is.na(auc_score)) {
  cat(sprintf("AUC:       %.4f\n", auc_score))
}

baseline_accuracy <- max(table(y_test)) / length(y_test)
cat(sprintf("\nBaseline: %.4f\n", baseline_accuracy))
cat(sprintf("Improvement: %.2f%%\n",
            100 * (accuracy - baseline_accuracy) / baseline_accuracy))

# ----- Feature Importance (Short Model) -----

cat("\n=== FEATURE IMPORTANCE (SHORT MODEL) ===\n")

# Extrahiere Feature Importance
importance_matrix_short <- xgb.importance(
  feature_names = feature_cols_short,
  model = final_model_short
)

cat(sprintf("\nTop 10 wichtigste Features:\n"))
print(head(importance_matrix_short, 10))

# Plot
if (interactive()) {
  xgb.plot.importance(importance_matrix_short, top_n = 15)
}

# Alle Features
cat(sprintf("\n=== ALLE FEATURES IM SHORT MODEL (%d Features) ===\n", length(feature_cols_short)))
cat(paste(feature_cols_short, collapse = "\n"))
cat("\n")

# ===== COMBINED EVALUATION ====================================================

cat("\n\n")
cat("================================================================================\n")
cat("===                    COMBINED LONG/SHORT EVALUATION                       ===\n")
cat("================================================================================\n")

cat("\n=== STEP 6: KOMBINIERE LONG & SHORT PREDICTIONS ===\n")

# Hier würden wir die Predictions der beiden Modelle kombinieren
# und eine finale Trading Strategy evaluieren

cat("\nLONG Model Performance:\n")
cat(sprintf("  Best Model: %s\n", model_results_long$best_model_type))
cat(sprintf("  CV Score: %.4f\n", model_results_long$best_cv_score))

cat("\nSHORT Model Performance:\n")
cat(sprintf("  Best Model: %s\n", model_results_short$best_model_type))
cat(sprintf("  CV Score: %.4f\n", model_results_short$best_cv_score))

# ===== Save Results ===========================================================

cat("\n=== STEP 7: SPEICHERE ERGEBNISSE ===\n")

# Speichere Long Model
model_long_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_long.rds")
)
saveRDS(model_results_long$best_model, model_long_file)
cat(sprintf("Long Model gespeichert: %s\n", model_long_file))

# Speichere Short Model
model_short_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_model_short.rds")
)
saveRDS(model_results_short$best_model, model_short_file)
cat(sprintf("Short Model gespeichert: %s\n", model_short_file))

# Speichere Feature Importance
importance_long_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_importance_long.csv")
)
fwrite(feature_importance_long, importance_long_file)
cat(sprintf("Long Feature Importance gespeichert: %s\n", importance_long_file))

importance_short_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_importance_short.csv")
)
fwrite(feature_importance_short, importance_short_file)
cat(sprintf("Short Feature Importance gespeichert: %s\n", importance_short_file))

cat("\n=== END ML BACKTEST PIPELINE (LONG/SHORT SPLIT) ===\n")
