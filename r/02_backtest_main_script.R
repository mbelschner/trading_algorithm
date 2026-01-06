# Script for running Machine Learning Backtest Pipeline (3-Class Multi-Class)
# Uses purged k-fold cross validation, feature importance (MDI/MDA/SFI),
# and tree-based models on triple barrier labels (Long=1, Neutral=0, Short=-1)

cat("\n=== START ML BACKTEST PIPELINE (3-CLASS MULTI-CLASS) ===\n")

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

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")

# Erstelle Output-Ordner falls nicht vorhanden
if (!dir.exists(backtest_output_path)) {
  dir.create(backtest_output_path, recursive = TRUE)
}
if (!dir.exists(features_cache_path)) {
  dir.create(features_cache_path, recursive = TRUE)
}

# ===== Configuration ==========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"

# Feature Caching: Setzt auf TRUE um Features neu zu berechnen
FORCE_RECALCULATE_FEATURES <- FALSE

# ===== STEP 0: Load Raw Price Data and Labels ================================

cat("\n=== STEP 0: LADE ROHE PREISDATEN UND LABELS ===\n")

# 1. Lade rohe Preisdaten
prices_file <- file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))
cat(sprintf("Lade Preisdaten: %s\n", prices_file))
dt_prices <- fread(prices_file)
setDT(dt_prices)

# Rename 'time' to 'datetime' if necessary
if ("time" %in% names(dt_prices)) {
  setnames(dt_prices, "time", "datetime")
}

cat(sprintf("  Preisdaten geladen: %s Zeilen\n", format(nrow(dt_prices), big.mark = ",")))
cat(sprintf("  Zeitraum: %s bis %s\n", min(dt_prices$datetime), max(dt_prices$datetime)))
cat(sprintf("  Spalten: %s\n", paste(names(dt_prices), collapse = ", ")))

# 2. Lade Labels (erstellt von 01_labelling_main_script.R)
labels_file <- file.path(labelled_data_path, paste0(EPIC, "_", INTERVAL, "_labeled.csv"))
cat(sprintf("\nLade Labels: %s\n", labels_file))
dt_labels <- fread(labels_file)
setDT(dt_labels)

cat(sprintf("  Labels geladen: %s Zeilen\n", format(nrow(dt_labels), big.mark = ",")))
cat(sprintf("  Label-Spalten: %s\n", paste(names(dt_labels), collapse = ", ")))

# Label distribution
cat("\n  Label Verteilung:\n")
print(table(dt_labels$label))

# 3. Verwende Preisdaten als Basis für Feature-Berechnung
dt <- copy(dt_prices)

cat("\n✓ Rohe Preisdaten bereit für Feature-Berechnung\n")
cat(sprintf("  Anzahl Zeilen: %s\n", format(nrow(dt), big.mark = ",")))

# Für schnellere Tests: Reduziere Datensatz (nur 2024-2025)
USE_SMALL_DATASET <- FALSE  # Auf FALSE setzen für vollständige Analyse

if (USE_SMALL_DATASET) {
  cat(sprintf("\n=== VERWENDE REDUZIERTES DATASET FÜR TESTS ===\n"))
  cat(sprintf("Original Preisdaten: %s Zeilen\n", format(nrow(dt), big.mark = ",")))

  # Filter Preisdaten
  dt <- dt[datetime >= "2024-01-01" & datetime <= "2025-12-31"]

  # Filter Labels entsprechend
  dt_labels <- dt_labels[datetime >= "2024-01-01" & datetime <= "2025-12-31"]

  cat(sprintf("Reduziert Preisdaten: %s Zeilen\n", format(nrow(dt), big.mark = ",")))
  cat(sprintf("Reduziert Labels: %s Zeilen\n\n", format(nrow(dt_labels), big.mark = ",")))
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

# Check if cached features exist
features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features.csv")
)

if (file.exists(features_cache_file) && !FORCE_RECALCULATE_FEATURES) {
  cat("\n=== LADE GECACHTE FEATURES ===\n")
  cat(sprintf("Lade Features aus Cache: %s\n", features_cache_file))

  dt_features <- fread(features_cache_file)
  setDT(dt_features)

  cat(sprintf("✓ Features geladen: %s Zeilen, %d Spalten\n",
              format(nrow(dt_features), big.mark = ","),
              ncol(dt_features)))
  cat("Hinweis: Setze FORCE_RECALCULATE_FEATURES = TRUE um Features neu zu berechnen\n")

} else {

  if (FORCE_RECALCULATE_FEATURES) {
    cat("\n=== FORCE_RECALCULATE_FEATURES = TRUE: Berechne Features neu ===\n")
  } else {
    cat("\n=== KEINE GECACHTEN FEATURES GEFUNDEN: Berechne Features ===\n")
  }

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

  # ===== SPEICHERE FEATURES IM CACHE =====

  cat("\n=== SPEICHERE FEATURES IM CACHE ===\n")
  fwrite(dt_features, features_cache_file)
  cat(sprintf("✓ Features gespeichert: %s\n", features_cache_file))
  cat(sprintf("  %s Zeilen, %d Spalten\n",
              format(nrow(dt_features), big.mark = ","),
              ncol(dt_features)))
}

# ===== STEP 2b: Merge Labels with Features ===================================

cat("\n=== STEP 2b: MERGE LABELS MIT FEATURES ===\n")

cat(sprintf("Features vor Merge: %s Zeilen\n", format(nrow(dt_features), big.mark = ",")))
cat(sprintf("Labels vor Merge: %s Zeilen\n", format(nrow(dt_labels), big.mark = ",")))

# Merge basierend auf datetime
# Wichtig: Nur Label-spezifische Spalten aus dt_labels nehmen
label_cols <- c("datetime", "label", "sample_weight", "barrier_touched",
                "bars_to_exit", "realized_return", "n_concurrent")

# Prüfe welche Spalten tatsächlich vorhanden sind
available_label_cols <- intersect(label_cols, names(dt_labels))
cat(sprintf("Verfügbare Label-Spalten: %s\n", paste(available_label_cols, collapse = ", ")))

# Merge
dt_features <- merge(
  dt_features,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all.x = FALSE,  # Nur Zeilen behalten die in beiden vorhanden sind
  all.y = FALSE
)

cat(sprintf("Features nach Merge: %s Zeilen\n", format(nrow(dt_features), big.mark = ",")))
cat(sprintf("Spalten nach Merge: %d\n", ncol(dt_features)))

# Prüfe ob Labels erfolgreich gemerged wurden
if (!"label" %in% names(dt_features)) {
  stop("FEHLER: Labels konnten nicht gemerged werden!")
}

cat("\n✓ Labels erfolgreich mit Features gemerged\n")
cat("  Label Verteilung nach Merge:\n")
print(table(dt_features$label))

# ===== Step 3: Feature Selection (2-Stage) ===================================

cat("\n=== STEP 3: FEATURE SELECTION (2-STAGE) ===\n")

# Definiere Target und Sample Weights
target_col <- "label"
weight_col <- "sample_weight"

# Prüfe ob sample_weight vorhanden ist
use_sample_weights <- weight_col %in% names(dt_features)

if (!use_sample_weights) {
  cat("WARNUNG: Keine sample_weights gefunden. Verwende gleichgewichtete Samples.\n")
  dt_features[, sample_weight := 1.0]
}

# ===== STUFE 1: XGBoost (schnell) - ~380 Features → 50 Features =====
cat("\n--- STUFE 1: XGBoost Feature Selection (schnell) ---\n")

tic()
feature_selection_stage1 <- select_important_features(
  dt = dt_features,
  target_col = target_col,
  weight_col = weight_col,
  method = "xgboost",           # Schnell und effizient
  n_top_features = 50,          # Reduziere auf 50
  cv_folds = 5,                 # 5-fold für Stabilität
  verbose = TRUE
)
toc()

top_features_stage1 <- feature_selection_stage1$top_features
cat(sprintf("\n✓ Stufe 1 abgeschlossen: %d Features reduziert auf %d Features\n",
            length(feature_selection_stage1$importance$feature),
            length(top_features_stage1)))

# Erstelle reduzierten Datensatz für Stufe 2
required_cols_temp <- c("datetime", "label", target_col, weight_col,
                        "barrier_touched", "bars_to_exit", "realized_return")
dt_stage1 <- dt_features[, c(required_cols_temp, top_features_stage1), with = FALSE]

# ===== STUFE 2: Boruta (gründlich) - 50 Features → 12 Features =====
cat("\n--- STUFE 2: Boruta Feature Selection (gründlich) ---\n")
cat("HINWEIS: Boruta ist langsam, aber findet die wirklich relevanten Features!\n\n")

tic()
feature_selection_stage2 <- select_important_features(
  dt = dt_stage1,
  target_col = target_col,
  weight_col = weight_col,
  method = "boruta",            # Gründlich und präzise
  n_top_features = 12,          # Finale Top 12
  cv_folds = 1,                 # Boruta braucht kein CV
  verbose = TRUE
)
toc()

top_features <- feature_selection_stage2$top_features
feature_importance <- feature_selection_stage2$importance

cat(sprintf("\n✓ Stufe 2 abgeschlossen: %d Features → %d Features\n",
            length(top_features_stage1), length(top_features)))

cat(sprintf("\n=== FINALE TOP FEATURES FÜR 3-CLASS MODEL ===\n"))
print(head(feature_importance, 20))

# Erstelle finalen reduzierten Datensatz
required_cols <- c("datetime", target_col, weight_col,
                   "barrier_touched", "bars_to_exit", "realized_return")
feature_cols <- top_features

dt_reduced <- dt_features[, c(required_cols, feature_cols), with = FALSE]

cat(sprintf("\nReduzierter Datensatz: %d Features + %d Meta-Spalten\n",
            length(feature_cols), length(required_cols)))

# ===== Step 4: Purged K-Fold Cross Validation Setup ==========================

cat("\n=== STEP 4: PURGED K-FOLD CV SETUP ===\n")

# Erstelle Purged K-Fold Splits
cv_splits <- create_purged_kfold_splits(
  dt = dt_reduced,
  n_splits = 5,
  pct_embargo = 0.01,  # 1% Embargo nach jedem Test Set
  verbose = TRUE
)

cat(sprintf("CV Splits erstellt: %d Folds\n", length(cv_splits)))

# ===== Step 5: Model Training & Hyperparameter Tuning =========================

cat("\n=== STEP 5: MODEL TRAINING & HYPERPARAMETER TUNING (3-CLASS) ===\n")

# Definiere Hyperparameter Grid für 3-Class Classification
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

cat(sprintf("Hyperparameter Grid: %d XGBoost Kombinationen, %d RF Kombinationen\n",
            nrow(hyperparameter_grid$xgb),
            nrow(hyperparameter_grid$rf)))

tic()
model_results <- train_and_evaluate_models(
  dt = dt_reduced,
  cv_splits = cv_splits,
  target_col = target_col,
  weight_col = weight_col,
  feature_cols = feature_cols,
  hyperparam_grid = hyperparameter_grid,
  models = "xgboost",  # XGBoost für 3-Class Model
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)
toc()

cat("\n=== BESTE MODELLE (CV Performance) ===\n")
print(model_results$best_models)

# ===== Step 6: Test Set Evaluation ===========================================

cat("\n=== STEP 6: TEST SET EVALUATION (3-CLASS) ===\n")

# Verwende letzten Fold als Test Set (Out-of-Time Test)
test_fold_idx <- length(cv_splits)
test_indices <- cv_splits[[test_fold_idx]]$test_idx
train_indices <- cv_splits[[test_fold_idx]]$train_idx

cat(sprintf("Train Set: %d Samples\n", length(train_indices)))
cat(sprintf("Test Set: %d Samples (Out-of-Time)\n", length(test_indices)))

# Trainiere finales Modell auf allen Train-Daten
X_train <- as.matrix(dt_reduced[train_indices, ..feature_cols])
y_train <- dt_reduced[train_indices][[target_col]]
w_train <- dt_reduced[train_indices][[weight_col]]

X_test <- as.matrix(dt_reduced[test_indices, ..feature_cols])
y_test <- dt_reduced[test_indices][[target_col]]

# Konvertiere Labels für XGBoost (muss 0, 1, 2 sein statt -1, 0, 1)
y_train_xgb <- ifelse(y_train == -1, 0, ifelse(y_train == 0, 1, 2))
y_test_xgb <- ifelse(y_test == -1, 0, ifelse(y_test == 0, 1, 2))

# XGBoost DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train_xgb, weight = w_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test_xgb)

# Trainiere mit besten Hyperparametern
best_params <- model_results$best_hyperparams
params_list <- list(
  objective = "multi:softprob",  # Multi-class classification
  eval_metric = "mlogloss",
  num_class = 3,                 # 3 Klassen
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight
)

final_model <- xgb.train(
  params = params_list,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)

# Predictions (returns probabilities for each class)
y_pred_prob <- predict(final_model, dtest, reshape = TRUE)
# Klassen: Spalte 1 = Short (-1), Spalte 2 = Neutral (0), Spalte 3 = Long (1)

# Konvertiere Predictions zurück zu Original Labels
y_pred_class_xgb <- apply(y_pred_prob, 1, which.max) - 1  # 0, 1, 2
y_pred_class <- ifelse(y_pred_class_xgb == 0, -1,
                       ifelse(y_pred_class_xgb == 1, 0, 1))

# Konvertiere y_test_xgb zurück zu Original Labels
y_test_orig <- ifelse(y_test_xgb == 0, -1,
                      ifelse(y_test_xgb == 1, 0, 1))

# Evaluation Metrics
cat("\n=== TEST SET PERFORMANCE (3-CLASS MODEL) ===\n")

# Confusion Matrix
conf_matrix <- table(Predicted = y_pred_class, Actual = y_test_orig)
print(conf_matrix)

# Overall Accuracy
accuracy <- sum(y_pred_class == y_test_orig) / length(y_test_orig)
cat(sprintf("\nOverall Accuracy: %.4f\n", accuracy))

# Per-Class Metrics
cat("\n=== PER-CLASS METRICS ===\n")

for (class_label in c(-1, 0, 1)) {
  class_name <- ifelse(class_label == -1, "Short",
                       ifelse(class_label == 0, "Neutral", "Long"))

  # True Positives, False Positives, False Negatives
  tp <- sum(y_pred_class == class_label & y_test_orig == class_label)
  fp <- sum(y_pred_class == class_label & y_test_orig != class_label)
  fn <- sum(y_pred_class != class_label & y_test_orig == class_label)

  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  f1_score <- ifelse(precision + recall > 0,
                     2 * (precision * recall) / (precision + recall), 0)

  cat(sprintf("\n%s (label=%d):\n", class_name, class_label))
  cat(sprintf("  Precision: %.4f\n", precision))
  cat(sprintf("  Recall:    %.4f\n", recall))
  cat(sprintf("  F1-Score:  %.4f\n", f1_score))
}

# Baseline Comparison
baseline_accuracy <- max(table(y_test_orig)) / length(y_test_orig)
cat(sprintf("\nBaseline (immer häufigste Klasse): %.4f\n", baseline_accuracy))
cat(sprintf("Improvement over Baseline: %.2f%%\n",
            100 * (accuracy - baseline_accuracy) / baseline_accuracy))

# ===== Step 7: Feature Importance ============================================

cat("\n=== STEP 7: FEATURE IMPORTANCE (3-CLASS MODEL) ===\n")

# Extrahiere Feature Importance aus finalem Modell
importance_matrix <- xgb.importance(
  feature_names = feature_cols,
  model = final_model
)

cat(sprintf("\nTop Features:\n"))
print(head(importance_matrix, 12))

# Plot Feature Importance
if (interactive()) {
  xgb.plot.importance(importance_matrix, top_n = 12)
}

# Zeige alle Features die im Modell verwendet werden
cat(sprintf("\n=== ALLE FEATURES IM 3-CLASS MODEL (%d Features) ===\n", length(feature_cols)))
cat(paste(feature_cols, collapse = "\n"))
cat("\n")

# ===== Step 8: Save Results ===================================================

cat("\n=== STEP 8: SPEICHERE ERGEBNISSE ===\n")

# Speichere Modell
model_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_model.rds")
)
saveRDS(final_model, model_file)
cat(sprintf("Modell gespeichert: %s\n", model_file))

# Speichere Feature Importance
importance_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_importance.csv")
)
fwrite(importance_matrix, importance_file)
cat(sprintf("Feature Importance gespeichert: %s\n", importance_file))

# Speichere Test Set Predictions
predictions_dt <- data.table(
  datetime = dt_reduced[test_indices]$datetime,
  actual = y_test_orig,
  predicted = y_pred_class,
  prob_short = y_pred_prob[, 1],
  prob_neutral = y_pred_prob[, 2],
  prob_long = y_pred_prob[, 3]
)

predictions_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_predictions.csv")
)
fwrite(predictions_dt, predictions_file)
cat(sprintf("Test Predictions gespeichert: %s\n", predictions_file))

# Speichere Confusion Matrix
conf_matrix_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_confusion_matrix.csv")
)
fwrite(as.data.table(conf_matrix, keep.rownames = TRUE), conf_matrix_file)
cat(sprintf("Confusion Matrix gespeichert: %s\n", conf_matrix_file))

# Erstelle Summary Report
summary_lines <- c(
  "=== 3-CLASS MULTI-CLASS MODEL SUMMARY ===",
  "",
  sprintf("EPIC: %s", EPIC),
  sprintf("INTERVAL: %s", INTERVAL),
  sprintf("Training Samples: %d", length(train_indices)),
  sprintf("Test Samples: %d", length(test_indices)),
  "",
  "=== FEATURES ===",
  sprintf("Number of Features: %d", length(feature_cols)),
  paste("Features:", paste(feature_cols, collapse = ", ")),
  "",
  "=== MODEL PERFORMANCE ===",
  sprintf("Overall Accuracy: %.4f", accuracy),
  sprintf("Baseline Accuracy: %.4f", baseline_accuracy),
  sprintf("Improvement: %.2f%%",
          100 * (accuracy - baseline_accuracy) / baseline_accuracy),
  "",
  "=== CONFUSION MATRIX ===",
  capture.output(print(conf_matrix)),
  "",
  "=== PER-CLASS METRICS ===",
  sprintf("Short (-1): Precision=%.4f, Recall=%.4f, F1=%.4f",
          ifelse(sum(conf_matrix[1, ]) > 0,
                 conf_matrix[1, 1] / sum(conf_matrix[1, ]), 0),
          ifelse(sum(conf_matrix[, 1]) > 0,
                 conf_matrix[1, 1] / sum(conf_matrix[, 1]), 0),
          2 * (ifelse(sum(conf_matrix[1, ]) > 0,
                      conf_matrix[1, 1] / sum(conf_matrix[1, ]), 0)) *
            (ifelse(sum(conf_matrix[, 1]) > 0,
                    conf_matrix[1, 1] / sum(conf_matrix[, 1]), 0)) /
            (ifelse(sum(conf_matrix[1, ]) > 0,
                    conf_matrix[1, 1] / sum(conf_matrix[1, ]), 0) +
               ifelse(sum(conf_matrix[, 1]) > 0,
                      conf_matrix[1, 1] / sum(conf_matrix[, 1]), 0) + 1e-10)),
  sprintf("Neutral (0): Precision=%.4f, Recall=%.4f, F1=%.4f",
          ifelse(sum(conf_matrix[2, ]) > 0,
                 conf_matrix[2, 2] / sum(conf_matrix[2, ]), 0),
          ifelse(sum(conf_matrix[, 2]) > 0,
                 conf_matrix[2, 2] / sum(conf_matrix[, 2]), 0),
          2 * (ifelse(sum(conf_matrix[2, ]) > 0,
                      conf_matrix[2, 2] / sum(conf_matrix[2, ]), 0)) *
            (ifelse(sum(conf_matrix[, 2]) > 0,
                    conf_matrix[2, 2] / sum(conf_matrix[, 2]), 0)) /
            (ifelse(sum(conf_matrix[2, ]) > 0,
                    conf_matrix[2, 2] / sum(conf_matrix[2, ]), 0) +
               ifelse(sum(conf_matrix[, 2]) > 0,
                      conf_matrix[2, 2] / sum(conf_matrix[, 2]), 0) + 1e-10)),
  sprintf("Long (1): Precision=%.4f, Recall=%.4f, F1=%.4f",
          ifelse(sum(conf_matrix[3, ]) > 0,
                 conf_matrix[3, 3] / sum(conf_matrix[3, ]), 0),
          ifelse(sum(conf_matrix[, 3]) > 0,
                 conf_matrix[3, 3] / sum(conf_matrix[, 3]), 0),
          2 * (ifelse(sum(conf_matrix[3, ]) > 0,
                      conf_matrix[3, 3] / sum(conf_matrix[3, ]), 0)) *
            (ifelse(sum(conf_matrix[, 3]) > 0,
                    conf_matrix[3, 3] / sum(conf_matrix[, 3]), 0)) /
            (ifelse(sum(conf_matrix[3, ]) > 0,
                    conf_matrix[3, 3] / sum(conf_matrix[3, ]), 0) +
               ifelse(sum(conf_matrix[, 3]) > 0,
                      conf_matrix[3, 3] / sum(conf_matrix[, 3]), 0) + 1e-10)),
  "",
  "=== TOP FEATURES ===",
  capture.output(print(head(importance_matrix, 12)))
)

summary_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_3class_summary.txt")
)
writeLines(summary_lines, summary_file)
cat(sprintf("Summary Report gespeichert: %s\n", summary_file))

cat("\n=== END ML BACKTEST PIPELINE (3-CLASS MULTI-CLASS) ===\n")
