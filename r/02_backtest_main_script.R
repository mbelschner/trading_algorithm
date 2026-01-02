# Script for running Machine Learning Backtest Pipeline
# Uses purged k-fold cross validation, feature importance (MDI/MDA/SFI),
# and tree-based models on triple barrier and meta-labels

cat("\n=== START ML BACKTEST PIPELINE ===\n")

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
USE_SMALL_DATASET <- TRUE  # Auf FALSE setzen für vollständige Analyse

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

# ===== Step 3: Feature Selection (Top 25) ====================================

cat("\n=== STEP 3: FEATURE SELECTION (STAGE 1) ===\n")

# Definiere Target und Sample Weights
target_col <- "label"
weight_col <- "sample_weight"

# Prüfe ob sample_weight vorhanden ist
use_sample_weights <- weight_col %in% names(dt_features)

if (!use_sample_weights) {
  cat("WARNUNG: Keine sample_weights gefunden. Verwende gleichgewichtete Samples.\n")
  dt_features[, sample_weight := 1.0]
}

tic()
feature_selection_result <- select_important_features(
  dt = dt_features,
  target_col = target_col,
  weight_col = weight_col,
  method = "xgboost",              # "xgboost", "ranger", oder "boruta"
  n_top_features = 25,             # Top 25 Features
  cv_folds = 3,                    # 3-fold CV für schnellere Feature Selection
  verbose = TRUE
)
toc()

# Extrahiere Top Features
top_features <- feature_selection_result$top_features
feature_importance <- feature_selection_result$importance

cat(sprintf("\nTop 25 Features ausgewählt:\n"))
print(head(feature_importance, 25))

# Erstelle reduzierten Datensatz mit Top Features
required_cols <- c("datetime", target_col, weight_col,
                   "t1", "barrier_hit", "return")  # Meta-Daten für CV
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

cat("\n=== STEP 5: MODEL TRAINING & HYPERPARAMETER TUNING ===\n")

# Definiere Hyperparameter Grid für Tree-based Models
# Ziel: Deep Trees mit max 6-7 Features
hyperparameter_grid <- list(
  # XGBoost
  xgb = expand.grid(
    max_depth = c(6, 8, 10, 12),
    eta = c(0.01, 0.05, 0.1),
    subsample = c(0.8, 1.0),
    colsample_bytree = c(0.6, 0.8),
    min_child_weight = c(1, 3, 5),
    stringsAsFactors = FALSE
  ),

  # Random Forest
  rf = expand.grid(
    num.trees = c(500, 1000),
    mtry = c(3, 5, 7),  # Max Features pro Split
    max.depth = c(10, 15, 20, 25),
    min.node.size = c(5, 10, 20),
    stringsAsFactors = FALSE
  )
)

cat(sprintf("Hyperparameter Grid: %d XGBoost Kombinationen, %d RF Kombinationen\n",
            nrow(hyperparameter_grid$xgb),
            nrow(hyperparameter_grid$rf)))

# Trainiere und tune Modelle mit CV
tic()
model_results <- train_and_evaluate_models(
  dt = dt_reduced,
  cv_splits = cv_splits,
  target_col = target_col,
  weight_col = weight_col,
  feature_cols = feature_cols,
  hyperparam_grid = hyperparameter_grid,
  models = c("xgboost", "ranger"),  # Beide Modell-Typen
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)
toc()

# Zeige beste Modelle
cat("\n=== BESTE MODELLE (CV Performance) ===\n")
print(model_results$best_models)

# ===== Step 6: Feature Importance Analysis (MDI, MDA, SFI) ===================

cat("\n=== STEP 6: FEATURE IMPORTANCE ANALYSE ===\n")

# Berechne alle drei Importance-Metriken
importance_results <- calculate_feature_importance(
  model = model_results$best_model,
  dt = dt_reduced,
  cv_splits = cv_splits,
  target_col = target_col,
  feature_cols = feature_cols,
  weight_col = weight_col,
  methods = c("MDI", "MDA", "SFI"),
  n_cores = parallel::detectCores() - 1,
  verbose = TRUE
)

# Zeige Top Features nach verschiedenen Metriken
cat("\n=== TOP 10 FEATURES (MDI - Mean Decrease Impurity) ===\n")
print(head(importance_results$MDI, 10))

cat("\n=== TOP 10 FEATURES (MDA - Mean Decrease Accuracy) ===\n")
print(head(importance_results$MDA, 10))

cat("\n=== TOP 10 FEATURES (SFI - Single Feature Importance) ===\n")
print(head(importance_results$SFI, 10))

# Kombiniere Importance Rankings
combined_importance <- combine_importance_rankings(importance_results)
cat("\n=== TOP 10 FEATURES (KOMBINIERT) ===\n")
print(head(combined_importance, 10))

# ===== Step 7: Final Model Training mit Top Features =========================

cat("\n=== STEP 7: FINAL MODEL (6-7 TOP FEATURES) ===\n")

# Wähle Top 6-7 Features basierend auf kombiniertem Ranking
n_final_features <- 7
final_features <- combined_importance$feature[1:n_final_features]

cat(sprintf("Finale Features für Deep Tree Model:\n"))
print(final_features)

# Trainiere finales Modell
final_model <- train_final_model(
  dt = dt_reduced,
  cv_splits = cv_splits,
  target_col = target_col,
  weight_col = weight_col,
  feature_cols = final_features,
  model_type = model_results$best_model_type,
  hyperparams = model_results$best_hyperparams,
  verbose = TRUE
)

# ===== Step 8: Backtest Evaluation ============================================

cat("\n=== STEP 8: BACKTEST EVALUATION ===\n")

# Evaluiere Modell Performance
backtest_results <- evaluate_backtest(
  model = final_model,
  dt = dt_reduced,
  cv_splits = cv_splits,
  target_col = target_col,
  feature_cols = final_features,
  weight_col = weight_col,
  include_returns = TRUE,
  plot_results = TRUE,
  output_path = backtest_output_path,
  verbose = TRUE
)

# Zeige Backtest Metriken
cat("\n=== BACKTEST METRIKEN ===\n")
print(backtest_results$metrics)

# ===== Step 9: Save Results ===================================================

cat("\n=== STEP 9: SPEICHERE ERGEBNISSE ===\n")

# Speichere Modell
model_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_model.rds")
)
saveRDS(final_model, model_file)
cat(sprintf("Modell gespeichert: %s\n", model_file))

# Speichere Feature Importance
importance_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_importance.csv")
)
fwrite(combined_importance, importance_file)
cat(sprintf("Feature Importance gespeichert: %s\n", importance_file))

# Speichere Backtest Results
backtest_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_backtest.csv")
)
fwrite(backtest_results$predictions, backtest_file)
cat(sprintf("Backtest Predictions gespeichert: %s\n", backtest_file))

# Speichere Summary Report
summary_file <- file.path(
  backtest_output_path,
  paste0(EPIC, "_", INTERVAL, "_summary.txt")
)
writeLines(backtest_results$summary, summary_file)
cat(sprintf("Summary Report gespeichert: %s\n", summary_file))

cat("\n=== END ML BACKTEST PIPELINE ===\n")
