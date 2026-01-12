# Hyperparameter Tuning - Updated Approach (No CV)

## Änderungen zur vorherigen Version

Die Hyperparameter-Tuning-Methode wurde vereinfacht:
- **Vorher:** Grid Search mit 5-facher Cross-Validation (243 Kombinationen × 5 Folds = 1,215 Trainings)
- **Jetzt:** Grid Search ohne CV (243 Kombinationen = 243 Trainings)

## Neuer Ablauf

### 1. Training und Evaluation

Für jede Parameter-Kombination:

```r
1. Trainiere Modell auf 2019-2024 Training Set
   └─> Mit Early Stopping (80/20 train/val split für Early Stopping)

2. Evaluiere auf Training Set (2019-2024)
   └─> Train AUC, Precision, Recall, F1

3. Evaluiere auf Test Set (2025)
   └─> Test AUC, Precision, Recall, F1

4. Speichere beide Metriken in DataFrame
```

### 2. Best Parameter Selection

Die beste Kombination wird basierend auf **Training AUC** ausgewählt (konfigurierbar: `maximize_metric = "auc"` oder `"precision"`).

```r
# Beispiel Best Parameters Output:
=== BEST PARAMETERS ===
Combination ID: 87
Max Depth: 4
Eta: 0.050
Gamma: 0.10
Lambda: 1.50
Min Child Weight: 10

Training Performance:
  Train AUC: 0.7234
  Train Precision: 0.6523
  Train Recall: 0.6201
  Train F1: 0.6358

Test Performance:
  Test AUC: 0.7012
  Test Precision: 0.6345
  Test Recall: 0.6089
  Test F1: 0.6214

Best Iteration: 287
```

## Output Dataframe

### Spalten im Tuning Results CSV

```csv
combination_id,max_depth,eta,gamma,lambda,min_child_weight,train_auc,train_precision,train_recall,train_f1,test_auc,test_precision,test_recall,test_f1,best_iteration
1,3,0.03,0.0,1.0,5,0.6823,0.6234,0.5891,0.6057,0.6612,0.6001,0.5723,0.5856,245
2,3,0.03,0.0,1.0,10,0.6891,0.6312,0.5934,0.6115,0.6678,0.6089,0.5801,0.5941,267
...
87,4,0.05,0.1,1.5,10,0.7234,0.6523,0.6201,0.6358,0.7012,0.6345,0.6089,0.6214,287
...
```

### Spalten-Beschreibung

| Spalte | Beschreibung |
|--------|--------------|
| `combination_id` | ID der Parameter-Kombination (1-243) |
| `max_depth` | Maximale Baumtiefe |
| `eta` | Learning Rate |
| `gamma` | Min. Loss Reduction für Split |
| `lambda` | L2 Regularisierung |
| `min_child_weight` | Min. Summe der Gewichte in Child Node |
| `train_auc` | AUC auf Training Set (2019-2024) |
| `train_precision` | Precision auf Training Set |
| `train_recall` | Recall auf Training Set |
| `train_f1` | F1-Score auf Training Set |
| `test_auc` | AUC auf Test Set (2025) |
| `test_precision` | Precision auf Test Set |
| `test_recall` | Recall auf Test Set |
| `test_f1` | F1-Score auf Test Set |
| `best_iteration` | Beste Iteration (Early Stopping) |

## Vorteile der neuen Methode

### ✅ Schneller
- **Vorher:** 6-8 Stunden (1,215 Trainings)
- **Jetzt:** 1-2 Stunden (243 Trainings)
- **Speedup:** ~5× schneller

### ✅ Einfacher zu analysieren
- Eine Zeile pro Kombination (statt 5 Zeilen für 5 Folds)
- Direkter Vergleich von Train vs. Test Performance
- Alle Metriken in einem DataFrame

### ✅ Transparenter
- Siehst sofort, welche Kombinationen overfitting haben (hoher Train AUC, niedriger Test AUC)
- Kannst eigene Selektionskriterien anwenden (z.B. beste Test AUC statt Train AUC)

## Verwendung

### Basic Usage

```r
# In 02_backtest_main_script_ls_v2.R
tuning_result <- tune_xgboost_hyperparameters(
  dt_train = dt_train_tuning,      # Training data (2019-2024)
  dt_test = dt_test_tuning,        # Test data (2025)
  feature_cols = stable_features,
  target_col = "label_binary",
  weight_col = "sample_weight",
  param_grid = param_grid,
  early_stopping_rounds = 50,
  maximize_metric = "auc",         # Select best based on train_auc
  verbose = TRUE
)
```

### Analysiere Ergebnisse

```r
# Lade Tuning Results
results <- fread("backtest_output/hyperparameter_tuning/GOLD_MINUTE_15_long_tuning_results.csv")

# Top 10 nach Train AUC
setorder(results, -train_auc)
print(head(results, 10))

# Top 10 nach Test AUC
setorder(results, -test_auc)
print(head(results, 10))

# Finde Kombinationen mit wenig Overfitting (kleiner Gap)
results[, auc_gap := train_auc - test_auc]
setorder(results, auc_gap)
print(head(results, 10))

# Visualisierung
library(ggplot2)

# Train vs Test AUC
ggplot(results, aes(x = train_auc, y = test_auc, color = factor(max_depth))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Train vs Test AUC",
       x = "Train AUC",
       y = "Test AUC",
       color = "max_depth") +
  theme_minimal()

# Overfitting Analysis
results[, auc_gap := train_auc - test_auc]
ggplot(results, aes(x = eta, y = auc_gap, color = factor(max_depth))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~gamma) +
  labs(title = "Overfitting Gap (Train - Test AUC)",
       x = "Learning Rate (eta)",
       y = "AUC Gap",
       color = "max_depth") +
  theme_minimal()
```

## Alternative Selektionskriterien

Du kannst manuell die beste Kombination basierend auf anderen Kriterien auswählen:

### 1. Beste Test AUC (statt Train AUC)

```r
# Finde Kombination mit höchster Test AUC
best_idx <- which.max(results$test_auc)
best_params <- results[best_idx, .(max_depth, eta, gamma, lambda, min_child_weight)]

cat("Best parameters based on TEST AUC:\n")
print(best_params)
cat(sprintf("Test AUC: %.4f\n", results$test_auc[best_idx]))
```

### 2. Balance zwischen Train und Test

```r
# Finde Kombination mit kleinstem Gap und hohem Test AUC
results[, auc_gap := train_auc - test_auc]
results[, score := test_auc - 0.5 * auc_gap]  # Penalty für Overfitting

best_idx <- which.max(results$score)
best_params <- results[best_idx, .(max_depth, eta, gamma, lambda, min_child_weight)]

cat("Best parameters (balanced):\n")
print(best_params)
```

### 3. Precision-fokussiert

```r
# Beste Test Precision
best_idx <- which.max(results$test_precision)
best_params <- results[best_idx, .(max_depth, eta, gamma, lambda, min_child_weight)]

cat("Best parameters based on TEST PRECISION:\n")
print(best_params)
```

## Rechenzeit

### Erwartete Dauer (243 Kombinationen)

| Anzahl Features | Pro Kombination | Gesamt |
|-----------------|-----------------|--------|
| 15 Features | 20-30 Sekunden | 1.0-1.5 Stunden |
| 30 Features | 30-45 Sekunden | 1.5-2.5 Stunden |
| 50 Features | 45-60 Sekunden | 2.5-4.0 Stunden |

**Mit Early Stopping:** Viele Kombinationen stoppen früher → schneller als oben

### Reduzierter Grid (schneller)

```r
# Kleinerer Grid für schnelleres Tuning (32 Kombinationen)
param_grid_small <- list(
  max_depth = c(3, 4),           # 2 statt 3
  eta = c(0.03, 0.05),           # 2 statt 3
  gamma = c(0, 0.1),             # 2 statt 3
  lambda = c(1.0, 1.5),          # 2 statt 3
  min_child_weight = c(5, 10)    # 2 statt 3
)
# Kombinationen: 2^5 = 32
# Rechenzeit: ~10-20 Minuten
```

## Best Practices

### ✅ DO:

1. **Analysiere alle Ergebnisse**
   - Schaue nicht nur auf die beste Kombination
   - Identifiziere Trends (z.B. höhere eta = mehr Overfitting)

2. **Visualisiere Train vs. Test Performance**
   - Plot Train AUC vs. Test AUC
   - Punkte auf der Diagonale = kein Overfitting

3. **Überprüfe Overfitting**
   ```r
   results[, auc_gap := train_auc - test_auc]
   summary(results$auc_gap)
   ```
   - Gap < 0.05: Gut
   - Gap 0.05-0.10: Akzeptabel
   - Gap > 0.10: Overfitting

4. **Exportiere Top-N Kombinationen**
   ```r
   top_10 <- head(results[order(-test_auc)], 10)
   fwrite(top_10, "top_10_combinations.csv")
   ```

### ❌ DON'T:

1. **Nicht blind beste Train AUC wählen**
   - Kann starkes Overfitting haben
   - Vergleiche immer mit Test Performance

2. **Nicht nur eine Metrik betrachten**
   - AUC allein kann täuschen
   - Schaue auch Precision, Recall, F1

3. **Nicht ohne Visualisierung entscheiden**
   - Trends in Daten können wichtig sein
   - Plot mindestens Train vs. Test AUC

## Beispiel-Analyse

```r
# Lade Results
results <- fread("backtest_output/hyperparameter_tuning/GOLD_MINUTE_15_long_tuning_results.csv")

# 1. Top 5 nach Train AUC (default selection)
cat("\n=== TOP 5 by TRAIN AUC ===\n")
top_train <- head(results[order(-train_auc)], 5)
print(top_train[, .(combination_id, max_depth, eta, gamma, train_auc, test_auc,
                     auc_gap = train_auc - test_auc)])

# 2. Top 5 nach Test AUC (alternative)
cat("\n=== TOP 5 by TEST AUC ===\n")
top_test <- head(results[order(-test_auc)], 5)
print(top_test[, .(combination_id, max_depth, eta, gamma, train_auc, test_auc,
                    auc_gap = train_auc - test_auc)])

# 3. Kombinationen mit wenig Overfitting
cat("\n=== LOW OVERFITTING (AUC Gap < 0.05) ===\n")
low_overfit <- results[train_auc - test_auc < 0.05]
low_overfit_sorted <- head(low_overfit[order(-test_auc)], 5)
print(low_overfit_sorted[, .(combination_id, max_depth, eta, gamma, train_auc, test_auc,
                               auc_gap = train_auc - test_auc)])

# 4. Vergleiche mit automatisch gewählter Kombination
cat("\n=== AUTOMATICALLY SELECTED (Best Train AUC) ===\n")
best_idx <- which.max(results$train_auc)
print(results[best_idx, .(combination_id, max_depth, eta, gamma, lambda, min_child_weight,
                           train_auc, test_auc, train_precision, test_precision)])
```

## Zusammenfassung

Die neue Tuning-Methode ist:
- **Schneller:** 5× schneller als CV-Version
- **Einfacher:** Ein Ergebnis pro Kombination
- **Transparenter:** Train + Test Metrics direkt vergleichbar
- **Flexibler:** Du kannst eigene Selektionskriterien anwenden

Die Ergebnisse sind in einem einzigen CSV gespeichert mit allen Train/Test Metriken, sodass du verschiedene Auswahlstrategien testen kannst, ohne neu zu trainieren.
