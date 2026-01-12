# Hyperparameter Tuning - Implementation Guide

## Überblick

Das Backtest-Script führt jetzt automatisch Hyperparameter-Tuning für beide Modelle (LONG und SHORT) durch. Das Tuning erfolgt nach der Boruta Feature Selection und nutzt Grid Search mit 5-facher Cross-Validation.

## Tuning-Flow

```
1. Feature Selection (Boruta)
   └─> Finale Features (z.B. 15 Features)

2. Hyperparameter Tuning (Grid Search mit 5-Fold CV)
   └─> Training Data: 2019-2024
   └─> Target Metrics: AUC (primär), Precision, Recall
   └─> Output: Beste Parameter + detaillierte Ergebnisse

3. Final Model Training
   └─> Training mit besten Parametern
   └─> Early Stopping auf Validation Set

4. Test Set Evaluation
   └─> Evaluation auf 2025 Test Data
   └─> Metrics: AUC, Precision, Recall, F1-Score
```

## Parameter Grid

Der Standard-Tuning-Grid umfasst **243 Kombinationen**:

```r
param_grid <- list(
  max_depth = c(3, 4, 5),             # 3 Werte
  eta = c(0.03, 0.05, 0.1),           # 3 Werte
  gamma = c(0, 0.1, 0.2),             # 3 Werte
  lambda = c(1.0, 1.5, 2.0),          # 3 Werte
  min_child_weight = c(5, 10, 15)     # 3 Werte
)

# Gesamt: 3 × 3 × 3 × 3 × 3 = 243 Kombinationen
# Mit 5-Fold CV: 243 × 5 = 1,215 Trainings-Durchläufe
```

### Fixierte Parameter

Diese Parameter bleiben konstant während des Tunings:

```r
fixed_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  subsample = 0.8,
  colsample_bytree = 0.8,
  colsample_bynode = 0.8,
  scale_pos_weight = <berechnet>  # Automatisch basierend auf Class Balance
)
```

## Tuning-Prozess

### 1. Vorbereitung

```r
# Dataset für Tuning (nur finale Features nach Boruta)
dt_train_tuning <- dt_train[, c("label_binary", "sample_weight", stable_features), with = FALSE]

# Tuning-Grid definieren
param_grid_long <- list(
  max_depth = c(3, 4, 5),
  eta = c(0.03, 0.05, 0.1),
  gamma = c(0, 0.1, 0.2),
  lambda = c(1.0, 1.5, 2.0),
  min_child_weight = c(5, 10, 15)
)
```

### 2. Grid Search mit CV

Für jede Parameter-Kombination:
1. 5-Fold Cross-Validation auf 2019-2024 Training Data
2. Training mit Early Stopping (max 1000 Runden, Stop nach 50 ohne Improvement)
3. Berechnung von Train und Validation Metrics (AUC, Precision, Recall)
4. Speicherung der Ergebnisse für jeden Fold

### 3. Best Parameter Selection

Die beste Parameter-Kombination wird basierend auf dem **höchsten Validation AUC** (Durchschnitt über alle Folds) ausgewählt.

```r
# Beispiel Output:
=== BEST PARAMETERS ===
Combination ID: 87
Max Depth: 4
Eta: 0.050
Gamma: 0.10
Lambda: 1.50
Min Child Weight: 10

Validation AUC: 0.7234 (± 0.0156)
Validation Precision: 0.6523 (± 0.0234)
Best Iteration (avg): 287
```

### 4. Final Model Training

Das finale Modell wird mit den besten Parametern auf dem gesamten Training Set (2019-2024) trainiert:

```r
model_long <- xgb.train(
  params = best_params_long,  # Tuned parameters
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  verbose = 0
)
```

## Output Files

### Detaillierte Ergebnisse

**Datei:** `tuning_results/GOLD_MINUTE_15_long_tuning_detailed.csv`

Enthält für **jede Kombination** und **jeden CV-Fold**:
- `combination_id`: ID der Parameter-Kombination
- `max_depth`, `eta`, `gamma`, `lambda`, `min_child_weight`: Getestete Parameter
- `cv_fold`: Fold-Nummer (1-5)
- `train_auc`, `val_auc`: AUC auf Training und Validation
- `train_precision`, `val_precision`: Precision
- `train_recall`, `val_recall`: Recall
- `best_iteration`: Beste Iteration (Early Stopping)

**Zeilen:** 243 Kombinationen × 5 Folds = **1,215 Zeilen**

### Aggregierte Ergebnisse

**Datei:** `tuning_results/GOLD_MINUTE_15_long_tuning_aggregated.csv`

Enthält für **jede Kombination** den Durchschnitt über alle Folds:
- Parameter-Werte
- Durchschnittliche Train/Val Metrics
- Standardabweichungen der Validation Metrics
- Durchschnittliche best_iteration

**Zeilen:** **243 Zeilen** (eine pro Kombination)

### Beispiel Aggregated Results

```csv
combination_id,max_depth,eta,gamma,lambda,min_child_weight,val_auc,val_auc_sd,val_precision,val_recall
1,3,0.03,0.0,1.0,5,0.6823,0.0145,0.6234,0.5891
2,3,0.03,0.0,1.0,10,0.6891,0.0132,0.6312,0.5934
...
87,4,0.05,0.1,1.5,10,0.7234,0.0156,0.6523,0.6201  # BEST
...
```

## Test Set Evaluation

Nach dem Training werden beide Modelle auf den Test Set (2025) evaluiert:

```
=== LONG MODEL - TUNED PARAMETERS ===
  max_depth:        4
  eta:              0.050
  gamma:            0.10
  lambda:           1.50
  min_child_weight: 10

CV Performance (5-fold, 2019-2024):
  Validation AUC:       0.7234 (± 0.0156)
  Validation Precision: 0.6523
  Validation Recall:    0.6201

Test Performance (2025):
  Test AUC:       0.7012
  Test Precision: 0.6345
  Test Recall:    0.6089
  Test F1-Score:  0.6214
```

## Rechenzeit

### Erwartete Dauer

**Pro Kombination:**
- 1 Kombination × 5 Folds × ~15-30 Sekunden = **1-2.5 Minuten**

**Gesamtes Grid (243 Kombinationen):**
- 243 × 1.5 Minuten (Durchschnitt) = **6-8 Stunden**
- Mit Progress Bar für Echtzeit-Updates

**Hinweis:** Die Rechenzeit hängt ab von:
- Anzahl Features (nach Boruta: ~15 Features)
- Anzahl Samples (2019-2024: ~100k Samples)
- Hardware (CPU-Kerne)
- Early Stopping (stoppt früh wenn kein Improvement)

## Parameter-Tuning Guidelines

### Wenn CV AUC niedrig ist (< 0.65)

**Problem:** Underfitting

**Lösung:** Erlaube komplexere Modelle
```r
param_grid <- list(
  max_depth = c(4, 5, 6),           # Tiefere Bäume
  eta = c(0.05, 0.1, 0.15),         # Schnelleres Lernen
  gamma = c(0, 0.05),               # Weniger Regularisierung
  lambda = c(0.5, 1.0),             # Weniger L2
  min_child_weight = c(3, 5, 10)    # Niedrigere Schwelle
)
```

### Wenn CV AUC hoch, aber Test AUC niedrig (Overfitting)

**Problem:** Overfitting

**Lösung:** Erhöhe Regularisierung
```r
param_grid <- list(
  max_depth = c(2, 3, 4),           # Flachere Bäume
  eta = c(0.01, 0.03, 0.05),        # Langsameres Lernen
  gamma = c(0.1, 0.2, 0.3),         # Mehr Regularisierung
  lambda = c(1.5, 2.0, 3.0),        # Mehr L2
  min_child_weight = c(10, 15, 20)  # Höhere Schwelle
)
```

### Wenn Grid Search zu lange dauert

**Lösung 1:** Reduziere Grid-Größe
```r
param_grid <- list(
  max_depth = c(3, 4),              # 2 statt 3
  eta = c(0.03, 0.05),              # 2 statt 3
  gamma = c(0, 0.1),                # 2 statt 3
  lambda = c(1.0, 1.5),             # 2 statt 3
  min_child_weight = c(5, 10)       # 2 statt 3
)
# Kombinationen: 2^5 = 32 (statt 243)
# Rechenzeit: ~45-60 Minuten (statt 6-8 Stunden)
```

**Lösung 2:** Reduziere CV Folds
```r
tuning_result <- tune_xgboost_hyperparameters(
  ...,
  cv_folds = 3,  # Statt 5
  ...
)
# Rechenzeit: ~4-5 Stunden (statt 6-8 Stunden)
```

## Best Practices

### ✅ DO:

1. **Grid Search vor produktivem Einsatz**
   - Führe Tuning einmal durch, speichere beste Parameter
   - Verwende beste Parameter für zukünftige Trainings

2. **Tuning-Ergebnisse analysieren**
   ```r
   # Lade aggregierte Ergebnisse
   agg_results <- fread("tuning_results/GOLD_MINUTE_15_long_tuning_aggregated.csv")

   # Top 10 Kombinationen
   setorder(agg_results, -val_auc)
   print(head(agg_results, 10))

   # Visualisierung
   library(ggplot2)
   ggplot(agg_results, aes(x = eta, y = val_auc, color = factor(max_depth))) +
     geom_point() +
     facet_wrap(~gamma)
   ```

3. **CV und Test Performance vergleichen**
   - Große Differenz (> 0.05 AUC) = Overfitting
   - Kleine Differenz (< 0.03 AUC) = Gut

4. **Beste Parameter dokumentieren**
   - Speichere beste Parameter in separater Config-Datei
   - Verwende diese für zukünftige Runs

### ❌ DON'T:

1. **Nicht zu großen Grid verwenden**
   - Mehr als 500 Kombinationen = sehr lange Rechenzeit
   - Start mit kleinerem Grid, dann verfeinern

2. **Nicht auf Training Metrics optimieren**
   - Immer auf **Validation Metrics** optimieren (val_auc)
   - Training Metrics können täuschen (Overfitting)

3. **Nicht jedes Mal neu tunen**
   - Tuning ist teuer (Stunden)
   - Führe Tuning einmal durch, verwende Ergebnisse

4. **Nicht alle Parameter gleichzeitig variieren**
   - Start mit wichtigsten Parametern: `max_depth`, `eta`, `gamma`
   - Dann verfeinere mit `lambda`, `min_child_weight`

## Fehlerbehebung

### Problem: "Error in xgb.train: scale_pos_weight is NA"

**Ursache:** Class Balance Berechnung fehlgeschlagen

**Lösung:**
```r
# Überprüfe Class Balance
table(dt_train$label_binary)

# Falls eine Klasse fehlt, filtere Daten oder verwende scale_pos_weight = 1
```

### Problem: "CV AUC = 0.5 (Random)"

**Ursache:** Modell lernt nichts (zu restriktive Parameter oder schlechte Features)

**Lösung:**
1. Überprüfe Features: `summary(dt_train[, ..stable_features])`
2. Reduziere Regularisierung (niedrigere gamma, lambda)
3. Erhöhe Komplexität (höhere max_depth, eta)

### Problem: "Early Stopping bei Iteration 10-20"

**Ursache:** Modell konvergiert sehr schnell oder überlernt sofort

**Lösung:**
```r
tuning_result <- tune_xgboost_hyperparameters(
  ...,
  early_stopping_rounds = 100,  # Mehr Geduld
  ...
)
```

## Integration in Bestehende Pipelines

### Option 1: Einmaliges Tuning, dann fixe Parameter

```r
# 1. Einmaliges Tuning (speichere Ergebnisse)
tuning_result <- tune_xgboost_hyperparameters(...)
best_params <- tuning_result$best_params

# 2. Speichere beste Parameter
saveRDS(best_params, "config/best_params_long.rds")

# 3. Zukünftige Runs: Lade Parameter
best_params_long <- readRDS("config/best_params_long.rds")
model_long <- xgb.train(params = best_params_long, ...)
```

### Option 2: Optionales Tuning (Config-Flag)

```r
# In Configuration
ENABLE_HYPERPARAMETER_TUNING <- FALSE  # Set to TRUE for tuning

# Im Script
if (ENABLE_HYPERPARAMETER_TUNING) {
  tuning_result <- tune_xgboost_hyperparameters(...)
  best_params <- tuning_result$best_full_params
} else {
  # Verwende gespeicherte beste Parameter
  best_params <- readRDS("config/best_params_long.rds")
}

model_long <- xgb.train(params = best_params, ...)
```

## Zusammenfassung

Die Hyperparameter-Tuning-Integration bietet:

✅ **Automatisches Grid Search** mit 5-Fold CV
✅ **243 Parameter-Kombinationen** getestet
✅ **Detaillierte Ergebnisse** in CSV gespeichert
✅ **Test Set Evaluation** auf 2025 Daten
✅ **Optimale Parameter** für beide Modelle (LONG/SHORT)

**Erwartete Verbesserungen:**
- **+3-8% AUC** durch optimierte Parameter (vs. Default-Parameter)
- **Weniger Overfitting** durch systematisches Tuning
- **Bessere Generalisierung** auf Test Set

**Rechenzeit:**
- **Einmalig:** 6-8 Stunden für vollständiges Grid (243 Kombinationen)
- **Danach:** Normale Training-Zeit (~5-10 Minuten pro Modell)
