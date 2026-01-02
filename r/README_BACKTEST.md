# Machine Learning Backtest Pipeline

Dieses Modul implementiert eine vollständige ML-Pipeline für Trading-Strategien basierend auf den Prinzipien aus "Advances in Financial Machine Learning" von Marcos Lopez de Prado.

## 📁 Dateistruktur

```
r/
├── 02_backtest_main_script.R              # Haupt-Pipeline-Script
├── 02_01_indicator_calculation.R          # Technische Indikatoren
├── 02_02_feature_engineering.R            # Lags, Ableitungen, Rolling Stats
├── 02_03_feature_selection.R              # Top 25 Features (XGBoost/RF/Boruta)
├── 02_04_purged_kfold_cv.R               # Purged K-Fold Cross-Validation
├── 02_05_model_training.R                 # Training mit MDI/MDA/SFI
├── 02_06_backtest_evaluation.R           # Performance-Evaluation
└── README_BACKTEST.md                     # Diese Datei
```

## 🎯 Pipeline-Übersicht

Die Backtest-Pipeline folgt diesem Workflow:

```
1. Load Labels (Triple Barrier / Meta-Labels)
         ↓
2. Calculate Technical Indicators (20+ Indikatoren)
         ↓
3. Feature Engineering (Lags, Derivatives, Rolling Stats)
         ↓
4. Feature Selection Stage 1 (Reduktion auf Top 25)
         ↓
5. Purged K-Fold CV Setup
         ↓
6. Hyperparameter Tuning (XGBoost / Random Forest)
         ↓
7. Feature Importance (MDI, MDA, SFI)
         ↓
8. Final Model Training (Top 6-7 Features, Deep Trees)
         ↓
9. Backtest Evaluation & Visualization
```

## 🔧 Verwendung

### Basis-Verwendung

```r
# Haupt-Script ausführen
source("r/02_backtest_main_script.R")
```

Das Script lädt automatisch die gelabelten Daten aus dem `labelled_data/` Ordner und führt die komplette Pipeline durch.

### Konfiguration

Wichtige Parameter im Haupt-Script:

```r
# Dataset-Auswahl
USE_SMALL_DATASET <- TRUE  # TRUE für schnelle Tests, FALSE für vollständige Analyse

# Welche Labels verwenden?
# 1. Triple Barrier Labels (mit sample_weights)
# 2. Meta-Labels (Extrema-basiert)
# 3. Combined Labels (beides)

# Feature Engineering
lag_periods = c(1, 2, 3, 5, 10)
derivative_orders = c(1, 2)
hourly_aggregation = TRUE
rolling_windows = c(10, 20, 50)

# Feature Selection
n_top_features = 25

# Cross-Validation
n_splits = 5
pct_embargo = 0.01  # 1% Embargo

# Final Model
n_final_features = 7  # Deep Trees mit max 6-7 Features
```

## 📊 Technische Indikatoren

Das Modul berechnet folgende Indikatoren (mehrere Perioden wo angegeben):

### Trend-Indikatoren
- **EMA** (9, 21, 50, 100, 200): Exponential Moving Average
- **McGinley Dynamic** (10, 20): Adaptive MA
- **Ichimoku Cloud**: Tenkan, Kijun, Senkou Span A/B, Chikou

### Momentum-Indikatoren
- **RSI** (14, 28): Relative Strength Index
- **Momentum** (5, 10, 20)
- **ROC** (5, 10, 20): Rate of Change
- **Stochastic Oscillator** (14, 3, 3)
- **Williams %R** (14)
- **CCI** (20): Commodity Channel Index
- **CMO** (14): Chande Momentum Oscillator
- **Aroon** (25): Up, Down, Oscillator

### Volatilitäts-Indikatoren
- **ATR** (14, 28): Average True Range
- **Bollinger Bands** (20): Upper, Lower, Mid, %B, Bandwidth
- **Keltner Channel** (20): Upper, Lower, Mid, Position

### Trend-Stärke
- **ADX** (14): Average Directional Index + DI+/DI-
- **DPO** (20): Detrended Price Oscillator
- **VHF** (28): Vertical Horizontal Filter
- **Choppiness Index** (14)

### Weitere
- **Parabolic SAR**: Stop and Reverse
- **Donchian Channel** (20)
- **Volume Indicators**: Volume Ratio, OBV, VPT

**Total:** 100+ Features nach Indikator-Berechnung

## 🔨 Feature Engineering

Nach der Indikator-Berechnung werden zusätzliche Features erstellt:

### 1. Lag Features
Zeitverzögerte Versionen wichtiger Indikatoren:
- Lags: 1, 2, 3, 5, 10 Bars
- Angewendet auf: EMA, RSI, ATR, ADX, Momentum, Volume Ratio, etc.

### 2. Ableitungen (Derivatives)
- **1. Ableitung**: Rate of Change (Velocity)
- **2. Ableitung**: Acceleration

### 3. Stündliche Aggregate
Rollende 1-Stunden-Features:
- Mean Close, SD Close
- Volume Sum/Mean
- High/Low/Range

### 4. Rolling Statistics
Für wichtige Features (Close, Volume, ATR, RSI):
- Rolling Mean (10, 20, 50)
- Rolling SD
- Z-Score (Distanz zu Rolling Mean)

### 5. Zeit-basierte Features
- Hour, Day of Week, Month
- Session Indicator (Trading Hours: 2-21 Uhr)
- Sine/Cosine Encoding (zyklische Features)

**Total:** 500+ Features nach Feature Engineering

## 🎯 Feature Selection

### Stage 1: Top 25 Features

Reduziert 500+ Features auf die wichtigsten 25 Features mittels:

- **XGBoost** (default): Gain-basierte Importance
- **Random Forest**: Impurity-basierte Importance
- **Boruta** (optional): All-Relevant Feature Selection

Verwendet 3-Fold CV für stabilere Importance-Schätzungen.

### Stage 2: Top 6-7 Features

Nach Hyperparameter-Tuning werden mittels kombinierter Feature Importance (MDI + MDA + SFI) die finalen 6-7 Features ausgewählt.

**Ziel:** Deep Trees vermeiden Overfitting durch Begrenzung auf wenige, hochinformative Features.

## 🔄 Purged K-Fold Cross-Validation

Implementiert nach Lopez de Prado zur Vermeidung von Label-Leakage:

### 1. Purging
Entfernt überlappende Labels zwischen Train und Test Sets:
- Alle Train-Samples deren Label-Ende (`t1`) in die Test-Periode fällt werden entfernt
- Alle Train-Samples deren Label-Start nach der Test-Periode liegt bleiben

### 2. Embargo
Zeitliche Barriere nach jedem Test Set:
- Standard: 1% der Samples nach Test Set werden aus Train entfernt
- Verhindert Information Leakage durch zeitlich nahe Samples

### Visualisierung

```r
plot_cv_splits(dt, cv_splits, output_path = "backtest_results/cv_splits.png")
```

## 🎓 Model Training & Hyperparameter Tuning

### Unterstützte Modelle

1. **XGBoost**
   - Gradient Boosting für strukturierte Daten
   - Schnell und sehr gut für Tabellendaten

2. **Random Forest (ranger)**
   - Parallel-implementiertes Random Forest
   - Robust gegenüber Overfitting

### Hyperparameter Grid

**XGBoost:**
```r
max_depth: 6, 8, 10, 12
eta: 0.01, 0.05, 0.1
subsample: 0.8, 1.0
colsample_bytree: 0.6, 0.8
min_child_weight: 1, 3, 5
```

**Random Forest:**
```r
num.trees: 500, 1000
mtry: 3, 5, 7  (max features per split)
max.depth: 10, 15, 20, 25
min.node.size: 5, 10, 20
```

Alle Kombinationen werden mit Purged K-Fold CV evaluiert.

## 📈 Feature Importance Metriken

### MDI (Mean Decrease Impurity)
- Direkt aus Tree-basierten Modellen
- Misst durchschnittliche Reduktion der Impurity (Gini/Entropy)
- **Vorteil:** Schnell zu berechnen
- **Nachteil:** Bias zu hochkardinalitären Features

### MDA (Mean Decrease Accuracy)
- Permutation Importance
- Misst Genauigkeitsverlust wenn Feature permutiert wird
- **Vorteil:** Unbiased, modell-agnostisch
- **Nachteil:** Langsam (erfordert Re-Prediction)

### SFI (Single Feature Importance)
- Trainiert separates Modell mit nur einem Feature
- Orthogonalisierte Importance (keine Feature-Interaktionen)
- **Vorteil:** Zeigt echte standalone-Importance
- **Nachteil:** Sehr langsam (ein Modell pro Feature)

### Kombiniertes Ranking

```r
combined_importance <- combine_importance_rankings(importance_results)
```

Erstellt Consensus-Ranking über alle drei Metriken:
- Berechnet Rank pro Methode
- Durchschnitt der Ranks = Combined Rank
- Robuster gegenüber einzelnen Methoden-Bias

## 📊 Backtest Evaluation

### Classification Metriken
- **AUC**: Area Under ROC Curve
- **Accuracy**: Gesamt-Genauigkeit
- **Precision**: True Positives / (True Positives + False Positives)
- **Recall**: True Positives / (True Positives + False Negatives)
- **F1 Score**: Harmonisches Mittel von Precision und Recall
- **Log Loss**: Probabilistische Loss-Funktion
- **Confusion Matrix**: 2x2 Matrix (Actual vs Predicted)

### Returns Metriken (falls verfügbar)
- **Total Return (Buy & Hold)**: Passive Strategie
- **Total Return (Strategy)**: Nur Trades wo Modell 1 predicted
- **# Trades**: Anzahl positiver Predictions
- **Win Rate**: % profitable Trades
- **Average Win/Loss**: Durchschnittliche Gewinne und Verluste
- **Profit Factor**: Avg Win / |Avg Loss|
- **Sharpe Ratio**: Risk-adjusted Return (annualisiert)
- **Max Drawdown**: Größter Peak-to-Trough Verlust

### Visualisierungen

Automatisch erstellte Plots:

1. **Cumulative Returns**: Strategy vs Buy & Hold über Zeit
2. **Prediction Distribution**: Histogram der Predicted Probabilities
3. **Predictions Over Time**: Zeitreihe der Predictions (geglättet)
4. **Confusion Matrix**: Heatmap
5. **Feature Importance Comparison**: MDI vs MDA vs SFI (Top 15)

Alle Plots werden in `backtest_results/` gespeichert.

## 💾 Output-Dateien

Nach einem kompletten Durchlauf werden folgende Dateien erstellt:

```
backtest_results/
├── GOLD_MINUTE_15_model.rds                    # Trainiertes Modell
├── GOLD_MINUTE_15_importance.csv               # Feature Importance (kombiniert)
├── GOLD_MINUTE_15_backtest.csv                 # Out-of-Sample Predictions
├── GOLD_MINUTE_15_summary.txt                  # Text-Report
├── cumulative_returns.png                      # Plot 1
├── prediction_distribution.png                 # Plot 2
├── predictions_over_time.png                   # Plot 3
├── confusion_matrix.png                        # Plot 4
└── feature_importance_comparison.png           # Plot 5
```

## 🚀 Performance-Optimierung

### Parallelisierung
Die Pipeline nutzt `parallel` und `doParallel`:
- Feature Selection: Paralleles Training über Folds
- Hyperparameter Tuning: Parallel über Konfigurationen
- MDA Calculation: Parallel über Features

Standard: `n_cores = detectCores() - 1`

### data.table
Alle Datenoperationen nutzen `data.table` für:
- Schnelle Aggregationen
- Effiziente Merges
- Minimaler Memory Footprint

### Reduzierter Datensatz für Tests

```r
USE_SMALL_DATASET <- TRUE
dt_small <- dt[datetime >= "2024-01-01" & datetime <= "2025-12-31"]
```

Für Entwicklung/Testing: Nutze nur 1-2 Jahre Daten (10-20x schneller).

## 🧪 Vermeidung von Overfitting

Die Pipeline implementiert mehrere Anti-Overfitting Maßnahmen:

1. **Purged K-Fold CV**: Verhindert Label Leakage
2. **Sample Weights**: Berücksichtigt Label-Überlappung
3. **Feature Selection**: Reduziert Dimensionalität drastisch
4. **Limited Tree Depth**: Deep Trees mit max 6-7 Features
5. **Early Stopping**: Bei Hyperparameter-Tuning
6. **Out-of-Sample Evaluation**: Alle Metriken auf Test Sets

## 📝 Beispiel-Workflow

```r
# 1. Stelle sicher dass Labels existieren
# Führe zuerst 01_labelling_main_script.R aus

# 2. Starte Backtest Pipeline
source("r/02_backtest_main_script.R")

# 3. Warte auf Completion (kann 30 min - 2h dauern je nach Dataset-Größe)

# 4. Analysiere Ergebnisse
# - Öffne backtest_results/GOLD_MINUTE_15_summary.txt
# - Betrachte Plots in backtest_results/
# - Lade Modell: model <- readRDS("backtest_results/GOLD_MINUTE_15_model.rds")

# 5. Verwende Modell für neue Predictions
new_data <- ... # Neue Daten mit gleichen Features
preds <- predict(model, newdata = new_data)
```

## 🔧 Erweiterte Verwendung

### Nur bestimmte Module laden

```r
# Nur Indikator-Berechnung
source("r/02_01_indicator_calculation.R")
dt_ind <- calculate_all_indicators(dt, verbose = TRUE)

# Nur Feature Engineering
source("r/02_02_feature_engineering.R")
dt_feat <- engineer_features(dt_ind, lag_periods = c(1, 2, 3), verbose = TRUE)

# Nur Feature Selection
source("r/02_03_feature_selection.R")
result <- select_important_features(dt_feat, method = "xgboost", n_top_features = 25)
```

### Eigene Hyperparameter-Grid

```r
# Definiere eigenes Grid
my_grid <- list(
  xgb = expand.grid(
    max_depth = c(4, 6, 8),
    eta = c(0.05, 0.1),
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    stringsAsFactors = FALSE
  )
)

# Verwende in model_results
model_results <- train_and_evaluate_models(
  dt = dt_reduced,
  cv_splits = cv_splits,
  hyperparam_grid = my_grid,
  ...
)
```

### Meta-Labels verwenden

```r
# Im Hauptscript, ändere die Datei-Auswahl:
meta_labels_file <- file.path(
  labelled_data_path,
  paste0(EPIC, "_", INTERVAL, "_meta_labeled.csv")
)
dt <- fread(meta_labels_file)

# Target ist jetzt 'meta_label' statt 'label'
target_col <- "meta_label"

# Rest der Pipeline bleibt gleich
```

## 📚 Weiterführende Ressourcen

- **Buch**: "Advances in Financial Machine Learning" von Marcos Lopez de Prado
- **Purged K-Fold CV**: Kapitel 7 - Cross-Validation in Finance
- **Feature Importance**: Kapitel 8 - Feature Importance
- **Sample Weights**: Kapitel 4.5 - Uniqueness of Returns

## ⚠️ Wichtige Hinweise

1. **Zeitliche Reihenfolge**: Nutze IMMER zeitbasierte Splits, nie random splits!
2. **Label Leakage**: Purging ist essentiell bei überlappenden Labels
3. **Overfitting**: Mit vielen Features und wenig Samples sehr gefährlich
4. **Sample Weights**: Unbedingt verwenden wenn Labels überlappen
5. **Recency Bias**: Neuere Daten sind oft wichtiger (ggf. exponentielles Weighting)

## 🐛 Troubleshooting

### Problem: "Column 't1' not found"
**Lösung:** Die Labeling-Funktion muss `t1` (Label End Time) zurückgeben. Prüfe ob Labels mit `create_triple_barrier_labels()` erstellt wurden.

### Problem: Zu viele NA-Werte nach Feature Engineering
**Lösung:** Lags und Rolling Windows erzeugen NAs am Anfang. Diese werden automatisch mit `na.omit()` entfernt. Falls zu viele Samples verloren gehen, reduziere `lag_periods` und `rolling_windows`.

### Problem: Feature Selection findet keine Features
**Lösung:** Prüfe ob numerische Features vorhanden sind und nicht konstant sind (Varianz > 0).

### Problem: "No cores available for parallel processing"
**Lösung:** Reduziere `n_cores` oder setze `n_cores = 1` für sequentielle Verarbeitung.

### Problem: Script läuft sehr lange
**Lösung:**
- Setze `USE_SMALL_DATASET <- TRUE`
- Reduziere Hyperparameter-Grid
- Reduziere `n_splits` in CV
- Deaktiviere SFI (am langsamsten)

## 📈 Nächste Schritte

Nach erfolgreicher Backtest-Analyse:

1. **Walk-Forward Testing**: Rollende Out-of-Sample Tests über Zeit
2. **Live-Testing**: Paper Trading mit aktuellen Daten
3. **Ensemble Models**: Kombiniere mehrere Modelle
4. **Alternative Labels**: Teste Meta-Labels, Custom Labels
5. **Feature-Kombinationen**: Teste Interaction Features nach initialer Selection
6. **Alternative ML-Methoden**: Neural Networks, SVM, etc.

---

**Version:** 1.0
**Datum:** 2026-01-02
**Autor:** Backtest Pipeline Generator
