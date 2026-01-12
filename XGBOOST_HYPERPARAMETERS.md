# XGBoost Hyperparameter-Optimierung

## Überblick

Die XGBoost-Modelle wurden mit optimierten Hyperparametern ausgestattet, die Overfitting reduzieren, Class Imbalance behandeln und die Generalisierungsfähigkeit verbessern.

## Änderungen (Version 2)

### ✅ Implementierte Verbesserungen:

1. **Early Stopping mit Validation Set**
2. **Harmonisierte Parameter** zwischen Feature Selection und Training
3. **Class Imbalance Handling** mit `scale_pos_weight`
4. **Verbesserte Regularisierung** (gamma, lambda, alpha)

---

## 1. Early Stopping & Validation Set

### Vorher (❌):
```r
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,        # Feste 200 Runden
  verbose = 0           # Kein Early Stopping!
)
```

### Nachher (✅):
```r
# Split 80/20 für Training/Validation
set.seed(42)
val_idx <- sample(1:nrow(X_train), size = floor(0.2 * nrow(X_train)))
train_idx <- setdiff(1:nrow(X_train), val_idx)

dtrain <- xgb.DMatrix(data = X_train[train_idx, ], ...)
dval <- xgb.DMatrix(data = X_train[val_idx, ], ...)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,                              # Mehr Runden möglich
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,                  # Stoppt automatisch
  verbose = 0
)
```

**Vorteile:**
- ✅ Verhindert Overfitting durch automatisches Stoppen
- ✅ Optimale Anzahl an Iterationen wird gefunden
- ✅ Validation AUC wird überwacht

---

## 2. Harmonisierte Hyperparameter

### Konsistente Parameter zwischen Feature Selection & Training:

| Parameter | Feature Selection | Final Training | Beschreibung |
|-----------|-------------------|----------------|--------------|
| `max_depth` | 4 | **4** (vorher 6) | Max. Tiefe der Bäume (reduziert = weniger Overfitting) |
| `eta` | 0.05 | 0.05 | Learning Rate (konservativ) |
| `subsample` | 0.8 | 0.8 | Row Sampling pro Baum |
| `colsample_bytree` | 0.8 | 0.8 | Feature Sampling pro Baum |
| `colsample_bynode` | 0.8 | **0.8** (vorher fehlend) | Feature Sampling pro Split |
| `min_child_weight` | 10 | **10** (vorher 3) | Min. Summe der Gewichte in Child-Node |
| `gamma` | 0 | **0.1** (vorher fehlend) | Min. Loss Reduction für Split |
| `lambda` | 1 | **1.5** (vorher fehlend) | L2 Regularisierung |
| `alpha` | 0 | **0.1** (vorher fehlend) | L1 Regularisierung |

**Änderungen erklärt:**

- **`max_depth: 6 → 4`**: Flachere Bäume = weniger Overfitting, bessere Generalisierung
- **`min_child_weight: 3 → 10`**: Höhere Schwelle = konservativere Splits, weniger Noise
- **`gamma: 0 → 0.1`**: Bestraft unnötige Splits (Regularisierung)
- **`lambda: Standard → 1.5`**: L2-Regularisierung (bestraft große Gewichte)
- **`alpha: 0 → 0.1`**: L1-Regularisierung (Feature Selection innerhalb des Modells)
- **`colsample_bynode: neu`**: Zusätzliches Feature Sampling pro Node

---

## 3. Class Imbalance Handling

### Problem:
Long/Short-Signale sind oft deutlich seltener als Neutral-Signale.

### Lösung: `scale_pos_weight`

```r
# Berechne Class Balance
n_negative <- sum(y_train == 0)  # Neutral
n_positive <- sum(y_train == 1)  # Long/Short
scale_pos_weight <- n_negative / n_positive

# Beispiel Output:
#   Class balance: Negative=45,123, Positive=8,456
#   scale_pos_weight: 5.3378

params <- list(
  ...
  scale_pos_weight = scale_pos_weight  # Gewichtet Minority Class höher
)
```

**Was macht `scale_pos_weight`?**
- Erhöht das Gewicht der Minority Class (Long/Short)
- Verhindert, dass das Modell einfach "immer Neutral" vorhersagt
- Balanciert Precision und Recall

**Beispiel:**
- Ohne `scale_pos_weight`: Modell sagt zu 90% Neutral voraus (hohe Accuracy, aber nutzlos)
- Mit `scale_pos_weight`: Modell findet mehr Long/Short-Signale (besserer Recall)

---

## 4. Regularisierung im Detail

### Regularisierungs-Parameter:

| Parameter | Wert | Effekt |
|-----------|------|--------|
| `gamma` | 0.1 | Bestraft neue Splits, die wenig Improvement bringen |
| `lambda` (L2) | 1.5 | Bestraft große Gewichte (smooth predictions) |
| `alpha` (L1) | 0.1 | Feature Selection (zwingt Gewichte auf 0) |
| `min_child_weight` | 10 | Verhindert Splits in Nodes mit wenig Samples |

**Kombination:**
- `gamma` + `min_child_weight`: Verhindert zu tiefe, overfitted Bäume
- `lambda` + `alpha`: Glättet Predictions, reduziert Feature Noise
- `colsample_bynode` + `subsample`: Random Sampling reduziert Variance

---

## 5. Training-Flow

### Gesamter Ablauf:

```
1. Split Training Data (80% train, 20% validation)
   └─> Validation Set für Early Stopping

2. Berechne scale_pos_weight
   └─> Handhabt Class Imbalance

3. Trainiere mit nrounds=1000 + early_stopping=50
   └─> Stoppt automatisch bei Plateau

4. Evaluiere auf Full Training Set
   └─> Zeigt Train Performance (inkl. Validation Samples)

5. Evaluiere auf Test Set (2025)
   └─> Echte Out-of-Sample Performance
```

---

## 6. Parameter-Tuning Guidelines

### Wenn Overfitting auftritt (Train AUC >> Test AUC):

**Option 1: Mehr Regularisierung**
```r
max_depth = 3           # Noch flacher
min_child_weight = 15   # Höhere Schwelle
gamma = 0.2             # Mehr Penalty
lambda = 2.0            # Stärkere L2
```

**Option 2: Mehr Sampling**
```r
subsample = 0.7             # Weniger Rows
colsample_bytree = 0.7      # Weniger Features per Tree
colsample_bynode = 0.7      # Weniger Features per Node
```

**Option 3: Langsameres Lernen**
```r
eta = 0.03                  # Langsamere Learning Rate
early_stopping_rounds = 100 # Mehr Geduld
```

### Wenn Underfitting auftritt (Train AUC zu niedrig):

**Option 1: Weniger Regularisierung**
```r
max_depth = 5           # Tiefere Bäume
gamma = 0.05            # Weniger Penalty
min_child_weight = 5    # Niedrigere Schwelle
```

**Option 2: Mehr Bäume**
```r
nrounds = 2000          # Mehr Iterationen
eta = 0.03              # Langsamere LR = mehr Bäume nötig
```

---

## 7. Best Practices

### ✅ DO:

1. **Immer Early Stopping verwenden**
   - Verhindert Overfitting
   - Findet optimale Iteration automatisch

2. **Class Imbalance überprüfen**
   ```r
   table(y_train)  # Zeigt Balance
   ```
   - Wenn Ratio > 3:1 → `scale_pos_weight` verwenden

3. **Validation AUC beobachten**
   - Sollte nahe am Train AUC sein
   - Große Differenz = Overfitting

4. **Parameter harmonisieren**
   - Feature Selection & Training sollten ähnliche Settings haben

5. **Regularisierung graduell erhöhen**
   - Start mit moderaten Werten
   - Bei Overfitting: Schritt für Schritt erhöhen

### ❌ DON'T:

1. **Kein Early Stopping weglassen**
   - Führt fast immer zu Overfitting

2. **Nicht zu aggressive Regularisierung**
   - Kann zu Underfitting führen
   - Modell lernt nichts

3. **Nicht alle Parameter auf einmal ändern**
   - Schwer zu debuggen
   - Unklare Effekte

4. **Nicht `max_depth` > 6 ohne guten Grund**
   - Sehr tiefe Bäume = fast immer Overfitting

---

## 8. Erwartete Verbesserungen

Mit den neuen Hyperparametern sollten folgende Verbesserungen sichtbar sein:

### Metriken:

| Metrik | Vorher | Nachher (erwartet) |
|--------|--------|-------------------|
| **Train AUC** | 0.75-0.85 | 0.70-0.78 (↓ durch Regularisierung) |
| **Test AUC** | 0.55-0.65 | 0.62-0.72 (↑ bessere Generalisierung) |
| **AUC Gap** | 0.15-0.25 | 0.05-0.10 (↓ weniger Overfitting) |
| **Recall (Long/Short)** | 0.30-0.50 | 0.45-0.65 (↑ durch scale_pos_weight) |
| **Best Iteration** | 200 (fix) | 150-400 (variabel, optimal) |

### Qualitativ:

- ✅ **Weniger Overfitting**: Train und Test AUC näher beieinander
- ✅ **Bessere Balance**: Mehr Long/Short-Signale gefunden (nicht nur Neutral)
- ✅ **Robustere Predictions**: Weniger Ausreißer, stabilere Signale
- ✅ **Schnelleres Training**: Early Stopping reduziert unnötige Iterationen

---

## 9. Monitoring während Training

### Wichtige Ausgaben:

```
=== STEP 8a: TRAIN FINAL LONG MODEL ===
Train set: 95,234 rows, 15 features
Test set:  12,456 rows, 15 features

  Class balance: Negative=72,345, Positive=22,889
  scale_pos_weight: 3.1610

Training XGBoost model with early stopping...
✓ Model trained (best iteration: 287)
```

**Was zu beachten:**

1. **Class Balance**:
   - Ratio > 5:1 → sehr unbalanciert
   - `scale_pos_weight` sollte sichtbar sein

2. **Best Iteration**:
   - 100-500: Normal
   - < 50: Mögliches Underfitting (zu restriktive Parameter)
   - > 800: Mögliches Overfitting (Parameter lockern)

3. **Training Time**:
   - Mit Early Stopping: ~10-30 Sekunden
   - Ohne: 60+ Sekunden (für 1000 rounds)

---

## 10. Zusammenfassung der Änderungen

### Kernverbesserungen:

| # | Verbesserung | Impact |
|---|-------------|--------|
| 1 | Early Stopping + Validation Set | **Hoch** - Verhindert Overfitting |
| 2 | scale_pos_weight | **Hoch** - Bessere Signal-Erkennung |
| 3 | max_depth: 6→4 | **Mittel** - Weniger Overfitting |
| 4 | gamma: 0→0.1 | **Mittel** - Regularisierung |
| 5 | lambda: 1→1.5 | **Mittel** - L2 Regularisierung |
| 6 | min_child_weight: 3→10 | **Mittel** - Weniger Noise |
| 7 | alpha: 0→0.1 | **Niedrig** - L1 Regularisierung |
| 8 | colsample_bynode: neu | **Niedrig** - Extra Sampling |

### Parameter-Set (Final):

```r
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  colsample_bynode = 0.8,
  min_child_weight = 10,
  gamma = 0.1,
  lambda = 1.5,
  alpha = 0.1,
  scale_pos_weight = <berechnet>
)

# Training
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  verbose = 0
)
```

---

## Quellen & Referenzen

- [XGBoost Parameters](https://xgboost.readthedocs.io/en/stable/parameter.html)
- [Handling Imbalanced Data](https://xgboost.readthedocs.io/en/stable/tutorials/param_tuning.html#handle-imbalanced-dataset)
- [Avoiding Overfitting](https://xgboost.readthedocs.io/en/stable/tutorials/param_tuning.html#control-overfitting)
