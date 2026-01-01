# Kombinierte Labels: Triple Barrier + Meta-Labeling

## Übersicht

Dieses Modul kombiniert beide Labeling-Ansätze in einem einzigen Datensatz, um die Stärken beider Methoden zu nutzen:

### 🎯 Triple Barrier Labels (Primary)
- **Umfang:** Labels für ALLE Bars
- **Werte:** 1 (Long), -1 (Short), 0 (Neutral)
- **Basis:** Symmetrische Profit/Loss Barriers + ATR
- **Stärke:** Systematisch, vollständige Abdeckung
- **Feature:** Sample Weights (Uniqueness) für überlappende Labels

### 🎯 Meta-Labels (Secondary)
- **Umfang:** Labels nur für EXTREMA-SIGNALE
- **Werte:** 1 (Trade nehmen), 0 (Trade skippen)
- **Basis:** Lokale Minima/Maxima mit Confirmation
- **Stärke:** Filtert schlechte Trades, fokussiert auf Wendepunkte
- **Feature:** Primary Signal Side (Long/Short)

### 🎯 Kombinierte Labels (Ensemble)
- **Synergien:** Beide Ansätze ergänzen sich
- **Signal Strength:** Anzahl übereinstimmender Modelle
- **Confidence Score:** Gewichtete Kombination beider Labels
- **Ensemble Label:** Nur Signale wo beide Modelle zustimmen

## Hauptfunktionen

### 1. `combine_labels()`

Merged beide Label-Datensätze und erstellt neue Features.

```r
combined <- combine_labels(
  triple_barrier_labeled = labeled_weighted,
  meta_labeled = meta_labeled,
  prices = dt  # Optional
)
```

**Erstellt folgende neue Spalten:**

| Spalte | Beschreibung | Werte |
|--------|-------------|-------|
| `has_meta_signal` | Hat diese Bar ein Meta-Signal? | TRUE/FALSE |
| `ensemble_label` | Label wenn beide Modelle übereinstimmen | 1, -1, 0 |
| `signal_strength` | Anzahl Modelle die signalisieren | 0, 1, 2 |
| `confidence_score` | Gewichteter Confidence Score | 0.0 - 1.5 |
| `side_agreement` | Übereinstimmung der Side | "both_long", "conflict", etc. |
| `quality_agreement` | Übereinstimmung der Profitabilität | "both_profitable", etc. |

**Meta-Label Spalten (mit `ml_` Prefix):**
- `ml_primary_signal`: Side (1 = Long, -1 = Short)
- `ml_meta_label`: Qualität (1 = Trade, 0 = Skip)
- `ml_exit_reason`: Warum wurde exited?
- `ml_bars_held`: Holding Period
- `ml_realized_pnl`: Realisierter P&L
- `ml_profit_target`: Profit Target Level
- `ml_stop_loss`: Stop Loss Level

### 2. `analyze_combined_labels()`

Detaillierte Analyse der kombinierten Labels.

```r
analyze_combined_labels(combined_labels)
```

**Analysiert:**
- ✅ Side Agreement Rate (Stimmen die Richtungen überein?)
- ✅ Quality Agreement Rate (Stimmen die Profitabilität-Labels überein?)
- ✅ Performance nach Signal Strength (0, 1, 2)
- ✅ Ensemble vs. Individual Performance
- ✅ Confusion Matrix (Triple Barrier vs. Meta-Label)
- ✅ Cohen's Kappa (Interrater-Reliability)
- ✅ Feature Correlations mit Returns

### 3. `save_combined_labels()`

Speichert kombinierte Labels als CSV.

```r
save_combined_labels(
  combined_data = combined_labels,
  output_path = labelled_output_path,
  epic = "GOLD",
  interval = "MINUTE_15"
)
```

**Output:** `GOLD_MINUTE_15_combined_labels.csv`

### 4. `plot_combined_labels()`

Erstellt Visualisierungen.

```r
plot_combined_labels(
  combined_data = combined_labels,
  output_path = labelled_output_path,
  epic = "GOLD",
  interval = "MINUTE_15"
)
```

**Erstellt 3 Plots:**
1. **Signal Overlap:** Venn-Diagramm als Bar Chart
2. **Performance by Signal Strength:** Mean Return mit Error Bars
3. **Signals Timeline:** Price Chart mit Ensemble Signalen

## Verwendung im Hauptskript

```r
# 1. Erstelle Triple Barrier Labels
labeled_weighted <- create_triple_barrier_labels(...)

# 2. Erstelle Meta-Labels
meta_result <- generate_meta_labeled_signals(...)
meta_labeled <- meta_result$meta_labeled

# 3. Kombiniere beide
source("r/03_combine_triple_barrier_and_meta_labels.R")

combined_labels <- combine_labels(
  triple_barrier_labeled = labeled_weighted,
  meta_labeled = meta_labeled
)

# 4. Analysiere
analyze_combined_labels(combined_labels)

# 5. Speichere
save_combined_labels(combined_labels, labelled_output_path, "GOLD", "MINUTE_15")

# 6. Visualisiere
plot_combined_labels(combined_labels, labelled_output_path, "GOLD", "MINUTE_15")
```

## Anwendungsfälle

### 1. Ensemble Trading Strategy

Nutze nur Signale, wo beide Modelle übereinstimmen:

```r
# Nur Bars wo beide Modelle Long signalisieren
high_confidence_longs <- combined_labels[ensemble_label == 1]

# Nur Bars wo beide Modelle Short signalisieren
high_confidence_shorts <- combined_labels[ensemble_label == -1]
```

**Vorteil:** Höhere Precision, weniger False Positives

### 2. Signal Strength Filtering

Filtere Trades nach Signal Strength:

```r
# Nur sehr starke Signale (beide Modelle stimmen zu)
strong_signals <- combined_labels[signal_strength == 2]

# Moderate Signale (nur ein Modell)
moderate_signals <- combined_labels[signal_strength == 1]
```

### 3. Meta-Label als Filter für Triple Barrier

Verwende Meta-Labels um Triple Barrier Signals zu filtern:

```r
# Triple Barrier sagt Long, Meta-Label bestätigt
filtered_longs <- combined_labels[
  label == 1 & has_meta_signal == TRUE & ml_meta_label == 1
]

# Triple Barrier sagt Short, Meta-Label bestätigt
filtered_shorts <- combined_labels[
  label == -1 & has_meta_signal == TRUE & ml_meta_label == 1
]
```

### 4. Machine Learning Features

Verwende kombinierte Features für ML-Modelle:

```r
# Feature-Set für ML
ml_features <- combined_labels[, .(
  # Original Features
  atr, rsi, ema_fast, ema_slow,

  # Triple Barrier Features
  label, uniqueness, t1,

  # Meta-Label Features
  ml_primary_signal, ml_meta_label, ml_bars_held,

  # Kombinierte Features
  signal_strength, confidence_score, ensemble_label
)]

# Target Variable
y <- combined_labels$ret  # oder ensemble_label für Classification
```

### 5. Disagreement Analysis

Analysiere Fälle wo Modelle nicht übereinstimmen:

```r
# Konflikt: Triple Barrier sagt Long, Meta-Label sagt Short
conflicts <- combined_labels[side_agreement == "conflict_long_vs_short"]

# Triple Barrier Neutral, aber Meta-Label sagt Trade
tb_neutral_ml_trade <- combined_labels[
  quality_agreement == "tb_neutral_ml_trade"
]
```

## Interpretationsbeispiele

### Beispiel 1: High Confidence Long Signal

```
datetime: 2025-01-15 14:30:00
close: 2050.50

Triple Barrier:
  label: 1 (Long)
  uniqueness: 0.85 (hohe Qualität)
  t1: 8 bars

Meta-Label:
  ml_primary_signal: 1 (Long)
  ml_meta_label: 1 (Trade)
  ml_exit_reason: "profit_target"
  ml_bars_held: 5
  ml_realized_pnl: 0.0125 (1.25%)

Kombiniert:
  has_meta_signal: TRUE
  ensemble_label: 1 (beide sagen Long)
  signal_strength: 2 (beide Modelle)
  confidence_score: 1.35 (hoch)
  side_agreement: "both_long"
  quality_agreement: "both_profitable"
```

**Interpretation:** ⭐ Sehr starkes Long Signal - beide Modelle stimmen in Side und Quality überein!

### Beispiel 2: Disagreement

```
datetime: 2025-01-15 15:00:00
close: 2048.75

Triple Barrier:
  label: 0 (Neutral - kein klarer Trend)
  uniqueness: 0.45

Meta-Label:
  ml_primary_signal: 1 (Long von Extremum)
  ml_meta_label: 1 (Trade)
  ml_exit_reason: "profit_target"

Kombiniert:
  ensemble_label: 0 (kein Ensemble-Signal)
  signal_strength: 1 (nur Meta-Label)
  side_agreement: "meta_signal_tb_neutral"
  quality_agreement: "tb_neutral_ml_trade"
```

**Interpretation:** ⚠️ Moderates Signal - nur Meta-Label signalisiert. Triple Barrier sieht keinen klaren Trend. Vorsicht!

### Beispiel 3: Conflict

```
datetime: 2025-01-15 16:00:00
close: 2045.20

Triple Barrier:
  label: 1 (Long)

Meta-Label:
  ml_primary_signal: -1 (Short von Extremum)
  ml_meta_label: 1 (Trade)

Kombiniert:
  ensemble_label: 0 (Konflikt)
  signal_strength: 2 (beide signalisieren, aber verschiedene Seiten!)
  side_agreement: "conflict_long_vs_short"
```

**Interpretation:** 🚫 Konflikt - Modelle widersprechen sich in der Richtung. Skip!

## Performance-Metriken

### Agreement Rates

**Side Agreement Rate:** Wie oft stimmen die Richtungen überein?
- **> 70%:** Sehr gut - Modelle sind konsistent
- **50-70%:** Gut - moderate Übereinstimmung
- **< 50%:** Schlecht - Modelle widersprechen sich oft

**Quality Agreement Rate:** Wie oft stimmen die Profitabilität-Labels überein?
- **> 60%:** Sehr gut
- **40-60%:** Moderat
- **< 40%:** Schlecht

### Cohen's Kappa

Misst Interrater-Reliability zwischen beiden Labeling-Methoden:

- **0.81-1.00:** Almost perfect agreement
- **0.61-0.80:** Substantial agreement
- **0.41-0.60:** Moderate agreement ✅ Erwartet
- **0.21-0.40:** Fair agreement
- **< 0.20:** Slight/No agreement

**Typische Werte:** 0.40 - 0.60 (moderat - das ist OK, da die Methoden unterschiedliche Ansätze haben)

## Best Practices

### 1. Signal Selection Strategy

**Konservativ (hohe Precision):**
```r
signals <- combined_labels[ensemble_label != 0 & signal_strength == 2]
```

**Balanciert:**
```r
signals <- combined_labels[
  (ensemble_label != 0) |
  (signal_strength == 2) |
  (has_meta_signal & ml_meta_label == 1 & confidence_score > 0.8)
]
```

**Aggressiv (hohe Recall):**
```r
signals <- combined_labels[
  (label != 0) | (has_meta_signal & ml_meta_label == 1)
]
```

### 2. Risk Management

Positionsgröße basierend auf Signal Strength:

```r
combined_labels[, position_size := case_when(
  signal_strength == 2 ~ 1.0,   # Volle Position
  signal_strength == 1 ~ 0.5,   # Halbe Position
  TRUE ~ 0.0                     # Keine Position
)]
```

### 3. Machine Learning

**Classification Task:** Vorhersage von `ensemble_label`
```r
# Features: OHLC, Indikatoren, etc.
# Target: ensemble_label (1, -1, 0)
# Vorteil: Nur high-quality Signals als positive Samples
```

**Regression Task:** Vorhersage von `ret`
```r
# Features: Alle verfügbaren Features inkl. signal_strength
# Target: ret (Return)
# Sample Weights: confidence_score
```

## Output-Dateien

```
labelled_data/
├── GOLD_MINUTE_15_combined_labels.csv                     # Kombinierte Labels
├── GOLD_MINUTE_15_combined_signal_overlap.png            # Overlap Visualisierung
├── GOLD_MINUTE_15_combined_performance_by_strength.png   # Performance Chart
└── GOLD_MINUTE_15_combined_signals_timeline.png          # Timeline mit Signalen
```

## Troubleshooting

### Problem: Wenige Ensemble-Signale

**Ursache:** Modelle stimmen selten überein
**Lösung:**
- Prüfe Agreement Rates
- Adjustiere Parameter beider Labeling-Methoden
- Verwende `signal_strength >= 1` statt `== 2`

### Problem: Hohe Konfliktrate

**Ursache:** Verschiedene Zeitrahmen oder Methoden
**Lösung:**
- Normal! Triple Barrier und Meta-Labels haben unterschiedliche Ansätze
- Nutze Konflikte als zusätzliches Signal (z.B. "unsicherer Markt")
- Fokussiere auf Bars mit Agreement

### Problem: Low Cohen's Kappa

**Ursache:** Modelle messen unterschiedliche Dinge
**Lösung:**
- Kappa < 0.4 ist OK, da die Methoden komplementär sind
- Wichtiger: Performance-Verbesserung durch Kombination
- Vergleiche Returns von Ensemble vs. Individual Signals

## Zusammenfassung

Die kombinierte Label-Datei ermöglicht:

✅ **Ensemble Learning:** Nutze Stärken beider Modelle
✅ **Signal Filtering:** Reduziere False Positives
✅ **Confidence Scoring:** Gewichte Signale nach Qualität
✅ **Disagreement Analysis:** Verstehe Modell-Unsicherheiten
✅ **Feature Engineering:** Neue Features für ML
✅ **Performance Tracking:** Vergleiche verschiedene Strategien

**Next Steps:**
1. Führe [main_script.R](r/main_script.R) aus um kombinierte Labels zu erstellen
2. Analysiere Agreement Rates und Cohen's Kappa
3. Teste verschiedene Signal Selection Strategien
4. Backteste Ensemble Strategy vs. Individual Strategies
5. Verwende beste Strategie für Live Trading oder ML Training

---

**Autor:** Trading Algorithm Pipeline
**Datum:** 2025-12-30
**Version:** 1.0
