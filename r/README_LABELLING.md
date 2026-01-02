# Labelling Pipeline - Dokumentation

## Übersicht

Diese Pipeline implementiert zwei komplementäre Labelling-Methoden für Trading-Daten:

1. **Triple Barrier Labeling** - Klassifiziert jede Bar basierend auf Profit/Stop/Time-Barriers
2. **Extrema-basiertes Meta-Labeling** - Generiert Signale an lokalen Extrema und bewertet deren Qualität

Beide Methoden werden kombiniert, um ein vollständiges Dataset für Machine Learning zu erstellen.

---

## Script-Struktur

### Hauptscript
- **01_labelling_main_script.R** - Orchestriert die gesamte Pipeline

### Modul-Scripts (werden vom Hauptscript geladen)
1. **01_01_triple_barrier_labeling_optimized.R** - Optimierte Triple Barrier Hauptfunktion
2. **01_02_triple_barrier_labeling_helpers.R** - Helper-Funktionen für Analysen
3. **01_03_alternative_labeling_methods.R** - Alternative Labelling-Methoden
4. **01_04_meta_labeling_extrema_signals.R** - Extrema-Detektion & Meta-Labeling
5. **01_05_analyze_extrema_meta_labels.R** - Analyse & Visualisierung der Meta-Labels

---

## Pipeline-Ablauf

### 1. Setup & Datenladung
```r
# Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"

# Lade Preisdaten
dt = fread(paste0("price_data/", EPIC, "_", INTERVAL, ".csv"))
```

### 2. Triple Barrier Labeling

#### 2.1 Initiales Labeling
Erstellt Labels mit Startwerten für Parameter-Tests:
- **ATR Period**: 10 (Volatilitätsmessung)
- **ATR Multiplier**: 1.5 (Barrier-Distanz)
- **Max Horizon**: 8 bars (maximale Haltedauer)
- **Session**: 2:00 - 21:00 (Trading-Session)
- **Neutral Threshold**: 0.3 (30% des ATR für neutrale Labels)

#### 2.2 Label-Qualitäts-Analyse
- **Label Distribution**: Verteilung von Long/Short/Neutral
- **Uniqueness**: Wie viele Labels überlappen sich
- **Sample Weights**: Gewichtung basierend auf Überlappung
- **Holding Period**: Wie lange dauern Trades im Schnitt

#### 2.3 Parameter-Optimierung
Grid Search über:
- **ATR Periods**: [10, 12]
- **ATR Multipliers**: [1.5, 2]
- **Max Horizons**: [8, 11]

Bewertungskriterien:
- **Uniqueness Score**: Höher = weniger Überlappung (besser für Training)
- **Balance Score**: Niedriger = ausgeglichene Label-Verteilung
- **Combined Score**: Optimale Balance beider Kriterien

#### 2.4 Finale Labels mit optimierten Parametern
Erstellt Labels mit den besten Parametern aus dem Grid Search.

**Output:**
- `labelled_data/{EPIC}_{INTERVAL}_labeled.csv`

**Spalten:**
- `datetime` - Zeitstempel
- `open, high, low, close` - Preisdaten
- `atr` - Average True Range
- `upper_barrier, lower_barrier` - Profit/Stop Levels
- `label` - Klassifikation: -1 (Short), 0 (Neutral), 1 (Long)
- `exit_reason` - Warum wurde das Label gesetzt: "upper", "lower", "vertical"
- `bars_held` - Wie viele Bars bis Exit
- `pnl` - Profit/Loss in %
- `sample_weight` - Gewichtung für Training (basierend auf Uniqueness)

---

### 3. Extrema-basiertes Meta-Labeling

#### 3.1 Extrema-Detektion
Identifiziert lokale Minima und Maxima in den Preisdaten:

**Methode:**
- **Lookback**: 5 bars (Fenster für Extrema-Suche)
- **Confirmation**: 2 bars Bestätigung (verhindert False Positives)
- **RSI Filter**: Überkauft/Überverkauft-Zonen (35/65)

**Signal-Generierung:**
- **Minimum + RSI oversold** → Long Signal
- **Maximum + RSI overbought** → Short Signal

#### 3.2 Meta-Labeling
Bewertet jedes Signal:

**Parameter:**
- **Profit Target**: 2.0 × ATR
- **Stop Loss**: 1.5 × ATR
- **Max Holding**: 20 bars

**Meta-Label:**
- `1` = Signal war profitabel (Profit Target erreicht)
- `0` = Signal war nicht profitabel (Stop Loss oder Timeout)

#### 3.3 Analyse
- Win Rate pro Signal-Typ (Long/Short)
- Durchschnittlicher PnL
- Bars bis Exit
- Exit Reasons (Profit/Stop/Timeout)

**Outputs:**
- `labelled_data/{EPIC}_{INTERVAL}_meta_labeled.csv` - Nur Signal-Bars
- `labelled_data/{EPIC}_{INTERVAL}_with_extrema.csv` - Alle Bars mit Extrema-Markierungen

**Meta-Label Spalten:**
- `datetime` - Zeitstempel des Signals
- `primary_signal` - 1 (Long) oder -1 (Short)
- `meta_label` - 1 (erfolgreich) oder 0 (nicht erfolgreich)
- `exit_reason` - "profit", "stop_loss", "timeout"
- `bars_held` - Dauer des Trades
- `realized_pnl` - Tatsächlicher PnL in %
- `profit_target, stop_loss` - Verwendete Levels

---

### 4. Label-Kombination

Merged Triple Barrier und Meta-Labels in ein einziges Dataset:

**Strategie:**
- Left Join: Alle Triple Barrier Bars bleiben erhalten
- Meta-Labels werden mit Prefix `meta_*` hinzugefügt
- Bars ohne Meta-Signal haben `NA` in Meta-Spalten

**Output:**
- `labelled_data/{EPIC}_{INTERVAL}_combined_labels.csv`

**Verwendung:**
- **Triple Barrier Labels**: Primäre Klassifikation für jede Bar
- **Meta-Labels**: Zusätzliche Features für Signal-Timing
- **Sample Weights**: Für gewichtetes Training

---

## Parameter-Tuning Guide

### Triple Barrier Parameter

| Parameter | Beschreibung | Typische Werte | Auswirkung |
|-----------|--------------|----------------|------------|
| `atr_period` | Volatilitätsfenster | 10-14 | Höher = glattere ATR, reagiert langsamer |
| `atr_mult_barrier` | Barrier-Distanz | 1.5-3.0 | Höher = weniger, aber qualitativ bessere Exits |
| `max_horizon_bars` | Max. Haltedauer | 8-20 | Höher = mehr "vertical" Exits |
| `neutral_threshold` | Neutral-Zone | 0.1-0.5 | Höher = mehr neutrale Labels |

#### Optimierungskriterien (detailliert)

**1. Uniqueness Score (HAUPTKRITERIUM - höher = besser)**

**Was es misst:**
- Durchschnittliches Sample Weight = 1 / (Anzahl überlappender Labels)
- **1.0** = Perfekt! Keine überlappenden Labels
- **0.5** = Im Durchschnitt überlappen 2 Labels
- **0.1** = Im Durchschnitt überlappen 10 Labels

**Warum wichtig?**
- Überlappende Labels sind nicht unabhängig
- Machine Learning Modelle brauchen unabhängige Samples
- Hohe Uniqueness = bessere Generalisierung

**Beispiel:**
```
Setup A: avg_uniqueness = 0.85 → durchschnittlich 1.18 überlappende Labels ✓
Setup B: avg_uniqueness = 0.25 → durchschnittlich 4 überlappende Labels ✗
```

**2. Balance Score (ZWEITKRITERIUM - niedriger = besser)**

**Was es misst:**
- Wie ausbalanciert Long/Short/Neutral Labels sind
- **0.0** = Perfekt balanciert (33% / 33% / 33%)
- **0.5** = Stark unbalanciert
- **1.0** = Nur eine Klasse

**Warum wichtig?**
- Modelle lernen besser mit balancierten Klassen
- Vermeidet Bias zu Mehrheitsklasse

**Beispiel:**
```
Setup A: 35% Long, 32% Short, 33% Neutral → balance_score = 0.03 ✓
Setup B: 60% Long, 20% Short, 20% Neutral → balance_score = 0.27 ✗
```

**3. Concurrent Labels (Information)**
- Durchschnittliche Anzahl gleichzeitig aktiver Labels
- Direkt verwandt mit Uniqueness: avg_uniqueness = 1 / mean_concurrent
- Zeigt die "Dichte" der Labels

**4. Vertical Ratio (Information)**
- Anteil der Labels die durch Zeit-Ablauf entstanden (nicht Preis-Bewegung)
- **0.1** = 10% Timeouts → viele echte Signale ✓
- **0.7** = 70% Timeouts → zu konservative Parameter ✗

#### Ziel-Metriken

| Metrik | Ideal-Bereich | Bedeutung |
|--------|---------------|-----------|
| **avg_uniqueness** | 0.7 - 1.0 | Wenig Overlap |
| **balance_score** | 0.0 - 0.15 | Gut balanciert |
| **mean_concurrent** | 1.0 - 1.5 | Niedrige Überlappung |
| **vertical_ratio** | 0.1 - 0.3 | Genug echte Signale |
| **n_samples** | >1000 | Genug Trainingsdaten |

#### Parameter Trade-offs

**Kürzerer Horizon vs. Längerer Horizon**

Kurz (z.B. 8-10 bars):
- ✓ Höhere Uniqueness (weniger Overlap)
- ✓ Schnellere Entscheidungen
- ✗ Möglicherweise mehr Noise

Lang (z.B. 20-24 bars):
- ✗ Niedrigere Uniqueness (mehr Overlap)
- ✓ Klarere Trends
- ✓ Weniger Noise

**Höherer ATR Multiplier vs. Niedrigerer**

Hoch (z.B. 3.0):
- ✓ Klarere Signale (größere Bewegungen nötig)
- ✗ Weniger Samples
- ✗ Mehr Vertical Barrier Hits

Niedrig (z.B. 1.5):
- ✓ Mehr Samples
- ✗ Mehr Noise (kleine Bewegungen zählen schon)
- ✓ Schnellere Barrier Hits

### Meta-Labeling Parameter

| Parameter | Beschreibung | Typische Werte | Auswirkung |
|-----------|--------------|----------------|------------|
| `lookback_bars` | Extrema-Fenster | 3-7 | Höher = weniger, aber stärkere Signale |
| `confirmation_bars` | Bestätigungs-Bars | 1-3 | Höher = weniger False Positives |
| `rsi_oversold/overbought` | RSI-Schwellen | 30-35 / 65-70 | Enger = strengere Filter |
| `atr_mult_profit` | Profit Target | 1.5-3.0 | Höher = weniger Wins, höherer PnL |
| `atr_mult_stop` | Stop Loss | 1.0-2.0 | Niedriger = frühere Exits bei Verlust |
| `max_holding_bars` | Max. Haltedauer | 15-30 | Höher = weniger Timeouts |

**Ziel-Metriken:**
- Win Rate > 50% (Meta-Labels sind prädiktiv)
- Avg PnL (Winners) > 2× Avg PnL (Losers) (Risk/Reward)

---

## Verwendung der Output-Daten

### Für Machine Learning

```r
# Lade kombinierte Labels
data <- fread("labelled_data/GOLD_MINUTE_15_combined_labels.csv")

# Feature Engineering (Beispiel)
features <- data[, .(
  # Triple Barrier Features
  atr,
  upper_barrier,
  lower_barrier,

  # Meta-Label Features (falls vorhanden)
  has_meta_signal = !is.na(meta_primary_signal),
  meta_signal_direction = meta_primary_signal,

  # Weitere technische Indikatoren...
)]

# Training mit Sample Weights
model <- train(
  x = features,
  y = data$label,
  weights = data$sample_weight  # WICHTIG!
)
```

### Für Backtesting

```r
# Nutze Meta-Labels für Signal-Filtering
signals <- data[!is.na(meta_primary_signal)]

# Filtere nur erfolgreiche Signale (Meta-Label = 1)
quality_signals <- signals[meta_label == 1]

# Oder: Trainiere Binary Classifier für Meta-Labels
# Predict: Wird dieses Signal profitabel sein?
```

---

## Performance-Optimierungen

Die Pipeline ist für große Datasets optimiert:

- **Vektorisierte Operationen**: Keine Row-by-Row Loops
- **data.table**: Schnelle Aggregationen und Joins
- **RcppRoll**: Optimierte Rolling Window Berechnungen
- **Smart Early Termination**: Stoppt Berechnungen wenn möglich

**Benchmark:**
- Triple Barrier: ~5000-8000 rows/sec
- Meta-Labeling: ~10000+ rows/sec (vektorisiert)

---

## Troubleshooting

### Problem: Zu viele neutrale Labels
**Lösung:** Reduziere `neutral_threshold` oder erhöhe `atr_mult_barrier`

### Problem: Zu viel Label-Überlappung (niedriger Uniqueness Score)
**Lösung:** Reduziere `max_horizon_bars` oder erhöhe `atr_mult_barrier`

### Problem: Unausgewogene Label-Verteilung
**Lösung:** Teste verschiedene `neutral_threshold` Werte (siehe neutral_threshold_results)

### Problem: Meta-Labels haben niedrige Win Rate (<45%)
**Lösung:**
- Verschärfe Extrema-Kriterien (höherer `lookback_bars`, strengere RSI)
- Erhöhe `atr_mult_profit` (höhere Profit Targets)
- Reduziere `atr_mult_stop` (engere Stops)

### Problem: Zu wenige Meta-Signale
**Lösung:**
- Reduziere `lookback_bars`
- Lockere RSI-Schwellen
- Reduziere `confirmation_bars`

---

## Parameter-Optimierungs-Workflow

### Verwendung der optimize_labeling_parameters() Funktion

Die Funktion testet verschiedene Kombinationen und sortiert nach Qualitätskriterien.

#### Standard: Sortierung nach Uniqueness

```r
opt_params <- optimize_labeling_parameters(
  prices = dt,
  atr_periods = c(10, 14, 20),
  atr_mults = c(1.5, 2, 2.5, 3),
  max_horizons = c(8, 11, 14, 17),
  sort_by = "uniqueness"  # Default
)
```

**Ergebnis:** Top-Parameter haben höchste Uniqueness, dann beste Balance

#### Alternative: Sortierung nach Balance

```r
opt_params <- optimize_labeling_parameters(
  prices = dt,
  sort_by = "balance"
)
```

**Ergebnis:** Top-Parameter haben beste Balance, dann höchste Uniqueness

#### Kombiniert: Gewichteter Score

```r
opt_params <- optimize_labeling_parameters(
  prices = dt,
  sort_by = "combined"
)
```

**Gewichtung:** 70% Uniqueness + 30% Balance

### Beispiel-Output Interpretation

```
=== TOP 10 PARAMETER-KOMBINATIONEN ===
   atr_period atr_mult max_horizon n_samples avg_uniqueness balance_score mean_concurrent vertical_ratio
1:         14      3.0          10      2145         0.8523        0.0421             1.2          0.185
2:         14      2.5          10      2287         0.8341        0.0534             1.3          0.223
3:         20      3.0          11      2098         0.8124        0.0398             1.3          0.197
```

**Beste Kombination (Zeile 1):**
- ATR Period = 14, Multiplier = 3.0, Horizon = 10
- Uniqueness = 0.85 → Sehr gut! Wenig Overlap
- Balance Score = 0.04 → Sehr gut balanciert
- Mean Concurrent = 1.2 → Im Durchschnitt nur 1.2 Labels gleichzeitig aktiv
- Vertical Ratio = 0.19 → 81% echte Preis-Signale

### Empfohlener Workflow

1. **Starte mit großem Grid:**
   ```r
   opt_params <- optimize_labeling_parameters(
     prices = dt,  # Nutze vollen Datensatz für repräsentative Ergebnisse
     atr_periods = c(10, 14, 20),
     atr_mults = c(1.5, 2, 2.5, 3),
     max_horizons = c(8, 11, 14, 17, 20),
     sort_by = "uniqueness"
   )
   ```

2. **Analysiere Top 5 Kombinationen:**
   - Schaue auf avg_uniqueness, balance_score, mean_concurrent
   - Prüfe n_samples (genug Daten?)

3. **Verfeinere um beste Kombination:**
   ```r
   # Wenn Top = ATR 14, Mult 3.0, Horizon 10
   opt_params_refined <- optimize_labeling_parameters(
     prices = dt,
     atr_periods = c(12, 14, 16),
     atr_mults = c(2.5, 3.0, 3.5),
     max_horizons = c(8, 10, 12),
     sort_by = "uniqueness"
   )
   ```

4. **Teste mit optimalen Parametern:**
   ```r
   best <- opt_params[1]
   final_labeled <- create_triple_barrier_labels(
     prices = dt,
     atr_period = best$atr_period,
     atr_mult_barrier = best$atr_mult,
     max_horizon_bars = best$max_horizon
   )
   ```

### Performance-Tipp

Die Optimierung nutzt automatisch die optimierte Version von `create_triple_barrier_labels()`.

Bei großem Grid (z.B. 60 Kombinationen):
- Mit 1 Jahr Daten (15min): ~5-10 Minuten
- Performance: ~5000-8000 rows/sec

---

## Nächste Schritte

Nach dem Labeling:

1. **Feature Engineering** - Erstelle technische Indikatoren als Features
2. **Train/Test Split** - Zeitbasiert (z.B. 80% Train, 20% Test)
3. **Model Training** - Mit Sample Weights!
4. **Walk-Forward Validation** - Teste auf ungesehenen Daten
5. **Backtesting** - Simuliere Trading-Performance
