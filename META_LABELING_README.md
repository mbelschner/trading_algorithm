# Meta-Labeling für Extrema-basierte Trading Signale

## Übersicht

Dieses Script implementiert **Meta-Labeling** nach Lopez de Prado's "Advances in Financial Machine Learning". Meta-Labeling ist eine zweistufige Machine Learning Strategie:

1. **Primary Model (Stufe 1)**: Bestimmt die **SIDE** des Trades (Long/Short)
2. **Secondary Model (Stufe 2)**: Bestimmt die **SIZE** des Trades (0 = Skip, 1 = Take)

### Warum Meta-Labeling?

**Problem**: Klassische ML-Modelle versuchen gleichzeitig Side UND Size zu bestimmen → oft suboptimal

**Lösung**: Trenne beide Aufgaben:
- Primary Model: Spezialisiert auf Richtungserkennung (z.B. Extrema-Detection)
- Secondary Model (Meta-Model): Spezialisiert auf Qualitätsbewertung der Signale

**Vorteil**:
- Höhere Precision (weniger False Positives)
- Besseres Risk Management
- Modularer Ansatz (Primary Model kann ausgetauscht werden)

---

## Konzept: Extrema-basierte Signale

### 1. Extrema Detection

Das Script erkennt **lokale Minima** (für Long) und **lokale Maxima** (für Short):

```
Lookback Window:   [← 5 bars] [EXTREMUM] [5 bars →]

Beispiel Minimum:
               /\
              /  \
     ________/    \________
     ↑      ↑      ↑      ↑
    t-5    t-1     t     t+5

Bedingung: Low[t] <= min(Low[t-5:t-1]) AND Low[t] <= min(Low[t+1:t+5])
```

### 2. Confirmation (Look-Ahead Bias Vermeidung!)

**Problem**: Bei t können wir noch nicht wissen, ob es wirklich ein Extremum ist!

**Lösung**: Warte auf **Confirmation** durch eine der folgenden Methoden:

#### Methode A: Bar-basierte Confirmation
```
Minimum erkannt bei t=100
→ Warte 2 Bars
→ Wenn Close[t+1] oder Close[t+2] > Low[t] → CONFIRMED
→ Signal wird bei t+2 aktiv (NICHT bei t!)
```

#### Methode B: Derivative-basierte Confirmation
```
Verwende EMA-Ableitungen:
- Velocity = diff(EMA)
- Acceleration = diff(Velocity)

Minimum confirmed wenn:
  Velocity > 0  UND  Acceleration > 0
  (Preis dreht nach oben mit steigender Geschwindigkeit)

Maximum confirmed wenn:
  Velocity < 0  UND  Acceleration < 0
  (Preis dreht nach unten mit steigender Geschwindigkeit)
```

#### Methode C: Combined (Beide erforderlich)
```
Beide Bedingungen müssen erfüllt sein → strengste Methode
```

### 3. Primary Signal Generation

Nach Confirmation wird das **Primary Signal** generiert:

```r
primary_signal = 1   # Long (bei confirmed Minimum)
primary_signal = -1  # Short (bei confirmed Maximum)
primary_signal = 0   # Kein Signal
```

**Wichtig**: Signal wird **an der Confirmation Bar** generiert, nicht am Extremum selbst!

### 4. Meta-Labeling

Für jedes Primary Signal wird ein **Meta-Label** erstellt:

```
Entry: Close[signal_bar]

Profit Target (Long):  Entry + (ATR × Multiplier)
Stop Loss (Long):      Entry - (ATR × Multiplier)

Meta-Label = 1  →  Profit Target wurde erreicht
Meta-Label = 0  →  Stop Loss wurde erreicht ODER unprofitabel
```

**Exit Reasons**:
- `profit_target`: Gewinnziel erreicht ✅
- `stop_loss`: Stop Loss getroffen ❌
- `max_holding_positive`: Max Bars erreicht, im Gewinn ✅
- `max_holding_negative`: Max Bars erreicht, im Verlust ❌

---

## Script Struktur

### Datei: `02_meta_labeling_extrema_signals.R`

#### Funktion 1: `detect_local_extrema()`

```r
extrema_data <- detect_local_extrema(
  prices = dt,
  lookback_bars = 5,              # Window für Extrema-Suche
  confirmation_method = "bars",   # "bars", "derivative", "both"
  confirmation_bars = 2,          # N Bars für Confirmation
  use_rsi = TRUE,                 # RSI Filter aktivieren
  rsi_period = 14,
  rsi_oversold = 30,              # RSI < 30 für Minima
  rsi_overbought = 70             # RSI > 70 für Maxima
)
```

**Output**:
- `is_local_min_candidate`, `is_local_max_candidate`
- `extrema_confirmed`: TRUE wenn bestätigt
- `extrema_type`: "min" oder "max"
- `confirmation_bar`: Index der Confirmation Bar

#### Funktion 2: `generate_primary_signals()`

```r
signal_data <- generate_primary_signals(
  extrema_data = extrema_data,
  atr_period = 14
)
```

**Output**:
- `primary_signal`: 1 (Long), -1 (Short), 0 (kein Signal)
- `signal_bar`: Bar-Index des Signals

#### Funktion 3: `create_meta_labels()`

```r
meta_labeled <- create_meta_labels(
  signal_data = signal_data,
  atr_mult_profit = 2.0,    # 2× ATR für Profit Target
  atr_mult_stop = 1.5,      # 1.5× ATR für Stop Loss
  max_holding_bars = 20,    # Maximale Haltedauer
  use_stop_loss = TRUE      # Stop Loss aktivieren
)
```

**Output**:
- `meta_label`: 0 (Skip Trade), 1 (Take Trade)
- `exit_reason`: Grund des Exits
- `bars_held`: Gehaltene Bars
- `realized_pnl`: Realisierter P&L (%)

#### Wrapper: `generate_meta_labeled_signals()`

Führt alle Schritte in einem Durchlauf aus:

```r
result <- generate_meta_labeled_signals(
  prices = dt,
  # ... alle Parameter ...
)

meta_labeled <- result$meta_labeled    # Nur Signale
full_data <- result$full_data          # Kompletter Datensatz
```

---

## Verwendung

### Schritt 1: Lade Daten

```r
library(data.table)
library(TTR)

dt <- fread("price_data/GOLD_MINUTE_15.csv")
setnames(dt, "time", "datetime")
```

### Schritt 2: Source Script

```r
source("r/02_meta_labeling_extrema_signals.R")
```

### Schritt 3: Generiere Meta-Labels

```r
result <- generate_meta_labeled_signals(
  prices = dt,
  lookback_bars = 5,
  confirmation_method = "bars",
  confirmation_bars = 2,
  atr_mult_profit = 2.0,
  atr_mult_stop = 1.5,
  max_holding_bars = 20
)

meta_labeled <- result$meta_labeled
```

### Schritt 4: Analysiere Ergebnisse

```r
# Label Verteilung
table(meta_labeled$meta_label)

# Success Rate
mean(meta_labeled$meta_label, na.rm = TRUE)

# Performance nach Side
meta_labeled[, .(
  count = .N,
  success_rate = mean(meta_label, na.rm = TRUE),
  avg_pnl = mean(realized_pnl, na.rm = TRUE)
), by = primary_signal]
```

### Schritt 5: Speichern

```r
fwrite(meta_labeled, "labelled_data/GOLD_MINUTE_15_meta_labeled.csv")
```

---

## Test Script

Die Datei `test_meta_labeling.R` enthält 5 verschiedene Konfigurationen:

1. **Basic**: Standard-Parameter, bar-basierte Confirmation
2. **Derivative**: Verwendet Ableitungen für Confirmation
3. **Combined**: Beide Confirmation-Methoden
4. **Aggressive**: Mehr Signale, kürzere Holding
5. **Conservative**: Weniger Signale, höhere Qualität

**Ausführen**:

```r
source("r/test_meta_labeling.R")
```

**Output**:
- 5 CSV-Dateien mit Meta-Labels
- 1 CSV mit Vergleichstabelle
- 2 PNG-Plots (Success Rate, Distribution)

---

## Parameter-Tuning

### Extrema Detection

| Parameter | Beschreibung | Empfehlung |
|-----------|--------------|------------|
| `lookback_bars` | Window-Größe für Extrema | 5-10 (kleiner = mehr Signale) |
| `confirmation_bars` | Bars für Bestätigung | 2-3 (größer = sicherer, langsamer) |
| `rsi_oversold` | RSI-Filter für Minima | 25-35 (niedriger = strenger) |
| `rsi_overbought` | RSI-Filter für Maxima | 65-75 (höher = strenger) |

### Meta-Labeling

| Parameter | Beschreibung | Empfehlung |
|-----------|--------------|------------|
| `atr_mult_profit` | Profit Target Multiplier | 1.5-3.0 (größer = anspruchsvoller) |
| `atr_mult_stop` | Stop Loss Multiplier | 1.0-2.0 (kleiner = engerer Stop) |
| `max_holding_bars` | Max Haltedauer | 15-30 |

### Confirmation Method

- **"bars"**: Schnellste Methode, gut für mittelfristige Trends
- **"derivative"**: Präziser, erkennt Trendwenden früher
- **"both"**: Strengste Methode, höchste Qualität, weniger Signale

---

## Output Spalten

### Meta-Labeled Dataset

| Spalte | Typ | Beschreibung |
|--------|-----|--------------|
| `datetime` | POSIXct | Zeitstempel |
| `open`, `high`, `low`, `close` | numeric | OHLC-Preise |
| `atr` | numeric | Average True Range |
| `rsi` | numeric | RSI Indikator |
| `extrema_type` | character | "min" oder "max" |
| `extrema_confirmed` | logical | Extremum bestätigt? |
| `confirmation_bar` | integer | Bar der Bestätigung |
| `primary_signal` | integer | 1 (Long), -1 (Short) |
| `meta_label` | integer | 0 (Skip), 1 (Take) |
| `exit_reason` | character | Grund des Exits |
| `bars_held` | integer | Gehaltene Bars |
| `realized_pnl` | numeric | P&L in % |
| `profit_target` | numeric | Gewinnziel-Preis |
| `stop_loss` | numeric | Stop-Loss-Preis |

---

## Machine Learning Workflow

### Training

1. **Features erstellen** (aus deinem bestehenden Triple-Barrier Labeling)
2. **Meta-Labels als Target** verwenden
3. **Nur Bars mit `primary_signal != 0`** verwenden

```r
# Merge Triple-Barrier Features mit Meta-Labels
features <- fread("labelled_data/GOLD_MINUTE_15_labeled.csv")
meta <- fread("labelled_data/GOLD_MINUTE_15_meta_labeled.csv")

# Join auf datetime
training_data <- features[meta, on = "datetime", nomatch = 0]

# Target = meta_label
# Features = alle deine berechneten Features aus dem Triple-Barrier
```

### Prediction

```r
# Neues Signal kommt rein
new_signal <- data.table(
  datetime = "2025-01-15 10:00:00",
  primary_signal = 1,  # Long Signal
  # ... Features ...
)

# Meta-Model Vorhersage
prediction <- predict(meta_model, new_signal)

# prediction = 0 → Skip Trade
# prediction = 1 → Take Trade mit Size = f(confidence)
```

---

## Look-Ahead Bias Vermeidung

**Kritisch**: Das Script **vermeidet Look-Ahead Bias** durch:

1. ✅ **Confirmation Delay**: Signal wird erst N Bars NACH dem Extremum aktiv
2. ✅ **Entry = Confirmation Bar Close**: Nicht am Extremum-Preis
3. ✅ **Forward-Looking nur für Labeling**: Meta-Labels nutzen Zukunftsdaten (erlaubt für Training!)
4. ✅ **Primary Signal = lag-basiert**: Verwendet nur historische Daten

**Bei Live-Trading**:
- Primary Model: Erzeugt Signal ohne Zukunftsdaten ✅
- Meta-Model: Bewertet Signal-Qualität (trainiert auf historischen Meta-Labels) ✅

---

## Beispiel-Output

```
=== META-LABEL STATISTIKEN ===
Total Signals: 342

Meta-Label Verteilung:
  0   1
142 200

Prozentual:
  0    1
41.5 58.5

Exit Reasons:
profit_target             200
stop_loss                  89
max_holding_positive       38
max_holding_negative       15

Signal Performance by Side:
   primary_signal count success_rate avg_pnl
1:             -1   167         55.7    0.82
2:              1   175         61.1    1.15
```

---

## Erweiterungen

### 1. Dynamische Stop/Target
```r
# Anstelle von festen Multipliern:
atr_mult_profit <- function(volatility) {
  if(volatility > 2.0) return(1.5)  # Bei hoher Vola: engerer Target
  if(volatility < 0.5) return(3.0)  # Bei niedriger Vola: weiterer Target
  return(2.0)
}
```

### 2. Multi-Timeframe Confirmation
```r
# Prüfe Extrema auf 15min UND 1h Chart
extrema_15m <- detect_local_extrema(dt_15m, ...)
extrema_1h <- detect_local_extrema(dt_1h, ...)

# Signal nur wenn beide übereinstimmen
```

### 3. Volatility Filter
```r
# Filtere Signale in Low-Volatility Phasen
signal_data[atr_pct < 0.5, primary_signal := 0]
```

---

## Referenzen

- **Lopez de Prado, M.** (2018). *Advances in Financial Machine Learning*. Wiley.
  - Chapter 3: Labeling
  - Chapter 6: Meta-Labeling

- **Triple Barrier Method**: Siehe `01_triple_barrier_labeling_optimized.R`

---

## Support

Bei Fragen oder Problemen:
1. Prüfe die Beispiele in `test_meta_labeling.R`
2. Teste mit kleinem Datensatz (z.B. nur 2025)
3. Visualisiere Extrema und Confirmation-Bars

**Happy Trading! 📈**
