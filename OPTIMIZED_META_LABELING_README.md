# Meta-Labeling Performance Optimierungen

## Übersicht

Die Meta-Labeling Funktion wurde erheblich optimiert, um die Performance zu verbessern. Die neue Version nutzt:

- **Vektorisierte Operationen** statt langsame for-Loops
- **RcppRoll** für schnelle Rolling Window Berechnungen
- **data.table Syntax** für effiziente Datenmanipulation
- **Optionale Parallelisierung** für große Datensätze

## Installation

### 1. Installiere benötigte R-Packages

Führe in R oder RStudio aus:

```r
# Installiere RcppRoll (essentiell für Optimierungen)
install.packages("RcppRoll")

# Falls noch nicht installiert:
install.packages("data.table")
install.packages("TTR")
install.packages("parallel")
install.packages("tictoc")
install.packages("ggplot2")
```

### 2. Aktualisiere dein Hauptskript

Die optimierte Version wird automatisch geladen:

```r
# In main_script.R (bereits aktualisiert):
source("r/02_meta_labeling_extrema_signals_optimized.R")
```

## Verwendung

### Basis-Nutzung (Sequential)

```r
meta_result <- generate_meta_labeled_signals(
  prices = dt,
  lookback_bars = 5,
  confirmation_method = "bars",
  confirmation_bars = 2,
  use_rsi = FALSE,
  atr_period = 14,
  atr_mult_profit = 2.0,
  atr_mult_stop = 1.5,
  max_holding_bars = 20,
  use_stop_loss = TRUE,
  parallel = FALSE  # Sequential processing
)
```

### Mit Parallelisierung (für große Datensätze)

```r
meta_result <- generate_meta_labeled_signals(
  prices = dt,
  lookback_bars = 5,
  confirmation_method = "bars",
  confirmation_bars = 2,
  use_rsi = FALSE,
  atr_period = 14,
  atr_mult_profit = 2.0,
  atr_mult_stop = 1.5,
  max_holding_bars = 20,
  use_stop_loss = TRUE,
  parallel = TRUE,   # Aktiviere Parallelisierung
  n_cores = 4        # Anzahl CPU-Kerne
)
```

## Performance Benchmark

### Benchmark durchführen

```r
source("r/benchmark_meta_labeling.R")
```

Das Benchmark-Skript:
- Testet verschiedene Datensatzgrößen (1K, 5K, 10K, 20K, 50K Zeilen)
- Vergleicht Original vs. Optimized (Sequential) vs. Optimized (Parallel)
- Berechnet Speedup-Faktoren
- Erstellt Visualisierungen

### Erwartete Performance-Verbesserungen

| Datensatzgröße | Erwarteter Speedup (Sequential) | Erwarteter Speedup (Parallel) |
|----------------|----------------------------------|-------------------------------|
| 1,000 Zeilen   | 2-3x                             | N/A                           |
| 5,000 Zeilen   | 3-5x                             | N/A                           |
| 10,000 Zeilen  | 4-6x                             | 6-10x                         |
| 20,000 Zeilen  | 5-8x                             | 10-15x                        |
| 50,000+ Zeilen | 6-10x                            | 15-25x                        |

**Hinweis:** Tatsächliche Performance hängt von deiner Hardware ab.

## Hauptoptimierungen

### 1. Vektorisierte Extrema Detection

**Vorher (for-loop):**
```r
for(i in (lookback_bars + 1):(n - lookback_bars)) {
  current_low <- dt$low[i]
  left_window <- dt$low[(i - lookback_bars):(i - 1)]
  right_window <- dt$low[(i + 1):(i + lookback_bars)]

  if(all(current_low <= left_window) && all(current_low <= right_window)) {
    dt$is_local_min_candidate[i] <- TRUE
  }
}
```

**Nachher (vektorisiert mit RcppRoll):**
```r
dt[, roll_min_left := roll_minl(low, n = lookback_bars + 1, fill = NA)]
dt[, roll_min_right := roll_minr(shift(low, type = "lead"), n = lookback_bars + 1, fill = NA)]
dt[, is_local_min_candidate := (low <= roll_min_left) & (low <= roll_min_right)]
```

**Speedup:** ~5-10x schneller

### 2. Optimierte Meta-Labeling Loop

**Vorher:**
```r
for(j in (idx + 1):end_idx) {
  bars_elapsed <- j - idx

  if(side == 1) {
    if(dt$high[j] >= profit_target) {
      # ... einzelne Checks pro Bar
    }
  }
}
```

**Nachher (vektorisiert):**
```r
future_high <- dt$high[future_idx]
future_low <- dt$low[future_idx]

profit_hits <- which(future_high >= profit_target)
stop_hits <- which(future_low <= stop_loss_val)

# Finde ersten Hit
if(length(profit_hits) > 0 && (length(stop_hits) == 0 || profit_hits[1] < stop_hits[1])) {
  # ... direkte Berechnung
}
```

**Speedup:** ~3-5x schneller

### 3. Parallele Verarbeitung

Die Meta-Labeling Funktion kann mehrere Signale parallel verarbeiten:

```r
library(parallel)
cl <- makeCluster(n_cores)
results <- parLapply(cl, signal_indices, label_single_signal)
stopCluster(cl)
```

**Speedup:** ~2-4x zusätzlich (bei 4 Kernen)

## Dateistruktur

```
r/
├── 02_meta_labeling_extrema_signals.R           # Original Version
├── 02_meta_labeling_extrema_signals_optimized.R # Optimierte Version ✨
├── benchmark_meta_labeling.R                     # Performance Benchmark
└── main_script.R                                 # Hauptskript (updated)

labelled_data/
├── benchmark_runtime_comparison.png              # Runtime-Vergleich Plot
├── benchmark_speedup.png                         # Speedup Plot
└── benchmark_results.csv                         # Benchmark Ergebnisse
```

## Wann welche Version verwenden?

### Optimized Sequential (`parallel = FALSE`)
- **Wann:** Für die meisten Anwendungsfälle (Standard)
- **Vorteil:** Sehr schnell, kein Overhead
- **Empfohlen für:** < 50,000 Zeilen

### Optimized Parallel (`parallel = TRUE`)
- **Wann:** Bei sehr großen Datensätzen mit vielen Signalen
- **Vorteil:** Maximale Performance bei vielen Signalen (> 1000 Signale)
- **Empfohlen für:** > 50,000 Zeilen
- **Hinweis:** Hat Overhead, lohnt sich nur bei vielen Signalen

### Original Version
- **Wann:** Nur für Debugging oder Vergleichszwecke
- **Nicht empfohlen** für produktiven Einsatz

## Troubleshooting

### RcppRoll Installation fehlgeschlagen

**Windows:**
1. Installiere [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
2. Führe in R aus: `install.packages("RcppRoll")`

**macOS:**
1. Installiere Xcode Command Line Tools: `xcode-select --install`
2. Führe in R aus: `install.packages("RcppRoll")`

**Linux:**
```bash
# Ubuntu/Debian
sudo apt-get install r-base-dev

# Dann in R:
install.packages("RcppRoll")
```

### Parallelisierung funktioniert nicht

- Windows benötigt `makeCluster()` (kein fork, wird automatisch verwendet)
- Stelle sicher, dass du genug freie CPU-Kerne hast
- Prüfe mit: `parallel::detectCores()`

### Out of Memory Fehler

- Reduziere `n_cores`
- Verarbeite Datensatz in Chunks
- Setze `parallel = FALSE`

## Weitere Optimierungsmöglichkeiten

Falls die Performance immer noch nicht ausreicht:

1. **Reduziere `max_holding_bars`** - weniger Future-Bars scannen
2. **Deaktiviere RSI** (`use_rsi = FALSE`) - spart Berechnungszeit
3. **Verwende confirmation_method = "bars"** statt "derivative" oder "both"
4. **Pre-filter Daten** - nur Trading-Sessions verarbeiten

## Support

Bei Fragen oder Problemen:
1. Prüfe zuerst die Installation von RcppRoll
2. Führe das Benchmark-Skript aus, um die Performance zu messen
3. Vergleiche die Ergebnisse von Original vs. Optimized Version

---

**Autor:** Performance Optimierung
**Datum:** 2025-12-30
**Version:** 1.0
