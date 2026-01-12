# Additional Markets Feature - Benutzerhandbuch

## Überblick

Die neue **Additional Markets**-Funktionalität ermöglicht es, technische Indikatoren von zusätzlichen Märkten (z.B. DXY, VIX, SILVER) als Features in die Backtest-Pipeline zu integrieren.

## Features

Für jeden zusätzlichen Markt werden folgende technische Indikatoren berechnet:

1. **Close Price** - Schlusskurs
2. **Volume Indicators** - Handelsvolumen-basierte Features
   - **Volume** - Rohes Volumen
   - **Volume SMA 20** - 20-Perioden gleitender Durchschnitt des Volumens
   - **Volume Ratio** - Aktuelles Volumen / Volume SMA (Verhältnis)
   - **Volume ROC 5** - 5-Perioden Rate of Change des Volumens
   - **Volume ROC 10** - 10-Perioden Rate of Change des Volumens
   - **OBV** - On Balance Volume (kumulatives volumengewichtetes Momentum)
   - **VPT** - Volume Price Trend (volumengewichtete Preisänderung)
3. **ATR 14** - Average True Range (14 Perioden)
4. **RSI 14** - Relative Strength Index (14 Perioden)
5. **MACD** - Moving Average Convergence Divergence
   - MACD Line
   - Signal Line
   - MACD Difference (MACD - Signal)
6. **ADX 14** - Average Directional Index (14 Perioden)
   - DI+ (Directional Indicator Plus)
   - DI- (Directional Indicator Minus)

### Lag Features

Von jedem Indikator werden Lag-Features erstellt:
- **Lag 1** - Wert von 1 Periode zurück
- **Lag 4** - Wert von 4 Perioden zurück
- **Lag 8** - Wert von 8 Perioden zurück
- **Lag 20** - Wert von 20 Perioden zurück

**Beispiel:** `DXY_rsi_14_lag4` = RSI 14 von DXY vor 4 Perioden

### Feature-Anzahl pro Markt

Pro zusätzlichem Markt werden erstellt:
- **16 Base-Indikatoren**:
  - Preis: close
  - Volumen: volume, volume_sma_20, volume_ratio, volume_roc_5, volume_roc_10, obv, vpt
  - Trend/Momentum: atr_14, rsi_14, macd, macd_signal, macd_diff
  - Trend Stärke: adx_14, di_plus_14, di_minus_14
- **64 Lag-Features**: 16 Base-Indikatoren × 4 Lags (1, 4, 8, 20)
- **Gesamt: ~80 Features pro Markt**

## Konfiguration

### 1. Backtest Script (02_backtest_main_script_ls_v2.R)

Öffne `r/02_backtest_main_script_ls_v2.R` und passe die folgenden Parameter an:

```r
# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"

# Additional markets for feature analysis
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")  # Set to NULL or c() to disable
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)  # Lag periods for additional market features

# Feature caching
FORCE_RECALCULATE_FEATURES <- TRUE  # Set to TRUE to rebuild feature cache with new markets
```

#### Verfügbare Märkte

Aktuell verfügbare zusätzliche Märkte (basierend auf heruntergeladenen Daten):
- **DXY** - US Dollar Index
- **VIX** - Volatility Index
- **SILVER** - Silber

#### Konfigurationsoptionen

**Alle Märkte aktivieren:**
```r
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")
```

**Nur bestimmte Märkte:**
```r
ADDITIONAL_MARKETS <- c("DXY", "VIX")  # Nur DXY und VIX
```

**Zusätzliche Märkte deaktivieren:**
```r
ADDITIONAL_MARKETS <- NULL
# oder
ADDITIONAL_MARKETS <- c()
```

**Lag-Perioden anpassen:**
```r
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)  # Standard
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 5, 10)     # Weniger Features
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 2, 4, 8, 16, 32)  # Mehr Features
```

### 2. Feature Cache neu berechnen

**WICHTIG:** Nach dem Hinzufügen oder Ändern von zusätzlichen Märkten muss der Feature-Cache neu berechnet werden:

```r
FORCE_RECALCULATE_FEATURES <- TRUE
```

Dies stellt sicher, dass die neuen Market-Features in die Feature-Matrix einbezogen werden.

## Verwendung

### 1. Backtest mit zusätzlichen Märkten ausführen

```r
# In RStudio oder R Console
source("r/02_backtest_main_script_ls_v2.R")
```

Das Script wird:
1. Die konfigurierten zusätzlichen Märkte laden
2. Technische Indikatoren für jeden Markt berechnen
3. Lag-Features erstellen
4. Alle Features mit dem Haupt-Dataset (GOLD) mergen
5. Feature Selection durchführen (die besten Features aus allen Märkten werden automatisch ausgewählt)
6. Modelle trainieren und evaluieren

### 2. Test der Additional Markets Module

Ein separates Test-Script steht zur Verfügung:

```r
source("r/test_additional_markets.R")
```

Dieses Script testet:
- Laden der zusätzlichen Marktdaten
- Berechnung der technischen Indikatoren
- Erstellung der Lag-Features
- Merge mit dem Haupt-Dataset
- Überprüfung der Feature-Namen

## Architektur

### Module

#### 1. `02_01b_additional_markets.R`

Hauptmodul für zusätzliche Märkte mit folgenden Funktionen:

- **`load_additional_markets()`** - Lädt und verarbeitet alle konfigurierten Märkte
- **`calculate_market_indicators()`** - Berechnet technische Indikatoren für einen Markt
- **`create_market_lags()`** - Erstellt Lag-Features für einen Markt
- **`merge_additional_markets()`** - Merged zusätzliche Märkte mit Haupt-Dataset

#### 2. `02_backtest_main_script_ls_v2.R`

Haupt-Backtest-Script, erweitert um:
- Konfigurationsparameter für zusätzliche Märkte
- Integration der zusätzlichen Märkte in den Feature-Berechnungsprozess
- Automatische Feature Selection über alle Märkte hinweg

### Datenfluss

```
1. Lade Price Data für jeden zusätzlichen Markt
   └─> DXY_MINUTE_15.csv
   └─> VIX_MINUTE_15.csv
   └─> SILVER_MINUTE_15.csv

2. Berechne technische Indikatoren
   └─> DXY_close, DXY_volume, DXY_volume_ratio, DXY_obv, DXY_vpt
   └─> DXY_atr_14, DXY_rsi_14, DXY_macd, DXY_adx_14, ...
   └─> VIX_close, VIX_volume, VIX_volume_ratio, VIX_obv, ...
   └─> SILVER_close, SILVER_volume, SILVER_volume_ratio, ...

3. Erstelle Lag-Features
   └─> DXY_close_lag1, DXY_close_lag4, DXY_close_lag8, DXY_close_lag20
   └─> DXY_volume_ratio_lag1, DXY_volume_ratio_lag4, ...
   └─> DXY_atr_14_lag1, DXY_atr_14_lag4, ...

4. Merge alle Märkte (Inner Join auf datetime)
   └─> Kombinierte Feature-Matrix mit allen Märkten

5. Merge mit GOLD Haupt-Features (Left Join)
   └─> Finale Feature-Matrix für Modelltraining

6. Feature Selection (XGBoost + Boruta)
   └─> Automatische Auswahl der besten Features aus allen Märkten
```

## Feature Selection

Die Feature Selection (Walk-Forward XGBoost + Boruta) wählt automatisch die besten Features aus:
- **GOLD-Features** (technische Indikatoren, Lags, etc.)
- **Zusätzliche Market-Features** (DXY, VIX, SILVER)

Die relevantesten Features werden unabhängig von ihrer Herkunft ausgewählt. Dies ermöglicht es dem Modell, Cross-Market-Beziehungen zu nutzen.

**Beispiel:** Wenn VIX-Volatilität ein starker Prädiktor für GOLD-Bewegungen ist, wird `VIX_atr_14` oder `VIX_close_lag4` automatisch in die finalen Features aufgenommen.

## Erwartete Feature-Anzahl

**Ohne zusätzliche Märkte:**
- GOLD Features: ~500-800 Features

**Mit 3 zusätzlichen Märkten (DXY, VIX, SILVER):**
- GOLD Features: ~500-800
- DXY Features: ~80 (16 base + 64 lags)
- VIX Features: ~80 (16 base + 64 lags)
- SILVER Features: ~80 (16 base + 64 lags)
- **Gesamt: ~740-1040 Features**

Nach Feature Selection:
- Stage 1 (XGBoost): **50 Features**
- Stage 2 (Boruta): **15 finale Features**

## Performance-Überlegungen

### Rechenzeit

- **Laden und Berechnen der zusätzlichen Märkte:** +30-60 Sekunden
- **Feature Selection mit mehr Features:** +2-5 Minuten (wegen größerer Feature-Matrix)
- **Gesamte Pipeline:** ~30-45 Minuten (je nach Hardware)

### Feature Cache

Der Feature Cache (`feature_cache/GOLD_MINUTE_15_features_all.csv`) speichert:
- Alle GOLD-Features
- Alle zusätzlichen Market-Features
- Alle Lags

**Cache-Größe:** ~100-200 MB (je nach Datenmenge und Features)

## Troubleshooting

### Problem: "No markets were loaded"

**Ursache:** CSV-Dateien für zusätzliche Märkte nicht gefunden

**Lösung:**
1. Überprüfe, ob die Dateien existieren:
   ```r
   list.files("price_data", pattern = "DXY|VIX|SILVER")
   ```
2. Stelle sicher, dass die Dateinamen dem Muster folgen: `{MARKET}_{INTERVAL}.csv`
3. Lade fehlende Daten mit `python/get_data_from_capitalcomAPI.py` herunter

### Problem: "WARNING: X rows have NA values in additional markets"

**Ursache:** Zusätzliche Märkte haben unterschiedliche Zeiträume oder Lücken in den Daten

**Lösung:**
- Dies ist normal, wenn Märkte unterschiedliche Handelzeiten haben
- Die Pipeline verwendet `na.omit()`, um Zeilen mit NAs zu entfernen
- Überprüfe die Datenqualität der zusätzlichen Märkte:
  ```r
  dt_dxy <- fread("price_data/DXY_MINUTE_15.csv")
  summary(dt_dxy)
  ```

### Problem: Feature Cache wird nicht aktualisiert

**Ursache:** `FORCE_RECALCULATE_FEATURES = FALSE`

**Lösung:**
```r
FORCE_RECALCULATE_FEATURES <- TRUE  # Im Backtest Script setzen
```

### Problem: Zu viele NAs nach Merge

**Ursache:** Zeiträume der zusätzlichen Märkte überlappen nicht mit GOLD

**Lösung:**
1. Überprüfe Zeiträume:
   ```r
   dt_gold <- fread("price_data/GOLD_MINUTE_15.csv")
   dt_dxy <- fread("price_data/DXY_MINUTE_15.csv")

   range(dt_gold$datetime)
   range(dt_dxy$datetime)
   ```
2. Lade überlappende Zeiträume herunter

## Beispiel-Output

```
=== CONFIGURATION ===
  Label Version: enhanced_neutral
  Train Period: 2019-2024
  Test Period: 2025
  Additional Markets: DXY, VIX, SILVER
  Additional Markets Lag Periods: 1, 4, 8, 20

=== LOADING ADDITIONAL MARKETS ===

--- Processing DXY ---
  Loading: price_data/DXY_MINUTE_15.csv
  Rows: 124,532
  Calculating indicators...
  - DXY_Volume Indicators
  - DXY_ATR_14
  - DXY_RSI_14
  - DXY_MACD
  - DXY_ADX_14
  Creating lag features...
  ✓ 80 features created for DXY

--- Processing VIX ---
  Loading: price_data/VIX_MINUTE_15.csv
  Rows: 124,532
  Calculating indicators...
  - VIX_Volume Indicators
  - VIX_ATR_14
  - VIX_RSI_14
  - VIX_MACD
  - VIX_ADX_14
  Creating lag features...
  ✓ 80 features created for VIX

--- Processing SILVER ---
  Loading: price_data/SILVER_MINUTE_15.csv
  Rows: 124,532
  Calculating indicators...
  - SILVER_Volume Indicators
  - SILVER_ATR_14
  - SILVER_RSI_14
  - SILVER_MACD
  - SILVER_ADX_14
  Creating lag features...
  ✓ 80 features created for SILVER

--- Merging all markets ---
✓ Additional markets merged: 124,532 rows, 240 features
  Markets: DXY, VIX, SILVER

=== MERGING ADDITIONAL MARKETS WITH MAIN DATASET ===
Main dataset: 125,847 rows
Additional markets: 124,532 rows, 240 features
✓ Merged: 125,847 rows, 872 total columns
```

## Best Practices

1. **Start mit wenigen Märkten:** Beginne mit 1-2 zusätzlichen Märkten, um die Performance zu testen
2. **Überprüfe Datenqualität:** Stelle sicher, dass zusätzliche Märkte hochwertige, vollständige Daten haben
3. **Cache Management:** Lösche alte Caches, wenn du Konfigurationen änderst
4. **Feature Importance:** Nach dem Training, überprüfe welche zusätzlichen Market-Features wichtig sind:
   ```r
   print(importance_long)  # Long Model
   print(importance_short)  # Short Model
   ```
5. **Cross-Market-Korrelationen:** Analysiere Korrelationen zwischen Märkten vor dem Training:
   ```r
   cor(dt_features_all[, .(GOLD_close, DXY_close, VIX_close, SILVER_close)])
   ```

## Weitere Märkte hinzufügen

Um weitere Märkte hinzuzufügen:

1. **Daten herunterladen:**
   ```python
   # In python/get_data_from_capitalcomAPI.py
   # Füge neue Märkte zur Liste hinzu
   ```

2. **Konfiguration anpassen:**
   ```r
   ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER", "EURUSD", "SP500")
   ```

3. **Feature Cache neu berechnen:**
   ```r
   FORCE_RECALCULATE_FEATURES <- TRUE
   ```

## Zusammenfassung

Die Additional Markets-Funktionalität ermöglicht es, Cross-Market-Beziehungen zu nutzen und potenziell die Vorhersagegenauigkeit zu verbessern, indem:
- Makro-Indikatoren (DXY) Dollar-Stärke erfassen
- Volatilitäts-Indikatoren (VIX) Marktunsicherheit messen
- Korrelierte Assets (SILVER) ähnliche Bewegungsmuster haben

Die Features werden automatisch in der Feature Selection berücksichtigt, sodass nur die relevantesten Cross-Market-Beziehungen in das finale Modell einfließen.
