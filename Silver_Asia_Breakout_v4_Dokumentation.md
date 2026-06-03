# Silver Asia Breakout v4 — Strategie-Dokumentation

**Instrument:** CAPITALCOM:SILVER (CFD)  
**Timeframe:** 5 Minuten  
**Strategie-Typ:** Asia Range Breakout mit Multi-Filter-Einstieg + Session Pyramid  
**Dokumentationsstand:** 24. April 2026  

---

## 1. Strategie-Übersicht

Die Silver Asia Breakout v4 ist eine intraday CFD-Breakout-Strategie auf Silber. Sie definiert täglich eine Asian Session Range (00:00–06:00 Uhr Wiener Zeit) und handelt Ausbrüche aus dieser Range mit einem mehrstufigen Filterpaket, das Trend-, Volatilitäts-, Volumen- und Divergenz-Bedingungen kombiniert.

**Kern-Idee:** Silber bildet in der illiquiden Asien-Nacht oft eine enge konsolidierte Range. Bricht der Kurs nach Beginn der europäischen Session mit Volumen und Trend-Konfluenz aus dieser Range heraus, folgt häufig eine gerichtete Bewegung bis in die New-York-Session, die mit einer fixen TP-Schwelle oder einem Time-Exit um 21:00 Uhr abgeschlossen wird.

---

## 2. Einstiegs-Logik

### 2.1 Asia Range

| Parameter | Wert |
|---|---|
| Session Start | 00:00 Uhr (Europe/Vienna) |
| Session Ende | 06:00 Uhr (Europe/Vienna) |
| Min. Range-Größe | 0 (deaktiviert) |

In dieser Session werden das Hoch (`asia_high`) und das Tief (`asia_low`) des Tages festgestellt. Die Range gilt als gesetzt, sobald die erste Nicht-Asia-Bar erscheint.

### 2.2 Breakout-Erkennung

Ein Ausbruch wird registriert, wenn der Schlusskurs über `asia_high` (Long) bzw. unter `asia_low` (Short) kreuzt. Das Signal ist für `breakout_window = 15 Bars` (= 75 Minuten) aktiv, sofern der Kurs auf der Breakout-Seite bleibt.

### 2.3 Einstiegs-Filter (alle müssen erfüllt sein)

| Filter | Long | Short |
|---|---|---|
| **Keltner Channel (15min)** | Close > KC Upper | Close < KC Lower |
| **KC-Distanz Min.** | (Close − KC Upper) / Close ≥ 0.15% | (KC Lower − Close) / Close ≥ 0.15% |
| **1H EMA-Trend** | EMA(9,1H) > EMA(21,1H) | EMA(9,1H) < EMA(21,1H) |
| **Relatives Volumen** | Volume / SMA(Volume,20) ≥ 1.1 | Volume / SMA(Volume,20) ≥ 1.1 |
| **RSI-Divergenz** | Keine bärische Divergenz | Keine bullische Divergenz |
| **Asia D2D Change** | \|Asia-Close heute − gestern\| / gestern ≤ 3% | gleich |
| **Kein offener Trade** | Max. 1 Trade pro Tag | gleich |

**RSI-Divergenz-Definition:**  
- Bärisch: Kurs nahe Periodenhoch (−0.1%) & RSI mehr als 5 Punkte unter RSI-Periodenhoch  
- Bullisch: Kurs nahe Periodentief (+0.1%) & RSI mehr als 5 Punkte über RSI-Periodentief  
- Lookback: 20 Bars (100 Minuten)

---

## 3. Exit-Logik

### 3.1 Take Profit

Fixer TP von **1.7%** bezogen auf Entry-1-Preis. Beide Legs (Entry 1 + Pyramid) werden gemeinsam geschlossen (`strategy.close_all`).

### 3.2 Time Exit

Alle offenen Positionen werden um **21:00 Uhr** (Vienna) vollständig geschlossen, wenn kein TP zuvor ausgelöst wurde.

### 3.3 Stop Loss

Initial: `max(ATR(14) × 2.5, Close × 1.5%)` — ATR-basierter Mindest-Stop mit Prozent-Floor.

Nach Pyramid-Eröffnung: Entry 1 wird auf **Break-Even** gesetzt; anschließend Trailing-Stop beider Legs bei `max(ATR × 3.0, 1.5%)` unter/über dem jeweiligen High/Low (nur wenn Trail-Level über Entry-1-Preis liegt).

### 3.4 Exit-Hierarchie

```
1. TP (1.7%) → close_all → Zeit-Exit überschrieben
2. Time Exit (21:00) → close_all
3. Stop Loss → strategy.exit (Stop-Order)
4. Trailing Stop → strategy.exit (nach Pyramid-Confirm)
```

---

## 4. Session Pyramid (15:45)

| Parameter | Wert |
|---|---|
| Prüf-Zeitpunkt | 15:45 Uhr (Vienna) |
| NY-Referenz-Zeitpunkt | 14:00 Uhr Open |
| Bedingung Long | Close > Entry 1 & Close > NY-14:00-Open & EMA bullish |
| Bedingung Short | Close < Entry 1 & Close < NY-14:00-Open & EMA bearish |
| Pyramid-Größe | Gleich Entry 1 (40 Kontrakte) |

Nach Bestätigung des zweiten Legs (≥ 2 offene Trades):  
- Entry 1 → Stop auf BE gesetzt  
- Trailing-Stop aktiv für beide Legs

---

## 5. Risiko-Parameter

| Parameter | Wert |
|---|---|
| Initial Capital | 10.000 USD |
| Order-Größe | 40 Kontrakte |
| Margin Long/Short | 10% |
| Commission | 0.01% pro Trade |
| ATR-Länge | 14 |
| SL ATR-Multiplikator | 2.5 |
| Min. SL | 1.5% des Preises |
| Take Profit | 1.7% von Entry 1 |
| Trailing ATR-Mult | 3.0 |
| Pyramiding | 2 Orders max |

---

## 6. Backtest-Ergebnisse (TradingView)

**Backtesting-Zeitraum:** 25. April 2025 – 24. April 2026  
**Symbol:** CAPITALCOM:SILVER, 5min, Europe/Vienna

### 6.1 Performance-Übersicht

| Kennzahl | Gesamt | Long | Short |
|---|---|---|---|
| Nettogewinn (USD) | **985,08** | 660,98 | 324,10 |
| Nettogewinn (%) | **9,85%** | 6,61% | 3,24% |
| Bruttogewinn (USD) | 2.529,75 | 1.388,65 | 1.141,11 |
| Bruttoverlust (USD) | 1.544,67 | 727,67 | 817,00 |
| Erwarteter Payoff | 6,16 | 8,16 | 4,10 |
| Gezahlte Kommission | 66,04 | 35,26 | 30,78 |
| CAGR | 9,82% | 6,59% | 3,23% |
| Account Required | 787,19 USD | — | — |
| Return on Account Required | **125,14%** | 83,97% | 41,17% |

### 6.2 Trade-Statistiken

| Kennzahl | Gesamt | Long | Short |
|---|---|---|---|
| Anzahl Trades | **160** | 81 | 79 |
| Gewinner | 80 | 47 | 33 |
| Verlierer | 80 | 34 | 46 |
| **Win Rate** | **50,0%** | **58,0%** | **41,8%** |
| Ø Gewinn-Trade | 31,62 USD | 29,55 USD | 34,58 USD |
| Ø Verlust-Trade | 19,31 USD | 21,40 USD | 17,76 USD |
| Win/Loss-Ratio | **1,64** | 1,38 | 1,95 |
| Größter Gewinn | 80,72 USD | 80,72 USD | 66,10 USD |
| Größter Verlust | 72,59 USD | 72,59 USD | 52,58 USD |
| Ø Bars pro Trade | 73 | 78 | 68 |

### 6.3 Risikoadjustierte Kennzahlen

| Kennzahl | Wert |
|---|---|
| **Profit Factor** | **1,638** |
| Profit Factor Long | 1,908 |
| Profit Factor Short | 1,397 |
| Sharpe Ratio | 0,576 |
| Sortino Ratio | 1,655 |
| Max. Drawdown (Intrabar) | 163,04 USD / **1,63%** |
| Max. Drawdown (Close-to-Close) | 141,16 USD / 1,41% |
| Ø Drawdown-Dauer | 17 Tage |
| Max. Run-Up (Intrabar) | 1.158,14 USD / 10,46% |
| Nettogewinn / größter Verlust | 1.357% |

### 6.4 Jahresaufschlüsselung

| Jahr | Trades | Win Rate | Nettogewinn (USD) | Profit Factor |
|---|---|---|---|---|
| 2025 | 123 | 46,3% | 268,51 | 1,28 |
| 2026 (Jan–Apr) | 37 | 62,2% | 716,62 | 2,22 |
| **Gesamt** | **160** | **50,0%** | **985,13** | **1,64** |

> ⚠️ **Hinweis AYP:** Der Backtesting-Zeitraum umfasst lediglich ~12 Monate (ab April 2025). Eine belastbare All-Years-Profitable-Prüfung erfordert mindestens 3–5 vollständige Kalenderjahre. Die Python-Backtests sollten auf den vollen SILVER_MINUTE_5.csv-Datensatz (Feb 2024 – März 2026) ausgeweitet werden.

### 6.5 Exit-Analyse

| Exit-Typ | Anzahl | Anteil |
|---|---|---|
| Time Exit (21:00) | 58 | 36,3% |
| TP Exit (1.7%) | 55 | 34,4% |
| Stop Loss X_S1/X_L1 (Entry 1) | 44 | 27,5% |
| Stop Loss X_L2/X_S2 (Pyramid) | 3 | 1,9% |

---

## 7. Implementierungsparameter (optimierte Konfiguration)

```
Asia Session:       00:00 – 06:00 (Europe/Vienna)
Breakout Window:    15 Bars (75 min)
Volume Threshold:   1.1× SMA(20)
KC Length:          15 (auf 15min-TF)
KC Multiplier:      1.3
Min KC Distance:    0.15%
1H EMA Fast:        9
1H EMA Slow:        21
RSI Length:         14
RSI Div Lookback:   20 Bars
Max Asia D2D:       3.0%
ATR Length:         14
SL ATR Mult:        2.5×
Min SL:             1.5%
Take Profit:        1.7%
Pyramid Zeit:       15:45 (NY-Range: 14:00 Referenz)
Trail ATR Mult:     3.0×
Time Exit:          21:00 (Europe/Vienna)
```

---

## 8. Lookahead-Audit

| Komponente | Risiko | Status |
|---|---|---|
| `ta.atr`, `ta.sma`, `ta.rsi` (5min) | Kein Lookahead | ✅ Sauber |
| `request.security(..., "15", ..., lookahead_off)` | Gibt letzten *abgeschlossenen* 15min-Bar zurück | ⚠️ HTF-Shift (1 Bar) |
| `request.security(..., "60", ..., lookahead_off)` | Gibt letzten *abgeschlossenen* 1H-Bar zurück | ⚠️ HTF-Shift (1 Bar) |
| `barstate.isconfirmed` | Alle Signale nur auf bestätigten Bars | ✅ Korrekt |
| `ta.crossover/crossunder` | Auf 5min Close, bestätigt | ✅ Sauber |
| Asia Range `var` Tracking | Session-akkumuliert, kein Lookahead | ✅ Sauber |
| D2D Change Tracking | Nutzt vorherige Session-Close-Variable | ✅ Sauber |

> **Bekanntes Residualrisiko:** KC Upper/Lower (15min) und 1H EMAs werden mit `lookahead_off` abgerufen, liefern aber den Wert des *letzten abgeschlossenen HTF-Bars*. In Python-Backtests muss entsprechend geshiftet werden (`shift(1)` nach Resample). Quantifizierter Impact auf diese Strategie: ausstehend.

---

## 9. PineScript v6 — Vollständiger Quellcode

```pine
// =============================================================================
// Silver Asia Range Breakout v4 — TP + TIME EXIT
// PineScript v6 | 5min Chart | Silver CFDs
// =============================================================================
//@version=6
strategy("Silver Asia Breakout v4", overlay=true,
         margin_long=10, margin_short=10,
         pyramiding=2,
         commission_type=strategy.commission.percent, commission_value=0.02,
         default_qty_type=strategy.percent_of_equity, default_qty_value=50,
         calc_on_every_tick=false)

// SEKTION 1: INPUTS
string tz              = input.string("Europe/Vienna", "Timezone", group="Session")
int asia_start_hour    = input.int(0, "Asia Range Start (Hour)", group="Session")
int asia_end_hour      = input.int(6, "Asia Range End (Hour)", group="Session")
int pyr_check_hour     = input.int(15, "Pyramid Check Hour", group="Session")
int pyr_check_minute   = input.int(45, "Pyramid Check Minute", group="Session")
int exit_hour          = input.int(21, "Time Exit Hour", group="Session")
int exit_minute        = input.int(0, "Time Exit Minute", group="Session")

int vol_lookback       = input.int(20, "Volume SMA Lookback", group="Entry Filters")
float vol_threshold    = input.float(1.1, "Relative Volume Threshold", group="Entry Filters")
int kc_length          = input.int(15, "Keltner Channel Length (15min)", group="Entry Filters")
float kc_mult          = input.float(1.3, "Keltner Channel Mult", group="Entry Filters")
int ma_fast_len        = input.int(9, "1H Fast EMA Length", group="Entry Filters")
int ma_slow_len        = input.int(21, "1H Slow EMA Length", group="Entry Filters")
float min_range        = input.float(0.0, "Min Asia Range Size (0=off)", group="Entry Filters")
int breakout_window    = input.int(15, "Breakout Window (Bars)", group="Entry Filters")
float min_kc_dist_pct  = input.float(0.15, "Min KC Distance %", group="Entry Filters")
int rsi_length         = input.int(14, "RSI Length", group="Entry Filters")
int div_lookback       = input.int(20, "RSI Divergence Lookback (bars)", group="Entry Filters")
float max_dtd_pct      = input.float(3.0, "Max Asia D2D Change %", group="Entry Filters")

int atr_length         = input.int(14, "ATR Length", group="Risk")
float sl_atr_mult      = input.float(2.5, "SL ATR Multiplier", group="Risk")
float min_sl_pct       = input.float(1.5, "Min SL % of Price", group="Risk")
float tp_pct           = input.float(1.7, "Take Profit % (on Entry 1)", group="Risk")
float trail_atr_mult   = input.float(3.0, "Trailing ATR Mult (after pyramid)", group="Risk")
bool use_pyramid       = input.bool(true, "Enable Session Pyramid", group="Pyramiding")

// [... vollständiger Strategie-Code — siehe Sektion 9.1 unten ...]
```

> Der vollständige PineScript v6 Quellcode ist in der separaten Datei `Silver_Asia_Breakout_v4.txt` enthalten (TradingView-kompatibel, direkt einfügbar).

---

## 10. Python Webhook-Bot

Der Webhook-Bot (FastAPI) empfängt TradingView-Alerts und führt Orders auf Capital.com aus.

### 10.1 Architektur

```
TradingView Alert → POST /webhook → FastAPI Bot → Capital.com API
```

### 10.2 Silver-Konfiguration

```python
SYMBOL_EPIC_MAP = {
    "SILVER": {"epic": "SILVER", "size": 44}
}
```

**Order-Größe:** 44 Kontrakte (Capital.com-Lot-Anpassung für Silver CFD)

### 10.3 Unterstützte Intents

| Intent | Beschreibung | Silver-Verhalten |
|---|---|---|
| `open` | Initialer Einstieg | 44 Kontrakte, mit Stop Level |
| `pyramid` | Zweiter Einstieg (15:45) | Weitere 44 Kontrakte, `forceOpen=True` |
| `modify_sl` | Stop-Loss anpassen (BE nach Pyramid) | PUT auf älteste Position |
| `close` | Vollständiger Exit | DELETE alle Positionen für SILVER |
| `close_partial` | Teilschließung (aktuell nicht genutzt) | Prozentuale Schließung |

### 10.4 Alert-JSON-Format

**Entry (Long-Beispiel):**
```json
{
  "symbol": "SILVER",
  "action": "buy",
  "intent": "open",
  "reason": "enter_long",
  "stop_loss": 31.45
}
```

**Pyramid (Short-Beispiel):**
```json
{
  "symbol": "SILVER",
  "action": "sell",
  "intent": "pyramid",
  "reason": "session_pyramid",
  "stop_loss": 34.12
}
```

**Break-Even-Anpassung:**
```json
{
  "symbol": "SILVER",
  "action": "modify",
  "intent": "modify_sl",
  "reason": "be_entry1",
  "stop_loss": 33.18
}
```

**Exit:**
```json
{
  "symbol": "SILVER",
  "action": "sell",
  "intent": "close",
  "reason": "tp_exit",
  "size": "all"
}
```

### 10.5 Idempotenz & Session-Management

- Jedes Signal wird per `signal_id` dedupliziert (JSON-Store, 2-Tage-TTL)  
- Session-Token (CST, X-SECURITY-TOKEN) wird automatisch erneuert bei 401-Fehlern  
- Alle Aktionen werden in `webhook_log.txt` geloggt

---

## 11. Datenbasis

### 11.1 SILVER_MINUTE_5.csv (Projektordner)

| Attribut | Wert |
|---|---|
| Quelle | Capital.com (5min OHLCV) |
| Zeitraum | 22. Februar 2024 – 05. März 2026 |
| Zeilen | 144.058 Bars |
| Spalten | time, open, close, high, low, volume |
| Verwendung | Python-Backtest, Grid-Optimierung |

### 11.2 TradingView Backtest-Export

| Datei | Inhalt |
|---|---|
| `Silver_Asia_Breakout_v4_..._c559e.csv` | Trade-Liste (160 Exits, alle Felder) |
| `Silver_Asia_Breakout_v4_..._0eb5f.xlsx` | Performance, Trade Analysis, Risk, Properties |

---

## 12. Bekannte Risiken & offene Punkte

| Thema | Details | Status |
|---|---|---|
| **HTF Lookahead (KC, EMA)** | 15min KC und 1H EMA könnten in Python-Backtests 1 Bar versetzt sein | ⚠️ Quantifizierung ausstehend |
| **Kurzer Backtestzeitraum** | Nur ~12 Monate (Apr 2025–Apr 2026) → kein AYP über mehrere Jahre | ⚠️ Erweiterung auf vollen CSV-Zeitraum ausstehend |
| **Short-Seite schwächer** | WR 41.8%, PF 1.40 vs. Long PF 1.91 → strukturell asymmetrisch | ℹ️ Bekannt, akzeptiert |
| **Pyramiding in TradingView** | strategy.exit-Verhalten bei mehreren offenen Orders komplex | ⚠️ Live-Test empfohlen |
| **Spread-Kosten Silver** | Commission 0.01% im Backtest; realer Spread kann abweichen | ℹ️ Spread-Adjusted-PnL prüfen |

---

## 13. Korrelation zu anderen Portfolio-Strategien

| Strategie | Instrument | Korrelation zu Silver ARB |
|---|---|---|
| Gold ARB | Gold | Mittel-hoch (beide Edelmetalle, ähnliche Einstiegslogik) |
| DE40→US500 Lead-Lag | US500 | Gering (Cross-Asset, anderes Timing) |
| US500 Afternoon Fade | US500 | Gering (Mean Reversion, 15:00–21:00) |
| EU50 Gap Fade | EU50 | Gering (Gap-Fade, anderes Timing) |
| Brent First Hour Fade | Brent Oil | Gering (Rohstoff, aber Oil ≠ Metals) |

> **Hinweis:** Gold ARB und Silver ARB teilen dieselbe Strategie-Architektur und handeln in ähnlichen Zeitfenstern → beide Positionen gleichzeitig erhöhen das konzentrierte Edelmetall-Exposure im Portfolio erheblich.

---

*Dokumentation erstellt am 24. April 2026 | Capital.com CFD | TradingView PineScript v6*
