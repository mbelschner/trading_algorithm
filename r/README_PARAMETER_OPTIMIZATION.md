# Parameter-Optimierung für Triple Barrier Labeling

## Übersicht

Die `optimize_labeling_parameters()` Funktion testet verschiedene Kombinationen von:
- **ATR Period**: Wie viele Bars für ATR-Berechnung (Volatilität)
- **ATR Multiplier**: Wie weit die Barrieren vom Entry-Preis entfernt sind
- **Max Horizon**: Maximale Haltedauer in Bars

## Optimierungskriterien

### 1. **Uniqueness Score** (HAUPTKRITERIUM - höher = besser)

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

---

### 2. **Balance Score** (ZWEITKRITERIUM - niedriger = besser)

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

---

### 3. **Concurrent Labels** (Information)

**Was es misst:**
- Durchschnittliche Anzahl gleichzeitig aktiver Labels
- Direkt verwandt mit Uniqueness: avg_uniqueness = 1 / mean_concurrent

**Warum wichtig?**
- Zeigt die "Dichte" der Labels
- Niedrigere Werte = weniger Overlap

---

### 4. **Vertical Ratio** (Information)

**Was es misst:**
- Anteil der Labels die durch Zeit-Ablauf entstanden (nicht Preis-Bewegung)
- **0.1** = 10% Timeouts → viele echte Signale ✓
- **0.7** = 70% Timeouts → zu konservative Parameter ✗

---

## Verwendung

### Standard: Sortierung nach Uniqueness

```r
opt_params <- optimize_labeling_parameters(
  prices = dt_small,
  atr_periods = c(10, 14, 20),
  atr_mults = c(1.5, 2, 2.5, 3),
  max_horizons = c(8, 11, 14, 17),
  sort_by = "uniqueness"  # Default
)
```

**Ergebnis:** Top-Parameter haben höchste Uniqueness, dann beste Balance

---

### Alternative: Sortierung nach Balance

```r
opt_params <- optimize_labeling_parameters(
  prices = dt_small,
  sort_by = "balance"
)
```

**Ergebnis:** Top-Parameter haben beste Balance, dann höchste Uniqueness

---

### Kombiniert: Gewichteter Score

```r
opt_params <- optimize_labeling_parameters(
  prices = dt_small,
  sort_by = "combined"
)
```

**Gewichtung:** 70% Uniqueness + 30% Balance

---

## Interpretation der Ergebnisse

### Ideale Werte:

| Metrik | Ideal-Bereich | Bedeutung |
|--------|---------------|-----------|
| **avg_uniqueness** | 0.7 - 1.0 | Wenig Overlap |
| **balance_score** | 0.0 - 0.15 | Gut balanciert |
| **mean_concurrent** | 1.0 - 1.5 | Niedrige Überlappung |
| **vertical_ratio** | 0.1 - 0.3 | Genug echte Signale |
| **n_samples** | >1000 | Genug Trainingsdaten |

---

### Beispiel-Output:

```
=== TOP 10 PARAMETER-KOMBINATIONEN ===
   atr_period atr_mult max_horizon n_samples avg_uniqueness balance_score mean_concurrent vertical_ratio
1:         14      3.0          10      2145         0.8523        0.0421             1.2          0.185
2:         14      2.5          10      2287         0.8341        0.0534             1.3          0.223
3:         20      3.0          11      2098         0.8124        0.0398             1.3          0.197
4:         14      3.0          14      2412         0.7891        0.0512             1.4          0.241
...
```

**Beste Kombination (Zeile 1):**
- ATR Period = 14, Multiplier = 3.0, Horizon = 10
- Uniqueness = 0.85 → Sehr gut! Wenig Overlap
- Balance Score = 0.04 → Sehr gut balanciert
- Mean Concurrent = 1.2 → Im Durchschnitt nur 1.2 Labels gleichzeitig aktiv
- Vertical Ratio = 0.19 → 81% echte Preis-Signale

---

## Trade-offs verstehen

### Kürzerer Horizon vs. Längerer Horizon

**Kurz (z.B. 8-10 bars):**
- ✓ Höhere Uniqueness (weniger Overlap)
- ✓ Schnellere Entscheidungen
- ✗ Möglicherweise mehr Noise

**Lang (z.B. 20-24 bars):**
- ✗ Niedrigere Uniqueness (mehr Overlap)
- ✓ Klarere Trends
- ✓ Weniger Noise

### Höherer ATR Multiplier vs. Niedrigerer

**Hoch (z.B. 3.0):**
- ✓ Klarere Signale (größere Bewegungen nötig)
- ✗ Weniger Samples
- ✗ Mehr Vertical Barrier Hits

**Niedrig (z.B. 1.5):**
- ✓ Mehr Samples
- ✗ Mehr Noise (kleine Bewegungen zählen schon)
- ✓ Schnellere Barrier Hits

---

## Empfohlener Workflow

1. **Starte mit großem Grid:**
   ```r
   opt_params <- optimize_labeling_parameters(
     prices = dt_small,  # Kleiner Datensatz für Speed
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
     prices = dt_small,
     atr_periods = c(12, 14, 16),
     atr_mults = c(2.5, 3.0, 3.5),
     max_horizons = c(8, 10, 12),
     sort_by = "uniqueness"
   )
   ```

4. **Teste auf vollem Datensatz:**
   ```r
   final_labeled <- create_triple_barrier_labels(
     prices = dt,  # Voller Datensatz
     atr_period = best$atr_period,
     atr_mult_barrier = best$atr_mult,
     max_horizon_bars = best$max_horizon
   )
   ```

---

## Performance-Tipp

Die Optimierung nutzt automatisch die **optimierte Version** von `create_triple_barrier_labels()`.

Bei großem Grid (z.B. 60 Kombinationen):
- Mit 1 Jahr Daten: ~5-10 Minuten
- Mit kleinerem Subset: ~1-2 Minuten

→ Empfehlung: Starte mit `dt_small` für schnelle Iteration!
