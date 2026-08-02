# Strategien — überarbeitet (SL/TP/Overnight korrekt)

## Was geändert wurde

Der fehlerhafte Second-Pass (`entry_bar` + `nafill(locf)`) wurde in allen 12
betroffenen Strategien entfernt und durch einen gemeinsamen, sequentiellen
FSM-Barrier-Applikator ersetzt: `.apply_sl_tp_fsm()`.

Diese Funktion ist in JEDER Datei eingebettet (guarded via
`if (!exists(...))`), sodass die Dateien plugin-eigenständig bleiben. Eine
freistehende Referenzkopie liegt zusätzlich als `_fsm_barriers.R` bei.

### Behobene Bugs
1. **Zombie-Re-Entry**: Position lebte nach einem SL/TP-Hit wieder auf
   (auf echten Gold-Daten in 7,1 % aller Stop-Hits). Jetzt: nach Stop flach,
   bis das Rohsignal die Richtung verlässt (Lockout).
2. **Stale Barrieren**: SL/TP wurden bei jedem Entry frisch gesetzt statt per
   LOCF aus dem letzten Entry weitergeschleppt.
3. **Overnight**: konsistente UTC-Tageswechsel-Glattstellung im FSM.

### Verifiziert (Python-Port auf GOLD_MINUTE_5)
- Bars-in-Position variieren klar mit SL/TP (Sensitivität wiederhergestellt).
- 0 Zombie-Re-Entries.

## Dateien

### Korrigiert (12)
asia_breakout, atr_distance_mean_reversion, bollinger_zscore_mean_reversion,
cci_mean_reversion, cmo_vhf_stc, donchian_crossover, gap_fade, ichimoku_triple,
rsi_mean_reversion, silver_asia_breakout_v4_vectorized, tokyo_gap_range,
vwap_mean_reversion

### Neu — Commodities (5)
- **supertrend_atr** — ATR-getrailte Trendfolge (immer long/short)
- **keltner_squeeze_breakout** — Vola-Kompression → Expansion (TTM-Squeeze)
- **session_orb** — Opening Range Breakout (London/NY), intraday, flat zum Ende
- **macd_trend_momentum** — MACD-Cross mit langsamem EMA-Trendfilter
- **trend_rsi_pullback** — Trendfolge mit antizyklischem RSI-Pullback-Entry

## FSM-Policy (wichtig für R↔cBot-Konsistenz)
- Overnight: glattstellen. `overnight_lockout=FALSE` Default (MR darf morgens
  wieder einsteigen). Für die LOCF-Breakouts asia/silver: `TRUE` (kein Carry).
- SL/TP-Hit: glattstellen + Lockout der gestoppten Richtung bis das Rohsignal
  sie verlässt → kein sofortiges Wieder-Ausstoppen am selben Level.
- SL hat Vorrang vor TP bei gleichzeitigem Intrabar-Treffer.

## NICHT angefasst
`sma_crossover.R` und `toyota_breakout.R` waren bereits korrekt (integrierter
FSM-Loop). Sie nutzen jedoch eine etwas andere Re-Entry-Semantik (sofortiger
Re-Entry statt Lockout). Für 100 % Konsistenz mit den 17 hier sollten sie auf
`.apply_sl_tp_fsm()` migriert werden — sag Bescheid, dann ziehe ich das nach.

## Vor dem nächsten Run
- Smoke-Test pro Datei: `generate_signals(df, …, sl_atr_mult=1.5,…)` vs
  `…sl_atr_mult=2.5,…` muss unterschiedliche `Position` liefern.
- `atr_period`-Test: 10 vs 14 muss sich unterscheiden.
- Danach erst MCPT/DSR. Die alten Ergebnisse der 12 Strategien sind ungültig.
