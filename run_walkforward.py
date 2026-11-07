"""
run_walkforward.py — Walk-Forward-Test (anchored, 2 Folds).

Fold 1: schaetzen auf [0, 55 %),  pruefen auf [55 %, 70 %)  -> OS1_*
Fold 2: schaetzen auf [0, 70 %),  pruefen auf [70 %, Ende]  -> OS_*
Eine Kombination besteht nur, wenn sie zusaetzlich zu F1, F2, F4, F5, F6
in BEIDEN Pruef-Fenstern positiv ist (F8, mind. 10 Trades in Fold 1).
Das richtet sich gegen Selektionsverzerrung: Ueber tausende Kombinationen
gegen nur EIN OS-Fenster zu waehlen, findet sonst zuverlaessig Zufallstreffer.

Ergebnis: results/walkforward_<Zeitstempel>.csv
"""
import pandas as pd
import backtest_config as cfg
from pyrunner.runner import run_backtest

if __name__ == "__main__":
    results = run_backtest(
        strategies=cfg.STRATEGIES,
        instruments=cfg.INSTRUMENTS,
        data_dir=str(cfg.DATA_DIR),
        active_filters=["F1", "F2", "F4", "F5", "F6", "F8"],
        workers=cfg.WORKERS,
        max_combos=cfg.MAX_COMBOS,
        cost_bps=cfg.COST_BPS,
    )
    cfg.RESULTS_DIR.mkdir(exist_ok=True)
    out = cfg.RESULTS_DIR / f"walkforward_{pd.Timestamp.now():%Y%m%d_%H%M}.csv"
    results.to_csv(out, index=False)
    print(f"Gespeichert: {out}")
