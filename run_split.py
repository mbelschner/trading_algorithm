"""
run_split.py — normaler Test: In-Sample 70 % / Out-of-Sample 30 %.

Filter F1, F2, F4, F5, F6 bewerten IS und das letzte OS-Fenster.
Ergebnis: results/split_<Zeitstempel>.csv
"""
import pandas as pd
import backtest_config as cfg
from pyrunner.runner import run_backtest

if __name__ == "__main__":
    results = run_backtest(
        strategies=cfg.STRATEGIES,
        instruments=cfg.INSTRUMENTS,
        data_dir=str(cfg.DATA_DIR),
        active_filters=["F1", "F2", "F4", "F5", "F6"],
        workers=cfg.WORKERS,
        max_combos=cfg.MAX_COMBOS,
        cost_bps=cfg.COST_BPS,
    )
    cfg.RESULTS_DIR.mkdir(exist_ok=True)
    out = cfg.RESULTS_DIR / f"split_{pd.Timestamp.now():%Y%m%d_%H%M}.csv"
    results.to_csv(out, index=False)
    print(f"Gespeichert: {out}")
