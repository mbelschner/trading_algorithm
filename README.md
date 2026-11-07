# trading_algorithm

Intraday-CFD-Strategien für Pepperstone/cTrader: Daten laden → Strategien → normaler Test → Walk-Forward-Test → cBot.

```
download/            Daten über die cTrader Open API laden
  01_setup_oauth.py    einmalig: OAuth, schreibt tokens.json (nicht im Git)
  02_list_symbols.py   schreibt symbols.csv (Symbolnamen des Brokers)
  03_download.py       OHLCV-Bars eines Instruments als CSV
pyrunner/            Backtest-Engine
  indicators.py        Indikatoren (numpy/pandas)
  strategies.py        Strategien + Parameter-Grids (Numba-FSM, gemeinsame SL/TP-Logik)
  runner.py            Kosten, Kennzahlen, Filter F1–F8, Parallelisierung
backtest_config.py   welche Strategien/Instrumente, Worker, Kosten
run_split.py         normaler Test: In-Sample 70 % / Out-of-Sample 30 %
run_walkforward.py   Walk-Forward: 2 Folds (55 % / 70 %), beide müssen tragen (F8)
live/ctrader/        cBots (C#) für cTrader Automate + verify_ports.py
data/                CSVs (lokal, nicht im Git)
results/             Ergebnis-CSVs (lokal, nicht im Git)
```

## Ablauf

```bash
pip install -r requirements.txt

# 1 · Daten (einmalig Zugang einrichten, siehe download/README.md)
cd download
pip install -r requirements.txt
cp config_template.py config.py      # Client-ID/Secret eintragen — config.py bleibt lokal
python 01_setup_oauth.py
python 02_list_symbols.py
python 03_download.py --symbol XAUUSD --tf M15 --from 2023-01-01 --to 2026-06-30 --out ../data/GOLD_MINUTE_15.csv
cd ..

# 2 · Strategien und Instrumente in backtest_config.py auswählen

# 3 · Testen
python run_split.py          # -> results/split_<Zeit>.csv
python run_walkforward.py    # -> results/walkforward_<Zeit>.csv
```

Dateinamen in `data/` müssen dem Muster `<INSTRUMENT>_MINUTE_<5|15>.csv` folgen; daraus liest der Runner das Kostenkürzel (`SPREAD_BP` in `pyrunner/runner.py`). Format: `time,open,high,low,close,volume`, Zeit in UTC.

## Filter

| ID | Bedingung |
|---|---|
| F1 | OS Max-Drawdown > −15 % |
| F2 | OS Profit-Faktor > 1,2 |
| F3 | OS Sortino > 1,0 (optional) |
| F4 | OS Calmar > 0,5 |
| F5 | IS Profit-Faktor > 1,0 |
| F6 | OS Trefferquote 45–85 % |
| F7 | jedes OS-Jahr positiv (optional) |
| F8 | Walk-Forward: beide Prüf-Fenster positiv, ≥ 10 Trades im früheren |

## Zugangsdaten

`tokens.json`, `download/config.py`, `.env` und `api_data.txt` stehen in `.gitignore`. Nie committen — das Repo ist öffentlich.
