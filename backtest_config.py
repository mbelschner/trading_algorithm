"""
backtest_config.py — gemeinsame Einstellungen fuer run_split.py und run_walkforward.py.
Zeile auskommentieren (#), um eine Strategie oder ein Instrument auszuschliessen.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"          # CSVs aus download/03_download.py (nicht im Git)
RESULTS_DIR = ROOT / "results"    # Ergebnis-CSVs (nicht im Git)

WORKERS = 12                      # Prozesse; auf 1 setzen zum Debuggen
MAX_COMBOS = None                 # None = MAX_COMBOS_PER_STRATEGY aus runner.py, 0 = volle Enumeration
COST_BPS = None                   # None = SPREAD_BP je Instrument; Zahl = fuer alle (Kosten-Sensitivitaet)

STRATEGIES = [
    "RSI_MeanReversion",
    "Bollinger_ZScore_MR",
    "ATRDistance_MeanReversion",
    "CCI_MeanReversion",
    "CMO_VHF_STC",
    "Donchian_Breakout",
    "Ichimoku_7_20_60",
    "VWAP_MeanReversion",
    "SMA_Crossover",
    "Supertrend_ATR",
    "Keltner_Squeeze_Breakout",
    "MACD_Trend_Momentum",
    "Session_ORB",
    "Trend_RSI_Pullback",
    "Tokyo_Gap_Range",
    "SilverAsia_BreakoutV4",
    "Session_ZScore_MR",
    "VWAP_Band_MR",
    "OU_HalfLife_MR",
    "Overnight_Reversal",
    "RSI_Extreme_MR",
]

INSTRUMENTS = [
    #"AUDUSD_MINUTE_5.csv",
    #"AUDUSD_MINUTE_15.csv",
    #"COPPER_MINUTE_5.csv",
    "COPPER_MINUTE_15.csv",
    #"DE40_MINUTE_5.csv",
    #"DE40_MINUTE_15.csv",
    #"EURGBP_MINUTE_5.csv",
    #"EURGBP_MINUTE_15.csv",
    #"EURJPY_MINUTE_5.csv",
    #"EURJPY_MINUTE_15.csv",
    #"EURUSD_MINUTE_5.csv",
    #"EURUSD_MINUTE_15.csv",
    #"GOLD_MINUTE_5.csv",
    #"GOLD_MINUTE_15.csv",
    #"OIL_BRENT_MINUTE_5.csv",
    #"OIL_BRENT_MINUTE_15.csv",
    #"SILVER_MINUTE_5.csv",
    #"SILVER_MINUTE_15.csv",
    #"US500_MINUTE_5.csv",
    #"US500_MINUTE_15.csv",
    #"USDCAD_MINUTE_5.csv",
    #"USDCAD_MINUTE_15.csv",
    #"USDJPY_MINUTE_5.csv",
    #"USDJPY_MINUTE_15.csv",
]
