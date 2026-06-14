"""RSI Mean Reversion mit Wilder's Smoothing und expliziten Exits."""
import numpy as np
import pandas as pd

NAME = "RSI_MeanReversion"

PARAM_GRID = {
    "period":      [7, 14, 21],
    "lower":       [20, 25, 30],
    "upper":       [70, 75, 80],
    "exit_mid":    [True],   # Exit bei RSI-Crossing der 50-Linie
}


def _rsi_wilder(close: pd.Series, period: int) -> pd.Series:
    delta = close.diff()
    gain = delta.clip(lower=0.0)
    loss = -delta.clip(upper=0.0)
    # Wilder smoothing == EMA mit alpha = 1/period
    avg_gain = gain.ewm(alpha=1 / period, adjust=False, min_periods=period).mean()
    avg_loss = loss.ewm(alpha=1 / period, adjust=False, min_periods=period).mean()
    rs = avg_gain / (avg_loss + 1e-12)
    return 100 - (100 / (1 + rs))


def generate_signals(df: pd.DataFrame, period=14, lower=30, upper=70, exit_mid=True) -> pd.DataFrame:
    df = df.copy()
    df["RSI"] = _rsi_wilder(df["Close"], period)

    pos = np.zeros(len(df))
    state = 0
    rsi = df["RSI"].values
    for i in range(len(df)):
        r = rsi[i]
        if np.isnan(r):
            pos[i] = 0
            continue
        if state == 0:
            if r < lower:
                state = 1
            elif r > upper:
                state = -1
        elif state == 1:
            if exit_mid and r >= 50:
                state = 0
            elif r > upper:
                state = -1
        elif state == -1:
            if exit_mid and r <= 50:
                state = 0
            elif r < lower:
                state = 1
        pos[i] = state

    df["Position"] = pos
    return df