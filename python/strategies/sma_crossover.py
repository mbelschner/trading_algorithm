"""SMA Crossover - klassische Trend-Following Strategie."""
import numpy as np
import pandas as pd

NAME = "SMA_Crossover"

PARAM_GRID = {
    "fast": [10, 20, 30],
    "slow": [50, 100, 200],
}


def generate_signals(df: pd.DataFrame, fast=20, slow=50) -> pd.DataFrame:
    if fast >= slow:
        # Ungueltig - leere Position
        df = df.copy()
        df["Position"] = 0
        return df

    df = df.copy()
    df["SMA_fast"] = df["Close"].rolling(fast).mean()
    df["SMA_slow"] = df["Close"].rolling(slow).mean()

    pos = np.where(df["SMA_fast"] > df["SMA_slow"], 1,
          np.where(df["SMA_fast"] < df["SMA_slow"], -1, 0))
    pos = pd.Series(pos, index=df.index)
    pos[df["SMA_slow"].isna()] = 0
    df["Position"] = pos.astype(float)
    return df