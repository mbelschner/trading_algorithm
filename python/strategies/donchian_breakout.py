"""Donchian Channel Breakout - N-Bar High/Low Ausbruch."""
import numpy as np
import pandas as pd

NAME = "Donchian_Breakout"

PARAM_GRID = {
    "lookback": [20, 40, 60],
    "exit_n":   [10, 20],
}


def generate_signals(df: pd.DataFrame, lookback=20, exit_n=10) -> pd.DataFrame:
    df = df.copy()
    upper = df["High"].rolling(lookback).max().shift(1)  # shift damit aktuelle Bar nicht reinrechnet
    lower = df["Low"].rolling(lookback).min().shift(1)
    exit_upper = df["High"].rolling(exit_n).max().shift(1)
    exit_lower = df["Low"].rolling(exit_n).min().shift(1)

    pos = np.zeros(len(df))
    state = 0
    h = df["High"].values; l = df["Low"].values
    up = upper.values; lo = lower.values
    eu = exit_upper.values; el = exit_lower.values

    for i in range(len(df)):
        if np.isnan(up[i]) or np.isnan(lo[i]):
            pos[i] = 0
            continue
        if state == 0:
            if h[i] > up[i]:
                state = 1
            elif l[i] < lo[i]:
                state = -1
        elif state == 1:
            if l[i] < el[i]:
                state = 0
        elif state == -1:
            if h[i] > eu[i]:
                state = 0
        pos[i] = state

    df["Position"] = pos
    return df