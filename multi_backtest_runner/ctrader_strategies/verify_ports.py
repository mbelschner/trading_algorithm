"""
Equivalence check for the cTrader ports.

Re-implements, in Python, EXACTLY the incremental bar-by-bar logic written in the
C# cBots (StepBar / StepBarrier / *Signal), then compares the resulting position
array against the original vectorised strategies.py output on the real CSV data.

A perfect port => zero differing bars.
"""
import sys, math
from pathlib import Path
import numpy as np
import pandas as pd

ROOT = Path(r"c:\Users\maxib\OneDrive\Dokumente\trading_algorithm")
PY = ROOT / "multi_backtest_runner" / "pyrunner" / "pyrunner"
DATA = ROOT / "price_data" / "ctrader_data"
sys.path.insert(0, str(PY))

from strategies import (tokyo_gap_range, ichimoku_triple, rsi_mean_reversion,
                        trend_rsi_pullback)

NAN = float('nan')
isnan = math.isnan


# ---------------------------------------------------------------------------
# Mirror of the C# barrier FSM (identical in all five bots)
# ---------------------------------------------------------------------------
class Barrier:
    def __init__(self, sl_mult, tp_mult):
        self.sl_mult = sl_mult
        self.tp_mult = tp_mult
        self.state = 0
        self.locked = 0
        self.sl = NAN
        self.tp = NAN

    def _open(self, s, c, atr):
        self.state = s
        if s == 1:
            self.sl = c - self.sl_mult * atr
            self.tp = c + self.tp_mult * atr
        else:
            self.sl = c + self.sl_mult * atr
            self.tp = c - self.tp_mult * atr

    def step(self, raw_sig, h, l, c, atr, new_day):
        s = 0 if isnan(atr) else raw_sig

        if new_day and self.state != 0:
            self.state = 0
            self.sl = NAN
            self.tp = NAN

        if self.state == 1:
            if not isnan(self.sl) and l <= self.sl:
                self.locked = 1; self.state = 0; self.sl = self.tp = NAN
            elif not isnan(self.tp) and h >= self.tp:
                self.locked = 1; self.state = 0; self.sl = self.tp = NAN
        elif self.state == -1:
            if not isnan(self.sl) and h >= self.sl:
                self.locked = -1; self.state = 0; self.sl = self.tp = NAN
            elif not isnan(self.tp) and l <= self.tp:
                self.locked = -1; self.state = 0; self.sl = self.tp = NAN

        if self.locked != 0 and s != self.locked:
            self.locked = 0

        if self.state == 0:
            if s != 0 and s != self.locked and not isnan(atr):
                self._open(s, c, atr)
        else:
            if s == 0:
                self.state = 0; self.sl = self.tp = NAN
            elif s != self.state:
                self._open(s, c, atr)
        return self.state


def donch_mid(H, L, i, n):
    """Mirror of C# DonchMid()."""
    if i < 0 or i < n - 1:
        return NAN
    hi = max(H[i - n + 1:i + 1])
    lo = min(L[i - n + 1:i + 1])
    return (hi + lo) / 2.0


# ---------------------------------------------------------------------------
# Gold  —  Tokyo Gap Range
# ---------------------------------------------------------------------------
def csharp_tokyo(df, or_candles, gap_atr_mult, atr_n, sess_start_h, sess_end_h,
                 last_entry_h, atr_period, sl_mult, tp_mult):
    H = df['High'].values; L = df['Low'].values; C = df['Close'].values
    T = pd.DatetimeIndex(df['Timestamp'])
    n = len(C)
    a_gap = 2.0 / (atr_n + 1.0)
    a_bar = 2.0 / (atr_period + 1.0)
    tr_gap = tr_bar = 0.0
    raw_state = 0
    cur_date = None
    or_h = or_l = NAN
    or_cnt = 0
    or_done = False
    bar = Barrier(sl_mult, tp_mult)
    out = np.zeros(n, np.int32)

    sess_start = sess_start_h * 60
    sess_end = sess_end_h * 60
    last_entry = last_entry_h * 60

    for i in range(n):
        h, l, c = H[i], L[i], C[i]
        prev_c = C[i - 1] if i > 0 else c
        tr = max(h - l, abs(h - prev_c), abs(l - prev_c))
        tr_gap = tr if i == 0 else a_gap * tr + (1 - a_gap) * tr_gap
        tr_bar = tr if i == 0 else a_bar * tr + (1 - a_bar) * tr_bar
        atr_gap = NAN if i < atr_n else tr_gap
        atr_bar = NAN if i < atr_period else tr_bar

        t = T[i]
        bar_min = t.hour * 60 + t.minute
        d = t.date()
        if d != cur_date:
            cur_date = d
            or_h = or_l = NAN
            or_cnt = 0
            or_done = False
            raw_state = 0

        if bar_min >= sess_end:
            raw_state = 0
            raw_sig = 0
        elif bar_min < sess_start:
            raw_sig = raw_state
        elif not or_done:
            or_h = h if isnan(or_h) else max(or_h, h)
            or_l = l if isnan(or_l) else min(or_l, l)
            or_cnt += 1
            if or_cnt >= or_candles:
                or_done = True
            raw_sig = raw_state
        elif bar_min >= last_entry:
            raw_sig = raw_state
        else:
            if raw_state == 0:
                if not isnan(atr_gap):
                    thr = atr_gap * gap_atr_mult
                    if (not isnan(or_h)) and (or_h - or_l) > thr:
                        if c > or_h:
                            raw_state = 1
                        elif c < or_l:
                            raw_state = -1
            raw_sig = raw_state

        new_day = i > 0 and T[i - 1].date() != d
        out[i] = bar.step(raw_sig, h, l, c, atr_bar, new_day)
    return out


# ---------------------------------------------------------------------------
# Ichimoku Triple
# ---------------------------------------------------------------------------
def csharp_ichimoku(df, tenkan_n, kijun_n, senkou_b_n, displacement,
                    cloud_filter, atr_period, sl_mult, tp_mult):
    H = df['High'].values; L = df['Low'].values; C = df['Close'].values
    T = pd.DatetimeIndex(df['Timestamp'])
    n = len(C)
    a_bar = 2.0 / (atr_period + 1.0)
    tr_bar = 0.0
    n_start = max(kijun_n, senkou_b_n, displacement) + 1
    raw_state = 0
    bar = Barrier(sl_mult, tp_mult)
    out = np.zeros(n, np.int32)

    for i in range(n):
        h, l, c = H[i], L[i], C[i]
        prev_c = C[i - 1] if i > 0 else c
        tr = max(h - l, abs(h - prev_c), abs(l - prev_c))
        tr_bar = tr if i == 0 else a_bar * tr + (1 - a_bar) * tr_bar
        atr = NAN if i < atr_period else tr_bar

        if i < n_start:
            raw_sig = 0
        else:
            tenkan = donch_mid(H, L, i, tenkan_n)
            kijun = donch_mid(H, L, i, kijun_n)
            tenkan_p = donch_mid(H, L, i - 1, tenkan_n)
            kijun_p = donch_mid(H, L, i - 1, kijun_n)
            j = i - displacement
            if j >= 0:
                sa = (donch_mid(H, L, j, tenkan_n) + donch_mid(H, L, j, kijun_n)) / 2.0
                sb = donch_mid(H, L, j, senkou_b_n)
                chikou = C[j]
            else:
                sa = sb = chikou = NAN

            if isnan(tenkan) or isnan(kijun) or isnan(sa) or isnan(sb) or isnan(chikou):
                raw_sig = raw_state
            else:
                kt = tenkan > kijun; kt1 = tenkan_p <= kijun_p
                kd = tenkan < kijun; kd1 = tenkan_p >= kijun_p
                ku_top = max(sa, sb); ku_bot = min(sa, sb)
                tk_up = kt and kt1; tk_dn = kd and kd1
                ck_l = chikou > c; ck_s = chikou < c
                long_ok = tk_up and ((not cloud_filter) or c > ku_top) and ck_l
                short_ok = tk_dn and ((not cloud_filter) or c < ku_bot) and ck_s
                ex_l = (tenkan < kijun) or (cloud_filter and c < ku_bot)
                ex_s = (tenkan > kijun) or (cloud_filter and c > ku_top)

                if raw_state == 0:
                    if long_ok: raw_state = 1
                    elif short_ok: raw_state = -1
                elif raw_state == 1:
                    if ex_l or short_ok: raw_state = -1 if short_ok else 0
                else:
                    if ex_s or long_ok: raw_state = 1 if long_ok else 0
                raw_sig = raw_state

        new_day = i > 0 and T[i - 1].date() != T[i].date()
        out[i] = bar.step(raw_sig, h, l, c, atr, new_day)
    return out


# ---------------------------------------------------------------------------
# RSI Mean Reversion
# ---------------------------------------------------------------------------
def csharp_rsi_mr(df, period, lower, upper, atr_period, sl_mult, tp_mult):
    H = df['High'].values; L = df['Low'].values; C = df['Close'].values
    T = pd.DatetimeIndex(df['Timestamp'])
    n = len(C)
    a_bar = 2.0 / (atr_period + 1.0)
    a_rsi = 1.0 / period
    tr_bar = avg_up = avg_dn = 0.0
    raw_state = 0
    bar = Barrier(sl_mult, tp_mult)
    out = np.zeros(n, np.int32)

    for i in range(n):
        h, l, c = H[i], L[i], C[i]
        prev_c = C[i - 1] if i > 0 else c
        tr = max(h - l, abs(h - prev_c), abs(l - prev_c))
        tr_bar = tr if i == 0 else a_bar * tr + (1 - a_bar) * tr_bar
        atr = NAN if i < atr_period else tr_bar

        d = (c - C[i - 1]) if i > 0 else 0.0
        up = d if d > 0 else 0.0
        dn = -d if d < 0 else 0.0
        avg_up = up if i == 0 else a_rsi * up + (1 - a_rsi) * avg_up
        avg_dn = dn if i == 0 else a_rsi * dn + (1 - a_rsi) * avg_dn
        rs = avg_up / (1e-10 if avg_dn == 0 else avg_dn)
        rsi = NAN if i < period else 100.0 - 100.0 / (1.0 + rs)

        if isnan(rsi):
            raw_sig = 0
        else:
            if raw_state == 0:
                if rsi < lower: raw_state = 1
                elif rsi > upper: raw_state = -1
            elif raw_state == 1:
                if rsi >= 50.0: raw_state = 0
                elif rsi > upper: raw_state = -1
            else:
                if rsi <= 50.0: raw_state = 0
                elif rsi < lower: raw_state = 1
            raw_sig = raw_state

        new_day = i > 0 and T[i - 1].date() != T[i].date()
        out[i] = bar.step(raw_sig, h, l, c, atr, new_day)
    return out


# ---------------------------------------------------------------------------
# Trend RSI Pullback
# ---------------------------------------------------------------------------
def csharp_trsi(df, ema_len, rsi_len, rsi_buy, rsi_sell, atr_period, sl_mult, tp_mult):
    H = df['High'].values; L = df['Low'].values; C = df['Close'].values
    T = pd.DatetimeIndex(df['Timestamp'])
    n = len(C)
    a_bar = 2.0 / (atr_period + 1.0)
    a_ema = 2.0 / (ema_len + 1.0)
    a_rsi = 1.0 / rsi_len
    tr_bar = avg_up = avg_dn = 0.0
    ema_v = 0.0
    rsi_prev = NAN
    raw_state = 0
    bar = Barrier(sl_mult, tp_mult)
    out = np.zeros(n, np.int32)

    for i in range(n):
        h, l, c = H[i], L[i], C[i]
        prev_c = C[i - 1] if i > 0 else c
        tr = max(h - l, abs(h - prev_c), abs(l - prev_c))
        tr_bar = tr if i == 0 else a_bar * tr + (1 - a_bar) * tr_bar
        atr = NAN if i < atr_period else tr_bar

        ema_v = c if i == 0 else a_ema * c + (1 - a_ema) * ema_v
        ema_out = NAN if i < ema_len - 1 else ema_v

        d = (c - C[i - 1]) if i > 0 else 0.0
        up = d if d > 0 else 0.0
        dn = -d if d < 0 else 0.0
        avg_up = up if i == 0 else a_rsi * up + (1 - a_rsi) * avg_up
        avg_dn = dn if i == 0 else a_rsi * dn + (1 - a_rsi) * avg_dn
        rs = avg_up / (1e-10 if avg_dn == 0 else avg_dn)
        rsi = NAN if i < rsi_len else 100.0 - 100.0 / (1.0 + rs)

        if i < 1:
            raw_sig = 0
        elif isnan(ema_out) or isnan(rsi):
            raw_sig = raw_state
        else:
            up_t = c > ema_out
            dn_t = c < ema_out
            # NaN comparisons are False, matching NumPy
            lt = up_t and (rsi_prev < rsi_buy) and (rsi >= rsi_buy)
            st = dn_t and (rsi_prev > rsi_sell) and (rsi <= rsi_sell)
            if raw_state == 0:
                if lt: raw_state = 1
                elif st: raw_state = -1
            elif raw_state == 1:
                if c < ema_out or rsi >= 70.0: raw_state = 0
            else:
                if c > ema_out or rsi <= 30.0: raw_state = 0
            raw_sig = raw_state
        rsi_prev = rsi

        new_day = i > 0 and T[i - 1].date() != T[i].date()
        out[i] = bar.step(raw_sig, h, l, c, atr, new_day)
    return out


# ---------------------------------------------------------------------------
def load(name):
    df = pd.read_csv(DATA / name)
    df.columns = [c.lower() for c in df.columns]
    df.rename(columns={'time': 'Timestamp', 'open': 'Open', 'high': 'High',
                       'low': 'Low', 'close': 'Close', 'volume': 'Volume'},
              inplace=True)
    df['Timestamp'] = pd.to_datetime(df['Timestamp'], utc=True)
    df.sort_values('Timestamp', inplace=True)
    df.drop_duplicates(subset='Timestamp', keep='last', inplace=True)
    df.reset_index(drop=True, inplace=True)
    return df


def report(tag, ref, got):
    diff = np.flatnonzero(ref != got)
    n = len(ref)
    status = "OK " if len(diff) == 0 else "FAIL"
    print(f"[{status}] {tag:34s} bars={n:6d}  differing={len(diff)}")
    if len(diff):
        print("        first 10 differing indices:", diff[:10].tolist())
        for k in diff[:5]:
            print(f"          i={k} ref={ref[k]} got={got[k]}")
    return len(diff) == 0


ok = True

# --- Gold: Tokyo Gap Range -------------------------------------------------
df = load("GOLD_MINUTE_15.csv")
ref = tokyo_gap_range(df, or_candles=6, gap_atr_mult=0.5, atr_n=14,
                      session_start_h=0, session_end_h=9, last_entry_h=7,
                      adx_filter=False, atr_period=14,
                      sl_atr_mult=1.5, tp_atr_mult=3.0)
got = csharp_tokyo(df, 6, 0.5, 14, 0, 9, 7, 14, 1.5, 3.0)
ok &= report("GOLD Tokyo_Gap_Range M15", ref, got)

# --- Copper: Ichimoku ------------------------------------------------------
df = load("COPPER_MINUTE_15.csv")
ref = ichimoku_triple(df, tenkan_n=9, kijun_n=26, senkou_b_n=52,
                      displacement=26, cloud_filter=True, atr_period=10,
                      sl_atr_mult=2.5, tp_atr_mult=2.0)
got = csharp_ichimoku(df, 9, 26, 52, 26, True, 10, 2.5, 2.0)
ok &= report("COPPER Ichimoku M15", ref, got)

# --- Brent: Ichimoku -------------------------------------------------------
df = load("OIL_BRENT_MINUTE_15.csv")
ref = ichimoku_triple(df, tenkan_n=9, kijun_n=26, senkou_b_n=60,
                      displacement=26, cloud_filter=True, atr_period=10,
                      sl_atr_mult=1.5, tp_atr_mult=2.0)
got = csharp_ichimoku(df, 9, 26, 60, 26, True, 10, 1.5, 2.0)
ok &= report("BRENT Ichimoku M15", ref, got)

# --- AUDUSD: RSI MR --------------------------------------------------------
df = load("AUDUSD_MINUTE_15.csv")
# exit_mid entfernt: der Param wurde im Body nie gelesen (reine Grid-Attrappe).
ref = rsi_mean_reversion(df, period=21, lower=20, upper=75,
                         atr_period=14, sl_atr_mult=2.5, tp_atr_mult=2.0)
got = csharp_rsi_mr(df, 21, 20.0, 75.0, 14, 2.5, 2.0)
ok &= report("AUDUSD RSI_MeanReversion M15", ref, got)

# --- USDJPY: Trend RSI Pullback -------------------------------------------
df = load("USDJPY_MINUTE_15.csv")
ref = trend_rsi_pullback(df, ema_len=100, rsi_len=14, rsi_buy=35.0,
                         rsi_sell=65.0, atr_period=10,
                         sl_atr_mult=2.5, tp_atr_mult=2.0)
got = csharp_trsi(df, 100, 14, 35.0, 65.0, 10, 2.5, 2.0)
ok &= report("USDJPY Trend_RSI_Pullback M15", ref, got)

print()
print("ALL PORTS EQUIVALENT" if ok else "MISMATCHES FOUND")
sys.exit(0 if ok else 1)
