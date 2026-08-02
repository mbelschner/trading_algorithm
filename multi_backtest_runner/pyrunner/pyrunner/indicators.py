"""
indicators.py — vektorisierte Indikatoren (numpy/pandas)
Alle Funktionen geben np.ndarray zurück, NaN-gepadded wie TTR.
"""
import numpy as np
import pandas as pd


def ema(x: np.ndarray, period: int) -> np.ndarray:
    out = pd.Series(x).ewm(span=period, adjust=False).mean().values.copy()
    out[:period - 1] = np.nan
    return out


def sma(x: np.ndarray, period: int) -> np.ndarray:
    out = pd.Series(x).rolling(period).mean().values.copy()
    return out


def rsi_wilder(x: np.ndarray, period: int) -> np.ndarray:
    d = np.diff(x, prepend=x[0])
    up = np.where(d > 0, d, 0.0)
    dn = np.where(d < 0, -d, 0.0)
    alpha = 1.0 / period
    au = pd.Series(up).ewm(alpha=alpha, adjust=False).mean().values
    ad = pd.Series(dn).ewm(alpha=alpha, adjust=False).mean().values
    rs = au / np.where(ad == 0, 1e-10, ad)
    r = (100.0 - 100.0 / (1.0 + rs)).copy()
    r[:period] = np.nan
    return r


def atr(high: np.ndarray, low: np.ndarray, close: np.ndarray, period: int) -> np.ndarray:
    prev_c = np.roll(close, 1); prev_c[0] = close[0]
    tr = np.maximum(high - low, np.maximum(np.abs(high - prev_c), np.abs(low - prev_c)))
    out = pd.Series(tr).ewm(span=period, adjust=False).mean().values.copy()
    out[:period] = np.nan
    return out


def run_sd(x: np.ndarray, period: int) -> np.ndarray:
    return pd.Series(x).rolling(period).std(ddof=1).values.copy()


def run_max(x: np.ndarray, period: int) -> np.ndarray:
    return pd.Series(x).rolling(period).max().values.copy()


def run_min(x: np.ndarray, period: int) -> np.ndarray:
    return pd.Series(x).rolling(period).min().values.copy()


def cci(high: np.ndarray, low: np.ndarray, close: np.ndarray,
        period: int, c: float = 0.015) -> np.ndarray:
    tp = (high + low + close) / 3.0
    ma = pd.Series(tp).rolling(period).mean().values
    md = pd.Series(tp).rolling(period).apply(lambda x: np.mean(np.abs(x - x.mean())), raw=True).values
    return np.where(md == 0, 0.0, (tp - ma) / (c * md))


def macd(x: np.ndarray, fast: int, slow: int, signal: int):
    f = pd.Series(x).ewm(span=fast, adjust=False).mean().values
    s = pd.Series(x).ewm(span=slow, adjust=False).mean().values
    ml = f - s
    sig = pd.Series(ml).ewm(span=signal, adjust=False).mean().values
    return ml, sig


def keltner(high: np.ndarray, low: np.ndarray, close: np.ndarray,
            period: int, mult: float):
    ma = ema(close, period)
    a  = atr(high, low, close, period)
    return ma + mult * a, ma - mult * a   # upper, lower


def supertrend(high: np.ndarray, low: np.ndarray, close: np.ndarray,
               period: int, mult: float):
    hl2 = (high + low) / 2.0
    a   = pd.Series(np.maximum(high - low,
          np.maximum(np.abs(high - np.roll(close,1)),
                     np.abs(low  - np.roll(close,1))))
          ).rolling(period).mean().values
    ub = hl2 + mult * a
    lb = hl2 - mult * a
    n = len(close)
    fub = np.full(n, np.nan); flb = np.full(n, np.nan)
    st  = np.full(n, np.nan); d   = np.zeros(n, np.int32)
    for i in range(n):
        if np.isnan(a[i]): continue
        if i == 0 or np.isnan(fub[i-1]):
            fub[i]=ub[i]; flb[i]=lb[i]; st[i]=ub[i]; d[i]=-1
        else:
            fub[i] = ub[i] if (ub[i]<fub[i-1] or close[i-1]>fub[i-1]) else fub[i-1]
            flb[i] = lb[i] if (lb[i]>flb[i-1] or close[i-1]<flb[i-1]) else flb[i-1]
            if st[i-1]==fub[i-1]:
                if close[i]<=fub[i]: st[i]=fub[i]; d[i]=-1
                else:                st[i]=flb[i]; d[i]=1
            else:
                if close[i]>=flb[i]: st[i]=flb[i]; d[i]=1
                else:                st[i]=fub[i]; d[i]=-1
    return d


def ichimoku(high, low, close, tenkan_n, kijun_n, senkou_b_n, displacement):
    def donch_mid(h, l, n):
        return (run_max(h, n) + run_min(l, n)) / 2.0
    tenkan  = donch_mid(high, low, tenkan_n)
    kijun   = donch_mid(high, low, kijun_n)
    sa      = (tenkan + kijun) / 2.0
    sb      = donch_mid(high, low, senkou_b_n)
    # Senkou A/B: 26 Bars vorwärts projiziert → im Backtest nutzen wir den Wert
    # von vor 26 Bars (roll +displacement = sa[i-26])
    senkou_a = np.roll(sa,  displacement); senkou_a[:displacement]  = np.nan
    senkou_b = np.roll(sb,  displacement); senkou_b[:displacement]  = np.nan
    # FIX: Chikou Span = aktueller Close, 26 Bars zurück verschoben (Anzeige).
    # Für den Signal-Check bedeutet das: chikou[i] = close[i - displacement].
    # VORHER (BUG): np.roll(close, -displacement) → chikou[i] = close[i+26] = ZUKUNFT!
    # JETZT (FIX):  np.roll(close, +displacement) → chikou[i] = close[i-26] = Vergangenheit ✓
    chikou   = np.roll(close, displacement); chikou[:displacement] = np.nan
    return tenkan, kijun, senkou_a, senkou_b, chikou


def cmo(x: np.ndarray, period: int) -> np.ndarray:
    d = np.diff(x, prepend=x[0])
    up = np.where(d > 0, d, 0.0); dn = np.where(d < 0, -d, 0.0)
    su = pd.Series(up).rolling(period).sum().values
    sd = pd.Series(dn).rolling(period).sum().values
    denom = su + sd
    return np.where(denom == 0, 0.0, 100.0 * (su - sd) / denom)


def vhf(x: np.ndarray, period: int) -> np.ndarray:
    hh = run_max(x, period); ll = run_min(x, period)
    d  = np.abs(np.diff(x, prepend=x[0]))
    sd = pd.Series(d).rolling(period).sum().values
    denom = hh - ll
    return np.where(denom == 0, 0.0, sd / denom)


def stc(x: np.ndarray, fast: int, slow: int, cycle: int) -> np.ndarray:
    ml, _ = macd(x, fast, slow, 1)
    def stoch(s, p):
        hi = run_max(s, p); lo = run_min(s, p)
        denom = hi - lo
        return np.where(denom == 0, 50.0, 100.0 * (s - lo) / denom)
    k1 = stoch(ml, cycle)
    d1 = pd.Series(k1).ewm(span=cycle, adjust=False).mean().values
    k2 = stoch(d1, cycle)
    d2 = pd.Series(k2).ewm(span=cycle, adjust=False).mean().values
    return d2


def adx(high, low, close, period: int):
    prev_h = np.roll(high,1); prev_h[0]=high[0]
    prev_l = np.roll(low,1);  prev_l[0]=low[0]
    prev_c = np.roll(close,1); prev_c[0]=close[0]
    up  = high - prev_h; dn = prev_l - low
    pdm = np.where((up>dn)&(up>0), up, 0.0)
    ndm = np.where((dn>up)&(dn>0), dn, 0.0)
    tr  = np.maximum(high-low, np.maximum(np.abs(high-prev_c), np.abs(low-prev_c)))
    def ws(s, p): return pd.Series(s).ewm(alpha=1/p, adjust=False).mean().values
    tr_s = ws(tr,period); pdm_s = ws(pdm,period); ndm_s = ws(ndm,period)
    pdi = np.where(tr_s==0, 0.0, 100*pdm_s/tr_s)
    ndi = np.where(tr_s==0, 0.0, 100*ndm_s/tr_s)
    dx  = np.where((pdi+ndi)==0, 0.0, 100*np.abs(pdi-ndi)/(pdi+ndi))
    adx_val = ws(dx, period)
    return adx_val, pdi, ndi


def new_day_mask(timestamps: np.ndarray) -> np.ndarray:
    """UTC day-change mask, identisch zu ta.change(dayofmonth(time,'UTC'))!=0"""
    days = timestamps.astype('datetime64[D]').astype(np.int32)
    nd = np.concatenate([[False], days[1:] != days[:-1]])
    return nd


def rsi_divergence(high, low, rsi_v, lookback):
    """Bearish div: neues Preis-Hoch aber tieferes RSI-Hoch (simplified)"""
    n = len(high)
    bearish = np.zeros(n, bool)
    for i in range(lookback, n):
        w_h = high[i-lookback:i+1]; w_r = rsi_v[i-lookback:i+1]
        if np.any(np.isnan(w_r)): continue
        peak_idx = np.argmax(w_h)
        if peak_idx == lookback and w_h[lookback] > w_h[:lookback].max():
            prev_peak = np.argmax(w_h[:lookback])
            if w_r[lookback] < w_r[prev_peak]:
                bearish[i] = True
    return bearish