"""
strategies.py — alle 17 Strategien (Signal-Logik, 1:1 aus R portiert)
Jede Strategie: (param_grid: dict, generate_signals: callable)
generate_signals(df, **params) -> np.ndarray int32 Position (-1/0/+1)
df hat Spalten: Timestamp (datetime64[ns,UTC]), Open, High, Low, Close, Volume
"""
import numpy as np
import pandas as pd
from numba import njit
from indicators import (ema, sma, rsi_wilder, atr as calc_atr, run_sd,
                        run_max, run_min, cci as calc_cci, macd as calc_macd,
                        keltner, supertrend as calc_supertrend,
                        ichimoku, cmo, vhf, stc, adx,
                        new_day_mask, session_break_mask, local_hours,
                        session_mask, SESSION_TZ, SESSION_START_H, SESSION_END_H,
                        rolling_halflife, rsi_divergence)


# =============================================================================
# HANDELSSESSION (global, keine Strategie-Parameter)
# =============================================================================
# Definition steht in indicators.py, damit runner.py sie ohne Zirkelimport
# mitbenutzen kann: 01:00-22:00 Europe/Berlin. Siehe dort fuer die Begruendung.
_BREAK_CACHE = {}

def _session_arrays(df):
    """(in_session, session_break) fuer df — pro Zeitreihe nur einmal gerechnet."""
    ts = df['Timestamp'].values
    in_sess = session_mask(ts)
    key = (len(ts), ts[0].view('int64').item() if len(ts) else 0)
    brk = _BREAK_CACHE.get(key)
    if brk is None:
        brk = session_break_mask(ts, SESSION_START_H, SESSION_TZ)
        if len(_BREAK_CACHE) > 64:
            _BREAK_CACHE.clear()
        _BREAK_CACHE[key] = brk
    return in_sess, brk


# =============================================================================
# GEMEINSAMER FSM-BARRIER-APPLIKATOR (Numba JIT)
# =============================================================================
@njit(cache=True)
def fsm_barrier(sig, high, low, close, atr_v, new_day, in_sess,
                sl_mult, tp_mult, overnight_lockout=False, max_bars=0):
    n = len(sig)
    pos    = np.zeros(n, np.int32)
    sl_out = np.full(n, np.nan)
    tp_out = np.full(n, np.nan)
    state = 0; sl = np.nan; tp = np.nan; locked = 0; bars = 0
    for i in range(n):
        s = sig[i]; a = atr_v[i]; px = close[i]
        if new_day[i] and state != 0:
            if overnight_lockout: locked = state
            state = 0; sl = np.nan; tp = np.nan
        # Ausserhalb der Handelssession: glattstellen, keine neuen Positionen.
        # Damit ist garantiert flat ueber Mitternacht und ueber den Rollover.
        if not in_sess[i]:
            state = 0; sl = np.nan; tp = np.nan; locked = 0
            pos[i] = 0
            continue
        if state == 1:
            if not np.isnan(sl) and low[i] <= sl:
                locked=1; state=0; sl=np.nan; tp=np.nan
            elif not np.isnan(tp) and high[i] >= tp:
                locked=1; state=0; sl=np.nan; tp=np.nan
        elif state == -1:
            if not np.isnan(sl) and high[i] >= sl:
                locked=-1; state=0; sl=np.nan; tp=np.nan
            elif not np.isnan(tp) and low[i] <= tp:
                locked=-1; state=0; sl=np.nan; tp=np.nan
        # Zeit-Stop: dritte Barriere neben SL/TP. Gleiche Konvention wie ein
        # Barrier-Hit (locked=state), d.h. Re-Entry erst nach Signalwechsel.
        if max_bars > 0 and state != 0 and bars >= max_bars:
            locked = state; state = 0; sl = np.nan; tp = np.nan
        if locked != 0 and s != locked: locked = 0
        if state == 0:
            if s != 0 and s != locked and not np.isnan(a):
                state = s; bars = 0
                if s == 1: sl=px-sl_mult*a; tp=px+tp_mult*a
                else:      sl=px+sl_mult*a; tp=px-tp_mult*a
        else:
            if s == 0: state=0; sl=np.nan; tp=np.nan
            elif s != state:
                state = s; bars = 0
                if s == 1: sl=px-sl_mult*a; tp=px+tp_mult*a
                else:      sl=px+sl_mult*a; tp=px-tp_mult*a
        pos[i] = state
        if state != 0:
            sl_out[i]=sl; tp_out[i]=tp
            bars += 1
    return pos, sl_out, tp_out


def _apply_barriers(raw_sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                    overnight_lockout=False, max_bars=0):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    a=calc_atr(h,l,c,atr_period)
    in_sess, nd = _session_arrays(df)
    sig=raw_sig.copy().astype(np.float64); sig[np.isnan(a)]=0
    pos,_,_ = fsm_barrier(sig,h,l,c,a,nd,in_sess,sl_atr_mult,tp_atr_mult,
                          overnight_lockout, int(max_bars))
    return pos


# =============================================================================
# 1. RSI Mean Reversion
# =============================================================================
@njit(cache=True)
def _rsi_sig(rsi, lower, upper):
    n=len(rsi); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        r=rsi[i]
        if np.isnan(r): sig[i]=0; continue
        if state==0:
            if r<lower: state=1
            elif r>upper: state=-1
        elif state==1:
            if r>=50: state=0
            elif r>upper: state=-1
        elif state==-1:
            if r<=50: state=0
            elif r<lower: state=1
        sig[i]=state
    return sig

def rsi_mean_reversion(df, period=14, lower=30.0, upper=70.0,
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                       max_bars=0):
    r=rsi_wilder(df['Close'].values, period)
    sig=_rsi_sig(r.astype(np.float64), float(lower), float(upper))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# Lookbacks nach oben erweitert: die Reversion in Copper traegt erst ab ~4h
# (M15 period>=16), nicht auf 30-90min. tp<sl ergaenzt — MR will engen TP /
# weiten SL, der alte Grid hatte in 3 von 4 Faellen tp>=sl (Trend-Payoff).
RSI_MEAN_REVERSION_GRID = dict(
    period=[10,14,18,24,32,48], lower=[20,25,30], upper=[70,75,80],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5,3.5], tp_atr_mult=[0.75,1.5,2.5],
    max_bars=[0,16,32]
)


# =============================================================================
# 2. Bollinger ZScore MR
# =============================================================================
@njit(cache=True)
def _bollinger_sig(z, sd, entry_z, exit_z):
    n=len(z); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        zi=z[i]; si=sd[i]
        if np.isnan(zi) or np.isnan(si) or si==0: sig[i]=state; continue
        if state==0:
            if zi<=-entry_z: state=1
            elif zi>=entry_z: state=-1
        elif state==1:
            if zi>=-exit_z: state=0
            if zi>=entry_z: state=-1
        elif state==-1:
            if zi<=exit_z: state=0
            if zi<=-entry_z: state=1
        sig[i]=state
    return sig

def bollinger_zscore_mr(df, period=20, entry_z=2.0, exit_z=0.0,
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                        max_bars=0):
    c=df['Close'].values
    ma=sma(c,period); sd=run_sd(c,period)
    z=np.where(sd==0, 0.0, (c-ma)/sd)
    sig=_bollinger_sig(z,sd,float(entry_z),float(exit_z))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# exit_z-Konvention (siehe _bollinger_sig): Long exitet bei z >= -exit_z.
# exit_z>0 = vor dem Mittelwert raus, 0 = am Mittelwert, <0 = Overshoot halten.
BOLLINGER_ZSCORE_MR_GRID = dict(
    period=[20,30,50,80,120], entry_z=[1.5,2.0,2.5,3.0], exit_z=[-0.5,0.0,0.5,1.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5,3.5], tp_atr_mult=[0.75,1.5,2.5],
    max_bars=[0,16,32]
)


# =============================================================================
# 3. ATR Distance MR
# =============================================================================
@njit(cache=True)
def _atr_mr_sig(cl, ema_v, upper, lower):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if np.isnan(ema_v[i]) or np.isnan(upper[i]): sig[i]=0; continue
        if state==0:
            if cl[i]<lower[i]: state=1
            elif cl[i]>upper[i]: state=-1
        elif state==1:
            if cl[i]>=ema_v[i]: state=0
            elif cl[i]>upper[i]: state=-1
        elif state==-1:
            if cl[i]<=ema_v[i]: state=0
            elif cl[i]<lower[i]: state=1
        sig[i]=state
    return sig

def atr_distance_mr(df, ema_period=30, atr_period=14, mult=2.0,
                    sl_atr_mult=1.5, tp_atr_mult=2.0,
                    max_bars=0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    e=ema(c,ema_period); a=calc_atr(h,l,c,atr_period)
    sig=_atr_mr_sig(c, e, e+mult*a, e-mult*a)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

ATR_DISTANCE_MR_GRID = dict(
    ema_period=[30,50,80,120], atr_period=[10,14,20], mult=[1.5,2.0,2.5,3.0],
    sl_atr_mult=[1.5,2.5,3.5], tp_atr_mult=[0.75,1.5,2.5],
    max_bars=[0,16,32]
)


# =============================================================================
# 4. CCI Mean Reversion
# =============================================================================
@njit(cache=True)
def _cci_sig(cci_v, threshold, exit_level):
    n=len(cci_v); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        ci=cci_v[i]
        if np.isnan(ci): sig[i]=0; continue
        if state==0:
            if ci<-threshold: state=1
            elif ci>threshold: state=-1
        elif state==1:
            if ci>=-exit_level: state=0
            if ci>threshold: state=-1
        elif state==-1:
            if ci<=exit_level: state=0
            if ci<-threshold: state=1
        sig[i]=state
    return sig

def cci_mean_reversion(df, period=20, threshold=100.0, exit_level=0.0,
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                       max_bars=0):
    c=calc_cci(df['High'].values,df['Low'].values,df['Close'].values,period)
    sig=_cci_sig(c,float(threshold),float(exit_level))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

CCI_MEAN_REVERSION_GRID = dict(
    period=[20,30,50,80], threshold=[100.0,150.0,200.0], exit_level=[0.0,50.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5,3.5], tp_atr_mult=[0.75,1.5,2.5],
    max_bars=[0,16,32]
)


# =============================================================================
# 5. CMO VHF STC
# =============================================================================
@njit(cache=True)
def _cmo_sig(cmo_v, vhf_v, stc_v, cmo_thr, vhf_thr, stc_long, stc_short):
    n=len(cmo_v); sig=np.zeros(n,np.int32); state=0
    for i in range(1,n):
        if (np.isnan(cmo_v[i]) or np.isnan(vhf_v[i]) or
            np.isnan(stc_v[i]) or np.isnan(stc_v[i-1])): sig[i]=state; continue
        trending      = vhf_v[i]>vhf_thr
        bull_mom      = cmo_v[i]>cmo_thr
        bear_mom      = cmo_v[i]<-cmo_thr
        stc_up        = stc_v[i-1]<stc_long  and stc_v[i]>=stc_long
        stc_dn        = stc_v[i-1]>stc_short and stc_v[i]<=stc_short
        long_sig  = trending and bull_mom and stc_up
        short_sig = trending and bear_mom and stc_dn
        ex_l = stc_v[i]>stc_short or short_sig
        ex_s = stc_v[i]<stc_long  or long_sig
        if state==0:
            if long_sig: state=1
            elif short_sig: state=-1
        elif state==1:
            if ex_l: state=-1 if short_sig else 0
        elif state==-1:
            if ex_s: state=1 if long_sig else 0
        sig[i]=state
    return sig

def cmo_vhf_stc(df, cmo_period=14, cmo_threshold=20.0,
                vhf_period=28, vhf_threshold=0.35,
                stc_fast=23, stc_slow=50, stc_cycle=10,
                stc_entry_long=25.0, stc_entry_short=75.0,
                atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                max_bars=0):
    c=df['Close'].values
    cv=cmo(c,cmo_period); vv=vhf(c,vhf_period); sv=stc(c,stc_fast,stc_slow,stc_cycle)
    sig=_cmo_sig(cv,vv,sv,float(cmo_threshold),float(vhf_threshold),
                 float(stc_entry_long),float(stc_entry_short))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

CMO_VHF_STC_GRID = dict(
    cmo_period=[14,21], cmo_threshold=[15.0,20.0],
    vhf_period=[28], vhf_threshold=[0.30,0.35],
    stc_fast=[23], stc_slow=[50], stc_cycle=[10],
    stc_entry_long=[25.0], stc_entry_short=[75.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 6. Donchian Crossover
# =============================================================================
@njit(cache=True)
def _donchian_sig(h, l, upper, lower, exit_u, exit_l):
    n=len(h); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if np.isnan(upper[i]) or np.isnan(lower[i]): sig[i]=0; continue
        if state==0:
            if h[i]>upper[i]: state=1
            elif l[i]<lower[i]: state=-1
        elif state==1:
            if not np.isnan(exit_l[i]) and l[i]<exit_l[i]: state=0
        elif state==-1:
            if not np.isnan(exit_u[i]) and h[i]>exit_u[i]: state=0
        sig[i]=state
    return sig

def donchian_crossover(df, lookback=20, exit_n=10,
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                       max_bars=0):
    h=df['High'].values; l=df['Low'].values
    # shift 1 = R data.table::shift(x,1) = previous bar's value (no lookahead)
    upper = np.roll(run_max(h,lookback),1); upper[0]=np.nan
    lower = np.roll(run_min(l,lookback),1); lower[0]=np.nan
    eu    = np.roll(run_max(h,exit_n),1);   eu[0]=np.nan
    el    = np.roll(run_min(l,exit_n),1);   el[0]=np.nan
    sig=_donchian_sig(h,l,upper,lower,eu,el)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

DONCHIAN_CROSSOVER_GRID = dict(
    lookback=[10,20,40,60], exit_n=[5,10],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 7. Ichimoku Triple
# =============================================================================
@njit(cache=True)
def _ichi_sig(tenkan,kijun,sa,sb,chikou,cl,cloud_filter,n_start):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    for i in range(n_start,n):
        if (np.isnan(tenkan[i]) or np.isnan(kijun[i]) or
            np.isnan(sa[i]) or np.isnan(sb[i]) or np.isnan(chikou[i])):
            sig[i]=state; continue
        kt=tenkan[i]>kijun[i]; kt1=tenkan[i-1]<=kijun[i-1]
        kd=tenkan[i]<kijun[i]; kd1=tenkan[i-1]>=kijun[i-1]
        ku_top=max(sa[i],sb[i]); ku_bot=min(sa[i],sb[i])
        tk_up=kt and kt1; tk_dn=kd and kd1
        ck_l=chikou[i]>cl[i]; ck_s=chikou[i]<cl[i]
        long_ok  = tk_up and (not cloud_filter or cl[i]>ku_top)  and ck_l
        short_ok = tk_dn and (not cloud_filter or cl[i]<ku_bot) and ck_s
        ex_l = tenkan[i]<kijun[i] or (cloud_filter and cl[i]<ku_bot)
        ex_s = tenkan[i]>kijun[i] or (cloud_filter and cl[i]>ku_top)
        if state==0:
            if long_ok: state=1
            elif short_ok: state=-1
        elif state==1:
            if ex_l or short_ok: state=-1 if short_ok else 0
        elif state==-1:
            if ex_s or long_ok: state=1 if long_ok else 0
        sig[i]=state
    return sig

def ichimoku_triple(df, tenkan_n=7, kijun_n=20, senkou_b_n=60,
                    displacement=26, cloud_filter=True,
                    atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                    max_bars=0):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    t,k,sa,sb,ch=ichimoku(h,l,c,tenkan_n,kijun_n,senkou_b_n,displacement)
    n_start=max(kijun_n,senkou_b_n,displacement)+1
    sig=_ichi_sig(t,k,sa,sb,ch,c,cloud_filter,n_start)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

ICHIMOKU_TRIPLE_GRID = dict(
    tenkan_n=[7,9], kijun_n=[20,26], senkou_b_n=[52,60],
    displacement=[26], cloud_filter=[True],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 8. VWAP Mean Reversion
# =============================================================================
def _vwap_session(df):
    ts=df['Timestamp'].values.astype('datetime64[D]').astype(np.int32)
    c=df['Close'].values; v=df['Volume'].values
    tp=(df['High'].values+df['Low'].values+c)/3.0
    n=len(c); vwap=np.full(n,np.nan)
    cum_tv=0.0; cum_v=0.0; cur_day=-1
    for i in range(n):
        if ts[i]!=cur_day: cum_tv=0.0; cum_v=0.0; cur_day=ts[i]
        cum_tv+=tp[i]*v[i]; cum_v+=v[i]
        vwap[i]=cum_tv/cum_v if cum_v>0 else np.nan
    return vwap

@njit(cache=True)
def _vwap_sig(c, vwap, atr_v, mult, new_day, session_bar, min_session_bar):
    # FIX: min_session_bar tatsächlich anwenden — keine Entries in den ersten
    # N Bars einer Session (VWAP noch nicht stabil genug).
    # flat_overnight ist implizit durch new_day: Position wird bei Tageswechsel
    # auf 0 gesetzt (UTC Mitternacht), was overnight_lockout entspricht.
    n=len(c); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if new_day[i]: state=0
        if np.isnan(vwap[i]) or np.isnan(atr_v[i]): sig[i]=state; continue
        band=mult*atr_v[i]
        can_enter = session_bar[i] >= min_session_bar
        if state==0:
            if can_enter:
                if c[i]<vwap[i]-band: state=1
                elif c[i]>vwap[i]+band: state=-1
        elif state==1:
            if c[i]>=vwap[i]: state=0
        elif state==-1:
            if c[i]<=vwap[i]: state=0
        sig[i]=state
    return sig

def vwap_mean_reversion(df, mult=2.0, min_session_bar=6,
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                        max_bars=0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    vwap=_vwap_session(df)
    a=calc_atr(h,l,c,atr_period)
    nd=new_day_mask(df['Timestamp'].values)
    # Berechne Bar-Nummer innerhalb der Session (UTC-Tag)
    ts=df['Timestamp'].values.astype('datetime64[D]').astype(np.int32)
    n=len(c); sbar=np.zeros(n,np.int32); cnt=0; cur=-1
    for i in range(n):
        if ts[i]!=cur: cur=ts[i]; cnt=0
        sbar[i]=cnt; cnt+=1
    sig=_vwap_sig(c,vwap,a,float(mult),nd,sbar,int(min_session_bar))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# Bewusst unveraendert (ausser dem toten flat_overnight): dient als Baseline
# gegen VWAP_Band_MR. Ankert intern weiter am UTC-Tag — genau das ist der
# Unterschied, den der Vergleich messen soll.
VWAP_MEAN_REVERSION_GRID = dict(
    mult=[1.5,2.0,2.5,3.0], min_session_bar=[1,6],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 9. SMA Crossover (FSM-Loop, bereits korrekt in R)
# =============================================================================
@njit(cache=True)
def _sma_sig(fast_v, slow_v):
    n=len(fast_v); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if np.isnan(fast_v[i]) or np.isnan(slow_v[i]): sig[i]=0; continue
        new_s = 1 if fast_v[i]>slow_v[i] else (-1 if fast_v[i]<slow_v[i] else 0)
        if new_s!=state: state=new_s
        sig[i]=state
    return sig

def sma_crossover(df, fast=20, slow=50,
                  atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                  max_bars=0):
    if fast>=slow: return np.zeros(len(df),np.int32)
    c=df['Close'].values
    sig=_sma_sig(sma(c,fast),sma(c,slow))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

SMA_CROSSOVER_GRID = dict(
    fast=[10,20,30], slow=[40,50,80,100,200],
    atr_period=[10,14], sl_atr_mult=[1.5,2.0,2.5], tp_atr_mult=[2.0,3.0,4.0]
)


# =============================================================================
# 10. Supertrend ATR
# =============================================================================
def supertrend_atr(df, st_period=10, st_mult=3.0,
                   atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                   max_bars=0):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    sig=calc_supertrend(h,l,c,st_period,float(st_mult))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

SUPERTREND_ATR_GRID = dict(
    st_period=[10,14,20], st_mult=[2.0,3.0,4.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 11. Keltner Squeeze Breakout
# =============================================================================
@njit(cache=True)
def _squeeze_sig(cl, ma, bb_u, bb_l, kc_u, kc_l):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if np.isnan(ma[i]): sig[i]=state; continue
        sq = bb_u[i]<kc_u[i] and bb_l[i]>kc_l[i]
        released = i>0 and (not sq) and (bb_u[i-1]<kc_u[i-1] and bb_l[i-1]>kc_l[i-1])
        if state==0:
            if released: state=1 if cl[i]>ma[i] else -1
        elif state==1:
            if cl[i]<ma[i]: state=0
        elif state==-1:
            if cl[i]>ma[i]: state=0
        sig[i]=state
    return sig

def keltner_squeeze_breakout(df, period=20, bb_mult=2.0, kc_mult=1.5,
                              atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                              max_bars=0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    ma=sma(c,period); sd=run_sd(c,period)
    a=calc_atr(h,l,c,period)
    bbu=ma+bb_mult*sd; bbl=ma-bb_mult*sd
    kcu=ma+kc_mult*a;  kcl=ma-kc_mult*a
    sig=_squeeze_sig(c,ma,bbu,bbl,kcu,kcl)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

KELTNER_SQUEEZE_GRID = dict(
    period=[20,30,50], bb_mult=[2.0], kc_mult=[1.0,1.5],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 12. MACD Trend Momentum
# =============================================================================
@njit(cache=True)
def _macd_sig(ml, sig_l, ema_t, cl):
    n=len(ml); sig=np.zeros(n,np.int32); state=0
    for i in range(1,n):
        if np.isnan(ml[i]) or np.isnan(sig_l[i]) or np.isnan(ema_t[i]): sig[i]=state; continue
        cu=ml[i]>sig_l[i] and ml[i-1]<=sig_l[i-1]
        cd=ml[i]<sig_l[i] and ml[i-1]>=sig_l[i-1]
        if state==0:
            if cu and cl[i]>ema_t[i]: state=1
            elif cd and cl[i]<ema_t[i]: state=-1
        elif state==1:
            if cd: state=0
        elif state==-1:
            if cu: state=0
        sig[i]=state
    return sig

def macd_trend_momentum(df, fast=12, slow=26, signal=9, trend_len=200,
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                        max_bars=0):
    c=df['Close'].values
    ml,sl_=calc_macd(c,fast,slow,signal)
    et=ema(c,trend_len)
    sig=_macd_sig(ml,sl_,et,c)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

MACD_TREND_MOMENTUM_GRID = dict(
    fast=[8,12], slow=[26], signal=[9], trend_len=[100,200],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 13. Session ORB
# =============================================================================
@njit(cache=True)
def _orb_sig(h, l, cl, hour, in_sess, can_enter, or_bars):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    or_h=np.nan; or_l=np.nan; cnt=0; done=False
    for i in range(n):
        sess_open = in_sess[i] and (i==0 or not in_sess[i-1])
        if sess_open: or_h=np.nan; or_l=np.nan; cnt=0; done=False; state=0
        if not in_sess[i]: state=0; sig[i]=0; continue
        if not done:
            or_h=h[i] if np.isnan(or_h) else max(or_h,h[i])
            or_l=l[i] if np.isnan(or_l) else min(or_l,l[i])
            cnt+=1
            if cnt>=or_bars: done=True
            sig[i]=state; continue
        if state==0 and can_enter[i]:
            if cl[i]>or_h: state=1
            elif cl[i]<or_l: state=-1
        sig[i]=state
    return sig

def session_orb(df, session_start_h=7, session_end_h=20, or_bars=6,
                last_entry_h=18, atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                max_bars=0):
    ts=pd.DatetimeIndex(df['Timestamp'])
    hr=ts.hour.values
    ins=(hr>=session_start_h)&(hr<session_end_h)
    cane=(hr>=session_start_h)&(hr<last_entry_h)
    sig=_orb_sig(df['High'].values,df['Low'].values,df['Close'].values,
                 hr,ins,cane,int(or_bars))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

SESSION_ORB_GRID = dict(
    session_start_h=[7,13], session_end_h=[20], or_bars=[3,6],
    last_entry_h=[18], atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 14. Trend RSI Pullback
# =============================================================================
@njit(cache=True)
def _trsi_sig(cl, ema_v, rsi_v, rsi_buy, rsi_sell):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    for i in range(1,n):
        if np.isnan(ema_v[i]) or np.isnan(rsi_v[i]): sig[i]=state; continue
        up_t=cl[i]>ema_v[i]; dn_t=cl[i]<ema_v[i]
        lt=up_t and rsi_v[i-1]<rsi_buy  and rsi_v[i]>=rsi_buy
        st=dn_t and rsi_v[i-1]>rsi_sell and rsi_v[i]<=rsi_sell
        if state==0:
            if lt: state=1
            elif st: state=-1
        elif state==1:
            if cl[i]<ema_v[i] or rsi_v[i]>=70: state=0
        elif state==-1:
            if cl[i]>ema_v[i] or rsi_v[i]<=30: state=0
        sig[i]=state
    return sig

def trend_rsi_pullback(df, ema_len=100, rsi_len=14, rsi_buy=40.0, rsi_sell=60.0,
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                       max_bars=0):
    c=df['Close'].values
    ev=ema(c,ema_len); rv=rsi_wilder(c,rsi_len)
    sig=_trsi_sig(c,ev,rv,float(rsi_buy),float(rsi_sell))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

TREND_RSI_PULLBACK_GRID = dict(
    ema_len=[50,100,200], rsi_len=[14], rsi_buy=[35.0,40.0], rsi_sell=[60.0,65.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 15. Gap Fade
# =============================================================================
@njit(cache=True)
def _gap_sig(cl, h, l, bar_min, bar_date, sess_start_min, exit_min,
             gap_arr, direction, tp_arr, sl_arr):
    # direction: 0=Both, 1=LongOnly, 2=ShortOnly
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    traded=False; cur_date=-1
    for i in range(n):
        if bar_date[i]!=cur_date: cur_date=bar_date[i]; traded=False; state=0
        if bar_min[i]>=exit_min: state=0; sig[i]=0; continue
        if bar_min[i]==sess_start_min and not traded and not np.isnan(gap_arr[i]):
            g=gap_arr[i]
            if (g<0 and direction!=1) or (g>0 and direction!=2):
                state = -1 if g<0 else 1   # fade the gap
                traded=True
        sig[i]=state
    return sig

def gap_fade(df, prev_close_h=21, sess_start_h=9, sess_start_m=0,
             exit_h=15, exit_m=0, gap_thresh=0.7, tp_pct=0.4, sl_pct=1.0,
             direction='Both', atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
             max_bars=0):
    ts=pd.DatetimeIndex(df['Timestamp'])
    hr=ts.hour.values; mn=ts.minute.values
    bar_min=(hr*60+mn).astype(np.int32)
    bar_date=ts.normalize().asi8.astype(np.int64)  # nanoseconds, unique per date
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    n=len(c)
    sess_start_min=int(sess_start_h*60+sess_start_m)
    exit_min_v=int(exit_h*60+exit_m)
    # reference close: last bar of prev_close_h each day
    unique_dates=np.unique(bar_date)
    ref_close={}
    for d in unique_dates:
        mask=(bar_date==d)&(hr==prev_close_h)
        if mask.any(): ref_close[d]=c[np.where(mask)[0][-1]]
    gap_arr=np.full(n,np.nan)
    for i in range(n):
        if bar_min[i]==sess_start_min:
            d=bar_date[i]
            dates_sorted=sorted(ref_close.keys())
            idx=np.searchsorted(dates_sorted,d)
            if idx>0:
                prev_c=ref_close[dates_sorted[idx-1]]
                if prev_c>0: gap_arr[i]=(c[i]-prev_c)/prev_c*100
    dir_map={'Both':0,'LongOnly':1,'ShortOnly':2}
    dir_code=dir_map.get(direction,0)
    sig=_gap_sig(c,h,l,bar_min,bar_date,sess_start_min,exit_min_v,
                 gap_arr,dir_code,np.full(n,tp_pct),np.full(n,sl_pct))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

GAP_FADE_GRID = dict(
    prev_close_h=[21,22], sess_start_h=[9,8], sess_start_m=[0],
    exit_h=[15,16], exit_m=[0], gap_thresh=[0.3,0.5,0.7,1.0,1.5],
    direction=['Both','LongOnly','ShortOnly'],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 16. Tokyo Gap Range
# =============================================================================
@njit(cache=True)
def _tokyo_sig(h, l, cl, bar_min, bar_date, sess_start, sess_end, or_candles,
               gap_atr_v, atr_v, last_entry, adx_v, use_adx):
    n=len(cl); sig=np.zeros(n,np.int32); state=0
    or_h=np.nan; or_l=np.nan; cnt=0; done=False; cur_date=-1
    for i in range(n):
        if bar_date[i]!=cur_date:
            cur_date=bar_date[i]; or_h=np.nan; or_l=np.nan
            cnt=0; done=False; state=0
        if bar_min[i]>=sess_end: state=0; sig[i]=0; continue
        if bar_min[i]<sess_start: sig[i]=state; continue
        if not done:
            or_h=h[i] if np.isnan(or_h) else max(or_h,h[i])
            or_l=l[i] if np.isnan(or_l) else min(or_l,l[i])
            cnt+=1
            if cnt>=or_candles: done=True
            sig[i]=state; continue
        if bar_min[i]>=last_entry: sig[i]=state; continue
        if state==0:
            if np.isnan(atr_v[i]) or np.isnan(gap_atr_v[i]): sig[i]=state; continue
            ok_adx=True
            if use_adx and not np.isnan(adx_v[i]): ok_adx=adx_v[i]>25
            gap_ok=not np.isnan(or_h) and (or_h-or_l)>gap_atr_v[i]
            if gap_ok and ok_adx:
                if cl[i]>or_h: state=1
                elif cl[i]<or_l: state=-1
        sig[i]=state
    return sig

def tokyo_gap_range(df, or_candles=6, gap_atr_mult=0.5, atr_n=14,
                    session_start_h=0, session_end_h=9, last_entry_h=8,
                    adx_filter=False, atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0,
                    max_bars=0):
    ts=pd.DatetimeIndex(df['Timestamp'])
    hr=ts.hour.values; mn=ts.minute.values
    bmin=(hr*60+mn).astype(np.int32)
    bdate=ts.normalize().asi8.astype(np.int64)
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    a=calc_atr(h,l,c,atr_n)
    adx_v,_,_ = adx(h,l,c,14)
    gap_atr_v=a*gap_atr_mult
    sig=_tokyo_sig(h,l,c,bmin,bdate,
                   session_start_h*60, session_end_h*60,
                   int(or_candles), gap_atr_v, a, last_entry_h*60,
                   adx_v, bool(adx_filter))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

TOKYO_GAP_RANGE_GRID = dict(
    or_candles=[4,6], gap_atr_mult=[0.3,0.5],
    atr_n=[14], session_start_h=[0], session_end_h=[9],
    last_entry_h=[7,8], adx_filter=[False],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# 17. Silver/Asia Breakout V4
# =============================================================================
def silver_asia_breakout(df, asia_start_hour=0, asia_end_hour=6,
                         vol_lookback=20, vol_threshold=1.0,
                         kc_length=45, kc_mult=1.3,
                         ma_fast_len=108, ma_slow_len=252,
                         min_range=0.0, breakout_window=12,
                         min_kc_dist_pct=0.2,
                         rsi_length=14, div_lookback=20, max_dtd_pct=3.0,
                         atr_length=14, sl_atr_mult=2.5, min_sl_pct=1.5,
                         tp_pct=1.5, trail_atr_mult=3.0,
                         atr_period=14, tp_atr_mult=2.0,
                         max_bars=0):
    ts=pd.DatetimeIndex(df['Timestamp'])
    hr=ts.hour.values
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    v=df['Volume'].values; n=len(c)

    vol_ma=sma(v.astype(float),vol_lookback)
    vol_ratio=np.where((vol_ma==0)|np.isnan(vol_ma),0.0,v/vol_ma)
    kcu,kcl=keltner(h,l,c,kc_length,kc_mult)
    maf=ema(c,ma_fast_len); mas=ema(c,ma_slow_len)
    rsi_v=rsi_wilder(c,rsi_length)

    asia_in=(hr>=asia_start_hour)&(hr<asia_end_hour)
    bdate=ts.normalize().asi8.astype(np.int64)
    unique_dates=np.unique(bdate)

    sig=np.zeros(n,np.int32)
    for d in unique_dates:
        asia_mask=(bdate==d)&asia_in
        if not asia_mask.any(): continue
        asia_idx=np.where(asia_mask)[0]
        ah=h[asia_idx].max(); al=l[asia_idx].min()
        ar=ah-al
        if ar<min_range: continue
        # Post-asia bars on same date
        post_mask=(bdate==d)&(~asia_in)
        post_idx=np.where(post_mask)[0]
        if len(post_idx)==0: continue
        post_idx=post_idx[:breakout_window]
        for i in post_idx:
            if np.isnan(maf[i]) or np.isnan(mas[i]): continue
            vol_ok=vol_ratio[i]>=vol_threshold
            kc_long_ok  = (c[i]-kcu[i])/c[i]*100>=min_kc_dist_pct
            kc_short_ok = (kcl[i]-c[i])/c[i]*100>=min_kc_dist_pct
            ma_bull=maf[i]>mas[i]; ma_bear=maf[i]<mas[i]
            if vol_ok and c[i]>ah and kc_long_ok and ma_bull:
                sig[i]=1
            elif vol_ok and c[i]<al and kc_short_ok and ma_bear:
                sig[i]=-1
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           overnight_lockout=True, max_bars=max_bars)

SILVER_ASIA_BREAKOUT_GRID = dict(
    vol_lookback=[15,20], vol_threshold=[0.8,1.0],
    kc_length=[30,45], kc_mult=[1.0,1.3],
    ma_fast_len=[108], ma_slow_len=[252],
    min_range=[0.0], breakout_window=[6,12],
    min_kc_dist_pct=[0.1,0.2],
    rsi_length=[14], div_lookback=[20], max_dtd_pct=[3.0],
    atr_period=[14], sl_atr_mult=[2.5], tp_atr_mult=[2.0,3.0]
)


# =============================================================================
# GEMEINSAME HELFER FUER DIE SESSION-BASIERTEN MR-STRATEGIEN (18-22)
# =============================================================================
@njit(cache=True)
def _in_window(h, hour_start, hour_end):
    """Stunden-Fenster (lokale Zeit) mit Wrap-around.
    hour_start=SESSION_START_H, hour_end=SESSION_END_H = ganze Session."""
    if hour_start <= hour_end:
        return (h >= hour_start) and (h < hour_end)
    return (h >= hour_start) or (h < hour_end)


@njit(cache=True)
def _session_bar_idx(sb):
    """Bar-Nummer innerhalb der Session (0 = erster Bar nach dem Break)."""
    n = len(sb); out = np.zeros(n, np.int32); cnt = 0
    for i in range(n):
        if sb[i]: cnt = 0
        out[i] = cnt; cnt += 1
    return out


def _hours(df):
    """Lokale Stunde (Europe/Berlin) — alle Stunden-Parameter sind lokal gemeint,
    damit sie die Sommerzeit mitmachen und zur Session-Definition passen."""
    return local_hours(df['Timestamp'].values, SESSION_TZ)


def _zscore(c, period):
    ma = sma(c, period); sd = run_sd(c, period)
    sd = np.where((sd == 0) | np.isnan(sd), np.nan, sd)
    return (c - ma) / sd


# =============================================================================
# 18. Session Z-Score MR  — z-Score MR mit Handelsfenster + Regime-Gates
# Literatur: der Intraday-Reversal in Commodity-Futures ist liquiditaets-
# getrieben und auf bestimmte Session-Bloecke konzentriert (Zheng & Luo);
# MR braucht ein Ranging-Regime (ADX niedrig, ATR unter eigenem Schnitt).
# Messung auf Copper M15: Block 13-17 UTC beidseitig positiv, 18-20 UTC
# beidseitig negativ — bisher hatte keine MR-Strategie einen Session-Filter.
# =============================================================================
@njit(cache=True)
def _szmr_sig(z, hour, adx_v, atr_ratio, entry_z, exit_z,
              hour_start, hour_end, adx_max, atr_ratio_max):
    n=len(z); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        zi=z[i]
        if np.isnan(zi): sig[i]=state; continue
        calm=True
        if not np.isnan(adx_v[i]) and adx_v[i]>adx_max: calm=False
        if not np.isnan(atr_ratio[i]) and atr_ratio[i]>atr_ratio_max: calm=False
        # Gates nur fuer Entries — ein offener Trade darf jederzeit exiten
        if state==0:
            if calm and _in_window(hour[i],hour_start,hour_end):
                if zi<=-entry_z: state=1
                elif zi>=entry_z: state=-1
        elif state==1:
            if zi>=-exit_z: state=0
        elif state==-1:
            if zi<=exit_z: state=0
        sig[i]=state
    return sig

def session_zscore_mr(df, period=32, entry_z=2.0, exit_z=0.0,
                      hour_start=14, hour_end=19,
                      adx_max=25.0, adx_len=14,
                      atr_ratio_max=1.5, atr_ratio_len=100,
                      atr_period=14, sl_atr_mult=2.5, tp_atr_mult=1.25,
                      max_bars=16):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    z=_zscore(c,period)
    a=calc_atr(h,l,c,atr_period)
    a_ma=sma(a,atr_ratio_len)
    a_ma=np.where((a_ma==0)|np.isnan(a_ma), np.nan, a_ma)
    ratio=a/a_ma
    adx_v,_,_=adx(h,l,c,adx_len)
    sig=_szmr_sig(z.astype(np.float64), _hours(df), adx_v, ratio,
                  float(entry_z), float(exit_z),
                  int(hour_start), int(hour_end),
                  float(adx_max), float(atr_ratio_max))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# Stunden lokal (Europe/Berlin). hour_start=1, hour_end=22 = ganze Session
# (Kontrollgruppe). adx_max=100 / atr_ratio_max=99 schalten das jeweilige Gate ab.
SESSION_ZSCORE_MR_GRID = dict(
    period=[16,24,32,48,64], entry_z=[1.5,2.0,2.5,3.0], exit_z=[0.0,0.5,1.0],
    hour_start=[1,8,14], hour_end=[8,14,19,22],
    adx_max=[20.0,25.0,100.0], adx_len=[14],
    atr_ratio_max=[1.0,1.5,99.0], atr_ratio_len=[100],
    atr_period=[10,14], sl_atr_mult=[2.0,3.0], tp_atr_mult=[0.75,1.25,2.0],
    max_bars=[0,16,32]
)


# =============================================================================
# 19. VWAP Band MR — Session-VWAP mit Stdev-Baendern der Abweichung
# Literatur: Reversion an +/-2SD-VWAP-Baendern (~61-63% Trefferquote),
# VWAP an den Session-Open ankern statt an den Kalendertag.
# Unterschied zu VWAP_MeanReversion: Band-Basis (Stdev statt ATR),
# Anker (konfigurierbare Stunde statt UTC-Tag) und Zeit-Stop.
# =============================================================================
@njit(cache=True)
def _vwap_anchor_core(tp, v, sb):
    n=len(tp); vwap=np.full(n,np.nan)
    cum_tv=0.0; cum_v=0.0
    for i in range(n):
        if sb[i]: cum_tv=0.0; cum_v=0.0
        cum_tv+=tp[i]*v[i]; cum_v+=v[i]
        if cum_v>0: vwap[i]=cum_tv/cum_v
    return vwap

@njit(cache=True)
def _vband_sig(c, vwap, dev_sd, sbar, hour, entry_sd, exit_frac,
               min_session_bar, hour_start, hour_end):
    n=len(c); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        if np.isnan(vwap[i]) or np.isnan(dev_sd[i]) or dev_sd[i]<=0:
            sig[i]=state; continue
        d=(c[i]-vwap[i])/dev_sd[i]
        if state==0:
            if sbar[i]>=min_session_bar and _in_window(hour[i],hour_start,hour_end):
                if d<=-entry_sd: state=1
                elif d>=entry_sd: state=-1
        elif state==1:
            if d>=-exit_frac*entry_sd: state=0
        elif state==-1:
            if d<=exit_frac*entry_sd: state=0
        sig[i]=state
    return sig

def vwap_band_mr(df, anchor_h=SESSION_START_H, entry_sd=2.0, exit_frac=0.0, sd_len=96,
                 min_session_bar=6, hour_start=SESSION_START_H, hour_end=SESSION_END_H,
                 atr_period=14, sl_atr_mult=2.5, tp_atr_mult=1.25,
                 max_bars=0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    v=df['Volume'].values.astype(np.float64)
    tp=(h+l+c)/3.0
    sb=session_break_mask(df['Timestamp'].values, int(anchor_h), SESSION_TZ)
    vwap=_vwap_anchor_core(tp, v, sb)
    sbar=_session_bar_idx(sb)
    dev_sd=run_sd(c-vwap, sd_len)
    sig=_vband_sig(c, vwap, dev_sd, sbar, _hours(df),
                   float(entry_sd), float(exit_frac), int(min_session_bar),
                   int(hour_start), int(hour_end))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# exit_frac=0 -> Exit am VWAP, 0.5 -> auf halbem Weg zurueck.
VWAP_BAND_MR_GRID = dict(
    anchor_h=[1], entry_sd=[1.5,2.0,2.5,3.0], exit_frac=[0.0,0.25,0.5],
    sd_len=[48,96,192], min_session_bar=[6,12],
    hour_start=[1,14], hour_end=[19,22],
    atr_period=[10,14], sl_atr_mult=[2.0,3.0], tp_atr_mult=[0.75,1.25,2.0],
    max_bars=[0,16,32]
)


# =============================================================================
# 20. OU Half-Life MR — Fenster und Zeit-Stop aus der Halbwertszeit abgeleitet
# Literatur: Ornstein-Uhlenbeck-MR, Sizing ueber die Halbwertszeit, Gating auf
# die Reversionsgeschwindigkeit. Als einzige Strategie hier setzt sie den
# Horizont nicht voraus, sondern schaetzt ihn — und der Horizont ist auf Copper
# genau die Stelle, an der der Edge sitzt (k~16 Bars auf M15).
# Der Zeit-Stop ist pro Bar variabel und daher hier im Signal, nicht im FSM.
# =============================================================================
@njit(cache=True)
def _ou_sig(c, hl, entry_z, exit_z, hl_min, hl_max, lb_mult, ts_mult):
    n=len(c); sig=np.zeros(n,np.int32); state=0
    ps=np.zeros(n+1); ps2=np.zeros(n+1)
    for i in range(n):
        ps[i+1]=ps[i]+c[i]; ps2[i+1]=ps2[i]+c[i]*c[i]
    bars_in=0; stop_at=0
    for i in range(n):
        hli=hl[i]
        if np.isnan(hli) or hli<hl_min or hli>hl_max:
            # kein verwertbares MR-Regime: laufenden Trade halten, nichts Neues
            if state!=0:
                bars_in+=1
                if bars_in>=stop_at: state=0; bars_in=0
            sig[i]=state; continue
        lb=int(lb_mult*hli)
        if lb<5: lb=5
        if i<lb:
            sig[i]=state; continue
        s=ps[i+1]-ps[i+1-lb]; s2=ps2[i+1]-ps2[i+1-lb]
        m=s/lb
        var=(s2-lb*m*m)/(lb-1)
        if var<=0: sig[i]=state; continue
        z=(c[i]-m)/np.sqrt(var)
        if state==0:
            if z<=-entry_z: state=1
            elif z>=entry_z: state=-1
            if state!=0:
                bars_in=0
                stop_at=int(ts_mult*hli)
                if stop_at<1: stop_at=1
        else:
            bars_in+=1
            if state==1 and z>=-exit_z: state=0
            elif state==-1 and z<=exit_z: state=0
            elif bars_in>=stop_at: state=0
            if state==0: bars_in=0
        sig[i]=state
    return sig

def ou_halflife_mr(df, fit_len=500, hl_min=4.0, hl_max=64.0, hl_cap=500.0,
                   lb_mult=3.0, ts_mult=2.0, entry_z=2.0, exit_z=0.0,
                   atr_period=14, sl_atr_mult=2.5, tp_atr_mult=1.25,
                   max_bars=0):
    c=df['Close'].values.astype(np.float64)
    hl=rolling_halflife(c, int(fit_len), float(hl_cap))
    sig=_ou_sig(c, hl, float(entry_z), float(exit_z),
                float(hl_min), float(hl_max), float(lb_mult), float(ts_mult))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# hl_min/hl_max in Bars: auf M15 entspricht 8-64 rund 2h-16h.
OU_HALFLIFE_MR_GRID = dict(
    fit_len=[250,500,1000], hl_min=[2.0,4.0,8.0], hl_max=[32.0,64.0,128.0],
    hl_cap=[500.0], lb_mult=[2.0,3.0,4.0], ts_mult=[1.0,2.0,3.0],
    entry_z=[1.5,2.0,2.5], exit_z=[0.0,0.5],
    atr_period=[10,14], sl_atr_mult=[2.0,3.0], tp_atr_mult=[0.75,1.25,2.0],
    max_bars=[0]
)


# =============================================================================
# 21. Overnight Reversal — faded den Session-Open-Move in ATR-Einheiten
# Literatur: der Overnight-/Opening-Faktor ist der signifikanteste Intraday-
# Praediktor in Commodity-Futures; kleinere Gaps schliessen ueberwiegend
# am selben Tag. Ersetzt gap_fade, dessen gap_thresh/tp_pct/sl_pct nie
# gelesen werden (dessen 960er-Grid enthaelt dadurch 5x exakte Duplikate).
# =============================================================================
@njit(cache=True)
def _onrev_sig(c, sbar, ref_close, atr_v, measure_bars, entry_atr,
               target_frac, exit_bar):
    n=len(c); sig=np.zeros(n,np.int32); state=0
    entry_px=np.nan; move=np.nan; traded=False
    for i in range(n):
        if sbar[i]==0:
            state=0; traded=False; entry_px=np.nan; move=np.nan
        if sbar[i]>=exit_bar:
            state=0; sig[i]=0; continue
        if (not traded) and sbar[i]==measure_bars:
            rc=ref_close[i]; a=atr_v[i]
            if (not np.isnan(rc)) and (not np.isnan(a)) and a>0:
                mv=c[i]-rc
                if abs(mv)>=entry_atr*a:
                    state = -1 if mv>0 else 1      # gegen den Open-Move
                    entry_px=c[i]; move=mv; traded=True
        elif state!=0 and not np.isnan(move):
            # target_frac des Moves zurueckgelaufen; Ausdruck gilt fuer beide Seiten
            tgt=entry_px-target_frac*move
            if state==1 and c[i]>=tgt: state=0
            elif state==-1 and c[i]<=tgt: state=0
        sig[i]=state
    return sig

def overnight_reversal(df, anchor_h=0, measure_bars=4, entry_atr=1.0,
                       target_frac=0.5, exit_bar=64,
                       atr_period=14, sl_atr_mult=2.5, tp_atr_mult=1.5,
                       max_bars=0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    sb=session_break_mask(df['Timestamp'].values, int(anchor_h), SESSION_TZ)
    sbar=_session_bar_idx(sb)
    n=len(c)
    # Referenz = letzter Close vor dem Session-Break, konstant ueber die Session
    ref=np.full(n, np.nan)
    idx=np.where(sb)[0]
    for j in range(len(idx)):
        k=idx[j]
        end=idx[j+1] if j+1<len(idx) else n
        if k>0: ref[k:end]=c[k-1]
    a=calc_atr(h,l,c,atr_period)
    sig=_onrev_sig(c, sbar, ref, a, int(measure_bars), float(entry_atr),
                   float(target_frac), int(exit_bar))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

# anchor_h=0: der Anker ist der echte Marktstart 00:00 lokal, damit die Referenz
# der letzte Close VOR der 23:00-Pause ist (sonst waere es kein Overnight-Move).
# Gehandelt wird trotzdem erst ab 01:00 — measure_bars muss also gross genug
# sein, dass der Einstieg in der Session liegt (M15: >=4, M5: >=12).
OVERNIGHT_REVERSAL_GRID = dict(
    anchor_h=[0], measure_bars=[4,8,12,16], entry_atr=[0.5,1.0,1.5,2.0],
    target_frac=[0.3,0.5,1.0], exit_bar=[16,32,64],
    atr_period=[10,14], sl_atr_mult=[2.0,3.0], tp_atr_mult=[0.75,1.5,2.5],
    max_bars=[0,16,32]
)


# =============================================================================
# 22. RSI Extreme MR — Connors-RSI(2) mit Zeit-Stop
# Literatur: Connors RSI(2); Renditen bei Einstieg unter RSI 5 hoeher als
# unter 10, Exit nach 5-10 Bars. Deckt die kurze Extrem-Ecke ab, die
# RSI_MeanReversion nie erreicht (period>=10, lower>=20).
# Bewusst als Falsifikation gefahren: die Messung auf Copper zeigt auf
# k=2-6 Bars keinen Edge. Billig, und schliesst die Region ab.
# =============================================================================
@njit(cache=True)
def _rsix_sig(rsi, hour, lower, upper, exit_level, hour_start, hour_end):
    n=len(rsi); sig=np.zeros(n,np.int32); state=0
    for i in range(n):
        r=rsi[i]
        if np.isnan(r): sig[i]=state; continue
        if state==0:
            if _in_window(hour[i],hour_start,hour_end):
                if r<lower: state=1
                elif r>upper: state=-1
        elif state==1:
            if r>=exit_level: state=0
        elif state==-1:
            if r<=100.0-exit_level: state=0
        sig[i]=state
    return sig

def rsi_extreme_mr(df, period=3, lower=10.0, upper=90.0, exit_level=50.0,
                   hour_start=SESSION_START_H, hour_end=SESSION_END_H,
                   atr_period=14, sl_atr_mult=2.5, tp_atr_mult=1.25,
                   max_bars=8):
    r=rsi_wilder(df['Close'].values, period)
    sig=_rsix_sig(r.astype(np.float64), _hours(df),
                  float(lower), float(upper), float(exit_level),
                  int(hour_start), int(hour_end))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                           max_bars=max_bars)

RSI_EXTREME_MR_GRID = dict(
    period=[2,3,4,5,6], lower=[5.0,10.0,15.0], upper=[85.0,90.0,95.0],
    exit_level=[50.0,65.0,75.0], hour_start=[1,14], hour_end=[19,22],
    atr_period=[10,14], sl_atr_mult=[2.0,3.0], tp_atr_mult=[0.75,1.25,2.0],
    max_bars=[5,10,16]
)


# =============================================================================
# REGISTRY — {name: (fn, param_grid)}
# =============================================================================
STRATEGIES = {
    "RSI_MeanReversion":         (rsi_mean_reversion,      RSI_MEAN_REVERSION_GRID),
    "Bollinger_ZScore_MR":       (bollinger_zscore_mr,      BOLLINGER_ZSCORE_MR_GRID),
    "ATRDistance_MeanReversion": (atr_distance_mr,          ATR_DISTANCE_MR_GRID),
    "CCI_MeanReversion":         (cci_mean_reversion,       CCI_MEAN_REVERSION_GRID),
    "CMO_VHF_STC":               (cmo_vhf_stc,              CMO_VHF_STC_GRID),
    "Donchian_Breakout":         (donchian_crossover,       DONCHIAN_CROSSOVER_GRID),
    "Ichimoku_7_20_60":          (ichimoku_triple,          ICHIMOKU_TRIPLE_GRID),
    "VWAP_MeanReversion":        (vwap_mean_reversion,      VWAP_MEAN_REVERSION_GRID),
    "SMA_Crossover":             (sma_crossover,            SMA_CROSSOVER_GRID),
    "Supertrend_ATR":            (supertrend_atr,           SUPERTREND_ATR_GRID),
    "Keltner_Squeeze_Breakout":  (keltner_squeeze_breakout, KELTNER_SQUEEZE_GRID),
    "MACD_Trend_Momentum":       (macd_trend_momentum,      MACD_TREND_MOMENTUM_GRID),
    "Session_ORB":               (session_orb,              SESSION_ORB_GRID),
    "Trend_RSI_Pullback":        (trend_rsi_pullback,       TREND_RSI_PULLBACK_GRID),
    # "GapFade" ist bewusst NICHT registriert: gap_thresh/tp_pct/sl_pct werden
    # im Body nie gelesen, das 960er-Grid bestand daher zu 4/5 aus Duplikaten.
    # Overnight_Reversal loest sie ab; Funktion + Grid bleiben fuer die Historie.
    "Tokyo_Gap_Range":           (tokyo_gap_range,          TOKYO_GAP_RANGE_GRID),
    "SilverAsia_BreakoutV4":     (silver_asia_breakout,     SILVER_ASIA_BREAKOUT_GRID),
    # --- neu: MR-Kandidaten aus der Copper-Analyse ---
    "Session_ZScore_MR":         (session_zscore_mr,        SESSION_ZSCORE_MR_GRID),
    "VWAP_Band_MR":              (vwap_band_mr,             VWAP_BAND_MR_GRID),
    "OU_HalfLife_MR":            (ou_halflife_mr,           OU_HALFLIFE_MR_GRID),
    "Overnight_Reversal":        (overnight_reversal,       OVERNIGHT_REVERSAL_GRID),
    "RSI_Extreme_MR":            (rsi_extreme_mr,           RSI_EXTREME_MR_GRID),
}