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
                        new_day_mask, rsi_divergence)


# =============================================================================
# GEMEINSAMER FSM-BARRIER-APPLIKATOR (Numba JIT)
# =============================================================================
@njit(cache=True)
def fsm_barrier(sig, high, low, close, atr_v, new_day,
                sl_mult, tp_mult, overnight_lockout=False):
    n = len(sig)
    pos    = np.zeros(n, np.int32)
    sl_out = np.full(n, np.nan)
    tp_out = np.full(n, np.nan)
    state = 0; sl = np.nan; tp = np.nan; locked = 0
    for i in range(n):
        s = sig[i]; a = atr_v[i]; px = close[i]
        if new_day[i] and state != 0:
            if overnight_lockout: locked = state
            state = 0; sl = np.nan; tp = np.nan
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
        if locked != 0 and s != locked: locked = 0
        if state == 0:
            if s != 0 and s != locked and not np.isnan(a):
                state = s
                if s == 1: sl=px-sl_mult*a; tp=px+tp_mult*a
                else:      sl=px+sl_mult*a; tp=px-tp_mult*a
        else:
            if s == 0: state=0; sl=np.nan; tp=np.nan
            elif s != state:
                state = s
                if s == 1: sl=px-sl_mult*a; tp=px+tp_mult*a
                else:      sl=px+sl_mult*a; tp=px-tp_mult*a
        pos[i] = state
        if state != 0: sl_out[i]=sl; tp_out[i]=tp
    return pos, sl_out, tp_out


def _apply_barriers(raw_sig, df, atr_period, sl_atr_mult, tp_atr_mult,
                    overnight_lockout=False):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    a=calc_atr(h,l,c,atr_period)
    nd=new_day_mask(df['Timestamp'].values)
    sig=raw_sig.copy().astype(np.float64); sig[np.isnan(a)]=0
    pos,_,_ = fsm_barrier(sig,h,l,c,a,nd,sl_atr_mult,tp_atr_mult,overnight_lockout)
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

def rsi_mean_reversion(df, period=14, lower=30.0, upper=70.0, exit_mid=True,
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    r=rsi_wilder(df['Close'].values, period)
    sig=_rsi_sig(r.astype(np.float64), float(lower), float(upper))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

RSI_MEAN_REVERSION_GRID = dict(
    period=[7,10,14,18,21,24], lower=[20,25,30,35], upper=[65,70,75,80],
    exit_mid=[True], atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
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
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values
    ma=sma(c,period); sd=run_sd(c,period)
    z=np.where(sd==0, 0.0, (c-ma)/sd)
    sig=_bollinger_sig(z,sd,float(entry_z),float(exit_z))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

BOLLINGER_ZSCORE_MR_GRID = dict(
    period=[10,20,30,50], entry_z=[1.5,2.0,2.5], exit_z=[0.0,0.5],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
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

def atr_distance_mr(df, ema_period=30, atr_period=14, mult=2.0, exit_mid=True,
                    sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    e=ema(c,ema_period); a=calc_atr(h,l,c,atr_period)
    sig=_atr_mr_sig(c, e, e+mult*a, e-mult*a)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

ATR_DISTANCE_MR_GRID = dict(
    ema_period=[20,30,50], atr_period=[10,14,20], mult=[1.5,2.0,2.5],
    exit_mid=[True], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
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
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=calc_cci(df['High'].values,df['Low'].values,df['Close'].values,period)
    sig=_cci_sig(c,float(threshold),float(exit_level))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

CCI_MEAN_REVERSION_GRID = dict(
    period=[14,20,30,50], threshold=[80.0,100.0,150.0], exit_level=[0.0],
    atr_period=[10,14], sl_atr_mult=[1.5,2.5], tp_atr_mult=[2.0,3.0]
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
                atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values
    cv=cmo(c,cmo_period); vv=vhf(c,vhf_period); sv=stc(c,stc_fast,stc_slow,stc_cycle)
    sig=_cmo_sig(cv,vv,sv,float(cmo_threshold),float(vhf_threshold),
                 float(stc_entry_long),float(stc_entry_short))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    h=df['High'].values; l=df['Low'].values
    # shift 1 = R data.table::shift(x,1) = previous bar's value (no lookahead)
    upper = np.roll(run_max(h,lookback),1); upper[0]=np.nan
    lower = np.roll(run_min(l,lookback),1); lower[0]=np.nan
    eu    = np.roll(run_max(h,exit_n),1);   eu[0]=np.nan
    el    = np.roll(run_min(l,exit_n),1);   el[0]=np.nan
    sig=_donchian_sig(h,l,upper,lower,eu,el)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                    atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    t,k,sa,sb,ch=ichimoku(h,l,c,tenkan_n,kijun_n,senkou_b_n,displacement)
    n_start=max(kijun_n,senkou_b_n,displacement)+1
    sig=_ichi_sig(t,k,sa,sb,ch,c,cloud_filter,n_start)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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

def vwap_mean_reversion(df, mult=2.0, min_session_bar=6, flat_overnight=True,
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
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
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

VWAP_MEAN_REVERSION_GRID = dict(
    mult=[1.5,2.0,2.5,3.0], min_session_bar=[1,6],
    flat_overnight=[True],
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
                  atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    if fast>=slow: return np.zeros(len(df),np.int32)
    c=df['Close'].values
    sig=_sma_sig(sma(c,fast),sma(c,slow))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

SMA_CROSSOVER_GRID = dict(
    fast=[10,20,30], slow=[40,50,80,100,200],
    atr_period=[10,14], sl_atr_mult=[1.5,2.0,2.5], tp_atr_mult=[2.0,3.0,4.0]
)


# =============================================================================
# 10. Supertrend ATR
# =============================================================================
def supertrend_atr(df, st_period=10, st_mult=3.0,
                   atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    h=df['High'].values; l=df['Low'].values; c=df['Close'].values
    sig=calc_supertrend(h,l,c,st_period,float(st_mult))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                              atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values; h=df['High'].values; l=df['Low'].values
    ma=sma(c,period); sd=run_sd(c,period)
    a=calc_atr(h,l,c,period)
    bbu=ma+bb_mult*sd; bbl=ma-bb_mult*sd
    kcu=ma+kc_mult*a;  kcl=ma-kc_mult*a
    sig=_squeeze_sig(c,ma,bbu,bbl,kcu,kcl)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                        atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values
    ml,sl_=calc_macd(c,fast,slow,signal)
    et=ema(c,trend_len)
    sig=_macd_sig(ml,sl_,et,c)
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                last_entry_h=18, atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    ts=pd.DatetimeIndex(df['Timestamp'])
    hr=ts.hour.values
    ins=(hr>=session_start_h)&(hr<session_end_h)
    cane=(hr>=session_start_h)&(hr<last_entry_h)
    sig=_orb_sig(df['High'].values,df['Low'].values,df['Close'].values,
                 hr,ins,cane,int(or_bars))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                       atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
    c=df['Close'].values
    ev=ema(c,ema_len); rv=rsi_wilder(c,rsi_len)
    sig=_trsi_sig(c,ev,rv,float(rsi_buy),float(rsi_sell))
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
             direction='Both', atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
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
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                    adx_filter=False, atr_period=14, sl_atr_mult=1.5, tp_atr_mult=2.0):
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
    return _apply_barriers(sig, df, atr_period, sl_atr_mult, tp_atr_mult)

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
                         atr_period=14, tp_atr_mult=2.0):
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
                           overnight_lockout=True)

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
    "GapFade":                   (gap_fade,                 GAP_FADE_GRID),
    "Tokyo_Gap_Range":           (tokyo_gap_range,          TOKYO_GAP_RANGE_GRID),
    "SilverAsia_BreakoutV4":     (silver_asia_breakout,     SILVER_ASIA_BREAKOUT_GRID),
}