"""
runner.py — Walk-Forward Backtest Runner (Python, Numba-beschleunigt)
"""
import numpy as np
import pandas as pd
import itertools, time, os, sys, warnings
from multiprocessing import Pool, cpu_count
from pathlib import Path
warnings.filterwarnings('ignore')

# ---------------------------------------------------------------------------
# Konfiguration
# ---------------------------------------------------------------------------
IS_SPLIT = 0.70

SPREAD_BP = {   # Pepperstone bp/Side
    "USDJPY":1.4,"AUDJPY":5.7,"EURNZD":2.93,"OIL_BRENT":2.34,
    "DE40":0.18,"GOLD":0.22,"EURUSD":1.57,"EURGBP":2.31,
    "AUDUSD":1.71,"EURJPY":2.4,"USDCAD":0.95,"US500":0.27,
    "SILVER":4.01,"COPPER":1.57
}
SPREAD_DEFAULT = 5.0

# FIX: F3/F4/F7 aktiviert. F7 ist hartes Kriterium — alle Jahre profitabel.
ACTIVE_FILTERS = ["F1","F2","F3","F4","F5","F6","F7"]
# FIX: MIN_YEAR_BARS erhöht (2000 = ~7 Handelstage war zu wenig für sinnvollen F7-Check)
MIN_YEAR_BARS  = 5000


# ---------------------------------------------------------------------------
# Daten
# ---------------------------------------------------------------------------
def load_csv(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df.columns = [c.lower() for c in df.columns]
    rename = {'time':'Timestamp','open':'Open','high':'High',
              'low':'Low','close':'Close','volume':'Volume'}
    df.rename(columns=rename, inplace=True)
    df['Timestamp'] = pd.to_datetime(df['Timestamp'], utc=True)
    df.sort_values('Timestamp', inplace=True)
    df.drop_duplicates(subset='Timestamp', keep='last', inplace=True)
    df.reset_index(drop=True, inplace=True)
    return df

def instrument_symbol(stem: str) -> str:
    import re
    return re.sub(r'_(MINUTE|HOUR|DAY|WEEK).*$','',stem,flags=re.I)

def infer_bars_per_year(df: pd.DataFrame) -> int:
    if len(df)<2: return 252
    mins = df['Timestamp'].diff().dt.total_seconds().median()/60
    if mins >= 60*20: return 252
    return int((60*24/mins)*252)


# ---------------------------------------------------------------------------
# Return-Stream
# ---------------------------------------------------------------------------
def strat_returns(position: np.ndarray, df: pd.DataFrame,
                  cost_bps: float, execution: str = 'next_open'):
    pos = np.where(np.isnan(position.astype(float)), 0, position).astype(float)
    if execution == 'next_open':
        rets    = np.concatenate([[np.nan], np.diff(np.log(df['Open'].values))])
        pos_eff = np.concatenate([[0,0], pos[:-2]])   # shift 2
    else:
        rets    = np.concatenate([[np.nan], np.diff(np.log(df['Close'].values))])
        pos_eff = np.concatenate([[0], pos[:-1]])      # shift 1
    sr = pos_eff * rets
    cost = np.abs(np.concatenate([[np.nan], np.diff(pos)])) * (cost_bps/10000)
    cost = np.where(np.isnan(cost), 0, cost)
    return sr - cost, pos_eff


# ---------------------------------------------------------------------------
# Metriken
# ---------------------------------------------------------------------------
def compute_metrics(position, df, bpy, cost_bps, execution='next_open'):
    empty = dict(sharpe=0,sortino=0,calmar=0,max_dd=0,num_trades=0,
                 win_rate=0,profit_factor=0,total_return=0,cagr=0,exposure=0)
    sr, pos_eff = strat_returns(position, df, cost_bps, execution)
    ok = ~np.isnan(sr); sr=sr[ok]; pe=pos_eff[ok]
    if len(sr)==0: return empty
    mu=sr.mean(); sd=sr.std(ddof=1)
    sharpe  = (mu/sd)*np.sqrt(bpy) if sd>0 else 0
    dn = sr[sr<0]; ddev=np.sqrt(np.mean(dn**2)) if len(dn)>0 else 0
    sortino = (mu/ddev)*np.sqrt(bpy) if ddev>0 else 0
    cum=np.exp(np.cumsum(sr)); cmax=np.maximum.accumulate(cum)
    max_dd = float(np.min((cum-cmax)/cmax))
    total_ret = float(np.exp(sr.sum())-1)
    years = len(sr)/bpy
    cagr = (1+total_ret)**(1/years)-1 if years>0 else 0
    calmar = cagr/abs(max_dd) if max_dd<0 else (float('inf') if cagr>0 else 0)
    ids = np.cumsum(np.concatenate([[1],np.diff(pe)!=0]))
    trade_r = pd.Series(sr).groupby(ids).sum().values
    trade_p = pd.Series(pe).groupby(ids).first().values
    active  = trade_r[trade_p!=0]
    nt=len(active)
    if nt>0:
        wr = (active>0).mean()
        gp=active[active>0].sum(); gl=abs(active[active<0].sum())
        pf = gp/gl if gl>0 else float('inf')
    else: wr=0; pf=0
    return dict(sharpe=sharpe,sortino=sortino,calmar=calmar,max_dd=max_dd,
                num_trades=nt,win_rate=float(wr),profit_factor=float(pf),
                total_return=total_ret,cagr=float(cagr),
                exposure=float((pe!=0).mean()))

def compute_yearly(position, df, cost_bps, execution):
    sr, _ = strat_returns(position, df, cost_bps, execution)
    years = df['Timestamp'].dt.year.values
    ok=~np.isnan(sr)
    df2=pd.DataFrame({'year':years[ok],'r':sr[ok]})
    g=df2.groupby('year').agg(ret=('r',lambda x:np.exp(x.sum())-1),bars=('r','count'))
    return g.reset_index()


# ---------------------------------------------------------------------------
# Filter
# ---------------------------------------------------------------------------
FILTER_ID2COL = {'F1':'F1_OS_MaxDD_above_neg15','F2':'F2_OS_PF_gt_1.2',
                  'F3':'F3_OS_Sortino_gt_1.0','F4':'F4_OS_Calmar_gt_0.5',
                  'F5':'F5_IS_PF_gt_1.0','F6':'F6_OS_WinRate_band','F7':'F7_AllYearsProfitable'}

def apply_filters(is_m, os_m, yearly_os, active_filters, min_year_bars):
    def pf_ok(m,thr): return (np.isfinite(m['profit_factor']) and m['profit_factor']>thr) or np.isinf(m['profit_factor'])
    # FIX: yearly nur auf OS-Daten (yearly_os statt yearly_full).
    # Verhindert, dass gute IS-Jahre schlechte OS-Jahre in F7 überdecken.
    counted = yearly_os[yearly_os['bars']>=min_year_bars]
    years_ok = len(counted)>0 and bool((counted['ret']>0).all())
    f = {
        'F1_OS_MaxDD_above_neg15':  os_m['max_dd']>-0.15,
        'F2_OS_PF_gt_1.2':          pf_ok(os_m,1.2),
        'F3_OS_Sortino_gt_1.0':     os_m['sortino']>1.0,
        'F4_OS_Calmar_gt_0.5':      (np.isfinite(os_m['calmar']) and os_m['calmar']>0.5) or np.isinf(os_m['calmar']),
        'F5_IS_PF_gt_1.0':          pf_ok(is_m,1.0),
        # FIX: Untergrenze 0.30 → 0.45 (Design-Spec war 45–85%)
        'F6_OS_WinRate_band':       0.45<os_m['win_rate']<0.85,
        'F7_AllYearsProfitable':    years_ok,
    }
    active_cols = [FILTER_ID2COL[x] for x in active_filters if x in FILTER_ID2COL]
    f['Active_Filters'] = ','.join(active_filters) if active_filters else 'NONE'
    f['PASSED_ALL'] = all(f[c] for c in active_cols) if active_cols else True
    return f


# ---------------------------------------------------------------------------
# Expand param grid
# ---------------------------------------------------------------------------
def expand_grid(grid: dict):
    keys = list(grid.keys())
    vals = list(grid.values())
    return [dict(zip(keys,v)) for v in itertools.product(*vals)]


# ---------------------------------------------------------------------------
# Worker-Funktion (eine Task = eine Strategie × ein Instrument × ALLE Params)
# ---------------------------------------------------------------------------
def run_task(task):
    # Import hier, damit Multiprocessing-Worker ihren eigenen Namespace haben
    sys.path.insert(0, str(Path(__file__).parent))
    from strategies import STRATEGIES
    strat_name, csv_path, params_list, cost_bps, execution, active_filters, min_year_bars = task

    df = load_csv(csv_path)
    bpy = infer_bars_per_year(df)
    n = len(df); split = int(n*IS_SPLIT)
    stem = Path(csv_path).stem

    fn, _ = STRATEGIES[strat_name]
    rows = []
    for params in params_list:
        try:
            pos_full = fn(df, **params)
            pos_is   = pos_full[:split]
            pos_os   = pos_full[split:]
            df_is=df.iloc[:split]; df_os=df.iloc[split:]

            is_m = compute_metrics(pos_is, df_is, bpy, cost_bps, execution)
            os_m = compute_metrics(pos_os, df_os, bpy, cost_bps, execution)
            # FIX: F7 yearly NUR auf OS-Daten, nicht auf IS+OS zusammen.
            # IS-Jahre könnten schlechte OS-Jahre in F7 verstecken.
            yearly_os = compute_yearly(pos_os, df_os, cost_bps, execution)
            yearly_full = compute_yearly(pos_full, df, cost_bps, execution)
            filters = apply_filters(is_m, os_m, yearly_os, active_filters, min_year_bars)

            yearly_str = ' '.join(f"{int(r.year)}:{r.ret*100:+.1f}%"
                                  for _,r in yearly_full.iterrows())
            worst = float(yearly_full['ret'].min()) if len(yearly_full)>0 else float('nan')

            row = {'Strategy':strat_name,'Instrument':stem,
                   'Symbol':instrument_symbol(stem),'Params':
                   ', '.join(f"{k}={v}" for k,v in params.items())}
            row.update({f'Param_{k}':v for k,v in params.items()})
            row.update({'Cost_bps_side':cost_bps,'Execution':execution,
                        'Bars_per_Year':bpy,'Bars_IS':split,'Bars_OS':n-split,
                        'IS_Sharpe':round(is_m['sharpe'],3),
                        'IS_Sortino':round(is_m['sortino'],3),
                        'IS_ProfitFactor':round(is_m['profit_factor'],3) if np.isfinite(is_m['profit_factor']) else None,
                        'IS_Trades':is_m['num_trades'],
                        'OS_Sharpe':round(os_m['sharpe'],3),
                        'OS_Sortino':round(os_m['sortino'],3),
                        'OS_Calmar':round(os_m['calmar'],3) if np.isfinite(os_m['calmar']) else None,
                        'OS_CAGR':round(os_m['cagr'],4),
                        'OS_MaxDD':round(os_m['max_dd'],4),
                        'OS_TotalReturn':round(os_m['total_return'],4),
                        'OS_WinRate':round(os_m['win_rate'],4),
                        'OS_ProfitFactor':round(os_m['profit_factor'],3) if np.isfinite(os_m['profit_factor']) else None,
                        'OS_Trades':os_m['num_trades'],
                        'OS_Exposure':round(os_m['exposure'],3),
                        'Worst_Year_Ret':round(worst,4),
                        'Yearly_Breakdown':yearly_str,
                        'Error':None})
            row.update(filters)
            rows.append(row)
        except Exception as e:
            rows.append({'Strategy':strat_name,'Instrument':stem,
                         'Params':str(params),'PASSED_ALL':False,
                         'Error':f"{type(e).__name__}: {e}"})
    return rows


# ---------------------------------------------------------------------------
# Laufzeit-Schätzung (vor dem Run) & Fortschritts-Tracking (während des Runs)
# ---------------------------------------------------------------------------
def estimate_total_runtime(tasks, strat_row_counts, n_workers,
                           max_calib_strategies=8, parallel_efficiency=0.85,
                           verbose=True):
    """Kalibriert anhand je eines echten Tasks pro Strategie (nicht per Dummy-
    Timing), da sich die Kosten pro Parameter-Kombination stark zwischen den
    Strategien unterscheiden (z.B. GapFade-Schleifen vs. einfache SMA-Kreuzung).
    Nebeneffekt: waermt den Numba-cache=True Disk-Cache vor, wovon die
    Worker-Prozesse im echten Run profitieren (kein erneutes JIT-Compile)."""
    strat_names = list(strat_row_counts.keys())
    if not strat_names or not tasks:
        return None
    if len(strat_names) <= max_calib_strategies:
        sample_names = strat_names
    else:
        pos = np.linspace(0, len(strat_names)-1, max_calib_strategies).round().astype(int)
        sample_names = [strat_names[i] for i in sorted(set(pos))]

    sec_per_row = {}
    calib_t0 = time.perf_counter()
    for sname in sample_names:
        task = next((t for t in tasks if t[0] == sname), None)
        if task is None: continue
        t0 = time.perf_counter()
        rows = run_task(task)
        dt = time.perf_counter() - t0
        if len(rows) > 0:
            sec_per_row[sname] = dt / len(rows)
    calib_elapsed = time.perf_counter() - calib_t0
    if not sec_per_row:
        return None

    avg_sec_per_row = float(np.mean(list(sec_per_row.values())))
    # Strategien ohne eigene Messung -> Durchschnitt der gemessenen Strategien
    total_seq_sec = sum(strat_row_counts[s] * sec_per_row.get(s, avg_sec_per_row)
                        for s in strat_names)
    est = total_seq_sec / (max(1, n_workers) * parallel_efficiency)

    if verbose:
        detail = ', '.join(f"{s}={sec_per_row[s]*1000:.1f}ms/Run" for s in sample_names)
        print(f"Kalibrierung ({len(sample_names)}/{len(strat_names)} Strategien, "
              f"{calib_elapsed:.1f}s): {detail}")
        print(f"Geschaetzte Gesamtlaufzeit: {est:.0f}s (~{est/60:.1f} min) "
              f"bei {n_workers} Workern (Effizienzfaktor {parallel_efficiency})")
    return est


def _filter_pass_rate_table(results: pd.DataFrame, group_cols, active_filters) -> pd.DataFrame:
    fids = [f for f in active_filters if f in FILTER_ID2COL]
    agg_kwargs = {'n': ('PASSED_ALL', 'size')}
    for fid in fids:
        agg_kwargs[f'{fid}_pct'] = (FILTER_ID2COL[fid], 'mean')
    agg_kwargs['ALL_pct'] = ('PASSED_ALL', 'mean')
    table = results.groupby(group_cols).agg(**agg_kwargs).reset_index()
    pct_cols = [c for c in table.columns if c.endswith('_pct')]
    table[pct_cols] = (table[pct_cols] * 100).round(1)
    return table.sort_values(group_cols).reset_index(drop=True)

def print_filter_pass_rates(results: pd.DataFrame, active_filters):
    """Baut und druckt Filter-Passquoten (%) je (Strategie,Instrument), je
    Strategie und je Instrument. Gibt die drei Tabellen als DataFrames zurueck."""
    if results.empty or 'PASSED_ALL' not in results.columns:
        return None
    by_both       = _filter_pass_rate_table(results, ['Strategy', 'Instrument'], active_filters)
    by_strategy   = _filter_pass_rate_table(results, ['Strategy'], active_filters)
    by_instrument = _filter_pass_rate_table(results, ['Instrument'], active_filters)

    print("\nFilter-Passquoten (%) je Instrument x Strategie:")
    print(by_both.to_string(index=False))
    print("\nFilter-Passquoten (%) je Strategie:")
    print(by_strategy.to_string(index=False))
    print("\nFilter-Passquoten (%) je Instrument:")
    print(by_instrument.to_string(index=False))

    return {'by_instrument_strategy': by_both, 'by_strategy': by_strategy, 'by_instrument': by_instrument}


class _ProgressTracker:
    """Schätzt die Rate in Runs/Sekunde per EWMA (statt eines kumulativen
    Durchschnitts seit Start). Dadurch reagiert die ETA auf Geschwindigkeits-
    änderungen während des Runs (z.B. Numba-JIT-Warmup der ersten Tasks, oder
    spätere Strategien/Instrumente mit anderen Kosten pro Run) und verzerrt
    sich nicht durch diese anfängliche Verlangsamung."""
    def __init__(self, total_rows, alpha=0.3):
        self.total_rows = total_rows
        self.alpha = alpha
        self.rows_done = 0
        self.ewma_rate = None
        self.t0 = time.perf_counter()
        self.last_t = self.t0

    def update(self, n_rows):
        now = time.perf_counter()
        dt = now - self.last_t
        self.last_t = now
        self.rows_done += n_rows
        if dt > 0:
            inst_rate = n_rows / dt
            self.ewma_rate = inst_rate if self.ewma_rate is None \
                else self.alpha*inst_rate + (1-self.alpha)*self.ewma_rate
        return now

    def render(self):
        el = self.last_t - self.t0
        rate = self.ewma_rate if self.ewma_rate else (self.rows_done/el if el > 0 else 0)
        remaining = max(0, self.total_rows - self.rows_done)
        eta = remaining/rate if rate > 0 else float('nan')
        pct = self.rows_done/self.total_rows*100 if self.total_rows else 100.0
        return f"  {pct:5.1f}%  elapsed={el:.0f}s  eta={eta:.0f}s  rate={rate:.1f} runs/s"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def run_backtest(strategies=None, instruments=None, data_dir='/mnt/project',
                 active_filters=None, execution='next_open',
                 workers=None, chunk_size=128, verbose=True):
    if active_filters is None: active_filters = ACTIVE_FILTERS
    sys.path.insert(0, str(Path(__file__).parent))
    from strategies import STRATEGIES

    strat_names = list(STRATEGIES.keys()) if strategies is None else strategies
    if instruments is None:
        csvs = list(Path(data_dir).glob('*.csv'))
    else:
        csvs = [Path(data_dir)/i for i in instruments]
    csvs = [str(p) for p in csvs if Path(p).exists()]

    # Kombinations-Zahl melden (pro Strategie, für Kalibrierung/Gewichtung)
    strat_row_counts = {s: len(expand_grid(STRATEGIES[s][1]))*len(csvs) for s in strat_names}
    total = sum(strat_row_counts.values())
    if verbose:
        print(f"Strategien: {len(strat_names)}  |  Instrumente: {len(csvs)}  |  Runs: {total:,}")
        print(f"Aktive Filter: {active_filters}  |  Execution: {execution}")

    # Tasks: (strat, csv, params_block, cost, exec, filters, min_year_bars)
    tasks = []
    for sname in strat_names:
        _, grid = STRATEGIES[sname]
        all_params = expand_grid(grid)
        blocks = [all_params[i:i+chunk_size] for i in range(0,len(all_params),chunk_size)]
        for csv_path in csvs:
            sym = instrument_symbol(Path(csv_path).stem)
            cost = SPREAD_BP.get(sym, SPREAD_DEFAULT)
            for blk in blocks:
                tasks.append((sname, csv_path, blk, cost, execution,
                               active_filters, MIN_YEAR_BARS))

    n_workers = workers if workers else max(1, cpu_count()-1)
    if verbose: print(f"Tasks: {len(tasks)}  |  Workers: {n_workers}")

    # Laufzeit-Schätzung VOR dem eigentlichen Run (zählt nicht in die spätere Laufzeit)
    if verbose:
        estimate_total_runtime(tasks, strat_row_counts, n_workers)

    report_every = max(1, len(tasks)//20)
    progress = _ProgressTracker(total)
    if n_workers == 1:
        all_rows = []
        for i,task in enumerate(tasks):
            rows = run_task(task)
            all_rows.extend(rows)
            progress.update(len(rows))
            if verbose and (i+1)%report_every==0:
                print(progress.render(), end='\r')
    else:
        from multiprocessing import Pool
        all_rows = []
        with Pool(n_workers) as pool:
            done=0
            for rows in pool.imap_unordered(run_task, tasks):
                all_rows.extend(rows); done+=1
                progress.update(len(rows))
                if verbose and done%report_every==0:
                    print(progress.render(), end='\r')

    elapsed=progress.last_t-progress.t0
    results=pd.DataFrame(all_rows)
    passed=int(results['PASSED_ALL'].sum()) if 'PASSED_ALL' in results else 0
    if verbose:
        print(f"\nFertig: {len(results):,} Runs  |  Passed: {passed}  |  Laufzeit: {elapsed:.1f}s")
        print_filter_pass_rates(results, active_filters)
    return results


if __name__ == '__main__':
    data_dir = Path(__file__).parents[3] / "price_data" / "ctrader_data"

    # Alle Strategien — Zeile auskommentieren (#) um sie vom Test auszuschliessen
    strategies = [
        "RSI_MeanReversion",
        "Bollinger_ZScore_MR",
        #"ATRDistance_MeanReversion",
        #"CCI_MeanReversion",
        #"CMO_VHF_STC",
        "Donchian_Breakout",
        "Ichimoku_7_20_60",
        #"VWAP_MeanReversion",
        #"SMA_Crossover",
        #"Supertrend_ATR",
        "Keltner_Squeeze_Breakout",
        #"MACD_Trend_Momentum",
        "Session_ORB",
        "Trend_RSI_Pullback",
        #"GapFade",
        "Tokyo_Gap_Range",
        #"SilverAsia_BreakoutV4",
    ]

    # Alle Instrumente/Timeframes — Zeile auskommentieren (#) um sie vom Test auszuschliessen
    # (MINUTE_15 ist standardmaessig auskommentiert, da urspruenglich nur MINUTE_5 lief)
    instruments = [
        "AUDUSD_MINUTE_5.csv",
        "AUDUSD_MINUTE_15.csv",
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
        "GOLD_MINUTE_5.csv",
        "GOLD_MINUTE_15.csv",
        "OIL_BRENT_MINUTE_5.csv",
        "OIL_BRENT_MINUTE_15.csv",
        #"SILVER_MINUTE_5.csv",
        #"SILVER_MINUTE_15.csv",
        #"US500_MINUTE_5.csv",
        #"US500_MINUTE_15.csv",
        "USDCAD_MINUTE_5.csv",
        "USDCAD_MINUTE_15.csv",
        "USDJPY_MINUTE_5.csv",
        "USDJPY_MINUTE_15.csv",
    ]

    results = run_backtest(
        strategies=strategies,
        instruments=instruments,
        data_dir=str(data_dir),
        active_filters=['F1','F2', 'F4', 'F5','F6'],
        workers=12
    )
    out = Path(__file__).parent.parent / "results" / f"runner_results_py_{pd.Timestamp.now().strftime('%Y%m%d_%H%M')}.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    results.to_csv(out, index=False)
    print(f"Gespeichert: {out}")