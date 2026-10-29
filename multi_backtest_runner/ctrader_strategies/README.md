# cTrader cBots — five strategies selected from the 2026-08-05 run

Source of truth: `../pyrunner/results/runner_results_py_20260805_1027.csv`
(18,592 runs · 8 strategies × 14 instrument/timeframe combinations)

Each `.cs` file is a **self-contained cBot** — no shared project files, paste-and-build
in cTrader Automate.

---

## 1. What was selected

| # | Instrument | Timeframe | Strategy | File | Confidence |
|---|---|---|---|---|---|
| 1 | Gold (XAUUSD) | M15 | Tokyo Gap Range | `Gold_TokyoGapRange_m15.cs` | **High** |
| 2 | Copper | M15 | Ichimoku Triple | `Copper_IchimokuTriple_m15.cs` | **High** |
| 3 | Brent Crude | M15 | Ichimoku Triple | `OilBrent_IchimokuTriple_m15.cs` | **Medium-high** |
| 4 | AUDUSD | M15 | RSI Mean Reversion | `AUDUSD_RsiMeanReversion_m15.cs` | **Low — paper only** |
| 5 | USDJPY | M15 | Trend RSI Pullback | `USDJPY_TrendRsiPullback_m15.cs` | **Very low — paper only** |

### Measured performance of the exact shipped parameters

Walk-forward split is 70/30 by bar count. Data spans 2023-01 → 2026-06, so
**out-of-sample is a single ~12.5-month window (2025-06 → 2026-06)**.
Costs: Pepperstone spread in bps per side, applied on every position change.

| Instrument | IS Sharpe | IS PF | OS Sharpe | OS PF | OS MaxDD | OS trades | OS win rate | Yearly (full sample) |
|---|---|---|---|---|---|---|---|---|
| Gold | 0.25 | 1.07 | **2.06** | 1.40 | −5.8% | 247 | 44.5% | −0.2 / −0.3 / +7.0 / +13.0 % |
| Copper | 0.48 | 1.30 | 0.50 | 1.22 | −4.3% | 101 | 47.5% | +0.1 / −0.0 / +3.3 / +2.6 % |
| Brent | 0.96 | 1.52 | 0.62 | **1.62** | −7.2% | 85 | 38.8% | +0.7 / +1.2 / +2.1 / +14.0 % |
| AUDUSD | 0.01 | 1.15 | 1.45 | 2.65 | −0.8% | **33** | 69.7% | −0.5 / −0.0 / +1.1 / +1.7 % |
| USDJPY | −1.33 | **0.90** | 0.45 | 1.50 | −1.0% | 94 | 41.5% | −1.6 / −2.9 / +0.0 / +0.2 % |

Yearly columns are 2023 / 2024 / 2025 / 2026 and cover the **full** sample
(the first two are in-sample). Returns are on the runner's notional-equity model.

---

## 2. How they were picked

The runner's own `PASSED_ALL` flag was **not** used as the selection criterion.
It flags 168 rows, but many are artefacts — e.g. the four "passing" USDJPY
Keltner Squeeze configs each have exactly **2 out-of-sample trades**.

Selection instead ranked by *parameter-neighbourhood stability*: for each
(instrument, strategy) family with ≥30 OS trades, what fraction of the entire
parameter grid is profitable out-of-sample. A family where 64/64 configurations
work is evidence of an edge; a single config with a high Sharpe surrounded by
losers is evidence of overfitting.

**Metals / energy — the families that held up:**

| Instrument | Strategy | Configs | Median OS Sharpe | Median OS PF | % configs OS PF > 1 |
|---|---|---|---|---|---|
| Gold M15 | Tokyo Gap Range | 64 | +1.25 | 1.25 | **100%** |
| Gold M5 | Tokyo Gap Range | 64 | +1.01 | 1.19 | 100% |
| Copper M15 | Tokyo Gap Range | 64 | +0.73 | 1.20 | 100% |
| Copper M15 | Ichimoku | 64 | +0.56 | 1.28 | **100%** |
| Brent M15 | Ichimoku | 64 | +0.17 | 1.21 | 84% |

Gold Tokyo Gap Range is the standout: highest median Sharpe of any family in
the run and a fully profitable neighbourhood.

**Copper is Ichimoku, not Tokyo Gap Range,** even though Tokyo has the higher
median OS Sharpe. Tokyo on Copper loses roughly −11% in 2023 and −10% in 2024
and only recovers inside the OS window; Ichimoku is positive or flat in every
calendar year and has a profit factor above 1 in *both* halves of the split.
Consistency beat the higher single-window number.

**Brent** picks the `tenkan=9 / atr_period=10 / sl=1.5 / tp=2.0` config rather
than the highest-OS-Sharpe one, because it is the only Brent configuration with
PF > 1 in both halves (IS 1.52, OS 1.62) and every calendar year positive.

### The two currencies: an honest warning

**No FX pair in this run produced a defensible edge.** Applying a sample-size
floor of 80 out-of-sample trades together with OS PF > 1.1 and OS Sharpe > 0.5
leaves **zero** rows across EURUSD, USDCAD, USDJPY and AUDUSD. Every FX family
with a usable trade count has a negative median out-of-sample Sharpe:

| Instrument | Best family | Median OS Sharpe | % configs OS PF > 1 |
|---|---|---|---|
| AUDUSD M15 | RSI Mean Reversion | −1.08 | 61% |
| USDJPY M15 | Session ORB | −1.77 | 38% |
| USDCAD M15 | Trend RSI Pullback | −2.37 | 9% |
| EURUSD M5 | Session ORB | −5.12 | 0% |

The two FX bots ship because the task asked for two currencies, and they are the
best available candidates — not because the evidence supports them:

- **AUDUSD** has the most survivable FX family (61% of the RSI grid is OS-profitable)
  but the shipped config has only **33 OS trades**. That is far too few to
  distinguish skill from luck.
- **USDJPY** is worse still: the shipped config has an **in-sample profit factor
  of 0.90** — it lost money over the first 2.5 years and only turned positive in
  the 12-month OS window. Its parameter neighbourhood has a median OS Sharpe of −1.81.

Run both on demo only. If you want tradeable FX, the run needs more pairs,
longer history, or different strategy families — not a different config from
this grid.

### Additional caveat on all five

The OS window is a single 12.5-month period, and 2026 is by far the strongest
year in it for Gold, Brent and Copper. Some of the OS edge is a regime effect
(strong metals/energy trends in 2026), not a timeless one. Treat the OS numbers
as one observation, not a distribution.

---

## 3. Port fidelity

Every bot is a line-by-line port of `../pyrunner/pyrunner/strategies.py`.

**Indicators are re-implemented inside each bot, not taken from cAlgo's built-ins.**
cAlgo's `AverageTrueRange`, `RelativeStrengthIndex` etc. use different seeding
and warm-up conventions than `indicators.py`, which would silently shift signals.
The local implementations reproduce pandas' `ewm(adjust=False)` recursion and the
exact NaN warm-up masks:

| Python | Local C# equivalent |
|---|---|
| `atr()` — TR then `ewm(span=p)` | `alpha = 2/(p+1)`, NaN for `i < p` |
| `ema()` — `ewm(span=p)` | `alpha = 2/(p+1)`, NaN for `i < p−1` |
| `rsi_wilder()` — `ewm(alpha=1/p)` | `alpha = 1/p`, NaN for `i < p` |
| `ichimoku()` — rolling Donchian mids | `DonchMid()`, NaN for `i < n−1` |

**Execution timing.** The backtest runs `execution='next_open'` with a 2-bar
position shift: a signal formed at the close of bar *t* earns the return from
open(*t+1*) to open(*t+2*). The bots reproduce this exactly — `OnBar()` fires when
bar *t+1* opens, the FSM is stepped over the just-closed bar *t*, and the market
order is sent immediately. No look-ahead.

**Triple-barrier FSM.** `fsm_barrier()` is ported verbatim, including:
- SL/TP levels anchored to the **close of the signal bar**, not the fill price
- intrabar touch detection (`low <= sl`, `high >= tp`) evaluated at bar close
- the `locked` mechanism that blocks re-entry in the same direction after a
  barrier exit until the raw signal changes direction
- flat-at-UTC-midnight (`new_day_mask`)

**Ichimoku chikou is inverted vs. textbook.** `indicators.py` computes
`chikou[i] = close[i−26]` and `_ichi_sig` requires `chikou[i] > close[i]` for a
long — i.e. price *below* where it was 26 bars ago. This is the opposite of the
standard lagging-span rule. It is reproduced verbatim in both Ichimoku bots,
because that is the rule the measured edge was produced by. Changing it
invalidates the numbers above.

### Port verified against the originals

`verify_ports.py` re-implements, in Python, the *exact* incremental bar-by-bar
logic of the C# bots (`StepBar` / `StepBarrier` / the signal state machines) and
diffs the resulting position array against the original vectorised
`strategies.py` functions on the same CSVs the run used.

```
python verify_ports.py
```

Current result — all five are bit-identical, zero differing bars:

```
[OK ] GOLD Tokyo_Gap_Range M15           bars= 81866  differing=0
[OK ] COPPER Ichimoku M15                bars= 81707  differing=0
[OK ] BRENT Ichimoku M15                 bars= 77361  differing=0
[OK ] AUDUSD RSI_MeanReversion M15       bars= 86236  differing=0
[OK ] USDJPY Trend_RSI_Pullback M15      bars= 86219  differing=0
ALL PORTS EQUIVALENT
```

This validates the *logic*, not the C# syntax — no .NET SDK is installed here,
so the `.cs` files have not been compiled. Build them in cTrader before use.
Re-run this script after changing any signal or barrier code in the bots.

### Deliberate deviations

| Area | Backtest | Bots | Why |
|---|---|---|---|
| Position size | notional = 100% of equity, always | `RiskPercent` (default 0.5% per trade via SL distance) or `FixedLots` | The backtest's implicit 1× leverage is not a risk model. Sizing changes the equity-curve scale, not the trade sequence. |
| Barrier exit fill | next bar's open after the touch | same (default) | Faithful. Set **Attach broker SL/TP = true** to place real broker stops at the FSM levels instead — safer against gaps/outages, but exits at the stop price and will diverge from the backtest. |
| Spread | fixed bps/side | live spread | No max-spread filter is applied, matching the backtest. Consider adding one before live use. |

---

## 4. Installation

1. cTrader Desktop → **Automate** → **New cBot**.
2. Replace the generated file's contents with one `.cs` file from this folder.
3. Build (F6), then attach to a chart.

**Requirements**

- **Chart timeframe must be M15.** Each bot refuses to start otherwise
  (`Enforce M15 timeframe`, on by default).
- **Chart symbol must match the bot** — the bots do not check this. Attaching the
  Gold bot to a Copper chart will run happily and produce nonsense.
- **Bot timezone is pinned to UTC** via `[Robot(TimeZone = TimeZones.UTC)]`.
  The Gold session windows (00:00–09:00 UTC, last entry 07:00) and the daily
  flat-out are all UTC, matching the CSV data. Do not change this.
- **Load enough history.** Each bot replays all available closed bars in
  `OnStart()` to rebuild FSM state. The Ichimoku bots need at least ~90 bars of
  warm-up, the others ~100 (EMA-100). More is better; a few thousand is ideal.

Create each bot as its own cBot project — all five declare
`enum PositionSizingMode` in `cAlgo.Robots`, which collides if compiled together.

### Suggested first steps

1. Backtest each bot in cTrader over 2025-06 → 2026-06 and compare trade count
   and profit factor against the table in section 1. They will not match to the
   decimal (tick-level fills, real spread, real swaps), but trade counts should
   be close. A large divergence means something is wrong with the port or the
   data alignment — investigate before going further.
2. Run Gold / Copper / Brent on demo for at least a quarter.
3. Leave AUDUSD and USDJPY on demo indefinitely unless a fresh run with more
   data supports them.
