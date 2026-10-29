// ============================================================================
//  GOLD  -  Tokyo Gap Range  (M15)
// ----------------------------------------------------------------------------
//  1:1 port of  pyrunner/strategies.py :: tokyo_gap_range()  +  fsm_barrier()
//  Indicators re-implemented locally (NOT cAlgo built-ins) so that warm-up,
//  seeding and smoothing match indicators.py exactly.
//
//  Selected from results/runner_results_py_20260805_1027.csv
//    or_candles=6, gap_atr_mult=0.5, atr_n=14, session 00:00-09:00 UTC,
//    last_entry_h=7, adx_filter=False, atr_period=14, sl=1.5xATR, tp=3.0xATR
//    IS  (2023-01 .. 2025-06): Sharpe  0.25 | PF 1.07
//    OS  (2025-06 .. 2026-06): Sharpe  2.06 | PF 1.40 | MaxDD -5.8% | 247 trades
//    Whole 64-config parameter neighbourhood had OS PF > 1  (most robust pick).
// ============================================================================
using System;
using cAlgo.API;
using cAlgo.API.Internals;

namespace cAlgo.Robots
{
    public enum PositionSizingMode
    {
        RiskPercent,
        FixedLots
    }

    [Robot(AccessRights = AccessRights.None, TimeZone = TimeZones.UTC)]
    public class GoldTokyoGapRangeM15 : Robot
    {
        // ---------------- Signal parameters (from the backtest) --------------
        [Parameter("OR candles", Group = "Signal", DefaultValue = 6, MinValue = 1)]
        public int OrCandles { get; set; }

        [Parameter("Gap ATR mult", Group = "Signal", DefaultValue = 0.5)]
        public double GapAtrMult { get; set; }

        [Parameter("Gap ATR period (atr_n)", Group = "Signal", DefaultValue = 14, MinValue = 1)]
        public int AtrN { get; set; }

        [Parameter("Session start hour (UTC)", Group = "Signal", DefaultValue = 0, MinValue = 0, MaxValue = 23)]
        public int SessionStartH { get; set; }

        [Parameter("Session end hour (UTC)", Group = "Signal", DefaultValue = 9, MinValue = 1, MaxValue = 24)]
        public int SessionEndH { get; set; }

        [Parameter("Last entry hour (UTC)", Group = "Signal", DefaultValue = 7, MinValue = 1, MaxValue = 24)]
        public int LastEntryH { get; set; }

        // ---------------- Barrier parameters --------------------------------
        [Parameter("Barrier ATR period", Group = "Barriers", DefaultValue = 14, MinValue = 1)]
        public int AtrPeriod { get; set; }

        [Parameter("SL ATR mult", Group = "Barriers", DefaultValue = 1.5)]
        public double SlAtrMult { get; set; }

        [Parameter("TP ATR mult", Group = "Barriers", DefaultValue = 3.0)]
        public double TpAtrMult { get; set; }

        [Parameter("Attach broker SL/TP", Group = "Barriers", DefaultValue = false)]
        public bool UseBrokerStops { get; set; }

        // ---------------- Execution / risk ----------------------------------
        [Parameter("Sizing mode", Group = "Risk", DefaultValue = PositionSizingMode.RiskPercent)]
        public PositionSizingMode Sizing { get; set; }

        [Parameter("Risk % of equity", Group = "Risk", DefaultValue = 0.5, MinValue = 0.01)]
        public double RiskPercent { get; set; }

        [Parameter("Fixed lots", Group = "Risk", DefaultValue = 0.1, MinValue = 0.001)]
        public double FixedLots { get; set; }

        [Parameter("Enforce M15 timeframe", Group = "Risk", DefaultValue = true)]
        public bool EnforceTimeframe { get; set; }

        [Parameter("Verbose log", Group = "Risk", DefaultValue = false)]
        public bool Verbose { get; set; }

        private const string Label = "TGR_GOLD_M15";

        // ---------------- Streaming indicator state -------------------------
        private double _trEmaGap, _trEmaBar;
        private double _alphaGap, _alphaBar;

        // ---------------- Raw-signal (Tokyo) state --------------------------
        private int _rawState;
        private DateTime _curDate = DateTime.MinValue;
        private double _orH = double.NaN, _orL = double.NaN;
        private int _orCount;
        private bool _orDone;

        // ---------------- Barrier FSM state ---------------------------------
        private int _state, _locked;
        private double _sl = double.NaN, _tp = double.NaN, _entryAtr = double.NaN;

        private int _processed;
        private bool _warm;

        protected override void OnStart()
        {
            if (EnforceTimeframe && TimeFrame != TimeFrame.Minute15)
            {
                Print("ABORT: this cBot is calibrated for M15, chart is {0}. "
                    + "Switch the chart to M15 or disable 'Enforce M15 timeframe'.", TimeFrame);
                Stop();
                return;
            }

            _alphaGap = 2.0 / (AtrN + 1.0);
            _alphaBar = 2.0 / (AtrPeriod + 1.0);

            // Replay every already-closed bar so the FSM state is identical to
            // what the vectorised backtest would have produced at this point.
            for (int i = 0; i <= Bars.Count - 2; i++)
                StepBar(i);
            _processed = Math.Max(0, Bars.Count - 1);
            _warm = true;

            Print("{0} warmed up on {1} bars. Target position = {2}", Label, _processed, _state);
            Reconcile();
        }

        protected override void OnBar()
        {
            if (!_warm) return;
            int last = Bars.Count - 2;          // the bar that just closed
            while (_processed <= last)
            {
                StepBar(_processed);
                _processed++;
            }
            // Signal formed at the close of bar `last` -> fill at the open of
            // bar `last+1`, which is exactly now.
            Reconcile();
        }

        // ====================================================================
        //  One bar of the strategy: indicators -> raw signal -> barrier FSM
        // ====================================================================
        private void StepBar(int i)
        {
            double h = Bars.HighPrices[i];
            double l = Bars.LowPrices[i];
            double c = Bars.ClosePrices[i];
            DateTime t = Bars.OpenTimes[i];

            // ---- True Range -> two EMA-smoothed ATRs (indicators.py::atr) ---
            double prevC = i > 0 ? Bars.ClosePrices[i - 1] : c;
            double tr = Math.Max(h - l, Math.Max(Math.Abs(h - prevC), Math.Abs(l - prevC)));
            _trEmaGap = i == 0 ? tr : _alphaGap * tr + (1 - _alphaGap) * _trEmaGap;
            _trEmaBar = i == 0 ? tr : _alphaBar * tr + (1 - _alphaBar) * _trEmaBar;
            double atrGap = i < AtrN ? double.NaN : _trEmaGap;        // out[:period] = NaN
            double atrBar = i < AtrPeriod ? double.NaN : _trEmaBar;

            // ---- Raw signal: _tokyo_sig ------------------------------------
            int rawSig = TokyoSignal(i, h, l, c, t, atrGap);

            // ---- Barrier FSM: fsm_barrier ----------------------------------
            bool newDay = i > 0 && Bars.OpenTimes[i - 1].Date != t.Date;
            StepBarrier(rawSig, h, l, c, atrBar, newDay);
        }

        private int TokyoSignal(int i, double h, double l, double c, DateTime t, double atrGap)
        {
            int barMin = t.Hour * 60 + t.Minute;
            int sessStart = SessionStartH * 60;
            int sessEnd = SessionEndH * 60;
            int lastEntry = LastEntryH * 60;

            if (t.Date != _curDate)
            {
                _curDate = t.Date;
                _orH = double.NaN;
                _orL = double.NaN;
                _orCount = 0;
                _orDone = false;
                _rawState = 0;
            }

            if (barMin >= sessEnd)
            {
                _rawState = 0;
                return 0;
            }
            if (barMin < sessStart)
                return _rawState;

            if (!_orDone)
            {
                _orH = double.IsNaN(_orH) ? h : Math.Max(_orH, h);
                _orL = double.IsNaN(_orL) ? l : Math.Min(_orL, l);
                _orCount++;
                if (_orCount >= OrCandles) _orDone = true;
                return _rawState;
            }

            if (barMin >= lastEntry)
                return _rawState;

            if (_rawState == 0)
            {
                if (double.IsNaN(atrGap)) return _rawState;
                double gapThreshold = atrGap * GapAtrMult;
                bool gapOk = !double.IsNaN(_orH) && (_orH - _orL) > gapThreshold;
                if (gapOk)
                {
                    if (c > _orH) _rawState = 1;
                    else if (c < _orL) _rawState = -1;
                }
            }
            return _rawState;
        }

        // ====================================================================
        //  fsm_barrier - triple-barrier state machine (strategies.py:20-57)
        // ====================================================================
        private void StepBarrier(int rawSig, double h, double l, double c, double atr, bool newDay)
        {
            int s = double.IsNaN(atr) ? 0 : rawSig;   // _apply_barriers: sig[isnan(a)] = 0

            if (newDay && _state != 0)
            {
                _state = 0;
                _sl = double.NaN;
                _tp = double.NaN;
            }

            if (_state == 1)
            {
                if (!double.IsNaN(_sl) && l <= _sl) { _locked = 1; _state = 0; _sl = _tp = double.NaN; }
                else if (!double.IsNaN(_tp) && h >= _tp) { _locked = 1; _state = 0; _sl = _tp = double.NaN; }
            }
            else if (_state == -1)
            {
                if (!double.IsNaN(_sl) && h >= _sl) { _locked = -1; _state = 0; _sl = _tp = double.NaN; }
                else if (!double.IsNaN(_tp) && l <= _tp) { _locked = -1; _state = 0; _sl = _tp = double.NaN; }
            }

            if (_locked != 0 && s != _locked) _locked = 0;

            if (_state == 0)
            {
                if (s != 0 && s != _locked && !double.IsNaN(atr))
                    OpenState(s, c, atr);
            }
            else
            {
                if (s == 0) { _state = 0; _sl = _tp = double.NaN; }
                else if (s != _state) OpenState(s, c, atr);
            }
        }

        private void OpenState(int s, double c, double atr)
        {
            _state = s;
            _entryAtr = atr;
            if (s == 1) { _sl = c - SlAtrMult * atr; _tp = c + TpAtrMult * atr; }
            else { _sl = c + SlAtrMult * atr; _tp = c - TpAtrMult * atr; }
        }

        // ====================================================================
        //  Bring the live position in line with the FSM target
        // ====================================================================
        private void Reconcile()
        {
            var pos = Positions.Find(Label, SymbolName);

            if (pos != null)
            {
                int cur = pos.TradeType == TradeType.Buy ? 1 : -1;
                if (cur == _state) return;
                var cr = ClosePosition(pos);
                if (!cr.IsSuccessful)
                {
                    Print("{0}: close failed ({1}) - keeping position, will retry next bar.", Label, cr.Error);
                    return;
                }
                if (Verbose) Print("{0}: closed {1}", Label, cur > 0 ? "LONG" : "SHORT");
            }

            if (_state == 0) return;

            // Barrier distance of the bar that opened this state - identical to
            // the distance the backtest used for its SL level.
            double slDistance = SlAtrMult * _entryAtr;
            if (double.IsNaN(slDistance) || slDistance <= 0) return;

            double volume = ComputeVolumeUnits(slDistance);
            if (volume <= 0) return;

            var tt = _state == 1 ? TradeType.Buy : TradeType.Sell;
            var r = ExecuteMarketOrder(tt, SymbolName, volume, Label);
            if (!r.IsSuccessful)
            {
                Print("{0}: entry failed ({1})", Label, r.Error);
                return;
            }
            if (UseBrokerStops)
                ModifyPosition(r.Position, _sl, _tp);
            if (Verbose)
                Print("{0}: opened {1} vol={2} sl={3} tp={4}", Label, tt, volume, _sl, _tp);
        }

        private double ComputeVolumeUnits(double slDistancePrice)
        {
            double units;
            if (Sizing == PositionSizingMode.FixedLots)
            {
                units = Symbol.QuantityToVolumeInUnits(FixedLots);
            }
            else
            {
                double valuePerPricePerUnit = Symbol.PipValue / Symbol.PipSize;
                if (valuePerPricePerUnit <= 0) return 0;
                double riskAmount = Account.Equity * RiskPercent / 100.0;
                units = riskAmount / (slDistancePrice * valuePerPricePerUnit);
            }

            units = Symbol.NormalizeVolumeInUnits(units, RoundingMode.Down);
            if (units < Symbol.VolumeInUnitsMin)
            {
                Print("{0}: computed volume {1} below symbol minimum {2} - trade skipped. "
                    + "Increase 'Risk %' or account size.", Label, units, Symbol.VolumeInUnitsMin);
                return 0;
            }
            if (units > Symbol.VolumeInUnitsMax) units = Symbol.VolumeInUnitsMax;
            return units;
        }
    }
}
