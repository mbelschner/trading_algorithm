// ============================================================================
//  USDJPY  -  Trend RSI Pullback  (M15)
// ----------------------------------------------------------------------------
//  1:1 port of  pyrunner/strategies.py :: trend_rsi_pullback()  +  fsm_barrier()
//  and  pyrunner/indicators.py :: ema(), rsi_wilder(), atr().
//
//  Selected from results/runner_results_py_20260805_1027.csv
//    ema_len=100, rsi_len=14, rsi_buy=35, rsi_sell=65,
//    atr_period=10, sl=2.5xATR, tp=2.0xATR
//    IS  (2023-01 .. 2025-06): Sharpe -1.33 | PF 0.90
//    OS  (2025-06 .. 2026-06): Sharpe  0.45 | PF 1.50 | MaxDD -1.0% | 94 trades
//
//  *** LOWEST CONFIDENCE OF THE FIVE - PAPER TRADE ONLY ***
//  In-sample profit factor is BELOW 1: this configuration lost money over the
//  first 2.5 years and only turned positive in the 12-month out-of-sample
//  window. The parameter neighbourhood has a median OS Sharpe of -1.81. No FX
//  pair in this run produced a defensible edge; this is the least bad USD/JPY
//  candidate, included for completeness. See README.md.
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
    public class UsdJpyTrendRsiPullbackM15 : Robot
    {
        // ---------------- Signal parameters (from the backtest) --------------
        [Parameter("EMA length", Group = "Signal", DefaultValue = 100, MinValue = 2)]
        public int EmaLen { get; set; }

        [Parameter("RSI length", Group = "Signal", DefaultValue = 14, MinValue = 2)]
        public int RsiLen { get; set; }

        [Parameter("RSI buy trigger", Group = "Signal", DefaultValue = 35.0)]
        public double RsiBuy { get; set; }

        [Parameter("RSI sell trigger", Group = "Signal", DefaultValue = 65.0)]
        public double RsiSell { get; set; }

        // ---------------- Barrier parameters --------------------------------
        [Parameter("Barrier ATR period", Group = "Barriers", DefaultValue = 10, MinValue = 1)]
        public int AtrPeriod { get; set; }

        [Parameter("SL ATR mult", Group = "Barriers", DefaultValue = 2.5)]
        public double SlAtrMult { get; set; }

        [Parameter("TP ATR mult", Group = "Barriers", DefaultValue = 2.0)]
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

        private const string Label = "TRSI_USDJPY_M15";

        // ---------------- Streaming indicator state -------------------------
        private double _trEma, _alphaBar;
        private double _avgUp, _avgDn, _alphaRsi;
        private double _ema, _alphaEma;
        private double _rsiPrev = double.NaN;

        private int _rawState;
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

            _alphaBar = 2.0 / (AtrPeriod + 1.0);
            _alphaEma = 2.0 / (EmaLen + 1.0);
            _alphaRsi = 1.0 / RsiLen;             // Wilder smoothing

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
            int last = Bars.Count - 2;
            while (_processed <= last)
            {
                StepBar(_processed);
                _processed++;
            }
            Reconcile();
        }

        // ====================================================================
        private void StepBar(int i)
        {
            double h = Bars.HighPrices[i];
            double l = Bars.LowPrices[i];
            double c = Bars.ClosePrices[i];

            // ---- ATR (indicators.py::atr, EMA-smoothed TR) ------------------
            double prevC = i > 0 ? Bars.ClosePrices[i - 1] : c;
            double tr = Math.Max(h - l, Math.Max(Math.Abs(h - prevC), Math.Abs(l - prevC)));
            _trEma = i == 0 ? tr : _alphaBar * tr + (1 - _alphaBar) * _trEma;
            double atr = i < AtrPeriod ? double.NaN : _trEma;

            // ---- EMA (indicators.py::ema, out[:period-1] = NaN) -------------
            _ema = i == 0 ? c : _alphaEma * c + (1 - _alphaEma) * _ema;
            double emaV = i < EmaLen - 1 ? double.NaN : _ema;

            // ---- Wilder RSI (indicators.py::rsi_wilder) ---------------------
            double d = i > 0 ? c - Bars.ClosePrices[i - 1] : 0.0;
            double up = d > 0 ? d : 0.0;
            double dn = d < 0 ? -d : 0.0;
            _avgUp = i == 0 ? up : _alphaRsi * up + (1 - _alphaRsi) * _avgUp;
            _avgDn = i == 0 ? dn : _alphaRsi * dn + (1 - _alphaRsi) * _avgDn;
            double rs = _avgUp / (_avgDn == 0 ? 1e-10 : _avgDn);
            double rsi = i < RsiLen ? double.NaN : 100.0 - 100.0 / (1.0 + rs);

            // ---- Raw signal: _trsi_sig --------------------------------------
            int rawSig = TrendRsiSignal(i, c, emaV, rsi, _rsiPrev);
            _rsiPrev = rsi;

            bool newDay = i > 0 && Bars.OpenTimes[i - 1].Date != Bars.OpenTimes[i].Date;
            StepBarrier(rawSig, h, l, c, atr, newDay);
        }

        // ---- strategies.py :: _trsi_sig -------------------------------------
        private int TrendRsiSignal(int i, double c, double emaV, double rsi, double rsiPrev)
        {
            if (i < 1) return 0;                                  // loop starts at 1
            if (double.IsNaN(emaV) || double.IsNaN(rsi)) return _rawState;

            bool upTrend = c > emaV;
            bool dnTrend = c < emaV;
            // NaN comparisons are false in both C# and NumPy, so a NaN rsiPrev
            // simply blocks the crossing - same as the Python original.
            bool longTrigger = upTrend && rsiPrev < RsiBuy && rsi >= RsiBuy;
            bool shortTrigger = dnTrend && rsiPrev > RsiSell && rsi <= RsiSell;

            if (_rawState == 0)
            {
                if (longTrigger) _rawState = 1;
                else if (shortTrigger) _rawState = -1;
            }
            else if (_rawState == 1)
            {
                if (c < emaV || rsi >= 70.0) _rawState = 0;
            }
            else
            {
                if (c > emaV || rsi <= 30.0) _rawState = 0;
            }
            return _rawState;
        }

        // ====================================================================
        //  fsm_barrier - triple-barrier state machine (strategies.py:20-57)
        // ====================================================================
        private void StepBarrier(int rawSig, double h, double l, double c, double atr, bool newDay)
        {
            int s = double.IsNaN(atr) ? 0 : rawSig;

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
                if (s != 0 && s != _locked && !double.IsNaN(atr)) OpenState(s, c, atr);
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
            if (UseBrokerStops) ModifyPosition(r.Position, _sl, _tp);
            if (Verbose) Print("{0}: opened {1} vol={2} sl={3} tp={4}", Label, tt, volume, _sl, _tp);
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
