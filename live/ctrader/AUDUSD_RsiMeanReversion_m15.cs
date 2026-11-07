// ============================================================================
//  AUDUSD  -  RSI Mean Reversion  (M15)
// ----------------------------------------------------------------------------
//  1:1 port of  pyrunner/strategies.py :: rsi_mean_reversion()  +  fsm_barrier()
//  and  pyrunner/indicators.py :: rsi_wilder(), atr().
//
//  Selected from results/runner_results_py_20260805_1027.csv
//    period=21, lower=20, upper=75, exit_mid=True,
//    atr_period=14, sl=2.5xATR, tp=2.0xATR
//    IS  (2023-01 .. 2025-06): Sharpe 0.01 | PF 1.15
//    OS  (2025-06 .. 2026-06): Sharpe 1.45 | PF 2.65 | MaxDD -0.8% | 33 trades
//
//  *** LOW CONFIDENCE - PAPER TRADE ONLY ***
//  33 out-of-sample trades is far too small a sample to establish an edge, and
//  the surrounding RSI parameter neighbourhood on AUDUSD has a NEGATIVE median
//  out-of-sample Sharpe. This is the best FX candidate in the run, not a good
//  one. See README.md.
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
    public class AudUsdRsiMeanReversionM15 : Robot
    {
        // ---------------- Signal parameters (from the backtest) --------------
        [Parameter("RSI period", Group = "Signal", DefaultValue = 21, MinValue = 2)]
        public int RsiPeriod { get; set; }

        [Parameter("Lower threshold", Group = "Signal", DefaultValue = 20.0)]
        public double Lower { get; set; }

        [Parameter("Upper threshold", Group = "Signal", DefaultValue = 75.0)]
        public double Upper { get; set; }

        // ---------------- Barrier parameters --------------------------------
        [Parameter("Barrier ATR period", Group = "Barriers", DefaultValue = 14, MinValue = 1)]
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

        private const string Label = "RSIMR_AUDUSD_M15";

        // ---------------- Streaming indicator state -------------------------
        private double _trEma, _alphaBar;
        private double _avgUp, _avgDn, _alphaRsi;

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
            _alphaRsi = 1.0 / RsiPeriod;          // Wilder smoothing

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

            // ---- Wilder RSI (indicators.py::rsi_wilder) ---------------------
            double d = i > 0 ? c - Bars.ClosePrices[i - 1] : 0.0;   // np.diff(prepend=x[0])
            double up = d > 0 ? d : 0.0;
            double dn = d < 0 ? -d : 0.0;
            _avgUp = i == 0 ? up : _alphaRsi * up + (1 - _alphaRsi) * _avgUp;
            _avgDn = i == 0 ? dn : _alphaRsi * dn + (1 - _alphaRsi) * _avgDn;
            double rs = _avgUp / (_avgDn == 0 ? 1e-10 : _avgDn);
            double rsi = i < RsiPeriod ? double.NaN : 100.0 - 100.0 / (1.0 + rs);

            // ---- Raw signal: _rsi_sig ---------------------------------------
            int rawSig = RsiSignal(rsi);

            bool newDay = i > 0 && Bars.OpenTimes[i - 1].Date != Bars.OpenTimes[i].Date;
            StepBarrier(rawSig, h, l, c, atr, newDay);
        }

        // ---- strategies.py :: _rsi_sig --------------------------------------
        private int RsiSignal(double r)
        {
            if (double.IsNaN(r)) return 0;        // sig[i] = 0, state left untouched

            if (_rawState == 0)
            {
                if (r < Lower) _rawState = 1;
                else if (r > Upper) _rawState = -1;
            }
            else if (_rawState == 1)
            {
                if (r >= 50.0) _rawState = 0;
                else if (r > Upper) _rawState = -1;
            }
            else
            {
                if (r <= 50.0) _rawState = 0;
                else if (r < Lower) _rawState = 1;
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
