// ============================================================================
//  COPPER  -  Ichimoku Triple  (M15)
// ----------------------------------------------------------------------------
//  1:1 port of  pyrunner/strategies.py :: ichimoku_triple()  +  fsm_barrier()
//  and  pyrunner/indicators.py :: ichimoku(), atr().
//
//  NOTE ON CHIKOU: the backtested implementation uses  chikou[i] = close[i-26]
//  and requires  chikou[i] > close[i]  for a LONG.  That is the INVERSE of the
//  textbook Ichimoku lagging-span rule.  It is reproduced here verbatim because
//  that is the rule the reported edge was measured on.  Do not "fix" it without
//  re-running the backtest.
//
//  Selected from results/runner_results_py_20260805_1027.csv
//    tenkan=9, kijun=26, senkou_b=52, displacement=26, cloud_filter=True,
//    atr_period=10, sl=2.5xATR, tp=2.0xATR
//    IS  (2023-01 .. 2025-06): Sharpe 0.48 | PF 1.30
//    OS  (2025-06 .. 2026-06): Sharpe 0.50 | PF 1.22 | MaxDD -4.3% | 101 trades
//    Every year of the full sample positive; all 64 configs of the family had
//    OS PF > 1.
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
    public class CopperIchimokuTripleM15 : Robot
    {
        // ---------------- Signal parameters (from the backtest) --------------
        [Parameter("Tenkan n", Group = "Signal", DefaultValue = 9, MinValue = 1)]
        public int TenkanN { get; set; }

        [Parameter("Kijun n", Group = "Signal", DefaultValue = 26, MinValue = 1)]
        public int KijunN { get; set; }

        [Parameter("Senkou B n", Group = "Signal", DefaultValue = 52, MinValue = 1)]
        public int SenkouBN { get; set; }

        [Parameter("Displacement", Group = "Signal", DefaultValue = 26, MinValue = 1)]
        public int Displacement { get; set; }

        [Parameter("Cloud filter", Group = "Signal", DefaultValue = true)]
        public bool CloudFilter { get; set; }

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

        private const string Label = "ICHI_COPPER_M15";

        private double _trEma, _alphaBar;
        private int _nStart;

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
            _nStart = Math.Max(KijunN, Math.Max(SenkouBN, Displacement)) + 1;

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

            double prevC = i > 0 ? Bars.ClosePrices[i - 1] : c;
            double tr = Math.Max(h - l, Math.Max(Math.Abs(h - prevC), Math.Abs(l - prevC)));
            _trEma = i == 0 ? tr : _alphaBar * tr + (1 - _alphaBar) * _trEma;
            double atr = i < AtrPeriod ? double.NaN : _trEma;

            int rawSig = IchimokuSignal(i, c);

            bool newDay = i > 0 && Bars.OpenTimes[i - 1].Date != Bars.OpenTimes[i].Date;
            StepBarrier(rawSig, h, l, c, atr, newDay);
        }

        // ---- strategies.py :: _ichi_sig -------------------------------------
        private int IchimokuSignal(int i, double c)
        {
            if (i < _nStart) return 0;

            double tenkan = DonchMid(i, TenkanN);
            double kijun = DonchMid(i, KijunN);
            double tenkanPrev = DonchMid(i - 1, TenkanN);
            double kijunPrev = DonchMid(i - 1, KijunN);

            int j = i - Displacement;                       // np.roll(sa, +disp)
            double sa = double.NaN, sb = double.NaN;
            if (j >= 0)
            {
                sa = (DonchMid(j, TenkanN) + DonchMid(j, KijunN)) / 2.0;
                sb = DonchMid(j, SenkouBN);
            }
            double chikou = j >= 0 ? Bars.ClosePrices[j] : double.NaN;

            if (double.IsNaN(tenkan) || double.IsNaN(kijun) ||
                double.IsNaN(sa) || double.IsNaN(sb) || double.IsNaN(chikou))
                return _rawState;                            // sig[i] = state; continue

            bool kt = tenkan > kijun, kt1 = tenkanPrev <= kijunPrev;
            bool kd = tenkan < kijun, kd1 = tenkanPrev >= kijunPrev;
            double kuTop = Math.Max(sa, sb), kuBot = Math.Min(sa, sb);
            bool tkUp = kt && kt1, tkDn = kd && kd1;
            bool ckL = chikou > c, ckS = chikou < c;

            bool longOk = tkUp && (!CloudFilter || c > kuTop) && ckL;
            bool shortOk = tkDn && (!CloudFilter || c < kuBot) && ckS;
            bool exL = tenkan < kijun || (CloudFilter && c < kuBot);
            bool exS = tenkan > kijun || (CloudFilter && c > kuTop);

            if (_rawState == 0)
            {
                if (longOk) _rawState = 1;
                else if (shortOk) _rawState = -1;
            }
            else if (_rawState == 1)
            {
                if (exL || shortOk) _rawState = shortOk ? -1 : 0;
            }
            else
            {
                if (exS || longOk) _rawState = longOk ? 1 : 0;
            }
            return _rawState;
        }

        /// <summary>(rolling max of High + rolling min of Low) / 2 over n bars.</summary>
        private double DonchMid(int i, int n)
        {
            if (i < n - 1 || i < 0) return double.NaN;
            double hi = double.MinValue, lo = double.MaxValue;
            for (int k = i - n + 1; k <= i; k++)
            {
                double hv = Bars.HighPrices[k];
                double lv = Bars.LowPrices[k];
                if (hv > hi) hi = hv;
                if (lv < lo) lo = lv;
            }
            return (hi + lo) / 2.0;
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
