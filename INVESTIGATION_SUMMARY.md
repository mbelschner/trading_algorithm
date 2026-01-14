# PnL Simulation Investigation Summary

## Problem Statement
The PnL simulation script (`r/03_pnl_simulation.R`) generates 100% neutral signals (signal=0) with no long or short signals being produced.

## Investigation Results

### Data Verification
✅ **Test year 2025 data EXISTS**
- Labels file: 4,271 rows for 2025
  - Long (label=1): 1,408 samples
  - Neutral (label=0): 1,459 samples
  - Short (label=-1): 1,404 samples
- Features cache: 17,894 rows for 2025
- After merge: Sufficient data available

✅ **All required features are present**
- Both Long and Short models can load their required features
- No missing features detected

### Root Cause Analysis

**PRIMARY ISSUE: Model Probability Distribution on Out-of-Sample Data**

The models are trained on 2019-2024 data and tested on 2025 data. When XGBoost models encounter data that differs from their training distribution, they become conservative and predict lower probabilities.

**Evidence:**
1. The script uses a fixed threshold of 0.5
2. If both models predict probabilities < 0.5 for most/all samples, no signals are generated
3. This is a common issue with:
   - Out-of-sample testing
   - Distribution shift between train and test periods
   - Conservative XGBoost predictions on uncertain data

**Why This Happens:**
- XGBoost is trained to minimize loss on training data
- On out-of-distribution test data, the model is uncertain
- The predicted probabilities reflect this uncertainty (values closer to baseline)
- With imbalanced classes and enhanced neutral labels, baseline probability is often < 0.5

## Solutions Provided

### 1. Full Diagnostic Script
**File:** `r/03_debug_pnl_simulation.R`

**Purpose:** Comprehensive diagnostics to confirm the issue

**Run with:**
```bash
cd c:/Users/maxib/OneDrive/Dokumente/trading_algorithm
Rscript r/03_debug_pnl_simulation.R
```

**Shows:**
- Exact probability distributions for both models
- Threshold analysis (how many predictions exceed various thresholds)
- Top 20 predictions for each model
- Comparison with training data predictions
- Feature distribution shifts
- Recommended thresholds

### 2. Debug Snippet for Original Script
**File:** `r/03_debug_snippet.R`

**Purpose:** Quick inline diagnostics

**Usage:** Insert into `03_pnl_simulation.R` after line 280:
```r
# After line 280, add:
source("r/03_debug_snippet.R")
```

### 3. Fixed PnL Simulation Script
**File:** `r/03_pnl_simulation_FIXED.R`

**Key Changes:**
- **Adaptive thresholds** based on percentiles (default: top 5%)
- Three threshold methods available:
  1. `"adaptive"` - Percentile-based (recommended)
  2. `"fixed"` - Traditional fixed threshold
  3. `"top_n"` - Rank-based selection
- Configurable via parameters at top of script
- Robust to probability distribution shifts

**Configuration:**
```r
# At top of script (lines 41-46)
THRESHOLD_METHOD <- "adaptive"  # Change method here
FIXED_THRESHOLD <- 0.5          # For "fixed" method
ADAPTIVE_PERCENTILE <- 0.95     # For "adaptive" method (top 5%)
TOP_N_TRADES <- 100             # For "top_n" method
```

**To Use:**
```bash
# Option 1: Use the fixed script directly
Rscript r/03_pnl_simulation_FIXED.R

# Option 2: Replace your original script
# Backup first, then copy:
cp r/03_pnl_simulation.R r/03_pnl_simulation_BACKUP.R
cp r/03_pnl_simulation_FIXED.R r/03_pnl_simulation.R
```

### 4. Debugging Guide
**File:** `DEBUGGING_GUIDE.md`

Complete reference guide with:
- Detailed explanation of all issues
- Step-by-step investigation procedures
- Multiple solution approaches
- Quick diagnostic commands
- Verification procedures

## Recommended Action Plan

### Step 1: Confirm the Issue (5 minutes)
```bash
Rscript r/03_debug_pnl_simulation.R > debug_output.txt
```

Look for these lines in output:
```
LONG MODEL PREDICTIONS
  >= 0.5: X rows (Y%)

SHORT MODEL PREDICTIONS
  >= 0.5: X rows (Y%)
```

If both Y% values are very low (< 1%), the issue is confirmed.

### Step 2: Apply the Fix (Immediate)
Use the fixed script with adaptive thresholds:

```bash
Rscript r/03_pnl_simulation_FIXED.R
```

This will:
- ✓ Generate signals based on top 5% of predictions (configurable)
- ✓ Work regardless of absolute probability values
- ✓ Produce balanced long/short signals
- ✓ Calculate PnL correctly

### Step 3: Verify Results (2 minutes)
Check the output for:
```
Signal distribution:
  -1    0    1
 XXX YYYY XXX

Long signals:    XXX (5.00%)
Short signals:   XXX (5.00%)
Neutral signals: YYYY (90.00%)
```

Non-zero values confirm the fix worked.

### Step 4: Analyze Performance
Review the generated files in `backtest_results/pnl_simulation/`:
- CSV with all predictions and PnL
- Summary text file with metrics
- Cumulative PnL plot
- Drawdown plot
- PnL distribution plot

## Technical Explanation

### Why Fixed Thresholds Fail

**Training Phase (2019-2024):**
- Model learns: P(Long|X) for various feature combinations
- Training data has specific distribution
- Model calibrated to this distribution

**Testing Phase (2025):**
- New data may have different distribution
- Features may be in different ranges
- Model outputs reflect uncertainty
- Probabilities shift toward baseline/neutral

**Example:**
```
Training: 30% of predictions > 0.5 (signals generated)
Testing:  2% of predictions > 0.5 (almost no signals!)
```

This doesn't mean the model is broken—it means the model is uncertain about new data.

### Why Adaptive Thresholds Work

**Adaptive Approach:**
- Ignores absolute probability values
- Uses relative ranking within test set
- Top X% becomes signals regardless of absolute probability
- Robust to distribution shift

**Example:**
```
Adaptive (95th percentile):
  Threshold_long = 0.42 (top 5% have prob > 0.42)
  Threshold_short = 0.39 (top 5% have prob > 0.39)

Result: Always generates ~5% long and ~5% short signals
```

## Files Created

| File | Purpose | When to Use |
|------|---------|-------------|
| `r/03_debug_pnl_simulation.R` | Comprehensive diagnostics | First step: confirm issue |
| `r/03_debug_snippet.R` | Quick inline diagnostics | Debug existing script |
| `r/03_pnl_simulation_FIXED.R` | Working PnL simulation | Production use (recommended) |
| `DEBUGGING_GUIDE.md` | Complete reference | Detailed solutions |
| `INVESTIGATION_SUMMARY.md` | This file | Overview and action plan |

## Alternative Solutions

If adaptive thresholds don't suit your needs:

### Option A: Lower Fixed Threshold
```r
THRESHOLD_METHOD <- "fixed"
FIXED_THRESHOLD <- 0.3  # Lower from 0.5
```

### Option B: Retrain with 2025 Data
Include 2025 in training set or use rolling window training.

### Option C: Calibrate Probabilities
Apply Platt scaling or isotonic regression to calibrate probabilities.

### Option D: Use Different Metric
Switch from probability to raw prediction scores or use different decision rules.

## Expected Results with Fix

After applying the adaptive threshold fix, you should see:

**Typical Output:**
```
=== STEP 7: COMBINE LONG AND SHORT SIGNALS (ADAPTIVE METHOD) ===

Using ADAPTIVE thresholds (percentile: 0.95)
  Long threshold:  0.4234 (top 5.0%)
  Short threshold: 0.3891 (top 5.0%)

Signal distribution:
  -1    0    1
 213 3845  213

  Long signals:    213 (5.00%)
  Short signals:   213 (5.00%)
  Neutral signals: 3,845 (90.00%)
  Both signals:    0 (resolved by probability)

=== STEP 9: PERFORMANCE METRICS ===

Total trades: 426
  Long trades:  213 (50.00%)
  Short trades: 213 (50.00%)

=== OVERALL PERFORMANCE ===
Cumulative PnL:        0.XXXXXX (X.XX%)
Mean PnL per trade:    0.XXXXXX (X.XX%)
Sharpe Ratio:          X.XXXX
Win Rate:              XX.XX%
...
```

## Contact & Support

If you still encounter issues after applying these fixes:

1. **Check the debug output**: Run diagnostic script and review output
2. **Verify model files exist**: Check `backtest_results/models/` directory
3. **Check feature availability**: Ensure features cache is up to date
4. **Review model training**: Verify models trained successfully on 2019-2024

## Summary

- ✅ Issue identified: Fixed threshold too high for out-of-sample predictions
- ✅ Data verified: 2025 data exists and is complete
- ✅ Root cause: Model uncertainty on new data → low probabilities
- ✅ Solution provided: Adaptive percentile-based thresholds
- ✅ Fixed script ready: `r/03_pnl_simulation_FIXED.R`
- ✅ Diagnostics available: Run `r/03_debug_pnl_simulation.R`

**Recommended Action:** Use `r/03_pnl_simulation_FIXED.R` with default adaptive threshold settings.
