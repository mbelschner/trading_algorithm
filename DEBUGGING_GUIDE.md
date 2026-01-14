# PnL Simulation Debugging Guide

## Problem
The PnL simulation script (`r/03_pnl_simulation.R`) is generating 100% neutral signals (signal=0) with no long or short signals.

## Root Causes

Based on the investigation, there are several potential causes:

### 1. **Models Predicting Low Probabilities (Most Likely)**
- The XGBoost models are trained on 2019-2024 data
- When applied to 2025 data, they may be predicting very low probabilities (all < 0.5)
- This happens when there's a distribution shift between training and test data
- XGBoost models can be conservative on out-of-distribution data

### 2. **Binary Threshold Too High**
- The script uses a fixed threshold of 0.5
- If the models' maximum probability on test data is < 0.5, no signals will be generated
- This is common with imbalanced datasets even when models are working correctly

### 3. **Feature Distribution Shift**
- Market conditions in 2025 may differ from 2019-2024
- Features may have different statistical properties
- Models may be uncertain about new market regimes

## Investigation Steps

### Step 1: Run the Diagnostic Script

I've created a comprehensive diagnostic script:

```bash
cd c:/Users/maxib/OneDrive/Dokumente/trading_algorithm
Rscript r/03_debug_pnl_simulation.R
```

This will show you:
- ✓ Whether data for 2025 exists (already confirmed: 4,271 rows)
- ✓ Whether all features are present
- Probability distributions for both models
- How many predictions exceed various thresholds
- Top predictions for both models
- Comparison with training data predictions
- Feature distribution comparisons

### Step 2: Insert Debug Snippet into Original Script

Add the debug snippet to your original script to see what's happening:

```r
# In 03_pnl_simulation.R, after line 280, add:
source("r/03_debug_snippet.R")
```

## Solutions

### Solution 1: Use Adaptive Thresholds (Recommended)

Instead of a fixed 0.5 threshold, use percentile-based thresholds:

```r
# Replace lines 266-280 in 03_pnl_simulation.R with:

cat("\n=== STEP 7: COMBINE LONG AND SHORT SIGNALS (ADAPTIVE THRESHOLDS) ===\n")

# Calculate adaptive thresholds (top X% of predictions)
PERCENTILE <- 0.95  # Top 5% become signals

threshold_long <- quantile(pred_prob_long, PERCENTILE)
threshold_short <- quantile(pred_prob_short, PERCENTILE)

cat(sprintf("\nAdaptive thresholds:\n"))
cat(sprintf("  Long:  %.4f (top %.0f%%)\n", threshold_long, (1 - PERCENTILE) * 100))
cat(sprintf("  Short: %.4f (top %.0f%%)\n", threshold_short, (1 - PERCENTILE) * 100))

# Create binary signals using adaptive thresholds
dt_test[, signal_long := fifelse(pred_prob_long > threshold_long, 1, 0)]
dt_test[, signal_short := fifelse(pred_prob_short > threshold_short, 1, 0)]

# Combined signal: Long=+1, Short=-1, Neutral=0
dt_test[, signal := fcase(
  signal_long == 1 & signal_short == 0, 1L,
  signal_short == 1 & signal_long == 0, -1L,
  signal_long == 1 & signal_short == 1 & pred_prob_long > pred_prob_short, 1L,
  signal_long == 1 & signal_short == 1 & pred_prob_short >= pred_prob_long, -1L,
  default = 0L
)]
```

### Solution 2: Lower the Fixed Threshold

If you want to keep a fixed threshold, try lower values:

```r
# Replace line 267 in 03_pnl_simulation.R:
THRESHOLD <- 0.3  # Lower from 0.5 to 0.3
```

Test different thresholds: 0.3, 0.4, 0.45

### Solution 3: Use Probability Ranking

Generate signals for the top N trades by probability:

```r
# Replace lines 266-280 with:

cat("\n=== STEP 7: COMBINE SIGNALS (TOP N RANKING) ===\n")

N_TOP_TRADES <- 100  # Number of trades per direction

# Rank predictions
dt_test[, rank_long := frank(-pred_prob_long, ties.method = "random")]
dt_test[, rank_short := frank(-pred_prob_short, ties.method = "random")]

# Top N become signals
dt_test[, signal_long := fifelse(rank_long <= N_TOP_TRADES, 1, 0)]
dt_test[, signal_short := fifelse(rank_short <= N_TOP_TRADES, 1, 0)]

# Combined signal
dt_test[, signal := fcase(
  signal_long == 1 & signal_short == 0, 1L,
  signal_short == 1 & signal_long == 0, -1L,
  signal_long == 1 & signal_short == 1 & pred_prob_long > pred_prob_short, 1L,
  signal_long == 1 & signal_short == 1 & pred_prob_short >= pred_prob_long, -1L,
  default = 0L
)]

cat(sprintf("\nTop %d trades selected for each direction\n", N_TOP_TRADES))
```

### Solution 4: Model Calibration

If the models are systematically underconfident, you can apply probability calibration:

```r
# After line 260, add calibration:

# Calibrate probabilities (shift to make mean match training data)
# This assumes you know the training set prediction mean
TRAIN_MEAN_LONG <- 0.33   # Replace with actual training mean
TRAIN_MEAN_SHORT <- 0.33  # Replace with actual training mean

test_mean_long <- mean(pred_prob_long)
test_mean_short <- mean(pred_prob_short)

# Apply shift
shift_long <- TRAIN_MEAN_LONG - test_mean_long
shift_short <- TRAIN_MEAN_SHORT - test_mean_short

cat(sprintf("\nCalibrating probabilities:\n"))
cat(sprintf("  Long shift:  %.4f\n", shift_long))
cat(sprintf("  Short shift: %.4f\n", shift_short))

pred_prob_long_cal <- pmin(pmax(pred_prob_long + shift_long, 0), 1)
pred_prob_short_cal <- pmin(pmax(pred_prob_short + shift_short, 0), 1)

dt_test[, pred_prob_long := pred_prob_long_cal]
dt_test[, pred_prob_short := pred_prob_short_cal]
```

## Quick Diagnostic Commands

To quickly check what's happening, run these in R:

```r
# Load the models and data
source("r/03_pnl_simulation.R")  # Run up to line 260

# Check probability distributions
summary(dt_test$pred_prob_long)
summary(dt_test$pred_prob_short)

# Check how many exceed threshold
sum(dt_test$pred_prob_long > 0.5)
sum(dt_test$pred_prob_short > 0.5)

# Find what threshold would give you 5% signals
quantile(dt_test$pred_prob_long, 0.95)
quantile(dt_test$pred_prob_short, 0.95)

# Look at top predictions
head(dt_test[order(-pred_prob_long), .(datetime, pred_prob_long, pred_prob_short)], 20)
```

## Files Created

1. **`r/03_debug_pnl_simulation.R`** - Full diagnostic script
   - Run this standalone to diagnose the issue
   - Shows comprehensive statistics

2. **`r/03_debug_snippet.R`** - Debug code snippet
   - Insert this into your original script
   - Provides inline diagnostics

3. **`DEBUGGING_GUIDE.md`** - This guide
   - Reference for solutions

## Expected Outcomes

After applying Solution 1 (Adaptive Thresholds):
- You should see ~5% long signals and ~5% short signals (configurable)
- The signals will be based on relative probability ranking
- Works regardless of absolute probability values

After applying Solution 2 (Lower Threshold):
- You should see signals if max probability > new threshold
- May need experimentation to find optimal threshold

After applying Solution 3 (Top N Ranking):
- Guaranteed to generate exactly N long and N short signals
- Based purely on ranking, not probability magnitude

## Verification

After implementing a solution, verify it worked:

```r
# Check signal distribution
table(dt_test$signal)

# Should show non-zero counts for long and short:
#   -1    0    1
#  XXX XXXX XXX

# Check probability thresholds used
cat("Long signals probability range:",
    range(dt_test[signal == 1]$pred_prob_long), "\n")
cat("Short signals probability range:",
    range(dt_test[signal == -1]$pred_prob_short), "\n")
```

## Further Investigation

If signals are still all neutral after trying solutions:

1. Check if models are loading correctly:
   ```r
   print(model_long)
   print(model_short)
   ```

2. Check feature names match:
   ```r
   missing_long <- setdiff(model_long$feature_names, names(dt_test))
   missing_short <- setdiff(model_short$feature_names, names(dt_test))
   print(missing_long)
   print(missing_short)
   ```

3. Check for NA values in features:
   ```r
   na_counts <- colSums(is.na(dt_test))
   print(na_counts[na_counts > 0])
   ```

4. Compare training vs test feature distributions:
   ```r
   # For each feature, compute mean difference
   for (feat in model_long$feature_names) {
     train_mean <- mean(dt_train[[feat]], na.rm = TRUE)
     test_mean <- mean(dt_test[[feat]], na.rm = TRUE)
     if (abs(test_mean - train_mean) / (train_mean + 1e-10) > 0.5) {
       cat(sprintf("%s: train=%.4f, test=%.4f (%.1f%% diff)\n",
                   feat, train_mean, test_mean,
                   100 * (test_mean - train_mean) / train_mean))
     }
   }
   ```

## Recommended Next Steps

1. ✅ Run `r/03_debug_pnl_simulation.R` to confirm the issue
2. ✅ Apply **Solution 1 (Adaptive Thresholds)** - most robust
3. ✅ Verify signals are being generated
4. ✅ Analyze PnL performance
5. If performance is poor, investigate feature distribution shift

---

**Note**: The core issue is that fixed thresholds don't work well when models are uncertain about out-of-sample data. Adaptive approaches are more robust.
