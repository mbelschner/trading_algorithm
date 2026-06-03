# PnL-Based Feature Selection Pipeline

## Overview

This pipeline (`03_pnl_feature_selection.R`) implements a **profit-driven feature selection** approach for algorithmic trading. Unlike traditional ML approaches that optimize for label prediction accuracy (AUC, F1-Score), this pipeline evaluates features based on their ability to generate **profitable trades** using real trading metrics like Sharpe Ratio and Profit Factor.

### Key Innovation

**Traditional Approach:**
```
Features -> Model -> Predict Labels -> Optimize AUC/F1
```

**This Pipeline:**
```
Features -> Model -> Predict Signals -> Simulate Trades -> Optimize Sharpe/PnL
```

A feature group with 60% prediction accuracy might generate losing trades (bad entry timing, poor risk/reward), while a group with 55% accuracy might generate profitable trades due to better signal quality.

---

## Pipeline Structure

The pipeline follows the established naming convention:
- `01_labelling_main_script.R` - Triple Barrier Labeling
- `02_backtest_main_script_ls_v2.R` - Traditional ML Backtest
- **`03_pnl_feature_selection.R`** - PnL-Based Feature Selection (this script)

---

## Prerequisites

### Required Packages
```r
library(data.table)
library(xgboost)
library(progress)
library(lubridate)
library(pROC)  # For AUC calculation
```

### Required Input Files

1. **Cached Features** (from `02_backtest_main_script_ls_v2.R`):
   ```
   backtest_results/cache/GOLD_MINUTE_15_features.csv
   ```

2. **Labeled Data** (from `01_labelling_main_script.R`):
   ```
   labelled_data/GOLD_MINUTE_15_labeled.csv
   ```

3. **Raw Price Data**:
   ```
   price_data/GOLD_MINUTE_15.csv
   ```

---

## Pipeline Steps

### STEP 1: Feature Grouping (Semantic)

**Purpose:** Group ~500 features into semantically meaningful clusters for efficient evaluation.

**Method:**
- Pattern-based grouping (EMA_, RSI_, ADX_, etc.)
- Balanced groups (~10 features per group)
- Automatic merging of small groups
- Automatic splitting of large groups

**Output:**
- `feature_groups` list with 50-70 groups
- Each group contains related features (e.g., all EMA lags, all RSI derivatives)

**Example Groups:**
```
G01_EMA_lag_P1:     ema_9_lag1, ema_9_lag2, ema_9_lag3, ...
G02_RSI_base:       rsi_7, rsi_14, rsi_21, ...
G03_ATR_derivatives: atr_14_d1, atr_14_d2, ...
G04_Volume_rolling:  volume_roll_mean_10, volume_roll_sd_20, ...
```

---

### STEP 2: Trade Simulation Function

**Purpose:** Simulate realistic trades based on model predictions using Triple Barrier exit.

**Key Function:** `simulate_trades()`

**Parameters:**
| Parameter | Default | Description |
|-----------|---------|-------------|
| `entry_threshold` | 0.6 | Minimum prediction to enter trade |
| `atr_mult_tp` | 2.5 | Take Profit at 2.5x ATR |
| `atr_mult_sl` | 2.5 | Stop Loss at 2.5x ATR |
| `max_bars` | 16 | Time stop after 16 bars (4 hours) |
| `slippage_pct` | 0.0002 | 0.02% slippage per trade |
| `commission_pct` | 0.0002 | 0.02% commission round-trip |
| `session_filter` | TRUE | Only trade during session hours |

**Exit Logic:**
1. **Take Profit:** Price touches TP level (ATR-based)
2. **Stop Loss:** Price touches SL level (ATR-based)
3. **Time Stop:** Max holding period reached

**Calculated Metrics:**
- Sharpe Ratio (annualized for 15-min bars)
- Profit Factor (gross profit / gross loss)
- Win Rate
- Maximum Drawdown
- Total Return
- Average Bars Held

---

### STEP 3: Single Window Backtest

**Purpose:** Validate the pipeline on a single train/validation window before full execution.

**Key Function:** `evaluate_feature_group_single_window()`

**Dual-Data Approach:**
```
Training:   Use LABELED data only (rows with known labels)
Prediction: Use ALL price data (every 15-min bar)
Simulation: Use ALL price data (realistic trading)
```

This approach allows the model to generate signals for every bar, not just labeled ones.

---

### STEP 4: Full Walk-Forward Pipeline

**Purpose:** Evaluate all feature groups across multiple time windows to assess consistency.

**Key Functions:**
- `generate_walk_forward_windows()` - Create rolling train/val windows
- `evaluate_group_walk_forward()` - Evaluate one group across all windows
- `run_full_walk_forward()` - Orchestrate full evaluation

**Walk-Forward Windows:**
```
Window 1: Train 2019-06 to 2020-12 | Val 2021-01 to 2021-03
Window 2: Train 2019-09 to 2021-03 | Val 2021-04 to 2021-06
Window 3: Train 2019-12 to 2021-06 | Val 2021-07 to 2021-09
...
```

**Ranking Criteria (Composite Score):**
```
Score = 0.4 * Sharpe + 0.3 * Consistency + 0.2 * ProfitFactor + 0.1 * TradeCount
```

Where:
- **Sharpe:** Mean Sharpe Ratio across all windows
- **Consistency:** % of windows with positive Sharpe
- **ProfitFactor:** Mean Profit Factor
- **TradeCount:** Log-scaled total trades (ensures statistical significance)

**Output Files:**
```
backtest_results/cache/GOLD_MINUTE_15_walk_forward_results.rds
backtest_results/cache/GOLD_MINUTE_15_long_group_ranking.csv
backtest_results/cache/GOLD_MINUTE_15_short_group_ranking.csv
```

---

### STEP 5: Final Model & Reporting

**Purpose:** Build production models from top features and generate comprehensive report.

#### 5.1 Feature Selection with Correlation Cleaning

**Function:** `select_features_with_correlation_cleaning()`

**Process:**
1. Extract features from top 15 groups
2. Calculate correlation matrix
3. Greedy selection: remove features with >85% correlation
4. Limit to 50 features max

#### 5.2 Final Model Training

**Function:** `train_final_model()`

**Production Parameters:**
| Parameter | Value |
|-----------|-------|
| `max_depth` | 4 (stronger than feature selection) |
| `eta` | 0.03 (slower learning) |
| `nrounds` | 500 |
| `early_stopping` | 50 |
| `min_child_weight` | 10 |

#### 5.3 Test Set Evaluation

**Held-out Test Period:** 2024-Q4 (not seen during training or validation)

#### 5.4 Baseline Comparison

**Function:** `generate_baseline_comparison()`

Runs 100 simulations with random predictions to establish statistical baseline.

#### 5.5 HTML Report Generation

**Function:** `generate_html_report()`

Creates comprehensive report with:
- Executive Summary
- Test Results (Long & Short)
- Baseline Comparison
- Top Feature Groups Rankings
- Selected Features Lists
- Configuration Details

---

## Output Files

### Models
```
backtest_results/models/
├── GOLD_MINUTE_15_pnl_model_long.rds      # Full model object
├── GOLD_MINUTE_15_pnl_model_long.xgb      # XGBoost binary
├── GOLD_MINUTE_15_pnl_model_short.rds
├── GOLD_MINUTE_15_pnl_model_short.xgb
├── GOLD_MINUTE_15_pnl_selected_features_long.csv
└── GOLD_MINUTE_15_pnl_selected_features_short.csv
```

### Results
```
backtest_results/
├── GOLD_MINUTE_15_pnl_feature_selection_report.html
└── cache/
    ├── GOLD_MINUTE_15_walk_forward_results.rds
    ├── GOLD_MINUTE_15_long_group_ranking.csv
    ├── GOLD_MINUTE_15_short_group_ranking.csv
    ├── step1_complete.RData
    ├── step2_complete.RData
    ├── step3_complete.RData
    ├── step4_complete.RData
    └── step5_complete.RData
```

---

## Configuration

All parameters are centralized in the `CONFIG` list at the top of the script:

```r
CONFIG <- list(
  # Asset Configuration
  epic = "GOLD",
  interval = "MINUTE_15",

  # Trade Simulation
  entry_threshold = 0.6,        # Signal threshold
  atr_multiplier_tp = 2.5,      # Take Profit
  atr_multiplier_sl = 2.5,      # Stop Loss
  max_bars_held = 16,           # Time stop (4 hours)
  slippage_pct = 0.0002,        # 0.02%
  commission_pct = 0.0002,      # 0.02%
  use_session_filter = TRUE,    # Session hours only

  # XGBoost (Feature Selection - intentionally weak)
  xgb_max_depth = 2,
  xgb_n_estimators = 50,
  xgb_learning_rate = 0.05,
  xgb_early_stopping = 10,

  # Evaluation
  min_trades_significance = 30,
  annualization_factor = sqrt(26 * 252),

  # Paths
  cache_path = "backtest_results/cache"
)
```

---

## Usage

### Full Pipeline Execution

```r
# Run entire pipeline (takes several hours)
source("r/03_pnl_feature_selection.R")
```

### Step-by-Step Execution

```r
# Run STEP 1-3 (quick validation)
# Edit script: set test_mode = TRUE at end of STEP 4
source("r/03_pnl_feature_selection.R")

# Review results, then run full pipeline
# Edit script: set test_mode = FALSE
source("r/03_pnl_feature_selection.R")
```

### Resume from Checkpoint

```r
# Load previous state
load("backtest_results/cache/step3_complete.RData")

# Continue from STEP 4
# (Comment out STEP 1-3 in script)
```

---

## Key Insights

### Why PnL-Based Selection?

1. **Label Accuracy ≠ Profitability**
   - A model with 55% accuracy on labels might be more profitable than one with 65%
   - Entry timing, risk/reward ratios, and trade frequency matter

2. **Transaction Costs**
   - High-frequency signals with small edge become unprofitable after costs
   - This pipeline naturally penalizes over-trading

3. **Consistency Over Peak Performance**
   - A feature group with Sharpe 0.5 across all windows is better than one with Sharpe 2.0 in some windows and -1.0 in others

### XGBoost Parameter Strategy

**Feature Selection Phase (weak model):**
- `max_depth = 2` - Prevents overfitting to specific features
- `n_estimators = 50` - Quick training
- `eta = 0.05` - Moderate learning rate

**Final Model (stronger):**
- `max_depth = 4` - More expressive
- `n_estimators = 500` - More trees with early stopping
- `eta = 0.03` - Slower, more careful learning

---

## Troubleshooting

### "Label contains NaN"
- Ensure labels are merged correctly with features
- Check that `dt_merged[!is.na(label)]` is applied

### Zero trades generated
- Check prediction distribution: `summary(predictions)`
- Lower `entry_threshold` if predictions are clustered below threshold
- Verify `in_session` column exists and has TRUE values

### Memory issues
- Reduce `n_groups` in `create_balanced_groups()`
- Use `test_mode = TRUE` for initial validation
- Clear intermediate objects: `rm(list = ls(pattern = "temp_"))`

### Slow execution
- Reduce walk-forward windows: increase `step_months`
- Use fewer feature groups: set `test_mode = TRUE`
- Consider parallel processing (requires `future` package setup)

---

## Extending the Pipeline

### Adding New Feature Groups

```r
# Add custom group to semantic_groups
semantic_groups$custom_momentum <- c(
  "custom_indicator_1",
  "custom_indicator_2",
  "custom_roc_variant"
)
```

### Custom Ranking Criteria

```r
# Modify composite score in run_full_walk_forward()
long_summary[, composite_score := (
  0.5 * scale(mean_sharpe)[, 1] +      # More weight on Sharpe
  0.3 * scale(consistency)[, 1] +
  0.1 * scale(mean_profit_factor)[, 1] +
  0.1 * scale(log1p(total_trades))[, 1]
)]
```

### Different Assets

```r
# Change CONFIG
CONFIG$epic <- "EURUSD"
CONFIG$interval <- "MINUTE_5"

# Adjust ATR multipliers for different volatility
CONFIG$atr_multiplier_tp <- 2.0
CONFIG$atr_multiplier_sl <- 1.5
```

---

## Version History

- **v1.0** (2025-01): Initial implementation
  - Semantic feature grouping
  - Triple Barrier trade simulation
  - Walk-forward validation
  - Correlation-cleaned feature selection
  - HTML report generation

---

## Author

Generated with Claude Code assistance.

## License

Internal use only. Part of the algorithmic trading research project.

