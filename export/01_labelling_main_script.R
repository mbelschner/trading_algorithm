# =============================================================================
# META-LABELING PIPELINE FOR INTRADAY TRADING
# =============================================================================
#
# Komplett ueberarbeitetes Labeling-System basierend auf:
# - Lopez de Prado: "Advances in Financial Machine Learning" (2018)
# - Meta-Labeling: Primary Signal (Trend) + Secondary ML Target (TP/SL)
# - Dynamic Barriers mit Intraday-Saisonalitaet
# - Sample Uniqueness statt Hard Filtering
# - Statistische Validierung (T-Stat, Korrelation)
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("META-LABELING PIPELINE v2.0\n")
cat("=============================================================================\n")
cat(sprintf("Started: %s\n", Sys.time()))

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  TTR,             # Technical indicators
  ggplot2,         # Visualization
  gridExtra,       # Plot arrangement
  scales,          # Plot scales
  viridis,         # Color palettes
  progress,        # Progress bars
  tictoc,          # Timing
  parallel,        # Parallelization
  zoo              # Rolling functions
)

# ===== SOURCE MODULES ========================================================

source("r/01_01_meta_labeling_core.R")        # Core labeling functions
source("r/01_02_sample_uniqueness.R")         # Sample weights & bootstrapping
source("r/01_03_statistical_validation.R")    # T-stat, correlation analysis
source("r/01_04_labeling_visualizations.R")   # All visualizations

shift = data.table::shift

# ===== CONFIGURATION =========================================================

# -----------------------------------------------------------------------------
# AVAILABLE PRIMARY SIGNAL STRATEGIES:
# -----------------------------------------------------------------------------
# "ema_cross"      - EMA Crossover (50/200) - Classic trend following
# "cmo_vhf_stc"    - CMO-VHF-STC - Momentum + trend filter + early signals [RECOMMENDED]
# "adx_di"         - ADX + DI Crossover - Strong trend filter
# "ichimoku"       - Ichimoku Cloud Breakout - Multi-timeframe perspective
# "supertrend"     - Supertrend + RSI - ATR-based with momentum confirmation
# "macd_volume"    - MACD + Volume - Institutional approach
# "ema_alignment"  - Multi-EMA Alignment - High conviction pullback trades
# "rsi_breakout"   - RSI Breakout - Mean reversion signals
# "macd"           - Simple MACD Crossover - Momentum
# -----------------------------------------------------------------------------

CONFIG <- list(
  # ===== DATA SETTINGS =====
  epic = "GOLD",
  interval = "MINUTE_15",

  # ===== TRADING SESSION (UTC) =====
  session_start = 1,       # 01:00 UTC (Asia open)
  session_end = 22,        # 22:00 UTC (NY close)

  # Session Volatility Multipliers (Gold 15m Intraday Smile)
  # Barriers werden in high-vol Sessions weiter gefasst
  session_vol_multipliers = list(
    asia = 0.8,            # 01:00-08:00 UTC - lower volatility
    london = 1.2,          # 08:00-13:00 UTC - high volatility
    overlap = 1.4,         # 13:00-17:00 UTC - highest (London/NY overlap)
    ny = 1.1,              # 17:00-22:00 UTC - moderate volatility
    default = 1.0
  ),

  # ===== PRIMARY SIGNAL STRATEGY =====
  # Choose one of the strategies listed above
  primary_signal_method = "cmo_vhf_stc",

  # Strategy-specific parameters (adjust as needed)
  signal_params = list(
    # EMA settings
    ema_fast_col = "ema_fast",
    ema_slow_col = "ema_slow",

    # RSI settings
    rsi_overbought = 70,
    rsi_oversold = 30,

    # CMO-VHF-STC settings
    vhf_threshold = 0.35,      # VHF > threshold = trending market
    cmo_threshold = 20,        # CMO > threshold = bullish, < -threshold = bearish
    stc_long_cross = 25,       # STC crosses above this = long signal
    stc_short_cross = 75,      # STC crosses below this = short signal

    # ADX settings
    adx_threshold = 25,        # ADX > threshold = strong trend

    # Volume settings (for macd_volume)
    volume_mult = 1.2,         # Volume > mult * average = institutional interest

    # Signal validity
    signal_validity_bars = 5   # How many bars a signal remains valid
  ),

  # ===== TECHNICAL INDICATOR PERIODS =====
  ema_fast = 50,
  ema_slow = 200,
  rsi_period = 14,
  atr_period = 12,

  # ===== TRIPLE BARRIER SETTINGS =====
  atr_mult_tp = 2.5,           # Take Profit = ATR * multiplier
  atr_mult_sl = 2.0,           # Stop Loss = ATR * multiplier (asymmetric OK)
  max_horizon_bars = 16,       # Vertical barrier (max holding period)

  # Minimum Barrier Distance (Spread + Slippage Protection)
  # Gold spread ~0.00013 (13 pips), Slippage ~1 pip
  min_barrier_distance = 0.00013 * 3 + 0.0001,  # 3x spread + slippage

  # Neutral Threshold (for vertical barrier exits)
  neutral_threshold = 1.5,     # ATR multiplier for "too small" moves

  # ===== SAMPLE WEIGHTING =====
  use_sample_weights = TRUE,
  weight_method = "uniqueness",  # "uniqueness", "sequential_bootstrap", "decay"
  min_sample_weight = 0.1,       # Minimum weight to include sample

  # ===== STATISTICAL VALIDATION =====
  min_tstat = 2.0,             # Minimum T-stat for valid label set
  min_samples = 100,           # Minimum samples per label class

  # ===== OUTPUT =====
  output_path = "labelled_data",
  cache_path = "labelled_data/cache"
)

# Create directories
for (path in c(CONFIG$output_path, CONFIG$cache_path)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ===== LOAD DATA =============================================================

cat("\n=== LOADING DATA ===\n")
tic()

# Load price data
price_file <- file.path(
  "price_data",
  paste0(CONFIG$epic, "_", CONFIG$interval, ".csv")
)

if (!file.exists(price_file)) {
  stop(sprintf("Price file not found: %s", price_file))
}

dt_prices <- fread(price_file)

# Handle different column naming conventions
setnames(dt_prices, tolower(names(dt_prices)))
if ("time" %in% names(dt_prices) && !"datetime" %in% names(dt_prices)) {
  setnames(dt_prices, "time", "datetime")
}

dt_prices[, datetime := as.POSIXct(datetime)]
setorder(dt_prices, datetime)

cat(sprintf("Loaded: %s rows from %s to %s\n",
            format(nrow(dt_prices), big.mark = ","),
            min(dt_prices$datetime),
            max(dt_prices$datetime)))

toc()

# ===== STEP 1: CALCULATE TECHNICAL INDICATORS ================================

cat("\n=== STEP 1: TECHNICAL INDICATORS ===\n")
tic()

dt_prices <- calculate_technical_indicators(
  dt_prices,
  atr_period = CONFIG$atr_period,
  ema_fast = CONFIG$ema_fast,
  ema_slow = CONFIG$ema_slow,
  rsi_period = CONFIG$rsi_period
)

cat(sprintf("Indicators calculated. ATR median: %.5f\n",
            median(dt_prices$atr, na.rm = TRUE)))

toc()

# ===== STEP 2: GENERATE PRIMARY SIGNALS ======================================

cat("\n=== STEP 2: PRIMARY SIGNALS (SIDE) ===\n")
cat(sprintf("Strategy: %s\n", CONFIG$primary_signal_method))
tic()

# Show available strategies
cat("\nAvailable strategies:\n")
strategies <- list_primary_signal_strategies()
for (i in seq_along(strategies)) {
  marker <- ifelse(names(strategies)[i] == CONFIG$primary_signal_method, " <-- SELECTED", "")
  cat(sprintf("  - %s: %s%s\n", names(strategies)[i], strategies[i], marker))
}
cat("\n")

dt_prices <- generate_primary_signals(
  dt_prices,
  method = CONFIG$primary_signal_method,
  params = CONFIG$signal_params
)

# Signal statistics
signal_stats <- dt_prices[!is.na(primary_signal), .N, by = primary_signal]
cat("\nPrimary Signal Distribution:\n")
print(signal_stats)

toc()

# ===== STEP 3: APPLY DYNAMIC TRIPLE BARRIER ==================================

cat("\n=== STEP 3: DYNAMIC TRIPLE BARRIER LABELING ===\n")
tic()

dt_labeled <- apply_dynamic_triple_barrier(
  dt_prices,
  atr_mult_tp = CONFIG$atr_mult_tp,
  atr_mult_sl = CONFIG$atr_mult_sl,
  max_horizon = CONFIG$max_horizon_bars,
  session_start = CONFIG$session_start,
  session_end = CONFIG$session_end,
  session_vol_multipliers = CONFIG$session_vol_multipliers,
  min_barrier_distance = CONFIG$min_barrier_distance,
  neutral_threshold = CONFIG$neutral_threshold
)

cat(sprintf("Labeled: %s observations\n", format(nrow(dt_labeled), big.mark = ",")))

# Meta-label statistics
meta_stats <- dt_labeled[!is.na(meta_label), .N, by = meta_label]
cat("\nMeta-Label Distribution (1=TP hit, 0=SL/Timeout):\n")
print(meta_stats)

toc()

# ===== STEP 4: CALCULATE SAMPLE UNIQUENESS ===================================

cat("\n=== STEP 4: SAMPLE UNIQUENESS & WEIGHTS ===\n")
tic()

dt_weighted <- calculate_sample_uniqueness(
  dt_labeled,
  method = CONFIG$weight_method,
  min_weight = CONFIG$min_sample_weight
)

# Uniqueness statistics
cat(sprintf("\nSample Uniqueness Statistics:\n"))
cat(sprintf("  Mean uniqueness: %.4f\n", mean(dt_weighted$sample_weight)))
cat(sprintf("  Median uniqueness: %.4f\n", median(dt_weighted$sample_weight)))
cat(sprintf("  Effective sample size: %.0f (%.1f%% of original)\n",
            sum(dt_weighted$sample_weight),
            sum(dt_weighted$sample_weight) / nrow(dt_weighted) * 100))

toc()

# ===== STEP 5: STATISTICAL VALIDATION ========================================

cat("\n=== STEP 5: STATISTICAL VALIDATION ===\n")
tic()

validation_results <- validate_label_quality(
  dt_weighted,
  min_tstat = CONFIG$min_tstat,
  min_samples = CONFIG$min_samples
)

# Print validation results
cat("\n--- VALIDATION RESULTS ---\n")
cat(sprintf("T-Statistic (realized_return): %.2f %s\n",
            validation_results$tstat,
            ifelse(validation_results$tstat >= CONFIG$min_tstat, "[PASS]", "[FAIL]")))
cat(sprintf("Expected Return E[R|L=1]: %.4f%%\n", validation_results$expected_return_long * 100))
cat(sprintf("Expected Return E[R|L=-1]: %.4f%%\n", validation_results$expected_return_short * 100))
cat(sprintf("Correlation with Fixed-Horizon Returns: %.3f\n",
            validation_results$fixed_horizon_correlation))

if (!validation_results$is_valid) {
  cat("\nWARNING: Label set does not meet validation criteria!\n")
  cat("Consider adjusting parameters.\n")
}

toc()

# ===== STEP 6: GENERATE VISUALIZATIONS =======================================

cat("\n=== STEP 6: GENERATING VISUALIZATIONS ===\n")
tic()

# Create visualization output directory
viz_path <- file.path(CONFIG$output_path, "visualizations")
if (!dir.exists(viz_path)) dir.create(viz_path, recursive = TRUE)

# 1. Label Density by Hour
plot_label_density_by_hour(dt_weighted, output_path = viz_path)

# 2. Cumulative Edge Plot
plot_cumulative_edge(
  dt_weighted,
  spread = 0.00013,
  slippage_pips = 1.0,
  output_path = viz_path
)

# 3. Barrier Width vs Profitability Heatmap
plot_barrier_profitability_heatmap(dt_weighted, output_path = viz_path)

# 4. Sample Weight Distribution
plot_sample_weight_distribution(dt_weighted, output_path = viz_path)

# 5. Meta-Label Performance
plot_meta_label_performance(dt_weighted, output_path = viz_path)

# 6. Price Chart with Signals (2 example weeks with most signals)
plot_price_with_signals(
  dt_weighted,
  n_weeks = 2,
  output_path = viz_path,
  show_barriers = TRUE,
  show_plot = FALSE
)

cat(sprintf("Visualizations saved to: %s\n", viz_path))

toc()

# ===== STEP 7: SAVE LABELED DATA =============================================

cat("\n=== STEP 7: SAVING LABELED DATA ===\n")

# Output filename
output_file <- file.path(
  CONFIG$output_path,
  sprintf("%s_%s_meta_labeled.csv", CONFIG$epic, CONFIG$interval)
)

# Select columns for output
output_cols <- c(
  "datetime", "open", "high", "low", "close", "volume",
  "atr", "hour", "session", "in_session",
  "primary_signal",         # Side: -1 (short), 1 (long)
  "meta_label",             # ML target: 1 (TP hit), 0 (SL/timeout)
  "barrier_touched",        # Which barrier was hit
  "bars_to_exit",           # Holding period
  "realized_return",        # Actual return achieved
  "realized_return_adj",    # Return adjusted for costs
  "tp_distance", "sl_distance",  # Barrier distances
  "sample_weight",          # Sample uniqueness weight
  "n_concurrent"            # Number of overlapping labels
)

# Keep only existing columns
output_cols <- intersect(output_cols, names(dt_weighted))
dt_output <- dt_weighted[, ..output_cols]

fwrite(dt_output, output_file)
cat(sprintf("Saved: %s (%s rows)\n", output_file, format(nrow(dt_output), big.mark = ",")))

# Also save config for reproducibility
config_file <- file.path(CONFIG$output_path, "labeling_config.rds")
saveRDS(CONFIG, config_file)
cat(sprintf("Config saved: %s\n", config_file))

# ===== SUMMARY ===============================================================

cat("\n")
cat("=============================================================================\n")
cat("LABELING PIPELINE COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("Total observations: %s\n", format(nrow(dt_output), big.mark = ",")))
cat(sprintf("Primary signals: Long=%d, Short=%d\n",
            sum(dt_output$primary_signal == 1, na.rm = TRUE),
            sum(dt_output$primary_signal == -1, na.rm = TRUE)))
cat(sprintf("Meta-labels: TP=%d (%.1f%%), SL/TO=%d (%.1f%%)\n",
            sum(dt_output$meta_label == 1, na.rm = TRUE),
            mean(dt_output$meta_label == 1, na.rm = TRUE) * 100,
            sum(dt_output$meta_label == 0, na.rm = TRUE),
            mean(dt_output$meta_label == 0, na.rm = TRUE) * 100))
cat(sprintf("Effective sample size: %.0f\n", sum(dt_output$sample_weight, na.rm = TRUE)))
cat(sprintf("T-Statistic: %.2f\n", validation_results$tstat))
cat(sprintf("\nOutput: %s\n", output_file))
cat(sprintf("Finished: %s\n", Sys.time()))
cat("=============================================================================\n")
