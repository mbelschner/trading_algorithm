# =============================================================================
# META-LABELING PIPELINE FOR INTRADAY TRADING (OPTIMIZED)
# =============================================================================
# Lopez de Prado Meta-Labeling: Primary Signal + ML Target (TP/SL)
# Dynamic Barriers with Intraday Seasonality & Sample Uniqueness Weights
# =============================================================================

cat(sprintf("\n[%s] META-LABELING PIPELINE v2.1\n", Sys.time()))

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table, TTR, ggplot2, gridExtra,
  scales, viridis, progress, tictoc, parallel, zoo,
  tidyquant  # OHLC Candlestick charts
)

# ===== SOURCE MODULES ========================================================

source("r/01_01_meta_labeling_core.R")
source("r/01_02_sample_uniqueness.R")
source("r/01_03_statistical_validation.R")
source("r/01_04_labeling_visualizations.R")

# ===== CONFIGURATION =========================================================

CONFIG <- list(
  # Data
  epic = "GOLD",
  interval = "MINUTE_15",

  # Trading Session (UTC)
  session_start = 1,
  session_end = 22,

  # Session Volatility Multipliers
  session_vol_multipliers = list(
    asia = 0.8, london = 1.2, overlap = 1.4, ny = 1.1, default = 1.0
  ),

  # Primary Signal Strategy
  # Options: ema_cross, cmo_vhf_stc, adx_di, ichimoku, supertrend,
  #          macd_volume, ema_alignment, rsi_breakout, macd
  primary_signal_method = "cmo_vhf_stc",

  signal_params = list(
    ema_fast_col = "ema_fast", ema_slow_col = "ema_slow",
    rsi_overbought = 70, rsi_oversold = 30,
    vhf_threshold = 0.35, cmo_threshold = 20,
    stc_long_cross = 25, stc_short_cross = 75,
    adx_threshold = 25, volume_mult = 1.2,
    signal_validity_bars = 5
  ),

  # Indicator Periods
  ema_fast = 50, ema_slow = 200, rsi_period = 14, atr_period = 12,

  # Triple Barrier Settings
  atr_mult_tp = 2.5,
  atr_mult_sl = 2.0,
  max_horizon_bars = 16,
  min_barrier_distance = 0.00013 * 3 + 0.0001,
  neutral_threshold = 1.5,

  # Sample Weighting
  use_sample_weights = TRUE,
  weight_method = "uniqueness",
  min_sample_weight = 0.1,

  # Validation
  min_tstat = 2.0,
  min_samples = 100,

  # Output
  output_path = "labelled_data",
  cache_path = "labelled_data/cache"
)

# Create output directories
for (path in c(CONFIG$output_path, CONFIG$cache_path)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ===== LOAD DATA =============================================================

cat("\n=== LOADING DATA ===\n")
tic()

price_file <- file.path("price_data", paste0(CONFIG$epic, "_", CONFIG$interval, ".csv"))
stopifnot("Price file not found" = file.exists(price_file))

dt_prices <- fread(price_file)
setnames(dt_prices, tolower(names(dt_prices)))
if ("time" %in% names(dt_prices)) setnames(dt_prices, "time", "datetime")
dt_prices[, datetime := as.POSIXct(datetime)]
setorder(dt_prices, datetime)

cat(sprintf("Loaded: %s rows [%s to %s]\n",
            format(nrow(dt_prices), big.mark = ","),
            min(dt_prices$datetime), max(dt_prices$datetime)))
toc()

# ===== STEP 1: TECHNICAL INDICATORS ==========================================

cat("\n=== STEP 1: TECHNICAL INDICATORS ===\n")
tic()

dt_prices <- calculate_technical_indicators(
  dt_prices,
  atr_period = CONFIG$atr_period,
  ema_fast = CONFIG$ema_fast,
  ema_slow = CONFIG$ema_slow,
  rsi_period = CONFIG$rsi_period
)

cat(sprintf("ATR median: %.5f\n", median(dt_prices$atr, na.rm = TRUE)))
toc()

# ===== STEP 2: PRIMARY SIGNALS ===============================================

cat("\n=== STEP 2: PRIMARY SIGNALS ===\n")
cat(sprintf("Strategy: %s\n", CONFIG$primary_signal_method))
tic()

dt_prices <- generate_primary_signals(
  dt_prices,
  method = CONFIG$primary_signal_method,
  params = CONFIG$signal_params
)

signal_stats <- dt_prices[!is.na(primary_signal), .N, by = primary_signal]
cat("Signal Distribution:\n")
print(signal_stats)
toc()

# ===== STEP 3: TRIPLE BARRIER LABELING =======================================

cat("\n=== STEP 3: TRIPLE BARRIER LABELING ===\n")
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
print(dt_labeled[!is.na(meta_label), .N, by = meta_label])
toc()

# ===== STEP 4: SAMPLE UNIQUENESS =============================================

cat("\n=== STEP 4: SAMPLE UNIQUENESS ===\n")
tic()

dt_weighted <- calculate_sample_uniqueness(
  dt_labeled,
  method = CONFIG$weight_method,
  min_weight = CONFIG$min_sample_weight
)

cat(sprintf("Mean uniqueness: %.4f | Effective N: %.0f (%.1f%%)\n",
            mean(dt_weighted$sample_weight),
            sum(dt_weighted$sample_weight),
            sum(dt_weighted$sample_weight) / nrow(dt_weighted) * 100))
toc()

# ===== STEP 5: VALIDATION ====================================================

cat("\n=== STEP 5: VALIDATION ===\n")
tic()

validation_results <- validate_label_quality(
  dt_weighted,
  min_tstat = CONFIG$min_tstat,
  min_samples = CONFIG$min_samples
)

cat(sprintf("T-Stat: %.2f [%s] | E[R|Long]: %.4f%% | E[R|Short]: %.4f%%\n",
            validation_results$tstat,
            ifelse(validation_results$tstat >= CONFIG$min_tstat, "PASS", "FAIL"),
            validation_results$expected_return_long * 100,
            validation_results$expected_return_short * 100))
toc()

# ===== STEP 6: VISUALIZATIONS ================================================

cat("\n=== STEP 6: VISUALIZATIONS ===\n")
tic()

viz_path <- file.path(CONFIG$output_path, "visualizations")
if (!dir.exists(viz_path)) dir.create(viz_path, recursive = TRUE)

suppressMessages({
  plot_label_density_by_hour(dt_weighted, output_path = viz_path, show_plot = FALSE)
  plot_cumulative_edge(dt_weighted, spread = 0.00013, slippage_pips = 1.0,
                       output_path = viz_path, show_plot = FALSE)
  plot_barrier_profitability_heatmap(dt_weighted, output_path = viz_path, show_plot = FALSE)
  plot_sample_weight_distribution(dt_weighted, output_path = viz_path, show_plot = FALSE)
  plot_meta_label_performance(dt_weighted, output_path = viz_path, show_plot = FALSE)
  plot_price_with_signals(dt_weighted, n_weeks = 2, output_path = viz_path,
                          show_barriers = TRUE, show_plot = FALSE)
})

cat(sprintf("Saved to: %s\n", viz_path))
toc()

# ===== STEP 7: SAVE OUTPUT ===================================================

cat("\n=== STEP 7: SAVING ===\n")

output_file <- file.path(CONFIG$output_path,
                         sprintf("%s_%s_meta_labeled.csv", CONFIG$epic, CONFIG$interval))

# Define output columns
output_cols <- c(
  "datetime", "open", "high", "low", "close", "volume",
  "atr", "hour", "session", "in_session",
  "primary_signal", "meta_label", "barrier_touched", "bars_to_exit",
  "realized_return", "realized_return_adj",
  "tp_distance", "sl_distance", "sample_weight", "n_concurrent"
)
output_cols <- intersect(output_cols, names(dt_weighted))

fwrite(dt_weighted[, ..output_cols], output_file)
saveRDS(CONFIG, file.path(CONFIG$output_path, "labeling_config.rds"))

cat(sprintf("Output: %s (%s rows)\n", output_file, format(nrow(dt_weighted), big.mark = ",")))

# ===== SUMMARY ===============================================================

cat("\n=== COMPLETE ===\n")
cat(sprintf("Observations: %s | Long: %d | Short: %d\n",
            format(nrow(dt_weighted), big.mark = ","),
            sum(dt_weighted$primary_signal == 1, na.rm = TRUE),
            sum(dt_weighted$primary_signal == -1, na.rm = TRUE)))
cat(sprintf("Meta-Labels: TP=%d (%.1f%%) | SL/TO=%d (%.1f%%)\n",
            sum(dt_weighted$meta_label == 1, na.rm = TRUE),
            mean(dt_weighted$meta_label == 1, na.rm = TRUE) * 100,
            sum(dt_weighted$meta_label == 0, na.rm = TRUE),
            mean(dt_weighted$meta_label == 0, na.rm = TRUE) * 100))
cat(sprintf("T-Statistic: %.2f | Effective N: %.0f\n",
            validation_results$tstat,
            sum(dt_weighted$sample_weight, na.rm = TRUE)))
cat(sprintf("[%s] Done.\n", Sys.time()))
