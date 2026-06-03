# =============================================================================
# META-LABELING PIPELINE - STRATEGY COMPARISON
# =============================================================================
#
# Testet alle Primary-Signal-Strategien mit identischen Barrier-Parametern
# und sammelt die Ergebnisse in einem Vergleichs-DataFrame.
#
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("META-LABELING PIPELINE v3.0 - STRATEGY COMPARISON\n")
cat("=============================================================================\n")
cat(sprintf("Started: %s\n", Sys.time()))

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table, TTR, ggplot2, gridExtra,
  scales, viridis, progress, tictoc, parallel, zoo,
  tidyquant
)

# ===== SOURCE MODULES ========================================================

source("r/01_01_meta_labeling_core.R")
source("r/01_02_sample_uniqueness.R")
source("r/01_03_statistical_validation.R")
source("r/01_04_labeling_visualizations.R")

shift <- data.table::shift

# ===== CONFIGURATION =========================================================

CONFIG <- list(
  epic = "GOLD",
  interval = "MINUTE_15",

  session_start = 1,
  session_end = 22,
  session_vol_multipliers = list(
    asia = 0.8, london = 1.2, overlap = 1.4, ny = 1.1, default = 1.0
  ),

  signal_params = list(
    ema_fast_col = "ema_fast",
    ema_slow_col = "ema_slow",
    rsi_overbought = 70,
    rsi_oversold = 30,
    vhf_threshold = 0.2,
    cmo_threshold = 20,
    stc_long_cross = 30,
    stc_short_cross = 70,
    adx_threshold = 25,
    volume_mult = 1.2,
    signal_validity_bars = 4
  ),

  ema_fast = 50,
  ema_slow = 200,
  rsi_period = 14,
  atr_period = 12,

  atr_mult_tp = 3,
  atr_mult_sl = 2,
  max_horizon_bars = 20,
  min_barrier_distance = 0.00013 * 3 + 0.0001,
  neutral_threshold = 1.5,

  weight_method = "decay",
  min_sample_weight = 0.1,

  min_tstat = 2.0,
  min_samples = 100,

  output_path = "labelled_data",
  cache_path = "labelled_data/cache"
)

for (path in c(CONFIG$output_path, CONFIG$cache_path)) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# ===== LOAD DATA =============================================================

cat("\n=== LOADING DATA ===\n")
tic()

price_file <- file.path("price_data", paste0(CONFIG$epic, "_", CONFIG$interval, ".csv"))
if (!file.exists(price_file)) stop(sprintf("Price file not found: %s", price_file))

dt_raw <- fread(price_file)
setnames(dt_raw, tolower(names(dt_raw)))
if ("time" %in% names(dt_raw) && !"datetime" %in% names(dt_raw)) {
  setnames(dt_raw, "time", "datetime")
}
dt_raw[, datetime := as.POSIXct(datetime)]
setorder(dt_raw, datetime)

cat(sprintf("Loaded: %s rows from %s to %s\n",
            format(nrow(dt_raw), big.mark = ","),
            min(dt_raw$datetime), max(dt_raw$datetime)))
toc()

# ===== STEP 1: TECHNICAL INDICATORS (einmalig) ==============================

cat("\n=== STEP 1: TECHNICAL INDICATORS ===\n")
tic()

dt_base <- calculate_technical_indicators(
  dt_raw,
  atr_period = CONFIG$atr_period,
  ema_fast = CONFIG$ema_fast,
  ema_slow = CONFIG$ema_slow,
  rsi_period = CONFIG$rsi_period
)

cat(sprintf("Indicators calculated. ATR median: %.5f\n",
            median(dt_base$atr, na.rm = TRUE)))
toc()

# ===== STEP 2: STRATEGY COMPARISON LOOP =====================================

strategies <- names(list_primary_signal_strategies())

cat("\n=== STEP 2: TESTING ALL PRIMARY SIGNAL STRATEGIES ===\n")
cat(sprintf("Strategies to test: %d\n", length(strategies)))
cat(sprintf("Strategies: %s\n\n", paste(strategies, collapse = ", ")))

# Ergebnis-Container
results_list <- list()

for (strat in strategies) {

  cat(sprintf("\n--- [%s] ---\n", toupper(strat)))

  tryCatch({
    tic_result <- system.time({

      # 2a) Signale generieren (auf Kopie arbeiten)
      dt_signals <- copy(dt_base)
      invisible(capture.output(
        dt_signals <- generate_primary_signals(
          dt_signals,
          method = strat,
          params = CONFIG$signal_params
        )
      ))

      n_long <- sum(dt_signals$primary_signal == 1, na.rm = TRUE)
      n_short <- sum(dt_signals$primary_signal == -1, na.rm = TRUE)
      n_signals_total <- n_long + n_short

      if (n_signals_total < 10) {
        cat(sprintf("  SKIP: Nur %d Signale (< 10)\n", n_signals_total))
        next
      }

      cat(sprintf("  Signals: %d Long, %d Short\n", n_long, n_short))

      # 2b) Triple Barrier
      invisible(capture.output(
        dt_labeled <- apply_dynamic_triple_barrier(
          dt_signals,
          atr_mult_tp = CONFIG$atr_mult_tp,
          atr_mult_sl = CONFIG$atr_mult_sl,
          max_horizon = CONFIG$max_horizon_bars,
          session_start = CONFIG$session_start,
          session_end = CONFIG$session_end,
          session_vol_multipliers = CONFIG$session_vol_multipliers,
          min_barrier_distance = CONFIG$min_barrier_distance,
          neutral_threshold = CONFIG$neutral_threshold
        )
      ))

      if (nrow(dt_labeled) < 20) {
        cat(sprintf("  SKIP: Nur %d Labels (< 20)\n", nrow(dt_labeled)))
        next
      }

      # 2c) Sample Uniqueness
      invisible(capture.output(
        dt_weighted <- calculate_sample_uniqueness(
          dt_labeled,
          method = CONFIG$weight_method,
          min_weight = CONFIG$min_sample_weight
        )
      ))

      # 2d) Validation
      invisible(capture.output(
        val <- validate_label_quality(
          dt_weighted,
          min_tstat = CONFIG$min_tstat,
          min_samples = CONFIG$min_samples
        )
      ))

      # Metriken berechnen
      n_samples <- nrow(dt_weighted)
      win_rate <- val$win_rate
      tstat <- val$tstat
      mean_return <- val$mean_return
      mean_uniqueness <- mean(dt_weighted$sample_weight, na.rm = TRUE)
      effective_n <- sum(dt_weighted$sample_weight, na.rm = TRUE)
      e_return_long <- val$expected_return_long
      e_return_short <- val$expected_return_short
      avg_win <- val$avg_win
      avg_loss <- val$avg_loss
      info_ratio <- val$information_ratio
      fh_corr <- val$fixed_horizon_correlation

      # Barrier-Verteilung
      barrier_tab <- dt_weighted[, .N, by = barrier_touched]
      n_tp <- barrier_tab[grepl("take_profit", barrier_touched), sum(N)]
      n_sl <- barrier_tab[grepl("stop_loss", barrier_touched), sum(N)]
      n_timeout <- barrier_tab[grepl("timeout", barrier_touched), sum(N)]

      # Avg Holding Period
      avg_bars <- mean(dt_weighted$bars_to_exit, na.rm = TRUE)

      # Ergebnis-Zeile
      results_list[[strat]] <- data.table(
        strategy         = strat,
        n_signals        = n_signals_total,
        n_long           = n_long,
        n_short          = n_short,
        n_samples        = n_samples,
        n_tp             = n_tp,
        n_sl             = n_sl,
        n_timeout        = n_timeout,
        win_rate_pct     = round(win_rate * 100, 2),
        t_statistic      = round(tstat, 3),
        mean_return_pct  = round(mean_return * 100, 4),
        e_return_long    = round(e_return_long * 100, 4),
        e_return_short   = round(e_return_short * 100, 4),
        avg_win_pct      = round(avg_win * 100, 4),
        avg_loss_pct     = round(avg_loss * 100, 4),
        info_ratio       = round(info_ratio, 4),
        fh_correlation   = round(fh_corr, 4),
        mean_uniqueness  = round(mean_uniqueness, 4),
        effective_n      = round(effective_n, 1),
        avg_holding_bars = round(avg_bars, 1),
        is_valid         = val$is_valid
      )

      cat(sprintf("  Samples: %d | Win Rate: %.1f%% | T-Stat: %.2f | IR: %.4f\n",
                  n_samples, win_rate * 100, tstat, info_ratio))

    })

    cat(sprintf("  Time: %.1fs\n", tic_result["elapsed"]))

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

# ===== STEP 3: ERGEBNIS-TABELLE ==============================================

cat("\n")
cat("=============================================================================\n")
cat("STRATEGY COMPARISON RESULTS\n")
cat("=============================================================================\n\n")

dt_results <- rbindlist(results_list)
setorder(dt_results, -t_statistic)

# Kompakte Ansicht
print(dt_results[, .(
  strategy, n_samples, win_rate_pct, t_statistic, mean_return_pct,
  info_ratio, mean_uniqueness, effective_n, is_valid
)])

# Ergebnisse speichern
results_file <- file.path(
  CONFIG$output_path,
  sprintf("%s_%s_strategy_comparison.csv", CONFIG$epic, CONFIG$interval)
)
fwrite(dt_results, results_file)
cat(sprintf("\nResults saved: %s\n", results_file))

# ===== STEP 4: BESTE STRATEGIE AUSWÄHLEN UND VISUALISIEREN ==================

# Beste valide Strategie (nach T-Statistic)
best_valid <- dt_results[is_valid == TRUE]
if (nrow(best_valid) > 0) {
  best_strat <- best_valid[1, strategy]
} else {
  # Fallback: beste nach T-Stat auch wenn nicht valid
  best_strat <- dt_results[1, strategy]
  cat("\nWARNING: Keine Strategie erfuellt alle Validierungskriterien!\n")
  cat("Verwende beste nach T-Statistic als Fallback.\n")
}

cat(sprintf("\n=== BEST STRATEGY: %s ===\n", toupper(best_strat)))
print(dt_results[strategy == best_strat])

# Pipeline nochmal fuer die beste Strategie (fuer Visualisierung + Export)
cat(sprintf("\n=== RUNNING FULL PIPELINE FOR '%s' ===\n", best_strat))

dt_best <- copy(dt_base)
dt_best <- generate_primary_signals(dt_best, method = best_strat, params = CONFIG$signal_params)

dt_best_labeled <- apply_dynamic_triple_barrier(
  dt_best,
  atr_mult_tp = CONFIG$atr_mult_tp,
  atr_mult_sl = CONFIG$atr_mult_sl,
  max_horizon = CONFIG$max_horizon_bars,
  session_start = CONFIG$session_start,
  session_end = CONFIG$session_end,
  session_vol_multipliers = CONFIG$session_vol_multipliers,
  min_barrier_distance = CONFIG$min_barrier_distance,
  neutral_threshold = CONFIG$neutral_threshold
)

dt_best_weighted <- calculate_sample_uniqueness(
  dt_best_labeled,
  method = CONFIG$weight_method,
  min_weight = CONFIG$min_sample_weight
)

validation_results <- validate_label_quality(
  dt_best_weighted,
  min_tstat = CONFIG$min_tstat,
  min_samples = CONFIG$min_samples
)

# ===== STEP 5: VISUALIZATIONS (nur fuer beste Strategie) ====================

cat("\n=== STEP 5: VISUALIZATIONS ===\n")
tic()

viz_path <- file.path(CONFIG$output_path, "visualizations")
if (!dir.exists(viz_path)) dir.create(viz_path, recursive = TRUE)

suppressWarnings({
  plot_label_density_by_hour(dt_best_weighted, output_path = viz_path, show_plot = FALSE)
  plot_cumulative_edge(dt_best_weighted, spread = 0.00013, slippage_pips = 1.0,
                       output_path = viz_path, show_plot = FALSE)
  plot_barrier_profitability_heatmap(dt_best_weighted, output_path = viz_path, show_plot = FALSE)
  plot_sample_weight_distribution(dt_best_weighted, output_path = viz_path, show_plot = FALSE)
  plot_meta_label_performance(dt_best_weighted, output_path = viz_path, show_plot = FALSE)
  plot_price_with_signals(dt_best_weighted, n_weeks = 2, output_path = viz_path,
                          show_barriers = TRUE, show_plot = FALSE)
})

cat(sprintf("Visualizations saved to: %s\n", viz_path))
toc()

# ===== STEP 6: SAVE LABELED DATA ============================================

cat("\n=== STEP 6: SAVING LABELED DATA ===\n")

output_file <- file.path(
  CONFIG$output_path,
  sprintf("%s_%s_meta_labeled.csv", CONFIG$epic, CONFIG$interval)
)

output_cols <- c(
  "datetime", "open", "high", "low", "close", "volume",
  "atr", "hour", "session", "in_session",
  "primary_signal", "meta_label", "barrier_touched", "bars_to_exit",
  "realized_return", "realized_return_adj",
  "tp_distance", "sl_distance", "sample_weight", "n_concurrent"
)
output_cols <- intersect(output_cols, names(dt_best_weighted))
dt_output <- dt_best_weighted[, ..output_cols]

fwrite(dt_output, output_file)
cat(sprintf("Saved: %s (%s rows)\n", output_file, format(nrow(dt_output), big.mark = ",")))

config_file <- file.path(CONFIG$output_path, "labeling_config.rds")
CONFIG$best_strategy <- best_strat
saveRDS(CONFIG, config_file)
cat(sprintf("Config saved: %s\n", config_file))

# ===== SUMMARY ===============================================================

cat("\n")
cat("=============================================================================\n")
cat("PIPELINE COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("Strategies tested: %d\n", nrow(dt_results)))
cat(sprintf("Valid strategies:  %d\n", sum(dt_results$is_valid)))
cat(sprintf("Best strategy:     %s\n", best_strat))
cat(sprintf("  Samples:         %d\n", dt_results[strategy == best_strat, n_samples]))
cat(sprintf("  Win Rate:        %.1f%%\n", dt_results[strategy == best_strat, win_rate_pct]))
cat(sprintf("  T-Statistic:     %.2f\n", dt_results[strategy == best_strat, t_statistic]))
cat(sprintf("  Info Ratio:      %.4f\n", dt_results[strategy == best_strat, info_ratio]))
cat(sprintf("  Uniqueness:      %.4f\n", dt_results[strategy == best_strat, mean_uniqueness]))
cat(sprintf("\nOutput: %s\n", output_file))
cat(sprintf("Comparison: %s\n", results_file))
cat(sprintf("Finished: %s\n", Sys.time()))
cat("=============================================================================\n")
