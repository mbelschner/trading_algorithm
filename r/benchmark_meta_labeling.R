# =============================================================================
# PERFORMANCE BENCHMARK: Meta-Labeling Original vs. Optimized
# =============================================================================

library(data.table)
library(TTR)
library(tictoc)
library(ggplot2)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     META-LABELING PERFORMANCE BENCHMARK                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# =============================================================================
# Setup
# =============================================================================

price_data_path <- file.path("price_data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
prices_filename <- file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))

dt <- fread(prices_filename)
setnames(dt, "time", "datetime")

# Verschiedene Datensatzgrößen testen
test_sizes <- c(1000, 5000, 10000, 20000, 50000)

cat("=== TEST KONFIGURATION ===\n")
cat("Datensatz:", EPIC, INTERVAL, "\n")
cat("Verfügbare Zeilen:", nrow(dt), "\n")
cat("Test Größen:", paste(test_sizes, collapse = ", "), "\n\n")

# Parameter für Meta-Labeling
params <- list(
  lookback_bars = 5,
  confirmation_method = "bars",
  confirmation_bars = 2,
  use_rsi = FALSE,  # FALSE für schnelleren Benchmark
  rsi_period = 14,
  rsi_oversold = 30,
  rsi_overbought = 70,
  atr_period = 14,
  atr_mult_profit = 2.0,
  atr_mult_stop = 1.5,
  max_holding_bars = 20,
  use_stop_loss = TRUE
)

# =============================================================================
# Benchmark Function
# =============================================================================

run_benchmark <- function(size) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  BENCHMARK: %s Zeilen\n", format(size, big.mark = ",")))
  cat("═══════════════════════════════════════════════════════════════\n\n")

  # Erstelle Test-Subset
  test_data <- tail(dt, size)

  results <- list(
    size = size,
    original_time = NA,
    optimized_time = NA,
    optimized_parallel_time = NA,
    speedup = NA,
    speedup_parallel = NA
  )

  # -------------------------------------------------------------------------
  # Test 1: Original Version
  # -------------------------------------------------------------------------

  cat("Running ORIGINAL version...\n")

  # Lade Original Script
  source("r/02_meta_labeling_extrema_signals.R", local = TRUE)

  tic()
  result_original <- generate_meta_labeled_signals(
    prices = test_data,
    lookback_bars = params$lookback_bars,
    confirmation_method = params$confirmation_method,
    confirmation_bars = params$confirmation_bars,
    use_rsi = params$use_rsi,
    rsi_period = params$rsi_period,
    rsi_oversold = params$rsi_oversold,
    rsi_overbought = params$rsi_overbought,
    atr_period = params$atr_period,
    atr_mult_profit = params$atr_mult_profit,
    atr_mult_stop = params$atr_mult_stop,
    max_holding_bars = params$max_holding_bars,
    use_stop_loss = params$use_stop_loss
  )
  time_original <- toc(quiet = TRUE)
  results$original_time <- as.numeric(time_original$toc - time_original$tic)

  cat(sprintf("Original Runtime: %.2f seconds\n", results$original_time))

  # -------------------------------------------------------------------------
  # Test 2: Optimized Version (Sequential)
  # -------------------------------------------------------------------------

  cat("\nRunning OPTIMIZED version (sequential)...\n")

  # Lade Optimized Script
  source("r/02_meta_labeling_extrema_signals_optimized.R", local = TRUE)

  tic()
  result_optimized <- generate_meta_labeled_signals(
    prices = test_data,
    lookback_bars = params$lookback_bars,
    confirmation_method = params$confirmation_method,
    confirmation_bars = params$confirmation_bars,
    use_rsi = params$use_rsi,
    rsi_period = params$rsi_period,
    rsi_oversold = params$rsi_oversold,
    rsi_overbought = params$rsi_overbought,
    atr_period = params$atr_period,
    atr_mult_profit = params$atr_mult_profit,
    atr_mult_stop = params$atr_mult_stop,
    max_holding_bars = params$max_holding_bars,
    use_stop_loss = params$use_stop_loss,
    parallel = FALSE
  )
  time_optimized <- toc(quiet = TRUE)
  results$optimized_time <- as.numeric(time_optimized$toc - time_optimized$tic)

  cat(sprintf("Optimized Runtime: %.2f seconds\n", results$optimized_time))

  # -------------------------------------------------------------------------
  # Test 3: Optimized Version (Parallel) - nur bei größeren Datensätzen
  # -------------------------------------------------------------------------

  if(size >= 10000) {
    cat("\nRunning OPTIMIZED version (parallel, 4 cores)...\n")

    tic()
    result_parallel <- generate_meta_labeled_signals(
      prices = test_data,
      lookback_bars = params$lookback_bars,
      confirmation_method = params$confirmation_method,
      confirmation_bars = params$confirmation_bars,
      use_rsi = params$use_rsi,
      rsi_period = params$rsi_period,
      rsi_oversold = params$rsi_oversold,
      rsi_overbought = params$rsi_overbought,
      atr_period = params$atr_period,
      atr_mult_profit = params$atr_mult_profit,
      atr_mult_stop = params$atr_mult_stop,
      max_holding_bars = params$max_holding_bars,
      use_stop_loss = params$use_stop_loss,
      parallel = TRUE,
      n_cores = 4
    )
    time_parallel <- toc(quiet = TRUE)
    results$optimized_parallel_time <- as.numeric(time_parallel$toc - time_parallel$tic)

    cat(sprintf("Optimized Parallel Runtime: %.2f seconds\n", results$optimized_parallel_time))
  }

  # -------------------------------------------------------------------------
  # Berechne Speedup
  # -------------------------------------------------------------------------

  results$speedup <- results$original_time / results$optimized_time

  if(!is.na(results$optimized_parallel_time)) {
    results$speedup_parallel <- results$original_time / results$optimized_parallel_time
  }

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  SPEEDUP: %.2fx (Sequential)  |  %.2fx (Parallel)\n",
              results$speedup,
              ifelse(is.na(results$speedup_parallel), 0, results$speedup_parallel)))
  cat("═══════════════════════════════════════════════════════════════\n")

  return(results)
}

# =============================================================================
# Run Benchmarks
# =============================================================================

benchmark_results <- list()

for(size in test_sizes) {
  if(size <= nrow(dt)) {
    benchmark_results[[length(benchmark_results) + 1]] <- run_benchmark(size)
  }
}

# =============================================================================
# Zusammenfassung
# =============================================================================

cat("\n\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                   BENCHMARK ZUSAMMENFASSUNG                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

summary_dt <- rbindlist(benchmark_results)

cat("Performance-Tabelle:\n")
print(summary_dt[, .(
  Datensatzgroesse = format(size, big.mark = ","),
  Original = sprintf("%.2fs", original_time),
  Optimized = sprintf("%.2fs", optimized_time),
  `Speedup (Seq)` = sprintf("%.2fx", speedup),
  `Speedup (Par)` = ifelse(is.na(speedup_parallel), "N/A", sprintf("%.2fx", speedup_parallel))
)])

cat("\n\nDurchschnittlicher Speedup:\n")
cat(sprintf("  Sequential: %.2fx\n", mean(summary_dt$speedup, na.rm = TRUE)))
cat(sprintf("  Parallel:   %.2fx\n", mean(summary_dt$speedup_parallel, na.rm = TRUE)))

# =============================================================================
# Visualisierung
# =============================================================================

cat("\nErstelle Visualisierung...\n")

# Prepare data for plotting
plot_data <- summary_dt[, .(
  size = size,
  Original = original_time,
  `Optimized (Sequential)` = optimized_time,
  `Optimized (Parallel)` = optimized_parallel_time
)]

plot_data_long <- melt(plot_data, id.vars = "size",
                       variable.name = "Version",
                       value.name = "Runtime")

# Plot 1: Runtime Comparison
p1 <- ggplot(plot_data_long[!is.na(Runtime)], aes(x = size, y = Runtime, color = Version, group = Version)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Meta-Labeling Performance Vergleich",
    subtitle = "Runtime vs. Datensatzgröße",
    x = "Anzahl Zeilen",
    y = "Runtime (Sekunden)",
    color = "Version"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# Plot 2: Speedup
speedup_data <- summary_dt[, .(
  size = size,
  `Sequential` = speedup,
  `Parallel` = speedup_parallel
)]

speedup_data_long <- melt(speedup_data, id.vars = "size",
                          variable.name = "Type",
                          value.name = "Speedup")

p2 <- ggplot(speedup_data_long[!is.na(Speedup)], aes(x = size, y = Speedup, color = Type, group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Speedup Factor",
    subtitle = "Optimized vs. Original",
    x = "Anzahl Zeilen",
    y = "Speedup (x-fach)",
    color = "Parallelisierung"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

# Save plots
ggsave("labelled_data/benchmark_runtime_comparison.png", p1, width = 10, height = 6, dpi = 300)
ggsave("labelled_data/benchmark_speedup.png", p2, width = 10, height = 6, dpi = 300)

cat("Plots gespeichert:\n")
cat("  - labelled_data/benchmark_runtime_comparison.png\n")
cat("  - labelled_data/benchmark_speedup.png\n")

# Save results
fwrite(summary_dt, "labelled_data/benchmark_results.csv")
cat("  - labelled_data/benchmark_results.csv\n")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                   BENCHMARK ABGESCHLOSSEN                      ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")
