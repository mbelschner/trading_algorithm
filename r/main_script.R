#Script for running labelling methods

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(data.table,
               TTR,
               ggplot2,
               progress,
               tictoc)

# ===== Paths ==================================================================

price_data_path = file.path("price_data")
labelled_output_path = file.path("labelled_data")

#Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"
prices_filename = file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))

#Lade Daten
dt = fread(prices_filename)

# Rename 'time' column to 'datetime' for consistency with labeling scripts
setnames(dt, "time", "datetime")

# Wir sehen uns zuerst einmal nur 2023-2025 an um nicht zu große Dateien zu laden
dt_small = dt[datetime >= "2025-01-01" & datetime <= "2025-12-31"]

cat("\n=== DATENSATZ GRÖSSE ===\n")
cat(sprintf("dt_small: %s Zeilen\n", format(nrow(dt_small), big.mark = ",")))
cat(sprintf("dt (vollständig): %s Zeilen\n", format(nrow(dt), big.mark = ",")))
cat(sprintf("Faktor: %.1fx\n\n", nrow(dt) / nrow(dt_small)))

# 1. Lade beide Scripts
# WICHTIG: Die optimierte Version wird automatisch geladen
source("r/01_triple_barrier_labeling_optimized.R")  # Optimierte Hauptfunktion
source("r/01_triple_barrier_labeling.R")            # Helper-Funktionen
source("r/01_alternative_labeling_methods.R")

# 2. Starte mit Triple Barrier (OPTIMIERTE VERSION)
tic()
labeled <- create_triple_barrier_labels(
  prices = dt,
  atr_period = 10,
  atr_mult_barrier = 1.5,
  max_horizon_bars = 8,
  session_start = 4,
  session_end = 21,
  neutral_threshold = 0.3    # 10% des ATR als Neutral-Schwelle
)
toc()

# 3. Prüfe Label-Qualität
analyze_label_quality(labeled)

# 3a. Teste verschiedene Neutral-Thresholds
neutral_threshold_results <- test_neutral_thresholds(
  labeled_data = labeled,
  thresholds = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)
)
print(neutral_threshold_results)

plot_label_distribution(labeled)

# 4. Berechne Sample Weights (überlappende Labels)
labeled_weighted <- calculate_sample_weights(labeled)
analyze_sample_weights(labeled_weighted)
plot_sample_weights(labeled_weighted)

# 5. Analysiere Holding Period für die aktuelle Konfiguration
analyze_holding_period(labeled, max_horizon = 14)

# 6. Teste den Impact des Horizons
horizon_test_results <- test_horizon_impact(
  prices = dt,
  atr_period = 14,
  atr_mult_barrier = 3,
  horizons = c(7, 10, 14, 17, 20),
  session_start = 4,
  session_end = 21
)
print(horizon_test_results)

# 7. Optimiere Label Parameter (umfassender Grid Search)
opt_params <- optimize_labeling_parameters(
   prices = dt_small,
   atr_periods = c(10, 14, 20),
   atr_mults = c(1.5, 2, 2.5, 3),
   max_horizons = c(8, 11, 14, 17),
   sort_by = "combined"  # Sortierung: "uniqueness" (default), "balance", oder "combined"
)

View(opt_params)

#
# # Zeige beste Kombination
cat("\n=== BESTE PARAMETER-KOMBINATION ===\n")
best <- opt_params[1]
cat(sprintf("ATR Period: %d\n", best$atr_period))
cat(sprintf("ATR Multiplier: %.1f\n", best$atr_mult))
cat(sprintf("Max Horizon: %d bars\n", best$max_horizon))
cat(sprintf("\nUniqueness: %.4f (höher = besser)\n", best$avg_uniqueness))
cat(sprintf("Balance Score: %.3f (niedriger = besser)\n", best$balance_score))
cat(sprintf("Mean Concurrent Labels: %.1f\n", best$mean_concurrent))
cat(sprintf("Samples: %d\n", best$n_samples))