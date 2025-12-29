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

# 1. Lade beide Scripts
source("r/01_triple_barrier_labeling.R")
source("r/01_alternative_labeling_methods.R")

# 2. Starte mit Triple Barrier
tic()
labeled <- create_triple_barrier_labels(
  prices = dt_small,
  atr_period = 14,
  atr_mult_barrier = 2,    # Symmetrisch!
  max_horizon_bars = 14,     # 3h
  session_start = 4,
  session_end = 21
)
toc()

# 3. Prüfe Label-Qualität
analyze_label_quality(labeled)
plot_label_distribution(labeled)

# 4. Berechne Sample Weights (überlappende Labels)
labeled_weighted <- calculate_sample_weights(labeled)
analyze_sample_weights(labeled_weighted)
plot_sample_weights(labeled_weighted)

# 5. Optimiere Label Parameter
opt_params <- optimize_triple_barrier_params(
  prices = dt_small,
  atr_periods = c(10, 14, 20),
  atr_mult_barriers = c(1, 1.5, 2),
  max_horizon_bars = c(8, 11, 14, 17), 
  session_start = 4,
  session_end = 21
)

print(opt_params)