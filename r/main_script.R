#Script for running labelling methods

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(data.table,
               TTR,
               ggplot2)

# ===== Paths ==================================================================

price_data_path = file.path("price_data")
labelled_output_path = file.path("labelled_data")

#Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"
prices_filename = file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))

#Lade Daten
dt = fread(prices_filename)

# Wir sehen uns zuerst einmal nur 2023-2025 an um nicht zu große Dateien zu laden
dt_small = dt[time >= "2023-01-01" & time <= "2025-12-31"]

# 1. Lade beide Scripts
source("r/01_triple_barrier_labeling.R")
source("r/01_alternative_labeling_methods.R")

# 2. Starte mit Triple Barrier
labeled_gold <- create_triple_barrier_labels(
  prices = dt_small,
  atr_period = 14,
  atr_mult_barrier = 1.5,    # Symmetrisch!
  max_horizon_bars = 24,     # 6h
  session_start = 8,
  session_end = 19
)

# 3. Prüfe Label-Qualität
analyze_label_quality(labeled_gold)
plot_label_distribution(labeled_gold)

# 4.