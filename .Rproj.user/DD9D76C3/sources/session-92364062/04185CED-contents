#Script for running labelling methods

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(data.table,
               TTR,
               ggplot2)

# 1. Lade beide Scripts
source("triple_barrier_labeling.R")
source("alternative_labeling_methods.R")

# 2. Starte mit Triple Barrier
labeled_gold <- create_triple_barrier_labels(
  prices = gold_15min,
  atr_period = 14,
  atr_mult_barrier = 1.5,    # Symmetrisch!
  max_horizon_bars = 24,     # 6h
  session_start = 8,
  session_end = 19
)

# 3. Prüfe Label-Qualität
analyze_label_quality(labeled_gold)
plot_label_distribution(labeled_gold)

# 4. Optional: Parameter optimieren
best_params <- optimize_labeling_parameters(gold_15min)