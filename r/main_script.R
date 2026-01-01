#Script for running labelling methods

cat("\n=== START LABEL ANALYSIS ===\n")

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
  session_start = 2,
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
#horizon_test_results <- test_horizon_impact(
#  prices = dt,
#  atr_period = 14,
#  atr_mult_barrier = 3,
#  horizons = c(7, 10, 14, 17, 20),
#  session_start = 2,
#  session_end = 21
#)
#print(horizon_test_results)

# 7. Optimiere Label Parameter (umfassender Grid Search)
opt_params <- optimize_labeling_parameters(
   prices = dt,
   atr_periods = c(10, 12),
   atr_mults = c(1.5, 2),
   max_horizons = c(8, 11),
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

# ====== Erstelle Labels mit den besten Parametern =============================
cat("\n=== ERSTELLE LABELS MIT BESTEN PARAMETERN ===\n")

# Erstelle Labels mit den besten Parametern
labeled_optimized <- create_triple_barrier_labels(
  prices = dt,
  atr_period = best$atr_period,
  atr_mult_barrier = best$atr_mult,
  max_horizon_bars = best$max_horizon,
  session_start = 2,
  session_end = 21,
  neutral_threshold = 0.3    # 10% des ATR als Neutral-Schwelle
)

# Prüfe Label-Qualität
analyze_label_quality(labeled_optimized)

# Berechne Sample Weights (überlappende Labels)
labeled_weighted <- calculate_sample_weights(labeled_optimized)
analyze_sample_weights(labeled_weighted)

# Speichere die gelabelten Daten (Triple Barrier)
output_filename = file.path(labelled_output_path, paste0(EPIC, "_", INTERVAL, "_labeled.csv"))
fwrite(labeled_weighted, output_filename)
cat(sprintf("\nTriple Barrier Labels gespeichert in: %s\n", output_filename))


# =============================================================================
# EXTREMA-BASIERTE META-LABELING
# =============================================================================

cat("\n=== START META-LABELING (EXTREMA SIGNALS) ===\n")

# Lade Meta-Labeling Script (OPTIMIERTE VERSION)
source("r/02_meta_labeling_extrema_signals_optimized.R")

# Generiere Meta-Labels basierend auf Extrema
meta_result <- generate_meta_labeled_signals(
  prices = dt,

  # Extrema Detection
  lookback_bars = 5,
  confirmation_method = "bars",      # "bars", "derivative", "both"
  confirmation_bars = 2,
  use_rsi = TRUE,
  rsi_period = 14,
  rsi_oversold = 35,
  rsi_overbought = 65,

  # Signal Generation (verwendet gleiche ATR wie Triple Barrier)
  atr_period = best$atr_period,

  # Meta-Labeling
  atr_mult_profit = 2.0,            # 2x ATR Profit Target
  atr_mult_stop = 1.5,              # 1.5x ATR Stop Loss
  max_holding_bars = 20,
  use_stop_loss = TRUE
)

# Extrahiere Ergebnisse
meta_labeled <- meta_result$meta_labeled
full_data_with_signals <- meta_result$full_data

View(full_data_with_signals)

# Speichere Meta-Labels
meta_output_filename <- file.path(
  labelled_output_path,
  paste0(EPIC, "_", INTERVAL, "_meta_labeled.csv")
)
fwrite(meta_labeled, meta_output_filename)
cat(sprintf("\nMeta-Labels gespeichert in: %s\n", meta_output_filename))

# Optional: Speichere auch Full Data (alle Bars mit Extrema-Markierungen)
full_output_filename <- file.path(
  labelled_output_path,
  paste0(EPIC, "_", INTERVAL, "_with_extrema.csv")
)
fwrite(full_data_with_signals, full_output_filename)
cat(sprintf("Full Data mit Extrema gespeichert in: %s\n",
            full_output_filename))

# Detaillierte Analyse der Meta-Labels
source("r/analyze_extrema_meta_labels.R")

cat("\n=== ANALYSIERE EXTREMA META-LABELS ===\n")

# Führe Analyse durch
extrema_stats <- analyze_extrema_performance(
  meta_labeled = meta_labeled,
  triple_barrier = labeled_weighted
)

# Erstelle Visualisierungen
plot_extrema_analysis(
  meta_labeled = meta_labeled,
  output_path = labelled_output_path
)

cat("\n=== END META-LABELING ===\n")


# =============================================================================
# KOMBINIERE BEIDE LABEL-SETS (EINFACH)
# =============================================================================

cat("\n=== KOMBINIERE TRIPLE BARRIER + META-LABELS ===\n")

# Merge: Füge Meta-Label Spalten zu Triple Barrier Dataset hinzu
# Triple Barrier hat alle Bars, Meta-Labels nur Signal-Bars

# Prefix für Meta-Label Spalten
meta_cols <- c("primary_signal", "meta_label", "exit_reason", "bars_held",
               "realized_pnl", "profit_target", "stop_loss")

# Erstelle Meta-Dataset mit Prefix
meta_for_merge <- meta_labeled[, c("datetime", meta_cols), with = FALSE]
setnames(meta_for_merge, meta_cols, paste0("meta_", meta_cols))

# Merge: Left Join (alle Triple Barrier Bars bleiben)
combined_labels <- merge(
  labeled_weighted,
  meta_for_merge,
  by = "datetime",
  all.x = TRUE
)

cat(sprintf("Kombinierte Daten: %s Zeilen\n", format(nrow(combined_labels), big.mark = ",")))
cat(sprintf("Bars mit Meta-Signal: %s\n", sum(!is.na(combined_labels$meta_primary_signal))))

# Speichere kombinierte Labels
combined_output <- file.path(
  labelled_output_path,
  paste0(EPIC, "_", INTERVAL, "_combined_labels.csv")
)
fwrite(combined_labels, combined_output)
cat(sprintf("Kombinierte Labels gespeichert: %s\n", combined_output))

cat("\n=== END LABEL COMBINATION ===\n")
cat("\n=== END LABEL ANALYSIS ===\n")