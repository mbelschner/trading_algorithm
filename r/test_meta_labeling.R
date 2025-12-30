# =============================================================================
# TEST SCRIPT: META-LABELING MIT EXTREMA SIGNALS
# =============================================================================

cat("\n=== START META-LABELING TEST ===\n")

# ===== Set Up =================================================================

rm(list=ls())
gc()

# ===== Packages ===============================================================
pacman::p_load(data.table,
               TTR,
               ggplot2)

# ===== Paths ==================================================================

price_data_path = file.path("price_data")
output_path = file.path("labelled_data")

# Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"
prices_filename = file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))

# Lade Daten
dt = fread(prices_filename)

# Rename 'time' column to 'datetime' for consistency
setnames(dt, "time", "datetime")

# Fokus auf 2025 für schnellere Tests
dt_test = dt[datetime >= "2025-01-01" & datetime <= "2025-12-31"]

cat("\n=== DATENSATZ ===\n")
cat(sprintf("Zeilen: %s\n", format(nrow(dt_test), big.mark = ",")))
cat(sprintf("Zeitraum: %s bis %s\n\n", min(dt_test$datetime), max(dt_test$datetime)))

# ===== Lade Meta-Labeling Script ==============================================

source("r/02_meta_labeling_extrema_signals.R")


# =============================================================================
# TEST 1: BASIC META-LABELING MIT DEFAULT PARAMETERN
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 1: BASIC META-LABELING (Default Parameters)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

result_basic <- generate_meta_labeled_signals(
  prices = dt_test,
  # Extrema Detection
  lookback_bars = 5,
  confirmation_method = "bars",  # Einfachste Methode: Warte N Bars
  confirmation_bars = 2,
  use_rsi = TRUE,
  rsi_period = 14,
  rsi_oversold = 30,
  rsi_overbought = 70,
  # Signal Generation
  atr_period = 14,
  # Meta-Labeling
  atr_mult_profit = 2.0,   # 2x ATR Profit Target
  atr_mult_stop = 1.5,     # 1.5x ATR Stop Loss
  max_holding_bars = 20,
  use_stop_loss = TRUE
)

# Speichere Ergebnis
meta_labeled_basic <- result_basic$meta_labeled
fwrite(meta_labeled_basic, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_basic.csv")))
cat("\n✓ Gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_basic.csv")), "\n")


# =============================================================================
# TEST 2: DERIVATIVE-BASED CONFIRMATION
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 2: DERIVATIVE-BASED CONFIRMATION\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

result_derivative <- generate_meta_labeled_signals(
  prices = dt_test,
  lookback_bars = 5,
  confirmation_method = "derivative",  # Verwende Ableitungen für Confirmation
  confirmation_bars = 3,
  use_rsi = TRUE,
  rsi_period = 14,
  rsi_oversold = 30,
  rsi_overbought = 70,
  atr_period = 14,
  atr_mult_profit = 2.5,
  atr_mult_stop = 1.5,
  max_holding_bars = 20,
  use_stop_loss = TRUE
)

meta_labeled_deriv <- result_derivative$meta_labeled
fwrite(meta_labeled_deriv, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_derivative.csv")))
cat("\n✓ Gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_derivative.csv")), "\n")


# =============================================================================
# TEST 3: COMBINED CONFIRMATION (BARS + DERIVATIVE)
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 3: COMBINED CONFIRMATION (Bars + Derivative)\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

result_combined <- generate_meta_labeled_signals(
  prices = dt_test,
  lookback_bars = 7,
  confirmation_method = "both",  # Beide Bestätigungen erforderlich
  confirmation_bars = 2,
  use_rsi = TRUE,
  rsi_period = 14,
  rsi_oversold = 35,
  rsi_overbought = 65,
  atr_period = 14,
  atr_mult_profit = 2.0,
  atr_mult_stop = 1.5,
  max_holding_bars = 25,
  use_stop_loss = TRUE
)

meta_labeled_combined <- result_combined$meta_labeled
fwrite(meta_labeled_combined, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_combined.csv")))
cat("\n✓ Gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_combined.csv")), "\n")


# =============================================================================
# TEST 4: AGGRESSIVE SETTINGS (Mehr Signale, kürzere Holding)
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 4: AGGRESSIVE SETTINGS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

result_aggressive <- generate_meta_labeled_signals(
  prices = dt_test,
  lookback_bars = 3,          # Kürzeres Lookback
  confirmation_method = "bars",
  confirmation_bars = 1,       # Schnellere Confirmation
  use_rsi = FALSE,             # Kein RSI Filter
  atr_period = 10,
  atr_mult_profit = 1.5,       # Kleinere Targets
  atr_mult_stop = 1.0,
  max_holding_bars = 10,       # Kürzere Holding Period
  use_stop_loss = TRUE
)

meta_labeled_aggressive <- result_aggressive$meta_labeled
fwrite(meta_labeled_aggressive, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_aggressive.csv")))
cat("\n✓ Gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_aggressive.csv")), "\n")


# =============================================================================
# TEST 5: CONSERVATIVE SETTINGS (Weniger Signale, hohe Qualität)
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  TEST 5: CONSERVATIVE SETTINGS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

result_conservative <- generate_meta_labeled_signals(
  prices = dt_test,
  lookback_bars = 10,         # Längeres Lookback
  confirmation_method = "both",
  confirmation_bars = 3,       # Langsamere Confirmation
  use_rsi = TRUE,
  rsi_period = 14,
  rsi_oversold = 25,           # Strengere RSI-Filter
  rsi_overbought = 75,
  atr_period = 20,
  atr_mult_profit = 3.0,       # Größere Targets
  atr_mult_stop = 2.0,
  max_holding_bars = 30,
  use_stop_loss = TRUE
)

meta_labeled_conservative <- result_conservative$meta_labeled
fwrite(meta_labeled_conservative, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_conservative.csv")))
cat("\n✓ Gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeled_conservative.csv")), "\n")


# =============================================================================
# VERGLEICH DER VERSCHIEDENEN KONFIGURATIONEN
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  VERGLEICH ALLER KONFIGURATIONEN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

comparison <- data.table(
  Config = c("Basic", "Derivative", "Combined", "Aggressive", "Conservative"),
  Total_Signals = c(
    nrow(meta_labeled_basic),
    nrow(meta_labeled_deriv),
    nrow(meta_labeled_combined),
    nrow(meta_labeled_aggressive),
    nrow(meta_labeled_conservative)
  ),
  Success_Rate = c(
    mean(meta_labeled_basic$meta_label, na.rm = TRUE) * 100,
    mean(meta_labeled_deriv$meta_label, na.rm = TRUE) * 100,
    mean(meta_labeled_combined$meta_label, na.rm = TRUE) * 100,
    mean(meta_labeled_aggressive$meta_label, na.rm = TRUE) * 100,
    mean(meta_labeled_conservative$meta_label, na.rm = TRUE) * 100
  ),
  Avg_PnL_Pct = c(
    mean(meta_labeled_basic$realized_pnl, na.rm = TRUE) * 100,
    mean(meta_labeled_deriv$realized_pnl, na.rm = TRUE) * 100,
    mean(meta_labeled_combined$realized_pnl, na.rm = TRUE) * 100,
    mean(meta_labeled_aggressive$realized_pnl, na.rm = TRUE) * 100,
    mean(meta_labeled_conservative$realized_pnl, na.rm = TRUE) * 100
  ),
  Avg_Bars_Held = c(
    mean(meta_labeled_basic$bars_held, na.rm = TRUE),
    mean(meta_labeled_deriv$bars_held, na.rm = TRUE),
    mean(meta_labeled_combined$bars_held, na.rm = TRUE),
    mean(meta_labeled_aggressive$bars_held, na.rm = TRUE),
    mean(meta_labeled_conservative$bars_held, na.rm = TRUE)
  )
)

comparison[, Success_Rate := round(Success_Rate, 1)]
comparison[, Avg_PnL_Pct := round(Avg_PnL_Pct, 2)]
comparison[, Avg_Bars_Held := round(Avg_Bars_Held, 1)]

print(comparison)

# Speichere Vergleich
fwrite(comparison, file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeling_comparison.csv")))
cat("\n✓ Vergleich gespeichert:", file.path(output_path, paste0(EPIC, "_", INTERVAL, "_meta_labeling_comparison.csv")), "\n")


# =============================================================================
# VISUALISIERUNG: SIGNAL QUALITY ÜBER ZEIT
# =============================================================================

cat("\n=== ERSTELLE VISUALISIERUNGEN ===\n")

# Verwende Basic Config für Visualisierung
viz_data <- meta_labeled_basic
viz_data[, date := as.Date(datetime)]

# Daily aggregierte Signal-Qualität
daily_stats <- viz_data[, .(
  signals = .N,
  success_rate = mean(meta_label, na.rm = TRUE) * 100,
  avg_pnl = mean(realized_pnl, na.rm = TRUE) * 100
), by = date]

# Plot 1: Success Rate über Zeit
p1 <- ggplot(daily_stats, aes(x = date, y = success_rate)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray50") +
  labs(
    title = "Meta-Label Success Rate über Zeit",
    subtitle = paste(EPIC, INTERVAL, "- Basic Configuration"),
    x = "Datum",
    y = "Success Rate (%)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_path, paste0(EPIC, "_", INTERVAL, "_success_rate_over_time.png")),
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300
)

cat("✓ Plot gespeichert: success_rate_over_time.png\n")

# Plot 2: Signal Distribution by Side
p2 <- ggplot(viz_data, aes(x = factor(primary_signal), fill = factor(meta_label))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("0" = "firebrick", "1" = "forestgreen"),
    labels = c("Failed", "Success")
  ) +
  scale_x_discrete(labels = c("-1" = "Short", "1" = "Long")) +
  labs(
    title = "Meta-Label Distribution by Signal Side",
    x = "Primary Signal",
    y = "Proportion",
    fill = "Meta-Label"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_path, paste0(EPIC, "_", INTERVAL, "_metalabel_by_side.png")),
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

cat("✓ Plot gespeichert: metalabel_by_side.png\n")


cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("  META-LABELING TEST ABGESCHLOSSEN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("Alle Dateien gespeichert in:", output_path, "\n")
cat("\n=== END META-LABELING TEST ===\n")
