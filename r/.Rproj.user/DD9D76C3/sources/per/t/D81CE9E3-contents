# ==============================================================================
# FINANCIAL TIME SERIES LABELING: Triple Barrier Method (Directional)
# ==============================================================================

rm(list=ls())
gc()
options(scipen=999)

# Pakete laden -----------------------------------------------------------------

pacman::p_load(tidyverse,
               data.table,
               TTR,
               tictoc)

# Daten laden ------------------------------------------------------------------
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")

EPIC = "GOLD"
INTERVAL = "MINUTE_15"

filename = paste0(EPIC, "_", INTERVAL, ".csv")
directional_name = paste0("directional_labelled_", filename)
name_aggressive_labelling = paste0("aggressive_strat_", filename)

# Daten laden und vorbereiten
df_raw <- read_csv(file.path(input_path, filename)) %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M:%S")) %>%
  select(time, open, high, low, close, volume) %>%
  arrange(time) %>%
  # Log-Transformation für alle Preise
  mutate(
    log_open = log(open),
    log_high = log(high),
    log_low = log(low),
    log_close = log(close)
  )

# Für Test: nur 3 Monate
df_test = df_raw #%>%
  #filter(time >= as.Date("2025-07-01") & time <= as.Date("2025-10-30"))

# ==============================================================================
# 1. SYMMETRISCHE TRIPLE BARRIER METHOD (DIRECTIONAL)
# ==============================================================================
# Diese Methode erzeugt 3 Labels:
#   1  = Long ist profitabel (TP vor SL erreicht)
#  -1  = Short ist profitabel (TP vor SL erreicht)
#   0  = Beide unprofitabel (SL vor TP erreicht oder Time Barrier)
#
# Bei beiden profitabel: Die SCHNELLERE Richtung gewinnt
# ==============================================================================

create_directional_labels <- function(prices,
                                      risk_reward_ratio = 1.8,
                                      atr_period = 14,
                                      atr_mult_sl = 2.5,
                                      max_horizon = 24) {
  
  # Data.table für Performance
  dt <- data.table(prices)
  
  # ATR berechnen (Average True Range für dynamische Barrieren)
  dt[, atr := ATR(cbind(high, low, close), n = atr_period)[, "atr"]]
  
  # Log-Preis für bessere statistische Eigenschaften
  dt[, log_price := log(close)]
  
  # Take-Profit Multiplikator basierend auf Risk-Reward Ratio
  atr_mult_tp <- atr_mult_sl * risk_reward_ratio
  
  # Label-Spalten initialisieren
  dt[, label := NA_integer_]
  dt[, exit_reason := NA_character_]
  
  # Für jeden möglichen Entry-Punkt
  for(i in 1:(nrow(dt) - max_horizon)) {
    
    if(is.na(dt$atr[i])) next
    
    entry_price <- dt$log_price[i]
    atr_val <- dt$atr[i]
    price_val <- dt$close[i]
    
    # LONG Barrieren (symmetrisch)
    # TP: Preis steigt um atr_mult_tp * ATR
    # SL: Preis fällt um atr_mult_sl * ATR
    long_tp <- entry_price + (atr_mult_tp * atr_val / price_val)
    long_sl <- entry_price - (atr_mult_sl * atr_val / price_val)
    
    # SHORT Barrieren (symmetrisch)
    # TP: Preis fällt um atr_mult_tp * ATR
    # SL: Preis steigt um atr_mult_sl * ATR
    short_tp <- entry_price - (atr_mult_tp * atr_val / price_val)
    short_sl <- entry_price + (atr_mult_sl * atr_val / price_val)
    
    # Zukünftige Preise betrachten (bis max_horizon)
    future_idx <- (i+1):min(i + max_horizon, nrow(dt))
    future_log_prices <- dt$log_price[future_idx]
    
    # ==================================================
    # LONG: Prüfe ob TP vor SL erreicht wird
    # ==================================================
    long_tp_touch <- which(future_log_prices >= long_tp)
    long_sl_touch <- which(future_log_prices <= long_sl)
    
    long_profitable <- FALSE
    long_bars <- Inf
    
    if(length(long_tp_touch) > 0 & length(long_sl_touch) > 0) {
      # Beide Barrieren werden berührt - welche zuerst?
      if(long_tp_touch[1] < long_sl_touch[1]) {
        long_profitable <- TRUE
        long_bars <- long_tp_touch[1]
      }
    } else if(length(long_tp_touch) > 0) {
      # Nur TP berührt, kein SL
      long_profitable <- TRUE
      long_bars <- long_tp_touch[1]
    }
    
    # ==================================================
    # SHORT: Prüfe ob TP vor SL erreicht wird
    # ==================================================
    short_tp_touch <- which(future_log_prices <= short_tp)
    short_sl_touch <- which(future_log_prices >= short_sl)
    
    short_profitable <- FALSE
    short_bars <- Inf
    
    if(length(short_tp_touch) > 0 & length(short_sl_touch) > 0) {
      # Beide Barrieren werden berührt - welche zuerst?
      if(short_tp_touch[1] < short_sl_touch[1]) {
        short_profitable <- TRUE
        short_bars <- short_tp_touch[1]
      }
    } else if(length(short_tp_touch) > 0) {
      # Nur TP berührt, kein SL
      short_profitable <- TRUE
      short_bars <- short_tp_touch[1]
    }
    
    # ==================================================
    # LABEL ASSIGNMENT LOGIK
    # ==================================================
    if(long_profitable & !short_profitable) {
      # Nur Long profitabel
      dt$label[i] <- 1
      dt$exit_reason[i] <- "Long_Only"
      
    } else if(!long_profitable & short_profitable) {
      # Nur Short profitabel
      dt$label[i] <- -1
      dt$exit_reason[i] <- "Short_Only"
      
    } else if(long_profitable & short_profitable) {
      # BEIDE profitabel - wähle die SCHNELLERE Richtung
      if(long_bars < short_bars) {
        dt$label[i] <- 1
        dt$exit_reason[i] <- "Long_Faster"
      } else {
        dt$label[i] <- -1
        dt$exit_reason[i] <- "Short_Faster"
      }
      
    } else {
      # BEIDE unprofitabel
      dt$label[i] <- 0
      dt$exit_reason[i] <- "No_Profit"
    }
  }
  
  # Nur vollständig gelabelte Daten zurückgeben
  return(dt[!is.na(label)])
}

# ==============================================================================
# 2. LABELING DURCHFÜHREN
# ==============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  STARTE DIRECTIONAL LABELING\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

tic()
labelled_dt <- create_directional_labels(df_test)
toc()

cat("\nLabeling abgeschlossen!\n")
cat("Anzahl gelabelter Observations:", nrow(labelled_dt), "\n")

# ==============================================================================
# 3. DETAILLIERTE STATISTIKEN
# ==============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  LABEL VERTEILUNG\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Absolute Zahlen
label_counts <- table(labelled_dt$label)
cat("Absolute Zahlen:\n")
print(label_counts)

cat("\n\nProzentuale Verteilung:\n")
label_props <- prop.table(label_counts) * 100
print(round(label_props, 2))

# Exit Reasons
cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  EXIT REASONS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

exit_counts <- table(labelled_dt$exit_reason)
cat("Absolute Zahlen:\n")
print(exit_counts)

cat("\n\nProzentuale Verteilung:\n")
exit_props <- prop.table(exit_counts) * 100
print(round(exit_props, 2))

# Zusammenfassung
cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  ZUSAMMENFASSUNG\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

summary_stats <- data.frame(
  Metric = c(
    "Total Observations",
    "Long Profitable (%)",
    "Short Profitable (%)",
    "Unprofitable (%)",
    "Long-Short Bias (%)"
  ),
  Value = c(
    nrow(labelled_dt),
    round(sum(labelled_dt$label == 1) / nrow(labelled_dt) * 100, 2),
    round(sum(labelled_dt$label == -1) / nrow(labelled_dt) * 100, 2),
    round(sum(labelled_dt$label == 0) / nrow(labelled_dt) * 100, 2),
    round((sum(labelled_dt$label == 1) - sum(labelled_dt$label == -1)) / 
            nrow(labelled_dt) * 100, 2)
  )
)

print(summary_stats)

# ==============================================================================
# 4. WOCHEN-SPEZIFISCHE ANALYSE
# ==============================================================================

# Zwei Beispiel-Wochen definieren
week1_start <- as.POSIXct("2025-08-11 00:00:00")
week1_end <- as.POSIXct("2025-08-15 23:59:59")

week2_start <- as.POSIXct("2025-09-08 00:00:00")
week2_end <- as.POSIXct("2025-09-12 23:59:59")

# Funktion zur Wochen-Analyse
analyze_week <- function(dt, start_date, end_date, week_name) {
  
  week_data <- dt[time >= start_date & time <= end_date]
  
  cat("\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  cat(paste0(week_name, ": ", format(start_date, "%d.%m.%Y"), 
             " bis ", format(end_date, "%d.%m.%Y"), "\n"))
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  cat("Anzahl Observations:", nrow(week_data), "\n\n")
  
  cat("Label Verteilung:\n")
  print(table(week_data$label))
  
  cat("\nProzentual:\n")
  print(round(prop.table(table(week_data$label)) * 100, 2))
  
  return(week_data)
}

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  WOCHEN-SPEZIFISCHE ANALYSE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

week1_data <- analyze_week(labelled_dt, week1_start, week1_end, "Woche 1")
week2_data <- analyze_week(labelled_dt, week2_start, week2_end, "Woche 2")

# ==============================================================================
# 5. VISUALISIERUNGEN
# ==============================================================================

library(ggplot2)
library(gridExtra)
library(grid)

# Funktion: Preis + Labels visualisieren
plot_labels_on_price <- function(dt, week_start, week_end, week_name) {
  
  week_data <- dt[time >= week_start & time <= week_end]
  
  p <- ggplot(week_data, aes(x = time)) +
    # Preis-Linie
    geom_line(aes(y = close), color = "gray30", size = 0.5, alpha = 0.7) +
    
    # Long Labels (grün)
    geom_point(data = week_data[label == 1], 
               aes(y = close, color = "Long Profitable"), 
               size = 2, alpha = 0.6) +
    
    # Short Labels (rot)
    geom_point(data = week_data[label == -1], 
               aes(y = close, color = "Short Profitable"), 
               size = 2, alpha = 0.6) +
    
    # Unprofitable Labels (grau)
    geom_point(data = week_data[label == 0], 
               aes(y = close, color = "Unprofitable"), 
               size = 1.5, alpha = 0.4) +
    
    scale_color_manual(
      name = "Label",
      values = c(
        "Long Profitable" = "#2ecc71",    # Grün
        "Short Profitable" = "#e74c3c",   # Rot
        "Unprofitable" = "#95a5a6"        # Grau
      )
    ) +
    
    labs(
      title = paste0("Directional Labels: ", week_name),
      subtitle = paste0(format(week_start, "%d.%m"), " - ", 
                        format(week_end, "%d.%m.%Y")),
      x = "Zeit",
      y = EPIC
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  return(p)
}

cat("\n\nErstelle Plots...\n")

# Plot 1: Woche 1
p1 <- plot_labels_on_price(labelled_dt, week1_start, week1_end, "Woche 1")
print(p1)

ggsave(
  filename = file.path(output_path, "directional_week1.png"),
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Plot 2: Woche 2
p2 <- plot_labels_on_price(labelled_dt, week2_start, week2_end, "Woche 2")
print(p2)

ggsave(
  filename = file.path(output_path, "directional_week2.png"),
  plot = p2,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Combined Plot
combined_plot <- grid.arrange(
  p1, p2,
  ncol = 1,
  nrow = 2,
  top = textGrob(
    "Directional Triple Barrier Labels: Two Trading Weeks\nWoche 1: 11.-15. August | Woche 2: 8.-12. September 2025",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

ggsave(
  filename = file.path(output_path, "directional_comparison_2weeks.png"),
  plot = combined_plot,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("Wochen-Plots gespeichert!\n")

# ==============================================================================
# 6. ROLLING LABEL DISTRIBUTION
# ==============================================================================
# Zeigt wie sich die Label-Verteilung über die Zeit entwickelt
# Window = 96 Bars = 1 Handelstag (bei 15-Minuten-Intervallen)
# ==============================================================================

plot_rolling_label_distribution <- function(dt, window = 96) {
  
  dt_copy <- copy(dt)
  
  # Rolling Proportions für jedes Label berechnen
  dt_copy[, long_pct := frollapply(
    label,
    n = window,
    FUN = function(x) sum(x == 1) / length(x) * 100,
    align = "right"
  )]
  
  dt_copy[, short_pct := frollapply(
    label,
    n = window,
    FUN = function(x) sum(x == -1) / length(x) * 100,
    align = "right"
  )]
  
  dt_copy[, unprofitable_pct := frollapply(
    label,
    n = window,
    FUN = function(x) sum(x == 0) / length(x) * 100,
    align = "right"
  )]
  
  # Plot erstellen
  p <- ggplot(dt_copy, aes(x = time)) +
    geom_line(aes(y = long_pct, color = "Long"), 
              size = 1, alpha = 0.8) +
    geom_line(aes(y = short_pct, color = "Short"), 
              size = 1, alpha = 0.8) +
    geom_line(aes(y = unprofitable_pct, color = "Unprofitable"), 
              size = 0.7, alpha = 0.6) +
    
    scale_color_manual(
      name = "Label Type",
      values = c(
        "Long" = "#2ecc71",
        "Short" = "#e74c3c",
        "Unprofitable" = "#95a5a6"
      )
    ) +
    
    labs(
      title = "Rolling Label Distribution Over Time",
      subtitle = paste0("Window: ", window, " bars (≈ 1 Handelstag)"),
      x = "Zeit",
      y = "Prozent (%)"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    ylim(0, 100)
  
  return(p)
}

p_rolling <- plot_rolling_label_distribution(labelled_dt)
print(p_rolling)

ggsave(
  filename = file.path(output_path, "rolling_label_distribution.png"),
  plot = p_rolling,
  width = 14,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Rolling distribution plot gespeichert!\n")

# ==============================================================================
# 7. DATEN SPEICHERN
# ==============================================================================

write_csv(labelled_dt, file.path(output_path, directional_name))

cat("\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  DATEN GESPEICHERT\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Gespeicherte CSV:\n")
cat("  -", directional_name, "\n\n")

cat("Gespeicherte Plots:\n")
cat("  - directional_week1.png\n")
cat("  - directional_week2.png\n")
cat("  - directional_comparison_2weeks.png\n")
cat("  - rolling_label_distribution.png\n\n")

cat("Speicherort:", output_path, "\n\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  LABELING ABGESCHLOSSEN!\n")
cat(paste(rep("=", 70), collapse = ""), "\n")