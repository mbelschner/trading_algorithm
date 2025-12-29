# =============================================================================
# FEATURE SELECTION FÜR TRADING INDIKATOREN
# Basierend auf López de Prado (2018) - Advances in Financial Machine Learning
# und Stefan Jansen - Machine Learning for Algorithmic Trading
# =============================================================================

# Dieses Script implementiert drei komplementäre Feature Importance Methoden:
#
# 1. SFI (Single Feature Importance): 
#    - Bewertet jeden Indikator ISOLIERT
#    - Zeigt die eigenständige Vorhersagekraft ohne Interaktionen
#    - Keine Substitutionseffekte (korrelierte Features beeinflussen sich nicht)
#
# 2. MDI (Mean Decrease Impurity):
#    - Bewertet Features IM VERBUND (Random Forest)
#    - Misst wie viel jedes Feature zur Reduktion der Impurity beiträgt
#    - Schnell, aber In-Sample (kann überschätzen)
#
# 3. MDA (Mean Decrease Accuracy):
#    - Bewertet Features IM VERBUND (Out-of-Sample via Permutation)
#    - Misst den Accuracy-Verlust wenn ein Feature permutiert wird
#    - Langsamer, aber realistischer (OOS)
#
# Zusätzlich: Mutual Information für nicht-lineare Korrelationen

rm(list=ls())
gc()
options(scipen=999)

# =============================================================================
# PACKAGES LADEN
# =============================================================================

library(xgboost)       # Erst laden (hat eigene shift Funktion)
library(data.table)    # Danach laden (überschreibt shift)
library(TTR)           # Technische Indikatoren
library(randomForest)  # Für MDI
library(caret)         # Für Cross-Validation
library(tidyverse)
library(tictoc)
library(foreach)
library(doParallel)
library(infotheo)      # Für Mutual Information

# Fix für shift Konflikte
shift <- data.table::shift

# =============================================================================
# DATEN LADEN
# =============================================================================

#Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"

filename = paste0(EPIC, "_", INTERVAL, ".csv")

# Pfad zu deinen Daten
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")

#Pfad in Arbeit
#input_path <- file.path("Various")
#output_path <- file.path("Various", "outputs")

# Lade Preisdaten 
# Annahme: Daten haben Spalten: datetime, open, high, low, close, volume
prices <- fread(file.path(input_path, filename))
prices = prices %>%
  rename(datetime = time)
prices[, datetime := as.POSIXct(datetime)]
setorder(prices, datetime)

# =============================================================================
# TECHNISCHE INDIKATOREN BERECHNEN
# =============================================================================

cat("Berechne technische Indikatoren...\n")

prices[, log_close := log(close)]
prices[, log_high := log(high)]
prices[, log_low := log(low)]

# --- ATR + Ableitungen ---
atr_raw <- ATR(HLC = prices[, .(high, low, close)], n = 14)
prices[, ATR := atr_raw[, "atr"]]
prices[, ATR_d1 := c(NA, diff(ATR))]

# --- ADX + Ableitungen ---
adx_raw <- ADX(HLC = prices[, .(high, low, close)], n = 14)
prices[, ADX := adx_raw[, "ADX"]]
prices[, ADX_d1 := c(NA, diff(ADX))]

# --- RSI + Ableitungen ---
prices[, RSI := RSI(close, n = 14)]
prices[, RSI_d1 := c(NA, diff(RSI))]

# --- Hurst Exponent ---
calc_hurst <- function(x, n = 100) {
  if(length(x) < n) return(NA)
  rs_values <- numeric(0)
  for(k in c(10, 20, 50)) {
    if(k > n/2) next
    num_blocks <- floor(n / k)
    rs_block <- numeric(num_blocks)
    for(i in 1:num_blocks) {
      block <- x[((i-1)*k + 1):(i*k)]
      mean_block <- mean(block, na.rm = TRUE)
      y <- cumsum(block - mean_block)
      r <- max(y) - min(y)
      s <- sd(block, na.rm = TRUE)
      if(s > 0) rs_block[i] <- r / s
    }
    rs_values <- c(rs_values, mean(rs_block, na.rm = TRUE))
  }
  if(length(rs_values) < 2) return(NA)
  log_n <- log(c(10, 20, 50)[1:length(rs_values)])
  log_rs <- log(rs_values + 1e-10)
  coef(lm(log_rs ~ log_n))[2]
}
#prices[, Hurst := frollapply(log_close, n = 100, FUN = calc_hurst, align = "right")]

# --- VHF ---
calc_vhf <- function(close, high, low, n = 28) {
  hc <- frollapply(close, n = n, FUN = max, align = "right")
  lc <- frollapply(close, n = n, FUN = min, align = "right")
  numerator <- abs(hc - lc)
  changes <- abs(diff(close))
  denominator <- frollapply(c(NA, changes), n = n, FUN = sum, align = "right")
  numerator / denominator
}
prices[, VHF := calc_vhf(close, high, low, n = 28)]

# --- STC ---
calc_stc <- function(close, fast = 23, slow = 50, cycle = 10, smooth = 3) {
  macd <- EMA(close, n = fast) - EMA(close, n = slow)
  macd_min <- frollapply(macd, n = cycle, FUN = min, align = "right")
  macd_max <- frollapply(macd, n = cycle, FUN = max, align = "right")
  stoch_macd <- (macd - macd_min) / (macd_max - macd_min + 1e-10) * 100
  stoch_macd <- EMA(stoch_macd, n = smooth)
  stc_min <- frollapply(stoch_macd, n = cycle, FUN = min, align = "right")
  stc_max <- frollapply(stoch_macd, n = cycle, FUN = max, align = "right")
  stc <- (stoch_macd - stc_min) / (stc_max - stc_min + 1e-10) * 100
  EMA(stc, n = smooth)
}
prices[, STC := calc_stc(close)]

# --- Ichimoku ---
ichimoku <- function(high, low, close, tenkan = 9, kijun = 26, senkou = 52) {
  tenkan_sen <- (frollapply(high, n = tenkan, FUN = max, align = "right") +
                   frollapply(low, n = tenkan, FUN = min, align = "right")) / 2
  kijun_sen <- (frollapply(high, n = kijun, FUN = max, align = "right") +
                  frollapply(low, n = kijun, FUN = min, align = "right")) / 2
  senkou_a <- (tenkan_sen + kijun_sen) / 2
  senkou_b <- (frollapply(high, n = senkou, FUN = max, align = "right") +
                 frollapply(low, n = senkou, FUN = min, align = "right")) / 2
  list(
    tenkan_kijun_diff = (tenkan_sen - kijun_sen) / close * 100,
    price_tenkan_diff = (close - tenkan_sen) / close * 100,
    price_kijun_diff = (close - kijun_sen) / close * 100,
    cloud_width = (senkou_a - senkou_b) / close * 100
  )
}
ichi <- ichimoku(prices$high, prices$low, prices$close)
prices[, Ichimoku_TK := ichi$tenkan_kijun_diff]
prices[, Ichimoku_PT := ichi$price_tenkan_diff]

# --- Bollinger Band Width ---
bb <- BBands(prices$close, n = 20, sd = 2)
prices[, BB_Width := (bb[, "up"] - bb[, "dn"]) / bb[, "mavg"] * 100]

# --- Fisher Transform ---
calc_fisher <- function(high, low, n = 10) {
  mid <- (high + low) / 2
  min_mid <- frollapply(mid, n = n, FUN = min, align = "right")
  max_mid <- frollapply(mid, n = n, FUN = max, align = "right")
  raw <- (mid - min_mid) / (max_mid - min_mid + 1e-10) * 2 - 1
  raw <- pmax(pmin(raw, 0.999), -0.999)
  0.5 * log((1 + raw) / (1 - raw))
}
prices[, Fisher := calc_fisher(high, low)]

# --- Moving Averages ---
prices[, MA20 := SMA(close, n = 20)]
prices[, MA50 := SMA(close, n = 50)]
prices[, MA20_slope := (MA20 - data.table::shift(MA20, 5)) / data.table::shift(MA20, 5) * 100]
prices[, MA50_slope := (MA50 - data.table::shift(MA50, 5)) / data.table::shift(MA50, 5) * 100]

# --- Aroon ---
aroon_raw <- aroon(prices[, .(high, low)], n = 25)
prices[, Aroon_Up := aroon_raw[, "aroonUp"]]
prices[, Aroon_Dn := aroon_raw[, "aroonDn"]]
prices[, Aroon_Osc := Aroon_Up - Aroon_Dn]

# --- Choppiness Index ---
calc_chop <- function(high, low, close, n = 14) {
  atr_sum <- frollapply(ATR(cbind(high, low, close), n = 1)[, "atr"], 
                        n = n, FUN = sum, align = "right")
  high_n <- frollapply(high, n = n, FUN = max, align = "right")
  low_n <- frollapply(low, n = n, FUN = min, align = "right")
  100 * log10(atr_sum / (high_n - low_n + 1e-10)) / log10(n)
}
prices[, Choppiness := calc_chop(high, low, close)]

# --- CMO ---
prices[, CMO := CMO(close, n = 14)]

# --- Parabolic SAR ---
sar <- SAR(prices[, .(high, low)])
prices[, SAR := sar]
prices[, SAR_dist := (close - SAR) / close * 100]

# --- DPO Trading View ---
calc_dpo_tv <- function(close, period = 20, centered = FALSE) {
  barsback <- floor(period / 2) + 1
  ma <- SMA(close, n = period)
  
  if (centered) {
    # Centered: close[barsback] - ma
    # Shifte close zurück
    close_shifted <- shift(close, n = barsback, type = "lag")
    dpo <- close_shifted - ma
  } else {
    # Non-centered: close - ma[barsback]
    # Shifte ma zurück
    ma_shifted <- shift(ma, n = barsback, type = "lag")
    dpo <- close - ma_shifted
  }
  
  return(dpo)
}
prices[, DPO := calc_dpo_tv(close, period = 20, centered = FALSE)]
prices[, DPO_centered := calc_dpo_tv(close, period = 20, centered = TRUE)]

# --- Coppock Curve ---
calc_coppock <- function(close, n1 = 14, n2 = 11, wma_n = 10) {
  roc1 <- ROC(close, n = n1, type = "discrete") * 100
  roc2 <- ROC(close, n = n2, type = "discrete") * 100
  WMA(roc1 + roc2, n = wma_n)
}
prices[, Coppock := calc_coppock(close)]

# --- ROC ---
prices[, ROC := ROC(close, n = 12, type = "discrete") * 100]

# --- FRAMA ---
calc_frama <- function(close, n = 16) {
  half_n <- floor(n / 2)
  n1 <- frollapply(close, n = half_n, FUN = function(x) (max(x) - min(x)) / half_n, align = "right")
  n2 <- frollapply(close, n = half_n, FUN = function(x) (max(x) - min(x)) / half_n, align = "right")
  n2 <- data.table::shift(n2, half_n)
  n3 <- frollapply(close, n = n, FUN = function(x) (max(x) - min(x)) / n, align = "right")
  D <- (log(n1 + n2 + 1e-10) - log(n3 + 1e-10)) / log(2)
  alpha <- exp(-4.6 * (D - 1))
  alpha <- pmax(pmin(alpha, 1), 0.01)
  frama <- close
  for(i in 2:length(close)) {
    if(!is.na(alpha[i])) {
      frama[i] <- alpha[i] * close[i] + (1 - alpha[i]) * frama[i-1]
    }
  }
  (close - frama) / close * 100
}
prices[, FRAMA := calc_frama(close)]

# --- McGinley Dynamic ---
calc_mcginley <- function(close, n = 14) {
  md <- close
  for(i in 2:length(close)) {
    if(is.na(md[i-1])) {
      md[i] <- close[i]
    } else {
      md[i] <- md[i-1] + (close[i] - md[i-1]) / (n * (close[i] / md[i-1])^4)
    }
  }
  (close - md) / close * 100
}
prices[, McGinley := calc_mcginley(close)]

# --- KST ---
calc_kst <- function(close) {
  roc1 <- ROC(close, n = 10, type = "discrete")
  roc2 <- ROC(close, n = 15, type = "discrete")
  roc3 <- ROC(close, n = 20, type = "discrete")
  roc4 <- ROC(close, n = 30, type = "discrete")
  rcma1 <- SMA(roc1, n = 10)
  rcma2 <- SMA(roc2, n = 10)
  rcma3 <- SMA(roc3, n = 10)
  rcma4 <- SMA(roc4, n = 15)
  rcma1 * 1 + rcma2 * 2 + rcma3 * 3 + rcma4 * 4
}
prices[, KST := calc_kst(close)]

# --- Volume Features ---
prices[, Volume_ratio := volume / SMA(volume, n = 20)]
prices[, Volume_trend := SMA(volume, n = 5) / SMA(volume, n = 20)]

cat("Indikatoren berechnet.\n")

# =============================================================================
# FEATURE INTERACTIONS
# Basierend auf López de Prado (2018) - Kapitel 5: Feature Engineering
# =============================================================================

cat("\n=== Berechne Feature Interactions ===\n")

# --- 1. DPO + ATR Interaction ---
# Rationale: DPO zeigt Trendstärke, ATR normalisiert nach Volatilität
# Ein starkes DPO-Signal bei niedriger Volatilität ist aussagekräftiger
prices[, DPO_ATR_Ratio := DPO / (ATR + 1e-6)]
# Erklärung: Dividiert DPO durch ATR, um volatilitätsnormalisierte Signale zu erhalten
# +1e-6 verhindert Division durch Null

prices[, DPO_ATR_Product := DPO * ATR]
# Erklärung: Produkt verstärkt Signale wenn BEIDE hoch sind
# Nützlich um starke Trends MIT Volatilität zu identifizieren

# --- 2. ADX + RSI Interaction ---
# Rationale: ADX misst Trendstärke, RSI misst Momentum/Überkauft/Überverkauft
# Starke Trends (ADX>25) mit extremem RSI sind wichtige Signale
prices[, ADX_RSI_Product := ADX * RSI]
# Erklärung: Hohe Werte = starker Trend MIT Momentum

prices[, ADX_RSI_Divergence := (ADX - 25) * (RSI - 50)]
# Erklärung: Misst wie weit ADX von neutral (25) UND RSI von neutral (50) ist
# Positiv wenn beide in gleiche Richtung zeigen

# --- 3. BB_Width + RSI Interaction ---
# Rationale: Bollinger Band Width misst Volatilität, RSI misst Momentum
# Enge Bands (Squeeze) + extreme RSI = Ausbruch-Signal
prices[, BB_RSI_Product := BB_Width * RSI]
# Erklärung: Identifiziert volatile Phasen mit Momentum

prices[, BB_RSI_Ratio := BB_Width / (abs(RSI - 50) + 1)]
# Erklärung: Normalisiert BB_Width nach RSI-Extremität
# Niedrige Werte = enge Bands mit neutralem RSI (ruhige Phase)
# Hohe Werte = breite Bands mit extremem RSI (volatile Phase)

# --- 4. BB_Width + DPO Interaction ---
# Rationale: Volatilität (BB_Width) + Zyklusstärke (DPO)
# DPO erkennt Zyklen besser in volatilen vs. ruhigen Phasen
prices[, BB_DPO_Product := BB_Width * DPO]
# Erklärung: Verstärkt DPO-Signale in volatilen Phasen

cat("Feature Interactions berechnet.\n")

# =============================================================================
# KORRIGIERTER 1H MERGE - EXAKTES ALIGNMENT MIT TRADINGVIEW
# =============================================================================

# Funktion zum Aggregieren (UNVERÄNDERT)
aggregate_to_higher_tf <- function(data, tf_minutes) {
  data_copy <- copy(data)
  data_copy[, datetime_tf := lubridate::floor_date(datetime, paste0(tf_minutes, " mins"))]
  
  higher_tf <- data_copy[, .(
    open = first(open),
    high = max(high),
    low = min(low),
    close = last(close),
    volume = sum(volume)
  ), by = datetime_tf]
  
  return(higher_tf)
}

# --- 1H Aggregation ---
prices_1h <- aggregate_to_higher_tf(prices, 60)

# Berechne 1H Indikatoren
prices_1h[, DPO_1h := calc_dpo_tv(close, period = 21, centered = FALSE)]

atr_1h <- ATR(HLC = prices_1h[, .(high, low, close)], n = 14)
prices_1h[, ATR_1h := atr_1h[, "atr"]]

prices_1h[, RSI_1h := RSI(close, n = 14)]

adx_1h <- ADX(HLC = prices_1h[, .(high, low, close)], n = 14)
prices_1h[, ADX_1h := adx_1h[, "ADX"]]

sar_1h <- SAR(prices_1h[, .(high, low)])
prices_1h[, SAR_1h := sar_1h]
prices_1h[, SAR_dist_1h := (close - SAR_1h) / close * 100]

prices_1h[, Volume_ratio_1h := volume / SMA(volume, n = 20)]

# =============================================================================
# KRITISCHER FIX: Shifte datetime_tf um 45 MINUTEN VORWÄRTS
# =============================================================================

# Die 00:00-01:00 Bar soll bei 00:45 erscheinen (nicht bei 01:00)
# Daher: +45 Minuten
prices_1h[, datetime_tf_shifted := datetime_tf + lubridate::minutes(45)]

# Wähle Features für Merge
prices_1h_select <- prices_1h[, .(datetime_tf_shifted, DPO_1h, ATR_1h, RSI_1h, 
                                  ADX_1h, SAR_dist_1h, Volume_ratio_1h)]

# =============================================================================
# MERGE MIT SHIFTED DATETIME (45min offset)
# =============================================================================

# Füge datetime zu prices als merge key hinzu (direkt ohne ceiling/floor)
prices <- merge(prices, prices_1h_select, 
                by.x = "datetime", 
                by.y = "datetime_tf_shifted", 
                all.x = TRUE)

# Forward Fill
setorder(prices, datetime)
fill_cols_1h <- c("DPO_1h", "ATR_1h", "RSI_1h", "ADX_1h", "SAR_dist_1h", "Volume_ratio_1h")

for(col in fill_cols_1h) {
  prices[, (col) := nafill(get(col), type = "locf")]
}

cat("1H Features korrekt aligned und gemerged.\n")

# =============================================================================
# MULTI-TIMEFRAME ALIGNMENT FEATURES (nur 15min vs 1H)
# =============================================================================

cat("Berechne Multi-Timeframe Alignment Features...\n")

# --- MTF Alignment Scores (15min vs 1H) ---

# RSI Alignment
prices[, RSI_MTF_Alignment := sign(RSI - 50) + sign(RSI_1h - 50)]
# Erklärung: Score von -2 bis +2
# +2 = beide Timeframes bullish (RSI > 50)
# -2 = beide Timeframes bearish (RSI < 50)
# 0 = mixed signals

# DPO Alignment
prices[, DPO_MTF_Alignment := sign(DPO) + sign(DPO_1h)]

# Volume Confirmation
prices[, Volume_MTF_Strong := as.integer(Volume_ratio > 1 & Volume_ratio_1h > 1)]

# --- Higher Timeframe Context Features ---

# 1H Trend Context für 15min
prices[, TF_1h_Bullish := as.integer(RSI_1h > 50 & ADX_1h > 20 & DPO_1h > 0)]
prices[, TF_1h_Bearish := as.integer(RSI_1h < 50 & ADX_1h > 20 & DPO_1h < 0)]
# Erklärung: Definiert klaren 1H Trend als Kontext

# --- Divergence Features (15min vs. 1H) ---

# RSI Divergence
prices[, RSI_1h_Divergence := RSI - RSI_1h]
# Erklärung: Positiv = 15min RSI höher als 1H
# Kann Erschöpfung oder Beschleunigung signalisieren

# ATR Ratio (Volatilitätsvergleich)
prices[, ATR_1h_Ratio := ATR / (ATR_1h + 1e-6)]
# Erklärung: Ratio > 1 = 15min volatiler als 1H (unusual)

# Erklärung: 1 wenn 1H DPO-Signal stärker ist

cat("Multi-Timeframe Features berechnet.\n")

# =============================================================================
# NEUE FEATURE IDEAS (basierend auf Top Features)
# =============================================================================

# 1. MEHR VOLUME INTERACTIONS (Volume ist #1!)
prices[, Volume_1h_Acceleration := Volume_ratio - Volume_ratio_1h]
prices[, Volume_ATR_Product := Volume_ratio * ATR]
prices[, Volume_Trend_Strong := as.integer(Volume_trend > 1.2 & Volume_ratio > 1.5)]

# 2. MEHR MULTI-TIMEFRAME COMPARISONS
prices[, ATR_Expansion := ATR_1h_Ratio > 1.2]  # 15min volatiler als 1H
prices[, Trend_Alignment := as.integer(
  sign(MA20_slope) == sign(SAR_dist_1h) & 
    ADX_1h > 20
)]

# 3. MEHR BB + DPO INTERACTIONS (BB_DPO_Product ist Top 10!)
prices[, BB_DPO_Extreme := as.integer(
  BB_Width > quantile(BB_Width, 0.75, na.rm=TRUE) &
    abs(DPO) > quantile(abs(DPO), 0.75, na.rm=TRUE)
)]
prices[, BB_Volume_Product := BB_Width * Volume_ratio]

# 4. SAR INTERACTIONS (SAR_dist_1h ist #2!)
prices[, SAR_Divergence := SAR_dist - SAR_dist_1h]
prices[, SAR_Volume_Confirm := as.integer(
  abs(SAR_dist_1h) > 0.5 & Volume_ratio_1h > 1.2
)]

# 5. ICHIMOKU REFINEMENTS (beide Ichimoku Features in Top 20)
prices[, Ichimoku_Signal := as.integer(
  Ichimoku_TK > 0 & Ichimoku_PT > 0 & ADX_1h > 20
)]
prices[, Ichimoku_Volume := Ichimoku_TK * Volume_ratio]

# =============================================================================
# PRÜFE MTF FEATURES AUF KORREKTHEIT
# =============================================================================

cat("\n=== Validiere MTF Merge ===\n")

# Zähle wie viele Unique 1H Werte es gibt
n_unique_1h <- prices[, .(unique_val = uniqueN(DPO_1h, na.rm = TRUE))]

cat("Anzahl unique 1H Werte:", n_unique_1h$unique_val, "\n")
cat("Erwartete 1H bars:", ceiling(nrow(prices) / 4), "\n")

# Zeige Beispiel wie Forward Fill funktioniert
cat("\n=== Beispiel: Forward Fill Funktionsweise ===\n")
sample_rows <- prices[1:20, .(datetime, DPO, DPO_1h, RSI, RSI_1h)]
print(sample_rows)

# Prüfe auf NA
na_1h <- sum(is.na(prices$DPO_1h))

if(na_1h > 0) {
  cat("⚠️ Warning:", na_1h, "NA-Werte in 1H Features (erste Bars)\n")
} else {
  cat("✓ Keine NA in 1H Features\n")
}

cat("\n=== MTF Feature Statistiken ===\n")

# Alignment Score Verteilung
cat("\nRSI_MTF_Alignment Verteilung:\n")
print(table(prices$RSI_MTF_Alignment))

cat("\nDPO_MTF_Alignment Verteilung:\n")
print(table(prices$DPO_MTF_Alignment))

cat("\nDPO_MTF_Agreement Verteilung:\n")
print(table(prices$DPO_MTF_Agreement))

cat("\nTF_1h_Direction Verteilung:\n")
print(table(prices$TF_1h_Direction))

# =============================================================================
# LABELS LADEN (NUR DIRECTIONAL)
# =============================================================================

labels_directional <- fread(file.path(output_path, paste0("directional_labelled_", filename)))
labels_directional = labels_directional %>%
  dplyr::select(time, label) %>%
  rename(datetime = time, label = label)

prices <- merge(prices, labels_directional[, .(datetime, label)], 
                by = "datetime", all.x = TRUE)

prices <- prices[!is.na(label)]

# =============================================================================
# AKTUALISIERTE FEATURE COLUMNS LISTE (KORRIGIERT)
# =============================================================================

original_cols = colnames(prices)[10:(ncol(prices)-1)]

# FINALE FEATURE LISTE: Nur Previous Top + Neue Features
#selected_features <- c(previous_top_cols, interaction_features, mtf_features)
selected_features = original_cols

selected_features = c(
  # Multi-Timeframe (1H dominiert!)
  "Volume_ratio_1h",
  "SAR_dist_1h",
  "ATR_1h_Ratio",
  "ADX_1h",
  "ATR_1h",
  "RSI_1h",
  "DPO_1h",
  
  # Volume
  "Volume_trend",
  "Volume_ratio",
  
  # Ichimoku
  "Ichimoku_TK",
  "Ichimoku_PT",
  
  # Volatility
  "ATR_d1",
  "BB_Width",
  
  # DPO Interaction (einziges DPO Feature das Top ist!)
  "BB_DPO_Product",
  
  # Trend
  "MA20_slope",
  "DPO",
  "Coppock",
  "VHF",
  "DPO_centered",
  "RSI",
  
  
  #Neue Features
  "Volume_1h_Acceleration", "Volume_ATR_Product", "Volume_Trend_Strong",
  "ATR_Expansion", "Trend_Alignment", "BB_DPO_Extreme",
  "SAR_Divergence", "SAR_Volume_Confirm",
  "Ichimoku_Signal", "Ichimoku_Volume"
)

# Zeige alle Features
cat("Selected Features:\n")
print(selected_features)

# Prüfe ob alle Features existieren
cat("\n=== Validierung: Existieren alle Features? ===\n")
missing_features <- selected_features[!selected_features %in% names(prices)]

if(length(missing_features) > 0) {
  cat("⚠️ FEHLER: Diese Features existieren nicht:\n")
  print(missing_features)
  stop("Bitte behebe die fehlenden Features!")
} else {
  cat("✓ Alle Features existieren in prices\n")
}

# NA-Check BEVOR complete.cases
cat("\n=== NA-Check für selected_features ===\n")
na_summary <- data.table(
  Feature = selected_features,
  NA_Count = sapply(selected_features, function(f) sum(is.na(prices[[f]]))),
  NA_Percent = sapply(selected_features, function(f) 
    round(sum(is.na(prices[[f]])) / nrow(prices) * 100, 2))
)

cat("\nFeatures mit >1% NA:\n")
print(na_summary[NA_Percent > 1][order(-NA_Percent)])

# KORRIGIERT: Verwende .. Präfix
analysis_data <- prices[complete.cases(prices[, ..selected_features])]
cat("\nDatensatz nach NA-Entfernung:", nrow(analysis_data), "Zeilen\n")
cat("Verlust:", nrow(prices) - nrow(analysis_data), "Zeilen (",
    round((nrow(prices) - nrow(analysis_data)) / nrow(prices) * 100, 2), "%)\n")

reduced_cols = c("datetime", "close", "label", "DPO", "DPO_1h", "ADX", "ADX_1h", "RSI", "RSI_1h", "VHF", "MA20", "MA50")
reduced_df = analysis_data[, ..reduced_cols]

# =============================================================================
# 1. MUTUAL INFORMATION
# =============================================================================

cat("=== 1. MUTUAL INFORMATION ===\n")
tic()
calc_mi <- function(feature, label) {
  feat_disc <- discretize(feature, disc = "equalfreq", nbins = 10)
  lab_disc <- as.numeric(as.factor(label))
  mutinformation(feat_disc, lab_disc)
}

mi_values <- sapply(selected_features, function(f) {
  idx <- complete.cases(analysis_data[[f]], analysis_data$label)
  calc_mi(analysis_data[[f]][idx], analysis_data$label[idx])
})

mi_results <- data.table(
  Feature = selected_features,
  MI = mi_values,
  MI_Rank = rank(-mi_values)
)

print(mi_results[order(MI_Rank)])

# =============================================================================
# 2. SINGLE FEATURE IMPORTANCE (SFI)
# =============================================================================

cat("\n=== 2. SINGLE FEATURE IMPORTANCE (SFI) ===\n")

calc_sfi <- function(data, feature, label_col, n_folds = 5) {
  cols <- c(feature, label_col)
  subset_data <- data[, ..cols]
  idx <- complete.cases(subset_data)
  
  X <- as.matrix(subset_data[idx, ..feature])
  y <- as.factor(subset_data[[label_col]][idx])
  
  if(length(unique(y)) < 2) return(list(accuracy = NA, accuracy_sd = NA))
  
  set.seed(42)
  folds <- createFolds(y, k = n_folds, list = TRUE)
  accuracies <- numeric(n_folds)
  
  for(i in 1:n_folds) {
    test_idx <- folds[[i]]
    train_idx <- setdiff(1:length(y), test_idx)
    train_data <- data.frame(X = X[train_idx], y = y[train_idx])
    test_data <- data.frame(X = X[test_idx])
    
    tryCatch({
      model <- randomForest(y ~ X, data = train_data, ntree = 100)
      pred <- predict(model, test_data)
      accuracies[i] <- mean(pred == y[test_idx])
    }, error = function(e) { accuracies[i] <- NA })
  }
  
  list(accuracy = mean(accuracies, na.rm = TRUE),
       accuracy_sd = sd(accuracies, na.rm = TRUE))
}

sfi_values <- lapply(selected_features, function(f) {
  calc_sfi(analysis_data, f, "label")
})
names(sfi_values) <- selected_features

sfi_results <- data.table(
  Feature = selected_features,
  SFI_Acc = sapply(sfi_values, `[[`, "accuracy"),
  SFI_SD = sapply(sfi_values, `[[`, "accuracy_sd")
)
sfi_results[, SFI_Rank := rank(-SFI_Acc)]

print(sfi_results[order(SFI_Rank)])

# =============================================================================
# 3. MDI (Mean Decrease Impurity)
# =============================================================================

cat("\n=== 3. MEAN DECREASE IMPURITY (MDI) ===\n")

calc_mdi <- function(data, features, label_col) {
  cols <- c(features, label_col)
  subset_data <- data[, ..cols]
  idx <- complete.cases(subset_data)
  subset_data <- subset_data[idx]
  
  X <- as.matrix(subset_data[, ..features])
  y <- as.factor(subset_data[[label_col]])
  
  set.seed(42)
  rf <- randomForest(x = X, y = y, ntree = 500, mtry = 1, importance = TRUE)
  imp <- importance(rf, type = 2)
  
  data.table(Feature = rownames(imp), MDI = as.numeric(imp[, 1]))
}

mdi_results <- calc_mdi(analysis_data, selected_features, "label")
mdi_results[, MDI_Rank := rank(-MDI)]

print(mdi_results[order(MDI_Rank)])

# =============================================================================
# 4. MDA (Mean Decrease Accuracy)
# =============================================================================

cat("\n=== 4. MEAN DECREASE ACCURACY (MDA) ===\n")

calc_mda <- function(data, features, label_col, n_folds = 5, embargo_pct = 0.01) {
  cols <- c(features, label_col)
  subset_data <- data[, ..cols]
  idx <- complete.cases(subset_data)
  subset_data <- subset_data[idx]
  
  X <- as.matrix(subset_data[, ..features])
  y <- as.factor(subset_data[[label_col]])
  n <- nrow(X)
  
  fold_size <- floor(n / n_folds)
  embargo_size <- floor(fold_size * embargo_pct)
  
  baseline_acc <- numeric(n_folds)
  feature_acc <- matrix(0, nrow = n_folds, ncol = length(features))
  colnames(feature_acc) <- features
  
  for(k in 1:n_folds) {
    test_start <- (k - 1) * fold_size + 1
    test_end <- min(k * fold_size, n)
    test_idx <- test_start:test_end
    
    purge_before <- max(1, test_start - embargo_size)
    purge_after <- min(n, test_end + embargo_size)
    train_idx <- setdiff(1:n, purge_before:purge_after)
    
    if(length(train_idx) < 100) next
    
    rf <- randomForest(x = X[train_idx, , drop = FALSE], 
                       y = y[train_idx], ntree = 100)
    baseline_pred <- predict(rf, X[test_idx, , drop = FALSE])
    baseline_acc[k] <- mean(baseline_pred == y[test_idx])
    
    for(f in 1:length(features)) {
      X_perm <- X[test_idx, , drop = FALSE]
      X_perm[, f] <- sample(X_perm[, f])
      perm_pred <- predict(rf, X_perm)
      feature_acc[k, f] <- mean(perm_pred == y[test_idx])
    }
  }
  
  # FIXED: mean() für Vektor, colMeans() für Matrix
  baseline_mean <- mean(baseline_acc, na.rm = TRUE)
  mda_values <- baseline_mean - colMeans(feature_acc, na.rm = TRUE)
  mda_sd <- apply(feature_acc, 2, function(x) sd(baseline_acc - x, na.rm = TRUE))
  
  data.table(Feature = features, MDA = mda_values, MDA_SD = mda_sd)
}

mda_results <- calc_mda(analysis_data, selected_features, "label")
mda_results[, MDA_Rank := rank(-MDA)]

print(mda_results[order(MDA_Rank)])
toc()
# =============================================================================
# 5. ZUSAMMENFASSUNG
# =============================================================================

cat("\n")
cat("==================================================================\n")
cat("=== ZUSAMMENFASSUNG: FEATURE IMPORTANCE ===\n")
cat("==================================================================\n\n")

final_results <- Reduce(function(x, y) merge(x, y, by = "Feature"), 
                        list(mi_results, sfi_results, mdi_results, mda_results))

final_results[, Consensus_Rank := (MI_Rank + SFI_Rank + MDI_Rank + MDA_Rank) / 4]

summary_table <- final_results[, .(
  Feature,
  `MI` = round(MI, 4),
  `SFI` = round(SFI_Acc, 3),
  `MDI` = round(MDI, 2),
  `MDA` = round(MDA, 4),
  `Konsens` = round(Consensus_Rank, 1)
)][order(`Konsens`)]

print(summary_table)

cat("\n=== INTERPRETATION ===\n")
cat("- SFI hoch, MDA niedrig: Feature alleine gut, aber redundant im Verbund\n")
cat("- SFI niedrig, MDA hoch: Feature braucht andere Features (Interaktion)\n")
cat("- Beide hoch: Starkes, unabhängiges Feature\n")
cat("- Beide niedrig: Schwaches Feature oder Rauschen\n\n")

# =============================================================================
# 6. VISUALISIERUNG
# =============================================================================

plot_data <- melt(final_results[, .(Feature, SFI = SFI_Acc, MDA = MDA * 10)],
                  id.vars = "Feature", variable.name = "Method", value.name = "Score")

p1 <- ggplot(plot_data, aes(x = reorder(Feature, Score), y = Score, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Feature Importance: Allein (SFI) vs. Im Verbund (MDA)",
       subtitle = "MDA skaliert x10",
       x = "Feature", y = "Score") +
  theme_minimal() +
  scale_fill_manual(values = c("SFI" = "#2E86AB", "MDA" = "#A23B72"))

print(p1)

ranking_data <- final_results[, .(Feature, SFI_Rank, MDI_Rank, MDA_Rank, MI_Rank)]
ranking_melt <- melt(ranking_data, id.vars = "Feature", 
                     variable.name = "Method", value.name = "Rank")

p2 <- ggplot(ranking_melt, aes(x = Method, y = reorder(Feature, -Rank), fill = Rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Rank), color = "white", size = 4) +
  scale_fill_gradient(low = "#2E86AB", high = "#F18F01") +
  labs(title = "Feature Ranking Vergleich", x = "Methode", y = "Feature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# =============================================================================
# 7. EXPORT
# =============================================================================

fwrite(final_results, file.path(output_path, "feature_importance_results.csv"))
cat("\nErgebnisse gespeichert.\n")
cat("\n=== SCRIPT ABGESCHLOSSEN ===\n")

# =============================================================================
# FEATURE-LABEL BEZIEHUNGSANALYSE
# Verstehe WIE Features mit Labels zusammenhängen
# =============================================================================

# Füge diesen Code nach der Feature Selection Analyse hinzu

# =============================================================================
# 1. DESKRIPTIVE STATISTIK PRO LABEL-KLASSE
# =============================================================================

cat("\n")
cat("==================================================================\n")
cat("=== FEATURE-LABEL BEZIEHUNGSANALYSE ===\n")
cat("==================================================================\n\n")

cat("=== 1. MITTELWERTE PRO LABEL-KLASSE ===\n")
cat("Zeigt ob Feature bei Label=1 höher/niedriger ist als bei Label=-1\n\n")

# Berechne Mittelwerte pro Label für jedes Feature
label_means <- analysis_data[, lapply(.SD, mean, na.rm = TRUE), 
                             by = label, 
                             .SDcols = selected_features]

# Transponiere für bessere Lesbarkeit
label_means_t <- melt(label_means, id.vars = "label", 
                      variable.name = "Feature", value.name = "Mean")
label_means_wide <- dcast(label_means_t, Feature ~ label, value.var = "Mean")

# Berechne Differenz und Ratio
if("-1" %in% names(label_means_wide) && "1" %in% names(label_means_wide)) {
  label_means_wide[, Diff_1_vs_minus1 := `1` - `-1`]
  label_means_wide[, Ratio := `1` / (`-1` + 1e-10)]
  label_means_wide[, Direction := ifelse(Diff_1_vs_minus1 > 0, "↑ Höher bei Long", "↓ Niedriger bei Long")]
}

print(label_means_wide[order(-abs(Diff_1_vs_minus1))])

# =============================================================================
# 2. BOXPLOTS PRO LABEL-KLASSE
# =============================================================================

cat("\n=== 2. BOXPLOTS (Visualisierung) ===\n")

# Funktion für einzelne Boxplots
plot_feature_by_label <- function(data, feature_name) {
  ggplot(data, aes(x = as.factor(label), y = .data[[feature_name]], fill = as.factor(label))) +
    geom_boxplot(outlier.alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(title = paste("Verteilung von", feature_name, "pro Label"),
         x = "Label", y = feature_name) +
    scale_fill_manual(values = c("-1" = "#E74C3C", "0" = "#95A5A6", "1" = "#27AE60"),
                      name = "Label") +
    theme_minimal()
}

# Top 6 Features plotten
top_features <- summary_table$Feature[1:6]
boxplots <- lapply(top_features, function(f) plot_feature_by_label(analysis_data, f))

# Kombiniere Plots
library(gridExtra)
grid.arrange(grobs = boxplots, ncol = 2)

# =============================================================================
# 3. KORRELATIONSANALYSE (Linear vs. Nicht-Linear)
# =============================================================================

cat("\n=== 3. KORRELATIONSVERGLEICH ===\n")
cat("Pearson = lineare Korrelation | Spearman = monotone (nicht-lineare) Korrelation\n")
cat("Große Differenz = nicht-linearer Zusammenhang\n\n")

# Label als numerisch für Korrelation
analysis_data[, label_num := as.numeric(as.character(label))]

correlation_analysis <- data.table(
  Feature = selected_features,
  Pearson = sapply(selected_features, function(f) {
    cor(analysis_data[[f]], analysis_data$label_num, use = "complete.obs", method = "pearson")
  }),
  Spearman = sapply(selected_features, function(f) {
    cor(analysis_data[[f]], analysis_data$label_num, use = "complete.obs", method = "spearman")
  })
)

correlation_analysis[, Diff := abs(Pearson - Spearman)]
correlation_analysis[, Nonlinearity := ifelse(Diff > 0.05, "⚠️ Nicht-linear", "Linear")]
correlation_analysis[, Direction := ifelse(Pearson > 0, "↑ Positiv", "↓ Negativ")]

print(correlation_analysis[order(-abs(Pearson))])

# =============================================================================
# 4. CONDITIONAL DENSITY PLOTS
# =============================================================================

cat("\n=== 4. DENSITY PLOTS PRO LABEL ===\n")

plot_density_by_label <- function(data, feature_name) {
  ggplot(data, aes(x = .data[[feature_name]], fill = as.factor(label), color = as.factor(label))) +
    geom_density(alpha = 0.4) +
    labs(title = paste("Dichteverteilung von", feature_name),
         x = feature_name, y = "Dichte") +
    scale_fill_manual(values = c("-1" = "#E74C3C", "0" = "#95A5A6", "1" = "#27AE60"),
                      name = "Label") +
    scale_color_manual(values = c("-1" = "#E74C3C", "0" = "#95A5A6", "1" = "#27AE60"),
                       name = "Label") +
    theme_minimal()
}

density_plots <- lapply(top_features, function(f) plot_density_by_label(analysis_data, f))
grid.arrange(grobs = density_plots, ncol = 2)

# =============================================================================
# 5. ZUSAMMENFASSUNG: FEATURE-INTERPRETATIONEN
# =============================================================================

cat("\n")
cat("==================================================================\n")
cat("=== ZUSAMMENFASSUNG: WIE FEATURES MIT LABELS ZUSAMMENHÄNGEN ===\n")
cat("==================================================================\n\n")

# Kombiniere alle Erkenntnisse
interpretation <- merge(correlation_analysis[, .(Feature, Pearson, Direction, Nonlinearity)],
                        label_means_wide[, .(Feature, Diff_1_vs_minus1)],
                        by = "Feature")

interpretation[, Interpretation := paste0(
  Direction, " | ",
  ifelse(Diff_1_vs_minus1 > 0, "Long hat höhere Werte", "Short hat höhere Werte"),
  " | ", Nonlinearity
)]

# Sortiere nach Korrelationsstärke
interpretation <- interpretation[order(-abs(Pearson))]

cat("Feature-Interpretationen (sortiert nach Korrelationsstärke):\n\n")
for(i in 1:min(10, nrow(interpretation))) {
  row <- interpretation[i]
  cat(sprintf("%-15s: Korr=%.3f | %s\n", 
              row$Feature, row$Pearson, row$Interpretation))
}

# =============================================================================
# 6. REDUZIERTES DATA FRAME
# =============================================================================

top10_features <- summary_table$Feature[1:10]

reduced_cols = c("datetime", "open", "high", "low", "close", "label", "ADX", "RSI", "MA20", "MA50", top10_features)
reduced_df = analysis_data[, ..reduced_cols]

# =============================================================================
# 7. LABEL ANALYSE
# =============================================================================



# =============================================================================
# 8. EXPORT DER BEZIEHUNGSANALYSE
# =============================================================================

fwrite(reduced_df, file.path(output_path, paste0(EPIC, "_reduced_df.csv")))
fwrite(summary_table, file.path(output_path, paste0(EPIC, "_feature_summary.csv")))
fwrite(interpretation, file.path(output_path, "feature_label_relationships.csv"))
fwrite(analysis_data, file.path(output_path, paste0(EPIC, "_analysis_data.csv")))

cat("\n\nBeziehungsanalyse gespeichert.\n")
