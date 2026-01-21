# ============================================================================
# QUANTILE REGRESSION META-LABELLING - RISK-ADJUSTED RETURNS
# ============================================================================
#
# PURPOSE:
# - Predict Expected Upside (75th Percentile) of RISK-ADJUSTED trade PnL
# - Risk-adjusted = PnL / ATR (volatility-normalized returns)
# - Filter trades based on predicted risk-adjusted upside potential
#
# DIFFERENCE FROM ORIGINAL:
# - Target variable: pnl / atr_14 instead of raw pnl
# - This normalizes returns by current volatility regime
# - Should be more stable across different market conditions
#
# REQUIRES:
# - Run 02_backtest_main_script_ls_v2.R first (creates feature cache)
# - Trained Long/Short models
#
# ============================================================================

cat("\n=== QUANTILE REGRESSION META-LABELLING (RISK-ADJUSTED) ===\n")

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  xgboost,         # Quantile regression
  ggplot2,         # Visualization
  scales,          # Plot formatting
  TTR,             # Technical indicators
  zoo,             # Rolling functions
  jsonlite,        # Read JSON files
  dplyr,           # ntile function
  Boruta,          # Feature selection
  ranger,          # Random forest for Boruta
  doParallel,      # Parallel processing
  foreach          # Parallel foreach
)

# Setup parallel processing
n_cores <- parallel::detectCores() - 1  # Leave one core free
cat(sprintf("Setting up parallel processing with %d cores...\n", n_cores))
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
features_cache_path <- file.path("feature_cache")
models_path <- file.path(backtest_output_path, "models")
qr_output_path <- file.path(backtest_output_path, "quantile_regression_risk_adj")

# Create output folder
if (!dir.exists(qr_output_path)) {
  dir.create(qr_output_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
LABEL_VERSION <- "enhanced_neutral"
TEST_YEAR <- 2025

# Quantile regression parameters
QUANTILE_ALPHA <- 0.75  # Predict 75th percentile (upside)

# XGBoost parameters - more regularization than classification
xgb_params <- list(
  objective = "reg:quantileerror",
  quantile_alpha = QUANTILE_ALPHA,
  max_depth = 4,
  eta = 0.02,
  subsample = 0.7,
  colsample_bytree = 0.7,
  min_child_weight = 30
)

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Test Year: %d\n", TEST_YEAR))
cat(sprintf("  Quantile Alpha: %.2f\n", QUANTILE_ALPHA))
cat(sprintf("  Target: RISK-ADJUSTED PnL (pnl / atr)\n"))

# ===== STEP 1: LOAD TRAINED PRIMARY MODELS ===================================

cat("\n=== STEP 1: LOAD TRAINED PRIMARY MODELS ===\n")

model_long_file_json <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".json")
)

model_short_file_json <- file.path(
  models_path,
  paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".json")
)

# Check if models exist
if (!file.exists(model_long_file_json)) {
  stop(sprintf("ERROR: Long model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_long_file_json))
}
if (!file.exists(model_short_file_json)) {
  stop(sprintf("ERROR: Short model not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", model_short_file_json))
}

# Load models
cat("Loading Long model...\n")
model_long <- xgb.load(model_long_file_json)

cat("Loading Short model...\n")
model_short <- xgb.load(model_short_file_json)

# Extract feature names from JSON files
cat("Extracting feature names from JSON...\n")

model_long_json_data <- fromJSON(model_long_file_json)
features_long <- model_long_json_data$learner$feature_names

model_short_json_data <- fromJSON(model_short_file_json)
features_short <- model_short_json_data$learner$feature_names

cat(sprintf("Long model features: %d\n", length(features_long)))
cat(sprintf("Short model features: %d\n", length(features_short)))

# ===== STEP 2: LOAD CACHED FEATURES ==========================================

cat("\n=== STEP 2: LOAD CACHED FEATURES ===\n")

features_cache_file <- file.path(
  features_cache_path,
  paste0(EPIC, "_", INTERVAL, "_features_all.csv")
)

if (!file.exists(features_cache_file)) {
  stop(sprintf("ERROR: Features cache not found!\nPath: %s\n\nPlease run 02_backtest_main_script_ls_v2.R first!", features_cache_file))
}

cat(sprintf("Loading features from cache: %s\n", features_cache_file))
dt_features <- fread(features_cache_file)
setDT(dt_features)

# Convert datetime
if (is.character(dt_features$datetime)) {
  dt_features[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Features loaded: %s rows, %d columns\n",
            format(nrow(dt_features), big.mark = ","),
            ncol(dt_features)))

# ===== STEP 3: LOAD LABELS ===================================================

cat("\n=== STEP 3: LOAD LABELS ===\n")

labels_file <- file.path(labelled_data_path, paste0(EPIC, "_", INTERVAL, "_labeled_", LABEL_VERSION, ".csv"))

if (!file.exists(labels_file)) {
  stop(sprintf("ERROR: Labels not found!\nPath: %s", labels_file))
}

cat(sprintf("Loading labels: %s\n", labels_file))
dt_labels <- fread(labels_file)
setDT(dt_labels)

# Convert datetime
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

# ===== STEP 4: MERGE FEATURES AND LABELS =====================================

cat("\n=== STEP 4: MERGE FEATURES AND LABELS ===\n")

# Select label columns to merge (include atr for risk adjustment)
label_cols_to_merge <- c("datetime", "label", "sample_weight", "barrier_touched",
                         "bars_to_exit", "realized_return", "n_concurrent",
                         "realized_return_adj", "log_return", "atr", "atr_pct")

available_label_cols <- intersect(label_cols_to_merge, names(dt_labels))

cat("Merging features with labels...\n")

# Merge
dt_merged <- merge(
  dt_features,
  dt_labels[, ..available_label_cols],
  by = "datetime",
  all = FALSE
)

cat(sprintf("Merged dataset: %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# ===== STEP 5: GENERATE PRIMARY MODEL PREDICTIONS ============================

cat("\n=== STEP 5: GENERATE PRIMARY MODEL PREDICTIONS ===\n")

# Prepare features for prediction
cat("Preparing Long model features...\n")
missing_long <- setdiff(features_long, names(dt_merged))
if (length(missing_long) > 0) {
  cat(sprintf("WARNING: Missing Long features: %d\n", length(missing_long)))
}
available_features_long <- intersect(features_long, names(dt_merged))

cat("Preparing Short model features...\n")
missing_short <- setdiff(features_short, names(dt_merged))
if (length(missing_short) > 0) {
  cat(sprintf("WARNING: Missing Short features: %d\n", length(missing_short)))
}
available_features_short <- intersect(features_short, names(dt_merged))

# Generate predictions
cat("Generating Long predictions...\n")
X_long <- as.matrix(dt_merged[, ..available_features_long])
dmat_long <- xgb.DMatrix(data = X_long)
dt_merged[, pred_prob_long := predict(model_long, dmat_long)]

cat("Generating Short predictions...\n")
X_short <- as.matrix(dt_merged[, ..available_features_short])
dmat_short <- xgb.DMatrix(data = X_short)
dt_merged[, pred_prob_short := predict(model_short, dmat_short)]

cat(sprintf("Predictions generated for %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# ===== STEP 6: GENERATE SIGNALS AND CALCULATE PNL ============================

cat("\n=== STEP 6: GENERATE SIGNALS AND CALCULATE RISK-ADJUSTED PNL ===\n")

# Signal thresholds (same as backtest)
LONG_THRESHOLD <- 0.55
SHORT_THRESHOLD <- 0.55

# Generate signals
dt_merged[, signal_long := as.integer(pred_prob_long > LONG_THRESHOLD)]
dt_merged[, signal_short := as.integer(pred_prob_short > SHORT_THRESHOLD)]

# Combined signal: Long = 1, Short = -1, Neutral = 0
dt_merged[, signal := fifelse(
  signal_long == 1 & signal_short == 0, 1L,
  fifelse(
    signal_long == 0 & signal_short == 1, -1L,
    fifelse(
      signal_long == 1 & signal_short == 1,
      fifelse(pred_prob_long > pred_prob_short, 1L, -1L),
      0L
    )
  )
)]

# Filter to only rows with signals
dt_trades <- dt_merged[signal != 0]

cat(sprintf("Trades with signals: %s\n", format(nrow(dt_trades), big.mark = ",")))
cat(sprintf("  Long signals: %d\n", sum(dt_trades$signal == 1)))
cat(sprintf("  Short signals: %d\n", sum(dt_trades$signal == -1)))

# Calculate raw PnL
dt_trades[, pnl := fifelse(signal == 1, log_return, -log_return)]
dt_trades[, trade_direction := fifelse(signal == 1, "Long", "Short")]

# ===== CALCULATE RISK-ADJUSTED PNL ===========================================

cat("\n=== CALCULATING RISK-ADJUSTED PNL ===\n")

# Get ATR for risk adjustment - use atr_14 from features if available, else from labels
if ("atr_14" %in% names(dt_trades)) {
  dt_trades[, atr_for_adj := atr_14]
  cat("Using atr_14 from features for risk adjustment\n")
} else if ("atr" %in% names(dt_trades)) {
  dt_trades[, atr_for_adj := atr]
  cat("Using atr from labels for risk adjustment\n")
} else {
  stop("ERROR: No ATR column found for risk adjustment!")
}

# Risk-adjusted PnL = PnL / ATR
# This normalizes returns by volatility
dt_trades[, pnl_risk_adj := pnl / atr_for_adj]

# Handle edge cases (very small ATR)
min_atr <- quantile(dt_trades$atr_for_adj, 0.01, na.rm = TRUE)
dt_trades[atr_for_adj < min_atr, pnl_risk_adj := pnl / min_atr]

# Remove extreme outliers (beyond 5 std)
pnl_ra_mean <- mean(dt_trades$pnl_risk_adj, na.rm = TRUE)
pnl_ra_sd <- sd(dt_trades$pnl_risk_adj, na.rm = TRUE)
dt_trades[abs(pnl_risk_adj - pnl_ra_mean) > 5 * pnl_ra_sd, pnl_risk_adj := NA]

cat(sprintf("\nRaw PnL Summary:\n"))
cat(sprintf("  Mean: %.6f\n", mean(dt_trades$pnl, na.rm = TRUE)))
cat(sprintf("  Std:  %.6f\n", sd(dt_trades$pnl, na.rm = TRUE)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_trades$pnl > 0, na.rm = TRUE)))

cat(sprintf("\nRisk-Adjusted PnL Summary:\n"))
cat(sprintf("  Mean: %.4f\n", mean(dt_trades$pnl_risk_adj, na.rm = TRUE)))
cat(sprintf("  Std:  %.4f\n", sd(dt_trades$pnl_risk_adj, na.rm = TRUE)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_trades$pnl_risk_adj > 0, na.rm = TRUE)))

# Correlation between raw and risk-adjusted
cor_raw_adj <- cor(dt_trades$pnl, dt_trades$pnl_risk_adj, use = "complete.obs")
cat(sprintf("\nCorrelation (raw vs risk-adj): %.4f\n", cor_raw_adj))

# ===== STEP 7: COMPUTE META-FEATURES =========================================

cat("\n=== STEP 7: COMPUTE META-FEATURES ===\n")

setorder(dt_trades, datetime)

# --- 7.1: Signal Quality Features (from primary models) ---
cat("\n--- Signal Quality Features ---\n")

dt_trades[, clarity_gap := abs(pred_prob_long - pred_prob_short)]
dt_trades[, dominant_prob := pmax(pred_prob_long, pred_prob_short)]
dt_trades[, prob_sum := pred_prob_long + pred_prob_short]
dt_trades[, prob_diff := pred_prob_long - pred_prob_short]

cat("  clarity_gap, dominant_prob, prob_sum, prob_diff\n")

# --- 7.2: Volatility Features ---
cat("\n--- Volatility Features ---\n")

# ATR-based
if ("atr_14" %in% names(dt_trades)) {
  # ATR percentile (rolling)
  dt_trades[, atr_percentile := {
    n <- .N
    result <- rep(NA_real_, n)
    for (i in 60:n) {
      window <- atr_14[(i-59):i]
      result[i] <- sum(window <= atr_14[i], na.rm = TRUE) / sum(!is.na(window))
    }
    result
  }]
  cat("  atr_percentile: ATR_14 rolling percentile\n")
}

if ("atr_14" %in% names(dt_trades) && "atr_28" %in% names(dt_trades)) {
  dt_trades[, atr_ratio := atr_14 / atr_28]
  cat("  atr_ratio: ATR_14 / ATR_28\n")
}

# Bollinger Band width
if ("bb_bandwidth_20" %in% names(dt_trades)) {
  dt_trades[, bb_width_percentile := {
    n <- .N
    result <- rep(NA_real_, n)
    for (i in 60:n) {
      window <- bb_bandwidth_20[(i-59):i]
      result[i] <- sum(window <= bb_bandwidth_20[i], na.rm = TRUE) / sum(!is.na(window))
    }
    result
  }]
  cat("  bb_width_percentile: BB width rolling percentile\n")
}

# Keltner Channel width
if (all(c("kc_upper_20", "kc_lower_20", "kc_mid_20") %in% names(dt_trades))) {
  dt_trades[, kc_width := (kc_upper_20 - kc_lower_20) / kc_mid_20]
  cat("  kc_width: Keltner Channel width\n")
}

# VHF (Vertical Horizontal Filter)
if ("vhf_28" %in% names(dt_trades)) {
  cat("  vhf_28: Already available\n")
}

# Choppiness
if ("choppiness_14" %in% names(dt_trades)) {
  cat("  choppiness_14: Already available\n")
}

# --- 7.3: Trend Features ---
cat("\n--- Trend Features ---\n")

# ADX and components
if ("adx_14" %in% names(dt_trades)) {
  cat("  adx_14: Already available\n")
}

if ("di_diff_14" %in% names(dt_trades)) {
  cat("  di_diff_14: Already available\n")
}

# EMA slopes and distances
ema_features <- c("ema_9_slope", "ema_21_slope", "ema_50_slope",
                  "dist_ema_9", "dist_ema_21", "dist_ema_50")
available_ema <- intersect(ema_features, names(dt_trades))
if (length(available_ema) > 0) {
  cat(sprintf("  EMA features: %s\n", paste(available_ema, collapse = ", ")))
}

# EMA cross signals
if ("ema_9_21_cross" %in% names(dt_trades)) {
  cat("  ema_9_21_cross: Already available\n")
}
if ("ema_21_50_cross" %in% names(dt_trades)) {
  cat("  ema_21_50_cross: Already available\n")
}

# Aroon
if (all(c("aroon_up", "aroon_down", "aroon_oscillator") %in% names(dt_trades))) {
  cat("  aroon_up, aroon_down, aroon_oscillator: Already available\n")
}

# Ichimoku
if ("ichimoku_position" %in% names(dt_trades)) {
  cat("  ichimoku_position: Already available\n")
}

# SAR signal
if ("sar_signal" %in% names(dt_trades)) {
  cat("  sar_signal: Already available\n")
}

# Donchian position
if ("donchian_position_20" %in% names(dt_trades)) {
  cat("  donchian_position_20: Already available\n")
}

# --- 7.4: Momentum Features ---
cat("\n--- Momentum Features ---\n")

# RSI
if (all(c("rsi_14", "rsi_28") %in% names(dt_trades))) {
  dt_trades[, rsi_diff := rsi_14 - rsi_28]
  cat("  rsi_14, rsi_28, rsi_diff\n")
}

# Stochastic
if (all(c("stoch_k", "stoch_d") %in% names(dt_trades))) {
  cat("  stoch_k, stoch_d: Already available\n")
}
if ("stoch_k_d_diff" %in% names(dt_trades)) {
  cat("  stoch_k_d_diff: Already available\n")
}

# ROC (Rate of Change)
roc_features <- c("roc_5", "roc_10", "roc_20")
available_roc <- intersect(roc_features, names(dt_trades))
if (length(available_roc) > 0) {
  cat(sprintf("  ROC features: %s\n", paste(available_roc, collapse = ", ")))
}

# Momentum
mom_features <- c("momentum_5", "momentum_10", "momentum_20")
available_mom <- intersect(mom_features, names(dt_trades))
if (length(available_mom) > 0) {
  cat(sprintf("  Momentum features: %s\n", paste(available_mom, collapse = ", ")))
}

# DPO
if ("dpo_20" %in% names(dt_trades)) {
  cat("  dpo_20: Already available\n")
}

# --- 7.5: Volume Features ---
cat("\n--- Volume Features ---\n")

if ("volume_ratio" %in% names(dt_trades)) {
  cat("  volume_ratio: Already available\n")
}

if ("obv" %in% names(dt_trades)) {
  # OBV slope
  dt_trades[, obv_slope := obv - shift(obv, 5)]
  cat("  obv, obv_slope\n")
}

if ("vpt" %in% names(dt_trades)) {
  cat("  vpt: Already available\n")
}

# --- 7.6: Bollinger Band Position ---
cat("\n--- Mean Reversion Features ---\n")

if ("bb_pct_20" %in% names(dt_trades)) {
  cat("  bb_pct_20: Already available\n")
}

if ("kc_position_20" %in% names(dt_trades)) {
  cat("  kc_position_20: Already available\n")
}

# --- 7.7: Time/Session Features ---
cat("\n--- Time/Session Features ---\n")

dt_trades[, hour := hour(datetime)]
dt_trades[, day_of_week := wday(datetime)]  # 1=Sunday, 7=Saturday
dt_trades[, minute_of_day := hour * 60 + minute(datetime)]

# Session flags (approximate)
# Asian: 00:00-08:00 UTC
# London: 08:00-16:00 UTC
# New York: 13:00-21:00 UTC
dt_trades[, session_asian := as.integer(hour >= 0 & hour < 8)]
dt_trades[, session_london := as.integer(hour >= 8 & hour < 16)]
dt_trades[, session_newyork := as.integer(hour >= 13 & hour < 21)]
dt_trades[, session_overlap := as.integer(hour >= 13 & hour < 16)]  # London/NY overlap

cat("  hour, day_of_week, minute_of_day\n")
cat("  session_asian, session_london, session_newyork, session_overlap\n")

# --- 7.8: Trade Direction Feature ---
cat("\n--- Trade Context Features ---\n")

dt_trades[, is_long := as.integer(signal == 1)]
cat("  is_long: Trade direction indicator\n")

# ===== STEP 8: TRAIN/TEST SPLIT ==============================================

cat("\n=== STEP 8: TRAIN/TEST SPLIT ===\n")

dt_trades[, year := year(datetime)]

# Remove rows with NA in target
dt_trades <- dt_trades[!is.na(pnl_risk_adj)]

dt_train <- dt_trades[year < TEST_YEAR]
dt_test <- dt_trades[year == TEST_YEAR]

cat(sprintf("Training data: %s trades (before %d)\n", format(nrow(dt_train), big.mark = ","), TEST_YEAR))
cat(sprintf("Test data: %s trades (%d)\n", format(nrow(dt_test), big.mark = ","), TEST_YEAR))

cat(sprintf("\nTraining set (Risk-Adjusted PnL):\n"))
cat(sprintf("  Mean: %.4f\n", mean(dt_train$pnl_risk_adj)))
cat(sprintf("  Std:  %.4f\n", sd(dt_train$pnl_risk_adj)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_train$pnl_risk_adj > 0)))

cat(sprintf("\nTest set (Risk-Adjusted PnL):\n"))
cat(sprintf("  Mean: %.4f\n", mean(dt_test$pnl_risk_adj)))
cat(sprintf("  Std:  %.4f\n", sd(dt_test$pnl_risk_adj)))
cat(sprintf("  Win Rate: %.1f%%\n", 100 * mean(dt_test$pnl_risk_adj > 0)))

# ===== STEP 9: DEFINE CANDIDATE FEATURES =====================================

cat("\n=== STEP 9: DEFINE CANDIDATE FEATURES ===\n")

# 30 curated meta-features
candidate_features <- c(
  # Signal quality (4)
  "clarity_gap", "dominant_prob", "pred_prob_long", "pred_prob_short",

  # Volatility (5)
  "atr_14", "atr_14_pct", "bb_bandwidth_20", "vhf_28", "choppiness_14",

  # Trend (6)
  "adx_14", "di_diff_14", "ema_21_slope", "aroon_oscillator",
  "ichimoku_position", "donchian_position_20",

  # Momentum (7)
  "rsi_14", "rsi_14_slope", "stoch_k", "stoch_k_d_diff",
  "roc_10", "momentum_10", "dpo_20",

  # Volume (2)
  "volume_ratio", "obv_slope",

  # Mean reversion (2)
  "bb_pct_20", "kc_position_20",

  # Time/Session (3)
  "hour", "day_of_week", "session_overlap",

  # Trade context (1)
  "is_long"
)

# Filter to available features
available_features <- intersect(candidate_features, names(dt_train))
cat(sprintf("Candidate features: %d defined, %d available\n",
            length(candidate_features), length(available_features)))

# Remove features with too many NAs
na_counts <- sapply(available_features, function(f) sum(is.na(dt_train[[f]])))
na_pct <- na_counts / nrow(dt_train)
valid_features <- available_features[na_pct < 0.1]  # Max 10% NA

cat(sprintf("Features with <10%% NA: %d\n", length(valid_features)))

# ===== STEP 10: BORUTA FEATURE SELECTION =====================================

cat("\n=== STEP 10: BORUTA FEATURE SELECTION ===\n")

# Prepare data for Boruta - use RISK-ADJUSTED PnL as target
train_for_boruta <- dt_train[complete.cases(dt_train[, ..valid_features])]
cat(sprintf("Training samples for Boruta: %d\n", nrow(train_for_boruta)))

# Create formula
X_boruta <- as.data.frame(train_for_boruta[, ..valid_features])
y_boruta <- train_for_boruta$pnl_risk_adj  # RISK-ADJUSTED TARGET

cat(sprintf("\nRunning Boruta with %d parallel workers...\n", getDoParWorkers()))
cat("Target: pnl_risk_adj (risk-adjusted returns)\n")

set.seed(42)
boruta_result <- Boruta(
  x = X_boruta,
  y = y_boruta,
  maxRuns = 100,
  doTrace = 1,
  num.trees = 500,
  num.threads = n_cores  # Parallel execution within ranger
)

# Get selected features
boruta_decision <- boruta_result$finalDecision
confirmed_features <- names(boruta_decision[boruta_decision == "Confirmed"])
tentative_features <- names(boruta_decision[boruta_decision == "Tentative"])
rejected_features <- names(boruta_decision[boruta_decision == "Rejected"])

cat(sprintf("\n--- Boruta Results ---\n"))
cat(sprintf("Confirmed features (%d): %s\n", length(confirmed_features),
            paste(confirmed_features, collapse = ", ")))
cat(sprintf("Tentative features (%d): %s\n", length(tentative_features),
            paste(tentative_features, collapse = ", ")))
cat(sprintf("Rejected features (%d): %d\n", length(rejected_features), length(rejected_features)))

# Use confirmed + tentative features
selected_features <- c(confirmed_features, tentative_features)

if (length(selected_features) == 0) {
  cat("WARNING: No features selected by Boruta. Using top 10 by importance.\n")
  # Fallback: use importance from Boruta
  imp <- attStats(boruta_result)
  imp <- imp[order(-imp$meanImp), ]
  selected_features <- rownames(imp)[1:min(10, nrow(imp))]
}

cat(sprintf("\nFinal selected features (%d): %s\n",
            length(selected_features), paste(selected_features, collapse = ", ")))

# Save Boruta results
boruta_importance <- attStats(boruta_result)
boruta_importance$feature <- rownames(boruta_importance)
boruta_importance <- as.data.table(boruta_importance)
setorder(boruta_importance, -meanImp)

boruta_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_boruta_importance_risk_adj_", LABEL_VERSION, ".csv"))
fwrite(boruta_importance, boruta_file)
cat(sprintf("Boruta importance saved: %s\n", boruta_file))

# ===== STEP 11: TRAIN FINAL QUANTILE MODEL ===================================

cat("\n=== STEP 11: TRAIN FINAL QUANTILE MODEL ===\n")

# Prepare training data
train_complete <- dt_train[complete.cases(dt_train[, ..selected_features])]
test_complete <- dt_test[complete.cases(dt_test[, ..selected_features])]

cat(sprintf("Training samples: %d\n", nrow(train_complete)))
cat(sprintf("Test samples: %d\n", nrow(test_complete)))

X_train <- as.matrix(train_complete[, ..selected_features])
y_train <- train_complete$pnl_risk_adj  # RISK-ADJUSTED TARGET

X_test <- as.matrix(test_complete[, ..selected_features])
y_test <- test_complete$pnl_risk_adj  # RISK-ADJUSTED TARGET

# Split for early stopping
set.seed(42)
val_idx <- sample(1:nrow(X_train), size = floor(0.2 * nrow(X_train)))
train_idx <- setdiff(1:nrow(X_train), val_idx)

dtrain <- xgb.DMatrix(data = X_train[train_idx, , drop = FALSE], label = y_train[train_idx])
dval <- xgb.DMatrix(data = X_train[val_idx, , drop = FALSE], label = y_train[val_idx])

# Train with early stopping
cat("Training XGBoost with early stopping...\n")
cat("Target: pnl_risk_adj (risk-adjusted returns)\n")

model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 1000,
  evals = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 100
)

best_iter <- model$best_iteration
if (is.null(best_iter) || length(best_iter) == 0) {
  best_iter <- 200
}
cat(sprintf("\nBest iteration: %d\n", best_iter))

# Retrain on full training data
dtrain_full <- xgb.DMatrix(data = X_train, label = y_train)
final_model <- xgb.train(
  params = xgb_params,
  data = dtrain_full,
  nrounds = best_iter,
  verbose = 0
)

# Save model
model_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_quantile_model_risk_adj_", LABEL_VERSION, ".json"))
xgb.save(final_model, model_file)
cat(sprintf("Model saved: %s\n", model_file))

# Feature importance
importance <- xgb.importance(feature_names = selected_features, model = final_model)
cat("\nXGBoost Feature Importance:\n")
print(importance)

# ===== STEP 12: EVALUATE ON TEST SET =========================================

cat("\n=== STEP 12: EVALUATE ON TEST SET ===\n")

# Predictions
dtest <- xgb.DMatrix(data = X_test, label = y_test)
pred_train <- predict(final_model, dtrain_full)
pred_test <- predict(final_model, dtest)

# Spearman correlations (for risk-adjusted target)
spearman_train <- cor(pred_train, y_train, method = "spearman", use = "complete.obs")
spearman_test <- cor(pred_test, y_test, method = "spearman", use = "complete.obs")

cat(sprintf("\nSpearman Correlation (Risk-Adjusted PnL):\n"))
cat(sprintf("  Training: %.4f\n", spearman_train))
cat(sprintf("  Test:     %.4f\n", spearman_test))
cat(sprintf("  Diff:     %+.4f\n", spearman_test - spearman_train))

# Also check correlation with raw PnL
spearman_test_raw <- cor(pred_test, test_complete$pnl, method = "spearman", use = "complete.obs")
cat(sprintf("\nSpearman Correlation (Raw PnL):\n"))
cat(sprintf("  Test:     %.4f\n", spearman_test_raw))

# Add predictions to test data
test_complete[, pred_q75_risk_adj := pred_test]
test_complete[, actual_pnl_risk_adj := y_test]
test_complete[, actual_pnl := pnl]

# ===== STEP 13: BINNING ANALYSIS =============================================

cat("\n=== STEP 13: BINNING ANALYSIS ===\n")

# Create 5 quantile bins
test_complete[, pred_bin := ntile(pred_q75_risk_adj, 5)]

# Analyze by bin - for BOTH risk-adjusted and raw PnL
bin_analysis <- test_complete[, .(
  mean_pred = mean(pred_q75_risk_adj),
  mean_pnl_risk_adj = mean(actual_pnl_risk_adj),
  mean_pnl_raw = mean(actual_pnl),
  median_pnl_risk_adj = median(actual_pnl_risk_adj),
  q75_pnl_risk_adj = quantile(actual_pnl_risk_adj, 0.75),
  win_rate = mean(actual_pnl > 0),
  total_pnl_raw = sum(actual_pnl),
  n = .N
), by = pred_bin][order(pred_bin)]

cat("\n--- Binning Analysis (5 Quantile Bins) ---\n")
print(bin_analysis)

# Check monotonicity for risk-adjusted
monotonic_check_risk_adj <- all(diff(bin_analysis$mean_pnl_risk_adj) >= 0)
cat(sprintf("\nMonotonicity check (risk-adj mean_pnl increases with bin): %s\n",
            ifelse(monotonic_check_risk_adj, "PASSED", "FAILED")))

# Check monotonicity for raw pnl
monotonic_check_raw <- all(diff(bin_analysis$mean_pnl_raw) >= 0)
cat(sprintf("Monotonicity check (raw mean_pnl increases with bin): %s\n",
            ifelse(monotonic_check_raw, "PASSED", "FAILED")))

# Spread between top and bottom bin
spread_risk_adj <- bin_analysis[pred_bin == 5]$mean_pnl_risk_adj - bin_analysis[pred_bin == 1]$mean_pnl_risk_adj
spread_raw <- bin_analysis[pred_bin == 5]$mean_pnl_raw - bin_analysis[pred_bin == 1]$mean_pnl_raw
cat(sprintf("Spread Risk-Adj (Bin 5 - Bin 1): %.4f\n", spread_risk_adj))
cat(sprintf("Spread Raw (Bin 5 - Bin 1): %.6f\n", spread_raw))

# ===== STEP 14: THRESHOLD ANALYSIS ===========================================

cat("\n=== STEP 14: THRESHOLD ANALYSIS ===\n")

# Use quantiles of predictions as thresholds
pred_quantiles <- quantile(test_complete$pred_q75_risk_adj, probs = c(0, 0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
threshold_candidates <- as.numeric(pred_quantiles)

threshold_results <- data.table(
  threshold = numeric(),
  n_trades = integer(),
  pct_trades = numeric(),
  mean_pnl_risk_adj = numeric(),
  mean_pnl_raw = numeric(),
  total_pnl_raw = numeric(),
  win_rate = numeric()
)

cat("\n--- Threshold Comparison ---\n")
cat(sprintf("%-12s %10s %10s %12s %12s %12s %10s\n",
            "Threshold", "Trades", "% Kept", "Mean RA PnL", "Mean Raw", "Total Raw", "Win Rate"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (thresh in threshold_candidates) {
  filtered <- test_complete[pred_q75_risk_adj > thresh]

  if (nrow(filtered) > 0) {
    result <- data.table(
      threshold = thresh,
      n_trades = nrow(filtered),
      pct_trades = 100 * nrow(filtered) / nrow(test_complete),
      mean_pnl_risk_adj = mean(filtered$actual_pnl_risk_adj),
      mean_pnl_raw = mean(filtered$actual_pnl),
      total_pnl_raw = sum(filtered$actual_pnl),
      win_rate = mean(filtered$actual_pnl > 0)
    )
    threshold_results <- rbind(threshold_results, result)

    cat(sprintf("%-12.4f %10d %9.1f%% %12.4f %12.6f %12.6f %9.1f%%\n",
                thresh, result$n_trades, result$pct_trades,
                result$mean_pnl_risk_adj, result$mean_pnl_raw,
                result$total_pnl_raw, result$win_rate * 100))
  }
}

# Find optimal threshold (based on raw PnL since that's what we trade)
optimal_thresh <- threshold_results[pct_trades >= 30][which.max(mean_pnl_raw)]$threshold
if (length(optimal_thresh) == 0) optimal_thresh <- min(threshold_candidates)

cat(sprintf("\nRecommended threshold (max raw mean_pnl with >=30%% trades): %.4f\n", optimal_thresh))

# ===== STEP 15: COMPARISON VS UNFILTERED =====================================

cat("\n=== STEP 15: COMPARISON VS UNFILTERED ===\n")

unfiltered_stats <- test_complete[, .(
  n_trades = .N,
  mean_pnl_risk_adj = mean(actual_pnl_risk_adj),
  mean_pnl_raw = mean(actual_pnl),
  total_pnl_raw = sum(actual_pnl),
  win_rate = mean(actual_pnl > 0)
)]

filtered_data <- test_complete[pred_q75_risk_adj > optimal_thresh]
filtered_stats <- filtered_data[, .(
  n_trades = .N,
  mean_pnl_risk_adj = mean(actual_pnl_risk_adj),
  mean_pnl_raw = mean(actual_pnl),
  total_pnl_raw = sum(actual_pnl),
  win_rate = mean(actual_pnl > 0)
)]

rejected_data <- test_complete[pred_q75_risk_adj <= optimal_thresh]
rejected_stats <- if (nrow(rejected_data) > 0) {
  rejected_data[, .(
    n_trades = .N,
    mean_pnl_risk_adj = mean(actual_pnl_risk_adj),
    mean_pnl_raw = mean(actual_pnl),
    total_pnl_raw = sum(actual_pnl),
    win_rate = mean(actual_pnl > 0)
  )]
} else {
  data.table(n_trades = 0, mean_pnl_risk_adj = NA, mean_pnl_raw = NA, total_pnl_raw = 0, win_rate = NA)
}

cat(sprintf("\n                    UNFILTERED    FILTERED      REJECTED\n"))
cat(sprintf("                    (All)         (pred>%.2f)  (pred<=%.2f)\n", optimal_thresh, optimal_thresh))
cat(paste(rep("-", 65), collapse = ""), "\n")
cat(sprintf("Number of Trades:   %-14d%-14d%d\n",
            unfiltered_stats$n_trades, filtered_stats$n_trades, rejected_stats$n_trades))
cat(sprintf("Mean RA PnL:        %-14.4f%-14.4f%.4f\n",
            unfiltered_stats$mean_pnl_risk_adj, filtered_stats$mean_pnl_risk_adj, rejected_stats$mean_pnl_risk_adj))
cat(sprintf("Mean Raw PnL:       %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$mean_pnl_raw, filtered_stats$mean_pnl_raw, rejected_stats$mean_pnl_raw))
cat(sprintf("Total Raw PnL:      %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$total_pnl_raw, filtered_stats$total_pnl_raw, rejected_stats$total_pnl_raw))
cat(sprintf("Win Rate:           %-13.1f%%%-13.1f%%%.1f%%\n",
            unfiltered_stats$win_rate * 100, filtered_stats$win_rate * 100, rejected_stats$win_rate * 100))

# ===== STEP 16: SAVE OUTPUTS =================================================

cat("\n=== STEP 16: SAVE OUTPUTS ===\n")

# Feature Selection Results
fs_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_selected_features_risk_adj_", LABEL_VERSION, ".csv"))
fwrite(data.table(feature = selected_features), fs_file)
cat(sprintf("Selected features saved: %s\n", fs_file))

# Binning Analysis
bin_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_binning_analysis_risk_adj_", LABEL_VERSION, ".csv"))
fwrite(bin_analysis, bin_file)
cat(sprintf("Binning analysis saved: %s\n", bin_file))

# Threshold Results
thresh_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_threshold_analysis_risk_adj_", LABEL_VERSION, ".csv"))
fwrite(threshold_results, thresh_file)
cat(sprintf("Threshold analysis saved: %s\n", thresh_file))

# Test Predictions
output_cols <- c("datetime", "signal", "pnl", "pnl_risk_adj", "atr_for_adj",
                 "pred_prob_long", "pred_prob_short",
                 "pred_q75_risk_adj", "pred_bin", selected_features)
output_cols <- intersect(output_cols, names(test_complete))

filtered_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_test_predictions_risk_adj_", LABEL_VERSION, ".csv"))
fwrite(test_complete[, ..output_cols], filtered_file)
cat(sprintf("Test predictions saved: %s\n", filtered_file))

# ===== STEP 17: VISUALIZATIONS ===============================================

cat("\n=== STEP 17: VISUALIZATIONS ===\n")

# Feature Importance Plot
cat("Creating feature importance plot...\n")

p_importance <- ggplot(importance[1:min(20, nrow(importance))], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = sprintf("Quantile Regression Feature Importance (Risk-Adjusted) - %s %s", EPIC, INTERVAL),
    subtitle = sprintf("Top %d features (Boruta selected) | Target: pnl/ATR", min(20, nrow(importance))),
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

importance_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_feature_importance_risk_adj_", LABEL_VERSION, ".png"))
ggsave(importance_file, p_importance, width = 10, height = 8, dpi = 300)
cat(sprintf("Feature importance plot saved: %s\n", importance_file))

# Binning Plot - show both raw and risk-adjusted
cat("Creating binning analysis plot...\n")

p_bins <- ggplot(bin_analysis, aes(x = factor(pred_bin))) +
  geom_bar(aes(y = mean_pnl_raw * 1000, fill = "Mean Raw PnL (x1000)"), stat = "identity", alpha = 0.7) +
  geom_point(aes(y = win_rate, color = "Win Rate"), size = 4) +
  geom_line(aes(y = win_rate, group = 1, color = "Win Rate"), linewidth = 1) +
  scale_y_continuous(
    name = "Mean Raw PnL (x1000)",
    sec.axis = sec_axis(~., name = "Win Rate")
  ) +
  scale_fill_manual(values = c("Mean Raw PnL (x1000)" = "darkgreen")) +
  scale_color_manual(values = c("Win Rate" = "coral")) +
  labs(
    title = sprintf("Performance by Risk-Adj Predicted Q75 Bin - %s %s (Test %d)", EPIC, INTERVAL, TEST_YEAR),
    subtitle = "Model predicts Q75 of risk-adjusted returns, evaluated on raw PnL",
    x = "Prediction Quintile (1=Low, 5=High)",
    fill = "", color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

bins_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_binning_plot_risk_adj_", LABEL_VERSION, ".png"))
ggsave(bins_file, p_bins, width = 10, height = 6, dpi = 300)
cat(sprintf("Binning plot saved: %s\n", bins_file))

# Boruta Importance Plot
cat("Creating Boruta importance plot...\n")

boruta_top <- boruta_importance[1:min(30, nrow(boruta_importance))]
boruta_top[, decision_color := fifelse(decision == "Confirmed", "Confirmed",
                                        fifelse(decision == "Tentative", "Tentative", "Rejected"))]

p_boruta <- ggplot(boruta_top, aes(x = reorder(feature, meanImp), y = meanImp, fill = decision_color)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("Confirmed" = "darkgreen", "Tentative" = "orange", "Rejected" = "red")) +
  labs(
    title = sprintf("Boruta Feature Importance (Risk-Adjusted Target) - %s %s", EPIC, INTERVAL),
    subtitle = "Target: pnl / ATR (volatility-normalized returns)",
    x = "Feature",
    y = "Mean Importance",
    fill = "Decision"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

boruta_plot_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_boruta_plot_risk_adj_", LABEL_VERSION, ".png"))
ggsave(boruta_plot_file, p_boruta, width = 10, height = 10, dpi = 300)
cat(sprintf("Boruta plot saved: %s\n", boruta_plot_file))

# ===== STEP 18: ANALYSIS REPORT ==============================================

cat("\n=== STEP 18: SAVE ANALYSIS REPORT ===\n")

report_file <- file.path(qr_output_path, paste0(EPIC, "_", INTERVAL, "_quantile_analysis_report_risk_adj_", LABEL_VERSION, ".txt"))

sink(report_file)

cat("================================================================================\n")
cat("      QUANTILE REGRESSION META-LABELLING ANALYSIS REPORT (RISK-ADJUSTED)\n")
cat("================================================================================\n\n")

cat("================================================================================\n")
cat("1. CONTEXT & APPROACH\n")
cat("================================================================================\n\n")

cat(sprintf("Asset: %s (Gold futures)\n", EPIC))
cat(sprintf("Timeframe: %s (15-minute bars)\n", INTERVAL))
cat(sprintf("Test Year: %d (out-of-sample)\n", TEST_YEAR))
cat(sprintf("Training Period: All data before %d\n\n", TEST_YEAR))

cat("APPROACH:\n")
cat("Quantile Regression predicting the 75th percentile of RISK-ADJUSTED PnL.\n")
cat("Risk-Adjusted PnL = raw_pnl / ATR_14\n")
cat("This normalizes returns by current volatility, making predictions\n")
cat("more stable across different market regimes.\n\n")

cat("Feature selection via Boruta algorithm (Random Forest based).\n\n")

cat("XGBoost Parameters:\n")
cat(sprintf("  objective: reg:quantileerror\n"))
cat(sprintf("  quantile_alpha: %.2f\n", QUANTILE_ALPHA))
cat(sprintf("  max_depth: %d\n", xgb_params$max_depth))
cat(sprintf("  eta: %.3f\n", xgb_params$eta))
cat(sprintf("  min_child_weight: %d\n\n", xgb_params$min_child_weight))

cat("================================================================================\n")
cat("2. DATA SUMMARY\n")
cat("================================================================================\n\n")

cat(sprintf("Training trades: %s (before %d)\n", format(nrow(dt_train), big.mark = ","), TEST_YEAR))
cat(sprintf("Test trades: %s (%d)\n\n", format(nrow(dt_test), big.mark = ","), TEST_YEAR))

cat("Training set performance (Risk-Adjusted):\n")
cat(sprintf("  Mean RA PnL: %.4f\n", mean(dt_train$pnl_risk_adj)))
cat(sprintf("  Std RA PnL:  %.4f\n", sd(dt_train$pnl_risk_adj)))
cat(sprintf("  Win Rate: %.1f%%\n\n", 100 * mean(dt_train$pnl_risk_adj > 0)))

cat("Test set performance (Risk-Adjusted):\n")
cat(sprintf("  Mean RA PnL: %.4f\n", mean(dt_test$pnl_risk_adj)))
cat(sprintf("  Std RA PnL:  %.4f\n", sd(dt_test$pnl_risk_adj)))
cat(sprintf("  Win Rate: %.1f%%\n\n", 100 * mean(dt_test$pnl_risk_adj > 0)))

cat("================================================================================\n")
cat("3. BORUTA FEATURE SELECTION\n")
cat("================================================================================\n\n")

cat(sprintf("Candidate features tested: %d\n", length(valid_features)))
cat(sprintf("Confirmed features: %d\n", length(confirmed_features)))
cat(sprintf("Tentative features: %d\n", length(tentative_features)))
cat(sprintf("Rejected features: %d\n\n", length(rejected_features)))

cat("Selected features:\n")
for (f in selected_features) {
  cat(sprintf("  - %s\n", f))
}

cat("\n\nBoruta Importance (Top 20):\n")
print(boruta_importance[1:min(20, nrow(boruta_importance)), .(feature, meanImp, decision)])

cat("\n")

cat("================================================================================\n")
cat("4. MODEL PERFORMANCE\n")
cat("================================================================================\n\n")

cat("Spearman Correlation (Risk-Adjusted Target):\n")
cat(sprintf("  Training: %.4f\n", spearman_train))
cat(sprintf("  Test:     %.4f\n", spearman_test))
cat(sprintf("  Diff:     %+.4f\n\n", spearman_test - spearman_train))

cat("Spearman Correlation (Raw PnL - what we actually trade):\n")
cat(sprintf("  Test:     %.4f\n\n", spearman_test_raw))

cat("Interpretation:\n")
cat("  0.00: No relationship\n")
cat("  0.10: Weak positive\n")
cat("  0.20: Moderate (good for finance)\n")
cat("  0.30+: Strong (rare)\n\n")

cat("================================================================================\n")
cat("5. XGBOOST FEATURE IMPORTANCE\n")
cat("================================================================================\n\n")

print(importance)

cat("\n")

cat("================================================================================\n")
cat("6. BINNING ANALYSIS\n")
cat("================================================================================\n\n")

print(bin_analysis[, .(
  pred_bin, n,
  mean_pred = round(mean_pred, 4),
  mean_pnl_risk_adj = round(mean_pnl_risk_adj, 4),
  mean_pnl_raw = round(mean_pnl_raw, 6),
  win_rate = round(win_rate, 4)
)])

cat(sprintf("\n\nMonotonicity (Risk-Adj): %s\n", ifelse(monotonic_check_risk_adj, "PASSED", "FAILED")))
cat(sprintf("Monotonicity (Raw PnL): %s\n", ifelse(monotonic_check_raw, "PASSED", "FAILED")))
cat(sprintf("Spread Risk-Adj (Bin 5 - Bin 1): %.4f\n", spread_risk_adj))
cat(sprintf("Spread Raw (Bin 5 - Bin 1): %.6f\n", spread_raw))

cat("\n")

cat("================================================================================\n")
cat("7. THRESHOLD ANALYSIS\n")
cat("================================================================================\n\n")

print(threshold_results[, .(
  threshold = round(threshold, 4),
  n_trades,
  pct_trades = round(pct_trades, 1),
  mean_pnl_risk_adj = round(mean_pnl_risk_adj, 4),
  mean_pnl_raw = round(mean_pnl_raw, 6),
  win_rate = round(win_rate, 4)
)])

cat(sprintf("\n\nRecommended threshold: %.4f\n", optimal_thresh))

cat("\n")

cat("================================================================================\n")
cat("8. FILTERED VS UNFILTERED\n")
cat("================================================================================\n\n")

cat(sprintf("                    UNFILTERED    FILTERED      REJECTED\n"))
cat(paste(rep("-", 65), collapse = ""), "\n")
cat(sprintf("Trades:             %-14d%-14d%d\n",
            unfiltered_stats$n_trades, filtered_stats$n_trades, rejected_stats$n_trades))
cat(sprintf("Mean RA PnL:        %-14.4f%-14.4f%.4f\n",
            unfiltered_stats$mean_pnl_risk_adj, filtered_stats$mean_pnl_risk_adj, rejected_stats$mean_pnl_risk_adj))
cat(sprintf("Mean Raw PnL:       %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$mean_pnl_raw, filtered_stats$mean_pnl_raw, rejected_stats$mean_pnl_raw))
cat(sprintf("Total Raw PnL:      %-14.6f%-14.6f%.6f\n",
            unfiltered_stats$total_pnl_raw, filtered_stats$total_pnl_raw, rejected_stats$total_pnl_raw))
cat(sprintf("Win Rate:           %-13.1f%%%-13.1f%%%.1f%%\n",
            unfiltered_stats$win_rate * 100, filtered_stats$win_rate * 100, rejected_stats$win_rate * 100))

cat("\n")

cat("================================================================================\n")
cat("9. COMPARISON: RISK-ADJUSTED VS RAW APPROACH\n")
cat("================================================================================\n\n")

cat("RATIONALE FOR RISK-ADJUSTED TARGET:\n")
cat("- Raw PnL varies greatly with market volatility\n")
cat("- In high-vol periods: large returns but also large losses\n")
cat("- In low-vol periods: small but potentially more consistent returns\n")
cat("- Risk-adjusted (PnL/ATR) normalizes for volatility regime\n")
cat("- Should produce more stable predictions across market conditions\n\n")

cat("EXPECTED BENEFITS:\n")
cat("- Better generalization across different volatility regimes\n")
cat("- More consistent feature importance\n")
cat("- Potentially better out-of-sample performance\n\n")

cat("POTENTIAL DRAWBACKS:\n")
cat("- May filter out high-vol/high-return opportunities\n")
cat("- Correlation with raw PnL may be lower\n")
cat("- Need to track both metrics for evaluation\n\n")

cat("================================================================================\n")
cat("10. QUESTIONS FOR ANALYSIS\n")
cat("================================================================================\n\n")

cat(sprintf("1. Is Spearman correlation of %.4f (risk-adj) meaningful?\n\n", spearman_test))
cat(sprintf("2. How does it compare to raw PnL correlation (%.4f)?\n\n", spearman_test_raw))
cat("3. Does the binning show proper monotonicity for BOTH metrics?\n\n")
cat(sprintf("4. Overfitting check: Train=%.4f vs Test=%.4f\n\n", spearman_train, spearman_test))
cat("5. Do the selected features differ from the raw PnL model?\n\n")
cat("6. Is the risk-adjusted approach better for filtering trades?\n\n")
cat("7. Recommendations: use risk-adjusted or raw PnL model?\n\n")

cat("================================================================================\n")
cat("END OF REPORT\n")
cat("================================================================================\n")

sink()

cat(sprintf("Analysis report saved: %s\n", report_file))

# ===== DONE ==================================================================

# Cleanup parallel cluster
stopCluster(cl)

cat("\n=== QUANTILE REGRESSION (RISK-ADJUSTED) COMPLETE ===\n")
cat(sprintf("\nKey Results:\n"))
cat(sprintf("  Boruta selected features: %d\n", length(selected_features)))
cat(sprintf("  Spearman Correlation (risk-adj, test): %.4f\n", spearman_test))
cat(sprintf("  Spearman Correlation (raw pnl, test): %.4f\n", spearman_test_raw))
cat(sprintf("  Recommended Threshold: %.4f\n", optimal_thresh))
cat(sprintf("  Monotonicity Check (risk-adj): %s\n", ifelse(monotonic_check_risk_adj, "PASSED", "FAILED")))
cat(sprintf("  Monotonicity Check (raw): %s\n", ifelse(monotonic_check_raw, "PASSED", "FAILED")))
cat(sprintf("\nOutputs saved to: %s\n", qr_output_path))
