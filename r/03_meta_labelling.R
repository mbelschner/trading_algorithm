# ============================================================================
# META-LABELLING - SIGNAL QUALITY PREDICTION
# ============================================================================
#
# PURPOSE:
# - Train a meta-model to predict which primary signals will be profitable
# - Use walk-forward validation (expanding window) like primary models
# - Filter signals based on meta-model confidence
# - Compare performance with and without meta-filtering
#
# ============================================================================

cat("\n=== META-LABELLING - SIGNAL QUALITY PREDICTION ===\n")

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
data.table,      # Fast data manipulation
xgboost,         # Meta-model training
ggplot2,         # Visualization
pROC,            # ROC curves and AUC
scales,          # Plot formatting
progress,        # Progress bars
tictoc,          # Timing
jsonlite         # JSON export
)

# ===== PATHS =================================================================

price_data_path <- file.path("price_data")
labelled_data_path <- file.path("labelled_data")
backtest_output_path <- file.path("backtest_results")
models_path <- file.path(backtest_output_path, "models")
pnl_output_path <- file.path(backtest_output_path, "pnl_simulation")
meta_output_path <- file.path(backtest_output_path, "meta_labelling")

# Create output folder
if (!dir.exists(meta_output_path)) {
dir.create(meta_output_path, recursive = TRUE)
}

# ===== CONFIGURATION =========================================================

EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
LABEL_VERSION <- "enhanced_neutral"
TEST_YEAR <- 2025

# Walk-forward parameters
INITIAL_TRAIN_MONTHS <- 6  # Start with 6 months of data
VALIDATION_MONTHS <- 1     # Validate on 1 month at a time

# Meta-model threshold candidates
THRESHOLD_CANDIDATES <- seq(0.4, 0.8, by = 0.05)

cat(sprintf("\nConfiguration:\n"))
cat(sprintf("  Epic: %s\n", EPIC))
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Label Version: %s\n", LABEL_VERSION))
cat(sprintf("  Test Year: %d\n", TEST_YEAR))

# ===== STEP 1: LOAD DATA =====================================================

cat("\n=== STEP 1: LOAD DATA ===\n")

# --- 1.1: Load Raw Price Data ---
cat("\n--- Loading Raw Price Data ---\n")

prices_file <- file.path(price_data_path, paste0(EPIC, "_", INTERVAL, ".csv"))
cat(sprintf("Loading: %s\n", prices_file))

dt_prices <- fread(prices_file)
setDT(dt_prices)

if ("time" %in% names(dt_prices)) {
setnames(dt_prices, "time", "datetime")
}

if (is.character(dt_prices$datetime)) {
dt_prices[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Price data loaded: %s rows\n", format(nrow(dt_prices), big.mark = ",")))

# --- 1.2: Load Labels ---
cat("\n--- Loading Labels ---\n")

label_file_map <- list(
"enhanced_neutral" = paste0(EPIC, "_", INTERVAL, "_labeled_enhanced_neutral.csv"),
"raw" = paste0(EPIC, "_", INTERVAL, "_labeled_raw.csv"),
"standard" = paste0(EPIC, "_", INTERVAL, "_labeled.csv"),
"unfiltered" = paste0(EPIC, "_", INTERVAL, "_labeled_unfiltered.csv")
)

labels_file <- file.path(labelled_data_path, label_file_map[[LABEL_VERSION]])
cat(sprintf("Loading labels: %s\n", labels_file))

dt_labels <- fread(labels_file)
setDT(dt_labels)

if (is.character(dt_labels$datetime)) {
dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("Labels loaded: %s rows\n", format(nrow(dt_labels), big.mark = ",")))

# --- 1.3: Calculate Features ---
cat("\n--- Calculating Features ---\n")

source("r/02_01_indicator_calculation.R")
source("r/02_01b_additional_markets.R")
source("r/02_02_feature_engineering.R")

tic()
dt_indicators <- calculate_all_indicators(
dt = copy(dt_prices),
ema_periods = c(9, 21, 50, 100),
rsi_periods = c(14, 28),
atr_periods = c(14, 28),
adx_periods = c(14),
bb_periods = c(20),
kc_periods = c(20),
verbose = TRUE
)
toc()

tic()
dt_features <- engineer_features(
dt = dt_indicators,
lag_periods = c(1, 2, 3, 5, 10),
derivative_orders = c(1, 2),
hourly_aggregation = TRUE,
rolling_windows = c(10, 20, 50),
interaction_features = TRUE,
verbose = TRUE
)
toc()

# Additional markets
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)

if (!is.null(ADDITIONAL_MARKETS) && length(ADDITIONAL_MARKETS) > 0) {
dt_additional_markets <- load_additional_markets(
  markets = ADDITIONAL_MARKETS,
  interval = INTERVAL,
  price_data_path = price_data_path,
  lag_periods = ADDITIONAL_MARKETS_LAG_PERIODS,
  verbose = TRUE
)

if (!is.null(dt_additional_markets)) {
  dt_features <- merge_additional_markets(
    dt_main = dt_features,
    dt_additional = dt_additional_markets,
    verbose = TRUE
  )
}
}

# Remove NAs
dt_features <- na.omit(dt_features)
cat(sprintf("Features calculated: %d columns, %s rows\n",
          ncol(dt_features), format(nrow(dt_features), big.mark = ",")))

# --- 1.4: Load Trained Models & Generate Predictions ---
cat("\n--- Loading Primary Models ---\n")

model_long_file <- file.path(models_path, paste0(EPIC, "_", INTERVAL, "_model_long_", LABEL_VERSION, ".json"))
model_short_file <- file.path(models_path, paste0(EPIC, "_", INTERVAL, "_model_short_", LABEL_VERSION, ".json"))

if (!file.exists(model_long_file) || !file.exists(model_short_file)) {
stop("Primary models not found. Run 02_training.R first.")
}

model_long <- xgb.load(model_long_file)
model_short <- xgb.load(model_short_file)

model_long_json <- fromJSON(model_long_file)
model_short_json <- fromJSON(model_short_file)

features_long <- model_long_json$learner$feature_names
features_short <- model_short_json$learner$feature_names

cat(sprintf("Long model features: %d\n", length(features_long)))
cat(sprintf("Short model features: %d\n", length(features_short)))

# Generate predictions
cat("\n--- Generating Primary Predictions ---\n")

X_long <- as.matrix(dt_features[, ..features_long])
X_short <- as.matrix(dt_features[, ..features_short])

dt_features[, pred_prob_long := predict(model_long, xgb.DMatrix(X_long))]
dt_features[, pred_prob_short := predict(model_short, xgb.DMatrix(X_short))]

# Generate signals
THRESHOLD <- 0.5
dt_features[, signal := fcase(
pred_prob_long > THRESHOLD & pred_prob_short <= THRESHOLD, 1L,
pred_prob_short > THRESHOLD & pred_prob_long <= THRESHOLD, -1L,
pred_prob_long > THRESHOLD & pred_prob_short > THRESHOLD & pred_prob_long > pred_prob_short, 1L,
pred_prob_long > THRESHOLD & pred_prob_short > THRESHOLD & pred_prob_short >= pred_prob_long, -1L,
default = 0L
)]

cat(sprintf("Signals generated: Long=%d, Short=%d, Neutral=%d\n",
          sum(dt_features$signal == 1),
          sum(dt_features$signal == -1),
          sum(dt_features$signal == 0)))

# --- 1.5: Merge Labels & Calculate PnL ---
cat("\n--- Merging Labels ---\n")

label_cols <- c("datetime", "label", "barrier_touched", "bars_to_exit", "log_return")
available_label_cols <- intersect(label_cols, names(dt_labels))

dt_data <- merge(
dt_features,
dt_labels[, ..available_label_cols],
by = "datetime",
all = FALSE
)

# Calculate PnL for each signal
setorder(dt_data, datetime)
dt_data[, idx := .I]
dt_data[, exit_idx := idx + bars_to_exit]
dt_data[, entry_price := close]
dt_data[, exit_price := dt_data$close[pmin(exit_idx, nrow(dt_data))]]
dt_data[, log_return_calc := log(exit_price / entry_price)]
dt_data[, pnl := signal * log_return_calc]
dt_data[signal == 0, pnl := 0]

cat(sprintf("Merged dataset: %s rows\n", format(nrow(dt_data), big.mark = ",")))

# ===== STEP 2: PREPARE META-LABELLING DATA ===================================

cat("\n=== STEP 2: PREPARE META-LABELLING DATA ===\n")

# Filter to trades only (non-zero signals)
dt_trades <- dt_data[signal != 0]
cat(sprintf("Total trades: %s\n", format(nrow(dt_trades), big.mark = ",")))

# --- 2.1: Create Target Variable ---
cat("\n--- Creating Target Variable ---\n")

# Target: Was the trade profitable?
dt_trades[, target := as.integer(pnl > 0)]

cat(sprintf("Target distribution: Profitable=%d (%.1f%%), Not Profitable=%d (%.1f%%)\n",
          sum(dt_trades$target == 1), 100 * mean(dt_trades$target == 1),
          sum(dt_trades$target == 0), 100 * mean(dt_trades$target == 0)))

# --- 2.2: Create Meta-Features ---
cat("\n--- Creating Meta-Features ---\n")

# Clarity gap: How different are long vs short probabilities
dt_trades[, clarity_gap := abs(pred_prob_long - pred_prob_short)]

# ATR ratio: Short-term vs long-term volatility
if ("atr_14" %in% names(dt_trades) && "atr_28" %in% names(dt_trades)) {
dt_trades[, atr_ratio := atr_14 / atr_28]
} else {
dt_trades[, atr_ratio := NA_real_]
cat("  Warning: ATR columns not found, setting atr_ratio to NA\n")
}

# RSI difference: Momentum divergence
if ("rsi_14" %in% names(dt_trades) && "rsi_28" %in% names(dt_trades)) {
dt_trades[, rsi_diff := rsi_28 - rsi_14]
} else {
dt_trades[, rsi_diff := NA_real_]
cat("  Warning: RSI columns not found, setting rsi_diff to NA\n")
}

# Hour of day
dt_trades[, hour := as.integer(format(datetime, "%H"))]

# Trade direction as numeric
dt_trades[, trade_direction := as.numeric(signal)]

# Winning probability (the probability that triggered the trade)
dt_trades[, trade_probability := fifelse(signal == 1, pred_prob_long, pred_prob_short)]

# Losing probability (the other model's probability)
dt_trades[, opposing_probability := fifelse(signal == 1, pred_prob_short, pred_prob_long)]

# Add year/month for walk-forward
dt_trades[, year := as.integer(format(datetime, "%Y"))]
dt_trades[, month := as.integer(format(datetime, "%m"))]
dt_trades[, year_month := year * 100 + month]

# --- 2.3: Define Meta-Features ---
cat("\n--- Selecting Meta-Features ---\n")

# Core meta-features
meta_feature_candidates <- c(
# Probability features
"clarity_gap",
"trade_probability",
"opposing_probability",
"pred_prob_long",
"pred_prob_short",

# Volatility features
"atr_ratio",
"atr_14",
"atr_28",

# Momentum features
"rsi_diff",
"rsi_14",
"rsi_28",

# Time features
"hour",

# Direction
"trade_direction",

# Additional indicators (if available)
"adx_14",
"bb_pct_b_20",
"macd_histogram",
"obv"
)

# Check which features are available
meta_features <- intersect(meta_feature_candidates, names(dt_trades))

# Remove any with all NA
for (feat in meta_features) {
if (all(is.na(dt_trades[[feat]]))) {
  meta_features <- setdiff(meta_features, feat)
  cat(sprintf("  Removed %s (all NA)\n", feat))
}
}

cat(sprintf("Meta-features selected: %d\n", length(meta_features)))
cat("Features:\n")
print(meta_features)

# Remove rows with NA in meta-features
dt_meta <- na.omit(dt_trades, cols = meta_features)
cat(sprintf("Meta dataset after NA removal: %s rows\n", format(nrow(dt_meta), big.mark = ",")))

# ===== STEP 3: WALK-FORWARD META-MODEL TRAINING ==============================

cat("\n=== STEP 3: WALK-FORWARD META-MODEL TRAINING ===\n")

# Get unique year-months
unique_months <- sort(unique(dt_meta$year_month))
cat(sprintf("Data spans %d months: %d to %d\n",
          length(unique_months), min(unique_months), max(unique_months)))

# Determine walk-forward splits
# Start training after INITIAL_TRAIN_MONTHS, validate on VALIDATION_MONTHS at a time
if (length(unique_months) <= INITIAL_TRAIN_MONTHS) {
stop("Not enough data for walk-forward validation. Need more than INITIAL_TRAIN_MONTHS months.")
}

# Create walk-forward folds
wf_folds <- list()
for (i in (INITIAL_TRAIN_MONTHS + 1):length(unique_months)) {
train_months <- unique_months[1:(i-1)]
val_month <- unique_months[i]

wf_folds[[length(wf_folds) + 1]] <- list(
  fold_id = length(wf_folds) + 1,
  train_months = train_months,
  val_month = val_month
)
}

cat(sprintf("Walk-forward folds: %d\n", length(wf_folds)))

# --- 3.1: Train Meta-Models with Walk-Forward ---
cat("\n--- Training Meta-Models ---\n")

# Store predictions for all validation sets
all_val_predictions <- data.table()

# XGBoost parameters for meta-model
xgb_params <- list(
objective = "binary:logistic",
eval_metric = "auc",
max_depth = 4,
eta = 0.1,
subsample = 0.8,
colsample_bytree = 0.8,
min_child_weight = 5
)

pb <- progress_bar$new(
format = "  Training [:bar] :percent | Fold :current/:total | ETA: :eta",
total = length(wf_folds),
clear = FALSE
)

for (fold in wf_folds) {
pb$tick()

# Split data
dt_train <- dt_meta[year_month %in% fold$train_months]
dt_val <- dt_meta[year_month == fold$val_month]

if (nrow(dt_train) < 100 || nrow(dt_val) < 10) {
  cat(sprintf("  Skipping fold %d: insufficient data (train=%d, val=%d)\n",
              fold$fold_id, nrow(dt_train), nrow(dt_val)))
  next
}

# Prepare matrices
X_train <- as.matrix(dt_train[, ..meta_features])
y_train <- dt_train$target

X_val <- as.matrix(dt_val[, ..meta_features])
y_val <- dt_val$target

# Create DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)

# Train model
model_meta <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 20,
  verbose = 0
)

# Predict on validation
val_preds <- predict(model_meta, dval)

# Store predictions
val_results <- data.table(
  datetime = dt_val$datetime,
  year_month = dt_val$year_month,
  fold_id = fold$fold_id,
  signal = dt_val$signal,
  pnl = dt_val$pnl,
  target = y_val,
  meta_prob = val_preds
)

all_val_predictions <- rbind(all_val_predictions, val_results)
}

cat(sprintf("\nWalk-forward predictions: %s rows\n", format(nrow(all_val_predictions), big.mark = ",")))

# --- 3.2: Train Final Meta-Model on All Data ---
cat("\n--- Training Final Meta-Model ---\n")

X_all <- as.matrix(dt_meta[, ..meta_features])
y_all <- dt_meta$target

dtrain_all <- xgb.DMatrix(data = X_all, label = y_all)

model_meta_final <- xgb.train(
params = xgb_params,
data = dtrain_all,
nrounds = 200,
verbose = 0
)

# Save final model
meta_model_file <- file.path(meta_output_path, paste0(EPIC, "_", INTERVAL, "_meta_model_", LABEL_VERSION, ".json"))
xgb.save(model_meta_final, meta_model_file)
cat(sprintf("Final meta-model saved: %s\n", meta_model_file))

# Feature importance
importance <- xgb.importance(feature_names = meta_features, model = model_meta_final)
cat("\nMeta-Model Feature Importance:\n")
print(importance)

# ===== STEP 4: EVALUATE META-MODEL ===========================================

cat("\n=== STEP 4: EVALUATE META-MODEL ===\n")

# --- 4.1: Calculate AUC ---
cat("\n--- ROC Analysis ---\n")

roc_obj <- roc(all_val_predictions$target, all_val_predictions$meta_prob, quiet = TRUE)
auc_value <- auc(roc_obj)

cat(sprintf("Meta-Model AUC: %.4f\n", auc_value))

# --- 4.2: Threshold Analysis ---
cat("\n--- Threshold Analysis ---\n")

threshold_results <- data.table()

for (thresh in THRESHOLD_CANDIDATES) {
# Apply threshold
preds_filtered <- all_val_predictions[meta_prob >= thresh]
preds_rejected <- all_val_predictions[meta_prob < thresh]

n_filtered <- nrow(preds_filtered)
n_rejected <- nrow(preds_rejected)
n_total <- nrow(all_val_predictions)

if (n_filtered > 0) {
  # Metrics for filtered trades
  precision <- mean(preds_filtered$target == 1)
  recall <- sum(preds_filtered$target == 1) / sum(all_val_predictions$target == 1)
  f1 <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0

  pnl_filtered <- sum(preds_filtered$pnl)
  pnl_rejected <- sum(preds_rejected$pnl)
  pnl_total <- sum(all_val_predictions$pnl)

  winrate_filtered <- mean(preds_filtered$pnl > 0)
  winrate_total <- mean(all_val_predictions$pnl > 0)

  threshold_results <- rbind(threshold_results, data.table(
    threshold = thresh,
    n_trades = n_filtered,
    pct_trades = 100 * n_filtered / n_total,
    precision = precision,
    recall = recall,
    f1_score = f1,
    winrate = winrate_filtered,
    winrate_lift = winrate_filtered - winrate_total,
    pnl = pnl_filtered,
    pnl_rejected = pnl_rejected,
    pnl_total = pnl_total,
    pnl_pct_captured = 100 * pnl_filtered / pnl_total
  ))
}
}

cat("\nThreshold Analysis Results:\n")
print(threshold_results)

# --- 4.3: Find Optimal Threshold ---
cat("\n--- Optimal Threshold Selection ---\n")

# Optimal by F1 score
optimal_f1 <- threshold_results[which.max(f1_score)]
cat(sprintf("Optimal threshold (by F1): %.2f\n", optimal_f1$threshold))
cat(sprintf("  F1 Score: %.4f\n", optimal_f1$f1_score))
cat(sprintf("  Precision: %.4f\n", optimal_f1$precision))
cat(sprintf("  Recall: %.4f\n", optimal_f1$recall))

# Optimal by PnL
optimal_pnl <- threshold_results[which.max(pnl)]
cat(sprintf("\nOptimal threshold (by PnL): %.2f\n", optimal_pnl$threshold))
cat(sprintf("  PnL: %.6f\n", optimal_pnl$pnl))
cat(sprintf("  Win Rate: %.2f%%\n", optimal_pnl$winrate * 100))

# Use F1-optimal as default
OPTIMAL_THRESHOLD <- optimal_f1$threshold

# ===== STEP 5: SIGNAL FILTERING ==============================================

cat("\n=== STEP 5: SIGNAL FILTERING ===\n")

# Apply meta-model to all trades
dt_meta[, meta_prob := predict(model_meta_final, xgb.DMatrix(as.matrix(dt_meta[, ..meta_features])))]

# Create filtered signal
dt_meta[, filtered_signal := fifelse(meta_prob >= OPTIMAL_THRESHOLD, signal, 0L)]

# --- 5.1: Compare Before vs After ---
cat("\n--- Performance Comparison: Before vs After Meta-Filtering ---\n")

# Before filtering
n_trades_before <- sum(dt_meta$signal != 0)
pnl_before <- sum(dt_meta$pnl)
winrate_before <- mean(dt_meta$pnl > 0)
mean_pnl_before <- mean(dt_meta$pnl)

# After filtering
dt_filtered <- dt_meta[filtered_signal != 0]
n_trades_after <- nrow(dt_filtered)
pnl_after <- sum(dt_filtered$pnl)
winrate_after <- if (n_trades_after > 0) mean(dt_filtered$pnl > 0) else NA
mean_pnl_after <- if (n_trades_after > 0) mean(dt_filtered$pnl) else NA

# Rejected trades
dt_rejected <- dt_meta[signal != 0 & filtered_signal == 0]
n_rejected <- nrow(dt_rejected)
pnl_rejected <- sum(dt_rejected$pnl)
winrate_rejected <- if (n_rejected > 0) mean(dt_rejected$pnl > 0) else NA

comparison_table <- data.table(
Metric = c("Number of Trades", "Total PnL", "Mean PnL per Trade", "Win Rate", "Sharpe Ratio"),
Before_Filter = c(
  n_trades_before,
  round(pnl_before, 6),
  round(mean_pnl_before, 6),
  round(winrate_before * 100, 2),
  round(mean_pnl_before / sd(dt_meta$pnl), 4)
),
After_Filter = c(
  n_trades_after,
  round(pnl_after, 6),
  round(mean_pnl_after, 6),
  round(winrate_after * 100, 2),
  if (n_trades_after > 1) round(mean_pnl_after / sd(dt_filtered$pnl), 4) else NA
),
Rejected = c(
  n_rejected,
  round(pnl_rejected, 6),
  if (n_rejected > 0) round(mean(dt_rejected$pnl), 6) else NA,
  round(winrate_rejected * 100, 2),
  if (n_rejected > 1) round(mean(dt_rejected$pnl) / sd(dt_rejected$pnl), 4) else NA
)
)

cat("\nPerformance Comparison:\n")
print(comparison_table)

cat(sprintf("\nTrades retained: %d / %d (%.1f%%)\n",
          n_trades_after, n_trades_before, 100 * n_trades_after / n_trades_before))
cat(sprintf("PnL improvement: %.6f -> %.6f (%.2f%%)\n",
          pnl_before, pnl_after,
          if (pnl_before != 0) 100 * (pnl_after - pnl_before) / abs(pnl_before) else NA))
cat(sprintf("Win rate improvement: %.2f%% -> %.2f%% (+%.2f pp)\n",
          winrate_before * 100, winrate_after * 100, (winrate_after - winrate_before) * 100))

# --- 5.2: Analysis by Trade Direction ---
cat("\n--- Performance by Trade Direction ---\n")

direction_comparison <- dt_meta[signal != 0, .(
n_before = .N,
pnl_before = sum(pnl),
winrate_before = mean(pnl > 0)
), by = signal]

direction_filtered <- dt_meta[filtered_signal != 0, .(
n_after = .N,
pnl_after = sum(pnl),
winrate_after = mean(pnl > 0)
), by = .(signal = filtered_signal)]

direction_comparison <- merge(direction_comparison, direction_filtered, by = "signal", all = TRUE)
direction_comparison[, direction := fifelse(signal == 1, "Long", "Short")]

print(direction_comparison[, .(direction, n_before, n_after, pnl_before, pnl_after, winrate_before, winrate_after)])

# ===== STEP 6: SAVE OUTPUT ===================================================

cat("\n=== STEP 6: SAVE OUTPUT ===\n")

# --- 6.1: Save Filtered Dataset ---
output_cols <- c(
"datetime", "close", "signal", "filtered_signal",
"pred_prob_long", "pred_prob_short", "meta_prob",
"pnl", "target", "clarity_gap"
)

available_output_cols <- intersect(output_cols, names(dt_meta))

filtered_output_file <- file.path(
meta_output_path,
paste0(EPIC, "_", INTERVAL, "_filtered_signals_", LABEL_VERSION, ".csv")
)
fwrite(dt_meta[, ..available_output_cols], filtered_output_file)
cat(sprintf("Filtered signals saved: %s\n", filtered_output_file))

# --- 6.2: Save Threshold Analysis ---
threshold_output_file <- file.path(
meta_output_path,
paste0(EPIC, "_", INTERVAL, "_threshold_analysis_", LABEL_VERSION, ".csv")
)
fwrite(threshold_results, threshold_output_file)
cat(sprintf("Threshold analysis saved: %s\n", threshold_output_file))

# --- 6.3: Save Summary ---
summary_file <- file.path(
meta_output_path,
paste0(EPIC, "_", INTERVAL, "_meta_summary_", LABEL_VERSION, ".txt")
)

sink(summary_file)
cat("=== META-LABELLING SUMMARY ===\n\n")
cat(sprintf("Epic: %s\n", EPIC))
cat(sprintf("Interval: %s\n", INTERVAL))
cat(sprintf("Label Version: %s\n\n", LABEL_VERSION))

cat("=== META-MODEL PERFORMANCE ===\n")
cat(sprintf("AUC: %.4f\n", auc_value))
cat(sprintf("Optimal Threshold (F1): %.2f\n\n", OPTIMAL_THRESHOLD))

cat("=== PERFORMANCE COMPARISON ===\n")
print(comparison_table)

cat("\n=== FEATURE IMPORTANCE ===\n")
print(importance)
sink()

cat(sprintf("Summary saved: %s\n", summary_file))

# ===== STEP 7: VISUALIZATIONS ================================================

cat("\n=== STEP 7: VISUALIZATIONS ===\n")

# --- 7.1: ROC Curve ---
cat("Creating ROC curve...\n")

roc_data <- data.table(
fpr = 1 - roc_obj$specificities,
tpr = roc_obj$sensitivities
)

p_roc <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
geom_line(color = "steelblue", size = 1.2) +
geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
annotate("text", x = 0.7, y = 0.3, label = sprintf("AUC = %.4f", auc_value),
         size = 5, fontface = "bold") +
labs(
  title = sprintf("Meta-Model ROC Curve - %s %s", EPIC, INTERVAL),
  x = "False Positive Rate (1 - Specificity)",
  y = "True Positive Rate (Sensitivity)"
) +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5, face = "bold"))

roc_file <- file.path(meta_output_path, paste0(EPIC, "_", INTERVAL, "_roc_curve_", LABEL_VERSION, ".png"))
ggsave(roc_file, p_roc, width = 8, height = 6, dpi = 300)
cat(sprintf("ROC curve saved: %s\n", roc_file))

# --- 7.2: Threshold vs Metrics ---
cat("Creating threshold analysis plot...\n")

p_threshold <- ggplot(threshold_results, aes(x = threshold)) +
geom_line(aes(y = precision, color = "Precision"), size = 1) +
geom_line(aes(y = recall, color = "Recall"), size = 1) +
geom_line(aes(y = f1_score, color = "F1 Score"), size = 1) +
geom_line(aes(y = winrate, color = "Win Rate"), size = 1, linetype = "dashed") +
geom_vline(xintercept = OPTIMAL_THRESHOLD, linetype = "dotted", color = "red", size = 1) +
annotate("text", x = OPTIMAL_THRESHOLD + 0.02, y = 0.9,
         label = sprintf("Optimal: %.2f", OPTIMAL_THRESHOLD),
         hjust = 0, color = "red") +
scale_color_manual(values = c(
  "Precision" = "steelblue",
  "Recall" = "coral",
  "F1 Score" = "darkgreen",
  "Win Rate" = "purple"
)) +
scale_y_continuous(labels = percent_format()) +
labs(
  title = sprintf("Meta-Model Threshold Analysis - %s %s", EPIC, INTERVAL),
  x = "Meta-Model Threshold",
  y = "Metric Value",
  color = "Metric"
) +
theme_minimal() +
theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  legend.position = "bottom"
)

threshold_plot_file <- file.path(meta_output_path, paste0(EPIC, "_", INTERVAL, "_threshold_plot_", LABEL_VERSION, ".png"))
ggsave(threshold_plot_file, p_threshold, width = 10, height = 6, dpi = 300)
cat(sprintf("Threshold plot saved: %s\n", threshold_plot_file))

# --- 7.3: Feature Importance Plot ---
cat("Creating feature importance plot...\n")

p_importance <- ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
coord_flip() +
labs(
  title = sprintf("Meta-Model Feature Importance - %s %s", EPIC, INTERVAL),
  x = "Feature",
  y = "Gain"
) +
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5, face = "bold"))

importance_file <- file.path(meta_output_path, paste0(EPIC, "_", INTERVAL, "_feature_importance_", LABEL_VERSION, ".png"))
ggsave(importance_file, p_importance, width = 10, height = 8, dpi = 300)
cat(sprintf("Feature importance saved: %s\n", importance_file))

# --- 7.4: PnL Comparison Bar Chart ---
cat("Creating PnL comparison chart...\n")

pnl_comparison_data <- data.table(
Category = c("Before Filter", "After Filter", "Rejected"),
PnL = c(pnl_before, pnl_after, pnl_rejected),
Trades = c(n_trades_before, n_trades_after, n_rejected)
)

p_pnl_comparison <- ggplot(pnl_comparison_data, aes(x = Category, y = PnL, fill = Category)) +
geom_bar(stat = "identity", alpha = 0.8) +
geom_text(aes(label = sprintf("%.4f\n(%d trades)", PnL, Trades)),
          vjust = ifelse(pnl_comparison_data$PnL >= 0, -0.3, 1.3)) +
scale_fill_manual(values = c(
  "Before Filter" = "gray50",
  "After Filter" = "seagreen",
  "Rejected" = "coral"
)) +
labs(
  title = sprintf("PnL Comparison: With vs Without Meta-Filter - %s %s", EPIC, INTERVAL),
  x = "",
  y = "Total PnL (Log Returns)",
  subtitle = sprintf("Optimal Threshold: %.2f | Win Rate: %.1f%% -> %.1f%%",
                     OPTIMAL_THRESHOLD, winrate_before * 100, winrate_after * 100)
) +
theme_minimal() +
theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5),
  legend.position = "none"
)

pnl_comparison_file <- file.path(meta_output_path, paste0(EPIC, "_", INTERVAL, "_pnl_comparison_", LABEL_VERSION, ".png"))
ggsave(pnl_comparison_file, p_pnl_comparison, width = 10, height = 6, dpi = 300)
cat(sprintf("PnL comparison saved: %s\n", pnl_comparison_file))

# ===== COMPLETE ==============================================================

cat("\n=== META-LABELLING COMPLETE ===\n")
cat(sprintf("\nOutput files saved to: %s\n", meta_output_path))
cat("  - Filtered signals CSV\n")
cat("  - Threshold analysis CSV\n")
cat("  - Meta-model (JSON)\n")
cat("  - Summary text file\n")
cat("  - ROC curve plot\n")
cat("  - Threshold analysis plot\n")
cat("  - Feature importance plot\n")
cat("  - PnL comparison plot\n")

cat(sprintf("\n=== KEY RESULTS ===\n"))
cat(sprintf("Meta-Model AUC: %.4f\n", auc_value))
cat(sprintf("Optimal Threshold: %.2f\n", OPTIMAL_THRESHOLD))
cat(sprintf("Trades: %d -> %d (%.1f%% retained)\n",
          n_trades_before, n_trades_after, 100 * n_trades_after / n_trades_before))
cat(sprintf("Win Rate: %.1f%% -> %.1f%% (+%.1f pp)\n",
          winrate_before * 100, winrate_after * 100, (winrate_after - winrate_before) * 100))
cat(sprintf("Total PnL: %.6f -> %.6f\n", pnl_before, pnl_after))
