# ============================================================================
# PnL-AWARE FEATURE SELECTION PIPELINE
# ============================================================================
#
# Evaluiert Features basierend auf tatsaechlicher Trading-Profitabilitaet
# statt Label-Prediction Accuracy.
#
# KEY INNOVATION:
# - Features werden nicht nach "predicting label 1 vs 0" bewertet
# - Sondern nach "wie profitabel sind die generierten Trades?"
#
# PIPELINE:
# 1. Semantische Feature-Gruppierung (50 Gruppen a 10 Features)
# 2. Trade Simulation mit Triple Barrier Exit
# 3. Walk-Forward Evaluation pro Feature-Gruppe
# 4. Ranking nach PnL-Metriken (Sharpe, Profit Factor, Consistency)
# 5. Correlation Cleaning der Top Features
#
# ============================================================================

cat("\n=== PnL-AWARE FEATURE SELECTION PIPELINE ===\n")
cat(sprintf("Started: %s\n", Sys.time()))

# ===== SETUP =================================================================

rm(list = ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,      # Fast data manipulation
  xgboost,         # Gradient boosting
  parallel,        # Parallelization
  doParallel,      # Parallel backend
  foreach,         # Parallel loops
  progress,        # Progress bars
  tictoc,          # Timing
  ggplot2,         # Visualization
  yaml,            # Config file
  jsonlite,        # JSON export
  lubridate,       # Date-time handling
  PerformanceAnalytics,  # Sharpe, Drawdown calculations
  tictoc
)

# ===== CONFIGURATION =========================================================
tic()
CONFIG <- list(
  # Data Settings
  epic = "GOLD",
  interval = "MINUTE_15",

  # Label Selection
  # Available options:
  #   "standard"         - _labeled.csv (filtered, recommended for backtesting)
  #   "unfiltered"       - _labeled_unfiltered.csv (no filtering)
  #   "raw"              - _labeled_raw.csv (raw labels, no processing)
  #   "enhanced_neutral" - _labeled_enhanced_neutral.csv (aggressive neutral relabeling)
  #   "meta"             - _meta_labeled.csv (meta labels from extrema signals)
  label_version = "enhanced_neutral",

  # Time Periods
  train_start = "2019-01-01",
  train_end = "2023-12-31",
  validation_start = "2024-01-01",
  validation_end = "2024-12-31",
  test_year = 2025,

  # Walk-Forward Settings
  wf_train_months = 18,      # Training window size
  wf_val_months = 6,        # Validation window size
  wf_step_months = 1,       # Step size between windows

  # Trade Simulation Settings
  entry_threshold = 0.55,     # Prediction threshold for entry
  atr_multiplier_tp = 2.5,   # ATR multiplier for Take Profit
  atr_multiplier_sl = 2.0,   # ATR multiplier for Stop Loss
  max_bars_held = 16,        # Maximum bars before time stop
  slippage_pct = 0.0002,     # 0.02% slippage per trade
  commission_pct = 0.0002,   # 0.02% commission per round-trip

  # Model Settings (same as 02_backtest for comparable results)
  xgb_max_depth = 4,
  xgb_n_estimators = 1000,
  xgb_learning_rate = 0.1,
  xgb_early_stopping = 50,

  # Pre-Filtering Settings (XGBoost-based feature importance)
  prefilter_n_features = 100,           # Keep top N features after XGBoost pre-filter
  prefilter_xgb_rounds = 100,           # XGBoost rounds for pre-filter model
  prefilter_importance_metric = "gain", # gain, cover, or frequency

  # Feature Selection Settings
  n_feature_groups = 10,   # 3 top-tier + 7 random groups per direction
  features_per_group = 10,
  min_trades_significance = 30,
  correlation_threshold = 0.7,
  top_groups_to_select = 5,

  # Sharpe Annualization (15min bars)
  # 26 bars per day * 252 trading days = 6552 bars/year
  annualization_factor = sqrt(26 * 252),

  # Parallel Processing
  n_cores = max(1, parallel::detectCores() - 2),

  # Session Filter
  use_session_filter = TRUE,

  # Output Paths
  output_path = "backtest_results/pnl_feature_selection",
  cache_path = "backtest_results/pnl_feature_selection/cache",
  log_path = "backtest_results/pnl_feature_selection/logs"
)

# Helper function to get label filename based on version
get_label_filename <- function(config) {
  suffix <- switch(config$label_version,
    "standard"         = "_labeled.csv",
    "unfiltered"       = "_labeled_unfiltered.csv",
    "raw"              = "_labeled_raw.csv",
    "enhanced_neutral" = "_labeled_enhanced_neutral.csv",
    "meta"             = "_meta_labeled.csv",
    stop(sprintf("Unknown label_version: '%s'. Use: standard, unfiltered, raw, enhanced_neutral, or meta",
                 config$label_version))
  )
  file.path("labelled_data", paste0(config$epic, "_", config$interval, suffix))
}

# Helper function to standardize label columns (handles different formats)
standardize_labels <- function(dt_labels, label_version) {
  dt <- copy(dt_labels)

  if (label_version == "meta") {
    # Meta labels have different structure:
    # - meta_label (0/1) instead of label (-1/0/1)
    # - primary_signal indicates direction (-1 = short, 1 = long)
    # - No in_session column

    cat("Converting meta labels to standard format...\n")

    # Create standard label based on primary_signal and meta_label
    # meta_label = 1 means the primary signal was correct
    # meta_label = 0 means the primary signal was wrong
    if ("meta_label" %in% names(dt) && "primary_signal" %in% names(dt)) {
      # If meta_label is 1 (correct), use primary_signal as label
      # If meta_label is 0 (wrong), use opposite of primary_signal or 0
      dt[, label := fifelse(meta_label == 1, primary_signal,
                           fifelse(meta_label == 0, 0L, NA_integer_))]

      cat(sprintf("  Meta labels converted: %d correct signals, %d wrong signals\n",
                  sum(dt$meta_label == 1, na.rm = TRUE),
                  sum(dt$meta_label == 0, na.rm = TRUE)))
    } else {
      stop("Meta label file missing required columns: meta_label, primary_signal")
    }

    # Add in_session if missing (assume all bars are valid for meta labels)
    if (!"in_session" %in% names(dt)) {
      dt[, in_session := TRUE]
      cat("  Added in_session = TRUE (not available in meta labels)\n")
    }
  }

  # Ensure label column exists
  if (!"label" %in% names(dt)) {
    stop("Label column not found after standardization")
  }

  return(dt)
}

# Print selected label version
cat(sprintf("\n=== LABEL VERSION: %s ===\n", toupper(CONFIG$label_version)))
cat(sprintf("Using: %s\n", get_label_filename(CONFIG)))
if (CONFIG$label_version == "meta") {
  cat("NOTE: Meta labels will be converted to standard format\n")
  cat("  - meta_label=1 with primary_signal → label = primary_signal\n")
  cat("  - meta_label=0 → label = 0 (neutral)\n")
}

# Create output directories
for (path in c(CONFIG$output_path, CONFIG$cache_path, CONFIG$log_path)) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# Save config for reproducibility
config_file <- file.path(CONFIG$output_path, "config_used.yaml")
write_yaml(CONFIG, config_file)
cat(sprintf("Configuration saved: %s\n", config_file))

# ===== LOGGING SETUP =========================================================

log_file <- file.path(
  CONFIG$log_path,
  paste0("pnl_feature_selection_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  cat(log_line, "\n")
  cat(log_line, "\n", file = log_file, append = TRUE)
}

log_message("Pipeline started")

# ============================================================================
# HELPER FUNCTIONS: QUALITY CHECKS & DYNAMIC THRESHOLD
# ============================================================================

#' Check prediction quality - detect if model is predicting usefully
#'
#' @param predictions Numeric vector of predictions (0-1)
#' @param labels Binary labels (0/1)
#' @param verbose Print detailed diagnostics
#' @return List with quality metrics and pass/fail status
check_prediction_quality <- function(predictions, labels, verbose = TRUE) {
  # Basic stats
  n <- length(predictions)
  mean_pred <- mean(predictions, na.rm = TRUE)
  sd_pred <- sd(predictions, na.rm = TRUE)
  min_pred <- min(predictions, na.rm = TRUE)
  max_pred <- max(predictions, na.rm = TRUE)
  range_pred <- max_pred - min_pred

  # Separation between classes
  pred_label_1 <- mean(predictions[labels == 1], na.rm = TRUE)
  pred_label_0 <- mean(predictions[labels == 0], na.rm = TRUE)
  separation <- pred_label_1 - pred_label_0

  # Quality checks
  issues <- character(0)

  # Check 1: Near-constant predictions
  if (sd_pred < 0.01) {
    issues <- c(issues, "CRITICAL: Near-constant predictions (sd < 0.01)")
  }


  # Check 2: Very narrow range
  if (range_pred < 0.1) {
    issues <- c(issues, "WARNING: Very narrow prediction range (< 0.1)")
  }

  # Check 3: No class separation
  if (is.na(separation) || separation < 0.02) {
    issues <- c(issues, "CRITICAL: No meaningful separation between classes")
  }

  # Check 4: Wrong direction (label 0 has higher predictions)
  if (!is.na(separation) && separation < 0) {
    issues <- c(issues, "CRITICAL: Predictions inverted - label 0 has higher mean")
  }

  # Check 5: Predictions too centered (all near 0.5)
  if (mean_pred > 0.45 && mean_pred < 0.55 && sd_pred < 0.05) {
    issues <- c(issues, "WARNING: Predictions clustered around 0.5")
  }

  passed <- length(issues) == 0

  if (verbose) {
    cat("\n=== PREDICTION QUALITY CHECK ===\n")
    cat(sprintf("N predictions: %d\n", n))
    cat(sprintf("Range: [%.4f, %.4f] (spread: %.4f)\n", min_pred, max_pred, range_pred))
    cat(sprintf("Mean: %.4f, SD: %.4f\n", mean_pred, sd_pred))
    cat(sprintf("Mean when Label=1: %.4f\n", pred_label_1))
    cat(sprintf("Mean when Label=0: %.4f\n", pred_label_0))
    cat(sprintf("Separation: %.4f\n", separation))

    if (passed) {
      cat("Status: PASSED\n")
    } else {
      cat("Status: FAILED\n")
      for (issue in issues) {
        cat(sprintf("  - %s\n", issue))
      }
    }
  }

  return(list(
    passed = passed,
    issues = issues,
    mean_pred = mean_pred,
    sd_pred = sd_pred,
    range = range_pred,
    separation = separation,
    pred_label_1 = pred_label_1,
    pred_label_0 = pred_label_0
  ))
}


#' Calculate dynamic threshold based on prediction distribution
#'
#' @param predictions Numeric vector of predictions
#' @param method Method for threshold selection: "percentile", "otsu", or "fixed"
#' @param percentile Percentile to use if method="percentile" (default 70)
#' @param min_threshold Minimum allowed threshold
#' @param max_threshold Maximum allowed threshold
#' @return Numeric threshold value
calculate_dynamic_threshold <- function(
    predictions,
    method = "percentile",
    percentile = 70,
    min_threshold = 0.3,
    max_threshold = 0.8
) {
  # Remove NA values
  preds <- predictions[!is.na(predictions)]

  if (length(preds) == 0) {
    return(0.5)  # Default fallback
  }

  threshold <- switch(method,
    "percentile" = {
      # Use specified percentile of predictions
      quantile(preds, probs = percentile / 100)
    },
    "otsu" = {
      # Otsu's method - find threshold that maximizes between-class variance
      # Simplified implementation for binary classification
      hist_data <- hist(preds, breaks = 50, plot = FALSE)
      breaks <- hist_data$breaks
      counts <- hist_data$counts

      best_thresh <- 0.5
      best_var <- 0

      for (i in 2:(length(breaks) - 1)) {
        t <- breaks[i]
        w0 <- sum(counts[1:(i-1)]) / sum(counts)
        w1 <- 1 - w0

        if (w0 > 0 && w1 > 0) {
          m0 <- mean(preds[preds < t])
          m1 <- mean(preds[preds >= t])
          var_between <- w0 * w1 * (m0 - m1)^2
          if (var_between > best_var) {
            best_var <- var_between
            best_thresh <- t
          }
        }
      }
      best_thresh
    },
    "fixed" = 0.5,
    0.5  # Default
  )

  # Clamp to valid range
  threshold <- max(min_threshold, min(max_threshold, threshold))

  return(as.numeric(threshold))
}


#' Check feature sanity before training
#'
#' @param dt_train Training data.table
#' @param feature_cols Vector of feature column names
#' @param verbose Print detailed diagnostics
#' @return List with valid features and removed features with reasons
check_feature_sanity <- function(dt_train, feature_cols, verbose = TRUE) {
  valid_features <- character(0)
  removed_features <- list()

  for (feat in feature_cols) {
    if (!feat %in% names(dt_train)) {
      removed_features[[feat]] <- "Column not found"
      next
    }

    values <- dt_train[[feat]]

    # Check 1: All NA
    if (all(is.na(values))) {
      removed_features[[feat]] <- "All NA values"
      next
    }

    # Check 2: All same value (zero variance)
    non_na_values <- values[!is.na(values)]
    if (length(unique(non_na_values)) == 1) {
      removed_features[[feat]] <- "Zero variance (constant)"
      next
    }

    # Check 3: Too many NA (>50%)
    na_pct <- sum(is.na(values)) / length(values)
    if (na_pct > 0.5) {
      removed_features[[feat]] <- sprintf("Too many NA (%.1f%%)", na_pct * 100)
      next
    }

    # Check 4: Infinite values
    if (any(is.infinite(non_na_values))) {
      removed_features[[feat]] <- "Contains Inf values"
      next
    }

    # Check 5: Extremely low variance (near-constant)
    feat_sd <- sd(non_na_values, na.rm = TRUE)
    feat_mean <- mean(abs(non_na_values), na.rm = TRUE)
    if (feat_mean > 0 && feat_sd / feat_mean < 0.001) {
      removed_features[[feat]] <- "Near-zero variance"
      next
    }

    valid_features <- c(valid_features, feat)
  }

  if (verbose) {
    cat(sprintf("\n=== FEATURE SANITY CHECK ===\n"))
    cat(sprintf("Input features: %d\n", length(feature_cols)))
    cat(sprintf("Valid features: %d\n", length(valid_features)))
    cat(sprintf("Removed features: %d\n", length(removed_features)))

    if (length(removed_features) > 0 && length(removed_features) <= 10) {
      cat("Removed:\n")
      for (feat in names(removed_features)) {
        cat(sprintf("  - %s: %s\n", feat, removed_features[[feat]]))
      }
    } else if (length(removed_features) > 10) {
      cat(sprintf("(Showing first 10 of %d removed features)\n", length(removed_features)))
      for (feat in names(removed_features)[1:10]) {
        cat(sprintf("  - %s: %s\n", feat, removed_features[[feat]]))
      }
    }
  }

  return(list(
    valid_features = valid_features,
    removed_features = removed_features,
    n_valid = length(valid_features),
    n_removed = length(removed_features)
  ))
}


#' Check label-trade consistency
#'
#' @param trades_df data.table of trades from simulate_trades()
#' @param dt_labeled data.table with labels
#' @param direction "long" or "short"
#' @param verbose Print detailed diagnostics
#' @return List with consistency metrics
check_label_trade_consistency <- function(trades_df, dt_labeled, direction, verbose = TRUE) {
  if (nrow(trades_df) == 0) {
    return(list(
      n_trades = 0,
      n_with_correct_label = NA,
      consistency_rate = NA,
      overlap_rate = NA
    ))
  }

  # Expected label value
  expected_label <- if (direction == "long") 1 else -1

  # Match trades to labels by signal_time (if available) or entry_time
  time_col <- if ("signal_time" %in% names(trades_df)) "signal_time" else "entry_time"

  trades_with_labels <- merge(
    trades_df,
    dt_labeled[, .(datetime, label)],
    by.x = time_col,
    by.y = "datetime",
    all.x = TRUE
  )

  # Calculate consistency
  n_total <- nrow(trades_with_labels)
  n_matched <- sum(!is.na(trades_with_labels$label))
  n_correct <- sum(trades_with_labels$label == expected_label, na.rm = TRUE)
  n_wrong <- n_matched - n_correct

  consistency_rate <- if (n_matched > 0) n_correct / n_matched else NA
  overlap_rate <- n_matched / n_total

  # Win rate when label matches vs doesn't match
  correct_label_trades <- trades_with_labels[label == expected_label]
  wrong_label_trades <- trades_with_labels[!is.na(label) & label != expected_label]

  win_rate_correct <- if (nrow(correct_label_trades) > 0) {
    mean(correct_label_trades$pnl_net > 0)
  } else NA

  win_rate_wrong <- if (nrow(wrong_label_trades) > 0) {
    mean(wrong_label_trades$pnl_net > 0)
  } else NA

  if (verbose) {
    cat("\n=== LABEL-TRADE CONSISTENCY CHECK ===\n")
    cat(sprintf("Direction: %s (expected label: %d)\n", toupper(direction), expected_label))
    cat(sprintf("Total trades: %d\n", n_total))
    cat(sprintf("Trades with labels: %d (%.1f%%)\n", n_matched, 100 * overlap_rate))
    cat(sprintf("Trades with correct label: %d (%.1f%%)\n", n_correct, 100 * consistency_rate))
    cat(sprintf("Win rate (correct label): %.1f%%\n", 100 * win_rate_correct))
    cat(sprintf("Win rate (wrong label): %.1f%%\n", 100 * win_rate_wrong))

    if (!is.na(win_rate_correct) && !is.na(win_rate_wrong)) {
      if (win_rate_correct <= win_rate_wrong) {
        cat("WARNING: Trades with correct labels don't perform better!\n")
      }
    }
  }

  return(list(
    n_trades = n_total,
    n_with_labels = n_matched,
    n_correct_label = n_correct,
    consistency_rate = consistency_rate,
    overlap_rate = overlap_rate,
    win_rate_correct_label = win_rate_correct,
    win_rate_wrong_label = win_rate_wrong
  ))
}

# ============================================================================
# STEP 1: FEATURE GROUPING (SEMANTISCH)
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 1: SEMANTIC FEATURE GROUPING\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

# Load cached features to get feature names
features_cache_file <- file.path(
  "feature_cache",
  paste0(CONFIG$epic, "_", CONFIG$interval, "_features_all.csv")
)

if (!file.exists(features_cache_file)) {
  stop("Feature cache not found. Please run 02_backtest_main_script_ls_v2.R first.")
}

log_message(sprintf("Loading features from: %s", features_cache_file))

# Read just the header to get column names (memory efficient)
dt_features_sample <- fread(features_cache_file, nrows = 100)
all_columns <- names(dt_features_sample)

# Define meta columns to exclude
meta_cols <- c(
  "datetime", "year", "label", "label_binary", "sample_weight",
  "barrier_touched", "bars_to_exit", "realized_return",
  "n_concurrent", "realized_return_adj", "log_return",
  "open", "high", "low", "close", "volume", "time"
)

# Define ATR columns (reserved for trade simulation, not as features)
atr_cols <- grep("^atr_|_atr_", all_columns, value = TRUE, ignore.case = TRUE)

# Define session/hour columns (reserved for meta-labeling)
session_cols <- c(
  "hour", "hour_sin", "hour_cos", "hour_open", "hour_high", "hour_low",
  "hour_close", "hour_volume", "hour_close_mean", "hour_close_sd",
  "session_london", "session_ny", "session_asia", "session_overlap",
  "session", "in_session", "bars_until_session_end"
)

# Get all feature columns
excluded_cols <- unique(c(meta_cols, atr_cols, session_cols))
feature_cols <- setdiff(all_columns, excluded_cols)

# Filter to numeric columns only
numeric_feature_cols <- feature_cols[sapply(dt_features_sample[, ..feature_cols], is.numeric)]

cat(sprintf("Total columns in dataset: %d\n", length(all_columns)))
cat(sprintf("Meta columns excluded: %d\n", length(intersect(meta_cols, all_columns))))
cat(sprintf("ATR columns excluded: %d\n", length(atr_cols)))
cat(sprintf("Session columns excluded: %d\n", length(intersect(session_cols, all_columns))))
cat(sprintf("Numeric feature columns: %d\n", length(numeric_feature_cols)))

# ============================================================================
# STEP 0.5: XGBOOST PRE-FILTERING
# ============================================================================
# Filter features using XGBoost importance BEFORE group creation.
# This removes features with zero/minimal predictive power.
# Run separately for LONG and SHORT directions.
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 0.5: XGBOOST PRE-FILTERING\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Starting XGBoost pre-filtering...")

# ===== LOAD FULL DATA FOR PRE-FILTERING =====================================

cat("Loading full dataset for pre-filtering...\n")

# Load features
dt_features_full <- fread(features_cache_file)
cat(sprintf("Features loaded: %d rows\n", nrow(dt_features_full)))

# Load labels
label_file <- get_label_filename(CONFIG)
if (!file.exists(label_file)) {
  stop(sprintf("Label file not found: %s", label_file))
}
dt_labels <- fread(label_file)
dt_labels <- standardize_labels(dt_labels, CONFIG$label_version)
cat(sprintf("Labels loaded: %d rows\n", nrow(dt_labels)))

# Merge features and labels
dt_features_full[, datetime := as.POSIXct(datetime)]
dt_labels[, datetime := as.POSIXct(datetime)]
dt_merged <- merge(dt_features_full, dt_labels[, .(datetime, label)], by = "datetime")
cat(sprintf("Merged dataset: %d rows\n", nrow(dt_merged)))

# Filter to training period only for pre-filtering
dt_train_prefilter <- dt_merged[
  datetime >= as.POSIXct(CONFIG$train_start) &
  datetime <= as.POSIXct(CONFIG$train_end)
]
cat(sprintf("Training period for pre-filter: %d rows (%s to %s)\n",
            nrow(dt_train_prefilter), CONFIG$train_start, CONFIG$train_end))

# ===== PRE-FILTER FUNCTION ==================================================

prefilter_features_xgb <- function(dt_train, feature_cols, direction, config) {
  #' Use XGBoost to rank features by importance for a specific direction

  #'
  #' @param dt_train Training data.table with features and label column
  #' @param feature_cols Character vector of feature column names
  #' @param direction "long" or "short"
  #' @param config Configuration list
  #' @return Character vector of top N feature names sorted by importance

  cat(sprintf("\n--- Pre-filtering for %s direction ---\n", toupper(direction)))


  # Create binary target based on direction
  if (direction == "long") {
    # Long: predict label == 1 vs rest
    dt_train[, target := fifelse(label == 1, 1L, 0L)]
  } else {
    # Short: predict label == -1 vs rest
    dt_train[, target := fifelse(label == -1, 1L, 0L)]
  }

  # Check class balance and calculate scale_pos_weight
  n_positive <- sum(dt_train$target == 1)
  n_negative <- sum(dt_train$target == 0)
  scale_pos_weight <- if (n_positive > 0) n_negative / n_positive else 1
  cat(sprintf("Class balance: %d positive (%.1f%%), %d negative (%.1f%%), scale_pos_weight=%.2f\n",
              n_positive, 100 * n_positive / nrow(dt_train),
              n_negative, 100 * n_negative / nrow(dt_train),
              scale_pos_weight))

  # Prepare feature matrix
  feature_matrix <- as.matrix(dt_train[, ..feature_cols])

  # Handle missing values
  feature_matrix[is.na(feature_matrix)] <- 0
  feature_matrix[is.infinite(feature_matrix)] <- 0

  # Create DMatrix
  dtrain <- xgb.DMatrix(
    data = feature_matrix,
    label = dt_train$target
  )

  # XGBoost parameters with scale_pos_weight for class imbalance
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 4,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 10,
    nthread = config$n_cores,
    scale_pos_weight = scale_pos_weight
  )

  # Train model
  cat("Training XGBoost for feature importance...\n")
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = config$prefilter_xgb_rounds,
    verbose = 0
  )

  # Extract feature importance
  importance <- xgb.importance(
    feature_names = feature_cols,
    model = model
  )

  cat(sprintf("Features with non-zero importance: %d / %d\n",
              nrow(importance), length(feature_cols)))

  # Sort by gain (or specified metric)
  metric_col <- switch(config$prefilter_importance_metric,
    "gain" = "Gain",
    "cover" = "Cover",
    "frequency" = "Frequency",
    "Gain"  # default
  )

  setorderv(importance, metric_col, order = -1)

  # Select top N features
  n_select <- min(config$prefilter_n_features, nrow(importance))
  top_features <- importance$Feature[1:n_select]

  cat(sprintf("Selected top %d features for %s\n", n_select, toupper(direction)))

  # Show top 10
  cat("\nTop 10 features:\n")
  for (i in 1:min(10, nrow(importance))) {
    cat(sprintf("  %2d. %s (gain=%.4f)\n",
                i, importance$Feature[i], importance$Gain[i]))
  }

  # Return importance table for saving
  attr(top_features, "importance") <- importance

  # Clean up
  dt_train[, target := NULL]

  return(top_features)
}

# ===== RUN PRE-FILTERING FOR BOTH DIRECTIONS ================================

# Pre-filter for LONG
top_features_long <- prefilter_features_xgb(
  dt_train = copy(dt_train_prefilter),
  feature_cols = numeric_feature_cols,
  direction = "long",
  config = CONFIG
)

# Pre-filter for SHORT
top_features_short <- prefilter_features_xgb(
  dt_train = copy(dt_train_prefilter),
  feature_cols = numeric_feature_cols,
  direction = "short",
  config = CONFIG
)

# ===== SAVE FEATURE IMPORTANCE RANKINGS =====================================

# Save LONG importance
importance_long <- attr(top_features_long, "importance")
importance_long_file <- file.path(
  CONFIG$cache_path,
  sprintf("%s_%s_long_feature_importance.csv", CONFIG$epic, CONFIG$interval)
)
fwrite(importance_long, importance_long_file)
cat(sprintf("\nLong feature importance saved: %s\n", importance_long_file))

# Save SHORT importance
importance_short <- attr(top_features_short, "importance")
importance_short_file <- file.path(
  CONFIG$cache_path,
  sprintf("%s_%s_short_feature_importance.csv", CONFIG$epic, CONFIG$interval)
)
fwrite(importance_short, importance_short_file)
cat(sprintf("Short feature importance saved: %s\n", importance_short_file))

# Summary
cat(sprintf("\n=== PRE-FILTERING SUMMARY ===\n"))
cat(sprintf("Original features: %d\n", length(numeric_feature_cols)))
cat(sprintf("Top features for LONG: %d\n", length(top_features_long)))
cat(sprintf("Top features for SHORT: %d\n", length(top_features_short)))

# Overlap analysis
overlap <- intersect(top_features_long, top_features_short)
cat(sprintf("Overlap between LONG and SHORT: %d features (%.1f%%)\n",
            length(overlap), 100 * length(overlap) / CONFIG$prefilter_n_features))

log_message(sprintf("Pre-filtering complete: %d long features, %d short features",
                    length(top_features_long), length(top_features_short)))

# Clean up large objects
rm(dt_features_full, dt_merged, dt_train_prefilter)
gc()

# ============================================================================
# STEP 1: RANDOM FEATURE GROUPING (based on pre-filtered features)
# ============================================================================
# Creates random groups from pre-filtered features.
# Special groups for Top 1-10, 11-20, 21-30 features.
# Remaining features (31-100) grouped randomly.
# Separate groups for LONG and SHORT directions.
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 1: RANDOM FEATURE GROUPING\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Creating random feature groups from pre-filtered features...")

# ===== RANDOM GROUP CREATION FUNCTION ========================================

create_random_groups <- function(feature_names_sorted, direction, config, seed = 42) {
  #' Create feature groups from importance-sorted features

  set.seed(seed)
  prefix <- toupper(direction)
  n_features <- length(feature_names_sorted)
  group_size <- config$features_per_group

  groups <- list()

 # 1. Top-tier groups (importance-based, not random)
  if (n_features >= 10) {
    groups[[paste0(prefix, "_TOP_01_10")]] <- feature_names_sorted[1:10]
  }
  if (n_features >= 20) {
    groups[[paste0(prefix, "_TOP_11_20")]] <- feature_names_sorted[11:20]
  }
  if (n_features >= 30) {
    groups[[paste0(prefix, "_TOP_21_30")]] <- feature_names_sorted[21:30]
  }

  # 2. Remaining features (31-N) in random groups
  if (n_features > 30) {
    remaining <- feature_names_sorted[31:n_features]
    remaining_shuffled <- sample(remaining)

    n_random_groups <- ceiling(length(remaining_shuffled) / group_size)
    for (i in seq_len(n_random_groups)) {
      start_idx <- (i - 1) * group_size + 1
      end_idx <- min(i * group_size, length(remaining_shuffled))
      groups[[paste0(prefix, "_RANDOM_", sprintf("%02d", i))]] <- remaining_shuffled[start_idx:end_idx]
    }
  }

  return(groups)
}

# ===== CREATE GROUPS FOR BOTH DIRECTIONS ====================================

# Create LONG groups
feature_groups_long <- create_random_groups(
  feature_names_sorted = top_features_long,
  direction = "long",
  config = CONFIG,
  seed = 42
)

# Create SHORT groups
feature_groups_short <- create_random_groups(
  feature_names_sorted = top_features_short,
  direction = "short",
  config = CONFIG,
  seed = 42
)

# ===== SAVE GROUP MAPPINGS ==================================================

# Save LONG group mapping
group_mapping_long <- data.frame(
  direction = "long",
  group_id = rep(seq_along(feature_groups_long), sapply(feature_groups_long, length)),
  group_name = rep(names(feature_groups_long), sapply(feature_groups_long, length)),
  feature_name = unlist(feature_groups_long, use.names = FALSE),
  stringsAsFactors = FALSE
)

# Save SHORT group mapping
group_mapping_short <- data.frame(
  direction = "short",
  group_id = rep(seq_along(feature_groups_short), sapply(feature_groups_short, length)),
  group_name = rep(names(feature_groups_short), sapply(feature_groups_short, length)),
  feature_name = unlist(feature_groups_short, use.names = FALSE),
  stringsAsFactors = FALSE
)

# Combine and save
group_mapping <- rbind(group_mapping_long, group_mapping_short)
group_mapping_file <- file.path(
  CONFIG$cache_path,
  sprintf("%s_%s_group_mapping.csv", CONFIG$epic, CONFIG$interval)
)
fwrite(group_mapping, group_mapping_file)

# Save as JSON for easy loading
group_json_file_long <- file.path(CONFIG$output_path, "feature_groups_long.json")
group_json_file_short <- file.path(CONFIG$output_path, "feature_groups_short.json")
write_json(feature_groups_long, group_json_file_long, pretty = TRUE)
write_json(feature_groups_short, group_json_file_short, pretty = TRUE)

# ===== SUMMARY STATISTICS ===================================================

cat(sprintf("\nGroups created: %d LONG, %d SHORT\n",
            length(feature_groups_long), length(feature_groups_short)))

log_message(sprintf("Feature grouping complete: %d LONG groups, %d SHORT groups",
                    length(feature_groups_long), length(feature_groups_short)))

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 1 COMPLETE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Save workspace for continuation
save.image(file.path(CONFIG$cache_path, "step1_complete.RData"))
cat(sprintf("\nWorkspace saved: %s\n", file.path(CONFIG$cache_path, "step1_complete.RData")))

# ============================================================================
# STEP 2: TRADE SIMULATION FUNCTION
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 2: TRADE SIMULATION FUNCTION\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Implementing trade simulation functions...")

#' Simulate trades based on model predictions with Triple Barrier exit
#'
#' This function is the CORE of PnL-based feature evaluation.
#' Instead of evaluating features by label prediction accuracy,
#' we evaluate by actual trading profitability.
#'
#' @param predictions Numeric vector of model predictions (probabilities 0-1)
#' @param dt_prices data.table with datetime, open, high, low, close, atr columns
#' @param direction "long" or "short"
#' @param entry_threshold Minimum prediction to enter trade (default 0.6)
#' @param atr_mult_tp ATR multiplier for take profit (default 2.5)
#' @param atr_mult_sl ATR multiplier for stop loss (default 2.5)
#' @param max_bars Maximum bars to hold before time exit (default 16)
#' @param slippage_pct Slippage per trade (default 0.0002 = 0.02%)
#' @param commission_pct Commission per round-trip (default 0.0002 = 0.02%)
#' @param session_filter Only trade during in_session=TRUE (default TRUE)
#'
#' @return List with trades_df and metrics
simulate_trades <- function(
    predictions,
    dt_prices,
    direction = "long",
    entry_threshold = 0.6,
    atr_mult_tp = 2.5,
    atr_mult_sl = 2.5,
    max_bars = 16,
    slippage_pct = 0.0002,
    commission_pct = 0.0002,
    session_filter = TRUE
) {

  # ===== INPUT VALIDATION =====
  stopifnot(
    "predictions must be numeric" = is.numeric(predictions),
    "predictions must be between 0 and 1" = all(predictions >= 0 & predictions <= 1, na.rm = TRUE),
    "dt_prices must have required columns" = all(c("datetime", "open", "high", "low", "close") %in% names(dt_prices)),
    "direction must be 'long' or 'short'" = direction %in% c("long", "short")
  )

  # Check for ATR column
  atr_col <- NULL
  if ("atr_14" %in% names(dt_prices)) {
    atr_col <- "atr_14"
  } else if ("atr" %in% names(dt_prices)) {
    atr_col <- "atr"
  } else {
    # Calculate ATR if not present
    warning("ATR not found in data, calculating ATR(14)...")
    dt_prices[, atr_calc := {
      tr <- pmax(high - low,
                 abs(high - shift(close, 1)),
                 abs(low - shift(close, 1)),
                 na.rm = TRUE)
      frollmean(tr, n = 14, align = "right")
    }]
    atr_col <- "atr_calc"
  }

  # Ensure predictions length matches data
  if (length(predictions) != nrow(dt_prices)) {
    stop(sprintf("predictions length (%d) != data rows (%d)",
                 length(predictions), nrow(dt_prices)))
  }

  # ===== IDENTIFY ENTRY SIGNALS =====
  dt_sim <- copy(dt_prices)
  dt_sim[, pred := predictions]
  dt_sim[, row_idx := .I]

  # Apply session filter if requested
  if (session_filter && "in_session" %in% names(dt_sim)) {
    dt_sim[, signal := pred >= entry_threshold & in_session == TRUE]
  } else {
    dt_sim[, signal := pred >= entry_threshold]
  }

  # Get signal indices

  signal_indices <- which(dt_sim$signal)

  if (length(signal_indices) == 0) {
    # No trades generated
    return(list(
      trades_df = data.table(
        entry_time = as.POSIXct(character()),
        exit_time = as.POSIXct(character()),
        entry_price = numeric(),
        exit_price = numeric(),
        direction = character(),
        pnl_gross = numeric(),
        pnl_net = numeric(),
        bars_held = integer(),
        exit_reason = character(),
        atr_at_entry = numeric(),
        signal_time = as.POSIXct(character())
      ),
      metrics = list(
        n_trades = 0,
        sharpe = NA_real_,
        sortino_ratio = NA_real_,
        calmar_ratio = NA_real_,
        profit_factor = NA_real_,
        win_rate = NA_real_,
        max_drawdown = NA_real_,
        total_return = NA_real_,
        avg_bars_held = NA_real_,
        avg_win = NA_real_,
        avg_loss = NA_real_,
        avg_win_loss_ratio = NA_real_,
        n_trading_days = 0
      )
    ))
  }

  # ===== SIMULATE EACH TRADE =====
  # FIX: Entry auf Open von Bar N+1 (nicht Close von Signal Bar N)
  # FIX: TP/SL Overlap konservativ behandeln (SL first wenn beide möglich)
  trades_list <- vector("list", length(signal_indices))
  current_position <- FALSE
  position_exit_bar <- 0

  for (i in seq_along(signal_indices)) {
    signal_idx <- signal_indices[i]

    # Skip if still in previous position (no overlapping trades)
    if (signal_idx <= position_exit_bar) {
      next
    }

    # FIX: Entry auf nächster Bar (N+1), nicht Signal Bar (N)
    entry_idx <- signal_idx + 1

    # Check if entry bar exists
    if (entry_idx > nrow(dt_sim)) {
      next  # Cannot enter - no next bar available
    }

    # Get signal bar data for ATR (measured at signal time)
    signal_row <- dt_sim[signal_idx]
    atr_at_entry <- signal_row[[atr_col]]

    # Skip if ATR is NA or invalid
    if (is.na(atr_at_entry) || atr_at_entry <= 0) {
      next
    }

    # Get entry bar data - entry at OPEN of bar N+1
    entry_row <- dt_sim[entry_idx]
    entry_time <- entry_row$datetime
    entry_price_raw <- entry_row$open  # FIX: Open statt Close

    # Apply slippage to entry (worse price)
    if (direction == "long") {
      entry_price <- entry_price_raw * (1 + slippage_pct)
      tp_price <- entry_price + (atr_mult_tp * atr_at_entry)
      sl_price <- entry_price - (atr_mult_sl * atr_at_entry)
    } else {
      entry_price <- entry_price_raw * (1 - slippage_pct)
      tp_price <- entry_price - (atr_mult_tp * atr_at_entry)
      sl_price <- entry_price + (atr_mult_sl * atr_at_entry)
    }

    # ===== FIND EXIT =====
    exit_idx <- NA
    exit_price <- NA
    exit_reason <- NA

    # Look at subsequent bars for exit (starting from entry bar itself for intrabar exit)
    for (j in 0:max_bars) {
      check_idx <- entry_idx + j

      # Check if we're past the data
      if (check_idx > nrow(dt_sim)) {
        # Exit at last available price
        exit_idx <- nrow(dt_sim)
        exit_price <- dt_sim[exit_idx]$close
        exit_reason <- "data_end"
        break
      }

      check_row <- dt_sim[check_idx]

      # FIX: Konservative TP/SL Overlap-Behandlung
      # Wenn beide in derselben Bar getriggert werden könnten, nimm SL (pessimistisch)
      if (direction == "long") {
        tp_hit <- check_row$high >= tp_price
        sl_hit <- check_row$low <= sl_price

        if (tp_hit && sl_hit) {
          # Both triggered in same bar - assume SL hit first (conservative)
          exit_idx <- check_idx
          exit_price <- sl_price
          exit_reason <- "stop_loss"
          break
        } else if (sl_hit) {
          exit_idx <- check_idx
          exit_price <- sl_price
          exit_reason <- "stop_loss"
          break
        } else if (tp_hit) {
          exit_idx <- check_idx
          exit_price <- tp_price
          exit_reason <- "take_profit"
          break
        }
      } else {
        # Short direction
        tp_hit <- check_row$low <= tp_price
        sl_hit <- check_row$high >= sl_price

        if (tp_hit && sl_hit) {
          # Both triggered in same bar - assume SL hit first (conservative)
          exit_idx <- check_idx
          exit_price <- sl_price
          exit_reason <- "stop_loss"
          break
        } else if (sl_hit) {
          exit_idx <- check_idx
          exit_price <- sl_price
          exit_reason <- "stop_loss"
          break
        } else if (tp_hit) {
          exit_idx <- check_idx
          exit_price <- tp_price
          exit_reason <- "take_profit"
          break
        }
      }

      # Time stop at max_bars
      if (j == max_bars) {
        exit_idx <- check_idx
        exit_price <- check_row$close
        exit_reason <- "time_stop"
        break
      }
    }

    # Skip if no valid exit found
    if (is.na(exit_idx)) {
      next
    }

    # Apply slippage to exit (worse price)
    if (direction == "long") {
      exit_price_final <- exit_price * (1 - slippage_pct)
    } else {
      exit_price_final <- exit_price * (1 + slippage_pct)
    }

    # Calculate PnL
    if (direction == "long") {
      pnl_gross <- (exit_price_final - entry_price) / entry_price
    } else {
      pnl_gross <- (entry_price - exit_price_final) / entry_price
    }

    # Apply commission
    pnl_net <- pnl_gross - commission_pct

    # Record trade
    trades_list[[i]] <- data.table(
      entry_time = entry_time,
      exit_time = dt_sim[exit_idx]$datetime,
      entry_price = entry_price,
      exit_price = exit_price_final,
      direction = direction,
      pnl_gross = pnl_gross,
      pnl_net = pnl_net,
      bars_held = exit_idx - entry_idx,
      exit_reason = exit_reason,
      atr_at_entry = atr_at_entry,
      signal_time = signal_row$datetime  # Track original signal time
    )

    # Update position exit bar to prevent overlapping trades
    position_exit_bar <- exit_idx
  }

  # Combine trades
  trades_df <- rbindlist(trades_list[!sapply(trades_list, is.null)])

  # ===== CALCULATE METRICS =====
  if (nrow(trades_df) == 0) {
    return(list(
      trades_df = trades_df,
      metrics = list(
        n_trades = 0,
        sharpe = NA_real_,
        sortino_ratio = NA_real_,
        calmar_ratio = NA_real_,
        profit_factor = NA_real_,
        win_rate = NA_real_,
        max_drawdown = NA_real_,
        total_return = NA_real_,
        avg_bars_held = NA_real_,
        avg_win = NA_real_,
        avg_loss = NA_real_,
        avg_win_loss_ratio = NA_real_,
        n_trading_days = 0
      )
    ))
  }

  # Basic metrics
  n_trades <- nrow(trades_df)
  wins <- sum(trades_df$pnl_net > 0)
  losses <- sum(trades_df$pnl_net <= 0)
  win_rate <- wins / n_trades
  avg_bars_held <- mean(trades_df$bars_held)

  # Profit Factor
  gross_profit <- sum(trades_df$pnl_net[trades_df$pnl_net > 0])
  gross_loss <- abs(sum(trades_df$pnl_net[trades_df$pnl_net <= 0]))
  profit_factor <- if (gross_loss > 0) gross_profit / gross_loss else Inf

  # Total Return (compounded)
  total_return <- prod(1 + trades_df$pnl_net) - 1

  # Win/Loss Stats
  avg_win <- if (wins > 0) mean(trades_df$pnl_net[trades_df$pnl_net > 0]) else 0
  avg_loss <- if (losses > 0) mean(trades_df$pnl_net[trades_df$pnl_net <= 0]) else 0
  avg_win_loss_ratio <- if (avg_loss != 0) abs(avg_win / avg_loss) else Inf

  # ===== DAILY SHARPE RATIO (FIX) =====
  # Aggregate trades to daily returns for proper Sharpe calculation
  trades_df[, trade_date := as.Date(entry_time)]
  daily_returns <- trades_df[, .(daily_pnl = sum(pnl_net)), by = trade_date]

  if (nrow(daily_returns) > 1) {
    mean_daily <- mean(daily_returns$daily_pnl)
    sd_daily <- sd(daily_returns$daily_pnl)
    if (!is.na(sd_daily) && sd_daily > 0) {
      sharpe <- (mean_daily / sd_daily) * sqrt(252)  # Annualized daily Sharpe
    } else {
      sharpe <- if (mean_daily > 0) Inf else if (mean_daily < 0) -Inf else 0
    }
  } else {
    sharpe <- NA_real_
  }

  # ===== PERCENTAGE-BASED DRAWDOWN (FIX) =====
  # Equity curve starting at 1 (100%)
  equity_curve <- cumprod(1 + trades_df$pnl_net)
  running_max <- cummax(equity_curve)
  drawdowns_pct <- (running_max - equity_curve) / running_max  # Percentage drawdown
  max_drawdown <- max(drawdowns_pct, na.rm = TRUE)

  # ===== ADDITIONAL METRICS =====
  # Sortino Ratio (only downside deviation)
  negative_returns <- daily_returns$daily_pnl[daily_returns$daily_pnl < 0]
  if (length(negative_returns) > 1) {
    downside_dev <- sd(negative_returns)
    sortino_ratio <- if (downside_dev > 0) (mean(daily_returns$daily_pnl) / downside_dev) * sqrt(252) else NA_real_
  } else {
    sortino_ratio <- NA_real_
  }

  # Calmar Ratio (return / max drawdown)
  calmar_ratio <- if (max_drawdown > 0) total_return / max_drawdown else NA_real_

  metrics <- list(
    n_trades = n_trades,
    wins = wins,
    losses = losses,
    sharpe = sharpe,
    sortino_ratio = sortino_ratio,
    calmar_ratio = calmar_ratio,
    profit_factor = profit_factor,
    win_rate = win_rate,
    max_drawdown = max_drawdown,
    total_return = total_return,
    avg_bars_held = avg_bars_held,
    gross_profit = gross_profit,
    gross_loss = gross_loss,
    mean_return_per_trade = mean(trades_df$pnl_net),
    avg_win = avg_win,
    avg_loss = avg_loss,
    avg_win_loss_ratio = avg_win_loss_ratio,
    n_trading_days = nrow(daily_returns)
  )

  return(list(
    trades_df = trades_df,
    metrics = metrics
  ))
}


#' Calculate comprehensive trading metrics from trades dataframe
#'
#' @param trades_df data.table of trades from simulate_trades()
#' @param annualization_factor Factor for annualizing Sharpe (default sqrt(252) for daily)
#' @return List of metrics
calculate_trading_metrics <- function(trades_df, annualization_factor = sqrt(252)) {

  if (nrow(trades_df) == 0) {
    return(list(
      n_trades = 0,
      sharpe = NA_real_,
      sortino_ratio = NA_real_,
      profit_factor = NA_real_,
      win_rate = NA_real_,
      max_drawdown_pct = NA_real_,
      total_return_pct = NA_real_,
      avg_bars_held = NA_real_,
      avg_win = NA_real_,
      avg_loss = NA_real_,
      expectancy = NA_real_
    ))
  }

  n_trades <- nrow(trades_df)
  returns <- trades_df$pnl_net

  # Win/Loss
  wins <- returns[returns > 0]
  losses <- returns[returns <= 0]
  win_rate <- length(wins) / n_trades

  # Averages
  avg_win <- if (length(wins) > 0) mean(wins) else 0
  avg_loss <- if (length(losses) > 0) mean(losses) else 0

  # Expectancy
  expectancy <- (win_rate * avg_win) + ((1 - win_rate) * avg_loss)

  # Profit Factor
  gross_profit <- sum(wins)
  gross_loss <- abs(sum(losses))
  profit_factor <- if (gross_loss > 0) gross_profit / gross_loss else Inf

  # Total Return (compounded)
  total_return <- prod(1 + returns) - 1

  # ===== DAILY SHARPE RATIO (FIX) =====
  avg_bars <- mean(trades_df$bars_held)
  if ("entry_time" %in% names(trades_df)) {
    trades_df[, trade_date := as.Date(entry_time)]
    daily_returns <- trades_df[, .(daily_pnl = sum(pnl_net)), by = trade_date]

    if (nrow(daily_returns) > 1) {
      mean_daily <- mean(daily_returns$daily_pnl)
      sd_daily <- sd(daily_returns$daily_pnl)
      if (!is.na(sd_daily) && sd_daily > 0) {
        sharpe <- (mean_daily / sd_daily) * sqrt(252)
      } else {
        sharpe <- NA_real_
      }

      # Sortino
      negative_rets <- daily_returns$daily_pnl[daily_returns$daily_pnl < 0]
      if (length(negative_rets) > 1) {
        downside_dev <- sd(negative_rets)
        sortino_ratio <- if (downside_dev > 0) (mean_daily / downside_dev) * sqrt(252) else NA_real_
      } else {
        sortino_ratio <- NA_real_
      }
    } else {
      sharpe <- NA_real_
      sortino_ratio <- NA_real_
    }
  } else {
    # Fallback if no entry_time
    mean_ret <- mean(returns)
    sd_ret <- sd(returns)
    if (!is.na(sd_ret) && sd_ret > 0) {
      sharpe <- (mean_ret / sd_ret) * annualization_factor
    } else {
      sharpe <- NA_real_
    }
    sortino_ratio <- NA_real_
  }

  # ===== PERCENTAGE-BASED DRAWDOWN (FIX) =====
  equity_curve <- cumprod(1 + returns)
  running_max <- cummax(equity_curve)
  drawdowns_pct <- (running_max - equity_curve) / running_max
  max_dd <- max(drawdowns_pct, na.rm = TRUE)

  list(
    n_trades = n_trades,
    sharpe = sharpe,
    sortino_ratio = sortino_ratio,
    profit_factor = profit_factor,
    win_rate = win_rate,
    max_drawdown_pct = max_dd * 100,
    total_return_pct = total_return * 100,
    avg_bars_held = avg_bars,
    avg_win = avg_win,
    avg_loss = avg_loss,
    expectancy = expectancy
  )
}


# ===== TEST TRADE SIMULATION ON SAMPLE DATA =================================

cat("Testing trade simulation on sample data (January 2024)...\n\n")

# Load full data for testing
dt_full <- fread(features_cache_file)
setDT(dt_full)

# Convert datetime
if (is.character(dt_full$datetime)) {
  dt_full[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

# Load labels for in_session column (uses selected label_version)
labels_file <- get_label_filename(CONFIG)
cat(sprintf("Loading labels from: %s\n", labels_file))
dt_labels <- fread(labels_file)
if (is.character(dt_labels$datetime)) {
  dt_labels[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

# Standardize labels (handles meta labels format)
dt_labels <- standardize_labels(dt_labels, CONFIG$label_version)

# Merge in_session if available
if ("in_session" %in% names(dt_labels)) {
  dt_full <- merge(dt_full, dt_labels[, .(datetime, in_session)],
                   by = "datetime", all.x = TRUE)
  dt_full[is.na(in_session), in_session := TRUE]
}

# Add ATR if not present
if (!"atr_14" %in% names(dt_full)) {
  dt_full[, atr_14 := {
    tr <- pmax(high - low,
               abs(high - shift(close, 1)),
               abs(low - shift(close, 1)),
               na.rm = TRUE)
    frollmean(tr, n = 14, align = "right")
  }]
}

# Filter to January 2024 for test
dt_test_sample <- dt_full[datetime >= "2024-01-01" & datetime < "2024-02-01"]
cat(sprintf("Test sample: %s rows (January 2024)\n", format(nrow(dt_test_sample), big.mark = ",")))

# Generate random predictions for testing
set.seed(123)
test_predictions <- runif(nrow(dt_test_sample), min = 0.3, max = 0.8)

# Add some "high conviction" signals
high_signal_idx <- sample(1:nrow(dt_test_sample), size = 50)
test_predictions[high_signal_idx] <- runif(50, min = 0.65, max = 0.85)

cat(sprintf("Signals above threshold (0.6): %d\n",
            sum(test_predictions >= CONFIG$entry_threshold)))

# Run simulation for LONG
cat("\n--- LONG TRADES ---\n")
result_long <- simulate_trades(
  predictions = test_predictions,
  dt_prices = dt_test_sample,
  direction = "long",
  entry_threshold = CONFIG$entry_threshold,
  atr_mult_tp = CONFIG$atr_multiplier_tp,
  atr_mult_sl = CONFIG$atr_multiplier_sl,
  max_bars = CONFIG$max_bars_held,
  slippage_pct = CONFIG$slippage_pct,
  commission_pct = CONFIG$commission_pct,
  session_filter = CONFIG$use_session_filter
)

cat(sprintf("Trades generated: %d\n", result_long$metrics$n_trades))
if (result_long$metrics$n_trades > 0) {
  cat(sprintf("Win Rate: %.1f%%\n", result_long$metrics$win_rate * 100))
  cat(sprintf("Profit Factor: %.2f\n", result_long$metrics$profit_factor))
  cat(sprintf("Sharpe Ratio: %.2f\n", result_long$metrics$sharpe))
  cat(sprintf("Total Return: %.2f%%\n", result_long$metrics$total_return * 100))
  cat(sprintf("Max Drawdown: %.2f%%\n", result_long$metrics$max_drawdown * 100))
  cat(sprintf("Avg Bars Held: %.1f\n", result_long$metrics$avg_bars_held))
}

# Run simulation for SHORT
cat("\n--- SHORT TRADES ---\n")
result_short <- simulate_trades(
  predictions = test_predictions,
  dt_prices = dt_test_sample,
  direction = "short",
  entry_threshold = CONFIG$entry_threshold,
  atr_mult_tp = CONFIG$atr_multiplier_tp,
  atr_mult_sl = CONFIG$atr_multiplier_sl,
  max_bars = CONFIG$max_bars_held,
  slippage_pct = CONFIG$slippage_pct,
  commission_pct = CONFIG$commission_pct,
  session_filter = CONFIG$use_session_filter
)

cat(sprintf("Trades generated: %d\n", result_short$metrics$n_trades))
if (result_short$metrics$n_trades > 0) {
  cat(sprintf("Win Rate: %.1f%%\n", result_short$metrics$win_rate * 100))
  cat(sprintf("Profit Factor: %.2f\n", result_short$metrics$profit_factor))
  cat(sprintf("Sharpe Ratio: %.2f\n", result_short$metrics$sharpe))
  cat(sprintf("Total Return: %.2f%%\n", result_short$metrics$total_return * 100))
  cat(sprintf("Max Drawdown: %.2f%%\n", result_short$metrics$max_drawdown * 100))
}

# Show sample trades
cat("\n=== SAMPLE TRADES (First 10 Long) ===\n")
if (nrow(result_long$trades_df) > 0) {
  sample_trades <- head(result_long$trades_df, 10)
  sample_trades[, `:=`(
    entry_time = format(entry_time, "%Y-%m-%d %H:%M"),
    exit_time = format(exit_time, "%Y-%m-%d %H:%M"),
    entry_price = round(entry_price, 2),
    exit_price = round(exit_price, 2),
    pnl_net = round(pnl_net * 100, 3)  # as percentage
  )]
  print(sample_trades[, .(entry_time, exit_time, entry_price, exit_price,
                           pnl_net, bars_held, exit_reason)])
} else {
  cat("No trades generated.\n")
}

log_message("Trade simulation function implemented and tested")

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 2 COMPLETE - TRADE SIMULATION TESTED\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Save workspace
save.image(file.path(CONFIG$cache_path, "step2_complete.RData"))
cat(sprintf("\nWorkspace saved: %s\n", file.path(CONFIG$cache_path, "step2_complete.RData")))

# ============================================================================
# STEP 3: SINGLE WINDOW BACKTEST (ONE GROUP, ONE WINDOW)
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 3: SINGLE WINDOW BACKTEST\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Testing single feature group on single window...")

#' Train XGBoost model and generate predictions for PnL evaluation
#'
#' @param dt_train Training data with features and labels
#' @param dt_val Validation data for predictions
#' @param feature_cols Vector of feature column names
#' @param target_col Name of target column (default "label_binary")
#' @param params XGBoost parameters list
#' @return List with model, predictions, and training info
train_xgb_for_pnl <- function(
    dt_train,
    dt_val,
    feature_cols,
    target_col = "label_binary",
    params = list(
      max_depth = 4,
      eta = 0.1,
      nrounds = 1000,
      early_stopping_rounds = 50,
      objective = "binary:logistic",
      eval_metric = "auc"
    )
) {

  # Validate inputs
  missing_train <- setdiff(feature_cols, names(dt_train))
  if (length(missing_train) > 0) {
    stop(sprintf("Missing features in training data: %s",
                 paste(head(missing_train, 5), collapse = ", ")))
  }

  missing_val <- setdiff(feature_cols, names(dt_val))
  if (length(missing_val) > 0) {
    stop(sprintf("Missing features in validation data: %s",
                 paste(head(missing_val, 5), collapse = ", ")))
  }

  # Prepare matrices
  X_train <- as.matrix(dt_train[, ..feature_cols])
  y_train <- dt_train[[target_col]]

  X_val <- as.matrix(dt_val[, ..feature_cols])
  y_val <- if (target_col %in% names(dt_val)) dt_val[[target_col]] else NULL

  # Handle NA values
  X_train[is.na(X_train)] <- 0
  X_val[is.na(X_val)] <- 0

  # ===== CLASS IMBALANCE HANDLING (FIX) =====
  n_positive <- sum(y_train == 1)
  n_negative <- sum(y_train == 0)
  scale_pos_weight <- if (n_positive > 0) n_negative / n_positive else 1

  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval <- xgb.DMatrix(data = X_val, label = y_val)

  # Set parameters with scale_pos_weight
  xgb_params <- list(
    objective = params$objective %||% "binary:logistic",
    eval_metric = params$eval_metric %||% "auc",
    max_depth = params$max_depth %||% 2,
    eta = params$eta %||% 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight
  )

  # Train with early stopping
  watchlist <- list(train = dtrain, val = dval)

  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = params$nrounds %||% 50,
    watchlist = watchlist,
    early_stopping_rounds = params$early_stopping_rounds %||% 10,
    verbose = 0
  )

  # Generate predictions
  pred_train <- predict(model, dtrain)
  pred_val <- predict(model, dval)

  # Validation: predictions must be between 0 and 1
  stopifnot(all(pred_val >= 0 & pred_val <= 1))

  return(list(
    model = model,
    predictions_train = pred_train,
    predictions_val = pred_val,
    best_iteration = model$best_iteration,
    feature_importance = xgb.importance(feature_names = feature_cols, model = model)
  ))
}


#' Evaluate a single feature group on a single time window
#'
#' KEY INSIGHT: Train on LABELED data, but predict/simulate on ALL price data
#' This allows the model to generate predictions for every bar, not just labeled ones
#'
#' @param dt_labeled Dataset with labels (for training)
#' @param dt_all_prices Full price dataset with features (for prediction/simulation)
#' @param feature_group Vector of feature names for this group
#' @param train_start Start date for training
#' @param train_end End date for training
#' @param val_start Start date for validation
#' @param val_end End date for validation
#' @param direction "long" or "short"
#' @param config Configuration list
#' @return List with metrics and trade details
evaluate_feature_group_single_window <- function(
    dt_labeled,
    dt_all_prices,
    feature_group,
    train_start,
    train_end,
    val_start,
    val_end,
    direction = "long",
    config = CONFIG
) {

  # Filter LABELED data for training (only rows with labels)
  dt_train <- dt_labeled[datetime >= train_start & datetime <= train_end]
  dt_train <- dt_train[!is.na(label)]

  # Filter ALL PRICES for validation (for prediction and simulation)
  dt_val_prices <- dt_all_prices[datetime >= val_start & datetime <= val_end]

  # Also get labeled data in validation period for model validation
  dt_val_labeled <- dt_labeled[datetime >= val_start & datetime <= val_end]
  dt_val_labeled <- dt_val_labeled[!is.na(label)]

  # Check minimum training data
  if (nrow(dt_train) < 50) {
    return(list(
      success = FALSE,
      error = sprintf("Insufficient training data: %d rows", nrow(dt_train)),
      metrics = NULL
    ))
  }

  # Create binary labels based on direction
  if (direction == "long") {
    dt_train[, label_binary := fifelse(label == 1, 1, 0)]
    if (nrow(dt_val_labeled) > 0) {
      dt_val_labeled[, label_binary := fifelse(label == 1, 1, 0)]
    }
  } else {
    dt_train[, label_binary := fifelse(label == -1, 1, 0)]
    if (nrow(dt_val_labeled) > 0) {
      dt_val_labeled[, label_binary := fifelse(label == -1, 1, 0)]
    }
  }

  # Filter features that exist in data
  available_features <- intersect(feature_group, names(dt_labeled))
  available_features <- intersect(available_features, names(dt_all_prices))

  if (length(available_features) < length(feature_group)) {
    # Silently handle missing features (don't spam log)
  }

  if (length(available_features) == 0) {
    return(list(
      success = FALSE,
      error = "No features available",
      metrics = NULL
    ))
  }

  # ===== FEATURE SANITY CHECK (FIX) =====
  # Remove features with zero variance, all NA, etc. before training
  sanity_result <- check_feature_sanity(dt_train, available_features, verbose = FALSE)
  available_features <- sanity_result$valid_features

  if (length(available_features) == 0) {
    return(list(
      success = FALSE,
      error = "All features failed sanity check",
      metrics = NULL
    ))
  }

  # Train model
  tryCatch({
    # Prepare matrices
    X_train <- as.matrix(dt_train[, ..available_features])
    y_train <- dt_train$label_binary

    # Handle NA values in features
    X_train[is.na(X_train)] <- 0

    # ===== CLASS IMBALANCE HANDLING (FIX) =====
    n_positive <- sum(y_train == 1)
    n_negative <- sum(y_train == 0)
    scale_pos_weight <- if (n_positive > 0) n_negative / n_positive else 1

    # Create DMatrix for training
    dtrain <- xgb.DMatrix(data = X_train, label = y_train)

    # If we have labeled validation data, use it for early stopping
    if (nrow(dt_val_labeled) > 0) {
      X_val_labeled <- as.matrix(dt_val_labeled[, ..available_features])
      X_val_labeled[is.na(X_val_labeled)] <- 0
      y_val_labeled <- dt_val_labeled$label_binary
      dval_labeled <- xgb.DMatrix(data = X_val_labeled, label = y_val_labeled)
      watchlist <- list(train = dtrain, val = dval_labeled)
    } else {
      watchlist <- list(train = dtrain)
    }

    # XGBoost parameters with scale_pos_weight for class imbalance
    xgb_params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = config$xgb_max_depth,
      eta = config$xgb_learning_rate,
      subsample = 0.8,
      colsample_bytree = 0.8,
      scale_pos_weight = scale_pos_weight
    )

    # Train model
    model <- xgb.train(
      params = xgb_params,
      data = dtrain,
      nrounds = config$xgb_n_estimators,
      evals = watchlist,
      early_stopping_rounds = config$xgb_early_stopping,
      verbose = 0
    )

    # Generate predictions on ALL PRICE DATA (not just labeled)
    X_val_all <- as.matrix(dt_val_prices[, ..available_features])
    X_val_all[is.na(X_val_all)] <- 0
    dval_all <- xgb.DMatrix(data = X_val_all)

    predictions_val <- predict(model, dval_all)


    # Run trade simulation on validation set (ALL prices)
    sim_result <- simulate_trades(
      predictions = predictions_val,
      dt_prices = dt_val_prices,
      direction = direction,
      entry_threshold = config$entry_threshold,
      atr_mult_tp = config$atr_multiplier_tp,
      atr_mult_sl = config$atr_multiplier_sl,
      max_bars = config$max_bars_held,
      slippage_pct = config$slippage_pct,
      commission_pct = config$commission_pct,
      session_filter = config$use_session_filter
    )

    # Feature importance
    feature_importance <- xgb.importance(feature_names = available_features, model = model)

    return(list(
      success = TRUE,
      metrics = sim_result$metrics,
      trades_df = sim_result$trades_df,
      predictions = predictions_val,
      feature_importance = feature_importance,
      model = model,
      n_train_rows = nrow(dt_train),
      n_val_rows = nrow(dt_val_prices)
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = as.character(e),
      metrics = NULL
    ))
  })
}


# ===== RUN SINGLE WINDOW TEST ================================================

cat("Setting up single window test...\n\n")

# Load labels and merge with features (uses selected label_version)
labels_file <- get_label_filename(CONFIG)
cat(sprintf("Loading labels from: %s\n", labels_file))

dt_labels_full <- fread(labels_file)
if (is.character(dt_labels_full$datetime)) {
  dt_labels_full[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

# Standardize labels (handles meta labels format)
dt_labels_full <- standardize_labels(dt_labels_full, CONFIG$label_version)

# Merge labels with features - INNER JOIN to keep only labeled rows
label_cols <- c("datetime", "label", "in_session", "atr")
available_label_cols <- intersect(label_cols, names(dt_labels_full))

dt_merged <- merge(
  dt_full,
  dt_labels_full[, ..available_label_cols],
  by = "datetime",
  all = FALSE  # INNER JOIN - only keep rows with labels
)

# Use atr from labels if available
if ("atr" %in% names(dt_merged) && !"atr_14" %in% names(dt_merged)) {
  setnames(dt_merged, "atr", "atr_14")
}

# Remove rows with NA labels
dt_merged <- dt_merged[!is.na(label)]

cat(sprintf("Merged dataset (with labels): %s rows\n", format(nrow(dt_merged), big.mark = ",")))

# === LABEL DIAGNOSTIK ===
cat("\n=== LABEL DIAGNOSTIK ===\n")
cat("Label-Verteilung:\n")
print(table(dt_merged$label, useNA = "ifany"))

# Berechne Class-Imbalance für Long (label == 1) und Short (label == -1)
n_total <- nrow(dt_merged)
n_long_positive <- sum(dt_merged$label == 1, na.rm = TRUE)
n_short_positive <- sum(dt_merged$label == -1, na.rm = TRUE)
n_neutral <- sum(dt_merged$label == 0, na.rm = TRUE)

cat(sprintf("\nClass Balance:\n"))
cat(sprintf("  Long  (label=1):  %s (%.1f%%)\n",
            format(n_long_positive, big.mark = ","), n_long_positive / n_total * 100))
cat(sprintf("  Short (label=-1): %s (%.1f%%)\n",
            format(n_short_positive, big.mark = ","), n_short_positive / n_total * 100))
cat(sprintf("  Neutral (label=0): %s (%.1f%%)\n",
            format(n_neutral, big.mark = ","), n_neutral / n_total * 100))

# Warnung bei extremer Imbalance
long_ratio <- n_long_positive / (n_long_positive + n_neutral + n_short_positive)
short_ratio <- n_short_positive / (n_long_positive + n_neutral + n_short_positive)

if (long_ratio < 0.05) {
  cat("\n⚠️ WARNUNG: Weniger als 5% Long-Labels!\n")
  cat("   → Model wird Schwierigkeiten haben, Long-Entries zu lernen\n")
}
if (short_ratio < 0.05) {
  cat("\n⚠️ WARNUNG: Weniger als 5% Short-Labels!\n")
  cat("   → Model wird Schwierigkeiten haben, Short-Entries zu lernen\n")
}
if (n_neutral / n_total > 0.9) {
  cat("\n⚠️ WARNUNG: Mehr als 90% neutrale Labels!\n")
  cat("   → Sehr wenige positive Beispiele zum Lernen\n")
}

# Define test window (longer validation period for more trades)
test_train_start <- "2022-01-01"
test_train_end <- "2023-06-30"
test_val_start <- "2023-07-01"
test_val_end <- "2023-09-30"  # 3 months validation

# For testing, use adaptive threshold based on prediction distribution
# The 0.6 threshold is too high for this weak model
# In production, threshold will be optimized per feature group
CONFIG_TEST <- CONFIG
CONFIG_TEST$entry_threshold <- 0.32  # Use 60th percentile of predictions
CONFIG_TEST$min_trades_significance <- 20  # Lower for testing

cat(sprintf("\n=== TEST WINDOW ===\n"))
cat(sprintf("Train: %s to %s (6 months)\n", test_train_start, test_train_end))
cat(sprintf("Val:   %s to %s (1 month)\n", test_val_start, test_val_end))

# Select first LONG feature group for testing
test_group_name <- names(feature_groups_long)[1]
test_features <- feature_groups_long[[test_group_name]]

cat(sprintf("\n=== TEST FEATURE GROUP: %s ===\n", test_group_name))
cat(sprintf("Features (%d):\n", length(test_features)))
for (f in test_features) {
  cat(sprintf("  - %s\n", f))
}

# We need the full price data (with features) for simulation
# dt_full already has all features loaded from STEP 2
# dt_merged has the labels merged in

cat(sprintf("\nUsing entry_threshold = %.2f for testing\n", CONFIG_TEST$entry_threshold))
cat("(Production will use optimized threshold based on validation performance)\n")

# Run evaluation for LONG
cat("\n--- LONG DIRECTION ---\n")
result_long_test <- evaluate_feature_group_single_window(
  dt_labeled = dt_merged,       # Labeled data for training
  dt_all_prices = dt_full,      # All price data for prediction/simulation
  feature_group = test_features,
  train_start = test_train_start,
  train_end = test_train_end,
  val_start = test_val_start,
  val_end = test_val_end,
  direction = "long",
  config = CONFIG_TEST  # Use test config with lower threshold
)

if (result_long_test$success) {
  m <- result_long_test$metrics
  cat(sprintf("\n  LONG Results:\n"))
  cat(sprintf("    Trades: %d\n", m$n_trades))
  if (m$n_trades >= CONFIG_TEST$min_trades_significance) {
    cat(sprintf("    Win Rate: %.1f%%\n", m$win_rate * 100))
    cat(sprintf("    Profit Factor: %.2f\n", m$profit_factor))
    cat(sprintf("    Sharpe Ratio: %.2f\n", m$sharpe))
    cat(sprintf("    Total Return: %.2f%%\n", m$total_return * 100))
    cat(sprintf("    Max Drawdown: %.2f%%\n", m$max_drawdown * 100))
    cat(sprintf("    Avg Bars Held: %.1f\n", m$avg_bars_held))
  } else {
    cat(sprintf("    WARNING: Insufficient trades (%d < %d)\n",
                m$n_trades, CONFIG_TEST$min_trades_significance))
  }
} else {
  cat(sprintf("  ERROR: %s\n", result_long_test$error))
}

# Run evaluation for SHORT
cat("\n--- SHORT DIRECTION ---\n")
result_short_test <- evaluate_feature_group_single_window(
  dt_labeled = dt_merged,       # Labeled data for training
  dt_all_prices = dt_full,      # All price data for prediction/simulation
  feature_group = test_features,
  train_start = test_train_start,
  train_end = test_train_end,
  val_start = test_val_start,
  val_end = test_val_end,
  direction = "short",
  config = CONFIG_TEST  # Use test config with lower threshold
)

if (result_short_test$success) {
  m <- result_short_test$metrics
  cat(sprintf("\n  SHORT Results:\n"))
  cat(sprintf("    Trades: %d\n", m$n_trades))
  if (m$n_trades >= CONFIG_TEST$min_trades_significance) {
    cat(sprintf("    Win Rate: %.1f%%\n", m$win_rate * 100))
    cat(sprintf("    Profit Factor: %.2f\n", m$profit_factor))
    cat(sprintf("    Sharpe Ratio: %.2f\n", m$sharpe))
    cat(sprintf("    Total Return: %.2f%%\n", m$total_return * 100))
    cat(sprintf("    Max Drawdown: %.2f%%\n", m$max_drawdown * 100))
  } else {
    cat(sprintf("    WARNING: Insufficient trades (%d < %d)\n",
                m$n_trades, CONFIG_TEST$min_trades_significance))
  }
} else {
  cat(sprintf("  ERROR: %s\n", result_short_test$error))
}

# Show feature importance
if (result_long_test$success && !is.null(result_long_test$feature_importance)) {
  cat("\n=== FEATURE IMPORTANCE (LONG MODEL) ===\n")
  print(head(result_long_test$feature_importance, 10))
}

# Show sample trades
if (result_long_test$success && nrow(result_long_test$trades_df) > 0) {
  cat("\n=== SAMPLE TRADES (First 5 Long) ===\n")
  sample_trades <- head(result_long_test$trades_df, 5)
  sample_trades_display <- sample_trades[, .(
    entry = format(entry_time, "%Y-%m-%d %H:%M"),
    exit = format(exit_time, "%Y-%m-%d %H:%M"),
    entry_px = round(entry_price, 2),
    exit_px = round(exit_price, 2),
    pnl_pct = round(pnl_net * 100, 3),
    bars = bars_held,
    reason = exit_reason
  )]
  print(sample_trades_display)
}

log_message("Single window backtest completed")

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 3 COMPLETE - SINGLE WINDOW BACKTEST TESTED\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Save workspace
save.image(file.path(CONFIG$cache_path, "step3_complete.RData"))
cat(sprintf("\nWorkspace saved: %s\n", file.path(CONFIG$cache_path, "step3_complete.RData")))

# ============================================================================
# STEP 4: FULL WALK-FORWARD PIPELINE
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 4: FULL WALK-FORWARD PIPELINE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Starting full walk-forward evaluation...")

#' Generate Walk-Forward Windows
#'
#' Creates a sequence of train/validation windows for walk-forward analysis.
#' Each window trains on historical data and validates on subsequent period.
#'
#' @param start_date Overall start date (string "YYYY-MM-DD")
#' @param end_date Overall end date (string "YYYY-MM-DD")
#' @param train_months Number of months for training window
#' @param val_months Number of months for validation window
#' @param step_months How many months to step forward between windows
#' @return data.table with window definitions
generate_walk_forward_windows <- function(
    start_date = "2019-01-01",
    end_date = "2024-12-31",
    train_months = 18,
    val_months = 3,
    step_months = 3
) {

  start <- as.Date(start_date)
  end <- as.Date(end_date)

  windows <- list()
  window_id <- 1

  current_train_start <- start

  while (TRUE) {
    # Calculate window boundaries
    train_end <- current_train_start %m+% months(train_months) - days(1)
    val_start <- train_end + days(1)
    val_end <- val_start %m+% months(val_months) - days(1)

    # Stop if validation period exceeds end date
    if (val_end > end) {
      break
    }

    windows[[window_id]] <- data.table(
      window_id = window_id,
      train_start = as.character(current_train_start),
      train_end = as.character(train_end),
      val_start = as.character(val_start),
      val_end = as.character(val_end)
    )

    # Step forward
    current_train_start <- current_train_start %m+% months(step_months)
    window_id <- window_id + 1
  }

  windows_dt <- rbindlist(windows)

  cat(sprintf("Generated %d walk-forward windows:\n", nrow(windows_dt)))
  for (i in 1:nrow(windows_dt)) {
    w <- windows_dt[i]
    cat(sprintf("  Window %d: Train %s to %s | Val %s to %s\n",
                w$window_id, w$train_start, w$train_end, w$val_start, w$val_end))
  }

  return(windows_dt)
}


#' Evaluate Single Feature Group Across All Windows
#'
#' Runs walk-forward evaluation for one feature group across all time windows.
#' Aggregates metrics to assess consistency of performance.
#'
#' @param group_name Name of the feature group
#' @param feature_cols Vector of feature column names
#' @param windows_dt data.table with window definitions
#' @param dt_labeled Labeled dataset for training
#' @param dt_all_prices Full price dataset for prediction/simulation
#' @param direction "long" or "short"
#' @param config Configuration list
#' @return data.table with results per window
evaluate_group_walk_forward <- function(
    group_name,
    feature_cols,
    windows_dt,
    dt_labeled,
    dt_all_prices,
    direction = "long",
    config = CONFIG
) {

  results <- list()

  for (i in 1:nrow(windows_dt)) {
    w <- windows_dt[i]

    # Run single window evaluation
    result <- tryCatch({
      evaluate_feature_group_single_window(
        dt_labeled = dt_labeled,
        dt_all_prices = dt_all_prices,
        feature_group = feature_cols,
        train_start = w$train_start,
        train_end = w$train_end,
        val_start = w$val_start,
        val_end = w$val_end,
        direction = direction,
        config = config
      )
    }, error = function(e) {
      list(success = FALSE, error = as.character(e), metrics = NULL)
    })

    # Extract metrics
    if (result$success && !is.null(result$metrics)) {
      m <- result$metrics
      results[[i]] <- data.table(
        group_name = group_name,
        window_id = w$window_id,
        direction = direction,
        train_start = w$train_start,
        train_end = w$train_end,
        val_start = w$val_start,
        val_end = w$val_end,
        n_trades = m$n_trades,
        win_rate = m$win_rate,
        profit_factor = m$profit_factor,
        sharpe = m$sharpe,
        total_return = m$total_return,
        max_drawdown = m$max_drawdown,
        avg_bars_held = m$avg_bars_held,
        success = TRUE,
        error = NA_character_
      )
    } else {
      results[[i]] <- data.table(
        group_name = group_name,
        window_id = w$window_id,
        direction = direction,
        train_start = w$train_start,
        train_end = w$train_end,
        val_start = w$val_start,
        val_end = w$val_end,
        n_trades = 0L,
        win_rate = NA_real_,
        profit_factor = NA_real_,
        sharpe = NA_real_,
        total_return = NA_real_,
        max_drawdown = NA_real_,
        avg_bars_held = NA_real_,
        success = FALSE,
        error = if (is.null(result$error)) "Unknown error" else result$error
      )
    }
  }

  return(rbindlist(results))
}


#' Run Full Walk-Forward Pipeline for All Feature Groups
#'
#' Main orchestration function that evaluates feature groups across
#' all walk-forward windows. Uses separate groups for LONG and SHORT.
#'
#' @param feature_groups_long List of feature groups for LONG direction
#' @param feature_groups_short List of feature groups for SHORT direction
#' @param windows_dt Walk-forward window definitions
#' @param dt_labeled Labeled dataset
#' @param dt_all_prices Full price dataset
#' @param config Configuration list
#' @param n_cores Number of parallel cores (1 = sequential)
#' @param test_mode If TRUE, only evaluate first 5 groups per direction
#' @return List with results_long, results_short, and aggregated summaries
run_full_walk_forward <- function(
    feature_groups_long,
    feature_groups_short,
    windows_dt,
    dt_labeled,
    dt_all_prices,
    config = CONFIG,
    n_cores = 1,
    test_mode = FALSE
) {

  # Select groups to evaluate
  if (test_mode) {
    groups_long <- head(names(feature_groups_long), 5)
    groups_short <- head(names(feature_groups_short), 5)
    cat(sprintf("TEST MODE: Evaluating first 5 groups per direction\n\n"))
  } else {
    groups_long <- names(feature_groups_long)
    groups_short <- names(feature_groups_short)
    cat(sprintf("FULL MODE: %d LONG groups, %d SHORT groups\n\n",
                length(groups_long), length(groups_short)))
  }

  total_evals <- (length(groups_long) + length(groups_short)) * nrow(windows_dt)
  cat(sprintf("Total evaluations: %d ((%d + %d) groups x %d windows)\n\n",
              total_evals, length(groups_long), length(groups_short), nrow(windows_dt)))

  # Initialize results storage
  all_results_long <- list()
  all_results_short <- list()

  # Progress tracking
  eval_count <- 0
  start_time <- Sys.time()

  # ===== EVALUATE LONG GROUPS =====
  cat("\n=== EVALUATING LONG GROUPS ===\n\n")

  for (group_name in groups_long) {
    feature_cols <- feature_groups_long[[group_name]]

    cat(sprintf("[LONG %d/%d] %s (%d features): ",
                which(groups_long == group_name),
                length(groups_long),
                group_name,
                length(feature_cols)))

    result_long <- tryCatch({
      evaluate_group_walk_forward(
        group_name = group_name,
        feature_cols = feature_cols,
        windows_dt = windows_dt,
        dt_labeled = dt_labeled,
        dt_all_prices = dt_all_prices,
        direction = "long",
        config = config
      )
    }, error = function(e) {
      cat(sprintf("ERROR - %s\n", e$message))
      NULL
    })

    if (!is.null(result_long)) {
      avg_sharpe <- mean(result_long$sharpe, na.rm = TRUE)
      total_trades <- sum(result_long$n_trades)
      cat(sprintf("Sharpe=%.2f, Trades=%d\n", avg_sharpe, total_trades))
      all_results_long[[group_name]] <- result_long
    }

    eval_count <- eval_count + 1
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    if (elapsed > 0) {
      rate <- eval_count / elapsed
      remaining <- (total_evals - eval_count) / rate
      cat(sprintf("  Progress: %.1f%% | ETA: %.1fmin\n",
                  eval_count / total_evals * 100, remaining))
    }
  }

  # ===== EVALUATE SHORT GROUPS =====
  cat("\n=== EVALUATING SHORT GROUPS ===\n\n")

  for (group_name in groups_short) {
    feature_cols <- feature_groups_short[[group_name]]

    cat(sprintf("[SHORT %d/%d] %s (%d features): ",
                which(groups_short == group_name),
                length(groups_short),
                group_name,
                length(feature_cols)))

    result_short <- tryCatch({
      evaluate_group_walk_forward(
        group_name = group_name,
        feature_cols = feature_cols,
        windows_dt = windows_dt,
        dt_labeled = dt_labeled,
        dt_all_prices = dt_all_prices,
        direction = "short",
        config = config
      )
    }, error = function(e) {
      cat(sprintf("ERROR - %s\n", e$message))
      NULL
    })

    if (!is.null(result_short)) {
      avg_sharpe <- mean(result_short$sharpe, na.rm = TRUE)
      total_trades <- sum(result_short$n_trades)
      cat(sprintf("Sharpe=%.2f, Trades=%d\n", avg_sharpe, total_trades))
      all_results_short[[group_name]] <- result_short
    }

    eval_count <- eval_count + 1
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    if (elapsed > 0) {
      rate <- eval_count / elapsed
      remaining <- (total_evals - eval_count) / rate
      cat(sprintf("  Progress: %.1f%% | ETA: %.1fmin\n",
                  eval_count / total_evals * 100, remaining))
    }
  }

  # Combine all results
  results_long_dt <- rbindlist(all_results_long, fill = TRUE)
  results_short_dt <- rbindlist(all_results_short, fill = TRUE)

  # ===== AGGREGATE RESULTS =====
  cat("\n=== AGGREGATING RESULTS ===\n")

  # Long aggregation
  long_summary <- results_long_dt[, .(
    n_windows = .N,
    n_windows_success = sum(success),
    total_trades = sum(n_trades, na.rm = TRUE),
    avg_trades_per_window = mean(n_trades, na.rm = TRUE),
    mean_sharpe = mean(sharpe, na.rm = TRUE),
    median_sharpe = median(sharpe, na.rm = TRUE),
    sd_sharpe = sd(sharpe, na.rm = TRUE),
    mean_profit_factor = mean(profit_factor, na.rm = TRUE),
    mean_win_rate = mean(win_rate, na.rm = TRUE),
    mean_return = mean(total_return, na.rm = TRUE),
    total_return = sum(total_return, na.rm = TRUE),
    mean_max_dd = mean(max_drawdown, na.rm = TRUE),
    consistency = sum(sharpe > 0, na.rm = TRUE) / sum(!is.na(sharpe))
  ), by = group_name]

  # Short aggregation
  short_summary <- results_short_dt[, .(
    n_windows = .N,
    n_windows_success = sum(success),
    total_trades = sum(n_trades, na.rm = TRUE),
    avg_trades_per_window = mean(n_trades, na.rm = TRUE),
    mean_sharpe = mean(sharpe, na.rm = TRUE),
    median_sharpe = median(sharpe, na.rm = TRUE),
    sd_sharpe = sd(sharpe, na.rm = TRUE),
    mean_profit_factor = mean(profit_factor, na.rm = TRUE),
    mean_win_rate = mean(win_rate, na.rm = TRUE),
    mean_return = mean(total_return, na.rm = TRUE),
    total_return = sum(total_return, na.rm = TRUE),
    mean_max_dd = mean(max_drawdown, na.rm = TRUE),
    consistency = sum(sharpe > 0, na.rm = TRUE) / sum(!is.na(sharpe))
  ), by = group_name]

  long_summary[, direction := "long"]
  short_summary[, direction := "short"]

  # ===== RANK FEATURE GROUPS =====
  # Composite score: weighted combination of Sharpe, Consistency, and Trade Count

  long_summary[, composite_score := (
    0.4 * scale(mean_sharpe)[, 1] +
      0.3 * scale(consistency)[, 1] +
      0.2 * scale(mean_profit_factor)[, 1] +
      0.1 * scale(log1p(total_trades))[, 1]
  )]

  short_summary[, composite_score := (
    0.4 * scale(mean_sharpe)[, 1] +
      0.3 * scale(consistency)[, 1] +
      0.2 * scale(mean_profit_factor)[, 1] +
      0.1 * scale(log1p(total_trades))[, 1]
  )]

  # Handle NaN from scaling
  long_summary[is.na(composite_score), composite_score := -Inf]
  short_summary[is.na(composite_score), composite_score := -Inf]

  # Sort by composite score
  setorder(long_summary, -composite_score)
  setorder(short_summary, -composite_score)

  # Add rank
  long_summary[, rank := .I]
  short_summary[, rank := .I]

  # Print top groups
  cat("\n=== TOP 10 FEATURE GROUPS (LONG) ===\n")
  print(long_summary[1:min(10, nrow(long_summary)), .(
    rank, group_name, mean_sharpe, consistency, mean_pf = mean_profit_factor,
    trades = total_trades, score = round(composite_score, 2)
  )])

  cat("\n=== TOP 10 FEATURE GROUPS (SHORT) ===\n")
  print(short_summary[1:min(10, nrow(short_summary)), .(
    rank, group_name, mean_sharpe, consistency, mean_pf = mean_profit_factor,
    trades = total_trades, score = round(composite_score, 2)
  )])

  # Return comprehensive results
  return(list(
    results_long = results_long_dt,
    results_short = results_short_dt,
    summary_long = long_summary,
    summary_short = short_summary,
    windows = windows_dt,
    config = config,
    timestamp = Sys.time()
  ))
}


# ===== EXECUTE STEP 4 ========================================================

# Generate walk-forward windows
cat("Generating walk-forward windows...\n\n")

walk_forward_windows <- generate_walk_forward_windows(
  start_date = "2019-06-01",    # Start with some buffer for feature calculation
  end_date = "2024-09-30",      # Leave 2024-Q4 for final test
  train_months = 18,            # 18 months training
  val_months = 3,               # 3 months validation
  step_months = 3               # Step forward 3 months
)

# Ensure data is loaded (from STEP 3)
if (!exists("dt_merged") || !exists("dt_full")) {
  stop("Required data not found. Please run STEP 1-3 first, or load step3_complete.RData")
}

# Run walk-forward evaluation
# Set test_mode = TRUE for initial validation, FALSE for full run
cat("\n")
cat(paste0(rep("-", 80), collapse = ""))
cat("\n")
cat("RUNNING WALK-FORWARD EVALUATION\n")
cat(paste0(rep("-", 80), collapse = ""))
cat("\n\n")

# Run with separate LONG and SHORT groups
walk_forward_results <- run_full_walk_forward(
  feature_groups_long = feature_groups_long,
  feature_groups_short = feature_groups_short,
  windows_dt = walk_forward_windows,
  dt_labeled = dt_merged,
  dt_all_prices = dt_full,
  config = CONFIG,
  n_cores = 1,
  test_mode = FALSE  # Set to TRUE to evaluate only first 5 groups per direction
)

# Save intermediate results
results_file <- file.path(
  CONFIG$cache_path,
  sprintf("%s_%s_walk_forward_results.rds", CONFIG$epic, CONFIG$interval)
)
saveRDS(walk_forward_results, results_file)
cat(sprintf("\nResults saved: %s\n", results_file))

# Also save as CSV for easy inspection
fwrite(
  walk_forward_results$summary_long,
  file.path(CONFIG$cache_path, sprintf("%s_%s_long_group_ranking.csv", CONFIG$epic, CONFIG$interval))
)
fwrite(
  walk_forward_results$summary_short,
  file.path(CONFIG$cache_path, sprintf("%s_%s_short_group_ranking.csv", CONFIG$epic, CONFIG$interval))
)

log_message("Walk-forward evaluation completed")

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 4 COMPLETE - WALK-FORWARD EVALUATION DONE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Save workspace
save.image(file.path(CONFIG$cache_path, "step4_complete.RData"))
cat(sprintf("\nWorkspace saved: %s\n", file.path(CONFIG$cache_path, "step4_complete.RData")))

# ============================================================================
# STEP 5: FINAL MODEL & REPORTING
# ============================================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 5: FINAL MODEL TRAINING & REPORTING\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

log_message("Building final models from top feature groups...")

#' Select Top Features with Correlation Cleaning
#'
#' Takes top-ranked feature groups, extracts individual features,
#' and removes highly correlated features to avoid redundancy.
#'
#' @param summary_dt Summary data.table with group rankings
#' @param feature_groups List of all feature groups
#' @param dt_sample Sample data for correlation calculation
#' @param n_top_groups Number of top groups to consider
#' @param max_features Maximum features to select
#' @param corr_threshold Correlation threshold for removal (default 0.85)
#' @return Vector of selected feature names
select_features_with_correlation_cleaning <- function(
    summary_dt,
    feature_groups,
    dt_sample,
    n_top_groups = 15,
    max_features = 50,
    corr_threshold = 0.85
) {

  cat(sprintf("Selecting features from top %d groups...\n", n_top_groups))

  # Get top groups
  top_groups <- head(summary_dt[order(-composite_score)]$group_name, n_top_groups)

  # Extract all features from top groups
  candidate_features <- unique(unlist(lapply(top_groups, function(g) {
    feature_groups[[g]]
  })))

  # Filter to features that exist in data
  candidate_features <- intersect(candidate_features, names(dt_sample))
  cat(sprintf("Candidate features: %d\n", length(candidate_features)))

  if (length(candidate_features) == 0) {
    warning("No candidate features found!")
    return(character(0))
  }

  # Calculate correlation matrix
  cat("Calculating correlation matrix...\n")

  # Use sample of data for speed
  sample_size <- min(10000, nrow(dt_sample))
  dt_corr_sample <- dt_sample[sample(.N, sample_size)]

  corr_matrix <- cor(
    as.matrix(dt_corr_sample[, ..candidate_features]),
    use = "pairwise.complete.obs"
  )

  # Handle NA correlations
  corr_matrix[is.na(corr_matrix)] <- 0

  # Greedy feature selection with correlation threshold
  selected_features <- character(0)
  remaining_features <- candidate_features

  while (length(remaining_features) > 0 && length(selected_features) < max_features) {
    # Take next feature
    next_feature <- remaining_features[1]
    selected_features <- c(selected_features, next_feature)

    # Remove highly correlated features
    if (length(remaining_features) > 1) {
      correlations <- abs(corr_matrix[next_feature, remaining_features[-1]])
      high_corr_mask <- correlations >= corr_threshold
      features_to_remove <- remaining_features[-1][high_corr_mask]

      if (length(features_to_remove) > 0) {
        cat(sprintf("  Removed %d features correlated with %s\n",
                    length(features_to_remove), next_feature))
      }

      remaining_features <- setdiff(remaining_features[-1], features_to_remove)
    } else {
      remaining_features <- character(0)
    }
  }

  cat(sprintf("\nSelected %d features after correlation cleaning\n", length(selected_features)))

  return(selected_features)
}


#' Train Final Production Model
#'
#' Trains a final XGBoost model on full training data with selected features.
#' Uses more trees and proper hyperparameters for production.
#'
#' @param dt_train Training dataset with labels
#' @param dt_val Validation dataset for early stopping
#' @param feature_cols Selected feature columns
#' @param direction "long" or "short"
#' @param params XGBoost parameters
#' @return List with model, metrics, and feature importance
train_final_model <- function(
    dt_train,
    dt_val,
    feature_cols,
    direction = "long",
    params = list(
      max_depth = 4,
      eta = 0.1,
      nrounds = 1000,
      early_stopping_rounds = 50,
      min_child_weight = 10,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
) {

  cat(sprintf("Training final %s model...\n", toupper(direction)))
  cat(sprintf("Features: %d\n", length(feature_cols)))
  cat(sprintf("Training rows: %s\n", format(nrow(dt_train), big.mark = ",")))
  cat(sprintf("Validation rows: %s\n", format(nrow(dt_val), big.mark = ",")))

  # Create binary labels based on direction
  if (direction == "long") {
    dt_train[, label_binary := fifelse(label == 1, 1, 0)]
    dt_val[, label_binary := fifelse(label == 1, 1, 0)]
  } else {
    dt_train[, label_binary := fifelse(label == -1, 1, 0)]
    dt_val[, label_binary := fifelse(label == -1, 1, 0)]
  }

  # Prepare matrices
  X_train <- as.matrix(dt_train[, ..feature_cols])
  y_train <- dt_train$label_binary
  X_val <- as.matrix(dt_val[, ..feature_cols])
  y_val <- dt_val$label_binary

  # Handle NA values
  X_train[is.na(X_train)] <- 0
  X_val[is.na(X_val)] <- 0

  # ===== CLASS IMBALANCE HANDLING (FIX) =====
  n_positive <- sum(y_train == 1)
  n_negative <- sum(y_train == 0)
  scale_pos_weight <- if (n_positive > 0) n_negative / n_positive else 1
  cat(sprintf("Class balance: %d positive (%.1f%%), %d negative (%.1f%%), scale_pos_weight=%.2f\n",
              n_positive, 100 * n_positive / length(y_train),
              n_negative, 100 * n_negative / length(y_train),
              scale_pos_weight))

  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval <- xgb.DMatrix(data = X_val, label = y_val)

  # Set parameters with scale_pos_weight for class imbalance
  xgb_params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = params$max_depth,
    eta = params$eta,
    min_child_weight = params$min_child_weight %||% 10,
    subsample = params$subsample %||% 0.8,
    colsample_bytree = params$colsample_bytree %||% 0.8,
    gamma = params$gamma %||% 0.1,
    scale_pos_weight = scale_pos_weight
  )

  # Train with early stopping
  cat("Training XGBoost model...\n")

  model <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = params$nrounds %||% 500,
    evals = list(train = dtrain, val = dval),
    early_stopping_rounds = params$early_stopping_rounds %||% 50,
    verbose = 1,
    print_every_n = 50
  )

  cat(sprintf("\nBest iteration: %d\n", model$best_iteration))

  # Predictions
  pred_train <- predict(model, dtrain)
  pred_val <- predict(model, dval)

  # Calculate AUC
  auc_train <- pROC::auc(pROC::roc(y_train, pred_train, quiet = TRUE))
  auc_val <- pROC::auc(pROC::roc(y_val, pred_val, quiet = TRUE))

  cat(sprintf("AUC Train: %.4f | AUC Val: %.4f\n", auc_train, auc_val))

  # === MODEL QUALITY DIAGNOSTIK ===
  cat("\n=== MODEL QUALITY CHECK ===\n")

  # AUC Interpretation
  if (auc_val < 0.52) {
    cat("⚠️ KRITISCH: AUC < 0.52 - Model ist SCHLECHTER als Zufall!\n")
  } else if (auc_val < 0.55) {
    cat("⚠️ WARNUNG: AUC < 0.55 - Model hat kaum Vorhersagekraft\n")
  } else if (auc_val < 0.60) {
    cat("ℹ️ INFO: AUC 0.55-0.60 - Schwache aber messbare Vorhersagekraft\n")
  } else {
    cat("✓ OK: AUC > 0.60 - Model hat Vorhersagekraft\n")
  }

  # Overfitting Check
  auc_gap <- as.numeric(auc_train) - as.numeric(auc_val)
  cat(sprintf("\nOverfitting Check: Train AUC - Val AUC = %.4f\n", auc_gap))
  if (auc_gap > 0.1) {
    cat("⚠️ WARNUNG: Grosser AUC-Gap (>0.1) - Moegliches Overfitting\n")
  }

  # Prediction vs Label Analyse
  pred_when_label_1 <- mean(pred_val[y_val == 1], na.rm = TRUE)
  pred_when_label_0 <- mean(pred_val[y_val == 0], na.rm = TRUE)
  separation <- pred_when_label_1 - pred_when_label_0

  cat(sprintf("\nMean Prediction when Label=1: %.4f\n", pred_when_label_1))
  cat(sprintf("Mean Prediction when Label=0: %.4f\n", pred_when_label_0))
  cat(sprintf("Separation: %.4f\n", separation))

  if (separation < 0.05) {
    cat("⚠️ WARNUNG: Predictions unterscheiden kaum zwischen Label 0/1\n")
    cat("   → Model hat keinen echten Signal gefunden\n")
  }

  # Feature importance
  importance <- xgb.importance(feature_names = feature_cols, model = model)

  # === FEATURE IMPORTANCE CHECK ===
  cat("\n=== TOP 10 FEATURE IMPORTANCE ===\n")
  top_10 <- head(importance, 10)
  for (i in 1:min(nrow(top_10), 10)) {
    cat(sprintf("  %2d. %s: Gain=%.4f, Cover=%.4f\n",
                i, top_10$Feature[i], top_10$Gain[i], top_10$Cover[i]))
  }

  # Check if one feature dominates
  if (nrow(importance) > 0 && importance$Gain[1] > 0.5) {
    cat(sprintf("\n⚠️ WARNUNG: Feature '%s' dominiert mit %.1f%% Importance\n",
                importance$Feature[1], importance$Gain[1] * 100))
    cat("   → Moeglicherweise Datenleck oder Feature-Problem\n")
  }

  return(list(
    model = model,
    feature_cols = feature_cols,
    direction = direction,
    auc_train = as.numeric(auc_train),
    auc_val = as.numeric(auc_val),
    best_iteration = model$best_iteration,
    importance = importance,
    predictions_val = pred_val,
    labels_val = y_val
  ))
}


#' Run Final Test Set Evaluation
#'
#' Evaluates final model on held-out test set (2024 Q4 or later).
#' Simulates trades and calculates final PnL metrics.
#'
#' @param model_result Result from train_final_model()
#' @param dt_test_prices Test set price data
#' @param config Configuration list
#' @return List with test metrics and trades
evaluate_on_test_set <- function(
    model_result,
    dt_test_prices,
    config = CONFIG
) {

  cat(sprintf("\n=== FINAL TEST SET EVALUATION (%s) ===\n", model_result$direction))
  cat(sprintf("Test period: %s to %s\n",
              min(dt_test_prices$datetime),
              max(dt_test_prices$datetime)))
  cat(sprintf("Test rows: %s\n", format(nrow(dt_test_prices), big.mark = ",")))

  # Prepare test features
  feature_cols <- model_result$feature_cols
  X_test <- as.matrix(dt_test_prices[, ..feature_cols])
  X_test[is.na(X_test)] <- 0
  dtest <- xgb.DMatrix(data = X_test)

  # Generate predictions
  predictions <- predict(model_result$model, dtest)

  # === PREDICTION DIAGNOSTIK ===
  cat("\n=== PREDICTION DIAGNOSTIK ===\n")
  cat("Predictions Summary:\n")
  cat(sprintf("  Min:    %.4f\n", min(predictions)))
  cat(sprintf("  25%%:    %.4f\n", quantile(predictions, 0.25)))
  cat(sprintf("  Median: %.4f\n", median(predictions)))
  cat(sprintf("  75%%:    %.4f\n", quantile(predictions, 0.75)))
  cat(sprintf("  Max:    %.4f\n", max(predictions)))
  cat(sprintf("  SD:     %.4f\n", sd(predictions)))

  # Confidence-Check
  pred_range <- max(predictions) - min(predictions)
  if (pred_range < 0.2) {
    cat("\n⚠️ WARNUNG: Predictions haben sehr geringe Varianz (Range < 0.2)\n")
    cat("   → Model ist unsicher / hat nichts gelernt\n")
  }

  if (max(predictions) < 0.55) {
    cat("\n⚠️ WARNUNG: Keine Prediction über 0.55\n")
    cat("   → Model sagt nie 'kaufen' mit Überzeugung\n")
  }

  # Histogram als ASCII
  cat("\nPrediction Distribution:\n")
  breaks <- seq(0, 1, by = 0.1)
  hist_counts <- hist(predictions, breaks = breaks, plot = FALSE)$counts
  max_count <- max(hist_counts)
  for (i in seq_along(hist_counts)) {
    bar_len <- round(hist_counts[i] / max_count * 40)
    cat(sprintf("  %.1f-%.1f: %s (%d)\n",
                breaks[i], breaks[i+1],
                paste(rep("#", bar_len), collapse = ""),
                hist_counts[i]))
  }

  # === THRESHOLD ANALYSE ===
  cat("\n=== THRESHOLD ANALYSE ===\n")
  for (thresh in c(0.50, 0.52, 0.55, 0.58, 0.60)) {
    n_signals <- sum(predictions >= thresh)
    pct_signals <- n_signals / length(predictions) * 100
    cat(sprintf("  Threshold %.2f: %d signals (%.1f%%)\n", thresh, n_signals, pct_signals))
  }

  # ===== DYNAMIC THRESHOLD (FIX) =====
  # Use dynamic threshold calculation based on prediction distribution
  dynamic_threshold <- calculate_dynamic_threshold(
    predictions = predictions,
    method = "percentile",
    percentile = 70,
    min_threshold = 0.5,
    max_threshold = 0.8
  )
  cat(sprintf("\nUsing dynamic threshold: %.3f\n", dynamic_threshold))
  cat(sprintf("Signals above threshold: %d\n", sum(predictions >= dynamic_threshold)))

  # Simulate trades
  sim_result <- simulate_trades(
    predictions = predictions,
    dt_prices = dt_test_prices,
    direction = model_result$direction,
    entry_threshold = dynamic_threshold,
    atr_mult_tp = config$atr_multiplier_tp,
    atr_mult_sl = config$atr_multiplier_sl,
    max_bars = config$max_bars_held,
    slippage_pct = config$slippage_pct,
    commission_pct = config$commission_pct,
    session_filter = config$use_session_filter
  )

  # Print results
  m <- sim_result$metrics
  cat(sprintf("\n--- %s TEST RESULTS ---\n", toupper(model_result$direction)))
  cat(sprintf("Trades: %d\n", m$n_trades))

  if (m$n_trades > 0) {
    cat(sprintf("Win Rate: %.1f%% (%d/%d)\n", m$win_rate * 100, m$wins, m$n_trades))
    cat(sprintf("Profit Factor: %.2f\n", m$profit_factor))
    cat(sprintf("Sharpe Ratio: %.2f\n", m$sharpe))
    cat(sprintf("Total Return: %.2f%%\n", m$total_return * 100))
    cat(sprintf("Max Drawdown: %.2f%%\n", m$max_drawdown * 100))
    cat(sprintf("Avg Bars Held: %.1f\n", m$avg_bars_held))
    cat(sprintf("Avg Win: %.3f%% | Avg Loss: %.3f%%\n",
                m$gross_profit / m$wins * 100,
                m$gross_loss / m$losses * 100))
  }

  return(list(
    direction = model_result$direction,
    metrics = sim_result$metrics,
    trades_df = sim_result$trades_df,
    predictions = predictions
  ))
}


#' Generate Baseline Comparison with Permutation Test
#'
#' Compares model performance against random prediction baseline.
#' Shows value added by feature selection and modeling.
#' Includes p-value calculation for statistical significance.
#'
#' @param dt_test_prices Test set price data
#' @param n_simulations Number of random simulations
#' @param config Configuration list
#' @return data.table with baseline statistics
generate_baseline_comparison <- function(
    dt_test_prices,
    n_simulations = 100,
    config = CONFIG
) {

  cat("\n=== GENERATING BASELINE COMPARISON (PERMUTATION TEST) ===\n")
  cat(sprintf("Running %d random simulations for statistical significance...\n", n_simulations))

  baseline_results <- list()

  pb <- progress_bar$new(
    format = "  [:bar] :percent eta: :eta",
    total = n_simulations * 2,  # long + short
    clear = FALSE
  )

  for (i in 1:n_simulations) {
    # Random predictions (uniform noise - no signal)
    random_preds <- runif(nrow(dt_test_prices), min = 0.3, max = 0.7)

    # Long simulation
    result_long <- simulate_trades(
      predictions = random_preds,
      dt_prices = dt_test_prices,
      direction = "long",
      entry_threshold = config$entry_threshold,
      atr_mult_tp = config$atr_multiplier_tp,
      atr_mult_sl = config$atr_multiplier_sl,
      max_bars = config$max_bars_held,
      slippage_pct = config$slippage_pct,
      commission_pct = config$commission_pct,
      session_filter = config$use_session_filter
    )
    pb$tick()

    # Short simulation
    result_short <- simulate_trades(
      predictions = random_preds,
      dt_prices = dt_test_prices,
      direction = "short",
      entry_threshold = config$entry_threshold,
      atr_mult_tp = config$atr_multiplier_tp,
      atr_mult_sl = config$atr_multiplier_sl,
      max_bars = config$max_bars_held,
      slippage_pct = config$slippage_pct,
      commission_pct = config$commission_pct,
      session_filter = config$use_session_filter
    )
    pb$tick()

    baseline_results[[i]] <- data.table(
      sim_id = i,
      long_sharpe = result_long$metrics$sharpe,
      long_pf = result_long$metrics$profit_factor,
      long_return = result_long$metrics$total_return,
      long_trades = result_long$metrics$n_trades,
      long_win_rate = result_long$metrics$win_rate,
      short_sharpe = result_short$metrics$sharpe,
      short_pf = result_short$metrics$profit_factor,
      short_return = result_short$metrics$total_return,
      short_trades = result_short$metrics$n_trades,
      short_win_rate = result_short$metrics$win_rate
    )
  }

  baseline_dt <- rbindlist(baseline_results)

  # Calculate statistics with percentiles
  cat("\n--- BASELINE STATISTICS (PERMUTATION DISTRIBUTION) ---\n")
  cat(sprintf("LONG  - Mean Sharpe: %.2f (SD: %.2f) | 95th pct: %.2f | Mean PF: %.2f | Mean Return: %.2f%%\n",
              mean(baseline_dt$long_sharpe, na.rm = TRUE),
              sd(baseline_dt$long_sharpe, na.rm = TRUE),
              quantile(baseline_dt$long_sharpe, 0.95, na.rm = TRUE),
              mean(baseline_dt$long_pf, na.rm = TRUE),
              mean(baseline_dt$long_return, na.rm = TRUE) * 100))
  cat(sprintf("SHORT - Mean Sharpe: %.2f (SD: %.2f) | 95th pct: %.2f | Mean PF: %.2f | Mean Return: %.2f%%\n",
              mean(baseline_dt$short_sharpe, na.rm = TRUE),
              sd(baseline_dt$short_sharpe, na.rm = TRUE),
              quantile(baseline_dt$short_sharpe, 0.95, na.rm = TRUE),
              mean(baseline_dt$short_pf, na.rm = TRUE),
              mean(baseline_dt$short_return, na.rm = TRUE) * 100))

  return(baseline_dt)
}


#' Calculate p-value for model vs baseline
#'
#' Tests whether model performance is significantly better than random.
#' Uses empirical p-value from permutation distribution.
#'
#' @param model_metric Model's metric value (e.g., Sharpe ratio)
#' @param baseline_metrics Vector of baseline metric values from permutation test
#' @return p-value (probability that random achieves >= model performance)
calculate_permutation_pvalue <- function(model_metric, baseline_metrics) {
  if (is.na(model_metric) || length(baseline_metrics) == 0) {
    return(NA_real_)
  }

  # Remove NAs from baseline
  baseline_clean <- baseline_metrics[!is.na(baseline_metrics)]
  if (length(baseline_clean) == 0) {
    return(NA_real_)
  }

  # Empirical p-value: proportion of baseline >= model
  p_value <- mean(baseline_clean >= model_metric)

  return(p_value)
}


#' Generate HTML Report
#'
#' Creates a comprehensive HTML report summarizing the feature selection
#' pipeline results, including rankings, metrics, and visualizations.
#'
#' @param walk_forward_results Results from run_full_walk_forward()
#' @param final_model_long Long model results
#' @param final_model_short Short model results
#' @param test_results_long Long test results
#' @param test_results_short Short test results
#' @param baseline_dt Baseline comparison data
#' @param output_file Output HTML file path
generate_html_report <- function(
    walk_forward_results,
    final_model_long,
    final_model_short,
    test_results_long,
    test_results_short,
    baseline_dt,
    selected_features_long,
    selected_features_short,
    output_file = "pnl_feature_selection_report.html"
) {

  cat(sprintf("\nGenerating HTML report: %s\n", output_file))

  # Build HTML content
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>PnL-Based Feature Selection Report</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; background: #f5f5f5; }
    .container { max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; }
    h3 { color: #7f8c8d; }
    table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f9f9f9; }
    tr:hover { background-color: #f5f5f5; }
    .metric-box { display: inline-block; background: #ecf0f1; padding: 15px 25px; margin: 10px; border-radius: 5px; text-align: center; }
    .metric-value { font-size: 24px; font-weight: bold; color: #2c3e50; }
    .metric-label { font-size: 12px; color: #7f8c8d; }
    .positive { color: #27ae60; }
    .negative { color: #e74c3c; }
    .section { margin: 30px 0; padding: 20px; background: #fafafa; border-radius: 5px; }
    .feature-list { column-count: 3; column-gap: 20px; }
    .feature-item { break-inside: avoid; padding: 5px 0; }
    .timestamp { color: #95a5a6; font-size: 12px; }
  </style>
</head>
<body>
<div class="container">
  <h1>PnL-Based Feature Selection Report</h1>
  <p class="timestamp">Generated: %s</p>

  <h2>Executive Summary</h2>
  <div class="section">
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Feature Groups Evaluated</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Walk-Forward Windows</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Selected Features (Long)</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Selected Features (Short)</div>
    </div>
  </div>

  <h2>Final Test Results</h2>
  <div class="section">
    <h3>Long Model</h3>
    <div class="metric-box">
      <div class="metric-value %s">%.2f</div>
      <div class="metric-label">Sharpe Ratio</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%.2f</div>
      <div class="metric-label">Profit Factor</div>
    </div>
    <div class="metric-box">
      <div class="metric-value %s">%.1f%%</div>
      <div class="metric-label">Total Return</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%.1f%%</div>
      <div class="metric-label">Win Rate</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Trades</div>
    </div>

    <h3>Short Model</h3>
    <div class="metric-box">
      <div class="metric-value %s">%.2f</div>
      <div class="metric-label">Sharpe Ratio</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%.2f</div>
      <div class="metric-label">Profit Factor</div>
    </div>
    <div class="metric-box">
      <div class="metric-value %s">%.1f%%</div>
      <div class="metric-label">Total Return</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%.1f%%</div>
      <div class="metric-label">Win Rate</div>
    </div>
    <div class="metric-box">
      <div class="metric-value">%d</div>
      <div class="metric-label">Trades</div>
    </div>
  </div>

  <h2>Baseline Comparison</h2>
  <div class="section">
    <p>Random prediction baseline (n=%d simulations):</p>
    <table>
      <tr>
        <th>Direction</th>
        <th>Baseline Mean Sharpe</th>
        <th>Model Sharpe</th>
        <th>Improvement</th>
      </tr>
      <tr>
        <td>Long</td>
        <td>%.2f</td>
        <td>%.2f</td>
        <td class="%s">%+.2f</td>
      </tr>
      <tr>
        <td>Short</td>
        <td>%.2f</td>
        <td>%.2f</td>
        <td class="%s">%+.2f</td>
      </tr>
    </table>
  </div>

  <h2>Top Feature Groups</h2>
  <div class="section">
    <h3>Long Direction - Top 10</h3>
    <table>
      <tr><th>Rank</th><th>Group</th><th>Mean Sharpe</th><th>Consistency</th><th>Profit Factor</th><th>Total Trades</th></tr>
      %s
    </table>

    <h3>Short Direction - Top 10</h3>
    <table>
      <tr><th>Rank</th><th>Group</th><th>Mean Sharpe</th><th>Consistency</th><th>Profit Factor</th><th>Total Trades</th></tr>
      %s
    </table>
  </div>

  <h2>Selected Features</h2>
  <div class="section">
    <h3>Long Model Features (%d)</h3>
    <div class="feature-list">
      %s
    </div>

    <h3>Short Model Features (%d)</h3>
    <div class="feature-list">
      %s
    </div>
  </div>

  <h2>Configuration</h2>
  <div class="section">
    <table>
      <tr><th>Parameter</th><th>Value</th></tr>
      <tr><td>Entry Threshold</td><td>%.2f</td></tr>
      <tr><td>ATR Multiplier (TP/SL)</td><td>%.1f / %.1f</td></tr>
      <tr><td>Max Bars Held</td><td>%d</td></tr>
      <tr><td>Slippage</td><td>%.4f%%</td></tr>
      <tr><td>Commission</td><td>%.4f%%</td></tr>
      <tr><td>XGBoost Max Depth (Feature Selection)</td><td>%d</td></tr>
      <tr><td>XGBoost Max Depth (Final Model)</td><td>%d</td></tr>
    </table>
  </div>

</div>
</body>
</html>',
    # Timestamp
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    # Summary metrics
    nrow(walk_forward_results$summary_long),
    nrow(walk_forward_results$windows),
    length(selected_features_long),
    length(selected_features_short),
    # Long test results
    ifelse(test_results_long$metrics$sharpe > 0, "positive", "negative"),
    test_results_long$metrics$sharpe %||% 0,
    test_results_long$metrics$profit_factor %||% 0,
    ifelse((test_results_long$metrics$total_return %||% 0) > 0, "positive", "negative"),
    (test_results_long$metrics$total_return %||% 0) * 100,
    (test_results_long$metrics$win_rate %||% 0) * 100,
    test_results_long$metrics$n_trades %||% 0,
    # Short test results
    ifelse(test_results_short$metrics$sharpe > 0, "positive", "negative"),
    test_results_short$metrics$sharpe %||% 0,
    test_results_short$metrics$profit_factor %||% 0,
    ifelse((test_results_short$metrics$total_return %||% 0) > 0, "positive", "negative"),
    (test_results_short$metrics$total_return %||% 0) * 100,
    (test_results_short$metrics$win_rate %||% 0) * 100,
    test_results_short$metrics$n_trades %||% 0,
    # Baseline comparison
    nrow(baseline_dt),
    mean(baseline_dt$long_sharpe, na.rm = TRUE),
    test_results_long$metrics$sharpe %||% 0,
    ifelse((test_results_long$metrics$sharpe %||% 0) > mean(baseline_dt$long_sharpe, na.rm = TRUE), "positive", "negative"),
    (test_results_long$metrics$sharpe %||% 0) - mean(baseline_dt$long_sharpe, na.rm = TRUE),
    mean(baseline_dt$short_sharpe, na.rm = TRUE),
    test_results_short$metrics$sharpe %||% 0,
    ifelse((test_results_short$metrics$sharpe %||% 0) > mean(baseline_dt$short_sharpe, na.rm = TRUE), "positive", "negative"),
    (test_results_short$metrics$sharpe %||% 0) - mean(baseline_dt$short_sharpe, na.rm = TRUE),
    # Top groups tables
    paste(sapply(1:min(10, nrow(walk_forward_results$summary_long)), function(i) {
      r <- walk_forward_results$summary_long[i]
      sprintf("<tr><td>%d</td><td>%s</td><td>%.2f</td><td>%.1f%%</td><td>%.2f</td><td>%d</td></tr>",
              i, r$group_name, r$mean_sharpe, r$consistency * 100, r$mean_profit_factor, r$total_trades)
    }), collapse = "\n"),
    paste(sapply(1:min(10, nrow(walk_forward_results$summary_short)), function(i) {
      r <- walk_forward_results$summary_short[i]
      sprintf("<tr><td>%d</td><td>%s</td><td>%.2f</td><td>%.1f%%</td><td>%.2f</td><td>%d</td></tr>",
              i, r$group_name, r$mean_sharpe, r$consistency * 100, r$mean_profit_factor, r$total_trades)
    }), collapse = "\n"),
    # Selected features
    length(selected_features_long),
    paste(sprintf('<div class="feature-item">%s</div>', selected_features_long), collapse = "\n"),
    length(selected_features_short),
    paste(sprintf('<div class="feature-item">%s</div>', selected_features_short), collapse = "\n"),
    # Configuration
    CONFIG$entry_threshold,
    CONFIG$atr_multiplier_tp, CONFIG$atr_multiplier_sl,
    CONFIG$max_bars_held,
    CONFIG$slippage_pct * 100,
    CONFIG$commission_pct * 100,
    CONFIG$xgb_max_depth,
    4  # Final model depth
  )

  # Write HTML file
  writeLines(html_content, output_file)
  cat(sprintf("Report saved: %s\n", output_file))

  return(invisible(output_file))
}


# ===== EXECUTE STEP 5 ========================================================

# Load walk-forward results if not in memory
if (!exists("walk_forward_results")) {
  results_file <- file.path(
    CONFIG$cache_path,
    sprintf("%s_%s_walk_forward_results.rds", CONFIG$epic, CONFIG$interval)
  )
  if (file.exists(results_file)) {
    walk_forward_results <- readRDS(results_file)
    cat(sprintf("Loaded walk-forward results from: %s\n", results_file))
  } else {
    stop("Walk-forward results not found. Please run STEP 4 first.")
  }
}

# ===== SELECT FEATURES =====
cat("\n--- FEATURE SELECTION ---\n")

# Select features for LONG model (using LONG groups)
selected_features_long <- select_features_with_correlation_cleaning(
  summary_dt = walk_forward_results$summary_long,
  feature_groups = feature_groups_long,
  dt_sample = dt_full,
  n_top_groups = 10,  # We have 10 LONG groups
  max_features = 50,
  corr_threshold = 0.85
)

cat(sprintf("\nSelected LONG features (%d):\n", length(selected_features_long)))
for (f in selected_features_long) {
  cat(sprintf("  - %s\n", f))
}

# Select features for SHORT model (using SHORT groups)
selected_features_short <- select_features_with_correlation_cleaning(
  summary_dt = walk_forward_results$summary_short,
  feature_groups = feature_groups_short,
  dt_sample = dt_full,
  n_top_groups = 10,  # We have 10 SHORT groups
  max_features = 50,
  corr_threshold = 0.85
)

cat(sprintf("\nSelected SHORT features (%d):\n", length(selected_features_short)))
for (f in selected_features_short) {
  cat(sprintf("  - %s\n", f))
}

# ===== TRAIN FINAL MODELS =====
cat("\n--- TRAINING FINAL MODELS ---\n")

# Define training period (full historical data)
train_end_date <- "2024-06-30"
val_end_date <- "2024-09-30"
test_start_date <- "2024-10-01"

# Prepare training data
dt_train_final <- dt_merged[datetime <= train_end_date]
dt_val_final <- dt_merged[datetime > train_end_date & datetime <= val_end_date]

cat(sprintf("Training data: up to %s (%s rows)\n",
            train_end_date, format(nrow(dt_train_final), big.mark = ",")))
cat(sprintf("Validation data: %s to %s (%s rows)\n",
            train_end_date, val_end_date, format(nrow(dt_val_final), big.mark = ",")))

# Train LONG model
final_model_long <- train_final_model(
  dt_train = copy(dt_train_final),
  dt_val = copy(dt_val_final),
  feature_cols = selected_features_long,
  direction = "long",
  params = list(
    max_depth = 4,
    eta = 0.1,
    nrounds = 1000,
    early_stopping_rounds = 50,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
)

# Train SHORT model
final_model_short <- train_final_model(
  dt_train = copy(dt_train_final),
  dt_val = copy(dt_val_final),
  feature_cols = selected_features_short,
  direction = "short",
  params = list(
    max_depth = 4,
    eta = 0.1,
    nrounds = 1000,
    early_stopping_rounds = 50,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
)

# ===== TEST SET EVALUATION =====
cat("\n--- FINAL TEST SET EVALUATION ---\n")

# Prepare test data (held-out period)
dt_test_prices <- dt_full[datetime >= test_start_date]

cat(sprintf("Test data: %s onwards (%s rows)\n",
            test_start_date, format(nrow(dt_test_prices), big.mark = ",")))

# Evaluate LONG model on test set
test_results_long <- evaluate_on_test_set(
  model_result = final_model_long,
  dt_test_prices = dt_test_prices,
  config = CONFIG
)

# Evaluate SHORT model on test set
test_results_short <- evaluate_on_test_set(
  model_result = final_model_short,
  dt_test_prices = dt_test_prices,
  config = CONFIG
)

# ===== BASELINE COMPARISON =====
baseline_dt <- generate_baseline_comparison(
  dt_test_prices = dt_test_prices,
  n_simulations = 100,
  config = CONFIG
)

# Compare model vs baseline with p-values
cat("\n=== MODEL VS BASELINE (STATISTICAL SIGNIFICANCE) ===\n")

# Helper to replace NA with default value (unlike %||% which only handles NULL)
na_replace <- function(x, default = 0) ifelse(is.na(x), default, x)

long_sharpe <- na_replace(test_results_long$metrics$sharpe, 0)
short_sharpe <- na_replace(test_results_short$metrics$sharpe, 0)
baseline_long <- mean(baseline_dt$long_sharpe, na.rm = TRUE)
baseline_short <- mean(baseline_dt$short_sharpe, na.rm = TRUE)

# Calculate p-values
long_pvalue <- calculate_permutation_pvalue(long_sharpe, baseline_dt$long_sharpe)
short_pvalue <- calculate_permutation_pvalue(short_sharpe, baseline_dt$short_sharpe)

long_improvement <- long_sharpe - baseline_long
short_improvement <- short_sharpe - baseline_short

# Significance interpretation
interpret_pvalue <- function(p) {
  if (is.na(p)) return("N/A")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  return("n.s.")
}

cat(sprintf("LONG  Sharpe: %.2f (Baseline: %.2f, Improvement: %+.2f) | p-value: %.3f %s\n",
            long_sharpe, baseline_long, long_improvement, long_pvalue, interpret_pvalue(long_pvalue)))
cat(sprintf("SHORT Sharpe: %.2f (Baseline: %.2f, Improvement: %+.2f) | p-value: %.3f %s\n",
            short_sharpe, baseline_short, short_improvement, short_pvalue, interpret_pvalue(short_pvalue)))

cat("\nSignificance: *** p<0.01, ** p<0.05, * p<0.10, n.s. = not significant\n")

# Warning if not significant
if (!is.na(long_pvalue) && long_pvalue >= 0.05) {
  cat("WARNING: LONG model not significantly better than random (p >= 0.05)\n")
}
if (!is.na(short_pvalue) && short_pvalue >= 0.05) {
  cat("WARNING: SHORT model not significantly better than random (p >= 0.05)\n")
}

# ===== SAVE FINAL MODELS =====
cat("\n--- SAVING FINAL MODELS ---\n")

# Create model output directory
model_output_dir <- file.path("backtest_results", "models")
dir.create(model_output_dir, recursive = TRUE, showWarnings = FALSE)

# Save LONG model
long_model_file <- file.path(
  model_output_dir,
  sprintf("%s_%s_pnl_model_long.rds", CONFIG$epic, CONFIG$interval)
)
saveRDS(list(
  model = final_model_long$model,
  feature_cols = selected_features_long,
  direction = "long",
  auc_val = final_model_long$auc_val,
  test_metrics = test_results_long$metrics,
  timestamp = Sys.time()
), long_model_file)
cat(sprintf("Long model saved: %s\n", long_model_file))

# Save XGBoost binary for production
xgb.save(final_model_long$model,
         file.path(model_output_dir,
                   sprintf("%s_%s_pnl_model_long.xgb", CONFIG$epic, CONFIG$interval)))

# Save SHORT model
short_model_file <- file.path(
  model_output_dir,
  sprintf("%s_%s_pnl_model_short.rds", CONFIG$epic, CONFIG$interval)
)
saveRDS(list(
  model = final_model_short$model,
  feature_cols = selected_features_short,
  direction = "short",
  auc_val = final_model_short$auc_val,
  test_metrics = test_results_short$metrics,
  timestamp = Sys.time()
), short_model_file)
cat(sprintf("Short model saved: %s\n", short_model_file))

# Save XGBoost binary for production
xgb.save(final_model_short$model,
         file.path(model_output_dir,
                   sprintf("%s_%s_pnl_model_short.xgb", CONFIG$epic, CONFIG$interval)))

# Save feature lists
fwrite(
  data.table(feature = selected_features_long, direction = "long"),
  file.path(model_output_dir,
            sprintf("%s_%s_pnl_selected_features_long.csv", CONFIG$epic, CONFIG$interval))
)
fwrite(
  data.table(feature = selected_features_short, direction = "short"),
  file.path(model_output_dir,
            sprintf("%s_%s_pnl_selected_features_short.csv", CONFIG$epic, CONFIG$interval))
)

# ===== GENERATE HTML REPORT =====
report_file <- file.path(
  "backtest_results",
  sprintf("%s_%s_pnl_feature_selection_report.html", CONFIG$epic, CONFIG$interval)
)

generate_html_report(
  walk_forward_results = walk_forward_results,
  final_model_long = final_model_long,
  final_model_short = final_model_short,
  test_results_long = test_results_long,
  test_results_short = test_results_short,
  baseline_dt = baseline_dt,
  selected_features_long = selected_features_long,
  selected_features_short = selected_features_short,
  output_file = report_file
)

# ===== FINAL SUMMARY =====
cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("STEP 5 COMPLETE - FINAL MODELS TRAINED AND EVALUATED\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n\n")

cat("=== PIPELINE SUMMARY ===\n")
cat(sprintf("Feature Groups Evaluated: %d\n", nrow(walk_forward_results$summary_long)))
cat(sprintf("Walk-Forward Windows: %d\n", nrow(walk_forward_results$windows)))
cat(sprintf("Selected Features (Long): %d\n", length(selected_features_long)))
cat(sprintf("Selected Features (Short): %d\n", length(selected_features_short)))
cat(sprintf("\nFinal Test Results:\n"))
cat(sprintf("  LONG  - Sharpe: %.2f | PF: %.2f | Return: %.1f%% | Trades: %d\n",
            na_replace(test_results_long$metrics$sharpe, 0),
            na_replace(test_results_long$metrics$profit_factor, 0),
            na_replace(test_results_long$metrics$total_return, 0) * 100,
            na_replace(test_results_long$metrics$n_trades, 0)))
cat(sprintf("  SHORT - Sharpe: %.2f | PF: %.2f | Return: %.1f%% | Trades: %d\n",
            na_replace(test_results_short$metrics$sharpe, 0),
            na_replace(test_results_short$metrics$profit_factor, 0),
            na_replace(test_results_short$metrics$total_return, 0) * 100,
            na_replace(test_results_short$metrics$n_trades, 0)))

cat(sprintf("\nOutput Files:\n"))
cat(sprintf("  - %s\n", long_model_file))
cat(sprintf("  - %s\n", short_model_file))
cat(sprintf("  - %s\n", report_file))

# Save final workspace
save.image(file.path(CONFIG$cache_path, "step5_complete.RData"))
cat(sprintf("\nFinal workspace saved: %s\n", file.path(CONFIG$cache_path, "step5_complete.RData")))

log_message("PnL-based Feature Selection Pipeline COMPLETE")

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("PIPELINE COMPLETE\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
toc()
