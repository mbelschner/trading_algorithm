# =============================================================================
# STATISTICAL VALIDATION FOR LABELS
# =============================================================================
#
# Validates label quality through statistical tests:
# - T-Statistic: Is the expected return significantly different from zero?
# - Correlation with Fixed-Horizon Returns: Do TBM labels add value?
# - Class Balance: Are labels reasonably balanced?
# - Information Ratio: Risk-adjusted return per label class
#
# =============================================================================

library(data.table)

# =============================================================================
# MAIN VALIDATION FUNCTION
# =============================================================================

#' Validate label quality with statistical tests
#'
#' @param dt data.table with labels and realized returns
#' @param min_tstat Minimum T-statistic required
#' @param min_samples Minimum samples per class
#' @return List with validation results
validate_label_quality <- function(
    dt,
    min_tstat = 2.0,
    min_samples = 100
) {

  dt <- copy(dt)

  cat("\n=== STATISTICAL VALIDATION ===\n")

  results <- list()

  # =========================================================================
  # 1. T-STATISTIC FOR REALIZED RETURNS
  # =========================================================================
  # Tests if the mean realized return is significantly different from zero

  cat("\n--- 1. T-Statistic Test ---\n")

  # Use adjusted returns (after costs)
  returns <- dt$realized_return_adj
  returns <- returns[!is.na(returns)]

  n <- length(returns)
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  se_ret <- sd_ret / sqrt(n)

  tstat <- mean_ret / se_ret
  pvalue <- 2 * pt(-abs(tstat), df = n - 1)

  results$tstat <- tstat
  results$pvalue <- pvalue
  results$mean_return <- mean_ret
  results$sd_return <- sd_ret
  results$n_samples <- n

  cat(sprintf("N samples: %d\n", n))
  cat(sprintf("Mean return: %.4f%% (%.4f%% annualized)\n",
              mean_ret * 100,
              mean_ret * 100 * 252 * 26))  # 15-min bars
  cat(sprintf("Std dev: %.4f%%\n", sd_ret * 100))
  cat(sprintf("T-Statistic: %.2f\n", tstat))
  cat(sprintf("P-value: %.4f\n", pvalue))

  if (tstat >= min_tstat) {
    cat(sprintf("PASS: T-stat >= %.1f\n", min_tstat))
  } else {
    cat(sprintf("FAIL: T-stat < %.1f\n", min_tstat))
  }

  # =========================================================================
  # 2. EXPECTED RETURNS BY META-LABEL
  # =========================================================================

  cat("\n--- 2. Expected Returns by Meta-Label ---\n")

  if ("meta_label" %in% names(dt)) {
    ret_by_label <- dt[!is.na(meta_label), .(
      n = .N,
      mean_return = mean(realized_return_adj, na.rm = TRUE),
      sd_return = sd(realized_return_adj, na.rm = TRUE),
      win_rate = mean(realized_return_adj > 0, na.rm = TRUE),
      sum_weight = sum(sample_weight, na.rm = TRUE)
    ), by = meta_label]

    print(ret_by_label)

    results$expected_return_tp <- ret_by_label[meta_label == 1, mean_return]
    results$expected_return_sl <- ret_by_label[meta_label == 0, mean_return]
  }

  # By primary signal (Long/Short)
  if ("primary_signal" %in% names(dt)) {
    ret_by_signal <- dt[!is.na(primary_signal), .(
      n = .N,
      mean_return = mean(realized_return_adj, na.rm = TRUE),
      sd_return = sd(realized_return_adj, na.rm = TRUE),
      win_rate = mean(realized_return_adj > 0, na.rm = TRUE)
    ), by = primary_signal]

    cat("\nBy Primary Signal:\n")
    print(ret_by_signal)

    results$expected_return_long <- ret_by_signal[primary_signal == 1, mean_return]
    results$expected_return_short <- ret_by_signal[primary_signal == -1, mean_return]
  }

  # =========================================================================
  # 3. CORRELATION WITH FIXED-HORIZON RETURNS
  # =========================================================================

  cat("\n--- 3. Correlation with Fixed-Horizon Returns ---\n")

  # Calculate fixed-horizon returns for comparison
  horizons <- c(4, 8, 16)  # 1h, 2h, 4h

  for (h in horizons) {
    # Shift close prices
    dt[, paste0("fh_return_", h) := shift(close, -h, type = "lead") / close - 1]
  }

  # Correlation between TBM label and fixed-horizon returns
  correlations <- sapply(horizons, function(h) {
    fh_col <- paste0("fh_return_", h)

    # Create numeric label: 1 for TP, -1 for SL
    if ("meta_label" %in% names(dt)) {
      numeric_label <- ifelse(dt$meta_label == 1, 1, -1)
    } else {
      numeric_label <- dt$label
    }

    cor(numeric_label, dt[[fh_col]], use = "complete.obs")
  })

  names(correlations) <- paste0("horizon_", horizons)

  results$fixed_horizon_correlations <- correlations
  results$fixed_horizon_correlation <- mean(correlations)

  cat("Correlation with Fixed-Horizon Returns:\n")
  for (i in seq_along(horizons)) {
    h <- horizons[i]
    cat(sprintf("  %d bars (%.0f min): %.3f\n", h, h * 15, correlations[i]))
  }
  cat(sprintf("  Average: %.3f\n", mean(correlations)))

  # Interpretation
  if (mean(correlations) > 0.3) {
    cat("  -> Strong relationship with forward returns (good signal)\n")
  } else if (mean(correlations) > 0.1) {
    cat("  -> Moderate relationship (acceptable)\n")
  } else {
    cat("  -> Weak relationship (consider adjusting barriers)\n")
  }

  # =========================================================================
  # 4. CLASS BALANCE CHECK
  # =========================================================================

  cat("\n--- 4. Class Balance ---\n")

  if ("meta_label" %in% names(dt)) {
    label_counts <- table(dt$meta_label)
    total <- sum(label_counts)

    results$class_balance <- as.list(label_counts / total)

    cat("Meta-Label Distribution:\n")
    for (lbl in names(label_counts)) {
      pct <- label_counts[lbl] / total * 100
      cat(sprintf("  Label %s: %d (%.1f%%)\n", lbl, label_counts[lbl], pct))
    }

    # Check if any class has too few samples
    if (any(label_counts < min_samples)) {
      cat(sprintf("WARNING: Some classes have < %d samples\n", min_samples))
      results$class_balance_ok <- FALSE
    } else {
      results$class_balance_ok <- TRUE
    }

    # Check imbalance ratio
    imbalance_ratio <- max(label_counts) / min(label_counts)
    results$imbalance_ratio <- imbalance_ratio

    if (imbalance_ratio > 3) {
      cat(sprintf("WARNING: High class imbalance (ratio: %.1f)\n", imbalance_ratio))
    }
  }

  # =========================================================================
  # 5. INFORMATION RATIO
  # =========================================================================

  cat("\n--- 5. Information Ratio ---\n")

  # IR = Mean Return / Std Dev (annualized)
  # For trading: IR = (Win Rate * Avg Win - Loss Rate * Avg Loss) / Std Dev

  wins <- dt[realized_return_adj > 0]
  losses <- dt[realized_return_adj <= 0]

  win_rate <- nrow(wins) / nrow(dt)
  loss_rate <- 1 - win_rate
  avg_win <- mean(wins$realized_return_adj)
  avg_loss <- mean(losses$realized_return_adj)

  expectancy <- win_rate * avg_win + loss_rate * avg_loss
  ir <- expectancy / sd_ret

  # Annualize (assume ~26 trades per day on 15-min bars with signals)
  trades_per_year <- 252 * 10  # Rough estimate
  ir_annual <- ir * sqrt(trades_per_year)

  results$information_ratio <- ir
  results$information_ratio_annual <- ir_annual
  results$win_rate <- win_rate
  results$avg_win <- avg_win
  results$avg_loss <- avg_loss
  results$expectancy <- expectancy

  cat(sprintf("Win Rate: %.1f%%\n", win_rate * 100))
  cat(sprintf("Avg Win: %.4f%%\n", avg_win * 100))
  cat(sprintf("Avg Loss: %.4f%%\n", avg_loss * 100))
  cat(sprintf("Expectancy: %.4f%%\n", expectancy * 100))
  cat(sprintf("Information Ratio: %.3f (annualized: %.3f)\n", ir, ir_annual))

  # =========================================================================
  # 6. OVERALL VALIDATION RESULT
  # =========================================================================

  results$is_valid <- (
    tstat >= min_tstat &&
    results$class_balance_ok %||% TRUE &&
    min(label_counts) >= min_samples
  )

  cat("\n--- OVERALL VALIDATION ---\n")
  if (results$is_valid) {
    cat("PASS: Label set meets all validation criteria\n")
  } else {
    cat("FAIL: Label set does not meet validation criteria\n")
    cat("Reasons:\n")
    if (tstat < min_tstat) cat(sprintf("  - T-stat %.2f < %.1f\n", tstat, min_tstat))
    if (!(results$class_balance_ok %||% TRUE)) cat("  - Class imbalance\n")
    if (min(label_counts) < min_samples) cat(sprintf("  - Insufficient samples in some classes\n"))
  }

  return(results)
}


# =============================================================================
# PARAMETER GRID TESTING
# =============================================================================

#' Test multiple parameter combinations and find optimal configuration
#'
#' @param dt_prices Price data with indicators
#' @param param_grid List of parameter vectors to test
#' @param primary_signal_col Name of primary signal column
#' @param n_cores Number of cores for parallel processing
#' @return data.table with results for each parameter combination
test_parameter_grid <- function(
    dt_prices,
    param_grid = list(
      atr_mult_tp = c(2.0, 2.5, 3.0),
      atr_mult_sl = c(1.5, 2.0, 2.5),
      max_horizon = c(12, 16, 20, 24),
      neutral_threshold = c(1.0, 1.5, 2.0)
    ),
    primary_signal_col = "primary_signal",
    n_cores = 1
) {

  cat("\n=== PARAMETER GRID TESTING ===\n")

  # Create all combinations
  grid <- expand.grid(param_grid)
  n_combos <- nrow(grid)

  cat(sprintf("Testing %d parameter combinations...\n\n", n_combos))

  results <- data.table()

  pb <- progress_bar$new(
    format = "[:bar] :current/:total :percent eta: :eta",
    total = n_combos,
    clear = FALSE
  )

  for (i in 1:n_combos) {
    pb$tick()

    params <- as.list(grid[i, ])

    # Apply labeling with these parameters
    tryCatch({
      dt_labeled <- suppressMessages(apply_dynamic_triple_barrier(
        dt_prices,
        atr_mult_tp = params$atr_mult_tp,
        atr_mult_sl = params$atr_mult_sl,
        max_horizon = params$max_horizon,
        neutral_threshold = params$neutral_threshold
      ))

      # Calculate sample weights
      dt_weighted <- suppressMessages(calculate_sample_uniqueness(
        dt_labeled,
        method = "uniqueness"
      ))

      # Validate
      validation <- suppressMessages(validate_label_quality(
        dt_weighted,
        min_tstat = 2.0,
        min_samples = 50
      ))

      # Record results
      results <- rbind(results, data.table(
        atr_mult_tp = params$atr_mult_tp,
        atr_mult_sl = params$atr_mult_sl,
        max_horizon = params$max_horizon,
        neutral_threshold = params$neutral_threshold,
        n_samples = nrow(dt_weighted),
        effective_n = sum(dt_weighted$sample_weight),
        tstat = validation$tstat,
        mean_return = validation$mean_return * 100,
        win_rate = validation$win_rate * 100,
        information_ratio = validation$information_ratio,
        fh_correlation = validation$fixed_horizon_correlation,
        is_valid = validation$is_valid
      ))

    }, error = function(e) {
      cat(sprintf("\nError with params: %s\n", paste(params, collapse = ", ")))
      cat(sprintf("  %s\n", e$message))
    })
  }

  # Sort by Information Ratio
  setorder(results, -information_ratio)

  cat("\n=== TOP 10 PARAMETER COMBINATIONS ===\n")
  print(head(results, 10))

  # Find best valid configuration
  best_valid <- results[is_valid == TRUE][1]

  if (nrow(best_valid) > 0) {
    cat("\n=== BEST VALID CONFIGURATION ===\n")
    cat(sprintf("ATR_mult_TP: %.1f\n", best_valid$atr_mult_tp))
    cat(sprintf("ATR_mult_SL: %.1f\n", best_valid$atr_mult_sl))
    cat(sprintf("Max Horizon: %d\n", best_valid$max_horizon))
    cat(sprintf("Neutral Threshold: %.1f\n", best_valid$neutral_threshold))
    cat(sprintf("Information Ratio: %.4f\n", best_valid$information_ratio))
    cat(sprintf("T-Statistic: %.2f\n", best_valid$tstat))
  }

  return(results)
}


# =============================================================================
# BOOTSTRAP SIGNIFICANCE TEST
# =============================================================================

#' Bootstrap test for return significance
#'
#' @param returns Vector of returns
#' @param n_bootstrap Number of bootstrap iterations
#' @param confidence Confidence level (e.g., 0.95)
#' @return List with bootstrap results
bootstrap_significance_test <- function(
    returns,
    n_bootstrap = 1000,
    confidence = 0.95
) {

  returns <- returns[!is.na(returns)]
  n <- length(returns)

  observed_mean <- mean(returns)

  # Bootstrap distribution of means
  boot_means <- replicate(n_bootstrap, {
    sample_returns <- sample(returns, n, replace = TRUE)
    mean(sample_returns)
  })

  # Confidence interval
  alpha <- 1 - confidence
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)

  # P-value (proportion of bootstrap means <= 0)
  p_value <- mean(boot_means <= 0)

  # Significant if CI doesn't include 0
  is_significant <- ci_lower > 0 || ci_upper < 0

  return(list(
    observed_mean = observed_mean,
    boot_mean = mean(boot_means),
    boot_sd = sd(boot_means),
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    is_significant = is_significant,
    confidence = confidence
  ))
}


cat("\n=== STATISTICAL VALIDATION MODULE LOADED ===\n")
cat("Functions:\n")
cat("  - validate_label_quality(dt)\n")
cat("  - test_parameter_grid(dt_prices, param_grid)\n")
cat("  - bootstrap_significance_test(returns)\n\n")
