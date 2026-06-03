# =============================================================================
# SAMPLE UNIQUENESS & SEQUENTIAL BOOTSTRAPPING
# =============================================================================
#
# Implements Lopez de Prado's sample weighting methods:
# - Sample Uniqueness: Weight = 1 / n_concurrent
# - Sequential Bootstrapping: Draw samples respecting temporal overlap
# - Decay Weights: Time-based decay for older samples
#
# KEY CONCEPT:
# Instead of hard-filtering overlapping labels (losing data),
# we WEIGHT them by their uniqueness/independence.
#
# =============================================================================

library(data.table)

# =============================================================================
# SAMPLE UNIQUENESS CALCULATION
# =============================================================================

#' Calculate sample uniqueness weights
#'
#' For each label, calculate how many other labels overlap with it.
#' Weight = 1 / n_concurrent (samples with more overlap get lower weight)
#'
#' @param dt data.table with datetime and bars_to_exit columns
#' @param method Weighting method: "uniqueness", "sequential_bootstrap", "decay"
#' @param min_weight Minimum weight (samples below this are excluded)
#' @return data.table with sample_weight and n_concurrent columns
calculate_sample_uniqueness <- function(
    dt,
    method = "uniqueness",
    min_weight = 0.1
) {

  dt <- copy(dt)
  n <- nrow(dt)

  cat(sprintf("Calculating sample uniqueness for %s observations...\n",
              format(n, big.mark = ",")))

  # Calculate entry and exit times
  dt[, entry_time := datetime]
  dt[, exit_time := datetime + bars_to_exit * 15 * 60]  # 15-min bars
  dt[, idx := .I]

  if (method == "uniqueness") {
    # =========================================================================
    # METHOD 1: Sample Uniqueness (Lopez de Prado)
    # =========================================================================
    # For each sample, count how many other samples overlap with it.
    # Weight = 1 / n_concurrent
    #
    # Optimized O(n log n) algorithm using event counting

    # Create events: +1 for entry, -1 for exit
    events <- rbindlist(list(
      dt[, .(time = entry_time, type = 1L, idx = idx)],
      dt[, .(time = exit_time, type = -1L, idx = idx)]
    ))

    # Sort events (entries before exits at same time)
    setorder(events, time, -type)

    # Count concurrent samples at each entry
    active_count <- 0L
    concurrent_at_entry <- integer(n)

    for (i in 1:nrow(events)) {
      if (events$type[i] == 1L) {
        active_count <- active_count + 1L
        concurrent_at_entry[events$idx[i]] <- active_count
      } else {
        active_count <- active_count - 1L
      }
    }

    dt[, n_concurrent := concurrent_at_entry[idx]]
    dt[, sample_weight := 1.0 / n_concurrent]

  } else if (method == "sequential_bootstrap") {
    # =========================================================================
    # METHOD 2: Sequential Bootstrapping Weights
    # =========================================================================
    # Assign weights based on average uniqueness over the sample's lifetime

    # Build indicator matrix (which samples are active at each time point)
    # For efficiency, we sample time points rather than using all

    time_points <- sort(unique(c(dt$entry_time, dt$exit_time)))
    n_times <- length(time_points)

    # For very large datasets, sample time points
    if (n_times > 10000) {
      sample_idx <- sort(sample(1:n_times, 10000))
      time_points <- time_points[sample_idx]
      n_times <- length(time_points)
    }

    # Calculate average uniqueness for each sample
    avg_uniqueness <- numeric(n)

    pb <- progress_bar$new(
      format = "  [:bar] :percent",
      total = n,
      clear = FALSE
    )

    for (i in 1:n) {
      pb$tick()

      # Time points where this sample is active
      active_times <- time_points[time_points >= dt$entry_time[i] &
                                  time_points <= dt$exit_time[i]]

      if (length(active_times) == 0) {
        avg_uniqueness[i] <- 1
        next
      }

      # For each active time, count how many samples are concurrent
      uniqueness_sum <- 0
      for (t in active_times) {
        n_active <- sum(dt$entry_time <= t & dt$exit_time >= t)
        uniqueness_sum <- uniqueness_sum + 1 / n_active
      }

      avg_uniqueness[i] <- uniqueness_sum / length(active_times)
    }

    dt[, sample_weight := avg_uniqueness]
    dt[, n_concurrent := round(1 / sample_weight)]

  } else if (method == "decay") {
    # =========================================================================
    # METHOD 3: Time Decay Weights
    # =========================================================================
    # More recent samples get higher weight

    # Simple exponential decay based on sample age
    dt[, days_ago := as.numeric(difftime(max(datetime), datetime, units = "days"))]
    decay_rate <- 0.01  # 1% decay per day

    dt[, sample_weight := exp(-decay_rate * days_ago)]
    dt[, n_concurrent := 1L]  # Not applicable for this method

    dt[, days_ago := NULL]

  } else {
    stop(sprintf("Unknown method: %s. Use 'uniqueness', 'sequential_bootstrap', or 'decay'", method))
  }

  # Apply minimum weight threshold
  n_before <- nrow(dt)
  dt <- dt[sample_weight >= min_weight]
  n_after <- nrow(dt)

  if (n_after < n_before) {
    cat(sprintf("Removed %d samples with weight < %.2f (%.1f%%)\n",
                n_before - n_after, min_weight,
                (n_before - n_after) / n_before * 100))
  }

  # Normalize weights to sum to effective sample size
  # (optional - keeps weights interpretable)
  # dt[, sample_weight := sample_weight / sum(sample_weight) * nrow(dt)]

  # Cleanup
  dt[, c("entry_time", "exit_time", "idx") := NULL]

  # Statistics
  cat(sprintf("\nSample Uniqueness Statistics:\n"))
  cat(sprintf("  Method: %s\n", method))
  cat(sprintf("  Mean weight: %.4f\n", mean(dt$sample_weight)))
  cat(sprintf("  Median weight: %.4f\n", median(dt$sample_weight)))
  cat(sprintf("  Min weight: %.4f\n", min(dt$sample_weight)))
  cat(sprintf("  Max weight: %.4f\n", max(dt$sample_weight)))
  cat(sprintf("  Mean concurrent: %.2f\n", mean(dt$n_concurrent)))
  cat(sprintf("  Max concurrent: %d\n", max(dt$n_concurrent)))
  cat(sprintf("  Effective sample size: %.0f (%.1f%% of original)\n",
              sum(dt$sample_weight),
              sum(dt$sample_weight) / nrow(dt) * 100))

  return(dt)
}


# =============================================================================
# SEQUENTIAL BOOTSTRAPPING
# =============================================================================

#' Perform sequential bootstrapping
#'
#' Draw samples one at a time, with probability proportional to uniqueness
#' given the samples already drawn.
#'
#' @param dt data.table with entry_time, exit_time columns
#' @param n_samples Number of samples to draw
#' @param seed Random seed for reproducibility
#' @return Integer vector of selected row indices
sequential_bootstrap <- function(dt, n_samples = NULL, seed = 42) {

  set.seed(seed)
  dt <- copy(dt)
  n <- nrow(dt)

  if (is.null(n_samples)) {
    n_samples <- n  # Draw same number as original
  }

  # Calculate entry and exit times
  if (!"entry_time" %in% names(dt)) {
    dt[, entry_time := datetime]
    dt[, exit_time := datetime + bars_to_exit * 15 * 60]
  }

  # Build indicator matrix (sparse representation)
  # For each time point, which samples are active?
  all_times <- sort(unique(c(dt$entry_time, dt$exit_time)))
  n_times <- length(all_times)

  # Sample time points if too many
  if (n_times > 5000) {
    time_idx <- sort(sample(1:n_times, 5000))
    all_times <- all_times[time_idx]
    n_times <- length(all_times)
  }

  # Create indicator matrix: indicator[t, i] = 1 if sample i active at time t
  cat("Building indicator matrix...\n")
  indicator <- matrix(0L, nrow = n_times, ncol = n)

  for (i in 1:n) {
    active <- all_times >= dt$entry_time[i] & all_times <= dt$exit_time[i]
    indicator[active, i] <- 1L
  }

  # Sequential bootstrap
  cat(sprintf("Drawing %d samples sequentially...\n", n_samples))

  selected <- integer(n_samples)
  phi <- rep(0, n_times)  # Running sum of selected indicators

  pb <- progress_bar$new(
    format = "  [:bar] :percent",
    total = n_samples,
    clear = FALSE
  )

  for (j in 1:n_samples) {
    pb$tick()

    # Calculate average uniqueness for each candidate sample
    # given the samples already selected
    avg_u <- numeric(n)

    for (i in 1:n) {
      active_times <- which(indicator[, i] == 1)
      if (length(active_times) == 0) {
        avg_u[i] <- 0
      } else {
        # Uniqueness at each time = 1 / (phi + 1) where phi is concurrent selected samples
        uniqueness <- 1 / (phi[active_times] + 1)
        avg_u[i] <- mean(uniqueness)
      }
    }

    # Normalize to probabilities
    prob <- avg_u / sum(avg_u)

    # Draw sample
    selected[j] <- sample(1:n, 1, prob = prob)

    # Update phi
    phi <- phi + indicator[, selected[j]]
  }

  cat(sprintf("\nUnique samples drawn: %d (%.1f%%)\n",
              length(unique(selected)),
              length(unique(selected)) / n_samples * 100))

  return(selected)
}


# =============================================================================
# WEIGHTED RESAMPLING FOR TRAINING
# =============================================================================

#' Get weighted training sample
#'
#' Resample training data according to sample weights.
#' Useful for models that don't natively support sample weights.
#'
#' @param dt data.table with sample_weight column
#' @param n_samples Number of samples to draw (default = nrow(dt))
#' @param replace Sample with replacement?
#' @param seed Random seed
#' @return data.table of resampled data
get_weighted_sample <- function(dt, n_samples = NULL, replace = TRUE, seed = 42) {

  set.seed(seed)

  if (is.null(n_samples)) {
    n_samples <- nrow(dt)
  }

  # Normalize weights to probabilities
  probs <- dt$sample_weight / sum(dt$sample_weight)

  # Draw sample indices
  idx <- sample(1:nrow(dt), size = n_samples, replace = replace, prob = probs)

  # Return resampled data
  return(dt[idx])
}


# =============================================================================
# PURGED K-FOLD CROSS-VALIDATION
# =============================================================================

#' Create purged K-fold CV splits
#'
#' Creates train/test splits that respect temporal overlap.
#' Observations in the test set are "purged" from training if they overlap.
#'
#' @param dt data.table with datetime and bars_to_exit
#' @param n_folds Number of folds
#' @param embargo_bars Number of bars to embargo after test set
#' @return List of train/test index pairs
create_purged_kfold <- function(dt, n_folds = 5, embargo_bars = 5) {

  dt <- copy(dt)
  n <- nrow(dt)

  # Calculate entry/exit times
  dt[, entry_time := datetime]
  dt[, exit_time := datetime + bars_to_exit * 15 * 60]
  dt[, idx := .I]

  # Sort by time
  setorder(dt, datetime)

  # Create fold indices
  fold_size <- ceiling(n / n_folds)

  folds <- list()

  for (k in 1:n_folds) {
    # Test set: k-th fold
    test_start <- (k - 1) * fold_size + 1
    test_end <- min(k * fold_size, n)
    test_idx <- test_start:test_end

    # Get test period times
    test_entry_min <- min(dt$entry_time[test_idx])
    test_exit_max <- max(dt$exit_time[test_idx])

    # Add embargo
    embargo_time <- test_exit_max + embargo_bars * 15 * 60

    # Training set: all samples not overlapping with test + embargo
    train_idx <- which(
      dt$exit_time < test_entry_min |  # Ends before test starts
      dt$entry_time > embargo_time      # Starts after embargo
    )

    folds[[k]] <- list(
      train = dt$idx[train_idx],
      test = dt$idx[test_idx],
      n_train = length(train_idx),
      n_test = length(test_idx),
      n_purged = n - length(train_idx) - length(test_idx)
    )

    cat(sprintf("Fold %d: Train=%d, Test=%d, Purged=%d\n",
                k, folds[[k]]$n_train, folds[[k]]$n_test, folds[[k]]$n_purged))
  }

  return(folds)
}


cat("\n=== SAMPLE UNIQUENESS MODULE LOADED ===\n")
cat("Functions:\n")
cat("  - calculate_sample_uniqueness(dt, method)\n")
cat("  - sequential_bootstrap(dt, n_samples)\n")
cat("  - get_weighted_sample(dt, n_samples)\n")
cat("  - create_purged_kfold(dt, n_folds)\n\n")
