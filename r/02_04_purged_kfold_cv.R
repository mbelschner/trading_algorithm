# Purged K-Fold Cross-Validation Module
# Implementiert nach "Advances in Financial Machine Learning" von Lopez de Prado
#
# Key Features:
# 1. Purging: Entfernt überlappende Labels zwischen Train und Test
# 2. Embargo: Fügt zeitliche Barriere nach Test Set hinzu
# 3. Respektiert zeitliche Struktur der Daten

#' Create purged k-fold cross-validation splits
#'
#' @param dt data.table with datetime and t1 (label end time) columns
#' @param n_splits Number of CV folds (default: 5)
#' @param pct_embargo Percentage of samples to embargo after test set (default: 0.01 = 1%)
#' @param verbose Print diagnostic information
#'
#' @return List of CV splits, each containing train_idx and test_idx
create_purged_kfold_splits <- function(
    dt,
    n_splits = 5,
    pct_embargo = 0.01,
    verbose = TRUE
) {

  if (verbose) cat("\n=== Purged K-Fold CV Setup ===\n")

  # Prüfe erforderliche Spalten
  if (!"datetime" %in% names(dt)) {
    stop("Column 'datetime' not found in data")
  }

  if (!"t1" %in% names(dt)) {
    stop("Column 't1' (label end time) not found in data")
  }

  # Konvertiere datetime zu POSIXct falls nötig
  if (!inherits(dt$datetime, "POSIXct")) {
    dt[, datetime := as.POSIXct(datetime)]
  }

  if (!inherits(dt$t1, "POSIXct")) {
    dt[, t1 := as.POSIXct(t1)]
  }

  n_samples <- nrow(dt)

  if (verbose) {
    cat(sprintf("Samples: %s\n", format(n_samples, big.mark = ",")))
    cat(sprintf("Splits: %d\n", n_splits))
    cat(sprintf("Embargo: %.2f%%\n", pct_embargo * 100))
  }

  # ===== Erstelle Sequential K-Fold Splits =====
  # Teile Datensatz in n_splits Segmente
  fold_size <- floor(n_samples / n_splits)

  # Erstelle Fold-Indizes
  fold_indices <- list()
  for (k in 1:n_splits) {
    start_idx <- (k - 1) * fold_size + 1
    end_idx <- ifelse(k == n_splits, n_samples, k * fold_size)
    fold_indices[[k]] <- start_idx:end_idx
  }

  # ===== Erstelle Purged Splits =====
  cv_splits <- list()

  for (k in 1:n_splits) {
    if (verbose) cat(sprintf("\nFold %d/%d:\n", k, n_splits))

    # Test Set: Fold k
    test_idx <- fold_indices[[k]]

    # Train Set: Alle anderen Folds
    train_idx <- setdiff(1:n_samples, test_idx)

    # ===== PURGING: Entferne überlappende Labels =====
    # Test Period
    test_start_time <- dt$datetime[min(test_idx)]
    test_end_time <- dt$datetime[max(test_idx)]

    if (verbose) {
      cat(sprintf("  Test Period: %s bis %s\n", test_start_time, test_end_time))
      cat(sprintf("  Test Samples (vor Purge): %d\n", length(test_idx)))
      cat(sprintf("  Train Samples (vor Purge): %d\n", length(train_idx)))
    }

    # Purge Train Set:
    # Entferne alle Train Samples deren Label (t1) in Test Period überlappt
    train_purged_idx <- train_idx[
      dt$t1[train_idx] < test_start_time |  # Label endet vor Test
      dt$datetime[train_idx] > test_end_time  # Label startet nach Test
    ]

    n_purged <- length(train_idx) - length(train_purged_idx)

    if (verbose) {
      cat(sprintf("  Train Samples (nach Purge): %d (-%d)\n",
                  length(train_purged_idx), n_purged))
    }

    # ===== EMBARGO: Zeitliche Barriere nach Test Set =====
    if (pct_embargo > 0) {
      # Berechne Embargo Size
      embargo_size <- ceiling(n_samples * pct_embargo)

      # Finde letzten Test Index (in Zeit)
      last_test_time <- max(dt$datetime[test_idx])

      # Finde alle Indizes innerhalb Embargo Period nach Test
      all_times <- dt$datetime
      embargo_mask <- all_times > last_test_time &
                      all_times <= all_times[min(which(all_times > last_test_time)) + embargo_size - 1]

      # Entferne Embargo Samples aus Train Set
      embargo_idx <- which(embargo_mask)
      train_final_idx <- setdiff(train_purged_idx, embargo_idx)

      n_embargoed <- length(train_purged_idx) - length(train_final_idx)

      if (verbose) {
        cat(sprintf("  Embargo Samples: %d\n", n_embargoed))
        cat(sprintf("  Train Samples (final): %d\n", length(train_final_idx)))
      }

    } else {
      train_final_idx <- train_purged_idx
    }

    # ===== Speichere Split =====
    cv_splits[[k]] <- list(
      fold = k,
      train_idx = train_final_idx,
      test_idx = test_idx,
      test_start_time = test_start_time,
      test_end_time = test_end_time,
      n_train = length(train_final_idx),
      n_test = length(test_idx),
      n_purged = n_purged,
      n_embargoed = ifelse(pct_embargo > 0, n_embargoed, 0)
    )
  }

  # ===== Summary =====
  if (verbose) {
    cat("\n=== CV Splits Summary ===\n")
    for (k in 1:n_splits) {
      split <- cv_splits[[k]]
      cat(sprintf("Fold %d: Train=%d, Test=%d, Purged=%d, Embargoed=%d\n",
                  k, split$n_train, split$n_test, split$n_purged, split$n_embargoed))
    }

    avg_train <- mean(sapply(cv_splits, function(x) x$n_train))
    avg_test <- mean(sapply(cv_splits, function(x) x$n_test))
    cat(sprintf("\nAverage: Train=%.0f, Test=%.0f\n", avg_train, avg_test))
  }

  return(cv_splits)
}


#' Calculate label uniqueness (für Sample Weighting in CV)
#'
#' Berechnet wie "unique" jedes Label ist basierend auf Überlappung
#' Nach Lopez de Prado: Samples mit weniger Überlappung sind informativer
#'
#' @param dt data.table with datetime and t1 columns
#'
#' @return Vector of uniqueness scores (0 to 1)
calculate_label_uniqueness <- function(dt) {

  n <- nrow(dt)
  uniqueness <- rep(0, n)

  # Für jedes Sample, zähle überlappende Samples
  for (i in 1:n) {
    t0_i <- dt$datetime[i]
    t1_i <- dt$t1[i]

    # Finde überlappende Samples
    # Overlap wenn: (t0_j < t1_i) AND (t1_j > t0_i)
    overlaps <- which(
      dt$datetime < t1_i &
      dt$t1 > t0_i
    )

    n_overlaps <- length(overlaps)

    # Uniqueness = 1 / Anzahl überlappender Samples
    # Samples mit weniger Überlappung sind "uniquer"
    uniqueness[i] <- 1.0 / n_overlaps
  }

  return(uniqueness)
}


#' Adjust sample weights by label uniqueness (für CV)
#'
#' @param dt data.table with sample_weight column
#'
#' @return data.table with adjusted sample_weight_cv column
adjust_weights_for_cv <- function(dt) {

  dt_adjusted <- copy(dt)

  # Berechne Uniqueness
  uniqueness <- calculate_label_uniqueness(dt_adjusted)

  # Adjustiere Sample Weights
  if ("sample_weight" %in% names(dt_adjusted)) {
    dt_adjusted[, sample_weight_cv := sample_weight * uniqueness]
  } else {
    dt_adjusted[, sample_weight_cv := uniqueness]
  }

  # Normalisiere auf Summe = n
  sum_weights <- sum(dt_adjusted$sample_weight_cv)
  dt_adjusted[, sample_weight_cv := sample_weight_cv / sum_weights * .N]

  return(dt_adjusted)
}


#' Get train/test data from CV split
#'
#' Helper Funktion um Daten aus CV Split zu extrahieren
#'
#' @param dt data.table with all data
#' @param cv_split Single CV split from create_purged_kfold_splits
#' @param feature_cols Vector of feature column names
#' @param target_col Name of target column
#' @param weight_col Name of weight column
#'
#' @return List with X_train, y_train, w_train, X_test, y_test, w_test
get_cv_split_data <- function(
    dt,
    cv_split,
    feature_cols,
    target_col = "label",
    weight_col = "sample_weight"
) {

  # Train Data
  train_idx <- cv_split$train_idx
  X_train <- as.matrix(dt[train_idx, ..feature_cols])
  y_train <- dt[[target_col]][train_idx]
  w_train <- dt[[weight_col]][train_idx]

  # Test Data
  test_idx <- cv_split$test_idx
  X_test <- as.matrix(dt[test_idx, ..feature_cols])
  y_test <- dt[[target_col]][test_idx]
  w_test <- dt[[weight_col]][test_idx]

  return(list(
    X_train = X_train,
    y_train = y_train,
    w_train = w_train,
    X_test = X_test,
    y_test = y_test,
    w_test = w_test,
    train_idx = train_idx,
    test_idx = test_idx
  ))
}


#' Visualize CV splits over time
#'
#' @param dt data.table with datetime column
#' @param cv_splits List of CV splits
#' @param output_path Path to save plot (optional)
plot_cv_splits <- function(dt, cv_splits, output_path = NULL) {

  n_splits <- length(cv_splits)

  # Erstelle Plot Data
  plot_data <- data.table()

  for (k in 1:n_splits) {
    split <- cv_splits[[k]]

    # Train Points
    train_dt <- data.table(
      datetime = dt$datetime[split$train_idx],
      fold = k,
      type = "Train"
    )

    # Test Points
    test_dt <- data.table(
      datetime = dt$datetime[split$test_idx],
      fold = k,
      type = "Test"
    )

    plot_data <- rbind(plot_data, train_dt, test_dt)
  }

  # Plot
  p <- ggplot(plot_data, aes(x = datetime, y = fold, color = type)) +
    geom_point(alpha = 0.3, size = 0.5) +
    scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
    labs(
      title = "Purged K-Fold CV Splits",
      x = "Time",
      y = "Fold",
      color = "Set"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Speichern falls Pfad angegeben
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = 12, height = 6)
    cat(sprintf("CV Splits Plot gespeichert: %s\n", output_path))
  }

  return(p)
}
