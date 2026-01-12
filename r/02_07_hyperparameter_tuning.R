# Hyperparameter Tuning Module
# Grid Search for XGBoost Hyperparameters (No CV, Train + Test Evaluation)

#' Hyperparameter Tuning for XGBoost with Grid Search
#'
#' @param dt_train Training dataset (2019-2024) with features and labels
#' @param dt_test Test dataset (2025) with features and labels
#' @param feature_cols Vector of feature column names
#' @param target_col Name of target column (default: "label_binary")
#' @param weight_col Name of sample weight column (default: "sample_weight")
#' @param param_grid List of parameter vectors to search over
#' @param early_stopping_rounds Early stopping rounds (default: 50)
#' @param maximize_metric Metric to maximize: "auc" or "precision" (default: "auc")
#' @param verbose Print progress messages
#'
#' @return List with best_params, tuning_results (data.frame), and best_model
tune_xgboost_hyperparameters <- function(
    dt_train,
    dt_test,
    feature_cols,
    target_col = "label_binary",
    weight_col = "sample_weight",
    param_grid = NULL,
    early_stopping_rounds = 50,
    maximize_metric = "auc",
    verbose = TRUE
) {

  if (verbose) cat("\n=== HYPERPARAMETER TUNING ===\n")

  # Default parameter grid if not provided
  if (is.null(param_grid)) {
    param_grid <- list(
      max_depth = c(3, 4, 5),
      eta = c(0.03, 0.05, 0.1),
      gamma = c(0, 0.1, 0.2),
      lambda = c(1.0, 1.5, 2.0),
      min_child_weight = c(5, 10, 15)
    )
  }

  # Calculate total number of combinations
  n_combinations <- prod(sapply(param_grid, length))

  if (verbose) {
    cat(sprintf("Parameter Grid:\n"))
    for (param_name in names(param_grid)) {
      cat(sprintf("  %s: %s\n", param_name, paste(param_grid[[param_name]], collapse = ", ")))
    }
    cat(sprintf("\nTotal combinations: %d\n", n_combinations))
    cat(sprintf("Maximize: %s (on training data)\n", toupper(maximize_metric)))
    cat(sprintf("Estimated time: ~%d-%d minutes\n\n",
                ceiling(n_combinations * 0.3),
                ceiling(n_combinations * 1.0)))
  }

  # Prepare training data
  X_train <- as.matrix(dt_train[, ..feature_cols])
  y_train <- dt_train[[target_col]]
  w_train <- dt_train[[weight_col]]

  # Prepare test data
  X_test <- as.matrix(dt_test[, ..feature_cols])
  y_test <- dt_test[[target_col]]

  # Calculate scale_pos_weight
  n_negative <- sum(y_train == 0)
  n_positive <- sum(y_train == 1)
  scale_pos_weight <- n_negative / (n_positive + 1e-10)

  if (verbose) {
    cat(sprintf("Training set: %s rows\n", format(nrow(dt_train), big.mark = ",")))
    cat(sprintf("Test set: %s rows\n", format(nrow(dt_test), big.mark = ",")))
    cat(sprintf("Class balance (train): Negative=%s, Positive=%s\n",
                format(n_negative, big.mark = ","),
                format(n_positive, big.mark = ",")))
    cat(sprintf("scale_pos_weight: %.4f\n\n", scale_pos_weight))
  }

  # Fixed parameters
  fixed_params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    subsample = 0.8,
    colsample_bytree = 0.8,
    colsample_bynode = 0.8,
    scale_pos_weight = scale_pos_weight
  )

  # Create all combinations of parameters
  param_combinations <- expand.grid(param_grid, stringsAsFactors = FALSE)

  # Initialize results storage
  tuning_results <- data.frame(
    combination_id = integer(),
    max_depth = numeric(),
    eta = numeric(),
    gamma = numeric(),
    lambda = numeric(),
    min_child_weight = numeric(),
    train_auc = numeric(),
    train_precision = numeric(),
    train_recall = numeric(),
    train_f1 = numeric(),
    test_auc = numeric(),
    test_precision = numeric(),
    test_recall = numeric(),
    test_f1 = numeric(),
    best_iteration = integer(),
    stringsAsFactors = FALSE
  )

  # Progress bar
  pb <- progress_bar$new(
    format = "  [:bar] :percent | Combination :current/:total | ETA: :eta",
    total = n_combinations,
    clear = FALSE,
    width = 80
  )

  # Split training data for early stopping validation
  set.seed(42)
  val_idx <- sample(1:nrow(X_train), size = floor(0.2 * nrow(X_train)))
  train_idx <- setdiff(1:nrow(X_train), val_idx)

  X_train_sub <- X_train[train_idx, ]
  y_train_sub <- y_train[train_idx]
  w_train_sub <- w_train[train_idx]

  X_val <- X_train[val_idx, ]
  y_val <- y_train[val_idx]
  w_val <- w_train[val_idx]

  # Grid search
  for (i in 1:nrow(param_combinations)) {

    params_to_test <- as.list(param_combinations[i, ])
    full_params <- c(fixed_params, params_to_test)

    # Create DMatrix
    dtrain <- xgb.DMatrix(data = X_train_sub, label = y_train_sub, weight = w_train_sub)
    dval <- xgb.DMatrix(data = X_val, label = y_val, weight = w_val)
    dtrain_full <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)
    dtest <- xgb.DMatrix(data = X_test, label = y_test)

    # Train model with early stopping
    model <- xgb.train(
      params = full_params,
      data = dtrain,
      nrounds = 1000,
      evals = list(train = dtrain, val = dval),
      early_stopping_rounds = early_stopping_rounds,
      verbose = 0
    )

    # Predictions on full training set
    pred_train <- predict(model, dtrain_full)
    train_metrics <- calculate_binary_metrics(y_train, pred_train)

    # Predictions on test set
    pred_test <- predict(model, dtest)
    test_metrics <- calculate_binary_metrics(y_test, pred_test)

    # Get best iteration (handle NULL case)
    best_iter <- ifelse(is.null(model$best_iteration), model$niter, model$best_iteration)

    # Store results
    tuning_results <- rbind(tuning_results, data.frame(
      combination_id = i,
      max_depth = params_to_test$max_depth,
      eta = params_to_test$eta,
      gamma = params_to_test$gamma,
      lambda = params_to_test$lambda,
      min_child_weight = params_to_test$min_child_weight,
      train_auc = train_metrics$auc,
      train_precision = train_metrics$precision,
      train_recall = train_metrics$recall,
      train_f1 = train_metrics$f1,
      test_auc = test_metrics$auc,
      test_precision = test_metrics$precision,
      test_recall = test_metrics$recall,
      test_f1 = test_metrics$f1,
      best_iteration = best_iter,
      stringsAsFactors = FALSE
    ))

    pb$tick()
  }

  # Find best parameters based on maximize_metric
  if (verbose) cat("\n\n=== Selecting Best Parameters ===\n")

  if (maximize_metric == "auc") {
    best_idx <- which.max(tuning_results$train_auc)
    metric_col <- "train_auc"
  } else if (maximize_metric == "precision") {
    best_idx <- which.max(tuning_results$train_precision)
    metric_col <- "train_precision"
  } else {
    stop("maximize_metric must be 'auc' or 'precision'")
  }

  best_params <- as.list(tuning_results[best_idx, c("max_depth", "eta", "gamma", "lambda", "min_child_weight")])
  best_score <- tuning_results[best_idx, metric_col]

  if (verbose) {
    cat("\n=== BEST PARAMETERS ===\n")
    cat(sprintf("Combination ID: %d\n", tuning_results$combination_id[best_idx]))
    cat(sprintf("Max Depth: %d\n", best_params$max_depth))
    cat(sprintf("Eta: %.3f\n", best_params$eta))
    cat(sprintf("Gamma: %.2f\n", best_params$gamma))
    cat(sprintf("Lambda: %.2f\n", best_params$lambda))
    cat(sprintf("Min Child Weight: %d\n", best_params$min_child_weight))
    cat(sprintf("\nTraining Performance:\n"))
    cat(sprintf("  Train AUC: %.4f\n", tuning_results$train_auc[best_idx]))
    cat(sprintf("  Train Precision: %.4f\n", tuning_results$train_precision[best_idx]))
    cat(sprintf("  Train Recall: %.4f\n", tuning_results$train_recall[best_idx]))
    cat(sprintf("  Train F1: %.4f\n", tuning_results$train_f1[best_idx]))
    cat(sprintf("\nTest Performance:\n"))
    cat(sprintf("  Test AUC: %.4f\n", tuning_results$test_auc[best_idx]))
    cat(sprintf("  Test Precision: %.4f\n", tuning_results$test_precision[best_idx]))
    cat(sprintf("  Test Recall: %.4f\n", tuning_results$test_recall[best_idx]))
    cat(sprintf("  Test F1: %.4f\n", tuning_results$test_f1[best_idx]))
    cat(sprintf("\nBest Iteration: %d\n", tuning_results$best_iteration[best_idx]))
  }

  # Train final model with best parameters on full training set
  if (verbose) cat("\n=== Training Final Model with Best Parameters ===\n")

  full_best_params <- c(fixed_params, best_params)

  # Use early stopping with validation split
  dtrain_final <- xgb.DMatrix(data = X_train_sub, label = y_train_sub, weight = w_train_sub)
  dval_final <- xgb.DMatrix(data = X_val, label = y_val, weight = w_val)

  best_model <- xgb.train(
    params = full_best_params,
    data = dtrain_final,
    nrounds = 1000,
    evals = list(train = dtrain_final, val = dval_final),
    early_stopping_rounds = early_stopping_rounds,
    verbose = 0
  )

  if (verbose) cat(sprintf("✓ Model trained with best parameters\n"))

  return(list(
    best_params = best_params,
    best_full_params = full_best_params,
    tuning_results = tuning_results,
    best_model = best_model,
    best_score = best_score
  ))
}


#' Calculate binary classification metrics
#'
#' @param y_true True labels
#' @param y_pred_prob Predicted probabilities
#' @param threshold Classification threshold (default: 0.5)
#'
#' @return List with AUC, Precision, Recall, F1
calculate_binary_metrics <- function(y_true, y_pred_prob, threshold = 0.5) {

  # Default values in case of error
  default_metrics <- list(auc = 0, precision = 0, recall = 0, f1 = 0)

  tryCatch({
    # Classification
    y_pred_class <- ifelse(y_pred_prob > threshold, 1, 0)

    # Confusion matrix
    conf_matrix <- table(Predicted = y_pred_class, Actual = y_true)

    # Handle edge cases
    if (nrow(conf_matrix) < 2 || ncol(conf_matrix) < 2) {
      return(default_metrics)
    }

    # Extract confusion matrix values safely
    TP <- if ("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["1", "1"]
    } else {
      0
    }

    TN <- if ("0" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["0", "0"]
    } else {
      0
    }

    FP <- if ("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix)) {
      conf_matrix["1", "0"]
    } else {
      0
    }

    FN <- if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0", "1"]
    } else {
      0
    }

    # Metrics
    precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
    recall <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    f1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)

    # AUC
    auc_score <- tryCatch({
      roc_obj <- pROC::roc(y_true, y_pred_prob, quiet = TRUE)
      as.numeric(pROC::auc(roc_obj))
    }, error = function(e) {
      0
    })

    return(list(
      auc = auc_score,
      precision = precision,
      recall = recall,
      f1 = f1
    ))
  }, error = function(e) {
    warning(sprintf("Error calculating metrics: %s", e$message))
    return(default_metrics)
  })
}
