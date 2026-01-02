# Model Training Module with Feature Importance
# Implementiert MDI, MDA, SFI nach Lopez de Prado
#
# MDI: Mean Decrease Impurity (aus Tree-basierten Modellen)
# MDA: Mean Decrease Accuracy (Permutation Importance)
# SFI: Single Feature Importance (Orthogonalisiert)

#' Train and evaluate models with hyperparameter tuning
#'
#' @param dt data.table with features and labels
#' @param cv_splits Purged K-Fold CV splits
#' @param target_col Target column name
#' @param weight_col Sample weight column name
#' @param feature_cols Vector of feature column names
#' @param hyperparam_grid List of hyperparameter grids (xgb, rf)
#' @param models Vector of model types to train ("xgboost", "ranger")
#' @param n_cores Number of cores for parallel processing
#' @param verbose Print progress
#'
#' @return List with best model, hyperparams, and CV results
train_and_evaluate_models <- function(
    dt,
    cv_splits,
    target_col = "label",
    weight_col = "sample_weight",
    feature_cols,
    hyperparam_grid,
    models = c("xgboost", "ranger"),
    n_cores = parallel::detectCores() - 1,
    verbose = TRUE
) {

  if (verbose) cat("\n=== Model Training & Hyperparameter Tuning ===\n")

  # Setup parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  results_list <- list()

  # ===== Train XGBoost Models =====
  if ("xgboost" %in% models) {
    if (verbose) cat("\nTrainiere XGBoost Modelle...\n")

    xgb_results <- train_xgboost_cv(
      dt = dt,
      cv_splits = cv_splits,
      target_col = target_col,
      weight_col = weight_col,
      feature_cols = feature_cols,
      hyperparam_grid = hyperparam_grid$xgb,
      verbose = verbose
    )

    results_list$xgboost <- xgb_results
  }

  # ===== Train Random Forest Models =====
  if ("ranger" %in% models) {
    if (verbose) cat("\nTrainiere Random Forest Modelle...\n")

    rf_results <- train_ranger_cv(
      dt = dt,
      cv_splits = cv_splits,
      target_col = target_col,
      weight_col = weight_col,
      feature_cols = feature_cols,
      hyperparam_grid = hyperparam_grid$rf,
      verbose = verbose
    )

    results_list$ranger <- rf_results
  }

  # Stop parallel backend
  stopCluster(cl)
  registerDoSEQ()

  # ===== Select Best Model =====
  best_model_info <- select_best_model(results_list, verbose = verbose)

  return(list(
    best_model = best_model_info$model,
    best_model_type = best_model_info$type,
    best_hyperparams = best_model_info$hyperparams,
    best_score = best_model_info$score,
    all_results = results_list,
    best_models = best_model_info$comparison_table
  ))
}


#' Train XGBoost with CV
train_xgboost_cv <- function(
    dt,
    cv_splits,
    target_col,
    weight_col,
    feature_cols,
    hyperparam_grid,
    verbose = TRUE
) {

  n_configs <- nrow(hyperparam_grid)
  if (verbose) cat(sprintf("Testing %d XGBoost configurations...\n", n_configs))

  # Tracking
  cv_results <- data.table()

  pb <- progress_bar$new(
    format = "  Config [:bar] :current/:total (:percent) eta: :eta",
    total = n_configs
  )

  for (config_idx in 1:n_configs) {
    params <- as.list(hyperparam_grid[config_idx, ])

    # CV Scores für diese Config
    fold_scores <- c()

    for (k in 1:length(cv_splits)) {
      split <- cv_splits[[k]]

      # Get Data
      cv_data <- get_cv_split_data(
        dt = dt,
        cv_split = split,
        feature_cols = feature_cols,
        target_col = target_col,
        weight_col = weight_col
      )

      # Konvertiere Labels zu 0/1
      y_train <- ifelse(cv_data$y_train == -1, 0, cv_data$y_train)
      y_test <- ifelse(cv_data$y_test == -1, 0, cv_data$y_test)

      # Train XGBoost
      dtrain <- xgb.DMatrix(data = cv_data$X_train, label = y_train,
                           weight = cv_data$w_train)
      dtest <- xgb.DMatrix(data = cv_data$X_test, label = y_test)

      xgb_params <- list(
        objective = "binary:logistic",
        eval_metric = "auc",
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree,
        min_child_weight = params$min_child_weight
      )

      model <- xgb.train(
        params = xgb_params,
        data = dtrain,
        nrounds = 200,
        verbose = 0,
        early_stopping_rounds = 20,
        watchlist = list(test = dtest)
      )

      # Predict
      preds <- predict(model, dtest)

      # AUC Score
      auc_score <- Metrics::auc(actual = y_test, predicted = preds)
      fold_scores <- c(fold_scores, auc_score)
    }

    # Durchschnitt über Folds
    mean_score <- mean(fold_scores)
    sd_score <- sd(fold_scores)

    cv_results <- rbind(cv_results, data.table(
      config_idx = config_idx,
      max_depth = params$max_depth,
      eta = params$eta,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      min_child_weight = params$min_child_weight,
      mean_auc = mean_score,
      sd_auc = sd_score
    ))

    pb$tick()
  }

  # Sortiere nach mean_auc
  setorder(cv_results, -mean_auc)

  # Trainiere finales Modell mit besten Hyperparameters auf allen Daten
  best_params <- cv_results[1]

  if (verbose) {
    cat("\nBeste XGBoost Config:\n")
    print(best_params)
  }

  # Trainiere auf allen Daten
  X_all <- as.matrix(dt[, ..feature_cols])
  y_all <- ifelse(dt[[target_col]] == -1, 0, dt[[target_col]])
  w_all <- dt[[weight_col]]

  dtrain_all <- xgb.DMatrix(data = X_all, label = y_all, weight = w_all)

  xgb_params_final <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    subsample = best_params$subsample,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight
  )

  final_model <- xgb.train(
    params = xgb_params_final,
    data = dtrain_all,
    nrounds = 200,
    verbose = 0
  )

  return(list(
    model = final_model,
    best_params = best_params,
    cv_results = cv_results
  ))
}


#' Train Random Forest (ranger) with CV
train_ranger_cv <- function(
    dt,
    cv_splits,
    target_col,
    weight_col,
    feature_cols,
    hyperparam_grid,
    verbose = TRUE
) {

  n_configs <- nrow(hyperparam_grid)
  if (verbose) cat(sprintf("Testing %d Random Forest configurations...\n", n_configs))

  cv_results <- data.table()

  pb <- progress_bar$new(
    format = "  Config [:bar] :current/:total (:percent) eta: :eta",
    total = n_configs
  )

  for (config_idx in 1:n_configs) {
    params <- as.list(hyperparam_grid[config_idx, ])

    fold_scores <- c()

    for (k in 1:length(cv_splits)) {
      split <- cv_splits[[k]]

      cv_data <- get_cv_split_data(
        dt = dt,
        cv_split = split,
        feature_cols = feature_cols,
        target_col = target_col,
        weight_col = weight_col
      )

      # Erstelle data.frame für ranger
      train_df <- as.data.frame(cv_data$X_train)
      colnames(train_df) <- feature_cols
      train_df$target <- as.factor(ifelse(cv_data$y_train == -1, 0, cv_data$y_train))

      test_df <- as.data.frame(cv_data$X_test)
      colnames(test_df) <- feature_cols

      # Train Ranger
      rf_model <- ranger(
        target ~ .,
        data = train_df,
        num.trees = params$num.trees,
        mtry = params$mtry,
        max.depth = params$max.depth,
        min.node.size = params$min.node.size,
        case.weights = cv_data$w_train,
        probability = TRUE,
        verbose = FALSE
      )

      # Predict
      preds <- predict(rf_model, data = test_df)$predictions[, 2]  # Probability of class 1

      # AUC
      y_test_binary <- ifelse(cv_data$y_test == -1, 0, cv_data$y_test)
      auc_score <- Metrics::auc(actual = y_test_binary, predicted = preds)
      fold_scores <- c(fold_scores, auc_score)
    }

    mean_score <- mean(fold_scores)
    sd_score <- sd(fold_scores)

    cv_results <- rbind(cv_results, data.table(
      config_idx = config_idx,
      num.trees = params$num.trees,
      mtry = params$mtry,
      max.depth = params$max.depth,
      min.node.size = params$min.node.size,
      mean_auc = mean_score,
      sd_auc = sd_score
    ))

    pb$tick()
  }

  setorder(cv_results, -mean_auc)

  best_params <- cv_results[1]

  if (verbose) {
    cat("\nBeste Random Forest Config:\n")
    print(best_params)
  }

  # Trainiere finales Modell
  train_df_all <- as.data.frame(as.matrix(dt[, ..feature_cols]))
  colnames(train_df_all) <- feature_cols
  train_df_all$target <- as.factor(ifelse(dt[[target_col]] == -1, 0, dt[[target_col]]))

  final_model <- ranger(
    target ~ .,
    data = train_df_all,
    num.trees = best_params$num.trees,
    mtry = best_params$mtry,
    max.depth = best_params$max.depth,
    min.node.size = best_params$min.node.size,
    case.weights = dt[[weight_col]],
    probability = TRUE,
    importance = "impurity",
    verbose = FALSE
  )

  return(list(
    model = final_model,
    best_params = best_params,
    cv_results = cv_results
  ))
}


#' Select best model from all trained models
select_best_model <- function(results_list, verbose = TRUE) {

  comparison <- data.table()

  for (model_type in names(results_list)) {
    result <- results_list[[model_type]]
    best_params <- result$best_params

    comparison <- rbind(comparison, data.table(
      model_type = model_type,
      mean_auc = best_params$mean_auc,
      sd_auc = best_params$sd_auc
    ))
  }

  setorder(comparison, -mean_auc)

  best_type <- comparison$model_type[1]
  best_result <- results_list[[best_type]]

  if (verbose) {
    cat("\n=== Model Comparison ===\n")
    print(comparison)
    cat(sprintf("\nBest Model: %s (AUC: %.4f ± %.4f)\n",
                best_type, comparison$mean_auc[1], comparison$sd_auc[1]))
  }

  return(list(
    type = best_type,
    model = best_result$model,
    hyperparams = best_result$best_params,
    score = comparison$mean_auc[1],
    comparison_table = comparison
  ))
}


#' Calculate Feature Importance: MDI, MDA, SFI
#'
#' @param model Trained model (xgboost or ranger)
#' @param dt data.table with features
#' @param cv_splits CV splits for MDA/SFI
#' @param target_col Target column
#' @param feature_cols Feature columns
#' @param weight_col Weight column
#' @param methods Vector of methods: "MDI", "MDA", "SFI"
#' @param n_cores Number of cores
#' @param verbose Print progress
#'
#' @return List with importance data.tables for each method
calculate_feature_importance <- function(
    model,
    dt,
    cv_splits,
    target_col,
    feature_cols,
    weight_col,
    methods = c("MDI", "MDA", "SFI"),
    n_cores = parallel::detectCores() - 1,
    verbose = TRUE
) {

  if (verbose) cat("\n=== Feature Importance Analysis ===\n")

  importance_results <- list()

  # ===== MDI: Mean Decrease Impurity =====
  if ("MDI" %in% methods) {
    if (verbose) cat("Berechne MDI (Mean Decrease Impurity)...\n")

    mdi_importance <- calculate_mdi(model, feature_cols, verbose = verbose)
    importance_results$MDI <- mdi_importance
  }

  # ===== MDA: Mean Decrease Accuracy =====
  if ("MDA" %in% methods) {
    if (verbose) cat("Berechne MDA (Mean Decrease Accuracy / Permutation Importance)...\n")

    mda_importance <- calculate_mda(
      model = model,
      dt = dt,
      cv_splits = cv_splits,
      target_col = target_col,
      feature_cols = feature_cols,
      weight_col = weight_col,
      n_cores = n_cores,
      verbose = verbose
    )
    importance_results$MDA <- mda_importance
  }

  # ===== SFI: Single Feature Importance =====
  if ("SFI" %in% methods) {
    if (verbose) cat("Berechne SFI (Single Feature Importance)...\n")

    sfi_importance <- calculate_sfi(
      dt = dt,
      cv_splits = cv_splits,
      target_col = target_col,
      feature_cols = feature_cols,
      weight_col = weight_col,
      verbose = verbose
    )
    importance_results$SFI <- sfi_importance
  }

  return(importance_results)
}


#' MDI: Mean Decrease Impurity (direkt aus Modell)
calculate_mdi <- function(model, feature_cols, verbose = TRUE) {

  if (inherits(model, "xgb.Booster")) {
    # XGBoost
    imp <- xgb.importance(model = model, feature_names = feature_cols)
    mdi_dt <- data.table(
      feature = imp$Feature,
      importance = imp$Gain
    )

  } else if (inherits(model, "ranger")) {
    # Ranger
    imp <- importance(model)
    mdi_dt <- data.table(
      feature = names(imp),
      importance = as.numeric(imp)
    )

  } else {
    stop("Unknown model type for MDI")
  }

  setorder(mdi_dt, -importance)

  if (verbose) {
    cat(sprintf("✓ MDI berechnet für %d Features\n", nrow(mdi_dt)))
  }

  return(mdi_dt)
}


#' MDA: Mean Decrease Accuracy (Permutation Importance)
calculate_mda <- function(
    model,
    dt,
    cv_splits,
    target_col,
    feature_cols,
    weight_col,
    n_cores,
    verbose = TRUE
) {

  # Initialisiere Importance Scores
  importance_scores <- setNames(rep(0, length(feature_cols)), feature_cols)

  # Über alle CV Folds
  for (k in 1:length(cv_splits)) {
    split <- cv_splits[[k]]

    cv_data <- get_cv_split_data(
      dt = dt,
      cv_split = split,
      feature_cols = feature_cols,
      target_col = target_col,
      weight_col = weight_col
    )

    # Baseline Score (ohne Permutation)
    baseline_score <- compute_model_score(
      model = model,
      X = cv_data$X_test,
      y = cv_data$y_test,
      feature_cols = feature_cols
    )

    # Permutiere jedes Feature einzeln
    for (feat_idx in 1:length(feature_cols)) {
      feat_name <- feature_cols[feat_idx]

      # Permutiere Feature
      X_permuted <- cv_data$X_test
      X_permuted[, feat_idx] <- sample(X_permuted[, feat_idx])

      # Score mit permutiertem Feature
      permuted_score <- compute_model_score(
        model = model,
        X = X_permuted,
        y = cv_data$y_test,
        feature_cols = feature_cols
      )

      # Importance = Decrease in Score
      decrease <- baseline_score - permuted_score
      importance_scores[feat_name] <- importance_scores[feat_name] + decrease
    }
  }

  # Durchschnitt über Folds
  importance_scores <- importance_scores / length(cv_splits)

  mda_dt <- data.table(
    feature = names(importance_scores),
    importance = as.numeric(importance_scores)
  )
  setorder(mda_dt, -importance)

  if (verbose) {
    cat(sprintf("✓ MDA berechnet für %d Features über %d Folds\n",
                length(feature_cols), length(cv_splits)))
  }

  return(mda_dt)
}


#' SFI: Single Feature Importance (orthogonalisiert)
calculate_sfi <- function(
    dt,
    cv_splits,
    target_col,
    feature_cols,
    weight_col,
    verbose = TRUE
) {

  importance_scores <- setNames(rep(0, length(feature_cols)), feature_cols)

  # Trainiere ein Modell pro Feature (einzeln)
  pb <- progress_bar$new(
    format = "  Feature [:bar] :current/:total (:percent) eta: :eta",
    total = length(feature_cols)
  )

  for (feat_name in feature_cols) {
    fold_scores <- c()

    for (k in 1:length(cv_splits)) {
      split <- cv_splits[[k]]

      cv_data <- get_cv_split_data(
        dt = dt,
        cv_split = split,
        feature_cols = feat_name,  # Nur dieses eine Feature!
        target_col = target_col,
        weight_col = weight_col
      )

      # Trainiere einfaches Modell mit nur diesem Feature
      y_train <- ifelse(cv_data$y_train == -1, 0, cv_data$y_train)
      y_test <- ifelse(cv_data$y_test == -1, 0, cv_data$y_test)

      # XGBoost mit einem Feature
      dtrain <- xgb.DMatrix(data = as.matrix(cv_data$X_train), label = y_train,
                           weight = cv_data$w_train)
      dtest <- xgb.DMatrix(data = as.matrix(cv_data$X_test), label = y_test)

      model_single <- xgb.train(
        params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = 3),
        data = dtrain,
        nrounds = 50,
        verbose = 0
      )

      preds <- predict(model_single, dtest)
      auc_score <- Metrics::auc(actual = y_test, predicted = preds)
      fold_scores <- c(fold_scores, auc_score)
    }

    importance_scores[feat_name] <- mean(fold_scores)
    pb$tick()
  }

  sfi_dt <- data.table(
    feature = names(importance_scores),
    importance = as.numeric(importance_scores)
  )
  setorder(sfi_dt, -importance)

  if (verbose) {
    cat(sprintf("✓ SFI berechnet für %d Features\n", length(feature_cols)))
  }

  return(sfi_dt)
}


#' Helper: Compute model score (AUC)
compute_model_score <- function(model, X, y, feature_cols) {

  if (inherits(model, "xgb.Booster")) {
    dmat <- xgb.DMatrix(data = X)
    preds <- predict(model, dmat)
    y_binary <- ifelse(y == -1, 0, y)
    score <- Metrics::auc(actual = y_binary, predicted = preds)

  } else if (inherits(model, "ranger")) {
    test_df <- as.data.frame(X)
    colnames(test_df) <- feature_cols
    preds <- predict(model, data = test_df)$predictions[, 2]
    y_binary <- ifelse(y == -1, 0, y)
    score <- Metrics::auc(actual = y_binary, predicted = preds)

  } else {
    stop("Unknown model type")
  }

  return(score)
}


#' Combine importance rankings from multiple methods
combine_importance_rankings <- function(importance_results) {

  # Erstelle Rankings pro Methode
  all_features <- unique(unlist(lapply(importance_results, function(x) x$feature)))

  combined <- data.table(feature = all_features)

  for (method in names(importance_results)) {
    imp_dt <- importance_results[[method]]

    # Rank (1 = höchste Importance)
    imp_dt[, rank := frank(-importance, ties.method = "min")]

    # Merge
    setnames(imp_dt, c("importance", "rank"), paste0(c("imp_", "rank_"), method))
    combined <- merge(combined, imp_dt[, c("feature", paste0("imp_", method), paste0("rank_", method)), with = FALSE],
                     by = "feature", all.x = TRUE)
  }

  # Kombinierter Rank: Durchschnitt der Ranks
  rank_cols <- grep("^rank_", names(combined), value = TRUE)
  combined[, combined_rank := rowMeans(.SD, na.rm = TRUE), .SDcols = rank_cols]

  setorder(combined, combined_rank)

  return(combined)
}


#' Train final model with selected features
train_final_model <- function(
    dt,
    cv_splits,
    target_col,
    weight_col,
    feature_cols,
    model_type,
    hyperparams,
    verbose = TRUE
) {

  if (verbose) {
    cat(sprintf("\n=== Training Final Model (%s) ===\n", model_type))
    cat(sprintf("Features: %d\n", length(feature_cols)))
  }

  X_all <- as.matrix(dt[, ..feature_cols])
  y_all <- ifelse(dt[[target_col]] == -1, 0, dt[[target_col]])
  w_all <- dt[[weight_col]]

  if (model_type == "xgboost") {
    dtrain <- xgb.DMatrix(data = X_all, label = y_all, weight = w_all)

    xgb_params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = hyperparams$max_depth,
      eta = hyperparams$eta,
      subsample = hyperparams$subsample,
      colsample_bytree = hyperparams$colsample_bytree,
      min_child_weight = hyperparams$min_child_weight
    )

    final_model <- xgb.train(
      params = xgb_params,
      data = dtrain,
      nrounds = 300,
      verbose = ifelse(verbose, 1, 0)
    )

  } else if (model_type == "ranger") {
    train_df <- as.data.frame(X_all)
    colnames(train_df) <- feature_cols
    train_df$target <- as.factor(y_all)

    final_model <- ranger(
      target ~ .,
      data = train_df,
      num.trees = hyperparams$num.trees,
      mtry = hyperparams$mtry,
      max.depth = hyperparams$max.depth,
      min.node.size = hyperparams$min.node.size,
      case.weights = w_all,
      probability = TRUE,
      importance = "impurity",
      verbose = verbose
    )
  }

  if (verbose) cat("✓ Final Model trainiert\n")

  return(final_model)
}
