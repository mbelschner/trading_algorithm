# Feature Selection Module
# Reduziert viele Features auf Top 25 wichtigste Features
# Verwendet XGBoost, Random Forest oder Boruta

#' Select important features from large feature set
#'
#' @param dt data.table with features and labels
#' @param target_col Name of target column (default: "label")
#' @param weight_col Name of sample weight column (default: "sample_weight")
#' @param method Feature selection method: "xgboost", "ranger", "boruta" (default: "xgboost")
#' @param n_top_features Number of top features to select (default: 25)
#' @param cv_folds Number of CV folds for stability (default: 3)
#' @param verbose Print progress messages
#'
#' @return List with top_features vector and importance data.table
select_important_features <- function(
    dt,
    target_col = "label",
    weight_col = "sample_weight",
    method = "xgboost",
    n_top_features = 25,
    cv_folds = 3,
    verbose = TRUE
) {

  if (verbose) cat(sprintf("\n=== Feature Selection (%s) ===\n", toupper(method)))

  # Identifiziere Feature-Spalten (exkludiere Meta-Daten und OHLCV)
  exclude_cols <- c("datetime", target_col, weight_col,
                    "barrier_touched", "bars_to_exit", "realized_return",
                    "upper_barrier", "lower_barrier", "effective_horizon",
                    "open", "high", "low", "close", "volume",
                    "label", "label_binary", "atr", "atr_pct", "hour", "date", "in_session")  # Exkludiere alle Label-Varianten und Meta-Daten
  all_cols <- names(dt)
  feature_cols <- setdiff(all_cols, exclude_cols)

  # Entferne nicht-numerische Features
  numeric_features <- feature_cols[sapply(dt[, ..feature_cols], is.numeric)]

  if (verbose) {
    cat(sprintf("Total Features: %d\n", length(numeric_features)))
    cat(sprintf("Target: %s\n", target_col))
    cat(sprintf("Samples: %s\n", format(nrow(dt), big.mark = ",")))
  }

  # Entferne Features mit zu vielen NAs oder konstante Features
  valid_features <- c()
  for (feat in numeric_features) {
    na_pct <- sum(is.na(dt[[feat]])) / nrow(dt)
    if (na_pct > 0.5) next  # Skip if > 50% NA

    # Check variance
    feat_var <- var(dt[[feat]], na.rm = TRUE)
    if (is.na(feat_var) || feat_var < 1e-10) next  # Skip constant features

    valid_features <- c(valid_features, feat)
  }

  if (verbose) {
    cat(sprintf("Valid Features (nach Filter): %d\n", length(valid_features)))
  }

  # Erstelle Feature Matrix und Target
  # Entferne Zeilen mit NA in Target oder Features
  dt_clean <- dt[, c(target_col, weight_col, valid_features), with = FALSE]
  dt_clean <- na.omit(dt_clean)

  if (verbose) {
    cat(sprintf("Samples nach NA-Entfernung: %s\n",
                format(nrow(dt_clean), big.mark = ",")))
  }

  X <- as.matrix(dt_clean[, ..valid_features])
  y <- dt_clean[[target_col]]
  weights <- dt_clean[[weight_col]]

  #Konvertiere Label zu 0/1 falls nötig
  if (!all(y %in% c(0, 1, -1))) {
    if (verbose) cat("Konvertiere Labels zu 0/1...\n")
    y <- as.integer(as.factor(y)) - 1
  } else if (all(y %in% c(-1, 1))) {
    # -1/1 zu 0/1
    y <- ifelse(y == -1, 0, 1)
  }

  # ===== Feature Selection per Method =====

  if (method == "xgboost") {
    importance_list <- select_features_xgboost(
      X = X,
      y = y,
      weights = weights,
      feature_names = valid_features,
      cv_folds = cv_folds,
      verbose = verbose
    )

  } else if (method == "ranger") {
    importance_list <- select_features_ranger(
      X = X,
      y = y,
      weights = weights,
      feature_names = valid_features,
      cv_folds = cv_folds,
      verbose = verbose
    )

  } else if (method == "boruta") {
    importance_list <- select_features_boruta(
      X = X,
      y = y,
      feature_names = valid_features,
      verbose = verbose
    )

  } else {
    stop("Unknown method. Choose 'xgboost', 'ranger', or 'boruta'")
  }

  # Sortiere nach Importance
  importance_dt <- data.table(
    feature = importance_list$features,
    importance = importance_list$importance
  )
  setorder(importance_dt, -importance)

  # Wähle Top N Features
  top_features <- importance_dt$feature[1:min(n_top_features, nrow(importance_dt))]

  if (verbose) {
    cat(sprintf("\n✓ Feature Selection abgeschlossen:\n"))
    cat(sprintf("  Top %d Features ausgewählt\n", length(top_features)))
    cat(sprintf("  Importance Range: %.4f - %.4f\n",
                min(importance_dt$importance[1:length(top_features)]),
                max(importance_dt$importance[1:length(top_features)])))
  }

  return(list(
    top_features = top_features,
    importance = importance_dt,
    method = method
  ))
}


#' XGBoost Feature Selection
select_features_xgboost <- function(X, y, weights, feature_names, cv_folds = 3, verbose = TRUE) {

  if (verbose) cat("Führe XGBoost Feature Selection durch...\n")

  # Initialisiere Importance-Tracker
  importance_scores <- rep(0, ncol(X))
  names(importance_scores) <- feature_names

  # CV Loop für stabilere Importance
  pb <- progress_bar$new(
    format = "  Fold [:bar] :current/:total (:percent) eta: :eta",
    total = cv_folds
  )

  for (fold in 1:cv_folds) {
    # Einfacher Random Split (keine zeitliche Berücksichtigung hier)
    set.seed(42 + fold)
    train_idx <- sample(1:nrow(X), size = floor(0.8 * nrow(X)))

    X_train <- X[train_idx, ]
    y_train <- y[train_idx]
    w_train <- weights[train_idx]

    # XGBoost DMatrix
    dtrain <- xgb.DMatrix(data = X_train, label = y_train, weight = w_train)

    # Trainiere XGBoost
    params <- list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 6,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )

    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 100,
      verbose = 0
    )

    # Extrahiere Importance (Gain)
    imp <- xgb.importance(model = xgb_model, feature_names = feature_names)

    # Akkumuliere Scores
    for (i in 1:nrow(imp)) {
      feat_name <- imp$Feature[i]
      importance_scores[feat_name] <- importance_scores[feat_name] + imp$Gain[i]
    }

    pb$tick()
  }

  # Durchschnitt über Folds
  importance_scores <- importance_scores / cv_folds

  return(list(
    features = feature_names,
    importance = importance_scores
  ))
}


#' Random Forest (ranger) Feature Selection
select_features_ranger <- function(X, y, weights, feature_names, cv_folds = 3, verbose = TRUE) {

  if (verbose) cat("Führe Random Forest Feature Selection durch...\n")

  # Initialisiere Importance-Tracker
  importance_scores <- rep(0, ncol(X))
  names(importance_scores) <- feature_names

  # Erstelle data.frame für ranger
  train_df <- as.data.frame(X)
  train_df$target <- as.factor(y)

  # CV Loop
  pb <- progress_bar$new(
    format = "  Fold [:bar] :current/:total (:percent) eta: :eta",
    total = cv_folds
  )

  for (fold in 1:cv_folds) {
    set.seed(42 + fold)
    train_idx <- sample(1:nrow(train_df), size = floor(0.8 * nrow(train_df)))

    df_train <- train_df[train_idx, ]
    w_train <- weights[train_idx]

    # Trainiere Random Forest mit Importance
    rf_model <- ranger(
      target ~ .,
      data = df_train,
      num.trees = 500,
      importance = "impurity",
      case.weights = w_train,
      verbose = FALSE
    )

    # Extrahiere Importance
    imp <- importance(rf_model)

    # Akkumuliere Scores
    importance_scores <- importance_scores + imp

    pb$tick()
  }

  # Durchschnitt über Folds
  importance_scores <- importance_scores / cv_folds

  return(list(
    features = feature_names,
    importance = importance_scores
  ))
}


#' Boruta Feature Selection (All-Relevant Features)
select_features_boruta <- function(X, y, feature_names, verbose = TRUE) {

  if (!requireNamespace("Boruta", quietly = TRUE)) {
    stop("Boruta package not installed. Install with: install.packages('Boruta')")
  }

  if (verbose) cat("Führe Boruta Feature Selection durch...\n")

  # Erstelle data.frame
  train_df <- as.data.frame(X)
  colnames(train_df) <- feature_names
  train_df$target <- as.factor(y)

  # Führe Boruta durch
  boruta_result <- Boruta::Boruta(
    target ~ .,
    data = train_df,
    doTrace = ifelse(verbose, 2, 0),
    maxRuns = 100
  )

  # Extrahiere bestätigte Features
  confirmed <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Confirmed"])
  tentative <- names(boruta_result$finalDecision[boruta_result$finalDecision == "Tentative"])

  # Importance Scores (Z-scores aus Boruta)
  imp_stats <- attStats(boruta_result)
  importance_scores <- imp_stats$meanImp
  names(importance_scores) <- rownames(imp_stats)

  if (verbose) {
    cat(sprintf("  Confirmed Features: %d\n", length(confirmed)))
    cat(sprintf("  Tentative Features: %d\n", length(tentative)))
  }

  return(list(
    features = feature_names,
    importance = importance_scores,
    confirmed = confirmed,
    tentative = tentative
  ))
}
