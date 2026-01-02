# Backtest Evaluation Module
# Evaluiert Modell-Performance mit verschiedenen Metriken
# Erstellt Visualisierungen und Performance-Reports

#' Evaluate backtest performance
#'
#' @param model Trained model
#' @param dt data.table with features and labels
#' @param cv_splits CV splits for out-of-sample evaluation
#' @param target_col Target column name
#' @param feature_cols Feature column names
#' @param weight_col Weight column name
#' @param include_returns Calculate returns-based metrics (requires 'return' column)
#' @param plot_results Create visualization plots
#' @param output_path Path to save plots and results
#' @param verbose Print progress
#'
#' @return List with metrics, predictions, and plots
evaluate_backtest <- function(
    model,
    dt,
    cv_splits,
    target_col = "label",
    feature_cols,
    weight_col = "sample_weight",
    include_returns = TRUE,
    plot_results = TRUE,
    output_path = "backtest_results",
    verbose = TRUE
) {

  if (verbose) cat("\n=== Backtest Evaluation ===\n")

  # ===== Generate Out-of-Sample Predictions =====
  if (verbose) cat("Generiere Out-of-Sample Predictions...\n")

  predictions_dt <- generate_oos_predictions(
    model = model,
    dt = dt,
    cv_splits = cv_splits,
    target_col = target_col,
    feature_cols = feature_cols,
    verbose = verbose
  )

  # ===== Calculate Classification Metrics =====
  if (verbose) cat("Berechne Classification Metriken...\n")

  class_metrics <- calculate_classification_metrics(
    predictions = predictions_dt,
    verbose = verbose
  )

  # ===== Calculate Returns-Based Metrics =====
  if (include_returns && "return" %in% names(dt)) {
    if (verbose) cat("Berechne Returns-basierte Metriken...\n")

    # Merge returns
    predictions_dt <- merge(
      predictions_dt,
      dt[, .(datetime, return)],
      by = "datetime",
      all.x = TRUE
    )

    returns_metrics <- calculate_returns_metrics(
      predictions = predictions_dt,
      verbose = verbose
    )
  } else {
    returns_metrics <- NULL
  }

  # ===== Create Plots =====
  if (plot_results) {
    if (verbose) cat("Erstelle Visualisierungen...\n")

    plots <- create_backtest_plots(
      predictions = predictions_dt,
      output_path = output_path,
      verbose = verbose
    )
  } else {
    plots <- NULL
  }

  # ===== Generate Summary Report =====
  summary_report <- generate_summary_report(
    class_metrics = class_metrics,
    returns_metrics = returns_metrics,
    predictions = predictions_dt
  )

  if (verbose) {
    cat("\n=== BACKTEST SUMMARY ===\n")
    cat(summary_report)
  }

  # ===== Return Results =====
  return(list(
    predictions = predictions_dt,
    metrics = list(
      classification = class_metrics,
      returns = returns_metrics
    ),
    plots = plots,
    summary = summary_report
  ))
}


#' Generate out-of-sample predictions from CV splits
generate_oos_predictions <- function(model, dt, cv_splits, target_col, feature_cols, verbose = TRUE) {

  all_predictions <- data.table()

  for (k in 1:length(cv_splits)) {
    split <- cv_splits[[k]]

    # Get Test Data
    test_idx <- split$test_idx
    X_test <- as.matrix(dt[test_idx, ..feature_cols])
    y_test <- dt[[target_col]][test_idx]
    datetime_test <- dt$datetime[test_idx]

    # Predict
    if (inherits(model, "xgb.Booster")) {
      dtest <- xgb.DMatrix(data = X_test)
      pred_proba <- predict(model, dtest)

    } else if (inherits(model, "ranger")) {
      test_df <- as.data.frame(X_test)
      colnames(test_df) <- feature_cols
      pred_proba <- predict(model, data = test_df)$predictions[, 2]
    }

    # Binary Prediction (threshold 0.5)
    pred_binary <- ifelse(pred_proba > 0.5, 1, -1)

    # Konvertiere y_test zu 0/1 für Metriken
    y_test_binary <- ifelse(y_test == -1, 0, y_test)

    # Sammle Predictions
    fold_preds <- data.table(
      datetime = datetime_test,
      actual = y_test,
      actual_binary = y_test_binary,
      pred_proba = pred_proba,
      pred_binary = pred_binary,
      fold = k
    )

    all_predictions <- rbind(all_predictions, fold_preds)
  }

  # Sortiere nach Zeit
  setorder(all_predictions, datetime)

  if (verbose) {
    cat(sprintf("  ✓ %d Out-of-Sample Predictions generiert\n", nrow(all_predictions)))
  }

  return(all_predictions)
}


#' Calculate classification metrics
calculate_classification_metrics <- function(predictions, verbose = TRUE) {

  actual <- predictions$actual_binary
  pred_proba <- predictions$pred_proba
  pred_binary <- ifelse(predictions$pred_binary == -1, 0, predictions$pred_binary)

  # AUC
  auc <- Metrics::auc(actual = actual, predicted = pred_proba)

  # Accuracy
  accuracy <- mean(actual == pred_binary)

  # Confusion Matrix
  cm <- table(Actual = actual, Predicted = pred_binary)

  # Precision, Recall, F1
  if (all(c(0, 1) %in% pred_binary)) {
    precision <- cm[2, 2] / sum(cm[, 2])
    recall <- cm[2, 2] / sum(cm[2, ])
    f1 <- 2 * (precision * recall) / (precision + recall)
  } else {
    precision <- recall <- f1 <- NA
  }

  # Log Loss
  logloss <- Metrics::logLoss(actual = actual, predicted = pred_proba)

  metrics <- list(
    auc = auc,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    logloss = logloss,
    confusion_matrix = cm
  )

  if (verbose) {
    cat(sprintf("  AUC: %.4f\n", auc))
    cat(sprintf("  Accuracy: %.4f\n", accuracy))
    cat(sprintf("  Precision: %.4f\n", precision))
    cat(sprintf("  Recall: %.4f\n", recall))
    cat(sprintf("  F1: %.4f\n", f1))
    cat(sprintf("  Log Loss: %.4f\n", logloss))
  }

  return(metrics)
}


#' Calculate returns-based metrics
calculate_returns_metrics <- function(predictions, verbose = TRUE) {

  # Strategy Returns: Only take trades where model predicts 1
  predictions[, strategy_return := ifelse(pred_binary == 1, return, 0)]

  # Cumulative Returns
  predictions[, cum_return := cumsum(return)]
  predictions[, cum_strategy_return := cumsum(strategy_return)]

  # Total Return
  total_return <- sum(predictions$return, na.rm = TRUE)
  total_strategy_return <- sum(predictions$strategy_return, na.rm = TRUE)

  # Number of Trades
  n_trades <- sum(predictions$pred_binary == 1)
  n_total <- nrow(predictions)

  # Win Rate (von Strategy Trades)
  strategy_trades <- predictions[pred_binary == 1]
  if (nrow(strategy_trades) > 0) {
    win_rate <- sum(strategy_trades$return > 0, na.rm = TRUE) / nrow(strategy_trades)
    avg_win <- mean(strategy_trades$return[strategy_trades$return > 0], na.rm = TRUE)
    avg_loss <- mean(strategy_trades$return[strategy_trades$return < 0], na.rm = TRUE)
    profit_factor <- ifelse(is.finite(avg_win / abs(avg_loss)), avg_win / abs(avg_loss), NA)
  } else {
    win_rate <- avg_win <- avg_loss <- profit_factor <- NA
  }

  # Sharpe Ratio (annualisiert, angenommen 252 * 24 * 4 = 24192 15-min bars/year)
  bars_per_year <- 252 * 24 * 4  # Für 15-min Bars
  sharpe_ratio <- ifelse(
    sd(predictions$strategy_return, na.rm = TRUE) > 0,
    mean(predictions$strategy_return, na.rm = TRUE) / sd(predictions$strategy_return, na.rm = TRUE) * sqrt(bars_per_year),
    NA
  )

  # Max Drawdown
  cum_max <- cummax(predictions$cum_strategy_return)
  drawdown <- predictions$cum_strategy_return - cum_max
  max_drawdown <- min(drawdown, na.rm = TRUE)

  metrics <- list(
    total_return = total_return,
    total_strategy_return = total_strategy_return,
    n_trades = n_trades,
    trade_frequency = n_trades / n_total,
    win_rate = win_rate,
    avg_win = avg_win,
    avg_loss = avg_loss,
    profit_factor = profit_factor,
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown
  )

  if (verbose) {
    cat(sprintf("  Total Return (Buy&Hold): %.4f\n", total_return))
    cat(sprintf("  Total Return (Strategy): %.4f\n", total_strategy_return))
    cat(sprintf("  # Trades: %d (%.1f%%)\n", n_trades, 100 * n_trades / n_total))
    cat(sprintf("  Win Rate: %.2f%%\n", 100 * win_rate))
    cat(sprintf("  Avg Win: %.4f, Avg Loss: %.4f\n", avg_win, avg_loss))
    cat(sprintf("  Profit Factor: %.2f\n", profit_factor))
    cat(sprintf("  Sharpe Ratio: %.2f\n", sharpe_ratio))
    cat(sprintf("  Max Drawdown: %.4f\n", max_drawdown))
  }

  return(metrics)
}


#' Create backtest visualization plots
create_backtest_plots <- function(predictions, output_path, verbose = TRUE) {

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  plots <- list()

  # ===== 1. Cumulative Returns =====
  if ("cum_return" %in% names(predictions)) {
    p1 <- ggplot(predictions, aes(x = datetime)) +
      geom_line(aes(y = cum_return, color = "Buy & Hold"), linewidth = 0.8) +
      geom_line(aes(y = cum_strategy_return, color = "Strategy"), linewidth = 0.8) +
      scale_color_manual(values = c("Buy & Hold" = "blue", "Strategy" = "green")) +
      labs(
        title = "Cumulative Returns: Strategy vs Buy & Hold",
        x = "Time",
        y = "Cumulative Return",
        color = ""
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    plot_file <- file.path(output_path, "cumulative_returns.png")
    ggsave(plot_file, p1, width = 12, height = 6)
    plots$cumulative_returns <- p1

    if (verbose) cat(sprintf("  ✓ Plot gespeichert: %s\n", plot_file))
  }

  # ===== 2. Prediction Distribution =====
  p2 <- ggplot(predictions, aes(x = pred_proba, fill = as.factor(actual))) +
    geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
    scale_fill_manual(values = c("0" = "red", "1" = "green"),
                     labels = c("0" = "Negative", "1" = "Positive")) +
    labs(
      title = "Prediction Probability Distribution",
      x = "Predicted Probability",
      y = "Count",
      fill = "Actual Label"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  plot_file <- file.path(output_path, "prediction_distribution.png")
  ggsave(plot_file, p2, width = 10, height = 6)
  plots$pred_distribution <- p2

  if (verbose) cat(sprintf("  ✓ Plot gespeichert: %s\n", plot_file))

  # ===== 3. Prediction Over Time =====
  # Zeige Predictions über Zeit (geglättet)
  predictions[, pred_smooth := frollmean(pred_proba, n = 100, align = "center")]

  p3 <- ggplot(predictions, aes(x = datetime, y = pred_smooth)) +
    geom_line(color = "blue", alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    labs(
      title = "Model Predictions Over Time (Smoothed)",
      x = "Time",
      y = "Predicted Probability (Smoothed 100-bar MA)"
    ) +
    theme_minimal()

  plot_file <- file.path(output_path, "predictions_over_time.png")
  ggsave(plot_file, p3, width = 12, height = 6)
  plots$pred_over_time <- p3

  if (verbose) cat(sprintf("  ✓ Plot gespeichert: %s\n", plot_file))

  # ===== 4. Confusion Matrix Heatmap =====
  # Nutze die berechnete Confusion Matrix
  cm_data <- as.data.table(table(
    Actual = predictions$actual_binary,
    Predicted = ifelse(predictions$pred_binary == -1, 0, predictions$pred_binary)
  ))

  p4 <- ggplot(cm_data, aes(x = Predicted, y = Actual, fill = N)) +
    geom_tile(color = "white") +
    geom_text(aes(label = N), color = "white", size = 6) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      title = "Confusion Matrix",
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  plot_file <- file.path(output_path, "confusion_matrix.png")
  ggsave(plot_file, p4, width = 8, height = 6)
  plots$confusion_matrix <- p4

  if (verbose) cat(sprintf("  ✓ Plot gespeichert: %s\n", plot_file))

  return(plots)
}


#' Generate summary report text
generate_summary_report <- function(class_metrics, returns_metrics, predictions) {

  report <- c(
    "========================================",
    "    BACKTEST SUMMARY REPORT",
    "========================================",
    "",
    "--- CLASSIFICATION METRICS ---",
    sprintf("AUC:          %.4f", class_metrics$auc),
    sprintf("Accuracy:     %.4f", class_metrics$accuracy),
    sprintf("Precision:    %.4f", class_metrics$precision),
    sprintf("Recall:       %.4f", class_metrics$recall),
    sprintf("F1 Score:     %.4f", class_metrics$f1),
    sprintf("Log Loss:     %.4f", class_metrics$logloss),
    "",
    "Confusion Matrix:",
    capture.output(print(class_metrics$confusion_matrix))
  )

  if (!is.null(returns_metrics)) {
    report <- c(
      report,
      "",
      "--- RETURNS METRICS ---",
      sprintf("Total Return (B&H):      %.4f", returns_metrics$total_return),
      sprintf("Total Return (Strategy): %.4f", returns_metrics$total_strategy_return),
      sprintf("Number of Trades:        %d (%.1f%%)",
              returns_metrics$n_trades,
              100 * returns_metrics$trade_frequency),
      sprintf("Win Rate:                %.2f%%", 100 * returns_metrics$win_rate),
      sprintf("Average Win:             %.4f", returns_metrics$avg_win),
      sprintf("Average Loss:            %.4f", returns_metrics$avg_loss),
      sprintf("Profit Factor:           %.2f", returns_metrics$profit_factor),
      sprintf("Sharpe Ratio:            %.2f", returns_metrics$sharpe_ratio),
      sprintf("Max Drawdown:            %.4f", returns_metrics$max_drawdown)
    )
  }

  report <- c(
    report,
    "",
    "--- DATA INFO ---",
    sprintf("Total Samples:     %d", nrow(predictions)),
    sprintf("Date Range:        %s to %s",
            min(predictions$datetime),
            max(predictions$datetime)),
    "",
    "========================================"
  )

  return(paste(report, collapse = "\n"))
}


#' Plot feature importance comparison (MDI, MDA, SFI)
plot_feature_importance <- function(importance_results, output_path, top_n = 15) {

  # Kombiniere alle Importance Metriken
  plot_data <- data.table()

  for (method in names(importance_results)) {
    imp_dt <- importance_results[[method]]

    # Top N Features
    top_features <- head(imp_dt, top_n)

    top_features[, method := method]
    plot_data <- rbind(plot_data, top_features[, .(feature, importance, method)])
  }

  # Normalisiere Importance pro Methode (0-1)
  plot_data[, importance_norm := importance / max(importance), by = method]

  # Plot
  p <- ggplot(plot_data, aes(x = reorder(feature, importance_norm), y = importance_norm, fill = method)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    scale_fill_manual(values = c("MDI" = "#1f77b4", "MDA" = "#ff7f0e", "SFI" = "#2ca02c")) +
    labs(
      title = sprintf("Feature Importance Comparison (Top %d)", top_n),
      x = "Feature",
      y = "Normalized Importance",
      fill = "Method"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  plot_file <- file.path(output_path, "feature_importance_comparison.png")
  ggsave(plot_file, p, width = 10, height = 8)

  cat(sprintf("Feature Importance Plot gespeichert: %s\n", plot_file))

  return(p)
}
