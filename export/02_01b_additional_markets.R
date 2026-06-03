# Additional Markets Module
# Lädt und verarbeitet zusätzliche Marktdaten (DXY, VIX, SILVER)
# Berechnet technische Indikatoren und erstellt Lag-Features

#' Load and process additional market data
#'
#' @param markets Vector of market symbols (e.g., c("DXY", "VIX", "SILVER"))
#' @param interval Time interval (e.g., "MINUTE_15")
#' @param price_data_path Path to price data folder
#' @param lag_periods Vector of lag periods for features (default: c(1, 4, 8, 20))
#' @param verbose Print progress messages
#'
#' @return data.table with datetime and all additional market features
load_additional_markets <- function(
    markets = c("DXY", "VIX", "SILVER"),
    interval = "MINUTE_15",
    price_data_path = "price_data",
    lag_periods = c(1, 4, 8, 20),
    verbose = TRUE
) {

  if (verbose) cat("\n=== LOADING ADDITIONAL MARKETS ===\n")

  # Liste für alle Market-DataTables
  market_features_list <- list()

  for (market in markets) {
    if (verbose) cat(sprintf("\n--- Processing %s ---\n", market))

    # Lade Price Data
    price_file <- file.path(price_data_path, paste0(market, "_", interval, ".csv"))

    if (!file.exists(price_file)) {
      if (verbose) cat(sprintf("  WARNING: File not found: %s (skipping)\n", price_file))
      next
    }

    if (verbose) cat(sprintf("  Loading: %s\n", price_file))
    dt_market <- fread(price_file)
    setDT(dt_market)

    # Rename 'time' to 'datetime' if necessary
    if ("time" %in% names(dt_market)) {
      setnames(dt_market, "time", "datetime")
    }

    # Convert datetime to POSIXct if character
    if (is.character(dt_market$datetime)) {
      dt_market[, datetime := as.POSIXct(datetime, tz = "UTC")]
    }

    if (verbose) cat(sprintf("  Rows: %s\n", format(nrow(dt_market), big.mark = ",")))

    # Berechne technische Indikatoren
    if (verbose) cat("  Calculating indicators...\n")
    dt_market <- calculate_market_indicators(
      dt = dt_market,
      market_prefix = market,
      verbose = FALSE
    )

    # Erstelle Lag Features
    if (verbose) cat("  Creating lag features...\n")
    dt_market <- create_market_lags(
      dt = dt_market,
      market_prefix = market,
      lag_periods = lag_periods,
      verbose = FALSE
    )

    # Wähle nur datetime und die neuen Features
    feature_cols <- setdiff(names(dt_market), c("open", "high", "low", "close", "volume"))
    dt_market_features <- dt_market[, ..feature_cols]

    if (verbose) cat(sprintf("  ✓ %d features created for %s\n",
                            ncol(dt_market_features) - 1, market))  # -1 for datetime

    market_features_list[[market]] <- dt_market_features
  }

  # Merge alle Markets zusammen (auf datetime)
  if (length(market_features_list) == 0) {
    if (verbose) cat("\nWARNING: No markets were loaded!\n")
    return(NULL)
  }

  if (verbose) cat("\n--- Merging all markets ---\n")

  # Start mit erstem Market
  dt_all_markets <- market_features_list[[1]]

  # Merge alle weiteren Markets
  if (length(market_features_list) > 1) {
    for (i in 2:length(market_features_list)) {
      dt_all_markets <- merge(
        dt_all_markets,
        market_features_list[[i]],
        by = "datetime",
        all = FALSE  # Inner join - nur gemeinsame Zeitpunkte
      )
    }
  }

  if (verbose) {
    cat(sprintf("✓ Additional markets merged: %s rows, %d features\n",
                format(nrow(dt_all_markets), big.mark = ","),
                ncol(dt_all_markets) - 1))  # -1 for datetime
    cat(sprintf("  Markets: %s\n", paste(markets, collapse = ", ")))
  }

  return(dt_all_markets)
}


#' Calculate technical indicators for additional market
#'
#' @param dt data.table with OHLCV data
#' @param market_prefix Prefix for column names (e.g., "DXY", "VIX")
#' @param verbose Print progress messages
#'
#' @return data.table with original columns plus indicators
calculate_market_indicators <- function(
    dt,
    market_prefix,
    verbose = TRUE
) {

  dt_ind <- copy(dt)

  # Prefix für alle neuen Spalten
  pfx <- paste0(market_prefix, "_")

  # --- 1. Close Price ---
  # Behalte close für Lags, aber benenne es um
  setnames(dt_ind, "close", paste0(pfx, "close"))

  # --- 2. Volume Features ---
  if (verbose) cat(sprintf("  - %sVolume Indicators\n", pfx))

  # Behalte Volume und benenne um
  setnames(dt_ind, "volume", paste0(pfx, "volume"))

  # Volume SMA (20 periods)
  dt_ind[, (paste0(pfx, "volume_sma_20")) := SMA(get(paste0(pfx, "volume")), n = 20)]

  # Volume Ratio (current volume / SMA)
  dt_ind[, (paste0(pfx, "volume_ratio")) :=
           get(paste0(pfx, "volume")) / (get(paste0(pfx, "volume_sma_20")) + 1e-10)]

  # Volume ROC (Rate of Change)
  dt_ind[, (paste0(pfx, "volume_roc_5")) := ROC(get(paste0(pfx, "volume")), n = 5)]
  dt_ind[, (paste0(pfx, "volume_roc_10")) := ROC(get(paste0(pfx, "volume")), n = 10)]

  # On Balance Volume (OBV)
  dt_ind[, close_prev_temp := data.table::shift(get(paste0(pfx, "close")), 1)]
  dt_ind[, obv_change_temp := fifelse(is.na(close_prev_temp), 0,
                                       fifelse(get(paste0(pfx, "close")) > close_prev_temp,
                                               get(paste0(pfx, "volume")),
                                               -get(paste0(pfx, "volume"))))]
  dt_ind[, (paste0(pfx, "obv")) := cumsum(obv_change_temp)]
  dt_ind[, c("close_prev_temp", "obv_change_temp") := NULL]

  # Volume Price Trend (VPT)
  dt_ind[, close_prev_temp := data.table::shift(get(paste0(pfx, "close")), 1)]
  dt_ind[, vpt_change_temp := fifelse(is.na(close_prev_temp), 0,
                                       get(paste0(pfx, "volume")) *
                                       (get(paste0(pfx, "close")) - close_prev_temp) /
                                       (close_prev_temp + 1e-10))]
  dt_ind[, (paste0(pfx, "vpt")) := cumsum(vpt_change_temp)]
  dt_ind[, c("close_prev_temp", "vpt_change_temp") := NULL]

  # --- 3. ATR 14 ---
  if (verbose) cat(sprintf("  - %sATR_14\n", pfx))
  atr_14 <- with(dt_ind, ATR(cbind(high, low, get(paste0(pfx, "close"))), n = 14)[, "atr"])
  dt_ind[, (paste0(pfx, "atr_14")) := atr_14]

  # --- 4. RSI 14 ---
  if (verbose) cat(sprintf("  - %sRSI_14\n", pfx))
  rsi_14 <- RSI(dt_ind[[paste0(pfx, "close")]], n = 14)
  dt_ind[, (paste0(pfx, "rsi_14")) := rsi_14]

  # --- 5. MACD ---
  if (verbose) cat(sprintf("  - %sMACD\n", pfx))
  macd_result <- MACD(dt_ind[[paste0(pfx, "close")]],
                      nFast = 12, nSlow = 26, nSig = 9)
  dt_ind[, (paste0(pfx, "macd")) := macd_result[, "macd"]]
  dt_ind[, (paste0(pfx, "macd_signal")) := macd_result[, "signal"]]
  dt_ind[, (paste0(pfx, "macd_diff")) := macd_result[, "macd"] - macd_result[, "signal"]]

  # --- 6. ADX 14 ---
  if (verbose) cat(sprintf("  - %sADX_14\n", pfx))
  adx_result <- with(dt_ind, ADX(cbind(high, low, get(paste0(pfx, "close"))), n = 14))
  dt_ind[, (paste0(pfx, "adx_14")) := adx_result[, "ADX"]]
  dt_ind[, (paste0(pfx, "di_plus_14")) := adx_result[, "DIp"]]
  dt_ind[, (paste0(pfx, "di_minus_14")) := adx_result[, "DIn"]]

  # Lösche OHLV (behalten nur die Indikatoren und Volume-Features)
  dt_ind[, c("open", "high", "low") := NULL]

  return(dt_ind)
}


#' Create lag features for additional market
#'
#' @param dt data.table with indicators
#' @param market_prefix Prefix for column names (e.g., "DXY", "VIX")
#' @param lag_periods Vector of lag periods (default: c(1, 4, 8, 20))
#' @param verbose Print progress messages
#'
#' @return data.table with lag features
create_market_lags <- function(
    dt,
    market_prefix,
    lag_periods = c(1, 4, 8, 20),
    verbose = TRUE
) {

  dt_lags <- copy(dt)

  # Finde alle Feature-Spalten (außer datetime)
  pfx <- paste0(market_prefix, "_")
  feature_cols <- grep(paste0("^", pfx), names(dt_lags), value = TRUE)

  if (verbose) {
    cat(sprintf("Creating lags for %d features with periods: %s\n",
                length(feature_cols),
                paste(lag_periods, collapse = ", ")))
  }

  # Erstelle Lags für alle Features
  for (col in feature_cols) {
    for (lag in lag_periods) {
      lag_col_name <- paste0(col, "_lag", lag)
      dt_lags[, (lag_col_name) := data.table::shift(get(col), n = lag)]
    }
  }

  if (verbose) {
    cat(sprintf("✓ Created %d lag features\n",
                length(feature_cols) * length(lag_periods)))
  }

  return(dt_lags)
}


#' Merge additional market features with main dataset
#'
#' @param dt_main Main dataset with datetime column
#' @param dt_additional Additional market features with datetime column
#' @param verbose Print progress messages
#'
#' @return Merged data.table
merge_additional_markets <- function(
    dt_main,
    dt_additional,
    verbose = TRUE
) {

  if (is.null(dt_additional)) {
    if (verbose) cat("No additional markets to merge.\n")
    return(dt_main)
  }

  if (verbose) {
    cat("\n=== MERGING ADDITIONAL MARKETS WITH MAIN DATASET ===\n")
    cat(sprintf("Main dataset: %s rows\n", format(nrow(dt_main), big.mark = ",")))
    cat(sprintf("Additional markets: %s rows, %d features\n",
                format(nrow(dt_additional), big.mark = ","),
                ncol(dt_additional) - 1))
  }

  # Merge (inner join - nur gemeinsame Zeitpunkte)
  dt_merged <- merge(
    dt_main,
    dt_additional,
    by = "datetime",
    all.x = TRUE  # Left join - behalte alle Main-Daten
  )

  if (verbose) {
    cat(sprintf("✓ Merged: %s rows, %d total columns\n",
                format(nrow(dt_merged), big.mark = ","),
                ncol(dt_merged)))

    # Check für NAs in zusätzlichen Features
    additional_cols <- setdiff(names(dt_additional), "datetime")
    na_count <- sum(is.na(dt_merged[[additional_cols[1]]]))
    if (na_count > 0) {
      cat(sprintf("  WARNING: %s rows have NA values in additional markets\n",
                  format(na_count, big.mark = ",")))
      cat("  (This is normal if additional markets have different date ranges)\n")
    }
  }

  return(dt_merged)
}
