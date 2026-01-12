# Test Script for Additional Markets Module
# Testet das Laden und Verarbeiten von zusätzlichen Marktdaten

cat("\n=== TEST: ADDITIONAL MARKETS MODULE ===\n")

rm(list=ls())
gc()

# ===== PACKAGES ==============================================================

pacman::p_load(
  data.table,
  TTR,
  zoo
)

# ===== LOAD MODULES ==========================================================

source("r/02_01_indicator_calculation.R")
cat("✓ Indicator Calculation loaded\n")
source("r/02_01b_additional_markets.R")
cat("✓ Additional Markets module loaded\n")

# ===== CONFIGURATION =========================================================

INTERVAL <- "MINUTE_15"
ADDITIONAL_MARKETS <- c("DXY", "VIX", "SILVER")
ADDITIONAL_MARKETS_LAG_PERIODS <- c(1, 4, 8, 20)
price_data_path <- "price_data"

cat("\nTest Configuration:\n")
cat(sprintf("  Interval: %s\n", INTERVAL))
cat(sprintf("  Markets: %s\n", paste(ADDITIONAL_MARKETS, collapse = ", ")))
cat(sprintf("  Lag Periods: %s\n", paste(ADDITIONAL_MARKETS_LAG_PERIODS, collapse = ", ")))

# ===== TEST 1: LOAD ADDITIONAL MARKETS =======================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("TEST 1: LOAD ADDITIONAL MARKETS\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

dt_additional <- load_additional_markets(
  markets = ADDITIONAL_MARKETS,
  interval = INTERVAL,
  price_data_path = price_data_path,
  lag_periods = ADDITIONAL_MARKETS_LAG_PERIODS,
  verbose = TRUE
)

# ===== TEST 2: INSPECT RESULTS ===============================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("TEST 2: INSPECT RESULTS\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

if (!is.null(dt_additional)) {
  cat(sprintf("\n✓ Additional markets loaded successfully\n"))
  cat(sprintf("  Rows: %s\n", format(nrow(dt_additional), big.mark = ",")))
  cat(sprintf("  Columns: %d\n", ncol(dt_additional)))

  cat("\nColumn names (first 20):\n")
  print(head(names(dt_additional), 20))

  cat("\nSample data (first 5 rows, first 10 columns):\n")
  print(head(dt_additional[, 1:min(10, ncol(dt_additional))], 5))

  cat("\nColumn summary by market:\n")
  for (market in ADDITIONAL_MARKETS) {
    market_cols <- grep(paste0("^", market, "_"), names(dt_additional), value = TRUE)
    cat(sprintf("  %s: %d features\n", market, length(market_cols)))
  }

  # Check for NAs
  cat("\nNA check (first 10 columns):\n")
  cols_to_check <- setdiff(names(dt_additional), "datetime")[1:min(10, ncol(dt_additional)-1)]
  for (col in cols_to_check) {
    na_count <- sum(is.na(dt_additional[[col]]))
    na_pct <- 100 * na_count / nrow(dt_additional)
    cat(sprintf("  %s: %d NAs (%.2f%%)\n", col, na_count, na_pct))
  }

} else {
  cat("\n✗ ERROR: No additional markets loaded!\n")
}

# ===== TEST 3: TEST MERGE WITH MAIN DATA =====================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("TEST 3: TEST MERGE WITH MAIN DATA\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

# Load GOLD data as main dataset
cat("\nLoading GOLD data as main dataset...\n")
prices_file <- file.path(price_data_path, "GOLD_MINUTE_15.csv")
dt_gold <- fread(prices_file)
setDT(dt_gold)

if ("time" %in% names(dt_gold)) {
  setnames(dt_gold, "time", "datetime")
}
if (is.character(dt_gold$datetime)) {
  dt_gold[, datetime := as.POSIXct(datetime, tz = "UTC")]
}

cat(sprintf("  GOLD data: %s rows\n", format(nrow(dt_gold), big.mark = ",")))

# Simulate a simple main dataset (just datetime and close)
dt_main_test <- dt_gold[, .(datetime, close)]
cat(sprintf("  Test main dataset: %s rows, %d columns\n",
            format(nrow(dt_main_test), big.mark = ","),
            ncol(dt_main_test)))

# Merge
cat("\nMerging additional markets with main dataset...\n")
dt_merged_test <- merge_additional_markets(
  dt_main = dt_main_test,
  dt_additional = dt_additional,
  verbose = TRUE
)

cat("\n✓ Merge test completed\n")
cat(sprintf("  Final dataset: %s rows, %d columns\n",
            format(nrow(dt_merged_test), big.mark = ","),
            ncol(dt_merged_test)))

# ===== TEST 4: VERIFY FEATURE NAMING =========================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("TEST 4: VERIFY FEATURE NAMING\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

cat("\nExpected features per market:\n")
cat("  - {MARKET}_close (+ lag1, lag4, lag8, lag20)\n")
cat("  - {MARKET}_atr_14 (+ lag1, lag4, lag8, lag20)\n")
cat("  - {MARKET}_rsi_14 (+ lag1, lag4, lag8, lag20)\n")
cat("  - {MARKET}_macd, {MARKET}_macd_signal, {MARKET}_macd_diff (each + lags)\n")
cat("  - {MARKET}_adx_14, {MARKET}_di_plus_14, {MARKET}_di_minus_14 (each + lags)\n")

expected_features_per_market <- (
  5 +  # close, atr_14, rsi_14, macd, adx_14 (base)
  3 +  # macd_signal, macd_diff, di_plus_14, di_minus_14 (extra base)
  8 * 4  # 8 base features * 4 lags each
)

cat(sprintf("\nExpected total features per market: ~%d\n", expected_features_per_market))

for (market in ADDITIONAL_MARKETS) {
  market_cols <- grep(paste0("^", market, "_"), names(dt_merged_test), value = TRUE)
  cat(sprintf("\n%s features (%d total):\n", market, length(market_cols)))

  # Zeige Beispiele
  cat("  Sample features:\n")
  print(head(market_cols, 15))

  # Check ob Lags vorhanden sind
  lag_cols <- grep("_lag", market_cols, value = TRUE)
  cat(sprintf("  Lag features: %d\n", length(lag_cols)))

  # Check ob alle erwarteten Base-Features vorhanden sind
  expected_base <- c(
    paste0(market, "_close"),
    paste0(market, "_atr_14"),
    paste0(market, "_rsi_14"),
    paste0(market, "_macd"),
    paste0(market, "_adx_14")
  )

  for (base_feat in expected_base) {
    if (base_feat %in% market_cols) {
      cat(sprintf("  ✓ %s found\n", base_feat))
    } else {
      cat(sprintf("  ✗ %s MISSING\n", base_feat))
    }
  }
}

# ===== FINAL SUMMARY =========================================================

cat("\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")
cat("TEST SUMMARY\n")
cat(paste0(rep("=", 80), collapse = ""))
cat("\n")

if (!is.null(dt_additional) && nrow(dt_additional) > 0) {
  cat("\n✓ ALL TESTS PASSED\n")
  cat("\nAdditional Markets Module is ready to use!\n")
  cat("\nNext steps:\n")
  cat("  1. Set ADDITIONAL_MARKETS in 02_backtest_main_script_ls_v2.R\n")
  cat("  2. Set FORCE_RECALCULATE_FEATURES = TRUE to rebuild feature cache\n")
  cat("  3. Run the full backtest pipeline\n")
} else {
  cat("\n✗ TESTS FAILED\n")
  cat("Check the error messages above.\n")
}

cat("\n=== TEST COMPLETE ===\n")
