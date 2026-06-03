rm(list = ls())
gc()

# Perfect Setup Backtest - 90/180 Day Performance vs Benchmarks
# ============================================================

pacman::p_load(
    quantmod,
    TTR,
    data.table,
    ggplot2,
    parallel,
    zoo,
    lubridate,
    tidyquant,
    httr,
    jsonlite
)

options(scipen=999)

# ===== CONFIGURATION =====

source("other_code/yahoo_ticker_converter.R")

#Load WATCHLIST from Trading View
watchlist = load_and_convert_watchlist("other_code/watchlist/Europe Alert Watchlist_2026-01-22.csv")

benchmark_symbols <- c("^GSPC", "URTH")  # S&P 500 and MSCI World ETF
start_date <- "2016-01-01"
end_date <- Sys.Date()

#sort by market.capitalization descending
watchlist = as.data.table(watchlist)
watchlist = watchlist[order(-Market.capitalization)]

symbols = watchlist$Yahoo_Ticker[1:50]

# ===== FUNDAMENTAL DATA FUNCTIONS =====

# Fetch quarterly financial data from Yahoo Finance API
fetch_fundamentals_yahoo <- function(symbol) {
  tryCatch({
    # Yahoo Finance API - Income Statement + Balance Sheet + Key Stats
    modules <- paste0("incomeStatementHistoryQuarterly,",
                      "balanceSheetHistoryQuarterly,",
                      "earningsTrend,defaultKeyStatistics,",
                      "financialData")

    url <- paste0("https://query2.finance.yahoo.com/v10/finance/quoteSummary/",
                  symbol, "?modules=", modules)

    resp <- GET(url, add_headers("User-Agent" = "Mozilla/5.0"))

    if (status_code(resp) != 200) return(NULL)

    json <- content(resp, as = "text", encoding = "UTF-8")
    parsed <- fromJSON(json, flatten = TRUE)

    result <- parsed$quoteSummary$result
    if (is.null(result) || length(result) == 0) return(NULL)

    fundamentals <- list()

    # --- Income Statement (Quarterly) ---
    is_data <- result$incomeStatementHistoryQuarterly.incomeStatementHistory[[1]]
    if (!is.null(is_data) && nrow(is_data) > 0) {
      is_dt <- data.table(
        date = as.Date(is_data$endDate.fmt),
        revenue = as.numeric(is_data$totalRevenue.raw),
        gross_profit = as.numeric(is_data$grossProfit.raw),
        net_income = as.numeric(is_data$netIncome.raw),
        ebit = as.numeric(is_data$ebit.raw),
        operating_income = as.numeric(is_data$operatingIncome.raw)
      )
      fundamentals$income <- is_dt
    }

    # --- Balance Sheet (Quarterly) ---
    bs_data <- result$balanceSheetHistoryQuarterly.balanceSheetStatements[[1]]
    if (!is.null(bs_data) && nrow(bs_data) > 0) {
      bs_dt <- data.table(
        date = as.Date(bs_data$endDate.fmt),
        total_assets = as.numeric(bs_data$totalAssets.raw),
        total_liabilities = as.numeric(bs_data$totalLiab.raw),
        total_equity = as.numeric(bs_data$totalStockholderEquity.raw),
        cash = as.numeric(bs_data$cash.raw),
        total_debt = as.numeric(bs_data$longTermDebt.raw)
      )
      fundamentals$balance <- bs_dt
    }

    # --- Key Statistics (current snapshot - used as fallback) ---
    key_stats <- result$defaultKeyStatistics
    fin_data <- result$financialData

    if (!is.null(key_stats) || !is.null(fin_data)) {
      fundamentals$current <- list(
        trailing_pe = as.numeric(key_stats$trailingEps.raw),
        forward_pe = as.numeric(key_stats$forwardEps.raw),
        peg_ratio = as.numeric(key_stats$pegRatio.raw),
        price_to_book = as.numeric(key_stats$priceToBook.raw),
        enterprise_value = as.numeric(key_stats$enterpriseValue.raw),
        profit_margin = as.numeric(fin_data$profitMargins.raw),
        operating_margin = as.numeric(fin_data$operatingMargins.raw),
        roe = as.numeric(fin_data$returnOnEquity.raw),
        roa = as.numeric(fin_data$returnOnAssets.raw),
        revenue_growth = as.numeric(fin_data$revenueGrowth.raw),
        earnings_growth = as.numeric(fin_data$earningsGrowth.raw),
        current_ratio = as.numeric(fin_data$currentRatio.raw),
        debt_to_equity = as.numeric(fin_data$debtToEquity.raw)
      )
    }

    return(fundamentals)

  }, error = function(e) {
    cat("    Fundamental data error for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

# Get fundamental metrics at a specific signal date (last known before signal)
get_fundamentals_at_signal <- function(fundamentals, signal_date, entry_price) {
  if (is.null(fundamentals)) {
    return(list(
      revenue = NA, gross_profit = NA, net_income = NA, ebit = NA,
      operating_income = NA, total_assets = NA, total_equity = NA,
      cash = NA, total_debt = NA, gross_margin = NA, net_margin = NA,
      debt_to_equity = NA, roe = NA, revenue_growth_qoq = NA,
      earnings_growth_qoq = NA, pe_ratio_approx = NA
    ))
  }

  result <- list()

  # --- Income Statement: letztes Quartal VOR Signal ---
  if (!is.null(fundamentals$income)) {
    is_dt <- fundamentals$income[date <= signal_date][order(-date)]

    if (nrow(is_dt) >= 1) {
      latest <- is_dt[1]
      result$revenue <- latest$revenue
      result$gross_profit <- latest$gross_profit
      result$net_income <- latest$net_income
      result$ebit <- latest$ebit
      result$operating_income <- latest$operating_income

      # Margins
      result$gross_margin <- ifelse(!is.na(latest$revenue) & latest$revenue != 0,
                                    latest$gross_profit / latest$revenue, NA)
      result$net_margin <- ifelse(!is.na(latest$revenue) & latest$revenue != 0,
                                  latest$net_income / latest$revenue, NA)

      # QoQ Growth (wenn 2 Quartale verfuegbar)
      if (nrow(is_dt) >= 2) {
        prev <- is_dt[2]
        result$revenue_growth_qoq <- ifelse(!is.na(prev$revenue) & prev$revenue != 0,
                                            (latest$revenue - prev$revenue) / abs(prev$revenue), NA)
        result$earnings_growth_qoq <- ifelse(!is.na(prev$net_income) & prev$net_income != 0,
                                             (latest$net_income - prev$net_income) / abs(prev$net_income), NA)
      } else {
        result$revenue_growth_qoq <- NA
        result$earnings_growth_qoq <- NA
      }
    } else {
      result$revenue <- NA; result$gross_profit <- NA; result$net_income <- NA
      result$ebit <- NA; result$operating_income <- NA
      result$gross_margin <- NA; result$net_margin <- NA
      result$revenue_growth_qoq <- NA; result$earnings_growth_qoq <- NA
    }
  } else {
    result$revenue <- NA; result$gross_profit <- NA; result$net_income <- NA
    result$ebit <- NA; result$operating_income <- NA
    result$gross_margin <- NA; result$net_margin <- NA
    result$revenue_growth_qoq <- NA; result$earnings_growth_qoq <- NA
  }

  # --- Balance Sheet: letztes Quartal VOR Signal ---
  if (!is.null(fundamentals$balance)) {
    bs_dt <- fundamentals$balance[date <= signal_date][order(-date)]

    if (nrow(bs_dt) >= 1) {
      latest_bs <- bs_dt[1]
      result$total_assets <- latest_bs$total_assets
      result$total_equity <- latest_bs$total_equity
      result$cash <- latest_bs$cash
      result$total_debt <- latest_bs$total_debt

      # Ratios
      result$debt_to_equity <- ifelse(!is.na(latest_bs$total_equity) & latest_bs$total_equity != 0,
                                      latest_bs$total_debt / latest_bs$total_equity, NA)

      # ROE (annualized from quarterly net income)
      if (!is.na(result$net_income) && !is.na(latest_bs$total_equity) && latest_bs$total_equity != 0) {
        result$roe <- (result$net_income * 4) / latest_bs$total_equity
      } else {
        result$roe <- NA
      }
    } else {
      result$total_assets <- NA; result$total_equity <- NA
      result$cash <- NA; result$total_debt <- NA
      result$debt_to_equity <- NA; result$roe <- NA
    }
  } else {
    result$total_assets <- NA; result$total_equity <- NA
    result$cash <- NA; result$total_debt <- NA
    result$debt_to_equity <- NA; result$roe <- NA
  }

  # --- Approximiertes P/E Ratio ---
  # Annualisiertes EPS aus letztem Quartal, geteilt durch Entry Price
  if (!is.na(result$net_income) && !is.na(result$total_equity)) {
    # Vereinfachtes P/E: entry_price / (annualized EPS)
    # Da wir keine Shares Outstanding haben, nutzen wir current stats als Fallback
    result$pe_ratio_approx <- NA
  } else {
    result$pe_ratio_approx <- NA
  }

  return(result)
}

# ===== HELPER FUNCTIONS =====

# Calculate Perfect Setup conditions and additional indicators
calculate_perfect_setup <- function(data) {

  # Moving Averages
  data$MA20 <- SMA(Cl(data), n = 20)
  data$MA50 <- SMA(Cl(data), n = 50)
  data$MA200 <- SMA(Cl(data), n = 200)

  # RSI
  data$RSI <- RSI(Cl(data), n = 14)

  # ADX and DMI
  adx_data <- ADX(HLC(data), n = 14)
  data$ADX <- adx_data[, "ADX"]
  data$DIplus <- adx_data[, "DIp"]
  data$DIminus <- adx_data[, "DIn"]

  # Volume
  data$Volume_MA20 <- SMA(Vo(data), n = 20)

  # ===== ADDITIONAL INDICATORS =====

  # MACD
  macd_data <- MACD(Cl(data), nFast = 12, nSlow = 26, nSig = 9)
  data$MACD <- macd_data[, "macd"]
  data$MACD_signal <- macd_data[, "signal"]
  data$MACD_hist <- data$MACD - data$MACD_signal

  # Bollinger Bands
  bb_data <- BBands(Cl(data), n = 20, sd = 2)
  data$BB_upper <- bb_data[, "up"]
  data$BB_lower <- bb_data[, "dn"]
  data$BB_mid <- bb_data[, "mavg"]
  data$BB_pct <- (Cl(data) - data$BB_lower) / (data$BB_upper - data$BB_lower)

  # ATR (Average True Range) - Volatility
  data$ATR <- ATR(HLC(data), n = 14)[, "atr"]
  data$ATR_pct <- data$ATR / Cl(data) * 100  # ATR as % of price

  # Stochastic
  stoch_data <- stoch(HLC(data), nFastK = 14, nFastD = 3, nSlowD = 3)
  data$Stoch_K <- stoch_data[, "fastK"] * 100
  data$Stoch_D <- stoch_data[, "fastD"] * 100

  # CCI (Commodity Channel Index)
  data$CCI <- CCI(HLC(data), n = 20)

  # OBV (On Balance Volume)
  data$OBV <- OBV(Cl(data), Vo(data))

  # Volume ratio
  data$Volume_ratio <- Vo(data) / data$Volume_MA20

  # Price relative to MAs (% distance)
  data$pct_from_MA20 <- (Cl(data) - data$MA20) / data$MA20 * 100
  data$pct_from_MA50 <- (Cl(data) - data$MA50) / data$MA50 * 100
  data$pct_from_MA200 <- (Cl(data) - data$MA200) / data$MA200 * 100

  # 52-week high/low
  data$high_52w <- runMax(Hi(data), n = 252)
  data$low_52w <- runMin(Lo(data), n = 252)
  data$pct_from_52w_high <- (Cl(data) - data$high_52w) / data$high_52w * 100
  data$pct_from_52w_low <- (Cl(data) - data$low_52w) / data$low_52w * 100

  # Calculate conditions
  data$uptrend <- data$MA20 > data$MA50 & data$MA50 > data$MA200
  data$golden_cross <- data$MA50 > data$MA200

  # Pullback detection
  data$high_20 <- runMax(Hi(data), n = 20)
  data$high_distance <- (data$high_20 - Cl(data)) / Cl(data)
  data$healthy_pullback <- data$high_distance > 0.02 & data$high_distance < 0.12
  data$near_ma50 <- abs(Cl(data) - data$MA50) / data$MA50 < 0.05
  data$near_ma20 <- abs(Cl(data) - data$MA20) / data$MA20 < 0.04
  data$pullback <- Cl(data) < data$MA20 & Cl(data) > data$MA50
  data$any_pullback <- data$pullback | data$healthy_pullback | data$near_ma50 | data$near_ma20

  data$price_above_ma50 <- Cl(data) >= data$MA50 * 0.96

  # RSI conditions
  data$rsi_optimal <- data$RSI >= 45 & data$RSI <= 65

  # Trend strength
  data$trend_very_strong <- data$ADX > 20 & data$DIplus > data$DIminus * 1.15

  # Volume
  data$volume_strong <- Vo(data) > data$Volume_MA20 * 1.15

  # Price action
  data$bullish_candle <- Cl(data) > Op(data)
  data$strong_bullish <- data$bullish_candle & (Cl(data) - Op(data)) / Op(data) > 0.008
  data$reasonable_price <- (Cl(data) - data$MA200) / data$MA200 < 0.25

  # PERFECT SETUP
  data$perfect_setup <- data$uptrend &
    data$any_pullback &
    data$price_above_ma50 &
    data$rsi_optimal &
    data$trend_very_strong &
    data$volume_strong &
    data$strong_bullish &
    data$reasonable_price

  # Replace NA with FALSE
  data$perfect_setup[is.na(data$perfect_setup)] <- FALSE

  return(data)
}

# Calculate forward returns
calculate_forward_returns <- function(prices, days) {
  # Convert to numeric vector if xts
  if (is.xts(prices) || is.zoo(prices)) {
    prices <- as.numeric(prices)
  }

  n <- length(prices)
  forward_returns <- rep(NA, n)

  for (i in 1:(n - days)) {
    if (!is.na(prices[i]) && !is.na(prices[i + days])) {
      forward_returns[i] <- (prices[i + days] - prices[i]) / prices[i]
    }
  }

  return(forward_returns)
}

# Calculate past returns (looking backwards)
calculate_past_returns <- function(prices, days) {
  if (is.xts(prices) || is.zoo(prices)) {
    prices <- as.numeric(prices)
  }

  n <- length(prices)
  past_returns <- rep(NA, n)

  for (i in (days + 1):n) {
    if (!is.na(prices[i]) && !is.na(prices[i - days])) {
      past_returns[i] <- (prices[i] - prices[i - days]) / prices[i - days]
    }
  }

  return(past_returns)
}

# ===== MAIN BACKTEST =====

cat("Starting Perfect Setup Backtest...\n")
cat("Loading data for", length(symbols), "symbols...\n\n")

# Load benchmark data
cat("Loading benchmark data...\n")
sp500 <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
msci_world <- getSymbols("URTH", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# Calculate benchmark forward returns
sp500_fwd_90d <- calculate_forward_returns(Cl(sp500), 90)
sp500_fwd_180d <- calculate_forward_returns(Cl(sp500), 180)
msci_fwd_90d <- calculate_forward_returns(Cl(msci_world), 90)
msci_fwd_180d <- calculate_forward_returns(Cl(msci_world), 180)

# Calculate benchmark past returns (for relative strength analysis)
sp500_past_30d <- calculate_past_returns(Cl(sp500), 30)
sp500_past_90d <- calculate_past_returns(Cl(sp500), 90)

# Results storage
results_list <- list()

# Process each symbol
for (sym in symbols) {
  
  cat("Processing", sym, "...\n")
  
  tryCatch({
    # Download data
    data <- getSymbols(sym, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    
    if (nrow(data) < 300) {
      cat("  Skipping", sym, "- insufficient data\n")
      next
    }
    
    # Calculate indicators and signals
    data <- calculate_perfect_setup(data)

    # Calculate forward returns
    data$return_90d <- calculate_forward_returns(Cl(data), 90)
    data$return_180d <- calculate_forward_returns(Cl(data), 180)

    # Calculate past returns
    data$past_return_30d <- calculate_past_returns(Cl(data), 30)
    data$past_return_90d <- calculate_past_returns(Cl(data), 90)

    # Find signal dates
    signal_indices <- which(data$perfect_setup == TRUE)

    if (length(signal_indices) == 0) {
      cat("  No Perfect Setup signals found for", sym, "\n")
      next
    }

    # Fetch fundamental data once per symbol
    cat("  Fetching fundamental data for", sym, "...\n")
    fund_data <- fetch_fundamentals_yahoo(sym)
    Sys.sleep(0.5)  # Rate limiting

    # Extract signal data
    for (idx in signal_indices) {
      signal_date <- index(data)[idx]

      # Get benchmark returns at signal date
      sp500_idx <- which(index(sp500) == signal_date)
      msci_idx <- which(index(msci_world) == signal_date)

      if (length(sp500_idx) == 0 || length(msci_idx) == 0) next

      # Get fundamentals at signal date (last known before signal)
      fund_at_signal <- get_fundamentals_at_signal(fund_data, signal_date,
                                                    as.numeric(Cl(data)[idx]))

      result <- data.table(
        symbol = sym,
        date = signal_date,
        entry_price = as.numeric(Cl(data)[idx]),

        # Forward returns
        return_90d = as.numeric(data$return_90d[idx]),
        return_180d = as.numeric(data$return_180d[idx]),

        # Benchmark forward returns
        sp500_fwd_90d = sp500_fwd_90d[sp500_idx[1]],
        sp500_fwd_180d = sp500_fwd_180d[sp500_idx[1]],
        msci_fwd_90d = msci_fwd_90d[msci_idx[1]],
        msci_fwd_180d = msci_fwd_180d[msci_idx[1]],

        # Past returns (momentum)
        past_return_30d = as.numeric(data$past_return_30d[idx]),
        past_return_90d = as.numeric(data$past_return_90d[idx]),

        # Benchmark past returns
        sp500_past_30d = sp500_past_30d[sp500_idx[1]],
        sp500_past_90d = sp500_past_90d[sp500_idx[1]],

        # Relative strength vs S&P500 (past performance)
        rel_strength_30d = as.numeric(data$past_return_30d[idx]) - sp500_past_30d[sp500_idx[1]],
        rel_strength_90d = as.numeric(data$past_return_90d[idx]) - sp500_past_90d[sp500_idx[1]],

        # === FUNDAMENTAL DATA ===
        fund_revenue = fund_at_signal$revenue,
        fund_gross_profit = fund_at_signal$gross_profit,
        fund_net_income = fund_at_signal$net_income,
        fund_ebit = fund_at_signal$ebit,
        fund_gross_margin = fund_at_signal$gross_margin,
        fund_net_margin = fund_at_signal$net_margin,
        fund_total_equity = fund_at_signal$total_equity,
        fund_total_debt = fund_at_signal$total_debt,
        fund_cash = fund_at_signal$cash,
        fund_debt_to_equity = fund_at_signal$debt_to_equity,
        fund_roe = fund_at_signal$roe,
        fund_revenue_growth_qoq = fund_at_signal$revenue_growth_qoq,
        fund_earnings_growth_qoq = fund_at_signal$earnings_growth_qoq,

        # Core indicators at signal
        rsi = as.numeric(data$RSI[idx]),
        adx = as.numeric(data$ADX[idx]),
        di_plus = as.numeric(data$DIplus[idx]),
        di_minus = as.numeric(data$DIminus[idx]),

        # MACD
        macd = as.numeric(data$MACD[idx]),
        macd_signal = as.numeric(data$MACD_signal[idx]),
        macd_hist = as.numeric(data$MACD_hist[idx]),

        # Bollinger Bands
        bb_pct = as.numeric(data$BB_pct[idx]),

        # Volatility
        atr_pct = as.numeric(data$ATR_pct[idx]),

        # Stochastic
        stoch_k = as.numeric(data$Stoch_K[idx]),
        stoch_d = as.numeric(data$Stoch_D[idx]),

        # CCI
        cci = as.numeric(data$CCI[idx]),

        # Volume
        volume_ratio = as.numeric(data$Volume_ratio[idx]),

        # Price position relative to MAs
        pct_from_ma20 = as.numeric(data$pct_from_MA20[idx]),
        pct_from_ma50 = as.numeric(data$pct_from_MA50[idx]),
        pct_from_ma200 = as.numeric(data$pct_from_MA200[idx]),

        # 52-week position
        pct_from_52w_high = as.numeric(data$pct_from_52w_high[idx]),
        pct_from_52w_low = as.numeric(data$pct_from_52w_low[idx])
      )

      results_list[[length(results_list) + 1]] <- result
    }
    
    cat("  Found", length(signal_indices), "Perfect Setup signals\n")
    
  }, error = function(e) {
    cat("  Error processing", sym, ":", e$message, "\n")
  })
}

# Combine results
if (length(results_list) == 0) {
  stop("No signals found across all symbols!")
}

results_all <- rbindlist(results_list)

# Create separate datasets for 90-day and 180-day analysis
results_90d <- results_all[!is.na(return_90d)]
results_180d <- results_all[!is.na(return_180d)]

# ============================================================
# ===== 90-DAY ANALYSIS (More signals, more recent data) =====
# ============================================================

cat("\n")
cat("################################################################\n")
cat("#                    90-DAY ANALYSIS                          #\n")
cat("################################################################\n\n")

cat("===== 90-DAY BACKTEST SUMMARY =====\n")
cat("Total Perfect Setup signals:", nrow(results_90d), "\n")
cat("Date range:", as.character(min(results_90d$date)), "to", as.character(max(results_90d$date)), "\n")
cat("Unique symbols with signals:", length(unique(results_90d$symbol)), "\n\n")

cat("===== 90-DAY FORWARD PERFORMANCE =====\n")
cat(sprintf("Perfect Setup Avg Return: %.2f%%\n", mean(results_90d$return_90d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg Return:       %.2f%%\n", mean(results_90d$sp500_fwd_90d, na.rm = TRUE) * 100))
cat(sprintf("MSCI World Avg Return:    %.2f%%\n", mean(results_90d$msci_fwd_90d, na.rm = TRUE) * 100))
cat(sprintf("Outperformance vs S&P:   %.2f%%\n",
            (mean(results_90d$return_90d, na.rm = TRUE) - mean(results_90d$sp500_fwd_90d, na.rm = TRUE)) * 100))
cat(sprintf("Outperformance vs MSCI:  %.2f%%\n",
            (mean(results_90d$return_90d, na.rm = TRUE) - mean(results_90d$msci_fwd_90d, na.rm = TRUE)) * 100))
cat(sprintf("Win Rate: %.1f%%\n", sum(results_90d$return_90d > 0, na.rm = TRUE) / nrow(results_90d) * 100))
cat(sprintf("Beat S&P 500: %.1f%%\n", sum(results_90d$return_90d > results_90d$sp500_fwd_90d, na.rm = TRUE) / nrow(results_90d) * 100))
cat(sprintf("Beat MSCI: %.1f%%\n\n", sum(results_90d$return_90d > results_90d$msci_fwd_90d, na.rm = TRUE) / nrow(results_90d) * 100))

# 90-day statistical tests
cat("===== 90-DAY STATISTICAL SIGNIFICANCE =====\n")
t_test_90_sp500 <- t.test(results_90d$return_90d, results_90d$sp500_fwd_90d, paired = TRUE)
t_test_90_msci <- t.test(results_90d$return_90d, results_90d$msci_fwd_90d, paired = TRUE)

cat("90-Day vs S&P 500:\n")
cat(sprintf("  t-statistic: %.3f\n", t_test_90_sp500$statistic))
cat(sprintf("  p-value: %.4f %s\n", t_test_90_sp500$p.value,
            ifelse(t_test_90_sp500$p.value < 0.05, "**", "")))

cat("90-Day vs MSCI World:\n")
cat(sprintf("  t-statistic: %.3f\n", t_test_90_msci$statistic))
cat(sprintf("  p-value: %.4f %s\n\n", t_test_90_msci$p.value,
            ifelse(t_test_90_msci$p.value < 0.05, "**", "")))

# ===== PAST PERFORMANCE (MOMENTUM) AT SIGNAL =====
cat("===== PAST PERFORMANCE AT SIGNAL (MOMENTUM) =====\n")
cat(sprintf("Stock Avg 30D Past Return:  %.2f%%\n", mean(results_90d$past_return_30d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg 30D Past Return: %.2f%%\n", mean(results_90d$sp500_past_30d, na.rm = TRUE) * 100))
cat(sprintf("Relative Strength 30D:      %.2f%%\n", mean(results_90d$rel_strength_30d, na.rm = TRUE) * 100))
cat(sprintf("Stock Avg 90D Past Return:  %.2f%%\n", mean(results_90d$past_return_90d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg 90D Past Return: %.2f%%\n", mean(results_90d$sp500_past_90d, na.rm = TRUE) * 100))
cat(sprintf("Relative Strength 90D:      %.2f%%\n\n", mean(results_90d$rel_strength_90d, na.rm = TRUE) * 100))

# ===== TECHNICAL INDICATORS AT SIGNAL =====
cat("===== TECHNICAL INDICATORS AT SIGNAL (Mean Values) =====\n")
cat(sprintf("RSI:                  %.1f\n", mean(results_90d$rsi, na.rm = TRUE)))
cat(sprintf("ADX:                  %.1f\n", mean(results_90d$adx, na.rm = TRUE)))
cat(sprintf("DI+:                  %.1f\n", mean(results_90d$di_plus, na.rm = TRUE)))
cat(sprintf("DI-:                  %.1f\n", mean(results_90d$di_minus, na.rm = TRUE)))
cat(sprintf("MACD Histogram:       %.4f\n", mean(results_90d$macd_hist, na.rm = TRUE)))
cat(sprintf("Bollinger %%B:         %.2f\n", mean(results_90d$bb_pct, na.rm = TRUE)))
cat(sprintf("ATR %%:                %.2f%%\n", mean(results_90d$atr_pct, na.rm = TRUE)))
cat(sprintf("Stochastic %%K:        %.1f\n", mean(results_90d$stoch_k, na.rm = TRUE)))
cat(sprintf("Stochastic %%D:        %.1f\n", mean(results_90d$stoch_d, na.rm = TRUE)))
cat(sprintf("CCI:                  %.1f\n", mean(results_90d$cci, na.rm = TRUE)))
cat(sprintf("Volume Ratio:         %.2fx\n", mean(results_90d$volume_ratio, na.rm = TRUE)))
cat(sprintf("%% from MA20:          %.2f%%\n", mean(results_90d$pct_from_ma20, na.rm = TRUE)))
cat(sprintf("%% from MA50:          %.2f%%\n", mean(results_90d$pct_from_ma50, na.rm = TRUE)))
cat(sprintf("%% from MA200:         %.2f%%\n", mean(results_90d$pct_from_ma200, na.rm = TRUE)))
cat(sprintf("%% from 52W High:      %.2f%%\n", mean(results_90d$pct_from_52w_high, na.rm = TRUE)))
cat(sprintf("%% from 52W Low:       %.2f%%\n\n", mean(results_90d$pct_from_52w_low, na.rm = TRUE)))

cat("===== TOP 5 SIGNALS (90-day return) =====\n")
top_signals_90d <- results_90d[order(-return_90d)][1:5]
print(top_signals_90d[, .(symbol, date, entry_price, return_90d = sprintf("%.2f%%", return_90d * 100),
                          rel_str_90d = sprintf("%.1f%%", rel_strength_90d * 100))])

cat("\n===== WORST 5 SIGNALS (90-day return) =====\n")
worst_signals_90d <- results_90d[order(return_90d)][1:5]
print(worst_signals_90d[, .(symbol, date, entry_price, return_90d = sprintf("%.2f%%", return_90d * 100),
                            rel_str_90d = sprintf("%.1f%%", rel_strength_90d * 100))])

# ============================================================
# ===== 180-DAY ANALYSIS (Fewer signals, longer horizon) =====
# ============================================================

cat("\n\n")
cat("################################################################\n")
cat("#                   180-DAY ANALYSIS                          #\n")
cat("################################################################\n\n")

cat("===== 180-DAY BACKTEST SUMMARY =====\n")
cat("Total Perfect Setup signals:", nrow(results_180d), "\n")
cat("Date range:", as.character(min(results_180d$date)), "to", as.character(max(results_180d$date)), "\n")
cat("Unique symbols with signals:", length(unique(results_180d$symbol)), "\n\n")

cat("===== 180-DAY FORWARD PERFORMANCE =====\n")
cat(sprintf("Perfect Setup Avg Return: %.2f%%\n", mean(results_180d$return_180d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg Return:       %.2f%%\n", mean(results_180d$sp500_fwd_180d, na.rm = TRUE) * 100))
cat(sprintf("MSCI World Avg Return:    %.2f%%\n", mean(results_180d$msci_fwd_180d, na.rm = TRUE) * 100))
cat(sprintf("Outperformance vs S&P:   %.2f%%\n",
            (mean(results_180d$return_180d, na.rm = TRUE) - mean(results_180d$sp500_fwd_180d, na.rm = TRUE)) * 100))
cat(sprintf("Outperformance vs MSCI:  %.2f%%\n",
            (mean(results_180d$return_180d, na.rm = TRUE) - mean(results_180d$msci_fwd_180d, na.rm = TRUE)) * 100))
cat(sprintf("Win Rate: %.1f%%\n", sum(results_180d$return_180d > 0, na.rm = TRUE) / nrow(results_180d) * 100))
cat(sprintf("Beat S&P 500: %.1f%%\n", sum(results_180d$return_180d > results_180d$sp500_fwd_180d, na.rm = TRUE) / nrow(results_180d) * 100))
cat(sprintf("Beat MSCI: %.1f%%\n\n", sum(results_180d$return_180d > results_180d$msci_fwd_180d, na.rm = TRUE) / nrow(results_180d) * 100))

# 180-day statistical tests
cat("===== 180-DAY STATISTICAL SIGNIFICANCE =====\n")
t_test_180_sp500 <- t.test(results_180d$return_180d, results_180d$sp500_fwd_180d, paired = TRUE)
t_test_180_msci <- t.test(results_180d$return_180d, results_180d$msci_fwd_180d, paired = TRUE)

cat("180-Day vs S&P 500:\n")
cat(sprintf("  t-statistic: %.3f\n", t_test_180_sp500$statistic))
cat(sprintf("  p-value: %.4f %s\n", t_test_180_sp500$p.value,
            ifelse(t_test_180_sp500$p.value < 0.05, "**", "")))

cat("180-Day vs MSCI World:\n")
cat(sprintf("  t-statistic: %.3f\n", t_test_180_msci$statistic))
cat(sprintf("  p-value: %.4f %s\n\n", t_test_180_msci$p.value,
            ifelse(t_test_180_msci$p.value < 0.05, "**", "")))

# ===== PAST PERFORMANCE (MOMENTUM) AT SIGNAL =====
cat("===== PAST PERFORMANCE AT SIGNAL (MOMENTUM) =====\n")
cat(sprintf("Stock Avg 30D Past Return:  %.2f%%\n", mean(results_180d$past_return_30d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg 30D Past Return: %.2f%%\n", mean(results_180d$sp500_past_30d, na.rm = TRUE) * 100))
cat(sprintf("Relative Strength 30D:      %.2f%%\n", mean(results_180d$rel_strength_30d, na.rm = TRUE) * 100))
cat(sprintf("Stock Avg 90D Past Return:  %.2f%%\n", mean(results_180d$past_return_90d, na.rm = TRUE) * 100))
cat(sprintf("S&P 500 Avg 90D Past Return: %.2f%%\n", mean(results_180d$sp500_past_90d, na.rm = TRUE) * 100))
cat(sprintf("Relative Strength 90D:      %.2f%%\n\n", mean(results_180d$rel_strength_90d, na.rm = TRUE) * 100))

# ===== TECHNICAL INDICATORS AT SIGNAL =====
cat("===== TECHNICAL INDICATORS AT SIGNAL (Mean Values) =====\n")
cat(sprintf("RSI:                  %.1f\n", mean(results_180d$rsi, na.rm = TRUE)))
cat(sprintf("ADX:                  %.1f\n", mean(results_180d$adx, na.rm = TRUE)))
cat(sprintf("DI+:                  %.1f\n", mean(results_180d$di_plus, na.rm = TRUE)))
cat(sprintf("DI-:                  %.1f\n", mean(results_180d$di_minus, na.rm = TRUE)))
cat(sprintf("MACD Histogram:       %.4f\n", mean(results_180d$macd_hist, na.rm = TRUE)))
cat(sprintf("Bollinger %%B:         %.2f\n", mean(results_180d$bb_pct, na.rm = TRUE)))
cat(sprintf("ATR %%:                %.2f%%\n", mean(results_180d$atr_pct, na.rm = TRUE)))
cat(sprintf("Stochastic %%K:        %.1f\n", mean(results_180d$stoch_k, na.rm = TRUE)))
cat(sprintf("Stochastic %%D:        %.1f\n", mean(results_180d$stoch_d, na.rm = TRUE)))
cat(sprintf("CCI:                  %.1f\n", mean(results_180d$cci, na.rm = TRUE)))
cat(sprintf("Volume Ratio:         %.2fx\n", mean(results_180d$volume_ratio, na.rm = TRUE)))
cat(sprintf("%% from MA20:          %.2f%%\n", mean(results_180d$pct_from_ma20, na.rm = TRUE)))
cat(sprintf("%% from MA50:          %.2f%%\n", mean(results_180d$pct_from_ma50, na.rm = TRUE)))
cat(sprintf("%% from MA200:         %.2f%%\n", mean(results_180d$pct_from_ma200, na.rm = TRUE)))
cat(sprintf("%% from 52W High:      %.2f%%\n", mean(results_180d$pct_from_52w_high, na.rm = TRUE)))
cat(sprintf("%% from 52W Low:       %.2f%%\n\n", mean(results_180d$pct_from_52w_low, na.rm = TRUE)))

cat("===== TOP 5 SIGNALS (180-day return) =====\n")
top_signals_180d <- results_180d[order(-return_180d)][1:5]
print(top_signals_180d[, .(symbol, date, entry_price, return_180d = sprintf("%.2f%%", return_180d * 100),
                           rel_str_90d = sprintf("%.1f%%", rel_strength_90d * 100))])

cat("\n===== WORST 5 SIGNALS (180-day return) =====\n")
worst_signals_180d <- results_180d[order(return_180d)][1:5]
print(worst_signals_180d[, .(symbol, date, entry_price, return_180d = sprintf("%.2f%%", return_180d * 100),
                             rel_str_90d = sprintf("%.1f%%", rel_strength_90d * 100))])

# ===== VISUALIZATIONS =====

cat("\n\nGenerating visualizations...\n")

# 1. Distribution comparison - 90 days (using 90d dataset)
p1 <- ggplot(results_90d) +
  geom_density(aes(x = return_90d * 100, fill = "Perfect Setup"), alpha = 0.5) +
  geom_density(aes(x = sp500_fwd_90d * 100, fill = "S&P 500"), alpha = 0.5) +
  geom_density(aes(x = msci_fwd_90d * 100, fill = "MSCI World"), alpha = 0.5) +
  scale_fill_manual(values = c("Perfect Setup" = "#00FF00", "S&P 500" = "#FF6B35", "MSCI World" = "#4169E1")) +
  labs(title = paste0("90-Day Forward Return Distribution (n=", nrow(results_90d), ")"),
       x = "Return (%)",
       y = "Density",
       fill = "Strategy") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# 2. Distribution comparison - 180 days (using 180d dataset)
p2 <- ggplot(results_180d) +
  geom_density(aes(x = return_180d * 100, fill = "Perfect Setup"), alpha = 0.5) +
  geom_density(aes(x = sp500_fwd_180d * 100, fill = "S&P 500"), alpha = 0.5) +
  geom_density(aes(x = msci_fwd_180d * 100, fill = "MSCI World"), alpha = 0.5) +
  scale_fill_manual(values = c("Perfect Setup" = "#00FF00", "S&P 500" = "#FF6B35", "MSCI World" = "#4169E1")) +
  labs(title = paste0("180-Day Forward Return Distribution (n=", nrow(results_180d), ")"),
       x = "Return (%)",
       y = "Density",
       fill = "Strategy") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# 3. Box plot comparison (separate datasets)
results_long <- rbindlist(list(
  data.table(Strategy = "Perfect Setup", Period = "90d", Return = results_90d$return_90d * 100),
  data.table(Strategy = "S&P 500", Period = "90d", Return = results_90d$sp500_fwd_90d * 100),
  data.table(Strategy = "MSCI World", Period = "90d", Return = results_90d$msci_fwd_90d * 100),
  data.table(Strategy = "Perfect Setup", Period = "180d", Return = results_180d$return_180d * 100),
  data.table(Strategy = "S&P 500", Period = "180d", Return = results_180d$sp500_fwd_180d * 100),
  data.table(Strategy = "MSCI World", Period = "180d", Return = results_180d$msci_fwd_180d * 100)
))

p3 <- ggplot(results_long, aes(x = Strategy, y = Return, fill = Strategy)) +
  geom_boxplot() +
  facet_wrap(~Period) +
  scale_fill_manual(values = c("Perfect Setup" = "#00FF00", "S&P 500" = "#FF6B35", "MSCI World" = "#4169E1")) +
  labs(title = "Return Distribution Comparison",
       y = "Return (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# 4. Scatter plot - 90d vs 180d (only for signals with both returns)
results_both <- results_all[!is.na(return_90d) & !is.na(return_180d)]
p4 <- ggplot(results_both, aes(x = return_90d * 100, y = return_180d * 100)) +
  geom_point(alpha = 0.6, color = "#00FF00") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = paste0("90-Day vs 180-Day Returns (n=", nrow(results_both), ")"),
       x = "90-Day Return (%)",
       y = "180-Day Return (%)") +
  theme_minimal()

print(p4)

# 5. Signals over time (90d dataset - more complete)
signals_by_month <- results_90d[, .(count = .N), by = .(year = year(date), month = month(date))]
signals_by_month[, date := as.Date(paste(year, month, "01", sep = "-"))]

p5 <- ggplot(signals_by_month, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "#00FF00") +
  labs(title = "Perfect Setup Signals Over Time (90-Day Analysis)",
       x = "Date",
       y = "Number of Signals") +
  theme_minimal()

print(p5)

# 6. Relative Strength at Signal vs Forward Return
p6 <- ggplot(results_90d, aes(x = rel_strength_90d * 100, y = return_90d * 100)) +
  geom_point(alpha = 0.5, color = "#00FF00") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "90D Relative Strength at Signal vs 90D Forward Return",
       x = "Relative Strength vs S&P 500 (Past 90D, %)",
       y = "Forward Return (90D, %)") +
  theme_minimal()

print(p6)

# 7. Past Return (Momentum) vs Forward Return
p7 <- ggplot(results_90d, aes(x = past_return_90d * 100, y = return_90d * 100)) +
  geom_point(alpha = 0.5, color = "#4169E1") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Past 90D Return (Momentum) vs 90D Forward Return",
       x = "Past Return (90D, %)",
       y = "Forward Return (90D, %)") +
  theme_minimal()

print(p7)

# 8. RSI at Signal vs Forward Return
p8 <- ggplot(results_90d, aes(x = rsi, y = return_90d * 100)) +
  geom_point(alpha = 0.5, color = "#FF6B35") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_vline(xintercept = c(45, 65), linetype = "dashed", color = "gray") +
  labs(title = "RSI at Signal vs 90D Forward Return",
       x = "RSI",
       y = "Forward Return (90D, %)") +
  theme_minimal()

print(p8)

# ===== FUNDAMENTAL DATA SUMMARY =====
cat("\n===== FUNDAMENTAL DATA AT SIGNAL (Mean Values) =====\n")
cat(sprintf("Signals with fundamental data: %d / %d\n",
            sum(!is.na(results_90d$fund_revenue)), nrow(results_90d)))
cat(sprintf("Gross Margin:           %.1f%%\n", mean(results_90d$fund_gross_margin, na.rm = TRUE) * 100))
cat(sprintf("Net Margin:             %.1f%%\n", mean(results_90d$fund_net_margin, na.rm = TRUE) * 100))
cat(sprintf("ROE (annualized):       %.1f%%\n", mean(results_90d$fund_roe, na.rm = TRUE) * 100))
cat(sprintf("Debt/Equity:            %.2f\n", mean(results_90d$fund_debt_to_equity, na.rm = TRUE)))
cat(sprintf("Revenue Growth QoQ:     %.1f%%\n", mean(results_90d$fund_revenue_growth_qoq, na.rm = TRUE) * 100))
cat(sprintf("Earnings Growth QoQ:    %.1f%%\n\n", mean(results_90d$fund_earnings_growth_qoq, na.rm = TRUE) * 100))

# ===== CORRELATION ANALYSIS =====
cat("\n===== INDICATOR CORRELATIONS WITH 90D FORWARD RETURN =====\n")
cor_vars <- c("past_return_30d", "past_return_90d", "rel_strength_30d", "rel_strength_90d",
              "rsi", "adx", "macd_hist", "bb_pct", "atr_pct", "stoch_k", "cci",
              "volume_ratio", "pct_from_ma20", "pct_from_ma50", "pct_from_ma200",
              "pct_from_52w_high", "pct_from_52w_low",
              "fund_gross_margin", "fund_net_margin", "fund_roe",
              "fund_debt_to_equity", "fund_revenue_growth_qoq",
              "fund_earnings_growth_qoq")

correlations <- sapply(cor_vars, function(var) {
  if (var %in% names(results_90d)) {
    cor(results_90d[[var]], results_90d$return_90d, use = "complete.obs")
  } else {
    NA
  }
})

cor_df <- data.table(
  Indicator = cor_vars,
  Correlation = round(correlations, 3)
)
cor_df <- cor_df[!is.na(Correlation)][order(-abs(Correlation))]
print(cor_df)
p
# Save results to CSV (all data, separate files for each analysis)
fwrite(results_all, "perfect_setup_backtest_results_all.csv")
fwrite(results_90d, "perfect_setup_backtest_results_90d.csv")
fwrite(results_180d, "perfect_setup_backtest_results_180d.csv")

cat("\n===== BACKTEST COMPLETE =====\n")
cat("Results saved to:\n")
cat("  - perfect_setup_backtest_results_all.csv (all signals)\n")
cat("  - perfect_setup_backtest_results_90d.csv (90-day analysis)\n")
cat("  - perfect_setup_backtest_results_180d.csv (180-day analysis)\n")