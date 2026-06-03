# Convert TradingView Tickers to Yahoo Finance Format
# ====================================================

#' Convert TradingView ticker to Yahoo Finance format
#'
#' @param symbol Character. The ticker symbol from TradingView
#' @param country Character. Country or region of registration
#' @param description Character. Company description (optional, for context)
#' @return Character. Yahoo Finance formatted ticker
#' @examples
#' convert_ticker_to_yahoo("2330", "Taiwan")
#' convert_ticker_to_yahoo("GOOG", "United States")
convert_ticker_to_yahoo <- function(symbol, country, description = "") {
  
  # Remove whitespace
  symbol <- trimws(symbol)
  country <- trimws(country)
  
  # United States - usually no suffix
  if (country == "United States") {
    # Clean up ADR notation
    symbol <- gsub(" Sponsored ADR", "", symbol)
    symbol <- gsub("Sponsored ADR", "", symbol)
    return(trimws(symbol))
  }
  
  # China - multiple exchanges
  else if (country == "China") {
    # Check if it's a number (mainland China) or already formatted
    if (grepl("^[0-9]+$", symbol)) {
      # Shanghai Stock Exchange (600xxx, 601xxx, 603xxx, 605xxx)
      if (substr(symbol, 1, 1) == "6") {
        return(paste0(symbol, ".SS"))
      }
      # Shenzhen Stock Exchange (000xxx, 002xxx, 300xxx, 301xxx)
      else if (substr(symbol, 1, 1) %in% c("0", "2", "3")) {
        return(paste0(symbol, ".SZ"))
      }
      # Hong Kong Stock Exchange (other numbers)
      else {
        return(paste0(symbol, ".HK"))
      }
    }
    # Already formatted (e.g., BABA, JD, etc.) - ADRs
    return(symbol)
  }
  
  # Hong Kong
  else if (country == "Hong Kong") {
    if (grepl("^[0-9]+$", symbol) && !grepl("\\.", symbol)) {
      return(paste0(symbol, ".HK"))
    }
    return(symbol)
  }
  
  # Taiwan
  else if (country == "Taiwan") {
    if (grepl("^[0-9]+$", symbol) && !grepl("\\.", symbol)) {
      return(paste0(symbol, ".TW"))
    }
    return(symbol)
  }
  
  # Japan
  else if (country == "Japan") {
    if (grepl("^[0-9]+$", symbol) && !grepl("\\.", symbol)) {
      return(paste0(symbol, ".T"))
    }
    return(symbol)
  }
  
  # South Korea
  else if (country == "South Korea") {
    if (grepl("^[0-9]+$", symbol) && !grepl("\\.", symbol)) {
      return(paste0(symbol, ".KS"))
    }
    return(symbol)
  }
  
  # India - NSE (National Stock Exchange) default
  else if (country == "India") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".NS"))
    }
    return(symbol)
  }
  
  # Canada - Toronto Stock Exchange
  else if (country == "Canada") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".TO"))
    }
    return(symbol)
  }
  
  # Australia - ASX
  else if (country == "Australia") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".AX"))
    }
    return(symbol)
  }
  
  # Brazil - B3
  else if (country == "Brazil") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".SA"))
    }
    return(symbol)
  }
  
  # Mexico
  else if (country == "Mexico") {
    # Remove /B, /A suffixes common in Mexican stocks
    symbol <- gsub("/[AB]$", "", symbol)
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".MX"))
    }
    return(symbol)
  }
  
  # Germany
  else if (country == "Germany") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".DE"))
    }
    return(symbol)
  }
  
  # France
  else if (country == "France") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".PA"))
    }
    return(symbol)
  }
  
  # United Kingdom - London Stock Exchange
  else if (country == "United Kingdom") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".L"))
    }
    return(symbol)
  }
  
  # Switzerland
  else if (country == "Switzerland") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".SW"))
    }
    return(symbol)
  }
  
  # Spain
  else if (country == "Spain") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".MC"))
    }
    return(symbol)
  }
  
  # Italy
  else if (country == "Italy") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".MI"))
    }
    return(symbol)
  }
  
  # Netherlands
  else if (country == "Netherlands") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".AS"))
    }
    return(symbol)
  }
  
  # Belgium
  else if (country == "Belgium") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".BR"))
    }
    return(symbol)
  }
  
  # Sweden
  else if (country == "Sweden") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".ST"))
    }
    return(symbol)
  }
  
  # Norway
  else if (country == "Norway") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".OL"))
    }
    return(symbol)
  }
  
  # Denmark
  else if (country == "Denmark") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".CO"))
    }
    return(symbol)
  }
  
  # Finland
  else if (country == "Finland") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".HE"))
    }
    return(symbol)
  }
  
  # Poland
  else if (country == "Poland") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".WA"))
    }
    return(symbol)
  }
  
  # Turkey
  else if (country == "Turkey") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".IS"))
    }
    return(symbol)
  }
  
  # South Africa - JSE
  else if (country == "South Africa") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".JO"))
    }
    return(symbol)
  }
  
  # Singapore
  else if (country == "Singapore") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".SI"))
    }
    return(symbol)
  }
  
  # Malaysia
  else if (country == "Malaysia") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".KL"))
    }
    return(symbol)
  }
  
  # Thailand
  else if (country == "Thailand") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".BK"))
    }
    return(symbol)
  }
  
  # Indonesia
  else if (country == "Indonesia") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".JK"))
    }
    return(symbol)
  }
  
  # Philippines
  else if (country == "Philippines") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".PS"))
    }
    return(symbol)
  }
  
  # Vietnam
  else if (country == "Vietnam") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".VN"))
    }
    return(symbol)
  }
  
  # New Zealand
  else if (country == "New Zealand") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".NZ"))
    }
    return(symbol)
  }
  
  # Argentina
  else if (country == "Argentina") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".BA"))
    }
    return(symbol)
  }
  
  # Chile
  else if (country == "Chile") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".SN"))
    }
    return(symbol)
  }
  
  # Peru
  else if (country == "Peru") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".LM"))
    }
    return(symbol)
  }
  
  # Colombia
  else if (country == "Colombia") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".BO"))  # Bogota Stock Exchange
    }
    return(symbol)
  }
  
  # Saudi Arabia
  else if (country == "Saudi Arabia") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".SAU"))
    }
    return(symbol)
  }
  
  # United Arab Emirates
  else if (country == "United Arab Emirates") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".AE"))
    }
    return(symbol)
  }
  
  # Qatar
  else if (country == "Qatar") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".QA"))
    }
    return(symbol)
  }
  
  # Egypt
  else if (country == "Egypt") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".CA"))
    }
    return(symbol)
  }
  
  # Israel
  else if (country == "Israel") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".TA"))
    }
    return(symbol)
  }
  
  # Ireland
  else if (country == "Ireland") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".IR"))
    }
    return(symbol)
  }
  
  # Austria
  else if (country == "Austria") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".VI"))
    }
    return(symbol)
  }
  
  # Greece
  else if (country == "Greece") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".AT"))
    }
    return(symbol)
  }
  
  # Luxembourg
  else if (country == "Luxembourg") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".LU"))
    }
    return(symbol)
  }
  
  # Portugal
  else if (country == "Portugal") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".LS"))
    }
    return(symbol)
  }
  
  # Pakistan
  else if (country == "Pakistan") {
    if (!grepl("\\.", symbol)) {
      return(paste0(symbol, ".KA"))
    }
    return(symbol)
  }
  
  # Default: return as-is if unknown country
  else {
    warning(paste("Unknown country:", country, "- returning symbol as-is:", symbol))
    return(symbol)
  }
}


#' Batch convert multiple tickers
#'
#' @param symbols Character vector. Ticker symbols
#' @param countries Character vector. Countries (same length as symbols)
#' @param descriptions Character vector. Descriptions (optional)
#' @return Character vector. Yahoo Finance formatted tickers
#' @examples
#' symbols <- c("2330", "GOOG", "700", "AAPL")
#' countries <- c("Taiwan", "United States", "China", "United States")
#' convert_tickers_batch(symbols, countries)
convert_tickers_batch <- function(symbols, countries, descriptions = NULL) {
  
  if (length(symbols) != length(countries)) {
    stop("symbols and countries must have the same length")
  }
  
  if (is.null(descriptions)) {
    descriptions <- rep("", length(symbols))
  }
  
  yahoo_tickers <- mapply(
    convert_ticker_to_yahoo,
    symbol = symbols,
    country = countries,
    description = descriptions,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  
  return(yahoo_tickers)
}


#' Load and convert TradingView watchlist CSV
#'
#' @param csv_path Character. Path to TradingView export CSV
#' @return data.frame with original and converted tickers
#' @examples
#' watchlist <- load_and_convert_watchlist("my_watchlist.csv")
load_and_convert_watchlist <- function(csv_path) {
  
  # Load CSV
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Check required columns
  required_cols <- c("Symbol", "Country.or.region.of.registration")
  if (!all(required_cols %in% names(df))) {
    stop(paste("CSV must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Add description column if it exists
  description_col <- if ("Description" %in% names(df)) df$Description else rep("", nrow(df))
  
  # Convert tickers
  df$Yahoo_Ticker <- convert_tickers_batch(
    symbols = df$Symbol,
    countries = df$Country.or.region.of.registration,
    descriptions = description_col
  )
  
  cat("Converted", nrow(df), "tickers\n")
  cat("\nTicker format breakdown:\n")
  
  # Show breakdown
  formats <- ifelse(
    grepl("\\.", df$Yahoo_Ticker),
    sub(".*\\.", "", df$Yahoo_Ticker),
    "No suffix"
  )
  print(table(formats))
  
  return(df)
}


# ===== EXAMPLE USAGE =====

# Single ticker conversion
# yahoo_ticker <- convert_ticker_to_yahoo("2330", "Taiwan")
# print(yahoo_ticker)  # "2330.TW"

# Batch conversion
# symbols <- c("2330", "GOOG", "700", "AAPL", "6503")
# countries <- c("Taiwan", "United States", "China", "United States", "Japan")
# yahoo_tickers <- convert_tickers_batch(symbols, countries)
# print(yahoo_tickers)

# Load from CSV
# watchlist <- load_and_convert_watchlist("Alert_Watchlist_2026-01-22.csv")
# View(watchlist[, c("Symbol", "Yahoo_Ticker", "Country.or.region.of.registration")])

cat("Ticker conversion functions loaded successfully!\n")
cat("Available functions:\n")
cat("  - convert_ticker_to_yahoo(symbol, country, description)\n")
cat("  - convert_tickers_batch(symbols, countries, descriptions)\n")
cat("  - load_and_convert_watchlist(csv_path)\n")