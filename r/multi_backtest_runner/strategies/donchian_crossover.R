# Donchian Channel Breakout - N-Bar High/Low Ausbruch.

NAME <- "Donchian_Breakout"

PARAM_GRID <- list(
  lookback = c(20L, 30L, 40L, 60L),
  exit_n   = c(10L, 20L)
)

generate_signals <- function(df, lookback = 20L, exit_n = 10L) {
  n <- nrow(df)
  # shift damit aktuelle Bar nicht im Lookback enthalten ist (kein Look-ahead)
  upper       <- data.table::shift(TTR::runMax(df$High, n = lookback), 1)
  lower       <- data.table::shift(TTR::runMin(df$Low,  n = lookback), 1)
  exit_upper  <- data.table::shift(TTR::runMax(df$High, n = exit_n),   1)
  exit_lower  <- data.table::shift(TTR::runMin(df$Low,  n = exit_n),   1)
  
  pos <- numeric(n)
  state <- 0
  h <- df$High; l <- df$Low
  
  for (i in seq_len(n)) {
    if (is.na(upper[i]) || is.na(lower[i])) { pos[i] <- 0; next }
    
    if (state == 0) {
      if      (h[i] > upper[i]) state <-  1
      else if (l[i] < lower[i]) state <- -1
    } else if (state ==  1) {
      if (!is.na(exit_lower[i]) && l[i] < exit_lower[i]) state <- 0
    } else if (state == -1) {
      if (!is.na(exit_upper[i]) && h[i] > exit_upper[i]) state <- 0
    }
    pos[i] <- state
  }
  
  out <- data.table::copy(df)
  out[, Position := pos]
  out
}