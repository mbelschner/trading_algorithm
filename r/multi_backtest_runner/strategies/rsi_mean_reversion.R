# RSI Mean Reversion mit Wilder's Smoothing und expliziten Exits.
#
# Jede Strategie-Datei muss definieren:
#   NAME              - character(1)
#   PARAM_GRID        - named list of vectors (alle Kombinationen werden expandiert)
#   generate_signals  - function(df, ...) -> data.table mit Spalte 'Position'
#
# df hat Spalten: Timestamp, Open, High, Low, Close, Volume

NAME <- "RSI_MeanReversion"

PARAM_GRID <- list(
  period   = c(7L, 10L, 14L, 18L, 21L),
  lower    = c(20, 25, 30),
  upper    = c(70, 75, 80),
  exit_mid = c(TRUE)
)

generate_signals <- function(df, period = 14L, lower = 30, upper = 70, exit_mid = TRUE) {
  # TTR::RSI mit maType = "EMA" entspricht Wilder's Smoothing (alpha = 1/n)
  rsi <- TTR::RSI(df$Close, n = period, maType = "EMA")
  
  n <- nrow(df)
  pos <- numeric(n)
  state <- 0
  
  for (i in seq_len(n)) {
    r <- rsi[i]
    if (is.na(r)) { pos[i] <- 0; next }
    
    if (state == 0) {
      if (r < lower)      state <-  1
      else if (r > upper) state <- -1
    } else if (state == 1) {
      if (exit_mid && r >= 50)  state <- 0
      else if (r > upper)       state <- -1
    } else if (state == -1) {
      if (exit_mid && r <= 50)  state <- 0
      else if (r < lower)       state <- 1
    }
    pos[i] <- state
  }
  
  out <- data.table::copy(df)
  out[, RSI := rsi]
  out[, Position := pos]
  out
}