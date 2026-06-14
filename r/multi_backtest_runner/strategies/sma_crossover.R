# SMA Crossover - klassische Trend-Following Strategie.

NAME <- "SMA_Crossover"

PARAM_GRID <- list(
  fast = c(10L, 20L, 30L),
  slow = c(40L, 50L, 80L, 100L, 200L)
)

generate_signals <- function(df, fast = 20L, slow = 50L) {
  out <- data.table::copy(df)
  
  if (fast >= slow) {
    out[, Position := 0]
    return(out)
  }
  
  out[, SMA_fast := TTR::SMA(Close, n = fast)]
  out[, SMA_slow := TTR::SMA(Close, n = slow)]
  
  out[, Position := data.table::fifelse(SMA_fast > SMA_slow, 1,
                                        data.table::fifelse(SMA_fast < SMA_slow, -1, 0))]
  out[is.na(SMA_slow), Position := 0]
  out[, Position := as.numeric(Position)]
  out
}