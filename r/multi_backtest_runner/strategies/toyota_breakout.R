# Toyota Intraday Breakout – N-Bar Channel
#
# Entry Long:  Close > N-Bar High (der vorigen Bar, kein Look-Ahead)
# Entry Short: Close < N-Bar Low
# Exit:        Gegensignal
#
# Originalstrategie hatte ATR-SL (1.5x) + ATR-TP (3.0x) und Session-Filter
# (08:00-22:00). Im vektoriellen Framework wird auf Gegensignal exitiert.
# Session-Filter benoetigt Timestamp-Awareness – er wird hier als optionaler
# Parameter mitgegeben und greift nur wenn Timestamp-Spalte vorhanden.
#
# Instrumente laut Original: DE40, J225, Gold (5min / 15min)

NAME <- "Toyota_Breakout"

PARAM_GRID <- list(
  lookback     = c(3L, 5L, 7L, 10L),
  atr_n        = c(14L),
  session_only = c(FALSE)  # TRUE: nur 08-22 UTC (benoetigt Timestamp)
)

generate_signals <- function(df, lookback = 5L, atr_n = 14L,
                              session_only = FALSE) {
  out <- data.table::copy(df)
  n   <- nrow(out)

  # Channel High/Low mit shift(1) -> kein Look-Ahead
  ch_high <- data.table::shift(TTR::runMax(out$High, lookback), 1)
  ch_low  <- data.table::shift(TTR::runMin(out$Low,  lookback), 1)

  # Optionaler Session-Filter (08:00–22:00 UTC)
  in_session <- rep(TRUE, n)
  if (session_only && "Timestamp" %in% names(out)) {
    h <- as.integer(format(out$Timestamp, "%H", tz = "UTC"))
    in_session <- h >= 8L & h < 22L
  }

  pos   <- numeric(n)
  state <- 0L

  for (i in 2:n) {
    if (is.na(ch_high[i]) || is.na(ch_low[i])) {
      pos[i] <- state; next
    }

    long_signal  <- out$Close[i] > ch_high[i] && in_session[i]
    short_signal <- out$Close[i] < ch_low[i]  && in_session[i]

    # Exit: Gegensignal
    if (state == 0L) {
      if      (long_signal)  state <-  1L
      else if (short_signal) state <- -1L
    } else if (state ==  1L) {
      if (short_signal) state <- -1L
    } else if (state == -1L) {
      if (long_signal)  state <-  1L
    }

    pos[i] <- state
  }

  out[, Position := pos]
  out
}
