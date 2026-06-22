# ATR-Distance (Keltner-style) Mean Reversion
#
# Archetyp: Reversion der Distanz Close <-> EMA, normiert in ATR-Einheiten.
# Einstieg wenn Close > EMA + mult*ATR (short) bzw. < EMA - mult*ATR (long),
# Ausstieg beim EMA-Rueck-Cross. Da die Bandbreite ueber ATR (Range-basiert)
# statt Stddev (Close-basiert) skaliert, passt sich die Schwelle bei
# Vola-Regimewechseln und Gaps schneller an -> komplementaer zu Bollinger,
# gut fuer gappy/trendige Commodities. 5m-1H.
#
# df hat Spalten: Timestamp, Open, High, Low, Close, Volume
NAME <- "ATRDistance_MeanReversion"
PARAM_GRID <- list(
  ema_period = c(20L, 30L, 50L),
  atr_period = c(10L, 14L, 20L),
  mult       = c(1.5, 2.0, 2.5),
  exit_mid   = c(TRUE)
)
generate_signals <- function(df, ema_period = 30L, atr_period = 14L,
                             mult = 2.0, exit_mid = TRUE) {
  ema <- TTR::EMA(df$Close, n = ema_period)
  hlc <- as.matrix(df[, .(High, Low, Close)])
  atr <- TTR::ATR(hlc, n = atr_period)[, "atr"]

  upper <- ema + mult * atr
  lower <- ema - mult * atr
  cl <- df$Close

  n <- nrow(df); pos <- numeric(n); state <- 0
  for (i in seq_len(n)) {
    if (is.na(ema[i]) || is.na(atr[i])) { pos[i] <- 0; next }

    if (state == 0) {
      if (cl[i] < lower[i])      state <-  1
      else if (cl[i] > upper[i]) state <- -1
    } else if (state == 1) {
      if (exit_mid && cl[i] >= ema[i])  state <- 0
      else if (cl[i] > upper[i])        state <- -1
    } else if (state == -1) {
      if (exit_mid && cl[i] <= ema[i])  state <- 0
      else if (cl[i] < lower[i])        state <-  1
    }
    pos[i] <- state
  }

  out <- data.table::copy(df)
  out[, EMA := ema]
  out[, ATR := atr]
  out[, Position := pos]
  out
}
