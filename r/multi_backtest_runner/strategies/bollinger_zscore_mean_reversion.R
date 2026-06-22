# Bollinger Z-Score Mean Reversion
#
# Archetyp: statistische Reversion. Standardisiert den Preis relativ zum
# gleitenden Mittel in Einheiten der rollierenden Streuung (z-Score). Einstieg
# bei Ueberdehnung (|z| > entry_z), Ausstieg bei Rueckkehr zum Mittel.
# Oekonomische Begruendung: Ornstein-Uhlenbeck-artige Reversion einer
# volatilitaets-normierten Abweichung. Universell ueber Indices/Commodities,
# 5m-1H. Vorteil ggue. RSI: misst Abweichung in sd-Einheiten, nicht Momentum.
#
# df hat Spalten: Timestamp, Open, High, Low, Close, Volume
NAME <- "Bollinger_ZScore_MR"
PARAM_GRID <- list(
  period   = c(15L, 20L, 30L, 50L),
  entry_z  = c(2.0, 2.5, 3.0),
  exit_z   = c(0.0, 0.5)   # 0 = exit an Nulllinie; >0 = etwas frueher (z noch auf Einstiegsseite)
)
generate_signals <- function(df, period = 20L, entry_z = 2.0, exit_z = 0.0) {
  ma  <- TTR::SMA(df$Close, n = period)
  sdv <- TTR::runSD(df$Close, n = period)
  z   <- (df$Close - ma) / sdv

  n <- nrow(df); pos <- numeric(n); state <- 0
  for (i in seq_len(n)) {
    zi <- z[i]; si <- sdv[i]
    if (is.na(zi) || is.na(si) || si == 0) { pos[i] <- state; next }

    if (state == 0) {
      if (zi <= -entry_z)      state <-  1
      else if (zi >=  entry_z) state <- -1
    } else if (state == 1) {              # long: warte auf Rueckkehr zum Mittel
      if (zi >= -exit_z)       state <-  0
      if (zi >=  entry_z)      state <- -1  # direkter Flip bei Gegen-Extrem
    } else if (state == -1) {             # short
      if (zi <=  exit_z)       state <-  0
      if (zi <= -entry_z)      state <-  1
    }
    pos[i] <- state
  }

  out <- data.table::copy(df)
  out[, ZScore := z]
  out[, Position := pos]
  out
}
