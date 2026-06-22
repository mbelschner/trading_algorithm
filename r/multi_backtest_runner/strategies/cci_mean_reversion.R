# CCI Mean Reversion
#
# Archetyp: Reversion einer um die mittlere absolute Abweichung normierten
# Abweichung der Typical Price vom gleitenden Mittel. Der CCI wurde
# urspruenglich fuer Rohstoffe entworfen -> natuerlicher Fit fuer Commodities.
# Einstieg bei |CCI| > threshold, Ausstieg an der Nulllinie.
# Unterscheidet sich vom z-Score durch HLC-Typical-Price und MAD-Normierung
# (robuster gegen einzelne Ausreisser-Closes). 5m-1H.
#
# df hat Spalten: Timestamp, Open, High, Low, Close, Volume
NAME <- "CCI_MeanReversion"
PARAM_GRID <- list(
  period     = c(14L, 20L, 30L, 40L),
  threshold  = c(100, 150, 200),
  exit_level = c(0)      # 0 = exit an Nulllinie; >0 = etwas frueher
)
generate_signals <- function(df, period = 20L, threshold = 100, exit_level = 0) {
  hlc <- as.matrix(df[, .(High, Low, Close)])
  cci <- TTR::CCI(hlc, n = period, c = 0.015)

  n <- nrow(df); pos <- numeric(n); state <- 0
  for (i in seq_len(n)) {
    ci <- cci[i]
    if (is.na(ci)) { pos[i] <- 0; next }

    if (state == 0) {
      if (ci < -threshold)      state <-  1
      else if (ci >  threshold) state <- -1
    } else if (state == 1) {
      if (ci >= -exit_level)    state <-  0
      if (ci >   threshold)     state <- -1
    } else if (state == -1) {
      if (ci <=  exit_level)    state <-  0
      if (ci <  -threshold)     state <-  1
    }
    pos[i] <- state
  }

  out <- data.table::copy(df)
  out[, CCI := cci]
  out[, Position := pos]
  out
}
