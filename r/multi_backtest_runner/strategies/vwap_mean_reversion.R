# VWAP Mean Reversion (session-anchored)
#
# Archetyp: Reversion zum volumengewichteten Tages-Fairvalue. VWAP ist der
# institutionelle Benchmark; grosse Intraday-Abweichungen werden tendenziell
# zurueckgeholt. Bands = VWAP +/- mult * volumengewichtete Streuung. Einstieg
# beim Band-Bruch, Ausstieg beim Rueck-Cross durch VWAP.
# Stark intraday-spezifisch (Session-Anker, Reset pro Tag) -> bestes Verhalten
# auf 5m-15m bei Indices/liquiden Commodities, wo Volumen aussagekraeftig ist.
#
# WICHTIG: braucht echtes Volumen. Bei CFD-Feeds (z.B. Capital.com) ist Volume
# oft Tick-Volumen/synthetisch -> vorab Datenqualitaet pruefen.
#
# df hat Spalten: Timestamp, Open, High, Low, Close, Volume
NAME <- "VWAP_MeanReversion"
PARAM_GRID <- list(
  mult            = c(1.5, 2.0, 2.5, 3.0),
  min_session_bar = c(3L, 6L),     # erste Bars der Session ueberspringen (VWAP noch instabil)
  flat_overnight  = c(TRUE)        # am Session-Ende glattstellen, kein Overnight-Hold
)
generate_signals <- function(df, mult = 2.0, min_session_bar = 3L, flat_overnight = TRUE) {
  out <- data.table::copy(df)
  out[, .sess := as.Date(Timestamp)]
  out[, .tp   := (High + Low + Close) / 3]

  # session-kumulative VWAP und volumengewichtete Streuung
  out[, .cum_v   := cumsum(Volume),               by = .sess]
  out[, .cum_pv  := cumsum(.tp * Volume),         by = .sess]
  out[, .cum_p2v := cumsum((.tp^2) * Volume),     by = .sess]
  out[, .vwap    := .cum_pv / .cum_v]
  out[, .vwstd   := sqrt(pmax(.cum_p2v / .cum_v - .vwap^2, 0))]
  out[, .barno   := seq_len(.N),                  by = .sess]
  out[, .last    := .I == max(.I),                by = .sess]

  vwap <- out$.vwap; vwstd <- out$.vwstd
  upper <- vwap + mult * vwstd
  lower <- vwap - mult * vwstd
  cl <- out$Close; barno <- out$.barno; lastb <- out$.last

  n <- nrow(out); pos <- numeric(n); state <- 0
  for (i in seq_len(n)) {
    skip <- barno[i] < min_session_bar || is.na(vwap[i]) ||
            is.na(vwstd[i]) || vwstd[i] == 0
    if (!skip) {
      if (state == 0) {
        if (cl[i] < lower[i])      state <-  1
        else if (cl[i] > upper[i]) state <- -1
      } else if (state == 1) {
        if (cl[i] >= vwap[i])      state <-  0
      } else if (state == -1) {
        if (cl[i] <= vwap[i])      state <-  0
      }
    }
    if (flat_overnight && lastb[i]) state <- 0   # Session-Ende: glattstellen
    pos[i] <- state
  }

  out[, VWAP := vwap]
  out[, Position := pos]
  out[, c(".sess", ".tp", ".cum_v", ".cum_pv", ".cum_p2v",
          ".vwap", ".vwstd", ".barno", ".last") := NULL]
  out
}
