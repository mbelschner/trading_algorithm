# Tokyo Gap & Range – J225 Intraday (5min)
#
# Regime-Switch basierend auf Gap-Groesse bei Session-Open:
#   Gap <= gap_atr_mult * ATR  →  Opening Range Breakout (ORB)
#   Gap >  gap_atr_mult * ATR  →  Gap Fade (Mean Reversion Richtung Vortagesschluss)
#
# ORB:      Long wenn Close > OR-High, Short wenn Close < OR-Low
#           OR = High/Low der ersten 'or_candles' Bars der Session
# Gap Fade: Fade den Gap (entgegen der Luecke), Target = 50% Gap Fill
#           Exit wenn 50% erreicht oder Session-Ende
#
# Session: Tokyo Cash 01:00-07:00 UTC (Winter) / 00:00-06:00 UTC (Sommer)
#          Hier als UTC-Stunden konfigurierbar.
# Force Close: Kein neuer Entry nach last_entry_h, alle Positionen zu session_end_h.
#
# Hinweis: Vektorielles Position-Modell. Keine echten ATR-Stops.
# Fuer finale Validierung den State-Machine-Backtest verwenden.

NAME <- "Tokyo_Gap_Range"

PARAM_GRID <- list(
  or_candles      = c(3L, 4L),
  gap_atr_mult    = c(1.0, 1.5),
  atr_n           = c(14L),
  session_start_h = c(1L),   # UTC Winter (JST = UTC+9, Tokyo Open 00:00 UTC)
  session_end_h   = c(7L),   # UTC Winter
  last_entry_h    = c(6L),   # Kein neuer Entry nach 06:00 UTC
  adx_filter      = c(TRUE)  # ORB nur wenn ADX > 20
)

generate_signals <- function(df,
                              or_candles = 3L, gap_atr_mult = 1.5, atr_n = 14L,
                              session_start_h = 1L, session_end_h = 7L,
                              last_entry_h = 6L, adx_filter = TRUE) {
  if (!"Timestamp" %in% names(df)) {
    out <- data.table::copy(df)
    out[, Position := 0]
    warning("Tokyo_Gap_Range benoetigt Timestamp-Spalte. Position = 0.")
    return(out)
  }

  out <- data.table::copy(df)
  n   <- nrow(out)

  atr_val <- TTR::ATR(out[, .(High, Low, Close)], n = atr_n)[, "atr"]
  adx_val <- if (adx_filter) TTR::ADX(out[, .(High, Low, Close)], n = 14L)[, "ADX"] else rep(99, n)

  hour_utc <- as.integer(format(out$Timestamp, "%H", tz = "UTC"))

  # Hilfsvektoren fuer Session-Tagging
  is_session     <- hour_utc >= session_start_h & hour_utc < session_end_h
  can_enter      <- hour_utc >= session_start_h & hour_utc < last_entry_h
  session_open_bar <- logical(n)  # Erste Bar der Session

  # Erste Bar einer Session markieren
  for (i in 2:n) {
    if (is_session[i] && !is_session[i - 1]) session_open_bar[i] <- TRUE
  }

  pos       <- numeric(n)
  state     <- 0L
  or_high   <- NA_real_
  or_low    <- NA_real_
  or_count  <- 0L
  or_done   <- FALSE
  prev_close <- NA_real_
  regime    <- ""   # "ORB" oder "FADE"
  gap_dir   <- 0L   # +1 Gap-up, -1 Gap-down

  for (i in 2:n) {
    # Session-Ende: Force-Close und Reset
    if (!is_session[i] && is_session[i - 1]) {
      state    <- 0L
      or_high  <- NA_real_; or_low <- NA_real_
      or_count <- 0L; or_done <- FALSE
    }

    # Letzter Close vor Session-Start merken
    if (!is_session[i] && !is_session[i - 1]) {
      prev_close <- out$Close[i]
    }

    if (!is_session[i]) { pos[i] <- state; next }

    # Opening Range aufbauen
    if (!or_done) {
      if (session_open_bar[i]) {
        or_count <- 1L
        or_high  <- out$High[i]
        or_low   <- out$Low[i]

        # Regime bestimmen
        if (!is.na(prev_close) && !is.na(atr_val[i])) {
          gap_size <- abs(out$Open[i] - prev_close)
          if (gap_size > gap_atr_mult * atr_val[i]) {
            regime  <- "FADE"
            gap_dir <- if (out$Open[i] > prev_close) 1L else -1L
          } else {
            regime <- "ORB"
          }
        }
      } else if (or_count > 0L && or_count < or_candles) {
        or_count <- or_count + 1L
        or_high  <- max(or_high, out$High[i])
        or_low   <- min(or_low,  out$Low[i])
        if (or_count == or_candles) or_done <- TRUE
      }
      pos[i] <- state; next
    }

    if (anyNA(c(or_high, or_low, atr_val[i]))) { pos[i] <- state; next }

    adx_ok <- is.na(adx_val[i]) || adx_val[i] > 20

    if (state == 0L && can_enter[i]) {
      if (regime == "ORB") {
        if      (out$Close[i] > or_high && adx_ok) state <-  1L
        else if (out$Close[i] < or_low  && adx_ok) state <- -1L
      } else if (regime == "FADE") {
        # Gap-up → Short (fade aufwaerts Gap), Gap-down → Long
        or_mid <- (or_high + or_low) / 2  # 50% Gap Fill Proxy
        if      (gap_dir ==  1L && out$Close[i] < or_mid) state <- -1L
        else if (gap_dir == -1L && out$Close[i] > or_mid) state <-  1L
      }
    } else if (state != 0L) {
      # Exit bei Gegensignal oder OR-Grenze
      if (state ==  1L && out$Close[i] < or_low)  state <- 0L
      if (state == -1L && out$Close[i] > or_high) state <- 0L
    }

    pos[i] <- state
  }

  out[, Position := pos]
  out
}
