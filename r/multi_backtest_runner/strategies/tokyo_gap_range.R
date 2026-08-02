# =============================================================================
# _fsm_barriers.R  —  Gemeinsamer SL/TP/Overnight-Applikator
# =============================================================================
# Wandelt einen Wunsch-Positionsvektor `sig` (-1/0/+1) in eine ausführbare
# Position um, indem SL/TP/Overnight als ECHTE Zustandsmaschine überlagert
# werden — sequentiell, nicht als vektorisierter Second-Pass.
#
# Behebt die zwei Bugs des entry_bar/LOCF-Ansatzes:
#   1. Position lebt nach einem Stop nicht wieder auf (Zombie-Re-Entry).
#   2. Barrieren werden bei jedem Entry FRISCH gesetzt (keine stale LOCF-Levels).
#
# Policy:
#   - Overnight (UTC-Tageswechsel): Position glattstellen.
#       overnight_lockout = FALSE (Default): nächster Tag darf sofort wieder
#         einsteigen, wenn das Rohsignal noch aktiv ist (für MR sinnvoll).
#       overnight_lockout = TRUE: gesperrt bis das Rohsignal die Richtung
#         verlässt (für Breakout-Strategien mit LOCF-Halten, kein Carry).
#   - SL/TP-Hit: glattstellen + Lockout der gestoppten Richtung, bis das
#       Rohsignal sie verlässt -> verhindert sofortiges Wieder-Ausstoppen
#       am selben Level.
#   - SL hat Vorrang vor TP bei gleichzeitigem Intrabar-Treffer (konservativ).
#
# Diese Funktion ist in jeder Strategie-Datei eingebettet (guarded), sodass
# die Dateien plugin-eigenständig bleiben. Bei Mehrfach-Source wird sie nur
# einmal definiert.
# =============================================================================

if (!exists(".apply_sl_tp_fsm")) {
  .apply_sl_tp_fsm <- function(sig, High, Low, Close, atr_vec, new_day,
                               sl_atr_mult, tp_atr_mult,
                               allow_reversal = TRUE,
                               overnight_lockout = FALSE) {
    n      <- length(sig)
    pos    <- integer(n)
    sl_out <- rep(NA_real_, n)
    tp_out <- rep(NA_real_, n)

    state    <- 0L
    sl_level <- NA_real_
    tp_level <- NA_real_
    locked   <- 0L   # Richtung, die nach Stop/TP/Overnight gesperrt ist

    for (i in seq_len(n)) {
      s  <- sig[i]; if (is.na(s)) s <- 0L
      a  <- atr_vec[i]
      px <- Close[i]

      # 1. Overnight-Glattstellung
      if (new_day[i] && state != 0L) {
        if (overnight_lockout) locked <- state
        state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
      }

      # 2. SL/TP-Check (SL Vorrang)
      if (state == 1L) {
        if (!is.na(sl_level) && Low[i] <= sl_level) {
          locked <- 1L; state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
        } else if (!is.na(tp_level) && High[i] >= tp_level) {
          locked <- 1L; state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
        }
      } else if (state == -1L) {
        if (!is.na(sl_level) && High[i] >= sl_level) {
          locked <- -1L; state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
        } else if (!is.na(tp_level) && Low[i] <= tp_level) {
          locked <- -1L; state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
        }
      }

      # 3. Lockout aufheben, sobald das Rohsignal die Richtung verlässt
      if (locked != 0L && s != locked) locked <- 0L

      # 4. Entry / Exit / Reversal anhand des Rohsignals
      if (state == 0L) {
        if (s != 0L && s != locked && !is.na(a)) {
          state <- s
          if (state == 1L) {
            sl_level <- px - sl_atr_mult * a; tp_level <- px + tp_atr_mult * a
          } else {
            sl_level <- px + sl_atr_mult * a; tp_level <- px - tp_atr_mult * a
          }
        }
      } else {
        if (s == 0L) {
          state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
        } else if (s != state) {
          if (allow_reversal && !is.na(a)) {
            state <- s
            if (state == 1L) {
              sl_level <- px - sl_atr_mult * a; tp_level <- px + tp_atr_mult * a
            } else {
              sl_level <- px + sl_atr_mult * a; tp_level <- px - tp_atr_mult * a
            }
          } else {
            state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
          }
        }
      }

      pos[i] <- state
      if (state != 0L) { sl_out[i] <- sl_level; tp_out[i] <- tp_level }
    }

    list(Position = pos, SL = sl_out, TP = tp_out)
  }
}


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
  session_start_h = c(1L),
  session_end_h   = c(7L),
  last_entry_h    = c(6L),
  adx_filter      = c(TRUE),
  atr_period      = c(10L, 14L),
  sl_atr_mult     = c(1.5, 2.0, 2.5),
  tp_atr_mult     = c(2.0, 3.0, 4.0)
)

generate_signals <- function(df,
                              or_candles = 3L, gap_atr_mult = 1.5, atr_n = 14L,
                              session_start_h = 1L, session_end_h = 7L,
                              last_entry_h = 6L, adx_filter = TRUE,
                              atr_period = 14L, sl_atr_mult = 1.5, tp_atr_mult = 2.0) {
  if (!"Timestamp" %in% names(df)) {
    out <- data.table::copy(df)
    out[, Position := 0L]
    out[, SL := NA_real_]
    out[, TP := NA_real_]
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
  regime    <- ""
  gap_dir   <- 0L

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

  out[, Position := as.integer(pos)]

  # ── SL / TP / Overnight via FSM-Barrier-Applikator ────────────────────────
  atr_vec <- TTR::ATR(
    HLC    = cbind(out$High, out$Low, out$Close),
    n      = atr_period,
    maType = "EMA"
  )[, "atr"]

  day_utc <- as.integer(format(out$Timestamp, "%d", tz = "UTC"))
  new_day <- c(FALSE, day_utc[-1L] != day_utc[-length(day_utc)])

  sig <- as.integer(out$Position)
  sig[is.na(atr_vec)] <- 0L

  res <- .apply_sl_tp_fsm(
    sig         = sig,
    High        = out$High,
    Low         = out$Low,
    Close       = out$Close,
    atr_vec     = atr_vec,
    new_day     = new_day,
    sl_atr_mult = sl_atr_mult,
    tp_atr_mult = tp_atr_mult,
    allow_reversal    = TRUE,
    overnight_lockout = FALSE
  )

  out[, Position := res$Position]
  out[, SL       := res$SL]
  out[, TP       := res$TP]
  out
}
