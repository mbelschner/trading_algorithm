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



# Keltner Squeeze Breakout — Volatilitäts-Kompression -> Expansion
#
# Archetyp: TTM-Squeeze-Logik. Wenn die Bollinger-Bänder INNERHALB der
# Keltner-Kanäle liegen (bb_u<kc_u & bb_l>kc_l), ist die Vola komprimiert
# (Squeeze). Löst sich der Squeeze, wird in Richtung der Mittellinien-
# Abweichung (Close vs SMA) eingestiegen; Exit beim Rück-Cross durch die
# Mittellinie. Commodities durchlaufen häufig enge Konsolidierungen vor
# impulsiven Ausbrüchen -> der Squeeze-Filter timt genau diese Expansion.
#
# df: Timestamp, Open, High, Low, Close, Volume
NAME <- "Keltner_Squeeze_Breakout"
PARAM_GRID <- list(
  period      = c(20L, 30L, 50L),
  bb_mult     = c(2.0),
  kc_mult     = c(1.0, 1.5),
  atr_period  = c(10L, 14L),
  sl_atr_mult = c(1.5, 2.5),
  tp_atr_mult = c(2.0, 3.0)
)
generate_signals <- function(df, period = 20L, bb_mult = 2.0, kc_mult = 1.5,
                             atr_period = 14L, sl_atr_mult = 1.5, tp_atr_mult = 2.0) {
  out <- data.table::copy(df)
  n   <- nrow(out)
  cl  <- out$Close

  ma     <- TTR::SMA(cl, n = period)
  sdv    <- TTR::runSD(cl, n = period)
  atr_kc <- TTR::ATR(cbind(out$High, out$Low, out$Close), n = period)[, "atr"]

  bb_u <- ma + bb_mult * sdv; bb_l <- ma - bb_mult * sdv
  kc_u <- ma + kc_mult * atr_kc; kc_l <- ma - kc_mult * atr_kc
  squeeze_on <- (bb_u < kc_u) & (bb_l > kc_l)

  pos <- integer(n); state <- 0L
  for (i in seq_len(n)) {
    if (is.na(ma[i]) || is.na(sdv[i]) || is.na(atr_kc[i])) { pos[i] <- state; next }
    released <- i > 1L && isTRUE(!squeeze_on[i]) && isTRUE(squeeze_on[i - 1L])
    if (state == 0L) {
      if (released) state <- if (cl[i] > ma[i]) 1L else -1L
    } else if (state == 1L) {
      if (cl[i] < ma[i]) state <- 0L
    } else if (state == -1L) {
      if (cl[i] > ma[i]) state <- 0L
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
