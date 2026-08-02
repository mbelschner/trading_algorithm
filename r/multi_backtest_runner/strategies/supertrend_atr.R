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



# Supertrend (ATR-Trend) — Commodity-Trendfolge
#
# Archetyp: ATR-getrailter Trendfilter. Die Supertrend-Linie liegt mult*ATR
# unter (Long) bzw. über (Short) dem HL2-Median und kippt erst bei einem
# klaren Gegenbruch. Commodities (Gold/Silber/Brent) zeigen ausgeprägte,
# volatilitätsskalierende Trends -> ATR-Trailing passt sich an Regimewechsel
# an, ohne fixe Punktdistanzen. Das Rohsignal ist immer long ODER short
# (kein Flat); SL/TP/Overnight werden vom FSM-Applikator überlagert.
#
# df: Timestamp, Open, High, Low, Close, Volume
NAME <- "Supertrend_ATR"
PARAM_GRID <- list(
  st_period   = c(10L, 14L, 20L),
  st_mult     = c(2.0, 3.0, 4.0),
  atr_period  = c(10L, 14L),
  sl_atr_mult = c(1.5, 2.5),
  tp_atr_mult = c(2.0, 3.0)
)
generate_signals <- function(df, st_period = 10L, st_mult = 3.0,
                             atr_period = 14L, sl_atr_mult = 1.5, tp_atr_mult = 2.0) {
  out <- data.table::copy(df)
  n   <- nrow(out)
  cl  <- out$Close
  hl2 <- (out$High + out$Low) / 2
  atr_st <- TTR::ATR(cbind(out$High, out$Low, out$Close), n = st_period)[, "atr"]

  up_basic <- hl2 + st_mult * atr_st
  dn_basic <- hl2 - st_mult * atr_st

  fub <- rep(NA_real_, n); flb <- rep(NA_real_, n)
  strend <- rep(NA_real_, n); dir <- integer(n)

  for (i in seq_len(n)) {
    if (is.na(atr_st[i])) { dir[i] <- 0L; next }
    bu <- up_basic[i]; bl <- dn_basic[i]
    if (i == 1L || is.na(fub[i - 1L])) {
      fub[i] <- bu; flb[i] <- bl; strend[i] <- bu; dir[i] <- -1L
    } else {
      fub[i] <- if (bu < fub[i - 1L] || cl[i - 1L] > fub[i - 1L]) bu else fub[i - 1L]
      flb[i] <- if (bl > flb[i - 1L] || cl[i - 1L] < flb[i - 1L]) bl else flb[i - 1L]
      if (strend[i - 1L] == fub[i - 1L]) {
        if (cl[i] <= fub[i]) { strend[i] <- fub[i]; dir[i] <- -1L }
        else                 { strend[i] <- flb[i]; dir[i] <-  1L }
      } else {
        if (cl[i] >= flb[i]) { strend[i] <- flb[i]; dir[i] <-  1L }
        else                 { strend[i] <- fub[i]; dir[i] <- -1L }
      }
    }
  }

  out[, Supertrend := strend]
  out[, Position   := as.integer(dir)]

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
