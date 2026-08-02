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


# CMO-VHF-STC – Trend-Filter + Momentum + Timing
#
# Setup:  VHF > threshold  (Markt trendet)
#         CMO > +threshold (bullishes Momentum) / < -threshold (bearish)
# Entry:  STC kreuzt ueber entry_long (25) → Long
#         STC kreuzt unter entry_short (75) → Short
# Bestaetigung: Close > Close[1] fuer Long, < Close[1] fuer Short
# Exit:   Gegensignal oder STC dreht in Gegenzone

NAME <- "CMO_VHF_STC"

PARAM_GRID <- list(
  cmo_period      = c(14L, 20L),
  cmo_threshold   = c(15, 20),
  vhf_period      = c(28L),
  vhf_threshold   = c(0.30, 0.35),
  stc_fast        = c(23L),
  stc_slow        = c(50L),
  stc_cycle       = c(10L),
  stc_entry_long  = c(25),
  stc_entry_short = c(75),
  atr_period      = c(10L, 14L),
  sl_atr_mult     = c(1.5, 2.0, 2.5),
  tp_atr_mult     = c(2.0, 3.0, 4.0)
)

# --- Hilfsfunktionen ---

.cmo <- function(x, n) {
  # Chande Momentum Oscillator: (sum_up - sum_dn) / (sum_up + sum_dn) * 100
  d    <- diff(x); d <- c(NA, d)
  up   <- ifelse(d > 0, d, 0)
  dn   <- ifelse(d < 0, -d, 0)
  s_up <- TTR::runSum(up, n)
  s_dn <- TTR::runSum(dn, n)
  cmo  <- (s_up - s_dn) / (s_up + s_dn + 1e-12) * 100
  cmo
}

.vhf <- function(x, n) {
  # Vertical Horizontal Filter
  hh  <- TTR::runMax(x, n)
  ll  <- TTR::runMin(x, n)
  num <- abs(hh - ll)
  den <- TTR::runSum(abs(c(NA, diff(x))), n)
  num / (den + 1e-12)
}

.stc <- function(x, fast = 23L, slow = 50L, cycle = 10L) {
  # Schaff Trend Cycle
  macd_line <- TTR::EMA(x, fast) - TTR::EMA(x, slow)
  macd_line[is.na(macd_line)] <- 0

  k1_high <- TTR::runMax(macd_line, cycle)
  k1_low  <- TTR::runMin(macd_line, cycle)
  k1      <- (macd_line - k1_low) / (k1_high - k1_low + 1e-12) * 100
  d1      <- TTR::EMA(k1, 3)

  k2_high <- TTR::runMax(d1, cycle)
  k2_low  <- TTR::runMin(d1, cycle)
  k2      <- (d1 - k2_low) / (k2_high - k2_low + 1e-12) * 100
  TTR::EMA(k2, 3)
}

generate_signals <- function(df,
                              cmo_period = 14L, cmo_threshold = 20,
                              vhf_period = 28L, vhf_threshold = 0.35,
                              stc_fast = 23L, stc_slow = 50L, stc_cycle = 10L,
                              stc_entry_long = 25, stc_entry_short = 75,
                              atr_period = 14L, sl_atr_mult = 1.5, tp_atr_mult = 2.0) {
  out <- data.table::copy(df)
  n   <- nrow(out)
  cl  <- out$Close

  cmo_val <- .cmo(cl, cmo_period)
  vhf_val <- .vhf(cl, vhf_period)
  stc_val <- .stc(cl, stc_fast, stc_slow, stc_cycle)

  pos   <- numeric(n)
  state <- 0L

  for (i in 2:n) {
    if (anyNA(c(cmo_val[i], vhf_val[i], stc_val[i], stc_val[i - 1]))) {
      pos[i] <- state; next
    }

    trending        <- vhf_val[i] > vhf_threshold
    bull_momentum   <- cmo_val[i] >  cmo_threshold
    bear_momentum   <- cmo_val[i] < -cmo_threshold
    stc_cross_up    <- stc_val[i - 1] < stc_entry_long  && stc_val[i] >= stc_entry_long
    stc_cross_dn    <- stc_val[i - 1] > stc_entry_short && stc_val[i] <= stc_entry_short
    price_confirm_l <- cl[i] > cl[i - 1]
    price_confirm_s <- cl[i] < cl[i - 1]

    long_signal  <- trending && bull_momentum && stc_cross_up  && price_confirm_l
    short_signal <- trending && bear_momentum && stc_cross_dn  && price_confirm_s

    # Exit: STC dreht in neutrale Zone oder Gegensignal
    exit_long  <- stc_val[i] > stc_entry_short || short_signal
    exit_short <- stc_val[i] < stc_entry_long  || long_signal

    if (state == 0L) {
      if      (long_signal)  state <-  1L
      else if (short_signal) state <- -1L
    } else if (state ==  1L) {
      if (exit_long)  state <- if (short_signal) -1L else 0L
    } else if (state == -1L) {
      if (exit_short) state <- if (long_signal)   1L else 0L
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
