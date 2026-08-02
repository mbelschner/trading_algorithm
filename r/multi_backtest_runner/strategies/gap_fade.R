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


# =============================================================================
# Gap Fade Intraday v4 — R Plugin
# Converted from PineScript v6
#
# Strategy Logic:
#   1. At i_prev_close_h (UTC): capture the closing price as the reference
#   2. At session open (i_sess_start_h:i_sess_start_m UTC): compute gap % vs prev close
#      - Gap UP   >= gap_thresh% → SHORT (fade the gap)
#      - Gap DOWN <= -gap_thresh% → LONG  (fade the gap)
#   3. Enter on the NEXT bar after the signal bar (open of that bar)
#   4. Exit via: TP at tp_pct%, SL at sl_pct%, or Time Exit at exit_h:exit_m UTC
#
# NOTE — Exits (TP/SL/Time) are not enforced in the signal layer.
#   Position holds until counter-signal or end of data.
#   The backtester must apply TP/SL/time-exit logic externally.
#   TP and SL levels are included as diagnostic columns for that purpose.
# =============================================================================

NAME <- "GapFadeIntradayV4"

PARAM_GRID <- list(
  # Session timing (UTC hours/minutes)
  prev_close_h  = c(21),
  sess_start_h  = c(2, 9),
  sess_start_m  = c(0),
  exit_h        = c(15, 17),
  exit_m        = c(0),

  # Strategy parameters
  gap_thresh    = c(0.3, 0.5, 1.0),
  tp_pct        = c(0.2, 0.3, 0.4),
  sl_pct        = c(0.5, 0.8, 1.0),
  direction     = c("Both"),

  # ATR-based SL/TP barriers (second-pass)
  atr_period    = c(14L),
  sl_atr_mult   = c(2.0, 2.5),
  tp_atr_mult   = c( 3.0, 4.0)
)


# =============================================================================
# MAIN SIGNAL FUNCTION
# =============================================================================
generate_signals <- function(
    df,

    # Session timing (UTC)
    prev_close_h  = 21,
    sess_start_h  = 9,
    sess_start_m  = 0,
    exit_h        = 15,
    exit_m        = 0,

    # Strategy
    gap_thresh    = 0.7,
    tp_pct        = 0.4,
    sl_pct        = 1.0,
    direction     = "Both",

    # ATR-based barriers
    atr_period    = 14L,
    sl_atr_mult   = 1.5,
    tp_atr_mult   = 2.0
) {
  # ---------------------------------------------------------------------------
  # 0. Setup
  # ---------------------------------------------------------------------------
  out <- data.table::copy(df)

  if (!inherits(out$Timestamp, "POSIXct")) {
    out[, Timestamp := as.POSIXct(Timestamp)]
  }

  ts_utc <- lubridate::with_tz(out$Timestamp, "UTC")

  bar_h   <- as.integer(format(ts_utc, "%H"))
  bar_m   <- as.integer(format(ts_utc, "%M"))
  bar_min <- bar_h * 60L + bar_m
  bar_date <- as.Date(ts_utc)

  N <- nrow(out)

  sess_start_min <- sess_start_h * 60L + sess_start_m
  exit_min_val   <- exit_h * 60L + exit_m

  # ---------------------------------------------------------------------------
  # 1. Capture reference close
  # ---------------------------------------------------------------------------
  unique_dates <- sort(unique(bar_date))
  day_ref_close <- setNames(rep(NA_real_, length(unique_dates)), as.character(unique_dates))

  for (d in as.character(unique_dates)) {
    idx <- which(as.character(bar_date) == d & bar_h == prev_close_h)
    if (length(idx) > 0) {
      day_ref_close[d] <- out$Close[max(idx)]
    }
  }

  prev_close_for_day <- function(d) {
    d_char <- as.character(d)
    d_idx  <- which(as.character(unique_dates) == d_char)
    if (d_idx <= 1) return(NA_real_)
    prev_d <- as.character(unique_dates[d_idx - 1])
    day_ref_close[prev_d]
  }

  # ---------------------------------------------------------------------------
  # 2. Detect session open bar and compute gap signal
  # ---------------------------------------------------------------------------
  is_session_open <- bar_min == sess_start_min

  signal_raw    <- integer(N)
  gap_pct_vec   <- rep(NA_real_, N)
  tp_level_vec  <- rep(NA_real_, N)
  sl_level_vec  <- rep(NA_real_, N)

  traded_today <- rep(FALSE, N)

  for (i in seq_len(N)) {
    d <- bar_date[i]

    if (i > 1 && bar_date[i] == bar_date[i - 1] && traded_today[i - 1]) {
      traded_today[i] <- TRUE
    }

    if (is_session_open[i] && !traded_today[i]) {
      prev_cl <- prev_close_for_day(d)

      if (!is.na(prev_cl) && prev_cl > 0) {
        gap <- (out$Open[i] - prev_cl) / prev_cl * 100
        gap_pct_vec[i] <- gap

        if (gap >= gap_thresh && direction %in% c("Both", "Short Only")) {
          signal_raw[i]    <- -1L
          tp_level_vec[i]  <- out$Open[i] * (1 - tp_pct / 100)
          sl_level_vec[i]  <- out$Open[i] * (1 + sl_pct / 100)
          traded_today[i]  <- TRUE
        } else if (gap <= -gap_thresh && direction %in% c("Both", "Long Only")) {
          signal_raw[i]    <- 1L
          tp_level_vec[i]  <- out$Open[i] * (1 + tp_pct / 100)
          sl_level_vec[i]  <- out$Open[i] * (1 - sl_pct / 100)
          traded_today[i]  <- TRUE
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 3. NO LOOK-AHEAD SHIFT
  # ---------------------------------------------------------------------------
  signal_shifted  <- data.table::shift(signal_raw,   1L, fill = 0L,        type = "lag")
  tp_level_shift  <- data.table::shift(tp_level_vec, 1L, fill = NA_real_,  type = "lag")
  sl_level_shift  <- data.table::shift(sl_level_vec, 1L, fill = NA_real_,  type = "lag")
  gap_pct_shift   <- data.table::shift(gap_pct_vec,  1L, fill = NA_real_,  type = "lag")

  # ---------------------------------------------------------------------------
  # 4. Build Position vector
  # ---------------------------------------------------------------------------
  position    <- integer(N)
  current_pos <- 0L

  for (i in seq_len(N)) {
    sig <- signal_shifted[i]

    if (sig == 1L && current_pos != 1L) {
      current_pos <- 1L
    } else if (sig == -1L && current_pos != -1L) {
      current_pos <- -1L
    }

    position[i] <- current_pos
  }

  # ---------------------------------------------------------------------------
  # 5. Attach original diagnostic columns
  # ---------------------------------------------------------------------------
  out[, Position := position]
  out[, TP_Level := tp_level_shift]
  out[, SL_Level := sl_level_shift]
  out[, Gap_Pct  := gap_pct_shift]
  out[, Exit_Min := exit_min_val]

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
