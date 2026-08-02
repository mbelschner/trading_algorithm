# Toyota Intraday Breakout – N-Bar Channel
#
# Entry Long:  Close > N-Bar High (der vorigen Bar, kein Look-Ahead)
# Entry Short: Close < N-Bar Low
# Exit:        Gegensignal (oder SL/TP/Overnight aus Second-Pass)
#
# Originalstrategie hatte ATR-SL (1.5x) + ATR-TP (3.0x) und Session-Filter
# (08:00-22:00). Im vektoriellen Framework wird auf Gegensignal exitiert.
# Session-Filter benoetigt Timestamp-Awareness – er wird hier als optionaler
# Parameter mitgegeben und greift nur wenn Timestamp-Spalte vorhanden.
#
# Instrumente laut Original: DE40, J225, Gold (5min / 15min)
#
# IMPLEMENTIERUNGSHINWEIS: SL/TP erfordern einen iterativen FSM-Loop, da das
# Channel-Signal nach einem SL-Hit aktiv bleibt (kein in-strategy Exit).
# Der LOCF-Ansatz würde veraltete Barrieren nach Re-Entries produzieren.

NAME <- "Toyota_Breakout"

PARAM_GRID <- list(
  lookback     = c(3L, 5L, 7L, 10L),
  atr_n        = c(14L),
  session_only = c(FALSE),
  atr_period   = c(10L, 14L),
  sl_atr_mult  = c(1.5, 2.0, 2.5),
  tp_atr_mult  = c(2.0, 3.0, 4.0)
)

generate_signals <- function(df, lookback = 5L, atr_n = 14L,
                              session_only = FALSE,
                              atr_period = 14L, sl_atr_mult = 1.5,
                              tp_atr_mult = 2.0) {
  out <- data.table::copy(df)
  n   <- nrow(out)

  # Channel High/Low mit shift(1) -> kein Look-Ahead
  ch_high <- data.table::shift(TTR::runMax(out$High, lookback), 1)
  ch_low  <- data.table::shift(TTR::runMin(out$Low,  lookback), 1)

  # Optionaler Session-Filter (08:00–22:00 UTC)
  in_session <- rep(TRUE, n)
  if (session_only && "Timestamp" %in% names(out)) {
    h <- as.integer(format(out$Timestamp, "%H", tz = "UTC"))
    in_session <- h >= 8L & h < 22L
  }

  # ATR (bestimmt Barrieren-Distanz)
  atr_vec <- TTR::ATR(
    HLC    = cbind(out$High, out$Low, out$Close),
    n      = atr_period,
    maType = "EMA"
  )[, "atr"]

  # Overnight-Maske (UTC-Tag-Wechsel)
  day_utc <- as.integer(format(out$Timestamp, "%d", tz = "UTC"))
  new_day <- c(FALSE, day_utc[-1L] != day_utc[-length(day_utc)])

  # Pre-extract Vektoren (kein $-Lookup im Loop)
  hi  <- out$High
  lo  <- out$Low
  cl  <- out$Close
  ch_h <- ch_high
  ch_l <- ch_low

  # Output-Vektoren
  pos    <- integer(n)
  sl_out <- numeric(n)
  tp_out <- numeric(n)

  state    <- 0L
  sl_level <- NA_real_
  tp_level <- NA_real_

  # ── Iterativer FSM-Pass ──────────────────────────────────────────────────
  for (i in seq_len(n)) {

    # 1. Overnight-Exit
    if (new_day[i] && state != 0L) {
      state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
    }

    # 2. SL/TP-Check (SL hat Vorrang)
    if (state == 1L) {
      if (!is.na(sl_level) && lo[i] <= sl_level) {
        state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
      } else if (!is.na(tp_level) && hi[i] >= tp_level) {
        state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
      }
    } else if (state == -1L) {
      if (!is.na(sl_level) && hi[i] >= sl_level) {
        state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
      } else if (!is.na(tp_level) && lo[i] <= tp_level) {
        state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
      }
    }

    # 3. Channel-Signal auswerten (kein Look-Ahead durch ch_high/ch_low Shift)
    if (is.na(ch_h[i]) || is.na(ch_l[i])) {
      pos[i]    <- state
      sl_out[i] <- if (state != 0L) sl_level else NA_real_
      tp_out[i] <- if (state != 0L) tp_level else NA_real_
      next
    }

    long_signal  <- cl[i] > ch_h[i] && in_session[i]
    short_signal <- cl[i] < ch_l[i] && in_session[i]

    # Entry / Re-Entry / Richtungswechsel:
    #   Toyota hält Position bis Gegensignal — kein Exit wenn Signal verschwindet.
    #   Nach SL/TP-Hit (state=0): Re-Entry sobald Channel-Signal wieder aktiv.
    if (long_signal && state != 1L) {
      state <- 1L
      if (!is.na(atr_vec[i])) {
        sl_level <- cl[i] - sl_atr_mult * atr_vec[i]
        tp_level <- cl[i] + tp_atr_mult * atr_vec[i]
      } else {
        sl_level <- NA_real_; tp_level <- NA_real_
      }
    } else if (short_signal && state != -1L) {
      state <- -1L
      if (!is.na(atr_vec[i])) {
        sl_level <- cl[i] + sl_atr_mult * atr_vec[i]
        tp_level <- cl[i] - tp_atr_mult * atr_vec[i]
      } else {
        sl_level <- NA_real_; tp_level <- NA_real_
      }
    }

    pos[i]    <- state
    sl_out[i] <- if (state != 0L) sl_level else NA_real_
    tp_out[i] <- if (state != 0L) tp_level else NA_real_
  }
  # ── Ende Loop ────────────────────────────────────────────────────────────

  out[, `:=`(Position = pos, SL = sl_out, TP = tp_out)]
  out
}
