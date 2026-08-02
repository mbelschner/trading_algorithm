# SMA Crossover - klassische Trend-Following Strategie.

NAME <- "SMA_Crossover"

PARAM_GRID <- list(
  fast        = c(10L, 20L, 30L),
  slow        = c(40L, 50L, 80L, 100L, 200L),
  atr_period  = c(10L, 14L),
  sl_atr_mult = c(1.5, 2.0, 2.5),
  tp_atr_mult = c(2.0, 3.0, 4.0)
)

generate_signals <- function(df, fast = 20L, slow = 50L,
                             atr_period = 14L, sl_atr_mult = 1.5,
                             tp_atr_mult = 2.0) {
  if (fast >= slow) {
    out <- data.table::copy(df)
    out[, `:=`(Position = 0L, SL = NA_real_, TP = NA_real_)]
    return(out)
  }

  out <- data.table::copy(df)
  out[, SMA_fast := TTR::SMA(Close, n = fast)]
  out[, SMA_slow := TTR::SMA(Close, n = slow)]

  # Rohes SMA-Signal
  out[, sma_signal := data.table::fifelse(
    SMA_fast > SMA_slow,  1L,
    data.table::fifelse(SMA_fast < SMA_slow, -1L, 0L)
  )]
  out[is.na(SMA_slow), sma_signal := 0L]

  # ATR (bestimmt Barrieren-Distanz)
  atr_vec <- TTR::ATR(
    HLC    = cbind(out$High, out$Low, out$Close),
    n      = atr_period,
    maType = "EMA"
  )[, "atr"]
  out[is.na(atr_vec), sma_signal := 0L]

  # Overnight-Maske (UTC-Tag-Wechsel)
  day_utc <- as.integer(format(out$Timestamp, "%d", tz = "UTC"))
  new_day <- c(FALSE, day_utc[-1L] != day_utc[-length(day_utc)])

  # Pre-extract Vektoren (kein $-Lookup im Loop)
  n   <- nrow(out)
  sig <- out$sma_signal
  hi  <- out$High
  lo  <- out$Low
  cl  <- out$Close

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

    # 2. SL/TP-Check (SL hat Vorrang bei gleichzeitigem Treffer)
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

    # 3. Entry / Richtungswechsel / Re-Entry basierend auf SMA-Signal
    #    Nach SL-Hit: sofortiger Re-Entry möglich wenn Signal noch aktiv,
    #    mit FRISCHEN Barrieren auf Basis des aktuellen Bars
    new_sig <- sig[i]
    if (new_sig != 0L && new_sig != state) {
      state <- new_sig
      if (!is.na(atr_vec[i])) {
        if (state == 1L) {
          sl_level <- cl[i] - sl_atr_mult * atr_vec[i]
          tp_level <- cl[i] + tp_atr_mult * atr_vec[i]
        } else {
          sl_level <- cl[i] + sl_atr_mult * atr_vec[i]
          tp_level <- cl[i] - tp_atr_mult * atr_vec[i]
        }
      } else {
        sl_level <- NA_real_; tp_level <- NA_real_
      }
    } else if (new_sig == 0L && state != 0L) {
      # SMA-Kreuzung verschwunden → glätten
      state <- 0L; sl_level <- NA_real_; tp_level <- NA_real_
    }

    pos[i]    <- state
    sl_out[i] <- if (state != 0L) sl_level else NA_real_
    tp_out[i] <- if (state != 0L) tp_level else NA_real_
  }
  # ── Ende Loop ────────────────────────────────────────────────────────────

  out[, `:=`(Position = pos, SL = sl_out, TP = tp_out)]
  out[, sma_signal := NULL]
  out
}
