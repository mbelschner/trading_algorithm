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
  stc_entry_short = c(75)
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
                              stc_entry_long = 25, stc_entry_short = 75) {
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

  out[, Position := pos]
  out
}
