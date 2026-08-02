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
# Silver Asia Range Breakout v4 — R Plugin
# Converted from PineScript v6 | Designed for 5-min Silver CFD data
#
# Strategy Logic:
#   1. Define Asia session range (default 00:00–06:00 Vienna time)
#   2. Wait for breakout of Asia High/Low after session closes
#   3. Confirm with: KC (15min EMA±ATR), 1H MA cross, Volume ratio, KC distance, RSI divergence, DTD filter
#   4. Hold position until counter-signal or new day (no ATR-stop simulation)
#
# NOTE — Filters approximated on the base timeframe (5min) since multi-TF
#   request.security() calls cannot be replicated without pre-computed HTF data:
#   - KC: computed on 5min bars (PineScript used 15min) → set kc_length=45 to approximate
#   - MA: computed on 5min bars (PineScript used 1H)    → set ma_fast_len=108, ma_slow_len=252 to approximate
#   - RSI divergence: rolling lookback window, same logic
#   - Pyramid / trailing / BE logic: NOT simulated (signal-only framework)
#   - ATR stops: noted in comments, not enforced
#   - Time exit (21:00): noted in comments, not enforced
# =============================================================================

NAME <- "SilverAsiaBreakoutV4"

PARAM_GRID <- list(
  # Session (hours in local/Vienna time) — fixed, not searched
  asia_start_hour  = c(0),
  asia_end_hour    = c(7),

  # Volume filter
  vol_lookback     = c(15),
  vol_threshold    = c(0.8, 1.1),

  # Keltner Channel (computed on 5min base TF)
  kc_length        = c(36),
  kc_mult          = c(1.2),

  # Moving averages (computed on 5min base TF)
  ma_fast_len      = c(72, 120),
  ma_slow_len      = c(180, 300),

  # Asia range / breakout window
  min_range        = c(0.0, 0.4),
  breakout_window  = c(8, 16),

  # KC distance filter
  min_kc_dist_pct  = c(0.10, 0.25),

  # RSI divergence
  rsi_length       = c(12, 18),
  div_lookback     = c(18, 28),

  # Day-to-Day Asia close filter
  max_dtd_pct      = c(0.0, 2.5),

  # Risk — fixed original params kept as-is
  atr_length       = c(14),
  sl_atr_mult      = c(2.5),
  min_sl_pct       = c(1.5),
  tp_pct           = c(1.5),
  trail_atr_mult   = c(3.0),

  # ATR-barrier second-pass (atr_period separate from atr_length)
  atr_period       = c(14L),
  tp_atr_mult      = c(2.0, 3.0, 4.0)
)
# Total combinations: 2^12 * 2 * 3 = 24,576


# =============================================================================
# HELPER: EMA (exponential moving average) using TTR
# =============================================================================
.ema <- function(x, n) {
  as.numeric(TTR::EMA(x, n = n))
}

# =============================================================================
# HELPER: RSI using TTR
# =============================================================================
.rsi <- function(x, n) {
  as.numeric(TTR::RSI(x, n = n))
}

# =============================================================================
# HELPER: ATR using TTR (requires HLC matrix)
# =============================================================================
.atr <- function(high, low, close, n) {
  hlc <- cbind(high, low, close)
  as.numeric(TTR::ATR(hlc, n = n)[, "atr"])
}

# =============================================================================
# HELPER: Keltner Channel
#   Upper = EMA(close, n) + mult * ATR(n)
#   Lower = EMA(close, n) - mult * ATR(n)
# =============================================================================
.keltner <- function(high, low, close, n, mult) {
  basis <- .ema(close, n)
  atr_v <- .atr(high, low, close, n)
  list(
    upper = basis + mult * atr_v,
    lower = basis - mult * atr_v
  )
}

# =============================================================================
# HELPER: Detect bullish/bearish RSI divergence
#   Bearish div: price at rolling high, RSI below its rolling high by > 5
#   Bullish div: price at rolling low,  RSI above its rolling low  by > 5
# =============================================================================
.rsi_divergence <- function(high, low, rsi_v, lookback) {
  n <- length(rsi_v)

  price_high_lb <- as.numeric(data.table::frollapply(high, lookback, max, fill = NA, align = "right"))
  price_low_lb  <- as.numeric(data.table::frollapply(low,  lookback, min, fill = NA, align = "right"))
  rsi_high_lb   <- as.numeric(data.table::frollapply(rsi_v, lookback, max, fill = NA, align = "right"))
  rsi_low_lb    <- as.numeric(data.table::frollapply(rsi_v, lookback, min, fill = NA, align = "right"))

  bearish_div <- high >= price_high_lb * 0.999 & rsi_v < rsi_high_lb - 5
  bullish_div <- low  <= price_low_lb  * 1.001 & rsi_v > rsi_low_lb  + 5

  # Replace NA with FALSE
  bearish_div[is.na(bearish_div)] <- FALSE
  bullish_div[is.na(bullish_div)] <- FALSE

  list(bearish = bearish_div, bullish = bullish_div)
}


# =============================================================================
# MAIN SIGNAL FUNCTION
# =============================================================================
generate_signals <- function(
    df,

    # Session
    asia_start_hour  = 0,
    asia_end_hour    = 6,

    # Entry filters
    vol_lookback     = 20,
    vol_threshold    = 1.0,
    kc_length        = 45,
    kc_mult          = 1.3,
    ma_fast_len      = 108,
    ma_slow_len      = 252,
    min_range        = 0.0,
    breakout_window  = 12,
    min_kc_dist_pct  = 0.2,
    rsi_length       = 14,
    div_lookback     = 20,
    max_dtd_pct      = 3.0,

    # Risk (original params — not enforced in signal layer)
    atr_length       = 14,
    sl_atr_mult      = 2.5,
    min_sl_pct       = 1.5,
    tp_pct           = 1.5,
    trail_atr_mult   = 3.0,

    # ATR-barrier second-pass
    atr_period       = 14L,
    tp_atr_mult      = 2.0
) {
  # ---------------------------------------------------------------------------
  # 0. Setup
  # ---------------------------------------------------------------------------
  out <- data.table::copy(df)

  # Ensure Timestamp is POSIXct; convert to Vienna time for session logic
  if (!inherits(out$Timestamp, "POSIXct")) {
    out[, Timestamp := as.POSIXct(Timestamp)]
  }
  ts_vienna <- lubridate::with_tz(out$Timestamp, "Europe/Vienna")

  bar_hour  <- as.integer(format(ts_vienna, "%H"))
  bar_date  <- as.Date(ts_vienna)

  N <- nrow(out)

  # ---------------------------------------------------------------------------
  # 1. Indicator computation (no look-ahead; results used with shift below)
  # ---------------------------------------------------------------------------

  # Volume ratio
  vol_sma   <- as.numeric(TTR::SMA(out$Volume, n = vol_lookback))
  vol_ratio <- ifelse(is.na(vol_sma) | vol_sma == 0, 0, out$Volume / vol_sma)

  # Keltner Channel (approximated on base TF)
  kc        <- .keltner(out$High, out$Low, out$Close, kc_length, kc_mult)
  kc_upper  <- kc$upper
  kc_lower  <- kc$lower

  # KC distance % beyond band
  kc_dist_long  <- (out$Close - kc_upper) / out$Close * 100
  kc_dist_short <- (kc_lower - out$Close) / out$Close * 100

  # Moving averages (1H approximated on base TF)
  ma_fast   <- .ema(out$Close, ma_fast_len)
  ma_slow   <- .ema(out$Close, ma_slow_len)
  ma_bull   <- ma_fast > ma_slow    # TRUE = bullish regime
  ma_bear   <- ma_fast < ma_slow    # TRUE = bearish regime

  # RSI + divergence
  rsi_v     <- .rsi(out$Close, rsi_length)
  div       <- .rsi_divergence(out$High, out$Low, rsi_v, div_lookback)
  bearish_div <- div$bearish
  bullish_div <- div$bullish

  # ATR (for reference — SL/TP/trail not enforced in signal layer)
  # atr_v  <- .atr(out$High, out$Low, out$Close, atr_length)
  # sl_atr_dist <- pmax(atr_v * sl_atr_mult, out$Close * min_sl_pct / 100)

  # ---------------------------------------------------------------------------
  # 2. Asia session range — computed per day, no look-ahead
  #    asia_high / asia_low are set on the BAR AFTER the Asia session closes.
  #    They become available (look-ahead free) from the first non-Asia bar
  #    of the same day onwards.
  # ---------------------------------------------------------------------------
  asia_high_vec <- rep(NA_real_, N)
  asia_low_vec  <- rep(NA_real_, N)
  range_set_vec <- rep(FALSE, N)

  # DTD tracking
  dtd_ok_vec    <- rep(TRUE, N)

  unique_days <- unique(bar_date)

  # Build per-day Asia range lookup
  day_asia_high     <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))
  day_asia_low      <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))
  day_asia_close    <- setNames(rep(NA_real_, length(unique_days)), as.character(unique_days))

  in_asia_mask <- bar_hour >= asia_start_hour & bar_hour < asia_end_hour

  for (d in as.character(unique_days)) {
    idx <- which(as.character(bar_date) == d & in_asia_mask)
    if (length(idx) > 0) {
      day_asia_high[d]  <- max(out$High[idx],  na.rm = TRUE)
      day_asia_low[d]   <- min(out$Low[idx],   na.rm = TRUE)
      day_asia_close[d] <- out$Close[max(idx)]
    }
  }

  # Assign per-bar: range available once Asia session has closed
  prev_day_close <- NA_real_

  for (i in seq_len(N)) {
    d <- as.character(bar_date[i])
    h <- bar_hour[i]

    # After Asia session and range exists
    if (h >= asia_end_hour && !is.na(day_asia_high[d])) {
      asia_high_vec[i] <- day_asia_high[d]
      asia_low_vec[i]  <- day_asia_low[d]
      range_set_vec[i] <- TRUE
    }

    # DTD check: use previous day's Asia close
    if (h >= asia_end_hour) {
      today_close <- day_asia_close[d]
      if (!is.na(prev_day_close) && prev_day_close > 0 && !is.na(today_close)) {
        dtd_chg <- abs(today_close - prev_day_close) / prev_day_close * 100
        dtd_ok_vec[i] <- (max_dtd_pct == 0) || (dtd_chg <= max_dtd_pct)
      }
    }

    # Update prev_day_close at first bar of a new day (last Asia close of previous day)
    if (i > 1 && bar_date[i] != bar_date[i - 1]) {
      prev_d <- as.character(bar_date[i - 1])
      if (!is.na(day_asia_close[prev_d])) {
        prev_day_close <- day_asia_close[prev_d]
      }
    }
  }

  range_size <- asia_high_vec - asia_low_vec
  range_valid <- range_set_vec & (min_range == 0 | (!is.na(range_size) & range_size >= min_range))

  # ---------------------------------------------------------------------------
  # 3. Breakout detection — crossover of Asia High/Low
  # ---------------------------------------------------------------------------
  close_lag1     <- data.table::shift(out$Close, 1, type = "lag")
  asia_high_lag1 <- data.table::shift(asia_high_vec, 1, type = "lag")
  asia_low_lag1  <- data.table::shift(asia_low_vec,  1, type = "lag")

  # Crossover: prev close was below/above, current close is above/below
  cross_above <- !is.na(close_lag1) & !is.na(asia_high_vec) &
    close_lag1 <= asia_high_lag1 & out$Close > asia_high_vec & range_valid
  cross_below <- !is.na(close_lag1) & !is.na(asia_low_vec)  &
    close_lag1 >= asia_low_lag1  & out$Close < asia_low_vec  & range_valid

  # Track bars since breakout, reset per day
  bars_since_long_bo  <- rep(999L, N)
  bars_since_short_bo <- rep(999L, N)

  for (i in seq_len(N)) {
    if (cross_above[i]) {
      bars_since_long_bo[i] <- 0L
    } else if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      bars_since_long_bo[i] <- min(bars_since_long_bo[i - 1] + 1L, 999L)
    }
    if (cross_below[i]) {
      bars_since_short_bo[i] <- 0L
    } else if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      bars_since_short_bo[i] <- min(bars_since_short_bo[i - 1] + 1L, 999L)
    }
    # Reset at start of new day
    if (i > 1 && bar_date[i] != bar_date[i - 1]) {
      bars_since_long_bo[i]  <- 999L
      bars_since_short_bo[i] <- 999L
    }
  }

  # Window: still within breakout_window bars AND price still beyond level
  in_long_window  <- bars_since_long_bo  <= breakout_window & out$Close > asia_high_vec
  in_short_window <- bars_since_short_bo <= breakout_window & out$Close < asia_low_vec

  # Replace NA
  in_long_window[is.na(in_long_window)]   <- FALSE
  in_short_window[is.na(in_short_window)] <- FALSE

  # ---------------------------------------------------------------------------
  # 4. All filter conditions
  # ---------------------------------------------------------------------------
  kc_dist_ok_long  <- !is.na(kc_dist_long)  & kc_dist_long  >= min_kc_dist_pct
  kc_dist_ok_short <- !is.na(kc_dist_short) & kc_dist_short >= min_kc_dist_pct

  no_bearish_div <- !bearish_div
  no_bullish_div <- !bullish_div

  enter_long_raw <- (
    in_long_window                          &
      !is.na(vol_ratio)  & vol_ratio >= vol_threshold  &
      !is.na(kc_upper)   & out$Close > kc_upper        &
      !is.na(ma_bull)    & ma_bull                     &
      kc_dist_ok_long                         &
      no_bearish_div                          &
      dtd_ok_vec
  )

  enter_short_raw <- (
    in_short_window                         &
      !is.na(vol_ratio)  & vol_ratio >= vol_threshold  &
      !is.na(kc_lower)   & out$Close < kc_lower        &
      !is.na(ma_bear)    & ma_bear                     &
      kc_dist_ok_short                        &
      no_bullish_div                          &
      dtd_ok_vec
  )

  # Replace any NA
  enter_long_raw[is.na(enter_long_raw)]   <- FALSE
  enter_short_raw[is.na(enter_short_raw)] <- FALSE

  # ---------------------------------------------------------------------------
  # 5. One trade per day constraint
  # ---------------------------------------------------------------------------
  traded_today <- rep(FALSE, N)
  for (i in seq_len(N)) {
    if (i > 1 && bar_date[i] == bar_date[i - 1]) {
      traded_today[i] <- traded_today[i - 1]
    }
    if ((enter_long_raw[i] || enter_short_raw[i]) && !traded_today[i]) {
      traded_today[i] <- TRUE
    }
  }

  can_trade <- !data.table::shift(traded_today, 1, fill = FALSE) |
    (bar_date != data.table::shift(bar_date, 1, fill = bar_date[1]))

  enter_long_raw[!can_trade]  <- FALSE
  enter_short_raw[!can_trade] <- FALSE

  # ---------------------------------------------------------------------------
  # 6. NO LOOK-AHEAD SHIFT
  # ---------------------------------------------------------------------------
  enter_long_shifted  <- data.table::shift(enter_long_raw,  1, fill = FALSE, type = "lag")
  enter_short_shifted <- data.table::shift(enter_short_raw, 1, fill = FALSE, type = "lag")

  # ---------------------------------------------------------------------------
  # 7. Build Position vector: hold until counter-signal
  # ---------------------------------------------------------------------------
  position <- integer(N)

  current_pos <- 0L
  for (i in seq_len(N)) {
    if (enter_long_shifted[i] && current_pos != 1L) {
      current_pos <- 1L
    } else if (enter_short_shifted[i] && current_pos != -1L) {
      current_pos <- -1L
    }
    position[i] <- current_pos
  }

  out[, Position := position]

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
    overnight_lockout = TRUE
  )

  out[, Position := res$Position]
  out[, SL       := res$SL]
  out[, TP       := res$TP]
  out
}
