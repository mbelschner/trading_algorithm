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
  prev_close_h  = c(17, 21),          # hour to capture reference close (21=EU/DE/Brent, 8=CN50)
  sess_start_h  = c(2, 9),            # session open hour (9=EU50/DE40, 2=CN50)
  sess_start_m  = c(0),               # session open minute
  exit_h        = c(15, 17),          # force-exit hour (15=EU50, 17=DE40/Brent)
  exit_m        = c(0),               # force-exit minute
  
  # Strategy parameters
  gap_thresh    = c(0.3, 0.5, 0.7, 1.0, 1.5),  # min gap % to trigger trade
  tp_pct        = c(0.2, 0.3, 0.4, 0.6),        # take profit %
  sl_pct        = c(0.5, 0.8, 1.0, 1.5),        # stop loss %
  direction     = c("Both", "Long Only", "Short Only")  # trade direction filter
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
    direction     = "Both"
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
  #    At each bar where bar_h == prev_close_h, store the closing price.
  #    This is the last close seen during that hour on each calendar day.
  #    No look-ahead: we use this value starting from the NEXT bar.
  # ---------------------------------------------------------------------------
  
  # Build per-day prev_close lookup:
  # For each day, the reference close = last Close during bars where bar_h == prev_close_h
  # We then look it up on the FOLLOWING day (i.e., the day the gap is measured).
  
  unique_dates <- sort(unique(bar_date))
  day_ref_close <- setNames(rep(NA_real_, length(unique_dates)), as.character(unique_dates))
  
  for (d in as.character(unique_dates)) {
    idx <- which(as.character(bar_date) == d & bar_h == prev_close_h)
    if (length(idx) > 0) {
      day_ref_close[d] <- out$Close[max(idx)]  # last bar of that hour
    }
  }
  
  # prev_close available on day D = ref_close captured on day D-1
  # (Handles overnight gap: prev_close_h is on day D-1, session opens on day D)
  prev_close_for_day <- function(d) {
    d_char <- as.character(d)
    d_idx  <- which(as.character(unique_dates) == d_char)
    if (d_idx <= 1) return(NA_real_)
    prev_d <- as.character(unique_dates[d_idx - 1])
    day_ref_close[prev_d]
  }
  
  # ---------------------------------------------------------------------------
  # 2. Detect session open bar and compute gap signal
  #    is_session_open: bar_min == sess_start_min (one bar per day)
  #    One trade per day enforced.
  # ---------------------------------------------------------------------------
  
  is_session_open <- bar_min == sess_start_min
  
  # Raw signal (evaluated on the session open bar, no look-ahead within the bar)
  signal_raw <- integer(N)   # 1=long, -1=short, 0=none
  gap_pct_vec   <- rep(NA_real_, N)   # for diagnostics
  tp_level_vec  <- rep(NA_real_, N)
  sl_level_vec  <- rep(NA_real_, N)
  
  traded_today <- rep(FALSE, N)
  
  for (i in seq_len(N)) {
    d <- bar_date[i]
    
    # Propagate traded_today within the day
    if (i > 1 && bar_date[i] == bar_date[i - 1] && traded_today[i - 1]) {
      traded_today[i] <- TRUE
    }
    
    if (is_session_open[i] && !traded_today[i]) {
      prev_cl <- prev_close_for_day(d)
      
      if (!is.na(prev_cl) && prev_cl > 0) {
        gap <- (out$Open[i] - prev_cl) / prev_cl * 100
        gap_pct_vec[i] <- gap
        
        if (gap >= gap_thresh && direction %in% c("Both", "Short Only")) {
          # Gap UP → fade short
          signal_raw[i] <- -1L
          tp_level_vec[i] <- out$Open[i] * (1 - tp_pct / 100)
          sl_level_vec[i] <- out$Open[i] * (1 + sl_pct / 100)
          traded_today[i] <- TRUE
        } else if (gap <= -gap_thresh && direction %in% c("Both", "Long Only")) {
          # Gap DOWN → fade long
          signal_raw[i] <- 1L
          tp_level_vec[i] <- out$Open[i] * (1 + tp_pct / 100)
          sl_level_vec[i] <- out$Open[i] * (1 - sl_pct / 100)
          traded_today[i] <- TRUE
        }
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # 3. NO LOOK-AHEAD SHIFT
  #    Signal on bar[i] (session open) → entry at open of bar[i+1]
  #    Shift signal, TP, and SL levels forward by 1 bar.
  # ---------------------------------------------------------------------------
  signal_shifted  <- data.table::shift(signal_raw,  1L, fill = 0L,        type = "lag")
  tp_level_shift  <- data.table::shift(tp_level_vec, 1L, fill = NA_real_, type = "lag")
  sl_level_shift  <- data.table::shift(sl_level_vec, 1L, fill = NA_real_, type = "lag")
  gap_pct_shift   <- data.table::shift(gap_pct_vec,  1L, fill = NA_real_, type = "lag")
  
  # ---------------------------------------------------------------------------
  # 4. Build Position vector
  #    Hold position until:
  #      a) counter-signal
  #      b) [not enforced here] TP hit, SL hit, or time exit at exit_h:exit_m UTC
  #    Position = 1 (long), -1 (short), 0 (flat)
  #
  #    NOTE: In live trading, exits are:
  #      - TP:        entry_open * (1 ± tp_pct/100)
  #      - SL:        entry_open * (1 ∓ sl_pct/100)
  #      - Time Exit: bar_min >= exit_h*60 + exit_m on the same day
  #    Apply these in the backtester using the TP_Level and SL_Level columns below.
  # ---------------------------------------------------------------------------
  position <- integer(N)
  current_pos <- 0L
  
  for (i in seq_len(N)) {
    sig <- signal_shifted[i]
    
    if (sig == 1L && current_pos != 1L) {
      current_pos <- 1L
    } else if (sig == -1L && current_pos != -1L) {
      current_pos <- -1L
    }
    
    # Optional: force flat at time exit (uncomment if backtester doesn't handle it)
    # if (!is.na(bar_min[i]) && bar_min[i] >= exit_min_val) {
    #   current_pos <- 0L
    # }
    
    position[i] <- current_pos
  }
  
  # ---------------------------------------------------------------------------
  # 5. Attach columns to output
  # ---------------------------------------------------------------------------
  out[, Position := position]
  
  # Diagnostic columns — used by backtester for TP/SL/time-exit simulation
  out[, TP_Level  := tp_level_shift]
  out[, SL_Level  := sl_level_shift]
  out[, Gap_Pct   := gap_pct_shift]    # gap % that triggered the signal (NA if no signal)
  out[, Exit_Min  := exit_min_val]     # backtester: close position if bar_min >= Exit_Min
  
  out
}