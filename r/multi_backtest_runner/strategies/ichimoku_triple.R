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


# Ichimoku Triple Confirmation – Custom 7/20/60 Settings
#
# Entry Long:  Tenkan > Kijun (bullish cross), Close > Kumo (Span A & B),
#              Chikou Span frei von Preisstruktur (kein historischer Close darueber)
# Entry Short: inverse
# Exit:        Gegensignal (alle drei Bedingungen flippen)
#
# Hinweis: Dieses Plugin verwendet das vereinfachte Position-Modell (-1/0/1).
# Kein ATR-Stop – Position haelt bis Gegensignal. Fuer finale Validierung
# den State-Machine-Backtest mit echtem SL/TP verwenden.

NAME <- "Ichimoku_7_20_60"

PARAM_GRID <- list(
  tenkan_n     = c(7L, 9L),
  kijun_n      = c(20L, 26L),
  senkou_b_n   = c(60L, 52L),
  displacement = c(26L),
  cloud_filter = c(TRUE),
  atr_period   = c(10L, 14L),
  sl_atr_mult  = c(1.5, 2.0, 2.5),
  tp_atr_mult  = c(2.0, 3.0, 4.0)
)

# Ichimoku-Komponenten manuell berechnen (TTR hat keine Ichimoku-Funktion)
.ichimoku <- function(high, low, close,
                      tenkan_n = 7L, kijun_n = 20L,
                      senkou_b_n = 60L, displacement = 26L) {
  n <- length(close)

  midpoint <- function(x, y, period) {
    (TTR::runMax(x, period) + TTR::runMin(y, period)) / 2
  }

  tenkan   <- midpoint(high, low, tenkan_n)
  kijun    <- midpoint(high, low, kijun_n)
  senkou_a <- data.table::shift((tenkan + kijun) / 2, -displacement)  # in die Zukunft
  senkou_b <- data.table::shift(midpoint(high, low, senkou_b_n), -displacement)
  chikou   <- data.table::shift(close, displacement)  # displacement Bars zurueck

  list(tenkan = tenkan, kijun = kijun,
       senkou_a = senkou_a, senkou_b = senkou_b, chikou = chikou)
}

generate_signals <- function(df, tenkan_n = 7L, kijun_n = 20L,
                              senkou_b_n = 60L, displacement = 26L,
                              cloud_filter = TRUE,
                              atr_period = 14L, sl_atr_mult = 1.5, tp_atr_mult = 2.0) {
  out <- data.table::copy(df)
  n   <- nrow(out)

  ichi <- .ichimoku(out$High, out$Low, out$Close,
                    tenkan_n, kijun_n, senkou_b_n, displacement)

  tenkan   <- ichi$tenkan
  kijun    <- ichi$kijun
  senkou_a <- ichi$senkou_a
  senkou_b <- ichi$senkou_b
  chikou   <- ichi$chikou
  cl       <- out$Close

  pos   <- numeric(n)
  state <- 0L

  for (i in (max(kijun_n, senkou_b_n, displacement) + 1L):n) {
    if (anyNA(c(tenkan[i], kijun[i], senkou_a[i], senkou_b[i], chikou[i]))) {
      pos[i] <- state; next
    }

    kumo_top    <- max(senkou_a[i], senkou_b[i])
    kumo_bot    <- min(senkou_a[i], senkou_b[i])
    tk_cross_up <- tenkan[i] > kijun[i] && tenkan[i - 1] <= kijun[i - 1]
    tk_cross_dn <- tenkan[i] < kijun[i] && tenkan[i - 1] >= kijun[i - 1]

    # Chikou Span: Close vor 'displacement' Bars sollte keine Preisstruktur haben
    # Approximation: Chikou (= aktueller Close vor displacement Bars) < Close[i-displacement]
    chikou_clear_long  <- chikou[i] > cl[i]   # Chikou ueber aktuellem Close
    chikou_clear_short <- chikou[i] < cl[i]

    long_ok  <- tk_cross_up &&
      (!cloud_filter || cl[i] > kumo_top) &&
      chikou_clear_long

    short_ok <- tk_cross_dn &&
      (!cloud_filter || cl[i] < kumo_bot) &&
      chikou_clear_short

    exit_long  <- tenkan[i] < kijun[i] || (cloud_filter && cl[i] < kumo_bot)
    exit_short <- tenkan[i] > kijun[i] || (cloud_filter && cl[i] > kumo_top)

    if (state == 0L) {
      if      (long_ok)  state <-  1L
      else if (short_ok) state <- -1L
    } else if (state ==  1L) {
      if (exit_long || short_ok) state <- if (short_ok) -1L else 0L
    } else if (state == -1L) {
      if (exit_short || long_ok) state <- if (long_ok)  1L  else 0L
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
