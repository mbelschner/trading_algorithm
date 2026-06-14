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
  tenkan_n    = c(7L, 9L),
  kijun_n     = c(20L, 26L),
  senkou_b_n  = c(60L, 52L),
  displacement = c(26L),
  cloud_filter = c(TRUE)  # Close muss ueber/unter Kumo sein
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
       senkou_a = senkou_a, senkou_b = senkou_b,
       chikou = chikou)
}

generate_signals <- function(df, tenkan_n = 7L, kijun_n = 20L,
                              senkou_b_n = 60L, displacement = 26L,
                              cloud_filter = TRUE) {
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

  out[, Position := pos]
  out
}
