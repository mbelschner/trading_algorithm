# =============================================================================
# Äquivalenz-Verifikation: asia_breakout.R vs. silver_asia_breakout_v4_vectorized.R
#
# Testet N_COMBOS zufällige Parameter-Kombinationen.
# Erfolg: identical(Position_Loop, Position_Vectorized) für alle Bars.
# Falls FAIL: zeigt erste abweichende Bar (Timestamp + Positions).
#
# Verwendung:
#   Rscript r/multi_backtest_runner/tests/verify_equivalence.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(TTR)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# Pfade — anpassen falls CSV einen anderen Namen hat
# -----------------------------------------------------------------------------
SCRIPT_DIR <- file.path(getwd(), "r", "multi_backtest_runner")
ORIG_PATH  <- file.path(SCRIPT_DIR, "strategies", "asia_breakout.R")
VECT_PATH  <- file.path(SCRIPT_DIR, "strategies", "silver_asia_breakout_v4_vectorized.R")

# Sucht nach SILVER oder GOLD CSV falls kein Silver vorhanden
find_data_path <- function() {
  candidates <- c(
    file.path(getwd(), "price_data", "SILVER_MINUTE_5.csv"),
    file.path(getwd(), "price_data", "GOLD_MINUTE_5.csv")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0)
    stop("Keine CSV gefunden. Bitte DATA_PATH manuell setzen.")
  found[1]
}
DATA_PATH <- find_data_path()

# Anzahl zufälliger Kombinationen zu testen
N_COMBOS <- 12

# Maximale Bars für den Test (kürzer = schneller)
MAX_BARS <- 60000L


# -----------------------------------------------------------------------------
# Beide Strategie-Environments laden
# -----------------------------------------------------------------------------
for (p in c(ORIG_PATH, VECT_PATH)) {
  if (!file.exists(p)) stop("Nicht gefunden: ", p)
}

env_loop <- new.env(parent = baseenv())
env_vect <- new.env(parent = baseenv())
sys.source(ORIG_PATH, envir = env_loop)
sys.source(VECT_PATH, envir = env_vect)


# -----------------------------------------------------------------------------
# Daten laden
# -----------------------------------------------------------------------------
cat(sprintf("Lade Daten: %s\n", basename(DATA_PATH)))
raw <- data.table::fread(DATA_PATH)
setnames(raw,
         old = c("time", "open", "high", "low", "close", "volume"),
         new = c("Timestamp", "Open", "High", "Low", "Close", "Volume"),
         skip_absent = TRUE)
raw[, Timestamp := as.POSIXct(Timestamp, tz = "UTC")]
setorder(raw, Timestamp)
raw <- unique(raw, by = "Timestamp", fromLast = TRUE)

dt_test <- raw[seq_len(min(MAX_BARS, nrow(raw)))]
cat(sprintf("Testdaten: %d Bars  (%s – %s)\n\n",
            nrow(dt_test),
            format(dt_test$Timestamp[1]),
            format(dt_test$Timestamp[nrow(dt_test)])))


# -----------------------------------------------------------------------------
# Parameter-Kombinationen auswählen
# -----------------------------------------------------------------------------
pg       <- env_loop$PARAM_GRID
pg_fixed  <- pg[sapply(pg, length) == 1]
pg_search <- pg[sapply(pg, length) > 1]

set.seed(42)
all_combos  <- do.call(expand.grid, c(pg_search, list(stringsAsFactors = FALSE)))
sample_idx  <- sample(nrow(all_combos), min(N_COMBOS, nrow(all_combos)))
test_combos <- all_combos[sample_idx, , drop = FALSE]
for (nm in names(pg_fixed)) test_combos[[nm]] <- pg_fixed[[nm]]

cat(sprintf("Teste %d Parameter-Kombinationen\n\n", nrow(test_combos)))


# -----------------------------------------------------------------------------
# Test-Loop
# -----------------------------------------------------------------------------
t_loop <- 0
t_vect <- 0
all_ok <- TRUE

for (i in seq_len(nrow(test_combos))) {
  params <- as.list(test_combos[i, , drop = FALSE])

  t1 <- system.time(
    out_loop <- tryCatch(
      do.call(env_loop$generate_signals, c(list(df = data.table::copy(dt_test)), params)),
      error = function(e) {
        cat(sprintf("[ERR ] combo %3d | Loop-Fehler: %s\n", i, conditionMessage(e)))
        NULL
      }
    )
  )["elapsed"]

  t2 <- system.time(
    out_vect <- tryCatch(
      do.call(env_vect$generate_signals, c(list(df = data.table::copy(dt_test)), params)),
      error = function(e) {
        cat(sprintf("[ERR ] combo %3d | Vektor-Fehler: %s\n", i, conditionMessage(e)))
        NULL
      }
    )
  )["elapsed"]

  t_loop <- t_loop + t1
  t_vect <- t_vect + t2

  if (is.null(out_loop) || is.null(out_vect)) {
    all_ok <- FALSE
    next
  }

  pos_l <- as.integer(out_loop$Position)
  pos_v <- as.integer(out_vect$Position)

  if (length(pos_l) != length(pos_v)) {
    cat(sprintf("[FAIL] combo %3d | Länge verschieden: loop=%d vect=%d\n",
                i, length(pos_l), length(pos_v)))
    all_ok <- FALSE
    next
  }

  diffs     <- which(pos_l != pos_v)
  diff_bars <- length(diffs)
  trades_l  <- sum(diff(c(0L, pos_l)) != 0, na.rm = TRUE)
  trades_v  <- sum(diff(c(0L, pos_v)) != 0, na.rm = TRUE)

  if (diff_bars == 0) {
    cat(sprintf("[OK  ] combo %3d | trades_loop=%d trades_vect=%d | diff_bars=0\n",
                i, trades_l, trades_v))
  } else {
    all_ok <- FALSE
    fd     <- diffs[1]
    cat(sprintf(
      "[FAIL] combo %3d | trades_loop=%d trades_vect=%d | diff_bars=%d | erste Abw. Bar %d ts=%s  pos_loop=%d pos_vect=%d\n",
      i, trades_l, trades_v, diff_bars, fd,
      format(out_loop$Timestamp[fd]), pos_l[fd], pos_v[fd]
    ))
  }
}


# -----------------------------------------------------------------------------
# Zusammenfassung
# -----------------------------------------------------------------------------
cat("\n========================================\n")
if (all_ok) {
  cat("Ergebnis: ALLE Kombinationen identisch\n")
} else {
  cat("Ergebnis: FEHLER - nicht alle Kombinationen identisch\n")
}
cat(sprintf("Zeit Loop:    %.2fs  (%d combos, %d bars)\n",
            t_loop, nrow(test_combos), nrow(dt_test)))
cat(sprintf("Zeit Vektor:  %.2fs\n", t_vect))
if (t_vect > 0)
  cat(sprintf("Speedup:      %.1fx\n", t_loop / t_vect))
cat("========================================\n")

if (!all_ok) quit(status = 1)
