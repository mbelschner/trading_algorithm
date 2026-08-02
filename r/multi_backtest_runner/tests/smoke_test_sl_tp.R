# =============================================================================
# smoke_test_sl_tp.R
# Equivalence test for refactored strategies (SL/TP/overnight second-pass).
#
# Method: single UTC day (00:00–23:55, no midnight crossing) + extreme barriers
#   sl_atr_mult = 99, tp_atr_mult = 99  → SL/TP never hit
#   No midnight UTC crossing            → overnight_exit never fires
#
# Expected: entry bars from original == entry bars from new for every strategy.
#
# Run from project root:
#   Rscript r/multi_backtest_runner/tests/smoke_test_sl_tp.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(TTR)
  if (requireNamespace("lubridate", quietly = TRUE)) library(lubridate)
})

# ── Synthetic test data: 288 five-min bars, 2020-01-15 UTC ────────────────────
set.seed(42)
N        <- 288L
ts_start <- as.POSIXct("2020-01-15 00:00:00", tz = "UTC")
bar_ts   <- ts_start + seq(0L, (N - 1L) * 300L, by = 300L)

# Price walk centred around 1800 (Silver-like range)
cl_walk  <- cumsum(c(1800, rnorm(N - 1L, 0, 4))) + 200
op       <- cl_walk + rnorm(N, 0, 2)
cl       <- cl_walk + rnorm(N, 0, 2)
hi       <- pmax(op, cl) + abs(rnorm(N, 0, 1))
lo       <- pmin(op, cl) - abs(rnorm(N, 0, 1))

test_df <- data.table(
  Timestamp = bar_ts,
  Open      = op,
  High      = hi,
  Low       = lo,
  Close     = cl,
  Volume    = runif(N, 5000L, 50000L)
)

# ── Helpers ───────────────────────────────────────────────────────────────────
entry_bars <- function(pos) {
  pos <- as.integer(round(pos))
  which(pos != 0L & c(0L, head(pos, -1L)) == 0L)
}

strat_dir  <- "r/multi_backtest_runner/strategies"
orig_files <- sort(list.files(strat_dir, pattern = "_original\\.R$", full.names = TRUE))

results <- vector("list", length(orig_files))
names(results) <- sub("_original\\.R$", ".R", basename(orig_files))

# ── Run each strategy ─────────────────────────────────────────────────────────
for (orig_f in orig_files) {
  fname <- sub("_original\\.R$", ".R", basename(orig_f))
  new_f <- file.path(strat_dir, fname)
  cat(sprintf("%-50s", fname))

  tryCatch({
    # Load original and new into isolated child environments
    e_orig <- new.env(parent = .GlobalEnv)
    source(orig_f, local = e_orig, echo = FALSE)

    e_new <- new.env(parent = .GlobalEnv)
    source(new_f, local = e_new, echo = FALSE)

    orig_out <- e_orig$generate_signals(test_df)
    new_out  <- e_new$generate_signals(test_df, sl_atr_mult = 99, tp_atr_mult = 99)

    orig_eb <- entry_bars(orig_out$Position)
    new_eb  <- entry_bars(new_out$Position)

    # Skip the pre-ATR-warmup window (first 20 bars): the new second-pass
    # correctly zeros positions where ATR is NA, so early entries intentionally
    # differ. Compare only bars where ATR is guaranteed to be available.
    WARMUP <- 20L
    orig_eb_cmp <- orig_eb[orig_eb > WARMUP]
    new_eb_cmp  <- new_eb[new_eb  > WARMUP]

    pass  <- identical(orig_eb_cmp, new_eb_cmp)
    smoke <- if (pass) "PASS" else {
      both <- sort(union(orig_eb_cmp, new_eb_cmp))
      diff <- both[!both %in% intersect(orig_eb_cmp, new_eb_cmp)]
      sprintf("FAIL @ bar %d", min(diff))
    }

    param_n <- tryCatch(
      prod(lengths(e_new$PARAM_GRID)),
      error = function(e) NA_integer_
    )

    results[[fname]] <- list(
      status = if (pass) "OK" else "FEHLER",
      param_n = param_n,
      smoke   = smoke
    )
    cat(smoke, "\n")

  }, error = function(e) {
    msg <- substr(conditionMessage(e), 1L, 70L)
    results[[fname]] <<- list(status = "FEHLER", param_n = NA_integer_,
                              smoke = paste("ERROR:", msg))
    cat("ERROR:", conditionMessage(e), "\n")
  })
}

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 78), "\n", sep = "")
cat(sprintf("%-42s %-8s %12s  %s\n",
            "Dateiname", "Status", "Param-Kombs", "Smoke-Test"))
cat(strrep("-", 78), "\n", sep = "")

for (fname in names(results)) {
  r <- results[[fname]]
  cat(sprintf("%-42s %-8s %12s  %s\n",
              fname,
              r$status,
              if (is.na(r$param_n)) "?" else format(r$param_n, big.mark = ","),
              r$smoke))
}

cat(strrep("=", 78), "\n", sep = "")
n_pass <- sum(sapply(results, function(r) r$smoke == "PASS"))
cat(sprintf("\n%d / %d PASS\n", n_pass, length(results)))
