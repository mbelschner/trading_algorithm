# Debug Script - Test Indicator Calculation
# Lineares Script zum Debuggen des "object 'high' not found" Fehlers

cat("\n=== DEBUG: INDICATOR CALCULATION ===\n\n")

# ===== 1. Pakete laden ========================================================
cat("1. Lade Pakete...\n")
library(data.table)
library(TTR)
library(zoo)
cat("   ✓ Pakete geladen\n\n")

# ===== 2. Daten laden =========================================================
cat("2. Lade Daten...\n")
dt <- fread("labelled_data/GOLD_MINUTE_15_labeled.csv")
setDT(dt)

cat(sprintf("   Zeilen: %s\n", format(nrow(dt), big.mark = ",")))
cat(sprintf("   Spalten: %d\n", ncol(dt)))
cat(sprintf("   Spaltennamen: %s\n", paste(head(names(dt), 10), collapse = ", ")))
cat("\n")

# ===== 3. Prüfe Spalten =======================================================
cat("3. Prüfe erforderliche Spalten...\n")
required_cols <- c("open", "high", "low", "close", "volume")
for (col in required_cols) {
  exists <- col %in% names(dt)
  cat(sprintf("   %s: %s\n", col, ifelse(exists, "✓", "✗ FEHLT")))
}
cat("\n")

# ===== 4. Teste data.table Syntax =============================================
cat("4. Teste data.table Column Access...\n")

# Test 1: Direkter Zugriff
cat("   Test 1 - Direkter Zugriff (dt$high):\n")
tryCatch({
  test1 <- head(dt$high, 3)
  cat(sprintf("      ✓ Funktioniert: %s\n", paste(test1, collapse = ", ")))
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})

# Test 2: Within data.table
cat("   Test 2 - Within data.table (dt[, high]):\n")
tryCatch({
  test2 <- dt[, head(high, 3)]
  cat(sprintf("      ✓ Funktioniert: %s\n", paste(test2, collapse = ", ")))
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})

# Test 3: cbind within data.table
cat("   Test 3 - cbind(high, low, close):\n")
tryCatch({
  test3 <- head(cbind(dt$high, dt$low, dt$close), 3)
  cat("      ✓ Funktioniert (mit dt$)\n")
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})

# Test 4: with() für cbind
cat("   Test 4 - with(dt, cbind(high, low, close)):\n")
tryCatch({
  test4 <- head(with(dt, cbind(high, low, close)), 3)
  cat("      ✓ Funktioniert (mit with())\n")
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})
cat("\n")

# ===== 5. Teste einfache Indikatoren ==========================================
cat("5. Teste einfache Indikator-Berechnungen...\n")

# Test 5a: Einfache Spalte erstellen
cat("   Test 5a - dt[, test := high + low]:\n")
tryCatch({
  dt[, test := high + low]
  cat("      ✓ Funktioniert\n")
  dt[, test := NULL]  # Cleanup
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})

# Test 5b: EMA mit close
cat("   Test 5b - EMA(close, n = 9):\n")
tryCatch({
  ema_test <- EMA(dt$close, n = 9)
  cat(sprintf("      ✓ Funktioniert: %d Werte\n", length(ema_test)))
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})

# Test 5c: ATR mit cbind
cat("   Test 5c - ATR(cbind(high, low, close)):\n")
tryCatch({
  atr_test <- ATR(cbind(dt$high, dt$low, dt$close), n = 14)
  cat(sprintf("      ✓ Funktioniert (mit dt$): %d Zeilen\n", nrow(atr_test)))
}, error = function(e) {
  cat(sprintf("      ✗ Fehler: %s\n", e$message))
})
cat("\n")

# ===== 6. Lade Indikator-Funktion =============================================
cat("6. Lade calculate_all_indicators Funktion...\n")
source("r/02_01_indicator_calculation.R")
cat("   ✓ Funktion geladen\n\n")

# ===== 7. Teste mit kleinem Datensatz =========================================
cat("7. Teste mit kleinem Datensatz (100 Zeilen)...\n")
dt_small <- dt[1:100]

tryCatch({
  dt_result <- calculate_all_indicators(
    dt = dt_small,
    ema_periods = c(9),
    rsi_periods = c(14),
    atr_periods = c(14),
    adx_periods = c(14),
    bb_periods = c(20),
    kc_periods = c(20),
    verbose = TRUE
  )
  cat(sprintf("\n   ✓ ERFOLG! Indikatoren berechnet\n"))
  cat(sprintf("   Spalten vorher: %d\n", ncol(dt_small)))
  cat(sprintf("   Spalten nachher: %d\n", ncol(dt_result)))
  cat(sprintf("   Neue Features: %d\n\n", ncol(dt_result) - ncol(dt_small)))
}, error = function(e) {
  cat(sprintf("\n   ✗ FEHLER: %s\n\n", e$message))
  cat("   Traceback:\n")
  print(traceback())
})

cat("\n=== DEBUG ABGESCHLOSSEN ===\n")
