# =============================================================================
# ALTERNATIVE LABELING METHODEN
# Zum Vergleich mit Triple Barrier Labeling
# =============================================================================
#
# Methoden:
# 1. Local Extrema (Swing Points) - mit Look-Ahead Bias Vermeidung
# 2. Trend Scanning (López de Prado) - t-Statistik basiert
# 3. Directional Change - Event-basiert
# 4. Fixed-Time Horizon (Baseline) - zum Vergleich
#
# =============================================================================

library(data.table)
library(TTR)

# =============================================================================
# METHODE 1: LOCAL EXTREMA (SWING POINTS) - VERBESSERT
# =============================================================================
#
# Problem: Standard Local Extrema hat LOOK-AHEAD BIAS!
# Lösung: Confirmation-based Detection
#
# Vorteile:
#   + Identifiziert echte Wendepunkte
#   + Gut für Mean-Reversion Strategien
#   + Intuitiv verständlich
#
# Nachteile:
#   - Inherenter Look-Ahead Bias (braucht zukünftige Daten zur Bestätigung)
#   - Weniger Samples (nur an Extrempunkten)
#   - Schwer in Realtime zu handeln
#
# =============================================================================

create_local_extrema_labels <- function(
    prices,
    window_size = 5,           # Bars links UND rechts zum Vergleich
    confirmation_bars = 3,     # Zusätzliche Bars zur Bestätigung
    atr_period = 14,
    min_move_atr = 0.5,        # Minimum Bewegung in ATR-Einheiten
    session_start = 8,
    session_end = 19
) {
  
  cat("=== LOCAL EXTREMA LABELING ===\n")
  cat("Window Size:", window_size, "(", 2*window_size+1, "Bars total)\n")
  cat("Confirmation Bars:", confirmation_bars, "\n")
  cat("Min Move:", min_move_atr, "x ATR\n\n")
  
  # Daten vorbereiten
  if(inherits(prices, "xts")) {
    dt <- data.table(
      datetime = index(prices),
      high = as.numeric(prices$High),
      low = as.numeric(prices$Low),
      close = as.numeric(prices$Close)
    )
  } else {
    dt <- as.data.table(prices)
    setnames(dt, tolower(names(dt)))
  }
  
  dt[, hour := hour(datetime)]
  dt[, in_session := hour >= session_start & hour < session_end]
  
  # ATR
  atr_result <- ATR(cbind(dt$high, dt$low, dt$close), n = atr_period)
  dt[, atr := atr_result[, "atr"]]
  
  # Initialisiere
  dt[, `:=`(
    is_swing_high = FALSE,
    is_swing_low = FALSE,
    label = NA_integer_,
    swing_type = NA_character_
  )]
  
  n <- nrow(dt)
  total_window <- window_size + confirmation_bars
  
  # -------------------------------------------------------------------------
  # Swing Points identifizieren (MIT Bestätigung)
  # -------------------------------------------------------------------------
  
  cat("Identifiziere Swing Points...\n")
  
  for(i in (total_window + 1):(n - total_window)) {
    
    if(!dt$in_session[i]) next
    
    # Definiere Fenster
    left_start <- i - window_size - confirmation_bars
    left_end <- i - confirmation_bars
    right_start <- i + 1
    right_end <- i + window_size
    
    left_highs <- dt$high[left_start:left_end]
    left_lows <- dt$low[left_start:left_end]
    right_highs <- dt$high[right_start:right_end]
    right_lows <- dt$low[right_start:right_end]
    
    current_high <- dt$high[i]
    current_low <- dt$low[i]
    current_atr <- dt$atr[i]
    
    if(is.na(current_atr)) next
    
    # Swing High: Höher als alle links UND alle rechts
    is_high <- current_high > max(left_highs) & 
      current_high > max(right_highs)
    
    # Swing Low: Niedriger als alle links UND alle rechts
    is_low <- current_low < min(left_lows) & 
      current_low < min(right_lows)
    
    # Zusätzlich: Mindestbewegung prüfen
    if(is_high) {
      move_from_left <- (current_high - min(left_lows)) / current_atr
      move_to_right <- (current_high - min(right_lows)) / current_atr
      
      if(move_from_left >= min_move_atr & move_to_right >= min_move_atr) {
        dt$is_swing_high[i] <- TRUE
      }
    }
    
    if(is_low) {
      move_from_left <- (max(left_highs) - current_low) / current_atr
      move_to_right <- (max(right_highs) - current_low) / current_atr
      
      if(move_from_left >= min_move_atr & move_to_right >= min_move_atr) {
        dt$is_swing_low[i] <- TRUE
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # Labels zwischen Swing Points
  # -------------------------------------------------------------------------
  
  swing_highs <- which(dt$is_swing_high)
  swing_lows <- which(dt$is_swing_low)
  
  cat("Gefundene Swing Highs:", length(swing_highs), "\n")
  cat("Gefundene Swing Lows:", length(swing_lows), "\n")
  
  # Alle Swings kombinieren und sortieren
  all_swings <- sort(c(swing_highs, swing_lows))
  
  if(length(all_swings) < 2) {
    cat("WARNUNG: Zu wenige Swing Points gefunden!\n")
    return(dt[!is.na(label)])
  }
  
  # Labels setzen: Zwischen zwei Swings
  for(j in 1:(length(all_swings) - 1)) {
    
    start_idx <- all_swings[j]
    end_idx <- all_swings[j + 1]
    
    start_is_high <- dt$is_swing_high[start_idx]
    start_is_low <- dt$is_swing_low[start_idx]
    
    # Von Swing Low zu Swing High = Aufwärtstrend = Label 1
    # Von Swing High zu Swing Low = Abwärtstrend = Label -1
    
    if(start_is_low) {
      # Aufwärts (Long)
      dt$label[start_idx:end_idx] <- 1L
      dt$swing_type[start_idx] <- "low_to_high"
    } else if(start_is_high) {
      # Abwärts (Short)
      dt$label[start_idx:end_idx] <- -1L
      dt$swing_type[start_idx] <- "high_to_low"
    }
  }
  
  # -------------------------------------------------------------------------
  # Ergebnis
  # -------------------------------------------------------------------------
  
  labeled_dt <- dt[!is.na(label) & in_session]
  
  cat("\n=== ERGEBNIS ===\n")
  cat("Total labeled:", nrow(labeled_dt), "\n")
  cat("\nLabel-Verteilung:\n")
  print(table(labeled_dt$label))
  
  # WICHTIGE WARNUNG
  cat("\n⚠️  WARNUNG: Local Extrema hat inherenten Look-Ahead Bias!\n")
  cat("   Die Labels verwenden zukünftige Daten zur Bestätigung.\n")
  cat("   Für ML-Training: Verwende diese Labels NUR für Analyse,\n")
  cat("   nicht als primäre Labeling-Methode.\n")
  
  return(labeled_dt)
}


# =============================================================================
# METHODE 2: TREND SCANNING (López de Prado)
# =============================================================================
#
# Konzept: Für jeden Zeitpunkt, finde den optimalen Vorhersage-Horizont
# durch Regression mit höchstem t-Statistik Wert.
#
# Vorteile:
#   + Adaptiver Horizont (nicht fixiert)
#   + Statistisch fundiert
#   + Kein Look-Ahead Bias wenn richtig implementiert
#   + T-Statistik als Sample Weight verwendbar
#
# Nachteile:
#   - Rechenintensiv
#   - Komplexer zu verstehen
#   - Braucht sorgfältige Parameter-Wahl
#
# =============================================================================

create_trend_scanning_labels <- function(
    prices,
    horizons = c(4, 8, 12, 16, 20, 24),  # Verschiedene Horizonte testen
    min_t_stat = 2.0,                    # Minimum t-Statistik für signifikanten Trend
    session_start = 8,
    session_end = 19
) {
  
  cat("=== TREND SCANNING LABELING ===\n")
  cat("Horizonte:", paste(horizons, collapse = ", "), "\n")
  cat("Min t-Statistik:", min_t_stat, "\n\n")
  
  # Daten vorbereiten
  if(inherits(prices, "xts")) {
    dt <- data.table(
      datetime = index(prices),
      close = as.numeric(prices$Close)
    )
  } else {
    dt <- as.data.table(prices)
    setnames(dt, tolower(names(dt)))
  }
  
  dt[, hour := hour(datetime)]
  dt[, in_session := hour >= session_start & hour < session_end]
  dt[, log_price := log(close)]
  
  # Initialisiere
  dt[, `:=`(
    label = NA_integer_,
    best_horizon = NA_integer_,
    t_stat = NA_real_,
    trend_strength = NA_real_
  )]
  
  n <- nrow(dt)
  max_horizon <- max(horizons)
  
  cat("Scanne Trends für", sum(dt$in_session & 1:n <= (n - max_horizon)), "Observations...\n")
  
  # Progress
  valid_count <- 0
  
  for(i in 1:(n - max_horizon)) {
    
    if(!dt$in_session[i]) next
    
    best_t <- 0
    best_h <- NA
    best_slope <- NA
    
    # Teste alle Horizonte
    for(h in horizons) {
      
      if(i + h > n) next
      
      # Log-Returns über Horizont h
      y <- dt$log_price[(i+1):(i+h)] - dt$log_price[i]
      x <- 1:h
      
      # Lineare Regression: y = alpha + beta * x
      fit <- lm(y ~ x)
      
      # T-Statistik für Steigung
      coefs <- summary(fit)$coefficients
      if(nrow(coefs) >= 2) {
        t_stat <- coefs[2, "t value"]
        slope <- coefs[2, "Estimate"]
        
        # Wähle Horizont mit höchstem |t|
        if(abs(t_stat) > abs(best_t)) {
          best_t <- t_stat
          best_h <- h
          best_slope <- slope
        }
      }
    }
    
    # Label zuweisen wenn signifikant
    if(!is.na(best_t) && abs(best_t) >= min_t_stat) {
      dt$label[i] <- as.integer(sign(best_t))
      dt$best_horizon[i] <- best_h
      dt$t_stat[i] <- best_t
      dt$trend_strength[i] <- abs(best_t)  # Kann als Sample Weight verwendet werden
      valid_count <- valid_count + 1
    } else {
      dt$label[i] <- 0L  # Kein signifikanter Trend
      dt$t_stat[i] <- best_t
    }
    
    # Progress alle 1000 Iterationen
    if(i %% 1000 == 0) {
      cat(sprintf("  Progress: %d/%d (%.1f%%)\n", i, n - max_horizon, i/(n-max_horizon)*100))
    }
  }
  
  # -------------------------------------------------------------------------
  # Ergebnis
  # -------------------------------------------------------------------------
  
  labeled_dt <- dt[!is.na(label) & in_session]
  
  cat("\n=== ERGEBNIS ===\n")
  cat("Total labeled:", nrow(labeled_dt), "\n")
  cat("Davon signifikant (|t| >=", min_t_stat, "):", sum(labeled_dt$label != 0), "\n")
  
  cat("\nLabel-Verteilung:\n")
  print(table(labeled_dt$label))
  
  cat("\nHorizont-Verteilung (für signifikante Trends):\n")
  print(table(labeled_dt[label != 0, best_horizon]))
  
  cat("\nT-Statistik Summary:\n")
  print(summary(labeled_dt[label != 0, t_stat]))
  
  return(labeled_dt)
}


# =============================================================================
# METHODE 3: DIRECTIONAL CHANGE
# =============================================================================
#
# Konzept: Event-basiertes Sampling bei signifikanten Preisbewegungen
# Ein "Event" tritt auf wenn Preis um θ% von letztem Extremum abweicht.
#
# Vorteile:
#   + Natürliche Filter für Rauschen
#   + Event-driven (wie echtes Trading)
#   + Gut für Regime-Detection
#
# Nachteile:
#   - Bestätigungsverzögerung (inherent)
#   - Parameter θ kritisch
#   - Weniger Samples in ruhigen Märkten
#
# =============================================================================

create_directional_change_labels <- function(
    prices,
    theta = 0.005,             # Schwellenwert für Richtungswechsel (0.5%)
    session_start = 8,
    session_end = 19
) {
  
  cat("=== DIRECTIONAL CHANGE LABELING ===\n")
  cat("Theta (Schwellenwert):", theta * 100, "%\n\n")
  
  # Daten vorbereiten
  if(inherits(prices, "xts")) {
    dt <- data.table(
      datetime = index(prices),
      high = as.numeric(prices$High),
      low = as.numeric(prices$Low),
      close = as.numeric(prices$Close)
    )
  } else {
    dt <- as.data.table(prices)
    setnames(dt, tolower(names(dt)))
  }
  
  dt[, hour := hour(datetime)]
  dt[, in_session := hour >= session_start & hour < session_end]
  
  # Initialisiere
  dt[, `:=`(
    label = NA_integer_,
    dc_event = NA_character_,
    extreme_price = NA_real_
  )]
  
  n <- nrow(dt)
  
  # State-Variablen
  mode <- 0  # 0 = undefiniert, 1 = uptrend, -1 = downtrend
  extreme_high <- dt$high[1]
  extreme_low <- dt$low[1]
  extreme_high_idx <- 1
  extreme_low_idx <- 1
  
  dc_events <- list()
  
  cat("Scanne Directional Changes...\n")
  
  for(i in 2:n) {
    
    current_high <- dt$high[i]
    current_low <- dt$low[i]
    
    if(mode == 0) {
      # Initialisierung: Warte auf ersten klaren Move
      if(current_high > extreme_high * (1 + theta)) {
        mode <- 1  # Uptrend
        extreme_high <- current_high
        extreme_high_idx <- i
        
        # Labele Bars seit letztem Low als "uptrend"
        dt$label[extreme_low_idx:i] <- 1L
        dt$dc_event[extreme_low_idx] <- "upturn"
        
      } else if(current_low < extreme_low * (1 - theta)) {
        mode <- -1  # Downtrend
        extreme_low <- current_low
        extreme_low_idx <- i
        
        # Labele Bars seit letztem High als "downtrend"
        dt$label[extreme_high_idx:i] <- -1L
        dt$dc_event[extreme_high_idx] <- "downturn"
      }
      
      # Update Extrema
      if(current_high > extreme_high) {
        extreme_high <- current_high
        extreme_high_idx <- i
      }
      if(current_low < extreme_low) {
        extreme_low <- current_low
        extreme_low_idx <- i
      }
      
    } else if(mode == 1) {
      # Im Uptrend: Suche nach Downturn
      
      # Update High wenn neues High
      if(current_high > extreme_high) {
        extreme_high <- current_high
        extreme_high_idx <- i
      }
      
      # Check für Downturn
      if(current_low < extreme_high * (1 - theta)) {
        # Downturn bestätigt!
        mode <- -1
        extreme_low <- current_low
        extreme_low_idx <- i
        
        # Labele bisherige Uptrend-Phase
        # (wurde schon progressiv gelabelt)
        dt$dc_event[extreme_high_idx] <- "downturn"
        
        dc_events[[length(dc_events) + 1]] <- list(
          type = "downturn",
          idx = extreme_high_idx,
          price = extreme_high
        )
      } else {
        # Weiter im Uptrend
        dt$label[i] <- 1L
      }
      
    } else if(mode == -1) {
      # Im Downtrend: Suche nach Upturn
      
      # Update Low wenn neues Low
      if(current_low < extreme_low) {
        extreme_low <- current_low
        extreme_low_idx <- i
      }
      
      # Check für Upturn
      if(current_high > extreme_low * (1 + theta)) {
        # Upturn bestätigt!
        mode <- 1
        extreme_high <- current_high
        extreme_high_idx <- i
        
        # Labele bisherige Downtrend-Phase
        dt$dc_event[extreme_low_idx] <- "upturn"
        
        dc_events[[length(dc_events) + 1]] <- list(
          type = "upturn",
          idx = extreme_low_idx,
          price = extreme_low
        )
      } else {
        # Weiter im Downtrend
        dt$label[i] <- -1L
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # Ergebnis
  # -------------------------------------------------------------------------
  
  labeled_dt <- dt[!is.na(label) & in_session]
  
  cat("\n=== ERGEBNIS ===\n")
  cat("Total labeled:", nrow(labeled_dt), "\n")
  cat("Directional Change Events:", length(dc_events), "\n")
  
  cat("\nLabel-Verteilung:\n")
  print(table(labeled_dt$label))
  
  cat("\nDC Event-Typen:\n")
  print(table(dt$dc_event, useNA = "no"))
  
  return(labeled_dt)
}


# =============================================================================
# METHODE 4: FIXED-TIME HORIZON (BASELINE)
# =============================================================================
#
# Die einfachste Methode - nur zum VERGLEICH!
# López de Prado warnt explizit davor, diese Methode zu verwenden.
#
# Probleme:
#   - Ignoriert Volatilität
#   - Ignoriert Pfad (Stop-Loss wäre ausgelöst worden)
#   - Führt zu unrealistischen Backtests
#
# =============================================================================

create_fixed_horizon_labels <- function(
    prices,
    horizon = 16,              # Fixer Horizont in Bars (4h bei 15min)
    threshold = 0.002,         # Fixer Schwellenwert (0.2%)
    session_start = 8,
    session_end = 19
) {
  
  cat("=== FIXED-TIME HORIZON LABELING (BASELINE) ===\n")
  cat("⚠️  WARNUNG: Diese Methode wird NICHT empfohlen!\n")
  cat("   Nur zum Vergleich mit besseren Methoden.\n\n")
  cat("Horizont:", horizon, "Bars\n")
  cat("Schwellenwert:", threshold * 100, "%\n\n")
  
  # Daten vorbereiten
  if(inherits(prices, "xts")) {
    dt <- data.table(
      datetime = index(prices),
      close = as.numeric(prices$Close)
    )
  } else {
    dt <- as.data.table(prices)
    setnames(dt, tolower(names(dt)))
  }
  
  dt[, hour := hour(datetime)]
  dt[, in_session := hour >= session_start & hour < session_end]
  
  # Initialisiere
  dt[, `:=`(
    label = NA_integer_,
    forward_return = NA_real_
  )]
  
  n <- nrow(dt)
  
  for(i in 1:(n - horizon)) {
    
    if(!dt$in_session[i]) next
    
    # Einfacher Forward Return
    forward_return <- (dt$close[i + horizon] - dt$close[i]) / dt$close[i]
    dt$forward_return[i] <- forward_return
    
    # Label basiert auf festem Schwellenwert
    if(forward_return > threshold) {
      dt$label[i] <- 1L
    } else if(forward_return < -threshold) {
      dt$label[i] <- -1L
    } else {
      dt$label[i] <- 0L
    }
  }
  
  # -------------------------------------------------------------------------
  # Ergebnis
  # -------------------------------------------------------------------------
  
  labeled_dt <- dt[!is.na(label) & in_session]
  
  cat("\n=== ERGEBNIS ===\n")
  cat("Total labeled:", nrow(labeled_dt), "\n")
  
  cat("\nLabel-Verteilung:\n")
  print(table(labeled_dt$label))
  
  cat("\n⚠️  ERINNERUNG: Diese Labels ignorieren:\n")
  cat("   - Volatilitätsänderungen\n")
  cat("   - Den Pfad (Stop-Loss Trigger)\n")
  cat("   - Session-Grenzen\n")
  
  return(labeled_dt)
}


# =============================================================================
# VERGLEICHSFUNKTION: ALLE METHODEN
# =============================================================================

compare_labeling_methods <- function(prices, 
                                     session_start = 8, 
                                     session_end = 19) {
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║           VERGLEICH ALLER LABELING-METHODEN                      ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n\n")
  
  results <- list()
  
  # 1. Triple Barrier (aus anderem Script)
  cat("─── 1. TRIPLE BARRIER ───\n")
  if(exists("create_triple_barrier_labels")) {
    tb <- create_triple_barrier_labels(prices, session_start = session_start, 
                                       session_end = session_end)
    results$triple_barrier <- tb
  } else {
    cat("   (Script nicht geladen - source('triple_barrier_labeling.R'))\n")
  }
  
  # 2. Local Extrema
  cat("\n─── 2. LOCAL EXTREMA ───\n")
  le <- create_local_extrema_labels(prices, session_start = session_start, 
                                    session_end = session_end)
  results$local_extrema <- le
  
  # 3. Trend Scanning
  cat("\n─── 3. TREND SCANNING ───\n")
  ts <- create_trend_scanning_labels(prices, session_start = session_start, 
                                     session_end = session_end)
  results$trend_scanning <- ts
  
  # 4. Directional Change
  cat("\n─── 4. DIRECTIONAL CHANGE ───\n")
  dc <- create_directional_change_labels(prices, session_start = session_start, 
                                         session_end = session_end)
  results$directional_change <- dc
  
  # 5. Fixed Horizon (Baseline)
  cat("\n─── 5. FIXED HORIZON (BASELINE) ───\n")
  fh <- create_fixed_horizon_labels(prices, session_start = session_start, 
                                    session_end = session_end)
  results$fixed_horizon <- fh
  
  # -------------------------------------------------------------------------
  # Zusammenfassung
  # -------------------------------------------------------------------------
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════╗\n")
  cat("║                    ZUSAMMENFASSUNG                               ║\n")
  cat("╚══════════════════════════════════════════════════════════════════╝\n\n")
  
  summary_dt <- data.table(
    Methode = character(),
    N_Samples = integer(),
    Pct_Long = numeric(),
    Pct_Short = numeric(),
    Pct_Neutral = numeric(),
    Balance_Score = numeric()
  )
  
  for(name in names(results)) {
    dt <- results[[name]]
    if(is.null(dt) || nrow(dt) == 0) next
    
    label_counts <- table(factor(dt$label, levels = c(-1, 0, 1)))
    total <- sum(label_counts)
    
    pct_long <- label_counts["1"] / total * 100
    pct_short <- label_counts["-1"] / total * 100
    pct_neutral <- label_counts["0"] / total * 100
    
    # Balance Score (0 = perfekt, 1 = total unbalanced)
    expected <- total / 3
    balance <- sum(abs(label_counts - expected)) / (2 * total)
    
    summary_dt <- rbind(summary_dt, data.table(
      Methode = name,
      N_Samples = nrow(dt),
      Pct_Long = round(pct_long, 1),
      Pct_Short = round(pct_short, 1),
      Pct_Neutral = round(pct_neutral, 1),
      Balance_Score = round(balance, 3)
    ))
  }
  
  print(summary_dt)
  
  cat("\n=== EMPFEHLUNG FÜR DEIN USE CASE ===\n")
  cat("(Gold/Silber/Kupfer, 15min, 8-19 Uhr Session)\n\n")
  
  cat("1. TRIPLE BARRIER (EMPFOHLEN für primäres Labeling)\n")
  cat("   → Realistisch, berücksichtigt Stop-Loss\n")
  cat("   → Dynamische ATR-basierte Schwellen\n")
  cat("   → Session-bounded\n\n")
  
  cat("2. TREND SCANNING (EMPFOHLEN für Feature Engineering)\n")
  cat("   → t-Statistik als Sample Weight\n")
  cat("   → Adaptiver Horizont\n")
  cat("   → Kein Look-Ahead Bias\n\n")
  
  cat("3. LOCAL EXTREMA (NUR für Analyse)\n")
  cat("   → Hat Look-Ahead Bias!\n")
  cat("   → Gut um Marktstruktur zu verstehen\n")
  cat("   → NICHT für ML-Training verwenden\n\n")
  
  cat("4. DIRECTIONAL CHANGE (Alternative)\n")
  cat("   → Event-basiert, natürlicher Filter\n")
  cat("   → Gut für Regime-Detection\n")
  cat("   → Weniger Samples\n\n")
  
  cat("5. FIXED HORIZON (VERMEIDEN)\n")
  cat("   → Unrealistisch\n")
  cat("   → Ignoriert Volatilität und Pfad\n")
  cat("   → Nur als Baseline zum Vergleich\n")
  
  return(results)
}


cat("\n=== ALTERNATIVE LABELING METHODEN GELADEN ===\n")
cat("Verfügbare Funktionen:\n")
cat("  - create_local_extrema_labels(prices, ...)\n")
cat("  - create_trend_scanning_labels(prices, ...)\n")
cat("  - create_directional_change_labels(prices, ...)\n")
cat("  - create_fixed_horizon_labels(prices, ...)\n")
cat("  - compare_labeling_methods(prices, ...)\n")