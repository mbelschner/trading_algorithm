"""
Databento OHLCV Downloader
==========================
Lädt 1-Minuten OHLCV-Daten von Databento und aggregiert auf 5 Minuten.
Symbole sind an die Capital.com Webhook-Symbole angelehnt (manuell anpassbar).

Voraussetzungen:
    pip install databento pandas pyarrow

Verwendung:
    1. API-Key in DATABENTO_API_KEY eintragen (oder als Umgebungsvariable setzen)
    2. SYMBOLS-Dict anpassen (Capital.com-Symbol → Databento-Symbol + Dataset)
    3. DOWNLOAD_SYMBOLS-Liste auf gewünschte Instrumente einschränken
    4. Script ausführen: python databento_downloader.py
"""

import os
import time
import logging
from datetime import datetime, timezone, timedelta
from pathlib import Path

from dotenv import load_dotenv
import databento as db
import pandas as pd

load_dotenv(Path(__file__).parent / ".env")

# =============================================================================
# KONFIGURATION – hier anpassen
# =============================================================================

# API-Key: entweder direkt eintragen oder als Umgebungsvariable DATABENTO_API_KEY setzen
DATABENTO_API_KEY = os.environ.get("DATABENTO_API_KEY", "DEIN_API_KEY_HIER")

# Zeitraum: letzte 5 Jahre
END_DATE   = datetime.now(timezone.utc).date()
START_DATE = END_DATE - timedelta(days=100)

# Ausgabe-Verzeichnis
OUTPUT_DIR = Path("./databento_data")

# -----------------------------------------------------------------------------
# SYMBOL-MAPPING
# Schlüssel  = Capital.com Webhook-Symbol (wie in deinem Bot verwendet)
# Wert       = dict mit Databento-Parametern
#
# Hinweis: Databento deckt hauptsächlich Futures/Equities ab.
# Für CFDs (Capital.com) gibt es kein direktes Databento-Äquivalent –
# du musst das jeweilige Futures-Instrument nehmen, das dem CFD zugrunde liegt.
#
# Verfügbare Datasets (Auswahl):
#   GLBX.MDP3  = CME Globex (Gold, Silver, Brent-ähnlich via CL, ES, NQ, etc.)
#   XNAS.ITCH  = Nasdaq
#   IFEU.IMPACT= ICE Europe (Brent Crude)
#   XCME.MDP3  = CME (Währungen)
# -----------------------------------------------------------------------------
SYMBOLS = {
    # ── Rohstoffe ──────────────────────────────────────────────────────────
    "GOLD": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "GCc1",          # Gold Continuous Front-Month Future (CME)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "SILVER": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "SIc1",          # Silver Continuous Future (CME)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "OIL_BRENT": {
        "dataset":    "IFEU.IMPACT",
        "symbol":     "Bc1",           # Brent Crude Continuous Future (ICE)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },

    # ── Aktienindizes ──────────────────────────────────────────────────────
    "US500": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "ESc1",          # E-mini S&P 500 Continuous Future (CME)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "DE40": {
        "dataset":    "EUREX",
        "symbol":     "FDXc1",         # DAX Future Continuous (Eurex)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "EU50": {
        "dataset":    "EUREX",
        "symbol":     "FESTc1",        # Euro Stoxx 50 Future Continuous (Eurex)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "J225": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "NIYc1",         # Nikkei 225 Yen Future Continuous (CME)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "HK50": {
        "dataset":    "HKEX",
        "symbol":     "HSIc1",         # Hang Seng Index Future Continuous
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "CN50": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "CNc1",          # FTSE China 50 Future (prüfe Verfügbarkeit)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },

    # ── Währungspaare (FX Futures als Proxy) ───────────────────────────────
    "EURUSD": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "E6c1",          # Euro FX Continuous Future (CME)
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "USDJPY": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "J6c1",          # Japanese Yen Continuous Future (CME) – invertiert zu USDJPY
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "AUDJPY": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "A6c1",          # Australian Dollar Future (CME), kombiniert mit J6c1
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
    "EURNZD": {
        "dataset":    "GLBX.MDP3",
        "symbol":     "E6c1",          # Kein direktes EURNZD-Future; E6 als Näherung
        "schema":     "ohlcv-1m",
        "stype_in":   "continuous",
    },
}

# Welche Symbole sollen heruntergeladen werden?
# Einfach die nicht benötigten auskommentieren:
DOWNLOAD_SYMBOLS = [
    "GOLD",
    "SILVER",
    #"OIL_BRENT",
    #"US500",
    #"DE40",
    #"EU50",
    #"J225",
    # "HK50",
    # "CN50",
    #"EURUSD",
    #"USDJPY",
    #"AUDJPY",
    # "EURNZD",
]

# =============================================================================
# LOGGING
# =============================================================================
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)


# =============================================================================
# HILFSFUNKTIONEN
# =============================================================================

def download_1m(client: db.Historical, cap_symbol: str, cfg: dict) -> pd.DataFrame:
    """Lädt 1m OHLCV-Daten von Databento und gibt einen DataFrame zurück."""
    log.info(f"[{cap_symbol}] Lade 1m-Daten  {START_DATE} → {END_DATE}  ...")

    data = client.timeseries.get_range(
        dataset   = cfg["dataset"],
        symbols   = [cfg["symbol"]],
        schema    = cfg["schema"],
        start     = str(START_DATE),
        end       = str(END_DATE),
        stype_in  = cfg.get("stype_in", "raw_symbol"),
    )

    df = data.to_df()

    # Spaltennamen normalisieren
    df = df.rename(columns={
        "open":   "open",
        "high":   "high",
        "low":    "low",
        "close":  "close",
        "volume": "volume",
    })

    # Index → Spalte "time" in UTC
    if df.index.name in ("ts_event", "timestamp"):
        df = df.reset_index().rename(columns={df.index.name: "time"})
    elif "ts_event" in df.columns:
        df = df.rename(columns={"ts_event": "time"})

    df["time"] = pd.to_datetime(df["time"], utc=True)
    df = df[["time", "open", "high", "low", "close", "volume"]].copy()
    df = df.sort_values("time").reset_index(drop=True)

    log.info(f"[{cap_symbol}] 1m-Daten geladen: {len(df):,} Zeilen")
    return df


def aggregate_to_5m(df_1m: pd.DataFrame) -> pd.DataFrame:
    """Aggregiert einen 1m-DataFrame auf 5-Minuten-Bars."""
    df = df_1m.set_index("time")

    df_5m = df.resample("5min", label="left", closed="left").agg(
        open   = ("open",   "first"),
        high   = ("high",   "max"),
        low    = ("low",    "min"),
        close  = ("close",  "last"),
        volume = ("volume", "sum"),
    ).dropna(subset=["open"])

    df_5m = df_5m.reset_index()
    df_5m["time"] = df_5m["time"].dt.tz_convert("UTC")
    return df_5m


def save_csv(df: pd.DataFrame, path: Path) -> None:
    df.to_csv(path, index=False, date_format="%Y-%m-%d %H:%M:%S")
    log.info(f"  → Gespeichert: {path}  ({len(df):,} Zeilen)")


# =============================================================================
# MAIN
# =============================================================================

def main():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    if DATABENTO_API_KEY == "DEIN_API_KEY_HIER":
        raise ValueError(
            "Kein API-Key gefunden. "
            "Setze DATABENTO_API_KEY als Umgebungsvariable oder direkt im Script."
        )

    client = db.Historical(DATABENTO_API_KEY)
    log.info(f"Zeitraum: {START_DATE} → {END_DATE}")
    log.info(f"Symbole:  {DOWNLOAD_SYMBOLS}")
    log.info(f"Output:   {OUTPUT_DIR.resolve()}")
    print()

    errors = []

    for cap_symbol in DOWNLOAD_SYMBOLS:
        if cap_symbol not in SYMBOLS:
            log.warning(f"[{cap_symbol}] Nicht in SYMBOLS-Dict definiert – übersprungen.")
            continue

        cfg = SYMBOLS[cap_symbol]

        try:
            # ── 1-Minuten-Daten herunterladen ──────────────────────────────
            df_1m = download_1m(client, cap_symbol, cfg)

            path_1m = OUTPUT_DIR / f"{cap_symbol}_MINUTE_1.csv"
            save_csv(df_1m, path_1m)

            # ── Auf 5 Minuten aggregieren ───────────────────────────────────
            df_5m = aggregate_to_5m(df_1m)
            log.info(f"[{cap_symbol}] Aggregiert auf 5m: {len(df_5m):,} Zeilen")

            path_5m = OUTPUT_DIR / f"{cap_symbol}_MINUTE_5.csv"
            save_csv(df_5m, path_5m)

        except Exception as exc:
            log.error(f"[{cap_symbol}] FEHLER: {exc}")
            errors.append((cap_symbol, str(exc)))

        # Kurze Pause zwischen Requests (Rate-Limit-Schutz)
        time.sleep(1)
        print()

    # ── Abschluss-Report ───────────────────────────────────────────────────
    print("=" * 60)
    log.info(f"Fertig. {len(DOWNLOAD_SYMBOLS) - len(errors)}/{len(DOWNLOAD_SYMBOLS)} Symbole erfolgreich.")
    if errors:
        log.warning("Fehlerhafte Symbole:")
        for sym, err in errors:
            log.warning(f"  {sym}: {err}")
    print("=" * 60)


if __name__ == "__main__":
    main()