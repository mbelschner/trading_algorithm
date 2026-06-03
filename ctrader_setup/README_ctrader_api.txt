================================================================
cTrader Open API – Daten-Downloader (Python)
================================================================

Lädt historische Bars von deinem Pepperstone (oder IC Markets)
cTrader-Konto via offizielle Spotware Open API.

Funktioniert mit Demo UND Live-Konten. Du brauchst KEIN
TradingView-Abo dafür.


================================================================
EINMALIGES SETUP
================================================================

SCHRITT 1: Anwendung bei Spotware registrieren
-----------------------------------------------
1. Gehe zu https://connect.spotware.com/apps
2. Logge dich ein (Account erstellen falls nötig – das ist NICHT dein
   Pepperstone-Konto, sondern ein separater Spotware-Developer-Account!)
3. "Create new application" oder "Neue Anwendung":
   - Name: z.B. "DataDownloader"
   - Beschreibung: "Historical data download for personal use"
   - Redirect URI: http://localhost:8080/
   - Scope: "trading" (gibt Lesezugriff auf Daten)
4. Nach dem Erstellen siehst du:
   - Client ID
   - Client Secret
   Diese beiden Werte notieren.

WICHTIG: Neue Apps müssen ggf. von Spotware geprüft werden, BEVOR
sie auf Live-Konten zugreifen können. Für DEMO funktionieren sie
sofort. Da wir mit Demo starten, kein Problem.

SCHRITT 2: Python-Umgebung
---------------------------
   python -m venv .venv
   source .venv/bin/activate         # Linux/Mac
   .venv\Scripts\activate            # Windows
   pip install -r requirements.txt

SCHRITT 3: config.py erstellen
-------------------------------
Kopiere config_template.py → config.py und trage Client ID/Secret ein.

SCHRITT 4: OAuth-Token holen (einmalig)
----------------------------------------
   python 01_setup_oauth.py

Was passiert:
- Browser öffnet sich mit Spotware-Login-Seite
- Du loggst dich mit deinem Pepperstone/IC-Markets-Account ein
  (NICHT mit dem Spotware-Developer-Account!)
- Erlaubst der App den Zugriff
- Browser leitet auf localhost:8080 weiter
- Script fängt den Auth-Code ab, tauscht ihn gegen Access + Refresh Token
- Holt die Liste deiner Trading-Konten (Demo + Live)
- Du wählst das gewünschte Konto aus
- Speichert alles in tokens.json

SCHRITT 5: Symbol-Mapping erstellen (einmalig)
-----------------------------------------------
   python 02_list_symbols.py

Lädt alle bei deinem Broker verfügbaren Symbole und schreibt sie
nach symbols.csv. Du findest darin den genauen Symbol-Namen, die
symbolId (für API-Calls) und die "digits" (Preis-Nachkommastellen).


================================================================
DATEN HERUNTERLADEN (beliebig oft wiederholbar)
================================================================

   python 03_download.py --symbol XBRUSD --tf M5 \
       --from 2024-01-22 --to 2026-03-31 \
       --out OIL_BRENT_M5.csv

Parameter:
   --symbol     Symbol-Name (exakt wie in symbols.csv)
   --tf         Timeframe: M1, M5, M15, M30, H1, H4, D1
   --from       Start-Datum (YYYY-MM-DD)
   --to         End-Datum (YYYY-MM-DD)
   --out        Output-CSV-Pfad (optional, Default: <symbol>_<tf>.csv)

Beispiele für deine 13 Instrumente (passe Symbol-Namen nach
symbols.csv an – die Namen variieren je Broker!):

   python 03_download.py --symbol XBRUSD  --tf M5 --from 2024-01-22 --to 2026-03-31 --out OIL_BRENT_M5.csv
   python 03_download.py --symbol XAGUSD  --tf M5 --from 2024-01-22 --to 2026-03-31 --out SILVER_M5.csv
   python 03_download.py --symbol XAUUSD  --tf M5 --from 2024-01-22 --to 2026-03-31 --out GOLD_M5.csv
   python 03_download.py --symbol STOXX50 --tf M5 --from 2024-01-22 --to 2026-03-31 --out EU50_M5.csv
   python 03_download.py --symbol GER40   --tf M5 --from 2024-01-22 --to 2026-03-31 --out DE40_M5.csv
   python 03_download.py --symbol SPX500  --tf M5 --from 2024-01-22 --to 2026-03-31 --out US500_M5.csv
   python 03_download.py --symbol JPN225  --tf M5 --from 2024-01-22 --to 2026-03-31 --out J225_M5.csv
   python 03_download.py --symbol CHN50   --tf M5 --from 2024-01-22 --to 2026-03-31 --out CN50_M5.csv
   python 03_download.py --symbol HK50    --tf M5 --from 2024-01-22 --to 2026-03-31 --out HK50_M5.csv
   python 03_download.py --symbol EURUSD  --tf M5 --from 2024-01-22 --to 2026-03-31 --out EURUSD_M5.csv
   python 03_download.py --symbol AUDJPY  --tf M5 --from 2024-01-22 --to 2026-03-31 --out AUDJPY_M5.csv
   python 03_download.py --symbol USDJPY  --tf M5 --from 2024-01-22 --to 2026-03-31 --out USDJPY_M5.csv
   python 03_download.py --symbol EURNZD  --tf M5 --from 2024-01-22 --to 2026-03-31 --out EURNZD_M5.csv

Auch zusätzlich Brent 15min für Ichimoku-Strategie:
   python 03_download.py --symbol XBRUSD  --tf M15 --from 2024-01-22 --to 2026-03-31 --out OIL_BRENT_M15.csv


================================================================
CSV-FORMAT (kompatibel zu deinen bestehenden Capital.com-CSVs)
================================================================

   time,open,high,low,close,volume
   2024-01-22 00:00:00,79.95,79.98,79.92,79.96,142
   2024-01-22 00:05:00,79.96,80.01,79.94,80.00,189
   ...

Zeitzone: UTC. Volumen: Tick-Volumen (cTrader liefert kein Echt-Volumen
für CFDs – wie bei Capital.com auch).


================================================================
WICHTIGE HINWEISE
================================================================

1. PAGINATION: Bei M5 über mehrere Jahre macht das Script automatisch
   30-Tage-Chunks (sonst limitiert die API). Erwartete Laufzeit pro
   Instrument: 1–3 Minuten für 2+ Jahre M5.

2. PREISFORMAT: cTrader liefert Bars im "delta-kodierten" Format
   (low + deltaOpen/High/Close). Script dekodiert das automatisch
   anhand der Symbol-"digits" (Nachkommastellen).

3. RATE LIMITS: Die API erlaubt ~50 Requests/Sekunde. Das Script
   wartet 200ms zwischen Chunks → unproblematisch.

4. TOKEN-EXPIRY: Der Access-Token läuft nach ~30 Tagen ab. Falls
   das passiert: erneut "python 01_setup_oauth.py" laufen lassen.
   (Optional: man könnte den Refresh-Token automatisch nutzen –
   für dieses Setup nicht eingebaut, weil 30 Tage reichen, um deine
   Initial-Backtest-Daten zu holen.)

5. UNTERSCHIED ZU CAPITAL.COM: Die Daten kommen von Pepperstone/IC.
   Bars-Werte werden leicht abweichen wegen unterschiedlicher
   Preis-Feeds. Das ist exakt der Punkt – DIESE Daten gelten für
   den neuen Broker, und auf diesen musst du backtesten.

6. DEMO vs LIVE: tokens.json enthält ein Flag "isLive". Du kannst
   für Demo UND Live je ein eigenes Token-Set anlegen (z.B.
   tokens_demo.json + tokens_live.json). Für den Daten-Download
   spielt das keine Rolle – die Bars sind gleich.


================================================================
DATEIEN IN DIESEM PAKET
================================================================

   README_ctrader_api.txt    Diese Datei
   requirements.txt          Python-Dependencies
   config_template.py        Template für Credentials (→ config.py)
   01_setup_oauth.py         OAuth-Flow + Token-Persistenz
   02_list_symbols.py        Symbol-Liste exportieren
   03_download.py            Bar-Daten downloaden (per CLI)
