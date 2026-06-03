"""
03_download.py — Lädt historische OHLCV-Bars für EIN Instrument
und schreibt sie als CSV im Format kompatibel zu deinen bestehenden
Capital.com-CSVs.

Format:    time,open,high,low,close,volume
Zeitzone:  UTC

Beispiele:
   python 03_download.py --symbol XBRUSD --tf M5 --from 2024-01-22 --to 2026-03-31
   python 03_download.py --symbol GER40  --tf M5 --from 2024-01-22 --to 2026-03-31 --out DE40_M5.csv
   python 03_download.py --symbol XBRUSD --tf M15 --from 2024-01-22 --to 2026-03-31 --out OIL_BRENT_M15.csv

Hinweise:
- M5 über mehrere Jahre wird automatisch in 30-Tage-Chunks geladen
  (API-Limit pro Request). Erwarte 1–3 Minuten Laufzeit für 2+ Jahre.
- Symbol-Namen müssen exakt mit symbols.csv übereinstimmen (Case-sensitive).
- "volume" ist Tick-Volumen (CFDs liefern kein Echt-Volumen).
"""

import argparse
import csv
import json
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

# Default output directory: price_data/ctrader_data/ relative to project root
_SCRIPT_DIR = Path(__file__).parent
_DEFAULT_OUT_DIR = _SCRIPT_DIR.parent / "price_data" / "ctrader_data"

from twisted.internet import reactor, task

from ctrader_open_api import Client, EndPoints, Protobuf, TcpProtocol
from ctrader_open_api.messages.OpenApiMessages_pb2 import (
    ProtoOAApplicationAuthReq,
    ProtoOAAccountAuthReq,
    ProtoOAGetTrendbarsReq,
    ProtoOAGetTrendbarsRes,
    ProtoOAErrorRes,
)
from ctrader_open_api.messages.OpenApiModelMessages_pb2 import (
    ProtoOATrendbarPeriod,
)

import config


# Timeframe-Name → Protobuf-Enum + ungefähre Bars/Tag + sichere Chunk-Größe (Tage)
TIMEFRAMES = {
    # name: (proto_period, bars_per_day, chunk_days)
    "M1":  (ProtoOATrendbarPeriod.M1,   1440,   7),
    "M5":  (ProtoOATrendbarPeriod.M5,    288,  30),
    "M15": (ProtoOATrendbarPeriod.M15,    96,  90),
    "M30": (ProtoOATrendbarPeriod.M30,    48, 180),
    "H1":  (ProtoOATrendbarPeriod.H1,     24, 365),
    "H4":  (ProtoOATrendbarPeriod.H4,      6, 730),
    "D1":  (ProtoOATrendbarPeriod.D1,      1, 3650),
}


def load_tokens():
    try:
        with open("tokens.json", "r", encoding="utf-8") as f:
            return json.load(f)
    except FileNotFoundError:
        print("[FEHLER] tokens.json nicht gefunden. Erst '01_setup_oauth.py' ausführen.")
        sys.exit(1)


def load_symbols():
    try:
        with open("symbols.csv", "r", encoding="utf-8") as f:
            return list(csv.DictReader(f))
    except FileNotFoundError:
        print("[FEHLER] symbols.csv nicht gefunden. Erst '02_list_symbols.py' ausführen.")
        sys.exit(1)


def find_symbol(symbols, name):
    matches = [s for s in symbols if s["symbolName"] == name]
    if not matches:
        # Case-insensitive Fallback + Vorschläge
        ci = [s for s in symbols if s["symbolName"].upper() == name.upper()]
        if ci:
            return ci[0]
        print(f"[FEHLER] Symbol '{name}' nicht gefunden.")
        partial = [s["symbolName"] for s in symbols
                   if name.upper() in s["symbolName"].upper()][:10]
        if partial:
            print(f"   Vorschläge: {', '.join(partial)}")
        sys.exit(1)
    return matches[0]


def parse_date(s):
    """YYYY-MM-DD → datetime in UTC."""
    return datetime.strptime(s, "%Y-%m-%d").replace(tzinfo=timezone.utc)


def to_ms(dt):
    return int(dt.timestamp() * 1000)


def decode_bar(bar, digits):
    """Konvertiert delta-kodierte Trendbar zu absoluten OHLC-Preisen."""
    factor = 10 ** digits
    low_p   = bar.low / factor
    open_p  = (bar.low + bar.deltaOpen)  / factor
    high_p  = (bar.low + bar.deltaHigh)  / factor
    close_p = (bar.low + bar.deltaClose) / factor
    ts = datetime.fromtimestamp(bar.utcTimestampInMinutes * 60, tz=timezone.utc)
    return {
        "time":   ts.strftime("%Y-%m-%d %H:%M:%S"),
        "open":   open_p,
        "high":   high_p,
        "low":    low_p,
        "close":  close_p,
        "volume": bar.volume,
    }


def main():
    ap = argparse.ArgumentParser(description="cTrader Open API – Bars Downloader")
    ap.add_argument("--symbol", required=True, help="z.B. XBRUSD, GER40, EURUSD")
    ap.add_argument("--tf",     required=True, choices=list(TIMEFRAMES.keys()),
                    help="Timeframe: M1, M5, M15, M30, H1, H4, D1")
    ap.add_argument("--from",   dest="from_date", required=True, help="YYYY-MM-DD")
    ap.add_argument("--to",     dest="to_date",   required=True, help="YYYY-MM-DD")
    ap.add_argument("--out",    default=None, help="Output-CSV (Default: <symbol>_<tf>.csv)")
    args = ap.parse_args()

    tokens  = load_tokens()
    symbols = load_symbols()
    sym     = find_symbol(symbols, args.symbol)
    symbol_id = int(sym["symbolId"])
    digits    = int(sym["digits"])

    period, bars_per_day, chunk_days = TIMEFRAMES[args.tf]
    from_dt = parse_date(args.from_date)
    to_dt   = parse_date(args.to_date)
    _DEFAULT_OUT_DIR.mkdir(parents=True, exist_ok=True)
    out_path = args.out or str(_DEFAULT_OUT_DIR / f"{args.symbol}_{args.tf}.csv")

    total_days = (to_dt - from_dt).days
    est_bars   = total_days * bars_per_day
    n_chunks   = max(1, (total_days + chunk_days - 1) // chunk_days)

    print(f"Symbol:    {args.symbol} (ID {symbol_id}, digits={digits})")
    print(f"Timeframe: {args.tf}")
    print(f"Range:     {args.from_date} → {args.to_date}  ({total_days} Tage)")
    print(f"Erwartet:  ~{est_bars:,} Bars in {n_chunks} Chunks à max {chunk_days} Tage")
    print(f"Output:    {out_path}")
    print()

    host = EndPoints.PROTOBUF_DEMO_HOST if config.USE_DEMO else EndPoints.PROTOBUF_LIVE_HOST
    client = Client(host, EndPoints.PROTOBUF_PORT, TcpProtocol)

    state = {
        "current_from": from_dt,
        "chunk_idx":    0,
        "all_bars":     [],
        "start_time":   None,
    }

    def on_error(failure):
        print(f"\n[FEHLER] {failure}")
        if reactor.running:
            reactor.stop()

    def on_connected(_client):
        print(f"[OK] Verbunden mit {host}")
        req = ProtoOAApplicationAuthReq()
        req.clientId     = config.CLIENT_ID
        req.clientSecret = config.CLIENT_SECRET
        d = client.send(req)
        d.addCallbacks(on_app_auth, on_error)

    def on_disconnected(_client, reason):
        pass

    def on_app_auth(response):
        req = ProtoOAAccountAuthReq()
        req.ctidTraderAccountId = int(tokens["ctidTraderAccountId"])
        req.accessToken         = tokens["accessToken"]
        d = client.send(req)
        d.addCallbacks(on_account_auth, on_error)

    def on_account_auth(response):
        state["start_time"] = time.time()
        request_next_chunk()

    def request_next_chunk():
        cur_from = state["current_from"]
        if cur_from >= to_dt:
            finish()
            return

        cur_to = min(cur_from + _timedelta_days(chunk_days), to_dt)
        state["chunk_idx"] += 1

        req = ProtoOAGetTrendbarsReq()
        req.ctidTraderAccountId = int(tokens["ctidTraderAccountId"])
        req.symbolId            = symbol_id
        req.period              = period
        req.fromTimestamp       = to_ms(cur_from)
        req.toTimestamp         = to_ms(cur_to)

        print(f"  Chunk {state['chunk_idx']:>3}/{n_chunks}: "
              f"{cur_from.date()} → {cur_to.date()} ... ", end="", flush=True)

        state["pending_to"] = cur_to
        d = client.send(req)
        d.addCallbacks(on_chunk, on_error)

    def on_chunk(response):
        msg = Protobuf.extract(response)
        if not isinstance(msg, ProtoOAGetTrendbarsRes):
            print(f"\n[FEHLER] Unerwartete Antwort: {type(msg).__name__}")
            reactor.stop()
            return

        bars = msg.trendbar
        print(f"{len(bars):>6} Bars")
        for b in bars:
            state["all_bars"].append(decode_bar(b, digits))

        # Nächster Chunk startet am Ende des bisherigen
        state["current_from"] = state["pending_to"]

        # 200ms Pause vor nächstem Request (rate-limit-freundlich)
        reactor.callLater(0.2, request_next_chunk)

    def finish():
        elapsed = time.time() - state["start_time"]
        bars = state["all_bars"]

        # De-Duplizieren (bei Chunk-Grenzen können Bars doppelt auftauchen)
        seen = set()
        unique = []
        for b in bars:
            if b["time"] not in seen:
                seen.add(b["time"])
                unique.append(b)
        unique.sort(key=lambda x: x["time"])

        with open(out_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(
                f, fieldnames=["time", "open", "high", "low", "close", "volume"]
            )
            writer.writeheader()
            for row in unique:
                writer.writerow(row)

        print()
        print(f"[OK] {len(unique):,} unique Bars geschrieben nach {out_path}")
        print(f"     Laufzeit: {elapsed:.1f}s")
        if unique:
            print(f"     Range:    {unique[0]['time']}  →  {unique[-1]['time']}")
        reactor.stop()

    client.setConnectedCallback(on_connected)
    client.setDisconnectedCallback(on_disconnected)
    client.startService()
    reactor.run()


def _timedelta_days(n):
    from datetime import timedelta
    return timedelta(days=n)


if __name__ == "__main__":
    main()
