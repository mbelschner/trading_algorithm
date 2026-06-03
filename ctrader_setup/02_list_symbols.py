"""
02_list_symbols.py — Lädt alle bei deinem Broker verfügbaren Symbole
und exportiert sie nach symbols.csv.

Output-Spalten:
   symbolId        — Integer-ID für API-Aufrufe (wird vom Downloader gebraucht)
   symbolName      — Display-Name (z.B. "XBRUSD", "GER40", "EURUSD")
   description     — Klartext-Beschreibung
   digits          — Nachkommastellen für Preise (wichtig zur Dekodierung)
   pipPosition     — Pip-Position
   enabled         — Ob handelbar
   baseAsset       — Base-Asset-Name
   quoteAsset      — Quote-Asset-Name

Run:  python 02_list_symbols.py
"""

import csv
import json
import sys

from twisted.internet import reactor

from ctrader_open_api import Client, EndPoints, Protobuf, TcpProtocol
from ctrader_open_api.messages.OpenApiCommonMessages_pb2 import (
    ProtoHeartbeatEvent,
)
from ctrader_open_api.messages.OpenApiMessages_pb2 import (
    ProtoOAApplicationAuthReq,
    ProtoOAApplicationAuthRes,
    ProtoOAAccountAuthReq,
    ProtoOAAccountAuthRes,
    ProtoOASymbolsListReq,
    ProtoOASymbolsListRes,
    ProtoOAErrorRes,
)

import config


def load_tokens():
    try:
        with open("tokens.json", "r", encoding="utf-8") as f:
            return json.load(f)
    except FileNotFoundError:
        print("[FEHLER] tokens.json nicht gefunden. Erst '01_setup_oauth.py' ausführen.")
        sys.exit(1)


def main():
    tokens = load_tokens()

    host = EndPoints.PROTOBUF_DEMO_HOST if config.USE_DEMO else EndPoints.PROTOBUF_LIVE_HOST
    client = Client(host, EndPoints.PROTOBUF_PORT, TcpProtocol)

    state = {"step": "init"}

    def on_error(failure):
        print(f"[FEHLER] {failure}")
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
        print(f"[INFO] Verbindung getrennt: {reason}")

    def on_app_auth(response):
        print("[OK] Application authentifiziert.")
        req = ProtoOAAccountAuthReq()
        req.ctidTraderAccountId = int(tokens["ctidTraderAccountId"])
        req.accessToken         = tokens["accessToken"]
        d = client.send(req)
        d.addCallbacks(on_account_auth, on_error)

    def on_account_auth(response):
        print(f"[OK] Account authentifiziert (#{tokens['accountNumber']}).")
        req = ProtoOASymbolsListReq()
        req.ctidTraderAccountId = int(tokens["ctidTraderAccountId"])
        req.includeArchivedSymbols = False
        d = client.send(req)
        d.addCallbacks(on_symbols, on_error)

    def on_symbols(response):
        msg = Protobuf.extract(response)
        if not isinstance(msg, ProtoOASymbolsListRes):
            print(f"[FEHLER] Unerwartete Antwort: {type(msg).__name__}")
            reactor.stop()
            return

        symbols = msg.symbol
        print(f"[OK] {len(symbols)} Symbole erhalten.")

        # Sortieren nach Name für bessere Übersicht
        sorted_syms = sorted(symbols, key=lambda s: s.symbolName)

        with open("symbols.csv", "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow([
                "symbolId", "symbolName", "description", "digits",
                "pipPosition", "enabled", "baseAsset", "quoteAsset",
                "symbolCategory",
            ])
            for s in sorted_syms:
                writer.writerow([
                    s.symbolId,
                    s.symbolName,
                    s.description if s.HasField("description") else "",
                    s.digits if s.HasField("digits") else "",
                    s.pipPosition if s.HasField("pipPosition") else "",
                    "yes" if s.enabled else "no",
                    s.baseAssetId if s.HasField("baseAssetId") else "",
                    s.quoteAssetId if s.HasField("quoteAssetId") else "",
                    s.symbolCategoryId if s.HasField("symbolCategoryId") else "",
                ])

        print(f"[OK] Liste geschrieben nach: symbols.csv  ({len(sorted_syms)} Zeilen)")
        print()
        print("Beispiel-Auszug (erste 30):")
        print(f"   {'ID':>8}  {'NAME':<15}  {'DIGITS':<7}  DESCRIPTION")
        for s in sorted_syms[:30]:
            desc = s.description if s.HasField("description") else ""
            print(f"   {s.symbolId:>8}  {s.symbolName:<15}  {s.digits:<7}  {desc[:50]}")
        if len(sorted_syms) > 30:
            print(f"   ... ({len(sorted_syms) - 30} weitere in symbols.csv)")

        reactor.stop()

    client.setConnectedCallback(on_connected)
    client.setDisconnectedCallback(on_disconnected)
    client.startService()
    reactor.run()


if __name__ == "__main__":
    main()
