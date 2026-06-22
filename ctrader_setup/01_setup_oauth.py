"""
01_setup_oauth.py — Einmaliger OAuth2-Flow für Spotware Open API.

Was passiert:
1. Lokaler HTTP-Server startet auf :8080 (für den OAuth-Redirect)
2. Browser öffnet sich mit Spotware-Login
3. Du loggst dich mit deinem PEPPERSTONE/IC-MARKETS-Konto ein
4. Auth-Code wird automatisch eingefangen
5. Code wird gegen Access- + Refresh-Token getauscht
6. Liste der verfügbaren Trading-Accounts wird abgerufen
7. Du wählst das gewünschte Konto (Demo/Live)
8. Alles wird in tokens.json gespeichert

Run:  python 01_setup_oauth.py
"""

import json
import sys
import urllib.parse
import webbrowser
from http.server import BaseHTTPRequestHandler, HTTPServer

import requests

import config

AUTH_URL  = "https://connect.spotware.com/apps/auth"
TOKEN_URL = "https://connect.spotware.com/apps/token"
ACCOUNTS_URL = "https://api.spotware.com/connect/tradingaccounts"

# Wird gesetzt, sobald der Redirect-Handler den Code empfangen hat
captured_code = {"value": None, "error": None}


class OAuthCallbackHandler(BaseHTTPRequestHandler):
    """Fängt den /?code=... Redirect von Spotware ab."""

    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        params = urllib.parse.parse_qs(parsed.query)

        if "code" in params:
            captured_code["value"] = params["code"][0]
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(
                "<html><body style='font-family: sans-serif; padding: 40px;'>"
                "<h2>✓ Authentifizierung erfolgreich</h2>"
                "<p>Du kannst dieses Fenster schließen und zum Terminal zurückkehren.</p>"
                "</body></html>".encode("utf-8")
            )
        elif "error" in params:
            captured_code["error"] = params.get("error_description", ["Unknown error"])[0]
            self.send_response(400)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(
                f"<html><body><h2>Fehler:</h2><p>{captured_code['error']}</p></body></html>".encode("utf-8")
            )
        else:
            self.send_response(404)
            self.end_headers()

    def log_message(self, *args, **kwargs):
        pass  # unterdrückt Default-Logging


def run_oauth_flow():
    """Startet HTTP-Server, öffnet Browser, wartet auf Auth-Code."""

    # 1) Auth-URL bauen
    params = {
        "client_id": config.CLIENT_ID,
        "redirect_uri": config.REDIRECT_URI,
        "scope": "trading",
        "response_type": "code",
    }
    auth_url = f"{AUTH_URL}?{urllib.parse.urlencode(params)}"

    print("=" * 70)
    print("OAuth-Setup für cTrader Open API")
    print("=" * 70)
    print()
    print("Öffne folgende URL im Browser, falls sie nicht automatisch erscheint:")
    print()
    print(auth_url)
    print()
    print("Logge dich mit deinem PEPPERSTONE/IC-MARKETS-Konto ein,")
    print("nicht mit dem Spotware-Developer-Account.")
    print()
    print("Lokaler Server lauscht auf http://localhost:8080 ...")
    print()

    # 2) Browser öffnen
    webbrowser.open(auth_url)

    # 3) HTTP-Server starten und auf einen Request warten
    redirect_parsed = urllib.parse.urlparse(config.REDIRECT_URI)
    port = redirect_parsed.port or 8080
    server = HTTPServer(("localhost", port), OAuthCallbackHandler)

    while captured_code["value"] is None and captured_code["error"] is None:
        server.handle_request()

    server.server_close()

    if captured_code["error"]:
        print(f"\n[FEHLER] OAuth fehlgeschlagen: {captured_code['error']}")
        sys.exit(1)

    print("[OK] Auth-Code empfangen.")
    return captured_code["value"]


def exchange_code_for_tokens(auth_code):
    """Tauscht den Auth-Code gegen Access + Refresh Token."""
    print("[...] Tausche Auth-Code gegen Tokens...")
    resp = requests.post(
        TOKEN_URL,
        data={
            "grant_type": "authorization_code",
            "code": auth_code,
            "redirect_uri": config.REDIRECT_URI,
            "client_id": config.CLIENT_ID,
            "client_secret": config.CLIENT_SECRET,
        },
        timeout=30,
    )
    resp.raise_for_status()
    tokens = resp.json()
    # Die API liefert "errorCode": None auch im Erfolgsfall mit.
    # Nur abbrechen, wenn errorCode tatsächlich gesetzt ist:
    if tokens.get("errorCode"):
        print(f"[FEHLER] Token-Exchange: {tokens}")
        sys.exit(1)
    print("[OK] Access- und Refresh-Token erhalten.")
    return tokens


def fetch_trading_accounts(access_token):
    """Lädt die Liste der mit diesem Access-Token verknüpften Trading-Accounts."""
    print("[...] Lade Liste der Trading-Accounts...")
    resp = requests.get(
        ACCOUNTS_URL,
        params={"access_token": access_token},
        timeout=30,
    )
    resp.raise_for_status()
    data = resp.json()

    if "data" not in data:
        print(f"[FEHLER] Account-Liste: {data}")
        sys.exit(1)

    accounts = data["data"]
    if not accounts:
        print("[FEHLER] Keine Trading-Accounts gefunden. Hast du dich mit dem richtigen Account eingeloggt?")
        sys.exit(1)

    # Debug: zeige die tatsächlichen Feldnamen des ersten Accounts
    print(f"[DEBUG] Verfügbare Felder pro Account: {list(accounts[0].keys())}")

    return accounts


def _get_account_id(acc):
    """Findet die ctidTraderAccountId unter den möglichen Feldnamen."""
    for key in ("ctidTraderAccountId", "accountId", "traderLogin", "traderId"):
        if key in acc and acc[key] is not None:
            return acc[key]
    # Fallback: erste Integer-artige ID
    return acc.get("accountNumber")


def _get_account_number(acc):
    for key in ("accountNumber", "traderLogin", "accountId"):
        if key in acc and acc[key] is not None:
            return acc[key]
    return "?"


def select_account(accounts):
    """Lässt den User ein Konto auswählen."""
    print()
    print("Verfügbare Trading-Accounts:")
    print("-" * 70)
    for i, acc in enumerate(accounts):
        live_str = "LIVE" if acc.get("live", False) else "DEMO"
        print(f"  [{i}] Account #{_get_account_number(acc)}  ({live_str})  "
              f"Broker: {acc.get('brokerName') or acc.get('brokerTitle', 'n/a')}  "
              f"Währung: {acc.get('depositCurrency') or acc.get('currency', 'n/a')}  "
              f"ID: {_get_account_id(acc)}")
    print()

    # Default-Wahl: erstes Demo-Konto wenn USE_DEMO, sonst erstes Live
    default_idx = 0
    for i, acc in enumerate(accounts):
        if acc.get("live", False) != config.USE_DEMO:
            default_idx = i
            break

    while True:
        choice = input(f"Welches Konto verwenden? [Default: {default_idx}] ").strip()
        if choice == "":
            return accounts[default_idx]
        try:
            idx = int(choice)
            if 0 <= idx < len(accounts):
                return accounts[idx]
        except ValueError:
            pass
        print("Ungültige Eingabe, nochmal.")


def main():
    auth_code = run_oauth_flow()
    tokens = exchange_code_for_tokens(auth_code)
    accounts = fetch_trading_accounts(tokens["accessToken"])
    selected = select_account(accounts)

    out = {
        "accessToken":  tokens["accessToken"],
        "refreshToken": tokens["refreshToken"],
        "tokenType":    tokens.get("tokenType", "bearer"),
        "expiresIn":    tokens.get("expiresIn"),
        "ctidTraderAccountId": _get_account_id(selected),
        "accountNumber":       _get_account_number(selected),
        "isLive":              selected.get("live", False),
        "brokerName":          selected.get("brokerName") or selected.get("brokerTitle"),
        "depositCurrency":     selected.get("depositCurrency") or selected.get("currency"),
    }

    with open("tokens.json", "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)

    print()
    print("=" * 70)
    print(f"[OK] Tokens gespeichert in tokens.json")
    print(f"     Account #{_get_account_number(selected)} ({'LIVE' if selected.get('live') else 'DEMO'})")
    print(f"     ctidTraderAccountId: {_get_account_id(selected)}")
    print("=" * 70)
    print()
    print("Nächster Schritt:  python 02_list_symbols.py")


if __name__ == "__main__":
    main()