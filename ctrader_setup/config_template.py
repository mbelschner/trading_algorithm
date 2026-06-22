"""
Config-Template – kopiere diese Datei nach 'config.py' und trage
deine Werte aus https://connect.spotware.com/apps ein.

WICHTIG: config.py NIEMALS in Git/Cloud hochladen!
Füge 'config.py' und 'tokens.json' zu deiner .gitignore hinzu.
"""

# Aus deiner Spotware-Application:
CLIENT_ID     = "29871_vnE1zQD8MskYFZ72dX6RGK2BHrNFDuKtKyd9jgqbv5HJ2QMUsM"
CLIENT_SECRET = "YBIL5BojmTANfa0tatTaMbxwypzEOlqJR5PO7t1tsHEbkMy26u"

# Muss exakt mit der Redirect-URI übereinstimmen, die du bei
# der App-Registrierung angegeben hast:
REDIRECT_URI = "http://localhost:8080/"

# Demo (True) oder Live (False) verwenden:
USE_DEMO = True
