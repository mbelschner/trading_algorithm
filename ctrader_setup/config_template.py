"""
Config-Template – kopiere diese Datei nach 'config.py' und trage
deine Werte aus https://connect.spotware.com/apps ein.

WICHTIG: config.py NIEMALS in Git/Cloud hochladen!
Füge 'config.py' und 'tokens.json' zu deiner .gitignore hinzu.
"""

# Aus deiner Spotware-Application:
CLIENT_ID     = "YOUR_CLIENT_ID_HERE"
CLIENT_SECRET = "YOUR_CLIENT_SECRET_HERE"

# Muss exakt mit der Redirect-URI übereinstimmen, die du bei
# der App-Registrierung angegeben hast:
REDIRECT_URI = "http://localhost:8080/"

# Demo (True) oder Live (False) verwenden:
USE_DEMO = True
