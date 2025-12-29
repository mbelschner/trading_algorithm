import http.client
import json
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import itertools


conn = http.client.HTTPSConnection("demo-api-capital.backend-capital.com")
payload = json.dumps({
  "identifier": "maxibelschner@gmx.at",
  "password": "API_Demo222555"
})
headers = {
  'X-CAP-API-KEY': 'TJPB1jNrf1mbl3hU',
  'Content-Type': 'application/json'
}
conn.request("POST", "/api/v1/session", payload, headers)
res = conn.getresponse()
data = res.read()
print(data.decode("utf-8"))

#get the tokens from the response headers
CST = res.headers.get("CST")
XST = res.headers.get("X-SECURITY-TOKEN")
# Use the tokens for subsequent requests

# Now, let's use the tokens to get market data
headers = {
  'CST': CST,
  'X-SECURITY-TOKEN': XST,
  'Content-Type': 'application/json'
}

#get historical data

epic = "GOLD"
resolution = "HOUR_4"

#try out the for loop
today = datetime.now().replace(hour=0, minute=0, second=0, microsecond=0)
end_date = today - timedelta(days=2)
start_date = end_date - timedelta(days=1500)
step = timedelta(days=5)

# Ergebnisliste
all_data = []

# Loop in 10-Tage-Schritten
current_start = start_date
while current_start < end_date:
    current_end = min(current_start + step, end_date)

    from_str = current_start.strftime("%Y-%m-%dT%H:%M:%S")
    to_str = current_end.strftime("%Y-%m-%dT%H:%M:%S")

    print(f"Hole Daten von {from_str} bis {to_str}")

    request_url = f"/api/v1/prices/{epic}?resolution={resolution}&max=1000&from={from_str}&to={to_str}"
    conn.request("GET", request_url, payload, headers)
    res = conn.getresponse()
    data = res.read()

    try:
        decoded_prices = json.loads(data.decode("utf-8"))
        if 'prices' in decoded_prices:
            df = pd.json_normalize(decoded_prices['prices'])
            if not df.empty:
                df['time'] = pd.to_datetime(df['snapshotTime'], format="%Y-%m-%dT%H:%M:%S")
                df = df[['time', 
                        'openPrice.bid', 
                        'closePrice.bid',
                        'highPrice.bid', 
                        'lowPrice.bid',
                         'lastTradedVolume']]
                all_data.append(df)
            else:
                print("⚠️ Leere Antwort für diesen Block.")
        else:
            print("⚠️ 'prices' nicht im Response enthalten.")
    except Exception as e:
        print(f"❌ Fehler beim Verarbeiten der Antwort: {e}")

    current_start = current_end

# Zusammenführen
if all_data:
    final_df = pd.concat(all_data).sort_values(by='time').reset_index(drop=True)
    print("✅ Daten erfolgreich gesammelt:")
    print(final_df.head())
else:
    print("❌ Es wurden keine Daten empfangen.")

###Additional Data Wrangling
#rename the columns
final_df.rename(columns={
    'openPrice.bid': 'open',
    'closePrice.bid': 'close',
    'highPrice.bid': 'high',
    'lowPrice.bid': 'low',
    'lastTradedVolume': 'volume'
}, inplace=True)

### Calculate our indicators for the strategy
# Achte darauf, dass der 'time'-Wert ein datetime-Objekt ist:
final_df['time'] = pd.to_datetime(final_df['time'])

# Ensure the 'time' column is sorted
final_df.sort_values(by='time', inplace=True)

#Now we export the final DataFrame to a CSV file, we name it with epic name, last date and resolution
#we want to save it in the api-data folder 
final_df.to_csv(f"C:/Users/maxib/OneDrive/Dokumente/trading_algorithm/price_data/{epic}_{resolution}.csv", index=False)

