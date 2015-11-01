#!/usr/bin/env python3
from sys import argv, exit
from urllib.request import urlopen
from json import loads
from urllib.error import URLError

# Bitcoin Price Index API powered by CoinDesk. 
# More information avaiable in: http://www.coindesk.com/api/

# Shows the list of currencies which the price of Bitcoin can be seen.
def help():
    url = 'http://api.coindesk.com/v1/bpi/supported-currencies.json'
    string_request = str(urlopen(url).read(), encoding='utf-8')
    data = loads(string_request)
    for content in data:
        print("%(currency)s - %(country)s" % content)
    print("\nType:")
    print("\t" + 'python3 bitcoin.py [currency1] [currency2] [currency3] ...')
    print("to view the price of Bitcoin in the respective currency.")

# Shows the price of the Bitcoin in the currency referred in the "currency" parameter.
def get_country(currency):
    url = 'https://api.coindesk.com/v1/bpi/currentprice/' + currency + '.json'
    
    try:
        string_request = str(urlopen(url).read(), encoding='utf-8')
    except URLError as e:
        print("\t" + "Currency " + currency + " not found.")
        print("\t" + "Type 'python3 bitcoin.py help' to view the list of currencies.")
        return
    
    data = loads(string_request)
    price = data['bpi'][currency]
    print("[" + price['code'] + "]" + " $" + price['rate'] + " - " + price['description'])

# Shows the price of the Bitcoin in the US Dollar, GB Pound and Euro currencies.
def default():
    priceUS = data['bpi']['USD']
    priceGBP = data['bpi']['GBP']
    priceEUR = data['bpi']['EUR']

    print("[" + priceUS['code'] + "] " + priceUS['symbol'] + " " 
        + priceUS['rate'] + " - " + priceUS['description'])
    
    print("[" + priceGBP['code'] + "] " + priceGBP['symbol'] + " "
        + priceGBP['rate'] + " - " + priceGBP['description'])
    
    print("[" + priceEUR['code'] + "] " + priceEUR['symbol'] + " " 
        + priceEUR['rate'] + " - " + priceEUR['description'])

# main function
country = argv
if len(country) > 1 and country[1].upper() == "HELP":
    help()
    exit(0)
url = 'https://api.coindesk.com/v1/bpi/currentprice.json'
string_request = str(urlopen(url).read(), encoding='utf-8')
data = loads(string_request)
print("Bitcoin price in " + data['time']['updated'])
if len(country) == 1:
    default()
else:
    for country in country[1:]:
        get_country(country.upper())
