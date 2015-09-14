import sys
import json
import urllib

# Bitcoin Price Index API powered by CoinDesk. 
# More information avaiable in: http://www.coindesk.com/api/

# Shows the list of currencies which the price of Bitcoin can be seen.
def help():
	data = json.loads(urllib.urlopen('http://api.coindesk.com/v1/bpi/supported-currencies.json').read())
	for content in data:
		print "%(currency)s - %(country)s" % content
	print "\nType:"
	print "\t" + 'python bitcoin.py [currency1] [currency2] [currency3] ...' 
	print "to view the price of Bitcoin in the respective currency."

# Shows the price of the Bitcoin in the currency referred in the "currency" parameter
def get_country(currency):
	try:
		data = json.loads(urllib.urlopen('https://api.coindesk.com/v1/bpi/currentprice/' 
			+ currency + '.json').read())
	except ValueError as e:
		print "\t" + "Currency " + currency + " not found." 
		print "\t" + "Type 'python bitcoin.py help' to view the list of currencies."
		sys.exit(1)
	price = data['bpi'][currency]
	print "[" + price['code'] + "]" + " $" + price['rate'] + " - " + price['description']

# Shows the price of the Bitcoin in the US Dollar, GB Pound and Euro currencies.
def default():
	priceUS = data['bpi']['USD']
	priceGBP = data['bpi']['GBP']
	priceEUR = data['bpi']['EUR']

	print "[" + priceUS['code'] + "] " + priceUS['symbol'] + " " + priceUS['rate'] + " - " + priceUS['description']
	print "[" + priceGBP['code'] + "] " + priceGBP['symbol'] + " " + priceGBP['rate'] + " - " + priceGBP['description']
	print "[" + priceEUR['code'] + "] " + priceEUR['symbol'] + " " + priceEUR['rate'] + " - " + priceEUR['description']

# main function
country = sys.argv
if len(country) > 1 and country[1].upper() == "HELP":
	help()
	sys.exit(0)
url = 'https://api.coindesk.com/v1/bpi/currentprice.json'
data = json.loads(urllib.urlopen(url).read())
print "Bitcoin price in " + data['time']['updated']
if len(country) == 1:
	default()
else:
	for i in range(1, len(country)):
		get_country(country[i].upper())
