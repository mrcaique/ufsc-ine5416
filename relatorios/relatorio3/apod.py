from json import loads
from urllib.request import urlopen

# Astronomy Picture of the Day API powered by NASA's Data Portal
# More information in: https://data.nasa.gov/developer

# main function
# prints on the screen the APOD title, the title of the image,
# the url of the image and, by end, a few explanation about the image
# written by an astronomer.
url = "https://api.nasa.gov/planetary/apod?api_key=DEMO_KEY"
string_request = str(urlopen(url).read(), encoding='ascii')
data = loads(string_request)

print("ASTRONOMY PICTURE OF THE DAY")
print(data['title'])
print(data['url'])
print(data['explanation'])
