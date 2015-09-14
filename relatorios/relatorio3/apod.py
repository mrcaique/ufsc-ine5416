import json
import urllib

# Astronomy Picture of the Day API powered by Nasa's Data Portal
# More information in: https://data.nasa.gov/developer

# main function
# prints on the screen the APOD title, the title of the image,
# the url of the image and, by end, a few explanation about the image
# written by an astronomer.
url = "https://api.nasa.gov/planetary/apod?concept_tags=True&api_key=DEMO_KEY"
data = json.loads(urllib.urlopen(url).read())

print "ASTRONOMY PICTURE OF THE DAY"
print data['title']
print data['url']
print data['explanation']
