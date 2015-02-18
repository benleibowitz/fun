'''
Created on Feb 7, 2015
    Get points from Reddit by parsing HTML
@author: ben leibowitz
'''
from bs4 import BeautifulSoup
from urllib.request import Request, urlopen

info = {}
url = 'http://www.reddit.com/user/YOUR_USER_NAME/'

hdr = {'User-Agent': 'Mozilla/5.0'}
req = Request(url, headers=hdr)
response = urlopen(req).read().decode('utf-8')

soup = BeautifulSoup(response)

commentlines = soup.find_all("p", {"class":"tagline"})

for tagline in commentlines:
    points = tagline.find("span", {"class":"score likes"})
    if points is not None:
        points = int(''.join(points.contents).split(' ')[0])
        info['comment points'] = (points if not 'comment points' in info.keys() else info['comment points'] + points)

print(info)
