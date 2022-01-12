from bs4.element import ResultSet
import requests
import csv
import codecs
import time
from bs4 import BeautifulSoup
from selenium import webdriver

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import addColEnd, scraper
from fbref_links import baseGetter, fbreflinkgetter2, fbrefstadiumsgetter, fbrefteamsgetter


stadiums=fbrefstadiumsgetter()
for i in range(len(stadiums)):
    stadiums[i]=stadiums[i].replace(" ", "-")
teams=fbrefteamsgetter()

travelDist=[]
travelDist.append("TravelDistance")

for i in range(len(stadiums)):

    base=baseGetter(teams[i])
    url="https://www.luftlinie.org/" + str(base) + "/" + str(stadiums[i])

    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url)
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()

    box=soup.find("div", class_="distance-step-box")
    distance=box.find("span", class_="value km").text.strip()
    distance=distance.replace(".", "")
    distance=distance.replace(",", ".")
    travelDist.append(distance)

    print("Match " + str((i+1)) + " von 102.")
    print(str(base) + " nach " + str(stadiums[i]) + ", Distanz = " + str(distance) + " km")

addColEnd(travelDist)


