from bs4 import BeautifulSoup
from selenium import webdriver
import pandas as pd

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import scraper, addColEnd
from fbref_links import fbreflinkgetter, fbrefstadiumsgetter, fbrefteamsgetter, baseGetter


stadiums=fbrefstadiumsgetter()
for i in range(len(stadiums)):
    stadiums[i]=stadiums[i].replace(" ", "-")
print(stadiums)
teams=fbrefteamsgetter()

travelDist=[]

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


df=pd.read_csv("Data_complete.csv")
df["TravelDistance"]=travelDist
df.to_csv("Data_complete.csv", index=False)