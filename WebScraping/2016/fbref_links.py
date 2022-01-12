import requests
import csv
import codecs
import time
from bs4 import BeautifulSoup
from selenium import webdriver

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import scraper


def fbreflinkgetter2():
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get("https://fbref.com/en/comps/676/10277/schedule/2016-UEFA-Euro-Scores-and-Fixtures")
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    links=[]
    table=soup.find(id="div_sched_all")
    body=table.find("tbody")
    rows=body.find_all("tr")
    rows.pop(12)
    rows.pop(24)
    rows.pop(28)
    rows.pop(36)

    for el in rows:
        helper1=el.find("td", class_="center")
        helper2=helper1.find("a")
        link=helper2["href"]
        links.append("https://fbref.com" + link)

    return links

def fbrefmatchgetter():
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get("https://fbref.com/en/comps/676/10277/schedule/2016-UEFA-Euro-Scores-and-Fixtures")
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    matches=[]
    table=soup.find(id="div_sched_all")
    body=table.find("tbody")
    rows=body.find_all("tr")
    rows.pop(12)
    rows.pop(24)
    rows.pop(28)
    rows.pop(36)

    for el in rows:
        helper1=el.find_all("td", class_="right")
        homeTeam=helper1[2].find("a").text.strip()
        helper2=el.find_all("td", class_="left")
        awayTeam=helper2[2].find("a").text.strip()
        matches.append(str(homeTeam) + " vs " + str(awayTeam))
        matches.append(str(awayTeam) + " vs " + str(homeTeam))

    return matches

def fbrefteamsgetter():
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get("https://fbref.com/en/comps/676/10277/schedule/2016-UEFA-Euro-Scores-and-Fixtures")
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    teams=[]
    table=soup.find(id="div_sched_all")
    body=table.find("tbody")
    rows=body.find_all("tr")
    rows.pop(12)
    rows.pop(24)
    rows.pop(28)
    rows.pop(36)

    for el in rows:
        helper1=el.find_all("td", class_="right")
        homeTeam=helper1[2].find("a").text.strip()
        helper2=el.find_all("td", class_="left")
        awayTeam=helper2[2].find("a").text.strip()
        teams.append(str(homeTeam))
        teams.append(str(awayTeam))

    return teams


def fbrefstadiumsgetter():
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get("https://fbref.com/en/comps/676/10277/schedule/2016-UEFA-Euro-Scores-and-Fixtures")
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    stadiums=[]
    table=soup.find(id="div_sched_all")
    body=table.find("tbody")
    rows=body.find_all("tr")
    rows.pop(12)
    rows.pop(24)
    rows.pop(28)
    rows.pop(36)

    for el in rows:
        helper2=el.find_all("td", class_="left")
        stadium=helper2[3].text.strip()
        stadium=stadium.replace(" (Neutral Site)", "")
        stadium=cleaner(stadium)
        stadiums.append(str(stadium))
        stadiums.append(str(stadium))
    return stadiums


def cleaner(string):
    result=string.replace("á", "a")
    result=result.replace("ţ", "t")
    result=result.replace("ă", "a")
    result=result.replace("ı", "u")
    result=result.replace("é", "e")
    result=result.replace("Hampden Park", "Hampden-Park,Glasgow")
    result=result.replace("Johan Cruyff ArenA", "Johan-Cruyff-ArenA,Amsterdam")

    return result


def baseGetter(team):
    if team == "Russia":
        reult="Croissy-sur-Seine,Frankreich"
    if team == "Republic of Ireland":
        reult="Versailles,Frankreich"
    if team == "France":
        reult="Clarefontaine-en-Yvelines,Frankreich"
    if team == "Portugal":
        reult="Marcoussis,Frankreich"
    if team == "England":
        reult="Chantilly,Frankreich"
    if team == "Romania":
        reult="Orry-la-Ville,Frankreich"
    if team == "Croatia":
        reult="Deauville,Frankreich"
    if team == "Wales":
        reult="Dinard,Frankreich"
    if team == "Albania":
        reult="Perros-Guirec,Frankreich"
    if team == "Czech Republic":
        reult="Tours,Frankreich"
    if team == "Sweden":
        reult="Pornichet,Frankreich"
    if team == "Spain":
        reult="Saint-Martin-de-Re,Frankreich"
    if team == "Belgium":
        reult="Bordeaux,Frankreich"
    if team == "Switzerland":
        reult="Montpellier,Frankreich"
    if team == "Italy":
        reult="Montpellier,Frankreich"
    if team == "Ukraine":
        reult="Aix-en-Provence,Frankreich"
    if team == "Austria":
        reult="Mallemort,Frankreich"
    if team == "Turkey":
        reult="Saint-Cyr-sur-Mer,Frankreich"
    if team == "Hungary":
        reult="Tourrettes,Frankreich"
    if team == "Iceland":
        reult="Annecy,Frankreich"
    if team == "Slovakia":
        reult="Vichy,Frankreich"
    if team == "Northern Ireland":
        reult="Saint-Georges-de-Reneins,Frankreich"
    if team == "Germany":
        reult="Evian-les-Bains,Frankreich"
    if team == "Poland":
        reult="La-Baule-Escoublac,Frankreich"

    return reult