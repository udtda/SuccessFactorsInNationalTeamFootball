
from bs4 import BeautifulSoup
from selenium import webdriver

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import scraper


def fbreflinkgetter():
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get("https://fbref.com/en/comps/676/schedule/UEFA-Euro-Scores-and-Fixtures")
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

    driver.get("https://fbref.com/en/comps/676/schedule/UEFA-Euro-Scores-and-Fixtures")
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

    driver.get("https://fbref.com/en/comps/676/schedule/UEFA-Euro-Scores-and-Fixtures")
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

    driver.get("https://fbref.com/en/comps/676/schedule/UEFA-Euro-Scores-and-Fixtures")
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
    if team == "Austria":
        reult="Seefeld,Oesterreich"
    if team == "Belgium":
        reult="Tubize,Belgien"
    if team == "Croatia":
        reult="Rovinj,Kroatien"
    if team == "Czech Republic":
        reult="Prag,Tschechische Republik"
    if team == "Denmark":
        reult="Helsingor,Daenemark"
    if team == "England":
        reult="Burton-upon-Trent,England"
    if team == "Finland":
        reult="Repino,St-Petersburg,Russland"
    if team == "France":
        reult="Clairefontaine-en-Yvelines,Frankreich"
    if team == "Germany":
        reult="Herzogenaurach,Deutschland"
    if team == "Hungary":
        reult="Telki,Ungarn"
    if team == "Italy":
        reult="Coverciano,Florenz,Italien"
    if team == "Netherlands":
        reult="Zeist,Niederlande"
    if team == "North Macedonia":
        reult="Bukarest,Rumaenien"
    if team == "Poland":
        reult="Sopot,Polen"
    if team == "Portugal":
        reult="Budapest,Ungarn"
    if team == "Russia":
        reult="Novogorsk,Moskau,Russland"
    if team == "Scotland":
        reult="Middlesbrough,England"
    if team == "Scotland":
        reult="Middlesbrough,England"
    if team == "Slovakia":
        reult="St-Petersburg,Russland"
    if team == "Spain":
        reult="Las-Rozas-de-Madrid,Madrid,Spanien"
    if team == "Sweden":
        reult="Goeteborg,Schweden"
    if team == "Switzerland":
        reult="Rom,Italien"
    if team == "Turkey":
        reult="Baku,Aserbaidschan"
    if team == "Ukraine":
        reult="Bukarest,Rumaenien"
    if team == "Wales":
        reult="Baku,Aserbaidschan"

    return reult