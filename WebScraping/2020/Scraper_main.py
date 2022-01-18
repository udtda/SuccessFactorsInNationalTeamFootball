
from bs4 import BeautifulSoup
from selenium import webdriver
import pandas as pd

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import scraper, addColEnd
from fbref_links import fbreflinkgetter, fbrefstadiumsgetter, fbrefteamsgetter, baseGetter, fbrefmatchgetter

whoscored=[]
for i in range(52):
    if i==0:
        continue
    whoscored.append("Ressources_WhoScored_Links/" + str(i) + ".html")


tm1=['https://www.transfermarkt.de/turkei_italien/aufstellung/spielbericht/3287199', 'https://www.transfermarkt.de/wales_schweiz/aufstellung/spielbericht/3287200', 'https://www.transfermarkt.de/danemark_finnland/aufstellung/spielbericht/3287202', 'https://www.transfermarkt.de/belgien_russland/aufstellung/spielbericht/3287201', 'https://www.transfermarkt.de/england_kroatien/aufstellung/spielbericht/3287204', 'https://www.transfermarkt.de/osterreich_nordmazedonien/aufstellung/spielbericht/3486424', 'https://www.transfermarkt.de/niederlande_ukraine/aufstellung/spielbericht/3287203', 'https://www.transfermarkt.de/schottland_tschechien/aufstellung/spielbericht/3486425', 'https://www.transfermarkt.de/polen_slowakei/aufstellung/spielbericht/3486423', 'https://www.transfermarkt.de/spanien_schweden/aufstellung/spielbericht/3287205', 'https://www.transfermarkt.de/ungarn_portugal/aufstellung/spielbericht/3486422', 'https://www.transfermarkt.de/frankreich_deutschland/aufstellung/spielbericht/3287206', 'https://www.transfermarkt.de/finnland_russland/aufstellung/spielbericht/3287209', 'https://www.transfermarkt.de/turkei_wales/aufstellung/spielbericht/3287208', 'https://www.transfermarkt.de/italien_schweiz/aufstellung/spielbericht/3287207', 'https://www.transfermarkt.de/ukraine_nordmazedonien/aufstellung/spielbericht/3486431', 'https://www.transfermarkt.de/danemark_belgien/aufstellung/spielbericht/3287210', 'https://www.transfermarkt.de/niederlande_osterreich/aufstellung/spielbericht/3287211', 'https://www.transfermarkt.de/schweden_slowakei/aufstellung/spielbericht/3486433', 'https://www.transfermarkt.de/kroatien_tschechien/aufstellung/spielbericht/3287212', 'https://www.transfermarkt.de/england_schottland/aufstellung/spielbericht/3486432', 'https://www.transfermarkt.de/ungarn_frankreich/aufstellung/spielbericht/3486434', 'https://www.transfermarkt.de/portugal_deutschland/aufstellung/spielbericht/3287214', 'https://www.transfermarkt.de/spanien_polen/aufstellung/spielbericht/3287213', 'https://www.transfermarkt.de/italien_wales/aufstellung/spielbericht/3287215', 'https://www.transfermarkt.de/schweiz_turkei/aufstellung/spielbericht/3287216', 'https://www.transfermarkt.de/nordmazedonien_niederlande/aufstellung/spielbericht/3486435', 'https://www.transfermarkt.de/ukraine_osterreich/aufstellung/spielbericht/3287219', 'https://www.transfermarkt.de/russland_danemark/aufstellung/spielbericht/3287218', 'https://www.transfermarkt.de/finnland_belgien/aufstellung/spielbericht/3287217', 'https://www.transfermarkt.de/kroatien_schottland/aufstellung/spielbericht/3486436', 'https://www.transfermarkt.de/tschechien_england/aufstellung/spielbericht/3287220', 'https://www.transfermarkt.de/schweden_polen/aufstellung/spielbericht/3287221', 'https://www.transfermarkt.de/slowakei_spanien/aufstellung/spielbericht/3486437', 'https://www.transfermarkt.de/deutschland_ungarn/aufstellung/spielbericht/3486438', 'https://www.transfermarkt.de/portugal_frankreich/aufstellung/spielbericht/3287222', 'https://www.transfermarkt.de/wales_danemark/aufstellung/spielbericht/3586063', 'https://www.transfermarkt.de/italien_osterreich/aufstellung/spielbericht/3586029', 'https://www.transfermarkt.de/niederlande_tschechien/aufstellung/spielbericht/3587340', 'https://www.transfermarkt.de/belgien_portugal/aufstellung/spielbericht/3587342', 'https://www.transfermarkt.de/kroatien_spanien/aufstellung/spielbericht/3587341', 'https://www.transfermarkt.de/frankreich_schweiz/aufstellung/spielbericht/3587344', 'https://www.transfermarkt.de/england_deutschland/aufstellung/spielbericht/3587345', 'https://www.transfermarkt.de/schweden_ukraine/aufstellung/spielbericht/3587343', 'https://www.transfermarkt.de/schweiz_spanien/aufstellung/spielbericht/3590598', 'https://www.transfermarkt.de/belgien_italien/aufstellung/spielbericht/3590599', 'https://www.transfermarkt.de/tschechien_danemark/aufstellung/spielbericht/3590600', 'https://www.transfermarkt.de/ukraine_england/aufstellung/spielbericht/3590627', 'https://www.transfermarkt.de/italien_spanien/aufstellung/spielbericht/3598434', 'https://www.transfermarkt.de/england_danemark/aufstellung/spielbericht/3599157', 'https://www.transfermarkt.de/italien_england/aufstellung/spielbericht/3605575']
tm2=['https://www.transfermarkt.de/turkei_italien/statistik/spielbericht/3287199', 'https://www.transfermarkt.de/wales_schweiz/statistik/spielbericht/3287200', 'https://www.transfermarkt.de/danemark_finnland/statistik/spielbericht/3287202', 'https://www.transfermarkt.de/belgien_russland/statistik/spielbericht/3287201', 'https://www.transfermarkt.de/england_kroatien/statistik/spielbericht/3287204', 'https://www.transfermarkt.de/osterreich_nordmazedonien/statistik/spielbericht/3486424', 'https://www.transfermarkt.de/niederlande_ukraine/statistik/spielbericht/3287203', 'https://www.transfermarkt.de/schottland_tschechien/statistik/spielbericht/3486425', 'https://www.transfermarkt.de/polen_slowakei/statistik/spielbericht/3486423', 'https://www.transfermarkt.de/spanien_schweden/statistik/spielbericht/3287205', 'https://www.transfermarkt.de/ungarn_portugal/statistik/spielbericht/3486422', 'https://www.transfermarkt.de/frankreich_deutschland/statistik/spielbericht/3287206', 'https://www.transfermarkt.de/finnland_russland/statistik/spielbericht/3287209', 'https://www.transfermarkt.de/turkei_wales/statistik/spielbericht/3287208', 'https://www.transfermarkt.de/italien_schweiz/statistik/spielbericht/3287207', 'https://www.transfermarkt.de/ukraine_nordmazedonien/statistik/spielbericht/3486431', 'https://www.transfermarkt.de/danemark_belgien/statistik/spielbericht/3287210', 'https://www.transfermarkt.de/niederlande_osterreich/statistik/spielbericht/3287211', 'https://www.transfermarkt.de/schweden_slowakei/statistik/spielbericht/3486433', 'https://www.transfermarkt.de/kroatien_tschechien/statistik/spielbericht/3287212', 'https://www.transfermarkt.de/england_schottland/statistik/spielbericht/3486432', 'https://www.transfermarkt.de/ungarn_frankreich/statistik/spielbericht/3486434', 'https://www.transfermarkt.de/portugal_deutschland/statistik/spielbericht/3287214', 'https://www.transfermarkt.de/spanien_polen/statistik/spielbericht/3287213', 'https://www.transfermarkt.de/italien_wales/statistik/spielbericht/3287215', 'https://www.transfermarkt.de/schweiz_turkei/statistik/spielbericht/3287216', 'https://www.transfermarkt.de/nordmazedonien_niederlande/statistik/spielbericht/3486435', 'https://www.transfermarkt.de/ukraine_osterreich/statistik/spielbericht/3287219', 'https://www.transfermarkt.de/russland_danemark/statistik/spielbericht/3287218', 'https://www.transfermarkt.de/finnland_belgien/statistik/spielbericht/3287217', 'https://www.transfermarkt.de/kroatien_schottland/statistik/spielbericht/3486436', 'https://www.transfermarkt.de/tschechien_england/statistik/spielbericht/3287220', 'https://www.transfermarkt.de/schweden_polen/statistik/spielbericht/3287221', 'https://www.transfermarkt.de/slowakei_spanien/statistik/spielbericht/3486437', 'https://www.transfermarkt.de/deutschland_ungarn/statistik/spielbericht/3486438', 'https://www.transfermarkt.de/portugal_frankreich/statistik/spielbericht/3287222', 'https://www.transfermarkt.de/wales_danemark/statistik/spielbericht/3586063', 'https://www.transfermarkt.de/italien_osterreich/statistik/spielbericht/3586029', 'https://www.transfermarkt.de/niederlande_tschechien/statistik/spielbericht/3587340', 'https://www.transfermarkt.de/belgien_portugal/statistik/spielbericht/3587342', 'https://www.transfermarkt.de/kroatien_spanien/statistik/spielbericht/3587341', 'https://www.transfermarkt.de/frankreich_schweiz/statistik/spielbericht/3587344', 'https://www.transfermarkt.de/england_deutschland/statistik/spielbericht/3587345', 'https://www.transfermarkt.de/schweden_ukraine/statistik/spielbericht/3587343', 'https://www.transfermarkt.de/schweiz_spanien/statistik/spielbericht/3590598', 'https://www.transfermarkt.de/belgien_italien/statistik/spielbericht/3590599', 'https://www.transfermarkt.de/tschechien_danemark/statistik/spielbericht/3590600', 'https://www.transfermarkt.de/ukraine_england/statistik/spielbericht/3590627', 'https://www.transfermarkt.de/italien_spanien/statistik/spielbericht/3598434', 'https://www.transfermarkt.de/england_danemark/statistik/spielbericht/3599157', 'https://www.transfermarkt.de/italien_england/statistik/spielbericht/3605575']

kicker=["https://www.kicker.de/tuerkei-gegen-italien-2021-europameisterschaft-4214059/spieldaten", "https://www.kicker.de/wales-gegen-schweiz-2021-europameisterschaft-4214060/spieldaten", "https://www.kicker.de/daenemark-gegen-finnland-2021-europameisterschaft-4214066/spieldaten", "https://www.kicker.de/belgien-gegen-russland-2021-europameisterschaft-4214065/spieldaten", "https://www.kicker.de/england-gegen-kroatien-2021-europameisterschaft-4214077/spieldaten", "https://www.kicker.de/oesterreich-gegen-nordmazedonien-2021-europameisterschaft-4214072/spieldaten", "https://www.kicker.de/niederlande-gegen-ukraine-2021-europameisterschaft-4214071/spieldaten", "https://www.kicker.de/schottland-gegen-tschechien-2021-europameisterschaft-4214078/spieldaten", "https://www.kicker.de/polen-gegen-slowakei-2021-europameisterschaft-4214086/spieldaten", "https://www.kicker.de/spanien-gegen-schweden-2021-europameisterschaft-4214085/spieldaten", "https://www.kicker.de/ungarn-gegen-portugal-2021-europameisterschaft-4214090/spieldaten", "https://www.kicker.de/frankreich-gegen-deutschland-2021-europameisterschaft-4214089/spieldaten", "https://www.kicker.de/finnland-gegen-russland-2021-europameisterschaft-4214068/spieldaten", "https://www.kicker.de/tuerkei-gegen-wales-2021-europameisterschaft-4214062/spieldaten", "https://www.kicker.de/italien-gegen-schweiz-2021-europameisterschaft-4214061/spieldaten", "https://www.kicker.de/ukraine-gegen-nordmazedonien-2021-europameisterschaft-4214074/spieldaten", "https://www.kicker.de/daenemark-gegen-belgien-2021-europameisterschaft-4214067/spieldaten", "https://www.kicker.de/niederlande-gegen-oesterreich-2021-europameisterschaft-4214073/spieldaten", "https://www.kicker.de/schweden-gegen-slowakei-2021-europameisterschaft-4214084/spieldaten", "https://www.kicker.de/kroatien-gegen-tschechien-2021-europameisterschaft-4214080/spieldaten", "https://www.kicker.de/england-gegen-schottland-2021-europameisterschaft-4214079/spieldaten", "https://www.kicker.de/ungarn-gegen-frankreich-2021-europameisterschaft-4214092/spieldaten", "https://www.kicker.de/portugal-gegen-deutschland-2021-europameisterschaft-4214091/spieldaten", "https://www.kicker.de/spanien-gegen-polen-2021-europameisterschaft-4214083/spieldaten", "https://www.kicker.de/italien-gegen-wales-2021-europameisterschaft-4214063/spieldaten", "https://www.kicker.de/schweiz-gegen-tuerkei-2021-europameisterschaft-4214064/spieldaten", "https://www.kicker.de/nordmazedonien-gegen-niederlande-2021-europameisterschaft-4214075/spieldaten", "https://www.kicker.de/ukraine-gegen-oesterreich-2021-europameisterschaft-4214076/spieldaten", "https://www.kicker.de/russland-gegen-daenemark-2021-europameisterschaft-4214070/spieldaten", "https://www.kicker.de/finnland-gegen-belgien-2021-europameisterschaft-4214069/spieldaten", "https://www.kicker.de/kroatien-gegen-schottland-2021-europameisterschaft-4214082/spieldaten", "https://www.kicker.de/tschechien-gegen-england-2021-europameisterschaft-4214081/spieldaten", "https://www.kicker.de/schweden-gegen-polen-2021-europameisterschaft-4214088/spieldaten", "https://www.kicker.de/slowakei-gegen-spanien-2021-europameisterschaft-4214087/spieldaten", "https://www.kicker.de/deutschland-gegen-ungarn-2021-europameisterschaft-4214093/spieldaten", "https://www.kicker.de/portugal-gegen-frankreich-2021-europameisterschaft-4214094/spieldaten", "https://www.kicker.de/wales-gegen-daenemark-2021-europameisterschaft-4214151/spieldaten", "https://www.kicker.de/italien-gegen-oesterreich-2021-europameisterschaft-4214058/spieldaten", "https://www.kicker.de/niederlande-gegen-tschechien-2021-europameisterschaft-4214097/spieldaten", "https://www.kicker.de/belgien-gegen-portugal-2021-europameisterschaft-4214096/spieldaten", "https://www.kicker.de/kroatien-gegen-spanien-2021-europameisterschaft-4214095/spieldaten", "https://www.kicker.de/frankreich-gegen-schweiz-2021-europameisterschaft-4214098/spieldaten", "https://www.kicker.de/england-gegen-deutschland-2021-europameisterschaft-4214100/spieldaten", "https://www.kicker.de/schweden-gegen-ukraine-2021-europameisterschaft-4214099/spieldaten", "https://www.kicker.de/schweiz-gegen-spanien-2021-europameisterschaft-4214101/spieldaten", "https://www.kicker.de/belgien-gegen-italien-2021-europameisterschaft-4214102/spieldaten", "https://www.kicker.de/tschechien-gegen-daenemark-2021-europameisterschaft-4214104/spieldaten", "https://www.kicker.de/ukraine-gegen-england-2021-europameisterschaft-4214103/spieldaten", "https://www.kicker.de/italien-gegen-spanien-2021-europameisterschaft-4214105/spieldaten", "https://www.kicker.de/england-gegen-daenemark-2021-europameisterschaft-4214106/spieldaten", "https://www.kicker.de/italien-gegen-england-2021-europameisterschaft-4214107/spieldaten"]
uefa=["https://www.uefa.com/uefaeuro-2020/match/2024447--turkey-vs-italy/statistics/?iv=true&iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024448--wales-vs-switzerland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024449--denmark-vs-finland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024450--belgium-vs-russia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024451--england-vs-croatia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024442--austria-vs-north-macedonia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024441--netherlands-vs-ukraine/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024452--scotland-vs-czech-republic/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024454--poland-vs-slovakia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024453--spain-vs-sweden/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024455--hungary-vs-portugal/statistics/?iv=true&iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024456--france-vs-germany/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024460--finland-vs-russia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024457--turkey-vs-wales/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024458--italy-vs-switzerland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024444--ukraine-vs-north-macedonia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024459--denmark-vs-belgium/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024443--netherlands-vs-austria/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024464--sweden-vs-slovakia/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024462--croatia-vs-czech-republic/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024461--england-vs-scotland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024465--hungary-vs-france/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024466--portugal-vs-germany/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024463--spain-vs-poland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024468--italy-vs-wales/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024467--switzerland-vs-turkey/statistics/?iv=true","https://www.uefa.com/uefaeuro-2020/match/2024445--north-macedonia-vs-netherlands/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024446--ukraine-vs-austria/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024469--russia-vs-denmark/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024470--finland-vs-belgium/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024472--croatia-vs-scotland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024471--czech-republic-vs-england/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024474--sweden-vs-poland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024473--slovakia-vs-spain/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024475--germany-vs-hungary/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024476--portugal-vs-france/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024478--wales-vs-denmark/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024477--italy-vs-austria/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024480--netherlands-vs-czech-republic/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024479--belgium-vs-portugal/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024482--croatia-vs-spain/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024481--france-vs-switzerland/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024484--england-vs-germany/statistics/?iv=true&iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024483--sweden-vs-ukraine/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024485--switzerland-vs-spain/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024486--belgium-vs-italy/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024488--czech-republic-vs-denmark/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024487--ukraine-vs-england/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024489--italy-vs-spain/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024490--england-vs-denmark/statistics/?iv=true", "https://www.uefa.com/uefaeuro-2020/match/2024491--italy-vs-england/statistics/?iv=true"]
fbref=fbreflinkgetter()

for i in range(51):
    url=[whoscored[i],  tm1[i], tm2[i], kicker[i], uefa[i], fbref[i]]
    if i==0:
        scraper(url, first=True)
    if i!=0:
        scraper(url, first=False)

stadiums=fbrefstadiumsgetter()
for i in range(len(stadiums)):
    stadiums[i]=stadiums[i].replace(" ", "-")
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

# Adding Travel distance
df=pd.read_csv("Data_complete.csv")
df["TravelDistance"]=travelDist

# Adding Match namees
match=fbrefmatchgetter()
df["Match1"]=match
df.insert(loc=0, column="Match",value=df["Match1"])
df=df.drop("Match1", 1)
df.to_csv("Data_complete.csv", index=False)