import csv
import codecs
from bs4 import BeautifulSoup
from selenium import webdriver

#Scraping von whoscored.com mithilfe lokal gespeicherter HTML
def scraper(url, first):
    f=codecs.open(url[0], 'r', 'utf-8')
    soup= BeautifulSoup(f.read(), "html.parser")
    score=soup.find(id="event-type-filters")
    statsTop=score.find_all("li", class_="filterz-option")
    statsBottom1=soup.find_all("div", class_="filterz-filter-group")
    if first==True:
        header=[]
    home=[]
    away=[]

    # Scraping der Überkategorien
    for el in statsTop:
        statName=el.find("h4")
        if first:
            header.append(statName.text.strip())
        elValues=el.find("div", class_="filterz-values")
        statHomeValue=elValues.find("span", {"data-field" : "home"}).text.strip()
        home.append(int(statHomeValue))
        statAwayValue=elValues.find("span", {"data-field" : "away"}).text.strip()
        away.append(int(statAwayValue))

    # Scraping der detaillierten Kategorien
    for elParent in statsBottom1:
        statName1Help=elParent.find("h3")
        if statName1Help is not None:
            statName1=statName1Help.text.strip()
        subGroup=elParent.find_all("div", class_="filterz-filter-sub-group")
        for elSub in subGroup:
            statName2=elSub.find("h4").text.strip()
            elSubSub=elSub.find_all("div", class_="filterz-filter")
            for a in elSubSub:
                statName3=a.find("label").text.strip()
                if(statName3=="All"):
                    continue
                statNameFull= statName1 +" - " + statName2 + " - " +  statName3
                if first:
                    header.append(statNameFull)
                statHomeValue=a.find("span", {"data-field" : "home"}).text.strip()
                home.append(int(statHomeValue))
                statAwayValue=a.find("span", {"data-field" : "away"}).text.strip()
                away.append(int(statAwayValue))


    #Scraping von transfermarkt.de

    driver = webdriver.Chrome(executable_path='./chromedriver')


    driver.get(url[1])
    soup2 = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    form=soup2.find("div", class_="row sb-formation")
    cols=form.find_all("div", class_="large-6 columns")
    if first:
        header.append("Average Age")
        header.append("Market Value")
    # Marktwert und ALter der Heimmannschaft
    footerHome=cols[0].find("div", class_="table-footer")
    tdsH= footerHome.find_all("td")
    stringAgeHome=tdsH[0].text.strip()
    l1H=len(stringAgeHome)
    ageH=stringAgeHome[9:l1H]
    stringValueH=tdsH[2].text.strip()
    l2H=len(stringValueH)
    valueH=stringValueH[11:(l2H-7)]
    ageH=ageH.replace(',', '.')
    valueH=valueH.replace(',', '.')
    home.append(float(ageH))
    home.append(float(valueH))

    # Marktwert und ALter der Auswärtsmannschaft
    footerAway=cols[1].find("div", class_="table-footer")
    tdsA= footerAway.find_all("td")
    stringAgeAway=tdsA[0].text.strip()
    l1A=len(stringAgeAway)
    ageA=stringAgeAway[9:l1A]
    stringValueAway=tdsA[2].text.strip()
    l2A=len(stringValueAway)
    valueA=stringValueAway[11:(l2A-7)]
    ageA=ageA.replace(',', '.')
    valueA=valueA.replace(',', '.')
    away.append(float(ageA))
    away.append(float(valueA))

    #Scraping des Ballbesitzes und der Zuschauerzahl von transfermarkt.de

    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url[2])
    soup3 = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    parent=soup3.find(id="yw1")
    tspans=parent.find_all("tspan")
    homePos=tspans[1].text.strip()
    awayPos=tspans[2].text.strip()
    if first:
        header.append("Ball Posession")
    home.append(int(homePos))
    away.append(int(awayPos))

    boxSpectators=soup3.find("p", class_="sb-zusatzinfos")
    spectatorsString=boxSpectators.find("strong").text.strip()
    lS=len(spectatorsString)
    spectatorsString=spectatorsString[0:(lS-10)]
    spectatorsString=spectatorsString.replace('.', '')
    if first:
        header.append("Spectators")
    home.append(int(spectatorsString))
    away.append(int(spectatorsString))

    #Scraping of Tackle rate and passing ratefrom kicker.de

    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url[3])
    soup4 = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    box=soup4.find("div", class_="kick__data-grid--max-width kick__data-grid--max-width")
    rows=box.find_all("div", recursive=False)
    passRow=rows[5]
    passHomeS=passRow.find("div", class_="kick__stats-bar__value kick__stats-bar__value--opponent1").text.strip()
    passHome=int(passHomeS[0:(len(passHomeS)-1)])
    passAwayS=passRow.find("div", class_="kick__stats-bar__value kick__stats-bar__value--opponent2").text.strip()
    passAway=int(passAwayS[0:(len(passAwayS)-1)])
    tackleRow=rows[7]
    tackleHomeS=tackleRow.find("div", class_="kick__stats-bar__value kick__stats-bar__value--opponent1").text.strip()
    tackleHome=int(tackleHomeS[0:(len(tackleHomeS)-1)])
    tackleAwayS=tackleRow.find("div", class_="kick__stats-bar__value kick__stats-bar__value--opponent2").text.strip()
    tackleAway=int(tackleAwayS[0:(len(tackleAwayS)-1)])
    if first:
        header.append("Pass rate")
    home.append(passHome)
    away.append(passAway)
    if first:
        header.append("Tackle rate")
    home.append(tackleHome)
    away.append(tackleAway)

    # Scraping of distance covered from uefa.com

    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url[4])
    soup4 = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    distanceBoxes=soup4.find_all("div", class_="stats-module__single-stat stats-module__single-stat--comparison")
    distanceBox=distanceBoxes[10]
    abc=distanceBox.find("pk-list-stat-item")
    homeD=abc["data"]
    distanceHome=float(homeD)
    awayD=abc["second-data"]
    distanceAway=float(awayD)
    if first:
        header.append("Distance covered")
    home.append(distanceHome)
    away.append(distanceAway)

    # Scraping of Result from fbref.com
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url[5])
    soup4 = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    
    #Result
    goals=soup4.find_all("div", class_="score")
    goalsH=int(goals[0].text.strip())
    goalsA=int(goals[1].text.strip())

    home.append(goalsH)
    away.append(goalsA)

    home.append((goalsH-goalsA))
    away.append(goalsA-goalsH)

    if first:
        header.append("Goals scored")
        header.append("Result-GoalDiff")
        header.append("Result-W/L/D")


    if (goalsH>goalsA):
        home.append(int("1"))
        away.append(int("-1"))
    if (goalsH<goalsA):
        home.append(int("-1"))
        away.append(int("1"))

    if (goalsH==goalsA):
        home.append(int("0"))
        away.append(int("0"))
    
    


    # Writing into a CSV file
    if first:
        file=open("Data_complete.csv", "w", newline="")
        writer=csv.writer(file)
        if first:
            writer.writerow(header)
        writer.writerow(home)
        writer.writerow(away)
    if first is False:
        file=open("Data_complete.csv", "a", newline="")
        writer=csv.writer(file)
        writer.writerow(home)
        writer.writerow(away)

def addColEnd(col):
    i=0
    with open("Data_complete.csv", "r", newline="") as input:
        with open("Data_complete.csv", "w", newline="") as output:
            writer=csv.writer(output)
            rows=csv.reader(input)
            for row in rows:
                writer.writerow(row + [col[i]])
                i=i+1

def addColBeg(col):
    i=0
    with open("Data_complete.csv", "r", newline="") as input:
        with open("Data_complete_addOn.csv", "w", newline="") as output:
            writer=csv.writer(output)
            rows=csv.reader(input)
            for row in rows:
                writer.writerow([col[i]] + row) 
                i=i+1
    