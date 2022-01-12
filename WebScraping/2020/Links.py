import csv
import codecs
from bs4 import BeautifulSoup
from selenium import webdriver


def tmLinkGetterAuf(url):
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url)
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()

    elAufstellung=soup.find(id="aufstellung")
    linkAufstellung=elAufstellung.find("a")["href"]
    linkAufstelllung="https://www.transfermarkt.de" + linkAufstellung

    

    return linkAufstelllung

def tmLinkGetterStat(url):
    driver = webdriver.Chrome(executable_path='./chromedriver')

    driver.get(url)
    soup = BeautifulSoup(driver.page_source, features="html.parser")
    driver.quit()
    elStatistik=soup.find(id="statistik")
    linkStatistik=elStatistik.find("a")["href"]
    linkStatistik="https://www.transfermarkt.de" + linkStatistik

    return linkStatistik