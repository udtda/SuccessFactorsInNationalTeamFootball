from bs4 import BeautifulSoup
from numpy.lib import index_tricks
from selenium import webdriver
import pandas as pd

from Links import tmLinkGetterAuf, tmLinkGetterStat
from Scraper import scraper, addColEnd
from fbref_links import fbreflinkgetter, fbrefmatchgetter, fbrefstadiumsgetter, fbrefteamsgetter, baseGetter

df=pd.read_csv("Data_complete.csv")

#match=fbrefmatchgetter()
#df["Match1"]=match
df.insert(loc=0, column="Matchh",value=df["Match"])
df=df.drop("Match", 1)
df = df.rename(columns={df.columns[0]: 'Match'})
print(df)

df.to_csv("Data_complete.csv", index=False)