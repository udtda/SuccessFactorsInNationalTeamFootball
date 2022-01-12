#!/usr/bin/env Rscript

library(highcharter)
library(dplyr)

setwd('..')
setwd('..')
data=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)
setwd('./Analyse/2020')


############## Exchanging Ball possession values due to wrong allocation
for (i in 1:(nrow(data)-1)){
  if(i%%2==1){
  helper=data$Ball.Posession[i+1]
  data[i+1,"Ball.Posession"]=data$Ball.Posession[i]
  data[i,"Ball.Posession"]=helper
}
}

############# Adding additional variables
wHA=c()
shotAccuracy=c()
SaveRate=c()
MarketValueCompFact=c()
DistanceCoveredComp=c()

HomeAdvantage=c(-1,1,0,0,1,-1,-1,1,1,-1,0,0,1,-1,1,-1,0,0,1,-1,1,-1,-1,1,-1,1,0,0,1,-1,0,0,1,-1,1,-1,0,0,0,0,1,-1,1,-1,-1,1,1,-1,1,-1,0,0,-1,1,0,0,-1,1,0,0,-1,1,-1,1,0,0,-1,1,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,-1,1)

for (i in 1:dim(data)[1]){
  helper=HomeAdvantage[i]*data$Spectators[i]
  wHA=append(wHA, helper)
}

for (i in 1:dim(data)[1]){
  helper=round((data$Shots...Results...Shots.on.Target[i]/data$Shots[i])*100, digits = 2)
  shotAccuracy=append(shotAccuracy, helper)
}

for (i in 1:dim(data)[1]){
  if (i%%2 == 1){
    helper=round((data$Saves[i]/data$Shots...Results...Shots.on.Target[i+1]*100), digits = 2)
    SaveRate=append(SaveRate, helper)
  }
  if (i%%2 == 0){
    helper=round((data$Saves[i]/data$Shots...Results...Shots.on.Target[i-1]*100), digits = 2)
    SaveRate=append(SaveRate, helper)
  }
  
}
indexna=which(is.na(SaveRate))
SaveRate[indexna]=0
indexinf=which(is.finite(SaveRate)==FALSE)
SaveRate[indexinf]=100

for (i in 1:dim(data)[1]){
  if (i%%2 == 1){
    helper=round(data$Market.Value[i]/data$Market.Value[i+1], digits = 2)
    MarketValueCompFact=append(MarketValueCompFact, helper)
  }
  if (i%%2 == 0){
    helper=round(data$Market.Value[i]/data$Market.Value[i-1], digits = 2)
    MarketValueCompFact=append(MarketValueCompFact, helper)
  }
  
}

for (i in 1:dim(data)[1]){
  if (i%%2 == 1){
    helper=round(data$Distance.covered[i]-data$Distance.covered[i+1], digits = 2)
    DistanceCoveredComp=append(DistanceCoveredComp, helper)
  }
  if (i%%2 == 0){
    helper=round(data$Distance.covered[i]-data$Distance.covered[i-1], digits = 2)
    DistanceCoveredComp=append(DistanceCoveredComp, helper)
  }
  
}


data=cbind(data, HomeAdvantage, wHA, shotAccuracy, SaveRate, MarketValueCompFact, DistanceCoveredComp)
data$MarketValueCompFact=log(data$MarketValueCompFact)
data=cbind(rownames(data), data)
data$SaveRate=round(data$SaveRate, digits = 0)
colnames(data)[1]=c("Match")
write.csv(data, "Data_complete_full.csv", row.names = FALSE)
data=data[,-1]

############# Pre-processing
# Removing "Clearances - off the line" (all zeros), 'Market Value' and 'Distance covered', and all Result columns
data_red=subset(data, select = -c(59,69,74,75,76,77))

#Testing for perfect correlation
for (i in 1:(dim(data_red)[2]-1)){
  for (j in (i+1):dim(data_red)[2]){
    test=cor(data_red[,i], data_red[,j])
    if (test==1 || test==-1){
      print(colnames(data_red)[i])
      print(colnames(data_red)[j])
      print(test)
      print(" ")
    }
  }  
}

#Deleting "Clearances - Outcome - total" and "Shots - Situation - Own Goal" due to perfect correlation
data_red=subset(data_red, select = -c(58, 30))

#Testing for almost perfect correlation

for (i in 1:(dim(data_red)[2]-1)){
  for (j in (i+1):dim(data_red)[2]){
    test=cor(data_red[,i], data_red[,j])
    if (test>0.95 || test<(-0.95)){
      print(colnames(data_red)[i])
      print(colnames(data_red)[j])
      print(test)
      print(' ')
    }
  }  
}
# Deleting "Touches" as well as "Passes - Length - Short", "Passes - Height - Ground", "Passes - Body Part - Feet", "Passes - Target Zone - Mid Third" and all "Passes - Direction"
data_red=subset(data_red, select = -c(11, 41,43,45, 46:49, 51))

#Testing for correlation > 0.9

for (i in 1:(dim(data_red)[2]-1)){
  for (j in (i+1):dim(data_red)[2]){
    test=cor(data_red[,i], data_red[,j])
    if (test>0.9 || test<(-0.9)){
      print(colnames(data_red)[i])
      print(colnames(data_red)[j])
      print(test)
      print(' ')
    }
  }  
}

#Checking VIFs for each remaining Variable

vifs=as.data.frame(matrix(ncol=2, nrow = dim(data_red)[2]))

for (i in 1:dim(data_red)[2]){
  helpset=subset(data_red, select = -c(i))
  helpmodel=lm(data=helpset, formula = data_red[,i] ~.)
  helprsquared=summary(helpmodel)$r.squared
  if (helprsquared != 1){
    vifhelp=1/(1-helprsquared)
  } else {
    vifhelp=99999
  }
  vifs[i,1]=colnames(data_red)[i]
  vifs[i,2]=round(vifhelp, digits = 2)
  
}

vifs

# Reducing Data by removing detailed components such as "Shots - Results" (relevant information is represented in shotAccuracy), 
# "Shots - Zones" ,"Shots - Body parts" (not relevant in theory) and any 
# detailed stats for Dribbles, Tackles, Clearances, Blocks, Loss of Possession and Errors
data_red=subset(data_red, select = -c(16:24, 29:32, 44:55))

#Checking VIFs for each remaining Variable

vifs=matrix(ncol=2, nrow = dim(data_red)[2])
vifs=as.data.frame(vifs)

for (i in 1:dim(data_red)[2]){
  helpset=subset(data_red, select = -c(i))
  helpmodel=lm(data=helpset, formula = data_red[,i] ~.)
  helprsquared=summary(helpmodel)$r.squared
  if (helprsquared != 1){
    vifhelp=1/(1-helprsquared)
  } else {
    vifhelp=99999
  }
  vifs[i,1]=colnames(data_red)[i]
  vifs[i,2]=round(vifhelp, digits = 2)
  
}

vifs


# Deleting Saves and spectators (correlation to result is obviously 0)
data_red=data_red[,-c(13,33)]

# Writing final data set into csv file
data_clean=cbind(rownames(data_red), data_red)
colnames(data_clean)[1]=c("Match")
write.csv(data_clean, "Data_clean.csv", row.names = FALSE)

