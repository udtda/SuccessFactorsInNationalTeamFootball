library(glmnet)
library(dplyr)

# Variablen mit Daten füllen
X=read.csv("Data_clean_2016.csv", header=TRUE, row.names = 1)
setwd('..')
setwd('..')
y1=read.csv("WebScraping/2016/Data_complete_2016.csv", header=TRUE, row.names = 1)$Result.W.L.D
y2=read.csv("WebScraping/2016/Data_complete_2016.csv", header=TRUE, row.names = 1)$Result.GoalDiff
setwd('./Analyse/2016')
C=1000
n=dim(X)[1]
p=dim(X)[2]

# Erstellen der Tabelle zum Sammeln der ERgebnisse
resulttableLASSOavgGB=cbind(rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]))
rownames(resulttableLASSOavgGB)=colnames(X)
resulttableLASSOavgGB=rbind(c(0,0,0), resulttableLASSOavgGB)
rownames(resulttableLASSOavgGB)[1]=c("Intercept")
colnames(resulttableLASSOavgGB)=c("Share.of.Selections", "Distance.next.variable", "Avg.Coefficient", "Standard.Deviance.Coefficient", "Coefficient.Variation", "Share.same.direction")

# Erstellen einer weiteren Tabelle
valuestableLASSOGB=matrix(ncol=(dim(X)[2]+1), nrow=1000)
colnames(valuestableLASSOGB)[2:(dim(X)[2]+1)]=colnames(X)
colnames(valuestableLASSOGB)[1]=c("Intercept")
valuestableLASSOGB[is.na(valuestableLASSOGB)]=0
valuestableLASSOGB=as.data.frame(valuestableLASSOGB)

options(scipen=999)
# Durchführung des Algorithmus
set.seed(100)
for (i in 1:C){
  # Aufteilen der Daten
  index=sample(1:n, n*0.5)
  X.sub1=X[index,]
  X.sub2=X[-index,]
  y2.sub1=y2[index]
  y2.sub2=y2[-index]
  X.sub1=scale(X.sub1)
  model=cv.glmnet(as.matrix(X.sub1), y2.sub1, type.measure = "mse")
  model.coef=coef(model, s=model$lambda.min)
  
  # Markieren der ausgewählten Koeffizienten in einem Index
  print(i)
  ind=integer()
  for (j in 1:length(model.coef)){
    if(model.coef[j]!=0){
      ind=c(ind,j)
    }
  }
  
  if (length(ind)!=1){
    # Umgehen der Intercept Spalte
    a=cbind(rep(1,dim(X.sub2)[1]), X.sub2)
    a=a[,ind]
    a=subset(a, select=-c(1))
    
    # Zweites Modell mit nur den im ersten Schritt ausgewählten Variablen, auf der zweiten Hälfte der Daten
    mod=lm(data=a, formula = y2.sub2~.)
    
    l=1
    for (k in 1:dim(valuestableLASSOGB)[2]){
      if(l<=length(ind) && k==ind[l]){
        valuestableLASSOGB[i,k]=valuestableLASSOGB[i,k]+as.double(coef(mod)[l])
        l=l+1
      } 
    }
  }
}

# Befüllen der ersten Spalte der resulttable mit der Anzahl der Selektionen
for (i in 1:dim(resulttableLASSOavgGB)[1]){
  resulttableLASSOavgGB[i,1]=sum(valuestableLASSOGB[,i]!=0)
}

# Befüllen der dritten Spalte der resulttable mit den durschnittlichen Koeffizienten
for (i in 1:dim(resulttableLASSOavgGB)[1]){
  help=subset(valuestableLASSOGB[,i], valuestableLASSOGB[,i]!=0)
  resulttableLASSOavgGB[i,3]=mean(help)
}

# Befüllen der vierten Spalte der resulttable mit der Standardabweichung der Koeffizienten
for (i in 1:dim(resulttableLASSOavgGB)[1]){
  help=subset(valuestableLASSOGB[,i], valuestableLASSOGB[,i]!=0)
  resulttableLASSOavgGB[i,4]=sd(help)
}

# Befüllen der fünften Spalte der resulttable mit dem Variationskoeffizienten der Koeffizienten
for (i in 1:dim(resulttableLASSOavgGB)[1]){
  resulttableLASSOavgGB[i,5]=resulttableLASSOavgGB[i,4]/resulttableLASSOavgGB[i,3]
}

# Befüllen der sechsten Spalte der resulttable mit dem Anteil an Schätzungen mit dem selben Vorzeichen wie der Durchschnittswert
for (i in 1:dim(resulttableLASSOavgGB)[1]){
  if(resulttableLASSOavgGB[i,3]>0){
    help=subset(valuestableLASSOGB[,i], valuestableLASSOGB[,i]>0)
    resulttableLASSOavgGB[i,6]=length(help)/resulttableLASSOavgGB[i,1]
  }
  if(resulttableLASSOavgGB[i,3]<0){
    help=subset(valuestableLASSOGB[,i], valuestableLASSOGB[,i]<0)
    resulttableLASSOavgGB[i,6]=length(help)/resulttableLASSOavgGB[i,1]
  }
}

# Teilen der Anzahl der Selektionen durch C
C=resulttableLASSOavgGB["Intercept",1]
resulttableLASSOavgGB=as.data.frame(resulttableLASSOavgGB)
for(i in 1:dim(resulttableLASSOavgGB)[1]){
  if(resulttableLASSOavgGB[i,1]!=0){
    resulttableLASSOavgGB[i,1]=round(resulttableLASSOavgGB[i,1]/C, digits = 3)
  }
}

# Sortieren der Tabelle und bestimmen des ABstands zur nächsten Klasse
resulttableLASSOavgGB=resulttableLASSOavgGB%>%arrange(Share.of.Selections)
for (i in 1:(dim(resulttableLASSOavgGB)[1]-1)){
  resulttableLASSOavgGB[i,2]=resulttableLASSOavgGB[i+1,1]-resulttableLASSOavgGB[i,1]
}

# Runden
resulttableLASSOavgGB[,4]=round(resulttableLASSOavgGB[,4], digits=3)
resulttableLASSOavgGB[,5]=round(resulttableLASSOavgGB[,5], digits=3)
resulttableLASSOavgGB[,6]=round(resulttableLASSOavgGB[,6], digits=3)

print(resulttableLASSOavgGB)

##### Zweiter Step


# Fitten mithilfe dieses Modells
X1=X[c("Shots...Situation...Fastbreak","MarketValueCompFact","DistanceCoveredComp","shotAccuracy","Clearances", "HomeAdvantage", "SaveRate")]
# Cross-Validation des fertigen Modells
library(caret)
set.seed(100)
X1=cbind(X1,y2)
train.control = trainControl(method = "cv", number = 5)
model = train(y2 ~., data = X1, method = "lm",
              trControl = train.control)
# Summarize the results
print(model)

