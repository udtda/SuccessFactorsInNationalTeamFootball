library(glmnet)
library(dplyr)

# Variablen mit Daten füllen
X=read.csv("Data_clean.csv", header=TRUE, row.names = 1)
setwd('..')
setwd('..')
y1=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.W.L.D
y2=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.GoalDiff
setwd('./Analyse/2020')
C=1000
n=dim(X)[1]
p=dim(X)[2]

#####Erster Step
# Erstellen der Tabelle zum Sammeln der ERgebnisse
resulttableLASSOavg=cbind(rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]),rep(0,dim(X)[2]))
rownames(resulttableLASSOavg)=colnames(X)
resulttableLASSOavg=rbind(c(0,0,0), resulttableLASSOavg)
rownames(resulttableLASSOavg)[1]=c("Intercept")
colnames(resulttableLASSOavg)=c("Share.of.Selections", "Distance.next.variable", "Avg.Coefficient", "Standard.Deviance.Coefficient", "Coefficient.Variation", "Share.same.direction")

# Erstellen einer weiteren Tabelle
valuestable=matrix(ncol=(dim(X)[2]+1), nrow=1000)
colnames(valuestable)[2:(dim(X)[2]+1)]=colnames(X)
colnames(valuestable)[1]=c("Intercept")
valuestable[is.na(valuestable)]=0
valuestable=as.data.frame(valuestable)

options(scipen=999)
# Durchführung des Algorithmus
set.seed(100)
for (i in 1:C){
  # Aufteilen der Daten
  index=sample(1:n, n*0.5)
  X.sub1=X[index,]
  X.sub2=X[-index,]
  y1.sub1=y1[index]
  y1.sub2=y1[-index]
  model=cv.glmnet(as.matrix(X.sub1), y1.sub1, type.measure = "mse")
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
    mod=lm(data=a, formula = y1.sub2~.)
    
    l=1
    for (k in 1:dim(valuestable)[2]){
      if(l<=length(ind) && k==ind[l]){
        valuestable[i,k]=valuestable[i,k]+as.double(coef(mod)[l])
        l=l+1
      } 
    }
  }
}

# Befüllen der ersten Spalte der resulttable mit der Anzahl der Selektionen
for (i in 1:dim(resulttableLASSOavg)[1]){
  resulttableLASSOavg[i,1]=sum(valuestable[,i]!=0)
}

# Befüllen der dritten Spalte der resulttable mit den durschnittlichen Koeffizienten
for (i in 1:dim(resulttableLASSOavg)[1]){
  help=subset(valuestable[,i], valuestable[,i]!=0)
  resulttableLASSOavg[i,3]=mean(help)
}

# Befüllen der vierten Spalte der resulttable mit der Standardabweichung der Koeffizienten
for (i in 1:dim(resulttableLASSOavg)[1]){
  help=subset(valuestable[,i], valuestable[,i]!=0)
  resulttableLASSOavg[i,4]=sd(help)
}

# Befüllen der fünften Spalte der resulttable mit dem Variationskoeffizienten der Koeffizienten
for (i in 1:dim(resulttableLASSOavg)[1]){
  resulttableLASSOavg[i,5]=resulttableLASSOavg[i,4]/resulttableLASSOavg[i,3]
}

# Befüllen der sechsten Spalte der resulttable mit dem Anteil an Schätzungen mit dem selben Vorzeichen wie der Durchschnittswert
for (i in 1:dim(resulttableLASSOavg)[1]){
  if(resulttableLASSOavg[i,3]>0){
    help=subset(valuestable[,i], valuestable[,i]>0)
    resulttableLASSOavg[i,6]=length(help)/resulttableLASSOavg[i,1]
  }
  if(resulttableLASSOavg[i,3]<0){
    help=subset(valuestable[,i], valuestable[,i]<0)
    resulttableLASSOavg[i,6]=length(help)/resulttableLASSOavg[i,1]
  }
}

# Teilen der Anzahl der Selektionen durch C
C=resulttableLASSOavg[1,1]
resulttableLASSOavg=as.data.frame(resulttableLASSOavg)
for(i in 1:dim(resulttableLASSOavg)[1]){
  if(resulttableLASSOavg[i,1]!=0){
    resulttableLASSOavg[i,1]=round(resulttableLASSOavg[i,1]/C, digits = 3)
  }
}

# Sortieren der Tabelle und bestimmen des ABstands zur nächsten Klasse
resulttableLASSOavg=resulttableLASSOavg%>%arrange(Share.of.Selections)
for (i in 1:(dim(resulttableLASSOavg)[1]-1)){
  resulttableLASSOavg[i,2]=resulttableLASSOavg[i+1,1]-resulttableLASSOavg[i,1]
}

# Runden
resulttableLASSOavg[,4]=round(resulttableLASSOavg[,4], digits=3)
resulttableLASSOavg[,5]=round(resulttableLASSOavg[,5], digits=3)
resulttableLASSOavg[,6]=round(resulttableLASSOavg[,6], digits=3)

print(resulttableLASSOavg)

##### Zweiter Step

# Cross-Validation des fertigen Modells
library(caret)
set.seed(100)
X1=X[,c("shotAccuracy", "DistanceCoveredComp","MarketValueCompFact","Passes...Pass.Type...Cross", "Tackles.Attempted",  "SaveRate", "Passes...Length...Long", "Dribbles", "Clearances")]
X1=cbind(X1,y1)
train.control = trainControl(method = "cv", number = 5)
model = train(y1 ~., data = X1, method = "lm",
              trControl = train.control)
# Summarize the results
print(model)

