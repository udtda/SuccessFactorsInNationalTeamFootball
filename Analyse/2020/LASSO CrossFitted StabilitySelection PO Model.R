library(glmnet)
library(glmnetcr)
library(ordinalNet)
library(nnet)
library(VGAM)
library(MASS)
library(dplyr)

# Variablen mit Daten füllen
X=read.csv("Data_clean.csv", header=TRUE, row.names = 1)
setwd('..')
setwd('..')
y1=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.W.L.D
y2=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.GoalDiff
setwd('./Analyse/2020')

#####Erster Step
# Erstellen der Tabelle zum Sammeln der ERgebnisse
resulttableLASSOavgMLOG2=matrix(nrow=dim(X)[2]+2, ncol=6)
resulttableLASSOavgMLOG2[is.na(resulttableLASSOavgMLOG2)]=0
rownames(resulttableLASSOavgMLOG2)[3:(dim(resulttableLASSOavgMLOG2)[1])]=colnames(X)
rownames(resulttableLASSOavgMLOG2)[1]=c("Intercept1")
rownames(resulttableLASSOavgMLOG2)[2]=c("Intercept2")
colnames(resulttableLASSOavgMLOG2)=c("Share.of.Selections", "Distance.next.variable", "Avg.Coefficient", "Standard.Deviance.Coefficient",  "Coefficient.Variation", "Share.same.direction")

# Erstellen einer weiteren Tabelle
valuestableMLOG2=matrix(0, nrow=1000, ncol=dim(resulttableLASSOavgMLOG2)[1])
valuestableMLOG2[is.na(valuestableMLOG2)]=0
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
  #X.sub1=scale(X.sub1)
  
  emptindex1=as.integer(which(colSums(X.sub1==0)==nrow(X.sub1)))
  emptindex2=as.integer(which(colSums(X.sub2==0)==nrow(X.sub2)))
  if(length(emptindex1)!=0){
    X.sub1=X.sub1[,-emptindex1]
    X.sub2=X.sub2[,-emptindex1]
  }
  if(length(emptindex2)!=0){
    X.sub1=X.sub1[,-emptindex2]
    X.sub2=X.sub2[,-emptindex2]
  }
  
  # Fitten des LASSO Modells und identifizieren des Lambdas
  model=ordinalNet(y=y1.sub1, x=as.matrix(X.sub1), family = "cumulative", reverse=TRUE, link="logit")
  a=ordinalNetTune(y=y1.sub1, x=as.matrix(X.sub1), family = "cumulative", reverse=TRUE, link="logit")
  b=which.max(summary(a)$loglik_avg)
  model.coef=model$coefs[b,]
  
  # Markieren der ausgewählten Koeffizienten in einem Index
  print(i)
  ind=integer()
  for (j in 1:length(model.coef)){
    if(model.coef[j]!=0){
      ind=c(ind,j)
    }
  }
  
  
  if (length(ind)>3){
    # Umgehen der Intercept Spalte
    a=cbind(rep(1,dim(X.sub2)[1]),rep(1,dim(X.sub2)[1]), X.sub2)
    a=a[,ind]
    a=subset(a, select=-c(1,2))
    # Unrestringiertes PO Model mit den Variablen aus Schritt 1 auf X2
    mod=vglm(data=a, formula=y1.sub2~., family=cumulative(parallel=TRUE, reverse = TRUE))
    mod.coef=mod@coefficients
    
    l=1
    for (k in 1:dim(valuestableMLOG2)[2]){
      if(l<=length(ind) && k==ind[l]){
        valuestableMLOG2[i,k]=valuestableMLOG2[i,k]+as.double(mod.coef[l])
        l=l+1
      }
    }
  }
}


# Befüllen der ersten Spalte der resulttable mit der Anzahl der Selektionen
for (i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  resulttableLASSOavgMLOG2[i,1]=sum(valuestableMLOG2[,i]!=0)
}

# Befüllen der dritten Spalte der resulttable mit den durschnittlichen Koeffizienten
for (i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  help=subset(valuestableMLOG2[,i], valuestableMLOG2[,i]!=0)
  resulttableLASSOavgMLOG2[i,3]=mean(help)
}

# Befüllen der vierten Spalte der resulttable mit der Standardabweichung der Koeffizienten
for (i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  help=subset(valuestableMLOG2[,i], valuestableMLOG2[,i]!=0)
  resulttableLASSOavgMLOG2[i,4]=sd(help)
}

# Befüllen der fünften Spalte der resulttable mit dem Variationskoeffizienten der Koeffizienten
for (i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  resulttableLASSOavgMLOG2[i,5]=resulttableLASSOavgMLOG2[i,4]/resulttableLASSOavgMLOG2[i,3]
}

# Befüllen der sechsten Spalte der resulttable mit dem Anteil an Schätzungen mit dem selben Vorzeichen wie der Durchschnittswert
for (i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  if(resulttableLASSOavgMLOG2[i,3]>0){
    help=subset(valuestableMLOG2[,i], valuestableMLOG2[,i]>0)
    resulttableLASSOavgMLOG2[i,6]=length(help)/resulttableLASSOavgMLOG2[i,1]
  }
  if(resulttableLASSOavgMLOG2[i,3]<0){
    help=subset(valuestableMLOG2[,i], valuestableMLOG2[,i]<0)
    resulttableLASSOavgMLOG2[i,6]=length(help)/resulttableLASSOavgMLOG2[i,1]
  }
}

# Teilen der Anzahl der Selektionen durch C
C=resulttableLASSOavgMLOG2[1,1]
resulttableLASSOavgMLOG2=as.data.frame(resulttableLASSOavgMLOG2)
for(i in 1:dim(resulttableLASSOavgMLOG2)[1]){
  if(resulttableLASSOavgMLOG2[i,1]!=0){
    resulttableLASSOavgMLOG2[i,1]=round(resulttableLASSOavgMLOG2[i,1]/C, digits = 3)
  }
}

# Sortieren der Tabelle und bestimmen des ABstands zur nächsten Klasse
resulttableLASSOavgMLOG2=resulttableLASSOavgMLOG2%>%arrange(Share.of.Selections)
for (i in 1:(dim(resulttableLASSOavgMLOG2)[1]-1)){
  resulttableLASSOavgMLOG2[i,2]=resulttableLASSOavgMLOG2[i+1,1]-resulttableLASSOavgMLOG2[i,1]
}


# Runden
resulttableLASSOavgMLOG2[,4]=round(resulttableLASSOavgMLOG2[,4], digits=3)
resulttableLASSOavgMLOG2[,5]=round(resulttableLASSOavgMLOG2[,5], digits=3)
resulttableLASSOavgMLOG2[,6]=round(resulttableLASSOavgMLOG2[,6], digits=3)
write.csv(resulttableLASSOavgMLOG2, "resultcumulative2020_cards.csv")
resulttableLASSOavgMLOG2=read.csv("resultcumulative2020_cards.csv", row.names = 1)
print(resulttableLASSOavgMLOG2)

##########################
##Step 2

# Brant test
finalset=c("MarketValueCompFact","shotAccuracy", "DistanceCoveredComp","SaveRate","Dribbles", "Tackles.Attempted","Passes...Pass.Type...Cross","Passes...Length...Long", "Clearances")
X1=X[,finalset]
source("BrantTest.R")
brant_cr(finalset,FALSE)

#########################
#CV
set.seed(123)
X2=cbind(X1, y1)
samp=sample(seq(1,102), 102, replace = F)
a=split(samp, 1:5)
accuracy.res=double()
for(i in 1:5){
  Xt=NULL
  k=rep(1:5)[-i]
  for(j in 1:length(k)){
    Xt=rbind(Xt, X2[a[[j]],])
  }
  mod=vglm(data=Xt, formula=y1~., family=cumulative(parallel=TRUE, reverse = TRUE))
  summary(mod)
  ab=X2[a[[i]],]
  ab=ab[,-which(colnames(ab)=="y1")]
  pred=predictvglm(mod, ab, type = "response")
  pred1=integer()
  for(k in 1:dim(pred)[1]){
    pred1=append(pred1,(as.integer(names(which.max(pred[k,])))))
  }
  actual=as.integer(X2[a[[i]],]$y1)
  actual=actual-rep(2, length(actual))
  res=data.frame(cbind(pred1, actual))
  tab=table(res$pred1, res$actual)
  print(tab)
  accuracy=(tab[1,1]+tab[2,2]+tab[3,3])/(dim(res)[1])
  print(accuracy)
  accuracy.res=append(accuracy.res, accuracy)
}
mean(accuracy.res)

##################

resulttableLASSOavgMLOG2=resulttableLASSOavgMLOG2%>%arrange(desc(Share.of.Selections))
plot(resulttableLASSOavgMLOG2$Share.of.Selections, type ="s", ylab="Share of selections")
rect(xleft=0, xright = 11, ybottom=0, ytop = 0.02, col= rgb(0,1,0,alpha=0.3), border = rgb(0,1,0,alpha=0.3))
rect(xleft=11, xright = 42, ybottom=0.0, ytop = 0.02, col= rgb(1,0,0,alpha=0.6), border = rgb(1,0,0,alpha=0.6))
