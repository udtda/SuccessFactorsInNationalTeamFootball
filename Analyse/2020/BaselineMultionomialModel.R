library(glmnet)
library(nnet)

X=read.csv("Data_clean.csv", header=TRUE, row.names = 1)
setwd('..')
setwd('..')
y1=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.W.L.D
y2=read.csv("WebScraping/2020/Data_complete.csv", header=TRUE, row.names = 1)$Result.GoalDiff
setwd('./Analyse/2020')

varnames=c("Shots", "Passes", "Dribbles", "Tackles attempted", "Interceptions", "Clearances", "Blocks",
           "Offsides", "Fouls", "Aerial duels", "Loss of possession", "Errors","Claims", "Punches", "Shots from open play", 
           "Shots from fastbreak", "Shots from set pieces", "Penalties", "Crosses", "Freekicks", "Corners", "Through balls",
           "Throw ins", "Key passes", "Long passes", "Chipped passes", "Headed passes", "Passes into defensive third", 
           "Passes into final third", "Average age", "Ball possession", "Pass rate", "Tackle rate", "Home advantage", "Travel distance", "wHA", "Shot accuracy", "Save rate", 
           "log(Market value ratio)", "Distance covered difference")

colnames(X)=varnames


model=multinom(data=X, formula = y1~.)
summary(model)

## Cross-Validation des fertigen Modells
library(caret)
set.seed(100)
X3=cbind(X,y1)
train.control = trainControl(method = "cv", number = 5)
modelCV = train(y1 ~., data = X3, method = "multinom", trControl = train.control)
print(modelCV)

