########### Baseline model

X=read.csv("Data_clean_2016.csv", header=TRUE, row.names = 1)
setwd('..')
setwd('..')
y1=read.csv("WebScraping/2016/Data_complete_2016.csv", header=TRUE, row.names = 1)$Result.W.L.D
y2=read.csv("WebScraping/2016/Data_complete_2016.csv", header=TRUE, row.names = 1)$Result.GoalDiff
setwd('./Analyse/2016')

basemodRB=lm(data=X, y1~.)
basemodGB=lm(data=X, y2~.)

options(scipen=999)
summary(basemodRB)
summary(basemodGB)

# Cross-Validation des fertigen RB Modells
library(caret)
set.seed(100)
X1=cbind(X,y1)
train.control = trainControl(method = "cv", number = 5)
basemodRBcv = train(y1 ~., data = X1, method = "lm",
                    trControl = train.control)
# Summarize the results
print(basemodRBcv)

# Cross-Validation des fertigen GB Modells
library(caret)
set.seed(100)
X2=cbind(X,y2)
train.control = trainControl(method = "cv", number = 5)
basemodGBcv = train(y2 ~., data = X2, method = "lm",
                    trControl = train.control)
# Summarize the results
print(basemodGBcv)
