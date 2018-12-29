library(rpart)
library(rpart.plot)
library(randomForest)
library(Hmisc)
library(caret)

#Set Working Directory
setwd("C:/Users/Amar/Documents/Data Analytics/Assignment/Finance and Risk Analytics/Final")

#Read Training Data <FRA data for R analysis .csv>
train <- read.csv(file.choose(),header=TRUE)

str (train)

#Run various combination of attributes to get best model
lmmodel1A <- Default ~ Net.worth+ PBDITA + PBT + Cash.profit + PBDITA + Deferred.tax.liability +Cumulative.retained.profits+ EPS+Adjusted.EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel1A, data = train) -> lmmodel1B
summary(lmmodel1B)
lmmodel2A <- Default ~ Net.worth+ PBDITA + PBT + Cash.profit + PBDITA +Cumulative.retained.profits+ EPS+Adjusted.EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel2A, data = train) -> lmmodel2B
summary(lmmodel2B)
lmmodel3A <- Default ~ Net.worth+ PBT + Cash.profit + PBDITA +Cumulative.retained.profits+ EPS+Adjusted.EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel3A, data = train) -> lmmodel3B
summary(lmmodel3B)

lmmodel4A <- Default ~ Net.worth+ PBT + Cash.profit + PBDITA +Cumulative.retained.profits+ EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel4A, data = train) -> lmmodel4B
summary(lmmodel4B)
lmmodel5A <- Default ~ Net.worth+ PBT + Cash.profit  +Cumulative.retained.profits+ EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel5A, data = train) -> lmmodel5B
summary(lmmodel5B)
lmmodel6A <- Default ~ Net.worth+ PBT + Cash.profit + PBDITA + EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel6A, data = train) -> lmmodel6B
summary(lmmodel6B)
lmmodel7A <- Default ~ Net.worth + Cash.profit + PBDITA + EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel7A, data = train) -> lmmodel7B
summary(lmmodel7B)
lmmodel8A <- Default ~ Net.worth+ PBT + Cash.profit + EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel8A, data = train) -> lmmodel8B
summary(lmmodel8B)
lmmodel9A <- Default ~ Net.worth + Cash.profit + EPS + Total.liabilities+ PE.on.BSE
lm(lmmodel9A, data = train) -> lmmodel9B
summary(lmmodel9B)

#Read the validation file <validation_data.csv>
test <- read.csv(file.choose(), header=TRUE)

# Run the logistic regression model
model <- glm(lmmodel9A,family=binomial(link='logit'),data=train)

# Fit the results on the training dataset
fitted.results <- predict(model,train,type="response")
summary(fitted.results)

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != train$Default)
print(paste('Accuracy',1-misClasificError))

cbind(fitted.results, train$Default)
confusionMatrix(train$Default, fitted.results)

#Fit the results on the testing dataset
fitted.results <- predict(model,test,type="response")
summary(fitted.results)

fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Classification Error
misClasificError <- mean(fitted.results != test$Default)
print(paste('Accuracy',1-misClasificError))

#COnfusion Matrix
cbind(fitted.results, testDefault)
confusionMatrix(test$Default, fitted.results)
