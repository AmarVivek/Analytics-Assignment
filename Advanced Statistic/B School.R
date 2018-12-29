library(car)
library(rattle)
library(MASS)
library("gdata")
library("lawstat")
library("psych")
library("Rcmdr")
library("compute.es")
library("effects")
library("ggplot2")
library("multcomp")
library("pastecs")
library("WRS")
library("nlme")
library("biotools")
library("mvoutlier")
library("mvnormtest")

setwd("C:/Users/Amar/Documents/Data Analytics/Assignment/Advanced Staistic/Group Assignment")

BSchool<-read.table("BSchool.txt", header = TRUE)
BSchoolframe<-data.frame("BSchool")
plot(BSchool[,-1],col=as.factor(BSchool[,2]))
plot( BSchool[,c(7,8,9,14,15)],col=BSchool[,2])

plot( BSchool[,c(7)],col=BSchool[,2])
plot( BSchool[,c(8)],col=BSchool[,2])
plot( BSchool[,c(9)],col=BSchool[,2])
plot( BSchool[,c(14)],col=BSchool[,2])
plot( BSchool[,c(15)],col=BSchool[,2])

BSchooltrain=subset(BSchool,BSchool$ID<=50)
BSchooltest=subset(BSchool,BSchool$ID>50)

#### FActors: B-School Classification

BSchoolClass <- factor(BSchooltrain$B.school, levels=c(1:4),labels=c("A","B","C","D"))

##### Box's M-Test: Equality of variance-covariance matrix. 
####Pooling can be done if the matrices are the same. Otherwise use qda

boxM(BSchooltrain[, c(7,8,9,14,15)], BSchooltrain$Bschool)

####LDA#####

BSchool.lda=lda(BSchooltrain[,c(7,8,9,14,15)],grouping=BSchooltrain[,2])
BSchool.lda
summary(BSchool.lda)
predict(BSchool.lda,c(3.9,710,2,1,4))
df<-predict(BSchool.lda,BSchooltest[,c(7,8,9,14,15)]) ### Predicting for test data#####
BSchooltest<-cbind(BSchooltest,df)
BSchooltest
table(BSchooltest$Bschool,BSchooltest$class)

BSchool.ldacv=lda(BSchooltrain[,c(7,8,9,14,15)],grouping=BSchooltrain[,2],CV=TRUE)

plot(BSchooltrain[,c(7,8,9,14,15)],col=as.factor(BSchooltrain[,2]),pch=as.numeric(BSchool.ldacv$class))

#### First Calcualte within group variances
calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}


#### Calculate within group covariance
calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the covariance of variable 1 and variable 2 for each group:
  Covw <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    # get the covariance for this group:
    term1 <- 0
    for (j in 1:levelilength)
    {
      term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
    }
    Cov_groupi <- term1 # covariance for this group
    Covw <- Covw + Cov_groupi
  }
  totallength <- nrow(variable1)
  Covw <- Covw / (totallength - numlevels)
  return(Covw)
}

groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(variablei)
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new["variablei_name"] <- variablei_new
  }
  return(variables_new)
}

groupstandardisedconcentrations <- groupStandardise(BSchooltrain[c(7,8,9,14,15)], BSchooltrain[2])
