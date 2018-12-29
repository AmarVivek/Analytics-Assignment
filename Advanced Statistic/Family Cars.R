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

FamilyCar<-read.table("family cars.txt", header = TRUE)
plot(FamilyCar[,-4],col=as.factor(FamilyCar[,4]))

ID <- c(1:36)
FamilyCar <- cbind(ID,FamilyCar)
Familytrain=subset(FamilyCar,FamilyCar$ID<=18)
Familytest=subset(FamilyCar,FamilyCar$ID>18)

#### FActors: Family Car Classification

VehicleClass <- as.factor(Familytrain$type_of_vehicle_owned)

FamilyCar$type_of_vehicle_owned <- FamilyCar$type_of_vehicle_owned + 1


##### Box's M-Test: Equality of variance-covariance matrix. 
####Pooling can be done if the matrices are the same. Otherwise use qda

boxM(FamilyCar[, 2:4], FamilyCar$type_of_vehicle_owned)

####QDA#####

FamilyCar.lda=lda(Familytrain[,2:4],grouping=Familytrain[,5])
FamilyCar.lda
summary(FamilyCar.lda)

df<-predict(FamilyCar.lda,Familytest[,2:4]) ### Predicting for test data#####
Familytest<-cbind(Familytest,df)
Familytest
table(Familytest$type_of_vehicle_owned,Familytest$class)
