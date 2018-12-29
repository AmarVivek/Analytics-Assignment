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

Magazine<-read.table("Magazines.txt", header = TRUE)
plot(Magazine[,-1],col=as.factor(Magazine[,1]))

ID <- c(1:36)
MagazineCar <- cbind(ID,MagazineCar)
Magazinetrain=subset(Magazine,Magazine$Panelist_ID<=170)
Magazinetest=subset(Magazine,Magazine$Panelist_ID>170)

#### FActors: Magazine Car Classification

MagazineClass <- factor(Magazinetrain$Mag_Subscription, levels=c(0:3),labels=c("Better Homes & Gardens","Reader's Digest","TV Guide","Newsweek"))



##### Box's M-Test: Equality of variance-covariance matrix. 
####Pooling can be done if the matrices are the same. Otherwise use qda

boxM(Magazine[, 3:12], Magazine$Mag_Subscription)

####QDA#####
names(Magazine)
attach(Magazine)
Magazine.manova <- manova(cbind(Family_Size,Income,Race,TVSets,Newspaper_Subscriber,No_Male_Head,No_Female_Head,Children,Age_Of_Head,Education_Head)~as.factor(Mag_Subscription),data=Magazine)
summary.aov(Magazine.manova)
Magazine.qda=qda(Magazinetrain[,cbind("Income","No_Male_Head","Education_Head")],grouping=Magazinetrain[,2])
Magazine.qda
Magazine.lda
summary(MagazineCar.lda)