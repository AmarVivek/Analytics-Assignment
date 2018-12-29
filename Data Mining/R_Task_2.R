setwd ("C:/Users/Amar/Documents/Data Analytics/E-book/in-Class Material/Data Mining/Residency 7")
getwd()


## Let us import the data that we need to perform the Market Basket Analysis

RTxn <- read.table("Market_Basket_Analysis.csv", sep = ",", header = T)
nrow(RTxn)

View(RTxn)

str(RTxn)
RTxn$Invoice_No <- as.factor(RTxn$Invoice_No)

## Aggregating the Invoices at Transaction Level
## We want one row per transaction. 
## The one row should have details of all the products purchased in that transaction
?split
Agg.RTxn <- split(RTxn$Item_Desc,RTxn$Invoice_No)
class(Agg.RTxn)
Agg.RTxn

## To see specific row number transaction
Agg.RTxn[3]


install.packages("arules")
library(arules)

Agg.RTxn_DD <- list()
for (i in 1:length(Agg.RTxn)) {
  Agg.RTxn_DD[[i]] <- as.character(Agg.RTxn[[i]][!duplicated(Agg.RTxn[[i]])])
}

Txns <- as(Agg.RTxn_DD,"transactions")
Txns
summary(Txns)

inspect(Txns[c(1:10)])


## Let us see the support
freq <- itemFrequency(Txns)
class(freq)
freq <- freq[order(-freq)]

freq["Bread"]

barplot( freq[1:20] )

?itemFrequencyPlot
itemFrequencyPlot(Txns, support = 0.10)
itemFrequencyPlot(Txns, topN = 10)


install.packages("arulesViz")
library("arulesViz")
?apriori
arules1 <- apriori(data = Txns)
summary(arules1)

## See the Association Rules
inspect(arules1)

inspect(sort(arules1, by = "lift") )


arules2 <- apriori(data = Txns, 
                   parameter = list(support = 0.05, confidence = 0.5, maxlen = 2 )
)

summary(arules2)
##inspect(arules2)
inspect(sort(arules2, by = "lift")[1:30] )



#Scatter plot of rules:
library("RColorBrewer")
plot(arules2,control=list(
  col=brewer.pal(11,"Spectral")),
  main="Association Rules Plot"
)

#Rules with high lift typically have low support.


## Plot Interactivee Graphs
subrules2 <- head(sort(arules2, by="support"), 20)
plot(subrules2, method="grouped" , interactive=TRUE )

#Plot graph-based visualisation:
plot(subrules2, method="graph",control=list(type="items",main=""))
inspect(subrules2)

plot(subrules2, method="grouped" , interactive=TRUE )

rules_df <- as(arules2,"data.frame")
rules_df$lhs_suuport <- rules_df$support / rules_df$confidence;
rules_df$rhs_support <- rules_df$confidence / rules_df$lift;
View(rules_df)
write.table(rules_df, file = "mba_output.csv", sep = "," , append = F, row.names = F)
unlink("mba_output.csv")

#######################################################
## Running Market Basket after combining a few items:##
#######################################################


RTxn1 <- read.table("Market_Basket_Analysis_V1.csv", sep = ",", header = T)
nrow(RTxn1)

RTxn1$Invoice_No <- as.factor(RTxn1$Invoice_No)

## Aggregating the Invoices at Transaction Level
## We want one row per transaction. 
## The one row should have details of all the products purchased in that transaction
?split
Agg.RTxn1 <- split(RTxn1$Item_Desc,RTxn1$Invoice_No)
class(Agg.RTxn1)
Agg.RTxn1

library(arules)

Agg.RTxn_DD <- list()
for (i in 1:length(Agg.RTxn1)) {
  Agg.RTxn_DD[[i]] <- as.character(Agg.RTxn1[[i]][!duplicated(Agg.RTxn1[[i]])])
}

Txns <- as(Agg.RTxn_DD,"transactions")
Txns
summary(Txns)

inspect(Txns[c(1:10)])


## Let us see the support
freq <- itemFrequency(Txns)
class(freq)
freq <- freq[order(-freq)]

freq["Bread"]

barplot( freq[1:20] )

?itemFrequencyPlot
itemFrequencyPlot(Txns, support = 0.10)
itemFrequencyPlot(Txns, topN = 10)

library("arulesViz")
?apriori
arules1 <- apriori(data = Txns)
summary(arules1)

## See the Association Rules
inspect(arules1)

inspect(sort(arules1, by = "lift") )


arules2 <- apriori(data = Txns, 
                   parameter = list(support = 0.05, confidence = 0.5, maxlen = 2 )
)

summary(arules2)
##inspect(arules2)
inspect(sort(arules2, by = "lift")[1:30] )



#Scatter plot of rules:
library("RColorBrewer")
plot(arules2,control=list(
  col=brewer.pal(11,"Spectral")),
  main="Association Rules Plot"
)

#Rules with high lift typically have low support.


## Plot Interactivee Graphs
subrules2 <- head(sort(arules2, by="support"), 20)
plot(subrules2, method="grouped" , interactive=TRUE )

#Plot graph-based visualisation:
plot(subrules2, method="graph",control=list(type="items",main=""))
inspect(subrules2)

plot(subrules2, method="grouped" , interactive=TRUE )

rules_df <- as(arules2,"data.frame")
rules_df$lhs_suuport <- rules_df$support / rules_df$confidence;
rules_df$rhs_support <- rules_df$confidence / rules_df$lift;
View(rules_df)
write.table(rules_df, file = "mba_output1.csv", sep = "," , append = F, row.names = F)
unlink("mba_output1.csv")
