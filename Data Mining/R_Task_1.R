

## Let us first set the working directory path
setwd ("C:/Users/rahul/Downloads")
getwd()


## Ideally for any modeling you should have Training & Testing dataset
## Typically you would use sampling strategy
## However for the Neural Net training I am supplying the Training & Testing data separately


##training dataset
nn.dev <- read.table("DEV_SAMPLE.csv", sep = ",", header = T)

##testing dataset
nn.holdout <- read.table("HOLDOUT_SAMPLE.csv", sep = ",", header = T)

str(nn.dev)

#library(caret)
#dummies <- predict(dummyVars(~ Gender , data = nn.dev), newdata = nn.dev)

str(nn.dev)
str(nn.holdout)
occ.dev <- model.matrix(~ Occupation - 1, data = nn.dev)
nn.dev <- data.frame(nn.dev, occ.dev)

Gender.dev <- model.matrix(~ Gender - 1, data = nn.dev)
nn.dev <- data.frame(nn.dev, Gender.dev)

AGE_BKT.dev <- model.matrix(~ AGE_BKT - 1, data = nn.dev)
nn.dev <- data.frame(nn.dev, AGE_BKT.dev)

occ.holdout <- model.matrix(~ Occupation - 1, data = nn.holdout)
nn.holdout <- data.frame(nn.holdout, occ.holdout)

Gender.holdout <- model.matrix(~ Gender - 1, data = nn.holdout)
nn.holdout <- data.frame(nn.holdout, Gender.holdout)

AGE_BKT.holdout <- model.matrix(~ AGE_BKT - 1, data = nn.holdout)
nn.holdout <- data.frame(nn.holdout, AGE_BKT.holdout)

str(nn.dev)
str(nn.holdout)

##final dataset 
nn.dev <- subset(nn.dev, select= -c(Cust_ID,Gender,Occupation,AGE_BKT))
nn.holdout <- subset(nn.holdout, select= -c(Gender,Occupation,AGE_BKT,Cust_ID))


#View(nn.holdout)
c(nrow(nn.dev), nrow(nn.holdout))
str(nn.dev)

## Response Rate
sum(nn.dev$Target) / nrow(nn.dev)   #in dev sample
sum(nn.holdout$Target) / nrow(nn.holdout)  ##in holdout sample


## Installing the Neural Net package; 

##install.packages("neuralnet")


library(neuralnet)
#?"neuralnet"

##adding names of dummy variable 
colnames(nn.dev)
col_list <- paste(c(colnames(nn.dev[,-c(1,21)])),collapse="+")
col_list <- paste(c("Target~",col_list),collapse="")
f <- formula(col_list)


x <- subset(nn.dev, select = -c(Target))

nn.devscaled <- scale(x)
nn.devscaled <- cbind(nn.dev[1], nn.devscaled)
View(nn.devscaled)

# Creating the neural model including the dummy variables
nn2 <- neuralnet(f, 
                 data = nn.devscaled, 
                 hidden = 3,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 1,
                 threshold = 0.1,
                 stepmax = 2000
)

plot(nn2)

## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn2$net.result[[1]] 
View(nn.dev)

## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(nn.dev$Prob)

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
nn.dev$deciles <- decile(nn.dev$Prob)

## Ranking code
library(data.table)

tmp_DT = data.table(nn.dev)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

## Assgining 0 / 1 class based on certain threshold
nn.dev$Class = ifelse(nn.dev$Prob>0.21,1,0)
with( nn.dev, table(Target, as.factor(Class)  ))  ## evaluating Target vs Class 

## We can use the confusionMatrix function of the caret package 
install.packages("caret")
library(caret)
confusionMatrix(nn.dev$Target, nn.dev$Class)


## Error Computation
MSE.nn <- sum((nn.dev$Target - nn.dev$Prob)^2)/2
MSE.nn


## Other Model Performance Measures

library(ROCR)
pred <- prediction(nn.dev$Prob, nn.dev$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])


auc <- performance(pred,"auc"); # Area under the curve
auc <- as.numeric(auc@y.values)

install.packages("ineq")
library(ineq)
gini = ineq(nn.dev$Prob, type="Gini")


auc
KS
gini



## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
View(nn.holdout)
y <- subset(nn.holdout, select = -c(Target))
y.scaled <- scale(y)
compute.output = neuralnet::compute(nn2, y.scaled)
nn.holdout$Predict.score <- compute.output$net.result

View(nn.holdout)

quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
nn.holdout$deciles <- decile(nn.holdout$Predict.score)

library(data.table)
tmp_DT = data.table(nn.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)


DT<- data.table(rank$ks,h_rank$ks)
setnames(DT,c("KS_dat","KS_holdout"))
View(DT)
