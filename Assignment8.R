library(ISLR)
data(OJ)
View(OJ)
library(caTools)
set.seed(1234)
split <- sample.split(OJ$Purchase, SplitRatio = .70)
train <- OJ[split,]
test <- OJ[!split,]

#part 1

#1
nrow(train)
# View(train)

#2
nrow(train[train$Purchase == "MM",])

#3
mean(train$PriceMM)

#4
mean(train$DiscMM)

#5
nrow(train[train$Purchase == "MM" & train$WeekofPurchase == 275,])


#Part 2 classification tree
#1
library(rpart); library(rpart.plot)
treeModel = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, method = 'class')

library(ROCR)
pred = predict(treeModel,newdata=test)
ROCRpred = prediction(pred[,2],test$Purchase)

auc <- performance(ROCRpred, "auc")@y.values[[1]] #answer
auc

#2
library(caret)
set.seed(100)
trControl = trainControl(method='cv',number = 10)
tuneGrid = expand.grid(.cp = seq(from = 0,to = 0.1,by = 0.001))
cvModel = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                data=train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)

cvModel$bestTune$cp

#3
library(ROCR)
treeModelWithCV = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, 
                        method = 'class', control = rpart.control(cp = cvModel$bestTune$cp))
predWithCV = predict(treeModelWithCV,newdata=test)
ROCRpredWithCV = prediction(predWithCV[,2],test$Purchase)

aucWithCV <- performance(ROCRpredWithCV, "auc")@y.values[[1]]
aucWithCV


#Part 3
#Bag 1
library(ROCR)
library(ipred)
set.seed(617)
bag <- bagging(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train,nbagg = 1000)
predBag <- predict(bag, type = "prob", newdata=test)
ROCRpredBag = prediction(predBag[,2],test$Purchase)
aucBag <- performance(ROCRpredBag, "auc")@y.values[[1]]
round(aucBag,2)

#Bag with Random forest
library(randomForest)
set.seed(617)
bagwithRandomForest <- randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train,mtry = 10,nbagg = 1000)
predBagwithRandomForest<- predict(bagwithRandomForest, type = "prob", newdata=test)
ROCRpredBagwithRandomForest = prediction(predBagwithRandomForest[,2],test$Purchase)
aucBagwithRandomForest <- performance(ROCRpredBagwithRandomForest, "auc")@y.values[[1]]
round(aucBagwithRandomForest,2)

#Random Forest 2
library(randomForest)
set.seed(617)
randomForest <- randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train, nbagg = 1000)
predrandomForest<- predict(randomForest, type = "prob", newdata=test)
ROCRpredrandomForest = prediction(predrandomForest[,2],test$Purchase)
aucrandomFores <- performance(ROCRpredrandomForest, "auc")@y.values[[1]]
round(aucrandomFores,2)


# Boosting using gbm 3
train$Purchase2 = as.numeric(train$Purchase)-1
test$Purchase2 = as.numeric(test$Purchase)-1
library(gbm)
set.seed(617)
boost <- gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,data = train,
             distribution="bernoulli",
             n.trees = 1000,
             interaction.depth = 1,
             shrinkage = 0.04
)

predBoost <- predict(boost, type = "response", newdata = test, n.trees = 100)
ROCRBoost <- prediction(predBoost, test$Purchase2)
aucBoost <- performance(ROCRBoost, "auc")@y.values[[1]]
round(aucBoost,2)





