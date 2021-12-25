ebay <- read.csv("eBayAssignment.csv", stringsAsFactors = T)
View(ebay)
#Part 1
#1
nrow(ebay)

#2
nrow(ebay[ebay$color == 'Black',])

#3
levels(ebay$productline)

#4
ebay[ebay$startprice == max(ebay$startprice),]['UniqueID']
max(ebay$startprice)

#Part 2
#1
library(caTools)
set.seed(196)
split = sample.split(ebay$sold,SplitRatio = 0.8)
train = ebay[split,]
test = ebay[!split,]
nrow(train)

nrow(train[train$sold == 0,][2])

#2
median(train[train$sold == 1,][2]$startprice)

#3
median(train[train$sold == 0,][2]$startprice)

class(train[train$sold == 0,][2]$startprice)

model1 <- glm(sold ~ biddable+startprice+condition+cellular+carrier+color+
                   storage+productline+ noDescription + charCountDescription +upperCaseDescription+startprice_99end,
                 data=train,
                 family='binomial')
#4
summary(model1)
coef(model1)
summary(model1)$aic

#5 #6 Compare the two models to check the influence of the variable
modeltest <- glm(sold ~ biddable+startprice+condition+cellular+carrier+color+
                   storage+productline+ noDescription + charCountDescription +upperCaseDescription,
                 data=train,
                 family='binomial')
anova(model1, modeltest, test="LRT")



#part 3

#1
model2 <- glm(sold ~ biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end
,data=train,family='binomial')
summary(model2)$aic

#2
coef(model2)

#3
levels(train$productline)
train$productline2 <- factor(train$productline,levels=c("Unknown","iPad 1","iPad 2","iPad 3","iPad 4","iPad Air 1/2","iPad mini","iPad mini 2","iPad mini Retina","iPad mini3"))
model2test <- glm(sold ~ biddable+startprice+condition+storage+productline2+upperCaseDescription+startprice_99end
              ,data=train,family='binomial')
coef(model2test)
#4 startprice goes up by $1, what will be the % reduction in the chance of selling an iPad
100 * (1 - exp(coef(model2)[3]))

#5
exp(coef(model2test)[9])
exp(coef(model2test)[13])/exp(coef(model2test)[9])

#6
model_productline <- glm(sold ~ productline,data=train,family='binomial')
coef(model_productline)
coef(model2)

#part 4

#1 #prediction on test set
library(ROCR)
pred_test <- predict(model2, newdata=test, 
                     type="response")
test_prediction <- data.frame(id = test$UniqueID, price = pred_test)
View(test_prediction)
test_prediction$price[test_prediction$id == 10940]


#2 #Accuracy
ct = table(sold = test$sold, predictions = as.integer(pred_test>0.5)); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy




#3 #4 #Accuracy of based model
prop.table(table(train$sold)) #can know which is the majority class in train sample
sum(test$sold==0)/nrow(test) #accuracy in test sample

#5 #AUC
pred_test <- predict(model2, newdata=test, 
                     type="response")
ROCRpred <- prediction(pred_test, test$sold)
auc <- performance(ROCRpred, "auc")@y.values[[1]]
auc



#extra
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)

plot(ROCRperf,colorize=TRUE,
     print.cutoffs.at=seq(0,2,0.1),
     text.adj=c(-0.3,2),
     xlab="1 - Specificity",ylab="Sensitivity")






