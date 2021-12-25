##simple random splitting
library(caret)
data(Sacramento)
set.seed(1706)
split = sample(x = 1:nrow(Sacramento),size = 0.8*nrow(Sacramento))
train = Sacramento[split,]
test = Sacramento[-split,]
abs(mean(train$price) - mean(test$price))


##stratified sampling createDataPartition
set.seed(1706)
split2 = createDataPartition(y = Sacramento$price, p = 0.8, list = F, groups = 50)
train2 = Sacramento[split2,]
test2 = Sacramento[-split2,]
abs(mean(train2$price) - mean(test2$price))

##stratified sampling ample.split
library(ISLR)
data(OJ)
set.seed(1706)
split3 = sample.split(Y = OJ$Purchase, SplitRatio = 0.8)
train3 = OJ[split3,]
test3 = OJ[!split3,]
mm <- train3[train3$Purchase == "MM",]
nrow(mm)/nrow(train3) #answer

##correlation code
cor(train$sqft_living, train$price)

##linear regression model
houses <- read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/houses.csv', stringsAsFactors = TRUE)
set.seed(1031)
split <- createDataPartition(y=houses$price, p=0.7, list=FALSE, group=100)
train <- houses[split,]
test  <- houses[-split,]

model4 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data=train)
summary(model4)
coef(model4)
#prediction on train data
pred4 <- predict(model4)
# Sum of square errors
(sse4 <- sum( (pred4 - train$price)^2 ))
# Sum of Sqaures total
(sst4 <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model4_r2 <- 1 - sse4/sst4)
#RMSE
(rmse4 <- sqrt(mean((pred4 - train$price)^2)))
RMSE(pred4,  train$price)


#Prediction on test data
pred_test <- predict(model4, newdata = test)
# Sum of square errors
(sse_test <- sum((pred_test - test$price)^2))
# Sum of Sqaures total
(sst_test <- sum((mean(train$price) - test$price)^2))
#R^2
(model4_r2_test <- 1 - sse_test/sst_test)
#RMSE
(rmse_test <- sqrt(mean((pred_test-test$price)^2)))
#Us this to know the influence of the variable f-value (heighest)
anova(model4)
library(lm.beta) 
lm.beta(model4) #use this



## to check direction of the trend
library(ggplot2)
# Basic scatter plot
ggplot(houses, aes(x=sqft_living, y=price)) + geom_point() +
  geom_point(size=2, shape=23)

ggplot(aes(x=sqft_living, y=price), data=train) +
  geom_point() +
  geom_smooth(method="lm", size=1.3, color='steelblue3', se=FALSE)


##Logistic regression
ebay <- read.csv("Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/eBayAssignment.csv", stringsAsFactors = T)
library(caTools)
set.seed(196)
split = sample.split(ebay$sold,SplitRatio = 0.8)
train = ebay[split,]
test = ebay[!split,]
nrow(train)

model1 <- glm(sold ~ biddable+startprice+condition+cellular+carrier+color+
                storage+productline+ noDescription + charCountDescription +upperCaseDescription+startprice_99end,
              data=train,
              family='binomial')

summary(model1)
coef(model1)
summary(model1)$aic

#Compare the two models to check the influence of the variable
modeltest <- glm(sold ~ biddable+startprice+condition+cellular+carrier+color+
                   storage+productline+ noDescription + charCountDescription +upperCaseDescription,
                 data=train,
                 family='binomial')
anova(model1, modeltest, test="LRT")



model2 <- glm(sold ~ biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end
              ,data=train,family='binomial')
summary(model2)$aic
# Start price goes up by $1, what will be the % reduction in the chance of selling an iPad
100 * (1 - exp(coef(model2)[3]))


exp(coef(model2test)[9])
exp(coef(model2test)[13])/exp(coef(model2test)[9])


#prediction on test set
library(ROCR)
pred_test <- predict(model2, newdata=test, 
                     type="response")
test_prediction <- data.frame(id = test$UniqueID, price = pred_test)
View(test_prediction)
test_prediction$price[test_prediction$id == 10940]

#Accuracy
ct = table(sold = test$sold, predictions = as.integer(pred_test>0.5)); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy


#Accuracy of base model
prop.table(table(train$sold)) #can know which is the majority class in train sample
sum(test$sold==0)/nrow(test) #accuracy in test sample

#AUC
pred_test <- predict(model2, newdata=test, 
                     type="response")
ROCRpred <- prediction(pred_test, test$sold)
auc <- performance(ROCRpred, "auc")@y.values[[1]]
auc

#Rearrange the level and check the affect of each level on 1st level of a catagoriacl variable.
levels(train$productline)
train$productline2 <- factor(train$productline,levels=c("Unknown","iPad 1","iPad 2","iPad 3","iPad 4","iPad Air 1/2","iPad mini","iPad mini 2","iPad mini Retina","iPad mini3"))
model2test <- glm(sold ~ biddable+startprice+condition+storage+productline2+upperCaseDescription+startprice_99end
                  ,data=train,family='binomial')
coef(model2test)

##SVM

data <- read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Lecture 10/winequality-white-1.csv',sep=";")
data$quality <- factor(ifelse(data$quality>mean(data$quality), 1, 0),labels = c('high','low'))

library(caTools)
set.seed(1706)
split <- sample.split(data$quality,SplitRatio = 0.7)
train <- data[split,]
test <- data[!split,]

head(train)

## Linear SVM
svmLinearTune <- tune(method = svm,
                      quality~alcohol+volatile.acidity,
                      data=train,kernel='linear',
                      type='C-classification', 
                      ranges = list(cost=c(0.1,1,10,100)))
summary(svmLinearTune)
pred_svmlineartuned <- predict(svmLinearTune$best.model,
                               newdata=test)

svmPolynomialTune <- tune(method = svm,quality~alcohol+volatile.acidity,
                          data=train, kernel='polynomial',
                          ranges=list(cost=c(0.01,1,10),
                                      degree=c(2,3)))
summary(svmPolynomialTune$best.model)

pred_svmpolytuned <- predict(svmPolynomialTune$best.model,
                             newdata=test)

table(pred_svmlineartuned, test$quality)
table(pred_svmpolytuned, test$quality)

plot(svmPolynomialTune$best.model, test, formula=alcohol~volatile.acidity)

svm_polyrefit <- svm(quality~alcohol+volatile.acidity,
                     type='C-classification',
                     data=train, kernel='polynomial', cost=10,
                     degree=3)

plot(svm_polyrefit, train, formula=alcohol~volatile.acidity)


