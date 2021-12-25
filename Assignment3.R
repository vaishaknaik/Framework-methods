library(ISLR)
library(caret)
library(caTools)


data(OJ)
data(Sacramento)
#View(OJ)
summary(OJ)

#6
set.seed(1706)
split = sample(x = 1:nrow(Sacramento),size = 0.8*nrow(Sacramento))
train = Sacramento[split,]
test = Sacramento[-split,]
abs(mean(train$price) - mean(test$price)) #answer

length(split)/nrow(Sacramento)
length(1:nrow(Sacramento))
nrow(train)
nrow(test)
mean(train$price);mean(test$price)
abs(mean(train$price) - mean(test$price))

#7
set.seed(1706)
split2 = createDataPartition(y = Sacramento$price, p = 0.8, list = F, groups = 50)
train2 = Sacramento[split2,]
test2 = Sacramento[-split2,]
abs(mean(train2$price) - mean(test2$price)) #answer

length(1:nrow(Sacramento))
nrow(train2)
nrow(test2)
length(split2)/nrow(Sacramento)
mean(train2$price);mean(test2$price)


#8
set.seed(1706)
split3 = sample.split(Y = OJ$Purchase, SplitRatio = 0.8)
train3 = OJ[split3,]
test3 = OJ[!split3,]
mm <- train3[train3$Purchase == "MM",]
nrow(mm)/nrow(train3) #answer

table(split3)
nrow(train3)
nrow(test3)
length(1:nrow(OJ))
train3$Purchase
table(train3$Purchase)
table(test3$Purchase)
334/nrow(train3)


