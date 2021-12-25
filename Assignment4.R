houses <- read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/houses.csv', stringsAsFactors = TRUE)
#View(houses)
library(caret)

#part 1

#1
set.seed(1031)
split <- createDataPartition(y=houses$price, p=0.7, list=FALSE, group=100)
train <- houses[split,]
test  <- houses[-split,]
mean(train$price)

#2
mean(test$price)

#3
library(dplyr); library(tidyr)
train %>%
  select(id,price:sqft_lot,age)%>%
  pivot_longer(price:age, names_to='numericVariable',values_to='value')%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales="free_y")
# View(train)
train$sqft_living[which.max(train$bedrooms)]

#4
library(ggplot2)
# Basic scatter plot
ggplot(houses, aes(x=sqft_living, y=price)) + geom_point() +
  geom_point(size=2, shape=23)

ggplot(aes(x=sqft_living, y=price), data=train) +
  geom_point() +
  geom_smooth(method="lm", size=1.3, color='steelblue3', se=FALSE)

#5
#correlation between sqft_living and price
?cor
cor(train$sqft_living, train$price)


#Part 2

#1
#model1
model1 <- lm(price ~ sqft_living, data=train)
# summarize - t-statisitc # F-test
summary(model1) #answer 1 & 2
anova(model1)



# See what the model predicts on the training data (in-sample)

pred <- coef(model1)[1] + coef(model1)[2]*train$sqft_living
pred <- predict(model1)

plot(pred, train$price)
abline(0,1)
?abline

#2
# Sum of square errors
(sse1 <- sum( (pred - train$price)^2 ))
# Sum of Sqaures total
(sst1 <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model1_r2 <- 1 - sse1/sst1)
# RMSE

#3
(rmse1 <- sqrt(mean((pred - train$price)^2)))
RMSE(pred, train$price)

#4
coef(model1)

#5
# better
predict(model1, newdata=data.frame(sqft_living=1400))

#6
200*coef(model1)[2]


#7 #8 #9
#model2
model2 <- lm(price ~ waterfront, data=train)
summary(model2)
# See what the model predicts on the training data (in-sample)
coef(model2)
pred2 <- coef(model2)[1] + coef(model2)[2]*train$waterfront
pred2 <- predict(model2)
plot(pred2, train$price)
# Sum of square errors
(sse2 <- sum( (pred2 - train$price)^2 ))
# Sum of Sqaures total
(sst2 <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model2_r2 <- 1 - sse2/sst2)

(rmse2 <- sqrt(mean((pred2- train$price)^2)))
anova(model2)


#part3

#1
#model3
model3 <- lm(price ~ sqft_living + waterfront, data=train)
summary(model3)
# See what the model predicts on the training data (in-sample)

#2S
coef(model3)
pred3 <- predict(model3)
plot(pred3, train$price)
# Sum of square errors
(sse3 <- sum( (pred3 - train$price)^2 ))
# Sum of Sqaures total
(sst3 <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model3_r2 <- 1 - sse3/sst3)
#RMSE
(rmse3 <- sqrt(mean((pred3- train$price)^2)))

#3 #4 #5 #6 
#model4
model4 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data=train)
summary(model4)
# See what the model predicts on the training data (in-sample)

#7
coef(model4)

#8
#install.packages("lm.beta")
library(lm.beta)
?lm.beta
lm.beta(model4)
anova(model4)

pred4 <- predict(model4)
plot(pred4, train$price)
abline(0,1)
# Sum of square errors
(sse4 <- sum( (pred4 - train$price)^2 ))
# Sum of Sqaures total
(sst4 <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model4_r2 <- 1 - sse4/sst4)
(rmse4 <- sqrt(mean((pred4 - train$price)^2)))

1*coef(model4)[2]
model4$coefficients


## Out of sample predictions

pred_test <- predict(model4, newdata = test)

(sse_test <- sum((pred_test - test$price)^2))
(sst_test <- sum((mean(train$price) - test$price)^2))

#9
(model4_r2_test <- 1 - sse_test/sst_test)

#10
(rmse_test <- sqrt(mean((pred_test-test$price)^2))); rmse_test

(rmse_train <- sqrt(mean((predict(model4)-train$price)^2)))


summary(model4)
#names(summary(model1))
#model1_summary <- summary(model1)
#model1_summary$fstatistic


















