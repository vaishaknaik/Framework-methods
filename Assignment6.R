houses <-read.table("~/Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/houses2.csv",header=TRUE,sep=",")
View(houses)

#part 1 

#1
library(caret)
?createDataPartition()
set.seed(1031)
split =  createDataPartition(y = houses$price, p = 0.7, list = F, groups = 100)
train = houses[split,]
test = houses[-split,]
mean(train$price)

#2
cor(train[,-15])
names(train)
library(ggcorrplot)
ggcorrplot(cor(train),
           method = 'square',
           type = 'lower',
           show.diag = F,
           colors = c('#e9a3c9', '#f7f7f7', '#a1d76a'))

#3
library(ggcorrplot)
ggcorrplot(cor(train[,c(3:7, 10:13,16)]),type = 'lower',show.diag = F,colors = c('red','white','darkgreen'))

#4
cor(train$sqft_living,train$sqft_above + train$sqft_basement)

#5
# install.packages("carData")
library(car)
model1 <- lm(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data = train)
max(vif(model1))

#part 2

#1 #Best Subset Selection
# install.packages('leaps')
library(leaps)
subsets = regsubsets(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,data=train, nvmax = 6)
summary(subsets)

#2 #R2
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)
subsets_measures

library(ggplot2)
library(tidyr)
subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  ggplot(aes(x=model,y=value))+
  geom_line()+
  geom_point()+
  facet_grid(type~., scales='free_y')

sel <- which.min(summary(subsets)$cp)
sel
coef(subsets, sel)

coef(subsets,which.min(summary(subsets)$cp))

#model_bestsubset
model_bestsubset <- lm(price ~  bedrooms + sqft_living +  waterfront + view + grade + age, data=train)
summary(model_bestsubset)
# See what the model predicts on the training data (in-sample)
coef(model_bestsubset)

predbestsubset <- predict(model_bestsubset)
# Sum of square errors
(ssebestsubset <- sum( (predbestsubset - train$price)^2 ))
# Sum of Sqaures total
(sstbestsubset <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model_bestsubset_r2 <- 1 - ssebestsubset/sstbestsubset)




#3 Forward stepwise selection
start_mod <- lm(price ~ 1, data= train)
empty_mod <- lm(price ~ 1, data= train)
full_mod <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data= train)

forwardstepwise <- step(start_mod, scope = list(upper=full_mod, lower = empty_mod),direction = 'forward')
summary(forwardstepwise)
forwardstepwise

#4 Backward stepwise selection
start_mod <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data= train)
empty_mod <- lm(price ~ 1, data= train)
full_mod <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data= train)

backwardstepwise <- step(start_mod, scope = list(upper=full_mod, lower = empty_mod),direction = 'backward')
summary(backwardstepwise)
backwardstepwise

#5 Hybrid setwise selection
start_mod <- lm(price ~ 1, data= train)
empty_mod <- lm(price ~ 1, data= train)
full_mod <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data= train)

hybridtepwise <- step(start_mod, scope = list(upper=full_mod, lower = empty_mod),direction = 'both')
hybridtepwise
summary(hybridtepwise)

#6 Lasso selection
#install.packages("Matrix")
library(glmnet) 
set.seed(1031)
x = model.matrix(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age,data=train)
y = train$price
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')
plot(cv_lasso)
coef(cv_lasso) #answer non zero

library(dplyr)
cv_lasso$lambda.min
coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)


#7
#model_fetaure_selected_by_lasso
model_fetaure_selected_by_lasso <- lm(price ~  bathrooms + sqft_living +  waterfront + view + grade + age, data=train)
summary(model_fetaure_selected_by_lasso)
# See what the model predicts on the training data (in-sample)
coef(model_fetaure_selected_by_lasso)

pred <- predict(model_fetaure_selected_by_lasso)
# Sum of square errors
(sse <- sum( (pred - train$price)^2 ))
# Sum of Sqaures total
(sst <- sum ( (mean(train$price) - train$price)^2 ))
# R^2
(model_fetaure_selected_by_lasso_r2 <- 1 - sse/sst)

#8 Dimension reduction
library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
ncol(trainComponents)
trainComponents$price = train$price
names(trainComponents)
ncol(trainComponents)

#9
train_model = lm(price~.,trainComponents)
summary(train_model)
predComponents <- predict(train_model)
# Sum of square errors
(sseComponents <- sum( (predComponents - trainComponents$price)^2 ))
# Sum of Sqaures total
(sstComponents <- sum ( (mean(trainComponents$price) - trainComponents$price)^2 ))
# R^2
(trainComponents_r2 <- 1 - sseComponents/sstComponents)


#10
testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price
predComponentstest <- predict(train_model, newdata = testComponents)
# Sum of square errors
(sseComponentstest <- sum( (predComponentstest - testComponents$price)^2 ))
# Sum of Sqaures total
(sstComponentstest <- sum ( (mean(testComponents$price) - testComponents$price)^2 ))
# R^2
(testComponents_r2 <- 1 - sseComponentstest/sstComponentstest)














# library(tidyr); library(dplyr); library(ggplot2)
# corMatrix = as.data.frame(cor(train[]))
# corMatrix$var1 = rownames(corMatrix)

# corMatrix %>%
#   gather(key=var2,value=r,1:11)%>%
#   arrange(var1,desc(var2))%>%
#   ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
#   geom_tile()+
#   geom_text(aes(label=round(r,2)),size=3)+
#   scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
#   theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

