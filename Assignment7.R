wages <- read.csv("Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/assignment7_wages.csv", stringsAsFactors = T)
#View(wages)

#2
wages = wages[wages$earn>0,]
table(wages$sex)
nrow(wages[wages$sex == "female",])/nrow(wages)

#3
levels(wages$race)
data_race <- wages[wages$race == "hispanic",]["earn"]
mean(data_race$earn)

#4 #5
set.seed(1731)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(wages))
## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(wages)), size = smp_size)

train <- wages[train_ind, ]
test <- wages[-train_ind, ]

mean(train$earn)
mean(train$height)

#Part 2

#1
#significance check
model1 <- lm(earn ~ ., data=train)
modelwithoutsex <- lm(earn ~ height + race + ed + age, data=train)
modelwithoutrace <- lm(earn ~ height + sex + ed + age, data=train)
summary(model1)
coef(model1)
anova(model1,modelwithoutsex, test="LRT")
anova(model1,modelwithoutrace, test="LRT")
anova(model1) #answer
library(lm.beta) 
lm.beta(model1)

#2
pred1 <- predict(model1)
RMSE(pred1, train$earn) #answer
plot(pred1, train$earn)
# Sum of square errors
(sse1 <- sum( (pred1 - train$earn)^2 ))
# Sum of Squares total
(sst1 <- sum ( (mean(train$earn) - train$earn)^2 ))
# R^2
(model1_r2 <- 1 - sse1/sst1)
(rmse1 <- sqrt(mean((pred1- train$earn)^2)))



#3 #4
library(ggplot2)
ggplot(data=train,aes(y=earn,x=ed))+ 
  geom_bar(stat="summary",fun="mean",fill='cadetblue')+
  facet_wrap(~sex)+
  theme_bw()

ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,10000)))+
  theme_bw()

#5 #6
model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)
summary(model_sex_ed)
coef(model_sex_ed)



#7 #8
model2 <- lm(earn ~  height + sex+ race + ed + age + sex*ed, data=train)
pred2 <- predict(model2)
(rmse2 <- sqrt(mean((pred2- train$earn)^2)))

modeltest <- lm(earn ~  . + sex*ed, data=train)
predtest <- predict(modeltest)
(rmsetest <- sqrt(mean((predtest- train$earn)^2)))

#9
model3 <- lm(earn ~  height + sex+ race + ed + age + sex*ed + sex*age, data=train)
pred3 <- predict(model3)
(rmse3 <- sqrt(mean((pred3- train$earn)^2)))

#10
model4 <- lm(earn ~  height + sex+ race + ed + age + sex*ed + sex*age + age*ed, data=train)
pred4 <- predict(model4)
(rmse4 <- sqrt(mean((pred4- train$earn)^2)))

#11
model5 = lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5 <- predict(model5)
(rmse5 <- sqrt(mean((pred5- train$earn)^2)))
# Part-4 1
predtest5 <- predict(model5, newdata = test)
(rmsetest5 <- sqrt(mean((predtest5- test$earn)^2)))

#12
summary(model5)


#part 3

## Tree
#1 #2 #3 #4
library(rpart); library(rpart.plot)
tree1 = rpart(earn~.,data = train, method = 'anova')

prp(tree1,digits=5)   # tree plot method 1
rpart.plot(tree1, digits=5) # tree plot method 2

#5
predt1 <- predict(tree1)
(rmset1 <- sqrt(mean((predt1- train$earn)^2)))
RMSE(predt1, train$earn)

# Part-4 2
predtestt1 <- predict(tree1, newdata = test)
(rmsetestt1 <- sqrt(mean((predtestt1- test$earn)^2)))
RMSE(predtestt1, test$earn)

#6
?rpart.control
treeSimp1 = rpart(earn~.,data=train,control=rpart.control(minbucket=20))
rpart.plot(treeSimp1, digits=5)

#7
predSimp1 <- predict(treeSimp1)
(rmseSimp1 <- sqrt(mean((predSimp1- train$earn)^2)))
predtestSimp1 <- predict(treeSimp1, newdata = test)
(rmsetestSimp1 <- sqrt(mean((predtestSimp1- test$earn)^2)))

#8
treeSimp2 = rpart(earn~.,data=train,control=rpart.control(minbucket=50))
rpart.plot(treeSimp2, digits=5)
#9
predSimp2 <- predict(treeSimp2)
(rmseSimp2 <- sqrt(mean((predSimp2- train$earn)^2)))

# Part-4 3
predtestSimp2 <- predict(treeSimp2, newdata = test)
(rmsetestSimp2 <- sqrt(mean((predtestSimp2- test$earn)^2)))


#10
treeComplex1 = rpart(earn~.,data=train,control=rpart.control(minbucket=5))
rpart.plot(treeComplex1, digits=5)

predComplex1 <- predict(treeComplex1)
(rmseComplex1 <- sqrt(mean((predComplex1- train$earn)^2)))

#11
treeComplex2 = rpart(earn~.,data=train,control=rpart.control(minbucket=1))
rpart.plot(treeComplex2, digits=5)

predComplex2 <- predict(treeComplex2)
(rmseComplex2 <- sqrt(mean((predComplex2- train$earn)^2)))

# Part-4 4
predtestComplex2 <- predict(treeComplex2, newdata = test)
(rmsetestComplex2 <- sqrt(mean((predtestComplex2- test$earn)^2)))

summary(treeComplex2)
 
#12

#compare all train RMSE

#Part-4 5
#compare all test RMSE












