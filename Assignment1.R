#Assignment 1
#1
123456*456789

#2
?log2()
log2(33554432)
log(33554432,2)

#3
cos(0)

#4
?rnorm()
length(rnorm(n=2021,mean = 10,sd = 5))
str(rnorm(n=2021,mean = 10,sd = 5))

#5
class(7)

#6
class('seven')

#7
class(2021 != 2020)

#8
class(23 * 37 + 67 * T)


#9
str(c("Am I really smart", TRUE, 1000))


#10
c(10,20,30,40) > 15


#11
c(10,20,30,40) > c(15,25,35,45)


#12
c(10,20,30,40) > c(15,25)

#part 2


day_of_week = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
number_of_smartwatches_sold = c(10,20,15,20,30,80,90)
price_per_smartwatch = c(200,200,200,200,200,150,150)
df = data.frame(day_of_week,number_of_smartwatches_sold,price_per_smartwatch)

#1
sum(df$number_of_smartwatches_sold)

#2
df[df$day_of_week=='Sunday','price_per_smartwatch']
df[df$day_of_week=='Sunday',3]
df$price_per_smartwatch[df$day_of_week=='Sunday']
df$day_of_week=='Sunday'

#3
df$number_of_smartwatches_sold* df$price_per_smartwatch
sum(df$number_of_smartwatches_sold* df$price_per_smartwatch)

#4
df$number_of_smartwatches_sold > 25
sum(df$number_of_smartwatches_sold > 25)

#5
price_per_smartwatch[c(6,7)] 
price_per_smartwatch[6 :7]


#6
mean(df$number_of_smartwatches_sold)
df$day_of_week[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)]

df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold)

df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),"day_of_week"]

df[df$number_of_smartwatches_sold>mean(df$number_of_smartwatches_sold),1]

df[df$number_of_smartwatches_sold>min(df$number_of_smartwatches_sold),1]

#part 3

# install.packages('ggplot2')
library(ggplot2)
library(readr)
library(dplyr)
diamondsData <- read_csv("Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/diamonds_data.csv")
View(diamondsData)

#1
mean(diamondsData$carat)

#2
?dplyr::filter()
filteredData <- dplyr::filter(diamondsData,grepl('Premium', cut))
filteredData
mean(filteredData$carat)

#4
testvar <- diamondsData %>%
  group_by(cut) %>%
  dplyr::summarize(varr = var(carat))
testvar
testvar[testvar$varr == max(testvar$varr),1]



#5
mean(diamondsData$price[diamondsData$carat > 2] * 0.85)

#6
ggplot(diamondsData, aes(x=price)) + 
  geom_density() + facet_grid(cut~.)

ggplot(diamondsData, aes(x=price, color = cut)) + 
  geom_density()

#7
ggplot(data=diamondsData,aes(x=carat ))+ 
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim=c(0,2.5))+
  scale_x_continuous(breaks=seq(0,2.5,0.1))












# 
# diamondsData %>%
#   group_by(cut) %>% filter(color == "G") %>%
#   dplyr::summarize(sum_cut = n())
# 
# str(diamondsData %>%
#   filter(diamondsData$carat > 2) %>%
#   dplyr::summarize(avg_price = mean(diamondsData$price))) * 0.85

test1 <- diamondsData  %>%
  dplyr::summarize(euro =  diamondsData$price * 0.85) %>%
  dplyr::filter(as.numeric(diamondsData$carat) > 2)  %>%
  dplyr::summarize(mean(euro))

str(test1)
diamondsData
class(diamondsData$carat)
mean(diamondsData[diamondsData$carat > 2, 7])
 filteredPriceData <- diamondsData[diamondsData$carat > 2, 7]
 mean((filteredPriceData$price * 0.85)) 
euro_test <- diamondsData %>%
  filter(diamondsData$carat > 2) %>%
  dplyr::summarize(euro =  diamondsData$price * 0.85)
diamondsData

mean(euro_test$euro)
mean(diamondsData$price)

summary(euro)
euro_test





ggplot(data=diamondsData,aes(x=carat))+ 
  geom_histogram()

