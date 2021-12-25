birthdays = read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/president_birthdays.csv')
heights = read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/president_heights.csv')
states = read.csv('Desktop/Applied Analytics/App_Anlytics_Framework1/Assignments/Framework-methods/president_states.csv')


#1
library(lubridate); 
mdy(birthdays$birthday)
as.Date(birthdays$birthday,format='%m / %d / %Y')

birthdays$stdBirthday <- mdy(birthdays$birthday)
birthdays


#2
library(readr)
library(stringr)
parse_character("    vaishak")
str_trim("   vaishak")
birthdays$stdName <- parse_character(birthdays$Name)
View(birthdays)

#3
library(dplyr)
birthdays %>%
  group_by(month(ymd(birthdays$stdBirthday ))) %>%
  dplyr::summarize(n = n())


#4
heights$stdHeight <- parse_number(heights$height)
mean(heights$stdHeight)
sd(heights$stdHeight)

#5
heights$cmHeight <- heights$stdHeight * 2.54
median(heights$cmHeight)

#6
#std normal form mean 0 sd 1
heights <- heights %>% 
  mutate(chHeight = (stdHeight - mean(stdHeight))/sd(stdHeight))
heights[heights$Name == "James Madison",]

mean(heights$chHeight)
sd(heights$chHeight)
hist(heights$chHeight)

#7
heights$height_cat4 <- cut(heights$stdHeight,
                           breaks=c(0,66,69,72,Inf),
                           include.lowest=TRUE,
                           labels=c("Short","Average","Tall","Very Tall"))
table(heights$height_cat4)
sort(heights$stdHeight)

#8
library(dplyr) 
#install.packages("forcats")
library(forcats)
he <- heights %>%
  mutate(height_cat = fct_recode(.f = height_cat4, "Average" = "Short", "Average" = "Average","Tall" = "Tall","Very Tall" = "Very Tall"))
levels(he$height_cat)
table(he$height_cat)
table(heights$height_cat4)

#9
mergedDataSet <- states %>% inner_join(heights, by = "Name")
mergedDataSet$Birth.State <- parse_character(mergedDataSet$Birth.State)
mean(mergedDataSet[mergedDataSet$Birth.State ==  "New Jersey", 4])


#10

model = paste('model',1:10,sep = '')
r2 = c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp = c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss = c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results = data.frame(model, r2, cp, rss)

results
library(tidyr)
results %>% pivot_longer(cols = 2:4, names_to = 'metric', values_to = 'value')







sort(heights$cmHeight)
mean(heights$stdHeight)
heights$changedHeight <- NULL
heights$changedHeight <- as.numeric(heights$stdHeight - mean(heights$stdHeight))
sum(heights$changedHeight)

heights$changedMean <- scale(heights$stdHeight)
sd(heights$changedMean)
mean(heights$changedMean)
heights$changedMean <- NULL

?scale
apply(scaled.dat, 2, sd)
?skewness
skewness(heights$chHeight)















