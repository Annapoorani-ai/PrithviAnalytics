
install.packages("tidyverse")
library(tidyverse)

setwd('E:/week3desstat')
#Upload Uber dataset csv file after setting your working directory
uber = read.csv("Uber .csv")

#To find no. of rows and columns in the dataset
dim(uber)

#To inspect top and bottom six rows
head(uber)
tail(uber)

#To study and find data type of variables and their five point summary
str(uber)
summary(uber)

# Almost all borough have identical distribution, few NA's are observed
# pickup shows possibility of outliers
#visibility of 0 shows extreme conditions, but cannot be ruled out
# temparatures are in farenheit so given range of 2 to 89 translates roughly -16 to 31 celsius

#To find NAs in the dataset
anyNA(uber)
sapply(uber, function(x) sum(is.na(x)))

# Mark NA as separate category and renaming them as "Unknown"
uber$borough = as.factor(replace(as.character(uber$borough), is.na(uber$borough),"Unknown"))
plot(aggregate(pickups~borough,data=uber, sum), type="b")

#To inspect the propotions of diferent areas
table(uber$borough) #notice all areas have equally represented excluding Unknowns

# Let us break date into hour, day, month
#study strptime function..advance functions for treating time stamp variable
?strptime
uber$start_date = strptime(uber$pickup_dt,'%Y-%m-%d %H:%M')

library(lubridate) #Lubridate is an R package that makes it easier to work with dates and times
uber$start_month = month(uber$start_date)
uber$start_day = day(uber$start_date)
uber$start_hour = hour(uber$start_date)
uber$wday = weekdays(uber$start_date)
uber = uber[,-14]

# try to get no of holidays in each month
#unique function used to keep only unique/distinct rows from a data frame
unique(uber[which(uber$hday=="Y"),c("start_day","start_month")])
# two holidays in Jan, feb and may while one in jun

table(uber$hday,uber$start_month)
# No trips in month 3 and 4. Looks like no holidays in these month

library(data.table) #widely used for fast aggregation of large datasets
?uniqueN
uniqueN(uber, by=c('start_month', 'start_day'))
# 181 days of data

plot(aggregate(pickups~hday,data=uber, mean), type="b")

#boxplot for wind speed variable
names(uber)
boxplot(uber$spd) # outliers present
hist(uber$spd)# skewed histogram

library(ggplot2) #a system for declaratively creating graphics
#for pick up counts
ggplot(uber, aes(pickups)) +
  geom_histogram(binwidth = 200)
# Histogram is heavily skewed

#Borough wise pickup
ggplot(uber, aes(pickups)) +
      geom_histogram() +
      facet_wrap(~ borough, ncol = 3)

uber %>% group_by(borough) %>% 
  summarise(`Total Pickups` = sum(pickups)) %>% 
  arrange(desc(`Total Pickups`))
# Majority of 0 rides are in unknown, staten island, EWR and Bronx
#Manhattan seems to have highest demand and then brooklyn

hist(uber$spd)
# Low speed for duration, except few outliers, avg is around 5

boxplot(uber[,c(4:7)])
boxplot(uber[,c(9:12)])

hist(uber$vsb, main= "Visibility")
# Almost clear weather 

hist(uber$temp, main="Temperature")
# Two peaks culd be seen around 35 and second around 60

plot(density(uber$dewp), main="Dew point variations")
# Distribution looks similar to that of temperature (bi-modal)

plot(density(uber$slp), main="Sea level pressure")
# Approximately normaldistribution
# Sea level, dew point(humidity), speed all would be related to temperatures

plot(density(uber$sd), main="Snow depth")
# No snow for majority of times

