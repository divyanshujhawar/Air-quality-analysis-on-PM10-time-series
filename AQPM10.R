## Libraries

library(imputeTS)
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(lubridate)
library(astsa)
library(vars)
```

## Reading file....
d <- read.csv("PRSA/PRSA_Data_Aotizhongxin_20130301-20170228.csv")
summary(d)


## Counting NA values
sapply(d, function(x) sum(is.na(x)))



## Filling in missing values
data <- na_ma(d, k = 10, weighting = "simple")
sapply(data, function(x) sum(is.na(x)))



## Storing variables
NO2 <- data_day$NO2
SO2 <- data_day$SO2
CO <- data_day$CO
O3 <- data_day$O3

PM2.5 <- data_day$PM2.5
PM10 <- data_day$PM10




## Plotting the variables wrt time
plot(PM10,type="l",xlab="time")


## GGplot
ggplot(data, aes(x=NO2, y=PM2.5), color="steelblue") + geom_line()



## Converting (year,month,date) -> (y-m-d) format
data$date <- as.Date(with(data, paste(year, month, day,sep="-")), "%Y-%m-%d")


## New column with summary
colnames(data)
summary(data)



## Converting data from hourly format to day format

# Removing unwanted attributs
data <- subset(data, select = -c(No, year,month,day,hour,wd,station))


data_day <- data.frame(aggregate(data,by=data["date"], mean))

data_day <- subset(data_day, select = -c(date.1))

#index(data_day, row = 1:nrow(data_day), col = NULL, value = c())
