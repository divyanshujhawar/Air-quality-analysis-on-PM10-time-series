
#install.packages("vars")
library(imputeTS)
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(lubridate)
library(astsa)
library(vars)

d <- read.csv("PRSA/PRSA_Data_Aotizhongxin_20130301-20170228.csv")
#summary(d)


sapply(d, function(x) sum(is.na(x)))



data <- na_ma(d, k = 10, weighting = "simple")
sapply(data, function(x) sum(is.na(x)))


NO2 <- data$NO2
SO2 <- data$SO2
CO <- data$CO
O3 <- data$O3
PM2.5 <- data$PM2.5
PM10 <- data$PM10


plot(PM10,type="l",xlab="time")


ggplot(data, aes(x=NO2, y=PM2.5), color="steelblue") + geom_line()


data$date <- as.Date(with(data, paste(year, month, day,sep="-")), "%Y-%m-%d")
