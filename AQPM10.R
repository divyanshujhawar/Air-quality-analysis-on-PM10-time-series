## Libraries

library(imputeTS)
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(lubridate)
library(astsa)
library(vars)


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


# New column with summary
colnames(data)
summary(data)



## Converting data from hourly format to day format

# Removing unwanted attributs
data <- subset(data, select = -c(No, year,month,day,hour,wd,station))

data_day <- data.frame(aggregate(data,by=data["date"], mean))
data_day <- subset(data_day, select = -c(date.1))

       
       
## Plot comparison of between two data-set
plot(data_day$date,data_day$PM10,type='l',col='#FF8F00', main = 'Data with time-stamp of a day');
plot(data$date,data$PM10,type='l',col='steelblue', main = 'Original data with time-stamp of an hour')


## Analyzing trends in the series
ggplot(data_day[1:365,], aes(x = data_day$date[1:365], y= data_day$PM10[1:365])) + geom_point(size=1) + geom_smooth(method = "loess")+
  xlab("Time(2013-2014)") +
  ylab("PM10")


## Dtrending the series
#https://stackoverflow.com/questions/46508919/flatten-or-detrend-a-seasonal-time-series

trend = stl(ts(data_day$PM10, frequency = 365), s.window = "periodic")$time.series[,2]
detrend_ts = data_day$PM10 - (trend - trend[1])

plot(data_day$PM10,type='l')
plot(detrend_ts)

acf(data_day$PM10, lag.max=80)
acf(detrend_ts, lag.max=80)



## Comparison between different years for PM10(this block yet to complete)
xaxis <- 1:365

matplot(xaxis, cbind(data_day$PM10[1:365],data_day$PM10[366:730]),
        type="l",col=c("red","black"),lty=c(1,1))

layout( matrix( c(1,1,1,1), nrow=4, ncol=1, byrow="FALSE"))

N = lengh(data_day$date)
i = 1

while (i < N){

  i = i + 1
}
  

 # 
 # plot(hr,type="l",xlab="time")
 # plot(dar,type="l",xlab="time")

       
## Effect of different input variables on output variables

plot(data_day$NO2,data_day$PM10)
plot(data_day$SO2,data_day$PM10)
plot(data_day$CO,data_day$PM10)
plot(data_day$O3,data_day$PM10)


## Smoothing and filtering
ws1 = 10
ws2 = 20

b1 = rep(1/ws1,ws1)
b2 = rep(1/ws2,ws2)


hrf1 = filter(data_day$PM10,b1,sides=2)
hrf2 = filter(data_day$PM10,b2,sides=2)


#original
plot(data_day$PM10,type="l",xlab="time",main="Original Signal(No smoothing)")

#1 ws = 10
plot(hrf1,type="l",xlab="time",main="Smoothing with window size = 10")

#2 ws = 20
plot(hrf2,type="l",xlab="time",main="Smoothing with window size = 20")


# Next thing could be to analyze the trend on the smoothed graphs
       
       
       

## Autocorrelation

acf1 <- acf(data_day$PM10, lag.max=80, plot = FALSE)
bacf1 <- with(acf1, data.frame(lag, acf))

ggplot(data = bacf1, mapping = aes(x = lag, y = acf)) +
       ggtitle("ACF plot for PM10") +
  theme(plot.title = element_text(hjust = 0.5)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))



#acf2 <- acf(data_day$PM10, lag.max=80, plot = TRUE)


pacf1 <- pacf(data_day$PM10, lag.max=80, plot = FALSE)
bacf2 <- with(pacf1, data.frame(lag, acf))

ggplot(data = bacf2, mapping = aes(x = lag, y = acf)) +
       ggtitle("PACF plot for PM10") +
  theme(plot.title = element_text(hjust = 0.5)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))






## Differencing Autocorrelation

t1 <- diff(data_day$PM10)
acf1 <- acf(t1, lag.max=80, plot = FALSE)
bacf1 <- with(acf1, data.frame(lag, acf))

ggplot(data = bacf1, mapping = aes(x = lag, y = acf)) +
       ggtitle("ACF plot for Differenced PM10") +
  theme(plot.title = element_text(hjust = 0.5)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))



#acf2 <- acf(data_day$PM10, lag.max=80, plot = TRUE)


pacf1 <- pacf(t1, lag.max=80, plot = FALSE)
bacf2 <- with(pacf1, data.frame(lag, acf))

ggplot(data = bacf2, mapping = aes(x = lag, y = acf)) +
       ggtitle("PACF plot for PM10") +
  theme(plot.title = element_text(hjust = 0.5)) +
       geom_hline(aes(yintercept = 0)) +
       geom_segment(mapping = aes(xend = lag, yend = 0))


#pacf(data_day$PM10, lag.max=80, plot = TRUE)

Box.test(t1, lag=10, type="Ljung-Box")




## Dtrending autocorrelation
require(pracma)
y1 <- detrend(data_day$PM10)

acf(data_day$PM10, lag.max = 80)
acf(y1,lag.max = 80)

Box.test(y1, lag=10, type="Ljung-Box")

