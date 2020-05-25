##Time-Series forecasting using Hybrid Forecast and ARIMA Model


# Packages required for time series analysis and forecasting 

library(timeSeries)
library(zoo)
library(xts)
library(TTR)
library(forecast)
library(fpp2)
library(lubridate)
library(tseries)
library(readxl)
library(dplyr)
library(caTools)
library(TSstudio)
library(forecastHybrid)

setwd("N:/R/directory")

rain <- read_xlsx("rainfall.xlsx")
head(rain)

odisha_rain <- rain[rain$SUBDIVISION == "Orissa",]
odisha_rain

rain_drop <- odisha_rain[-c(1,3:14,16:19)] ## Taking only Odisha Data
head(rain_drop)

summary(rain_drop)

annual_fall <- as.numeric(rain_drop$ANNUAL) ## Converting into numeric verctor

year <- rain_drop$YEAR

rain_df <- cbind.data.frame(year,annual_fall) ## combining both year and Annual Columns

head(rain_df)
summary(rain_df)


# add a normal distribution line in histogram

hist(rain_df$annual_fall, freq=FALSE, col="gray", xlab="Rain fall in inch", main="Colored histogram")
curve(dnorm(x, mean=mean(rain_df$annual_fall), sd=sd(rain_df$annual_fall)), add=TRUE, col="red") #line

## Bell shape curve shows its normally distributed



## Spliting Data into Train and Test

split = sample.split(rain_df$annual_fall, SplitRatio = 0.8)

train <- subset(rain_df,split == TRUE)
test <- subset(rain_df, split == FALSE)

dim(train)
dim(test)

## converting into TimeSeries

train_rain.ts <- ts(train$annual_fall, frequency = 1 , start = c(1901,1)) 
train_rain.ts  

test_rain.ts <- ts(test$annual_fall, freq = 1 , start = c(1994,1))
test_rain.ts

par(mfrow = c(1,1))
plot(train_rain.ts, col = "Blue")

tsoutliers(train_rain.ts)

##Dickey-Fuller Test
adf.test(train$annual_fall, alternative = "stationary")


## ACF & PACF Test 
par(mfrow = c(1,2))
acf(train_rain.ts, lag.max = 10, main = 'ACF to Rainfall')
pacf(train_rain.ts,lag.max = 10, main = 'PACF to Rainfall')


## Forecasting with auto.arima
auto_rain <- auto.arima(train_rain.ts, stepwise=FALSE, approximation=FALSE)
auto_rain
tsdisplay(residuals(auto_rain),lag.max = 10,main = '(0,0,0) check Model Residuals of Auto Arima')
auto_fc <-forecast(auto_rain,h=24)

accuracy(auto_fc)
plot(auto_fc)

checkresiduals(auto_rain)


##Trying with SARIMA
rain.fit <- Arima(train_rain.ts,order = c(0,1,1),
                   seasonal = list(order = c(2,1,0),period= 12))
rain.fit
tsdisplay(residuals(rain.fit),lag.max = 10,main = '(1,1,0) Model Residuals')

fc_fit <- forecast(rain.fit,h=24)
data_fc <- fc_fit$mean

plot(fc_fit)
lines(test_rain.ts, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Test Data","ARIMA Prediction"))

accuracy(data_fc, test_rain.ts)
summary(fc_fit)
checkresiduals(rain.fit)

#p-value = 0.01
#Residuals p-value = 0.1275
#MAPE = 9.623412
#AICc = 1116.7 

#Using Hybrid Forecast model 

hybrid_model <- hybridModel(train_rain.ts,weights = "insample.errors",
                            errorMethod = "RMSE")
hybrid_model

forecast_values <- forecast(hybrid_model, h=24 )
forecast_values

plot(hybrid_model, type = "fit")

