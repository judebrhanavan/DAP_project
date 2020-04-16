# Forecasting DailyHDD
# Ref : https://otexts.com/fpp2/arima-r.html, https://otexts.com/fpp2/seasonal-arima.html

#CHAGE ME : folder where the exported csv files are saved
setwd("C:/Users/AAJB/Desktop/MSc Data Analytics/Database and Analysis Programming/python/DAP_project")
options(scipen=999)  # suppress scientic notation 

# load necessary packages
library(fpp2)
library(forecast)
library(ggplot2)

# read csv file
data <- read.csv('dailyhdd.csv')

#create time series
hdd_ts <- ts(data[,3], start = c(2009,311), frequency = 365.25)
end(hdd_ts)
# plot the timeseries 
autoplot(hdd_ts) + xlab('Year') + ylab('Heating Energy Consumption')

# trainng & test set
trainingsetsize = length(hdd_ts) - 182 # test data would be last 6 months observations
train_hdd <- subset(hdd_ts, end=trainingsetsize)
test_hdd <- subset (hdd_ts, start = trainingsetsize+1)

## fit ARIMA
fit_arima = auto.arima(train_hdd)
summary(fit_arima)
fc_arima = forecast(fit_arima, h =182)
checkresiduals(fc_arima)
# residuals fail the white noise test 

# but the point forecasts are good
autoplot(fc_arima, PI=FALSE) + autolayer(test_hdd, series="Test Data") +xlab('Year') + ylab('Heating Energy Consumption')

# -------------------------------------------------------------------------------------------------------------------
# Foreasting hourlyHDD with dynamic regression 
# Ref : https://otexts.com/fpp2/dynamic.html
# Ref : https://otexts.com/fpp2/complexseasonality.html
# Ref : https://robjhyndman.com/hyndsight/seasonal-periods/


# read csv file
data <- read.csv('combined.csv')

#both daily and annual seasonality
hdd_ts <- msts(data[,3], start = c(2011,25), seasonal.periods=c(24,8766))
temperature <- data[,2]

# trainng set
trainingsetsize = nrow(data) - 24*7 # test set is one week data
train_hdd <- subset(hdd_ts, end=trainingsetsize)
temp_train <- head(temperature, trainingsetsize)

#test data 
test_hdd <- subset (hdd_ts, start = trainingsetsize+1)
temp_test <- tail(temperature, 24*7)

bestfit <- list(aicc=Inf)
bestfourier <- numeric(2)

# Find optimal harmonic frequencies (K values). 
# Note : This loop takes a while to run
for (i in 1:12){
  for (j in 1:8){
    myfourier <- c(i,j)
    print(paste(paste("i is ",i), paste("j is ", j)))
    z <- fourier(train_hdd, K=c(i,j))
    #seasonality is being handled by Fourier terms, set sesoanl = FALSE in arima
    fit <- auto.arima(train_hdd, xreg=cbind(z,temp_train), seasonal=FALSE)
    if (fit$aicc < bestfit$aicc) {
      bestfit <- fit
      bestfourier <-myfourier
    }
  }
}

# print the optimal harmonics frequencies
bestfourier #bestfourier = 8 7, AICc=-14631.78 

# Fourier Terms for Test Data
zf <- fourier(test_hdd, K=bestfourier, h =168)

#Generate Forecasts
fc <- forecast(bestfit, xreg=cbind(zf,temp_test), h=168)

# Plot forecasts and test data in the same figure
autoplot(fc)  + autolayer(test_hdd, series="Test Data") + xlab('Date') + ylab('Heating Energy Consumption')

