library(forecast)
library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)
library(datasets)

# non-stationary series: mean and variance are both changing over time
data(AirPassengers)
ts_plot(AirPassengers,
        title = "Monthly Airline Passenger Numbers 1949-1960",
        Ytitle = "Thousands of Passengers",
        Xtitle = "Year")

# remove the series trend
# variation of the series is still increasing over time
ts_plot(diff(AirPassengers, lag = 1),
        title = "AirPassengers Series - First Differencing",
        Xtitle = "Year",
        Ytitle = "Differencing of Thousands of Passengers")

# remove the series trend and seasonality
ts_plot(diff(diff(AirPassengers, lag = 1), 12),
        title = "AirPassengers Series - First and Seasonal Differencing",
        Xtitle = "Year",
        Ytitle = "Differencing of Thousands of Passengers")

# log transformation
ts_plot(diff(log(AirPassengers), lag = 1),
        title = "AirPassengers Series - First Differencing with Log Transformation",
        Xtitle = "Year",
        Ytitle = "Differencing/Log of Thousands of Passengers")



data("Coffee_Prices")
robusta_price <- window(Coffee_Prices[,1], start = c(2000, 1))
ts_plot(robusta_price,
        title = "The Robusta Coffee Monthly Prices",
        Ytitle = "Price in USD",
        Xtitle = "Year")
acf(robusta_price) # tails off
pacf(robusta_price) # cuts off at lag 1; AR(1)

# first differencing
robusta_price_d1 <- diff(robusta_price)
par(mfrow=c(1,2))
acf(robusta_price_d1)
pacf(robusta_price_d1)
# the series indicates an AR(1) process
# as the ACF tails off and the PACF cuts on the first lag
robusta_md <- arima(robusta_price, order = c(1, 1, 0))
summary(robusta_md)
checkresiduals(robusta_md) # errors are white noise


# SARIMA
# trend and seasonality exists
data(USgas)
ts_plot(USgas,
        title = "US Monthly Natural Gas consumption", 
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")

# train-test split
USgas_split <- ts_split(USgas, sample.out = 12)
train <- USgas_split$train
test <- USgas_split$test
par(mfrow=c(1,2))
acf(train, lag.max = 60)
pacf(train, lag.max = 60)

# removing the series trend; variation still exists
USgas_d12 <- diff(train, 12)
par(mfrow=c(1,2))
acf(USgas_d12, lag.max = 60)
pacf(USgas_d12, lag.max = 60)

ts_plot(USgas_d12,
        title = "US Monthly Natural Gas consumption - First Seasonal Difference", 
        Ytitle = "Billion Cubic Feet (First Difference)",
        Xtitle = "Year")

# removing first order difference along 
# with the first order seasonal difference
USgas_d12_1 <- diff(diff(USgas_d12, 1))
par(mfrow=c(1,2))
acf(USgas_d12_1, lag.max = 60) # cuts off at lag 1 
pacf(USgas_d12_1, lag.max = 60) # tails off
# detrended plot
ts_plot(USgas_d12_1,
        title = "US Monthly Natural Gas consumption - First Seasonal and Non-Seasonal Differencing", 
        Ytitle = "Billion Cubic Feet (Difference)",
        Xtitle = "Year")

# ACF and PACF
par(mfrow=c(1,2))
acf(USgas_d12_1, lag.max = 60)
pacf(USgas_d12_1, lag.max = 60)

# Auto-ARIMA and forecasting
USgas_auto_md1 <- auto.arima(train)
USgas_auto_md1 # ARIMA(2,1,1)(2,1,1)[12] w/ AIC=2599.67 

# final model
final_md <- arima(USgas, order = c(2,1,1), seasonal = list(order = c(2,1,1)))
final_md

# Check residuals
# Ljung-Box test: null hypothesis that the residuals are white noise
checkresiduals(final_md) 

# forecast
USgas_fc <- forecast(final_md, h = 12)
plot_forecast(USgas_fc,
              title = "US Natural Gas Consumption - Forecast",
              Ytitle = "Billion Cubic Feet",
              Xtitle = "Year")





