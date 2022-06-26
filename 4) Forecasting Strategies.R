library(TSstudio)
data(USgas)

# The sample.out argument set the size of the testing 
# partition (and therefore the training partition)
USgas_partitions <- ts_split(USgas, sample.out = 12)
train <- USgas_partitions$train
test <- USgas_partitions$test
ts_info(train)
ts_info(test)

# ARIMA 
library(forecast)
md <- auto.arima(train)
checkresiduals(md)
# 12 month forecast
fc <- forecast(md, h = 12)
accuracy(fc, test)  # check accuracy
test_forecast(actual = USgas,
               forecast.obj = fc,
               test = test)

# naive forecast
naive_model <- naive(train, h  = 12)
test_forecast(actual = USgas,
               forecast.obj = naive_model,
               test = test)
accuracy(naive_model, test)

# seasonal naive forecast
snaive_model <- snaive(train, h = 12)
test_forecast(actual = USgas,
               forecast.obj = snaive_model,
               test = test)
accuracy(snaive_model, test)

# ARIMA Test set RMSE: 103.22
# snaive Test set RMSE: 164.70

# Final Model
md_final <- auto.arima(USgas)
# Forecast
fc_final <- forecast(md_final, h = 12) 
plot_forecast(fc_final,
              title = "The US Natural Gas Consumption Forecast",
              Xtitle = "Year",
              Ytitle = "Billion Cubic Feet")

# forecast next 60 months with 80 and 90% CI
fc_final2 <- forecast(md_final, 
                      h = 60, 
                      level = c(80, 90))
plot_forecast(fc_final2,
              title = "The US Natural Gas Consumption Forecast",
              Xtitle = "Year",
              Ytitle = "Billion Cubic Feet")








