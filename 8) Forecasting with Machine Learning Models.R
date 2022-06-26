library(TSstudio)
library(plotly)
library(dplyr)
library(lubridate)

data(USVSales)
ts_info(USVSales)

# Exploratory analysis

# the series has cycle patterns
ts_plot(USVSales,
        title = "US Total Monthly Vehicle Sales",
        Ytitle = "Thousands of Units",
        Xtitle = "Year")

# Decomposing the series
ts_decompose(USVSales)

# Seasonal analysis
USVSales_detrend <- USVSales - decompose(USVSales)$trend
ts_seasonal(USVSales_detrend, type = "box")

# Correlation analysis
ts_cor(USVSales)
ts_lags(USVSales, lags = c(12, 24, 36))

## Conclusion from analysis
# The USVSales series is a monthly series with a clear monthly seasonality
# The series trend has a cyclic shape, and so the series has a cycle component embedded in the trend
# The series' most recent cycle starts right after the end of the 2008 economic crisis, between 2009 and 2010
# It seems that the current cycle reached its peak as the trend starts to flatten out
# The series has a strong correlation with its first seasonal lag

df <- ts_to_prophet(window(USVSales, start = c(2010,1))) 
names(df) <-  c("date", "y")
head(df)
ts_plot(df,
        title = "US Total Monthly Vehicle Sales (Subset)",
        Ytitle = "Thousands of Units",
        Xtitle = "Year")

# create monthly factor and season lag
df <- df %>% mutate(month = factor(month(date, label = TRUE), ordered = FALSE),
                    lag12 = lag(y, n = 12)) %>%
             filter(!is.na(lag12))

df$trend <- 1:nrow(df)
# adding trend and polynomial trend component
df$trend_sqr <- df$trend ^ 2
str(df)

h <- 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]
# forecast data frame
forecast_df <- data.frame(date = seq.Date(from = max(df$date) + month(1), length.out = h, by = "month"),
                          trend = seq(from = max(df$trend) + 1, length.out = h, by = 1))
forecast_df$trend_sqr <- forecast_df$trend ^ 2
forecast_df$month <- factor(month(forecast_df$date, label = TRUE), ordered = FALSE)
forecast_df$lag12 <- tail(df$y, 12)

library(earth)
modfit = earth(y ~ month + lag12 + trend + trend_sqr, data = train_df)
modfit
summary(modfit)
evimp(modfit)
p=predict(modfit, newdata =test_df)
head(p)
p=as.data.frame(p)
final=cbind(test_df$y,p)
head(final)
cor(test_df$y,p)
earth_resid <- final$test_df - final$y
car::qqPlot(earth_resid)
mape <- mean(abs(final$test_df - final$y) / final$test_df)
mape # mean absolute percentage error is only 3.75%

# y column is forecasted values
forecast <- predict(modfit, newdata=forecast_df)
forecast_df$forecast <- forecast
forecast_df
plot(forecast_df$forecast, type='l', x=forecast_df$date)

# xts plot
library(xts)
data_xts <- xts(x = forecast_df[,c("forecast")], 
                          frequency = 12,
                          order.by = forecast_df$date)
plot.xts(data_xts, 
         main = "Sales Forecast")

# TS Plot
forecast_ts <- ts_to_prophet(data_xts)
ts_plot(forecast_ts,
        title = "Sales Forecast",
        Ytitle = "Thousands of Units",
        Xtitle = "Month")
