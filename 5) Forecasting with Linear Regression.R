library(TSstudio)
library(forecast)

data(USgas)
ts_plot(USgas,
        title = "US Monthly Natural Gas consumption", 
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")

ts_info(USgas) 

# series components structure
ts_decompose(USgas)

# change series to data frame
USgas_df <- ts_to_prophet(USgas) 
head(USgas_df)

# index the series observations in chronological order
USgas_df$trend <- 1:nrow(USgas_df)

# extract the month of the year
library(lubridate) 
USgas_df$seasonal <- factor(month(USgas_df$ds, label = T), ordered = FALSE)

head(USgas_df) 

# split the series into a training and testing partition
h <- 12 # setting a testing partition length

train <- USgas_df[1:(nrow(USgas_df) - h), ]

test <- USgas_df[(nrow(USgas_df) - h + 1):nrow(USgas_df), ] 

# regressing the series with the trend variable
md_trend <- lm(y ~ trend, data = train)
summary(md_trend)

# predictions
train$yhat <- predict(md_trend, newdata = train)
test$yhat <- predict(md_trend, newdata = test)

# plot
library(plotly) 
plot_lm <- function(data, train, test, title = NULL){
 p <- plot_ly(data = data, 
         x = ~ ds, 
         y = ~ y, 
         type = "scatter",
         mode = "line",
         name = "Actual") %>%
   add_lines(x =  ~ train$ds,
             y = ~ train$yhat,
             line = list(color = "red"),
             name = "Fitted") %>%
   add_lines(x =  ~ test$ds,
             y = ~ test$yhat,
             line = list(color = "green", dash = "dot", width = 3),
             name = "Forecasted") %>%
   layout(title = title,
          xaxis = list(title = "Year"),
          yaxis = list(title = "Billion Cubic Feet"),
          legend = list(x = 0.05, y = 0.95))
 return(p)
 } 

plot_lm(data = USgas_df, 
         train = train, 
         test = test,
         title = "Predicting the Trend Component of the Series")

# measure the model error rate
mape_trend <- c(mean(abs(train$y - train$yhat) / train$y),
                 mean(abs(test$y - test$yhat) / test$y))
mape_trend 


# regressing the series with the seasonal variable
md_trend <- lm(y ~ seasonal, data = train)
summary(md_seasonal)

# predictions
train$yhat <- predict(md_seasonal, newdata = train)
test$yhat <- predict(md_seasonal, newdata = test)

plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Seasonal Component of the Series")

# measure the model error rate
mape_seasonal <- c(mean(abs(train$y - train$yhat) / train$y),
                   mean(abs(test$y - test$yhat) / test$y))
mape_seasonal 

# join the two components into one model
md1 <- lm(y ~ seasonal + trend, data = train)
summary(md1)
train$yhat <- predict(md1, newdata = train)
test$yhat <- predict(md1, newdata = test)
 
plot_lm(data = USgas_df, 
        train = train, 
        test = test,
        title = "Predicting the Trend and Seasonal Components of the Series")

mape_md1 <- c(mean(abs(train$y - train$yhat) / train$y),
              mean(abs(test$y - test$yhat) / test$y))
mape_md1


# add a second degree of the polynomial for the trend input
md2 <- lm(y ~ seasonal + trend + I(trend^2), data = train)
summary(md2)
train$yhat <- predict(md2, newdata = train)
test$yhat <- predict(md2, newdata = test)
plot_lm(data = USgas_df, 
         train = train, 
         test = test,
         title = "Predicting the Trend (Polynomial) and Seasonal Components of the Series")
mape_md2 <- c(mean(abs(train$y - train$yhat) / train$y),
              mean(abs(test$y - test$yhat) / test$y))
mape_md2


# add a non-linear trend
USgas_split <- ts_split(USgas, sample.out = h)
train.ts <- USgas_split$train
test.ts <- USgas_split$test
md3 <- tslm(train.ts ~ season + trend + I(trend^2))
summary(md3) 


# remodel the USgas series
# the series trend had a structural break around the year 2010
r <- which(USgas_df$ds == as.Date("2014-01-01"))
USgas_df$s_break <- ifelse(year(USgas_df$ds) >= 2010, 1, 0)
USgas_df$s_break[r] <- 1
md3 <- tslm(USgas ~ season + trend + I(trend^2) + s_break, data = USgas_df)
summary(md3)
checkresiduals(md3)
