library(UKgrid)
library(TSstudio)
library(plotly)

UKgrid_xts <- extract_grid(type = "xts", 
                           columns = "ND", 
                           aggregate = "hourly", 
                           na.rm = TRUE)

ts_plot(UKgrid, 
         title = "National Hourly Demand UK Grid", 
         Ytitle = "Megawatts",
         Xtitle = "Year",
         Xgrid = TRUE,
         Ygrid = TRUE)

# transform the UKgrid series into data.frame
library(xts)
UKgrid_df <- data.frame(time = index(UKgrid_xts), UKgrid = as.numeric(UKgrid_xts))
str(UKgrid_df)

# create seasonal features
library(lubridate)
UKgrid_df$hour <- hour(UKgrid_df$time)
UKgrid_df$weekday <- wday(UKgrid_df$time, label = TRUE, abbr = TRUE)
UKgrid_df$month <- factor(month.abb[month(UKgrid_df$time)], levels = month.abb)
head(UKgrid_df)

library(dplyr)
UKgrid_hourly <- UKgrid_df %>% 
 dplyr::group_by(hour) %>%
 dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE)) 

UKgrid_weekday <- UKgrid_df %>% 
 dplyr::filter(hour == 3 | hour == 9) %>%
 dplyr::group_by(hour, weekday) %>%
 dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE),
                  sd = sd(UKgrid, na.rm = TRUE)) 

UKgrid_weekday$hour <- factor(UKgrid_weekday$hour)

# Demand by Weekday Bar Plot
plot_ly(data = UKgrid_weekday, x = ~ weekday, y = ~ mean, type = "bar",color = ~ hour) %>%
   layout(title = "The Hourly Average Demand by Weekday",
          yaxis = list(title = "Mean", range = c(30000, 75000)), 
          xaxis = list(title = "Weekday"))

UKgrid_month <- UKgrid_df %>% 
   dplyr::filter(hour == 3 | hour == 9) %>%
 dplyr::group_by(hour, month) %>%
 dplyr::summarise(mean = mean(UKgrid, na.rm = TRUE),
                  sd = sd(UKgrid, na.rm = TRUE)) 

UKgrid_month$hour <- factor(UKgrid_month$hour)

# Demand by Month Bar Plot
plot_ly(data = UKgrid_month, x = ~ month, y = ~ mean, type = "bar",color = ~ hour) %>%
   layout(title = "The Hourly Average Demand by Month",
          yaxis = list(title = "Mean", range = c(30000, 75000)), 
          xaxis = list(title = "Month"))

# Density plot - 24 hour frequency
UKgrid_df$hour <- as.factor(UKgrid_df$hour)
ggplot(UKgrid_df, aes(x = UKgrid)) + 
   geom_density(aes(fill = hour)) + 
   ggtitle("UKgrid - Kernel Density Estimates by Hour of the Day") +
   facet_grid(rows = vars(as.factor(hour)))

# Density plot - Week Day frequency
UKgrid_df$weekday <- as.factor(UKgrid_df$weekday)
UKgrid_df %>% dplyr::filter(hour == 0) %>%
ggplot(aes(x = UKgrid)) + 
   geom_density(aes(fill = as.factor(weekday))) + 
   ggtitle("UKgrid - Kernel Density Estimates by Day of the Week") +
   facet_grid(rows = vars(as.factor(weekday)))

# Quantile Plots
ts_quantile(UKgrid)
ts_quantile(UKgrid, period = "weekdays", n = 2)

# 24-hour cycle by month
ts_quantile(UKgrid, period = "monthly", n = 2)








