library(TSstudio)
library(plotly)
data(USgas)

ts_plot(USgas, 
        title = "US Monthly Natural Gas consumption", 
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year",
        Xgrid = TRUE,
        Ygrid = TRUE)

# Transforming the ts object to data.frame object
USgas_df <- data.frame(year = floor(time(USgas)), month = cycle(USgas), USgas = as.numeric(USgas))
# Setting the month abbreviation and transforming it to a factor
USgas_df$month <- factor(month.abb[USgas_df$month], levels = month.abb)
head(USgas_df)

# Summarize the series by its frequency units
library(dplyr)
USgas_summary <- USgas_df %>% 
   group_by(month) %>%
   summarise(mean = mean(USgas),
                  sd = sd(USgas))
USgas_summary

library(plotly)
plot_ly (data = USgas_summary, x = ~ month, y = ~ mean, type = "bar", name = "Mean") %>%
layout (title = "USgas - Monthly Average", yaxis = list(title = "Mean", range = c(1500, 2700)))

# Plotting the density of each frequency unit
library(ggplot2)
ggplot(USgas_df, aes(x = USgas)) + 
   geom_density(aes(fill = month)) + 
   ggtitle("USgas - Kernel Density Estimates by Month") +
   facet_grid(rows = vars(as.factor(month)))

# simple method for detrending
USgas_df$USgas_detrend <- USgas_df$USgas - decompose(USgas)$trend
ggplot(USgas_df, aes(x = USgas_detrend)) + 
   geom_density(aes(fill = month)) + 
   ggtitle("USgas - Kernel Density Estimates by Month") +
   facet_grid(rows = vars(as.factor(month)))

# seasonal plot
library(forecast)
ggseasonplot(USgas,year.labels=TRUE,continuous=TRUE)
ggseasonplot(USgas,  polar = TRUE)
ts_seasonal(USgas,type ="normal")
ts_seasonal(USgas, type = "cycle")
ts_seasonal(USgas, type = "box")
# plot of all of the three seasonality plots:
# (normal, cycle, and box) side by side
ts_seasonal(USgas, type = "all")
ts_heatmap(USgas, color = "Reds")




