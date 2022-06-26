library(TSstudio)
data(USVSales)

#  shows seasonality and trend
ts_plot(USVSales,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of units",
        Xtitle = "Year")

acf(USVSales, lag.max = 60) 

# plot the correlation of the USVSales series
stats::acf(USVSales, lag.max = 60)

# partial autocorrelation function (PACF)
stats::pacf(USVSales, lag.max = 60

# Lag Plot
ts_lags(USVSales) 

data(USUnRate)
 
ts_plot(USUnRate,
        title = "US Monthly Civilian Unemployment Rate",
        Ytitle = "Unemployment Rate (%)",
        Xtitle = "Year")

# align the USUNRate and USVSales series to the same time frame
us_vsales <- window(USVSales, start = c(1976,1), end = c(2018,6))
us_unrate <- window(USUnRate, start = c(1976,1), end = c(2018,6)) 

library(plotly)
# plot the two series
plot_ly(x = time(us_vsales), 
         y = us_vsales, 
         type = "scatter", 
         mode = "line", 
         name = "Total Vehicle Sales") %>%
   add_lines(x = time(us_unrate), 
             y = us_unrate,
             name = "Unemployment Rate", 
             yaxis = "y2") %>%
   layout(
     title = "Total Monthly Vehicle Sales vs Unemployment Rate in the US", 
     yaxis2 =  list(
       overlaying = "y",
       side = "right",
       title = "Percentage",
       showgrid = FALSE
     ),
     yaxis = list(title = "Thousands of Units",
                  showgrid = FALSE),
     legend = list(orientation = 'h'),
     margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2)
   )

# # measure the level of correlation between the unemployment 
# rate and the vehicle sales and its lags using the ccf function
ccf(x = us_vsales, y = us_unrate, lag.max = 36) 

# plot the relationship between US vehicle sales and the lags of the unemployment rate
ccf_plot(x = USVSales, y = USUnRate, lags = 0:12) 

