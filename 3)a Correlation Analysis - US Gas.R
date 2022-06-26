library(TSstudio)
data(USgas)

# shows seasonality
ts_plot(USgas,
        title = "US Monthly Natural Gas consumption", 
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Year")

# plot the correlation of the USgas series
stats::acf(USgas, lag.max = 60)

# partial autocorrelation function (PACF)
stats::pacf(USgas, lag.max = 60) 

# Lag Plot
ts_lags(USgas) 

