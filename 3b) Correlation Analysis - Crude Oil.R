library(TSstudio)
data(EURO_Brent)
 
# shows no seasonality or trend
ts_plot(EURO_Brent,
        title = "Brent Crude Oil Prices",
        Ytitle = "US Dollars per Barrel",
        Xtitle = "Year")

acf(EURO_Brent, lag.max = 60)

# plot the correlation of the EURO_Brent series
stats::acf(EURO_Brent, lag.max = 60)

# partial autocorrelation function (PACF)
stats::pacf(EURO_Brent, lag.max = 60)

# Lag Plot
ts_lags(EURO_Brent) 
