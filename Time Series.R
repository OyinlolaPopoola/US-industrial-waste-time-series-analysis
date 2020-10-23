install.packages("autoplot")
library(tseries)
library(fpp2)
waste <- read.csv("waste.csv", fileEncoding = 'UTF-8-BOM')
twaste <- ts(waste, start=1990, frequency = 1)
twaste
autoplot(twaste)
autoplot(ma(twaste,10))#smoothing THE TIME SERIES

#handling missing values
statsNA(twaste)


#evaluate assumption of stationarity
install.packages("tseries")
install.packages("uroot")
library(tseries)
adf.test(twaste)
acf(twaste)

ndiffs(twaste)
difwaste<-diff(twaste,differences = 1)
autoplot(difwaste)
adf.test(difwaste)
acf(difwaste)
pacf(difwaste)
ggtsdisplay(difwaste)
#The ACF and PACF graphs suggest that the resulting time series 
#might be an AR(0) or MA(0)due to the no spike
#in both graphs but we use auto.arima() to decide the best model

fit1 <- Arima(twaste, c(0,1,0))
checkresiduals(fit1)
summary(fit1)

qqnorm(fit1$residuals)
qqline(fit1$residuals)
Box.test(fit1$residuals, type = "Ljung-Box")

#check
checkmodel <- auto.arima(twaste)
checkmodel

#forecasting
forecast(fit1, 3)
autoplot(forecast(fit1, 3), xlab = "Year", ylab = "Annual Value")



