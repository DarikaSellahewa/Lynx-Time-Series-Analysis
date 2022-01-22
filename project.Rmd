---
title: 'Project: Lynx analysis'
author: "Dilinee Sellahewa"
date: "13/11/2021"
output: word_document
---


library(ggplot2)


library(ggplot2)
## Warning: package 'ggplot2' was built under R version 4.0.5
#plot raw data
ts.plot(lynx, main = "Plot of raw Lynx Data")

#plotting histogram
tmp <- hist(lynx)
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),type="l")

#ACF and PACF of raw data
acf(lynx, lag.max = 50, main = "Correlogram of raw Lynx Data")
pacf(lynx, lag.max = 50, main = "Partial correlogram of raw Lynx Data")

#Spectrum
spectrum(lynx)
spectrum(lynx)$freq


##  [1] 0.008333333 0.016666667 0.025000000 0.033333333 0.041666667 0.050000000
##  [7] 0.058333333 0.066666667 0.075000000 0.083333333 0.091666667 0.100000000
## [13] 0.108333333 0.116666667 0.125000000 0.133333333 0.141666667 0.150000000
## [19] 0.158333333 0.166666667 0.175000000 0.183333333 0.191666667 0.200000000
## [25] 0.208333333 0.216666667 0.225000000 0.233333333 0.241666667 0.250000000
## [31] 0.258333333 0.266666667 0.275000000 0.283333333 0.291666667 0.300000000
## [37] 0.308333333 0.316666667 0.325000000 0.333333333 0.341666667 0.350000000
## [43] 0.358333333 0.366666667 0.375000000 0.383333333 0.391666667 0.400000000
## [49] 0.408333333 0.416666667 0.425000000 0.433333333 0.441666667 0.450000000
## [55] 0.458333333 0.466666667 0.475000000 0.483333333 0.491666667 0.500000000
spectrum(lynx)$spec
##  [1]  1491369.09   192249.98 12127402.99    92528.20  2243263.67  5487460.30
##  [7]   692561.93  2349961.56   726476.81  1620661.93 12499499.04 39718652.61
## [13] 37928771.24  3138246.18   459119.52  6224686.85   266138.79   954044.36
## [19]  2052613.89  1033288.58   512941.75  1057258.77  1234885.27  1599620.42
## [25]  6789826.50  1722482.65   159233.41   807666.22  1449885.79   295174.40
## [31]   345738.78   902552.87   281549.36   198469.87    38581.65   407818.71
## [37]   195889.90   662245.74   639850.47   187677.18   565705.70   301658.55
## [43]    20784.47   351624.67    62480.25   147071.28    12364.77   271902.91
## [49]    24376.38    27412.21   274800.54     4139.91   104315.18    19941.30
## [55]    20492.03   107270.43   104151.16    92778.74    74344.92    90696.80
#approximate seasonality
s1 <- 1/0.100000000
s2 <- 1/0.108333333
print(s1)
## [1] 10
print(s2)
## [1] 9.230769
#Selecting seasonality period
acf(diff(lynx, lag = 9),lag.max = 70, main = "Correlogram for first order difference of lynx data with seasonality = 9")
pacf(diff(lynx, lag = 9),lag.max = 70, main = "Partial Correlogram for first order difference of lynx data with seasonality = 9")

acf(diff(lynx, lag = 10),lag.max = 70, main = "Correlogram for first order difference of lynx data with seasonality = 10")
pacf(diff(lynx, lag = 10),lag.max = 70, main = "Partial Correlogram for first order difference of lynx data with seasonality = 10")


#As lag = 9 has less significant lags, proceeding with that.

#initial model
acf(arima(lynx, order = c(2,0,0), seasonal = list(order = c(0,1,0), period = 9))$resid, main = "Correlogram of ARIMA(2,0,0)(0,1,0), s = 9")

pacf(arima(lynx, order = c(2,0,0), seasonal = list(order = c(0,1,0), period = 9))$resid, main = "Partial correlogram of ARIMA(2,0,0)(0,1,0), s = 9")

arima(lynx, order = c(2,0,0), seasonal = list(order = c(0,1,0), period = 9))
## 
## Call:
## arima(x = lynx, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 0), period = 9))
## 
## Coefficients:
##          ar1      ar2
##       0.8909  -0.3360
## s.e.  0.0930   0.0926
## 
## sigma^2 estimated as 1094208:  log likelihood = -879.44,  aic = 1764.89
#Adding seasonal AR terms
acf(arima(lynx, order = c(2,0,0), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Correlogram of ARIMA(2,0,0)(1,1,0), s = 9")
pacf(arima(lynx, order = c(2,0,0), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Partial correlogram of ARIMA(2,0,0)(1,1,0), s = 9")

arima(lynx, order = c(2,0,0), seasonal = list(order = c(1,1,0), period = 9))
## 
## Call:
## arima(x = lynx, order = c(2, 0, 0), seasonal = list(order = c(1, 1, 0), period = 9))
## 
## Coefficients:
##          ar1      ar2     sar1
##       0.9513  -0.3986  -0.3086
## s.e.  0.0924   0.0923   0.0996
## 
## sigma^2 estimated as 994982:  log likelihood = -874.96,  aic = 1757.93
#adding non seasonal MA terms
acf(arima(lynx, order = c(2,0,1), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Correlogram of ARIMA(2,0,1)(1,1,0), s = 9")

pacf(arima(lynx, order = c(2,0,1), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Partial correlogram of ARIMA(2,0,1)(1,1,0), s = 9")

arima(lynx, order = c(2,0,1), seasonal = list(order = c(1,1,0), period = 9))
## 
## Call:
## arima(x = lynx, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 0), period = 9))
## 
## Coefficients:
##          ar1      ar2     ma1     sar1
##       0.7403  -0.2552  0.2542  -0.3148
## s.e.  0.2282   0.1794  0.2318   0.1011
## 
## sigma^2 estimated as 984851:  log likelihood = -874.47,  aic = 1758.94
#Reducing non-seasonal AR terms
acf(arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Correlogram of ARIMA(1,0,1)(1,1,0), s = 9")
pacf(arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,0), period = 9))$resid, main = "Partial correlogram of ARIMA(1,0,1)(1,1,0), s = 9")

arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,0), period = 9))
## 
## Call:
## arima(x = lynx, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 0), period = 9))
## 
## Coefficients:
##          ar1     ma1     sar1
##       0.4685  0.4926  -0.2943
## s.e.  0.1046  0.0945   0.1011
## 
## sigma^2 estimated as 1004090:  log likelihood = -875.39,  aic = 1758.79

#adding seasonal MA terms
acf(arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,1), period = 9))$resid, main = "Correlogram of ARIMA(1,0,1)(1,1,1), s = 9")
pacf(arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,1), period = 9))$resid, main = "Partial correlogram of ARIMA(1,0,1)(1,1,1), s = 9")

arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,1), period = 9))
## 
## Call:
## arima(x = lynx, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1), period = 9))
## 
## Coefficients:
##          ar1     ma1    sar1     sma1
##       0.4984  0.5052  0.4904  -1.0000
## s.e.  0.1009  0.0908  0.1108   0.1771
## 
## sigma^2 estimated as 813426:  log likelihood = -871.2,  aic = 1752.4
#Hence selected model is, 
#arima(lynx, order = c(1,0,1), seasonal = list(order = c(1,1,0), period = 9))

#predict
predict(arima(lynx, order=c(1,0,1),seasonal=list(order=c(1,1,0),period=9)),n.ahead=18)$pred
## Time Series:
## Start = 1935 
## End = 1952 
## Frequency = 1 
##  [1] 2010.3828  987.0359  339.9567  347.3815  522.0640  817.2527 1452.4525
##  [8] 2589.4898 3447.7899 2282.2489 1148.7780  395.5371  387.8586  563.2386
## [15]  871.0354 1492.9347 2609.3591 3432.5456

#Plotting actual and forecasts
actual_df <- data.frame(year = time(lynx), trappings = as.matrix(lynx))
pred <- predict(arima(lynx,order=c(1,0,1),seasonal=list(order=c(1,1,0),period=9)),n.ahead=18)$pred
pred_df <- data.frame(year = time(pred), trappings = as.matrix(pred))


ggplot() +
    geom_line(data = actual_df, aes(x = year, y = trappings, colour = "Actual lynx trappings")) +  # Plotting original data
    geom_line(data = pred_df, aes(x = year, y = trappings, colour = "Forecasted lynx trappings"))+ 
  #labs(color = "Legend")+
  scale_colour_manual("", 
                      breaks = c("Actual lynx trappings", "Forecasted lynx trappings"),
                      values = c("black", "blue")) + 
  labs(title = "Actual and Forecasted lynx trappings")+
  scale_x_continuous(n.breaks = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
