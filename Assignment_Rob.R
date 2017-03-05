## libaries
library(tseries)
library(forecast)
library(xts)
library(urca)
library(fGarch)


#setwd("")

## 1 production.txt
production_df<- read.delim("production(1).txt")
tail(production_df)

## a. plot ts
prod_ts = ts(production_df[-1], start=c(1990,1), end=c(2014,12), frequency=12) 
###plot of time series
par(mfrow=c(1,1))
plot(prod_ts) #plot
acf(prod_ts,main="Industrial production") #acf 
pacf(prod_ts,main="Industrial production") # pacf
adf.test(prod_ts) #augmented dickey fuller test for stationarity
###Dickey-Fuller = -1.5191, Lag order = 6, p-value = 0.779. So NOT STATIONARY


## b. difference
prod_diff = diff(prod_ts)
###plot of time series
plot(prod_diff,main="Industrial production diff.") #plot
acf(prod_diff,main="Industrial production diff.") #acf 
pacf(prod_diff,main="Industrial production diff.") # pacf
adf.test(prod_diff) #augmented dickey fuller test for stationarity
###Dickey-Fuller = -4.9932, Lag order = 6, p-value = 0.01. So, STATIONARY

## c. stationarity


## d. ARIMA
prod_model1 = auto.arima(prod_diff,ic="bic",seasonal=FALSE) #ARIMA based on BIC without seasonal lags
prod_model2 = auto.arima(prod_diff,ic="bic",seasonal=TRUE) #ARIMA based on BIC with seasonal lags
summary(prod_model1)
summary(prod_model2)
#ARIMA without seasonality: MA(1) model or ARIMA(0,0,1). Forecast based on moving average term with 1 lag
#ARIMA with seasonality: ARIMA(1,0,2)(0,1,1) model. AR term with 1 lag, MA with 2 lags, and seasonal differencing and MA with 1 lag
#model 1 has RMSE of 2.14 and model 2 has RMSE of 0.77
#model 2 has lower AIC and BIC, so, model 2 with seasonal lags is the better model


# e. Ljung Box test
Box.test(prod_model2$residuals, lag=1 ,type="Ljung-Box")
#Box-Ljung test
#data:  prod_model2$residuals
#X-squared = 0.011453, df = 1, p-value = 0.9148
#P-value is high, so model is adequate


## f. Forecast 12 months
Forecast_12 = predict(prod_model2,12)
Forecast_12$pred
plot(Forecast_12$pred)


##
##2. Rates.txt
rates_df<- read.delim("rates(1).txt")
date = as.Date(paste("01 ",rates_df[,1]), format = "%d %b-%Y")
rates_ts = as.xts(rates_df[,2:9], date)



## a. Stationary 
options(digits=4)

for(i in colnames(rates_ts))
{
  print(i)
  print(adf.test(rates_ts[,i]))
}
##None are stationary. All greater than .10 p value for adf test

## b. Cointegration. Phillips-Ouliaris Cointegration Test 

po.test(rates_ts[,c(1,2,3,4,5,6)])
#p-value=0.1 indicates that the residuals are stationary. So, the rates are cointegrated

#Johansen-Procedure: test # of cointegration
summary(ca.jo(rates_ts))
jo.out<-ca.jo(rates_ts)
ecm.out<-cajorls(jo.out,r=3)
summary(ecm.out$rlm)

####################################
## 3. yahoo log returns
yahoo_df<- read.delim("yahoo_logret(1).txt")

## a. AR(1)
yahoo_ar1 = arima(yahoo_df[,2],order = c(1,0,0)) 
summary(yahoo_ar1)
ar1_resid = yahoo_ar1$residuals #residuals

par(mfrow=c(1,1))
plot.ts(ar1_resid)
acf(ar1_resid) #look at autocorrelation of residuals
#residuals don't have high correlations with lags, so no heteroskedasticity

## b. GARCH(1,1)
garch_model=garchFit(formula~garch(1,1),data=yahoo_df[,2],trace=F,cond.dist="std")
summary(garch_model)

garch_resid = residuals(garch_model)

## Ljung box test
Box.test(garch_resid,type="Ljung-Box")
#data:  garch_1$residuals
#X-squared = 0.38, df = 1, p-value = 0.1
#p-value is high so model is adequate and residuals are independent

##c. Forecast 8 week forecast and volatility
Forecast_8 = predict(yahoo_ar1,8)
Forecast_8$pred
plot.ts(Forecast_8$pred)
predict(garch_model,8)[,2]