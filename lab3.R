library(forecast)
y = AirPassengers
tsdisplay(y)
library(tseries)
adf.test(y)

# Fitting seasonal component
# 12 period moving average 6 ahead and 6 back
#this allows us to remove the monthly ups and downs
TC=ma(y,12)

linear_tc = lm(TC~time(y))
#de-trended time series; only seasonality left
tsdisplay(y-TC)

# each row represents a month
# each column is a year 
pseudo_s = y/TC
matrix_s = matrix(pseudo_s,nrow=12)
# trying to estimate the avergae of each month though the yrs
S = rowMeans(matrix_s,na.rm=TRUE)

#centering the seasonal pattern, for a full cycle, the mean must be 1
S = S/mean(S)

#dividing the series by the linear trend and seasonal component
R=y/(TC*rep(S,12))
acf(na.omit(R), lag.max = 5)
pacf(na.omit(R), lag.max= 5)

m_decomp=decompose(y,type="mult")

plot(m_decomp)

#fitting arima to the residuals
fit=arima(R,order=c(2,0,1))
library(lmtest) # so we can test coefficients for statistical significance
coeftest(fit)
res=residuals(fit)
tsdisplay(res)
Box.test(res,lag=10,t='Ljung-Box')



ftime=seq(1961,length=24,by=deltat(y)) # 24 future months
#combining the seaosonal and linear components
d_fy=(linear_tc$coef[1]+linear_tc$coef[2]*ftime)*rep(S[1:12],2)


# fittig the historical model
fit=Arima(R,order=c(2,0,1), include.mean=T)
fy=forecast(fit,h=30) # forecasts 6 missing months plus 24 months mo

#forecast by multiplying all
msef=d_fy*fy$mean[-c(1:6)]


#plotting the forecast
plot(y,xlim=c(time(y)[1],ftime[length(ftime)]),ylim=c(0,700))
lines(ftime, msef, col=4)


#same thing but easier
m_ets=ets(y,model="MMM")
m_forecast=forecast(ets(y,model="MMM",damped=T),level=95) # 95% confidenc
plot(m_forecast)







