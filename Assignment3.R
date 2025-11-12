library(forecast)
library(tseries)
library(TSA)
library(urca)
library(lmtest) 
data("prescrip")
data("beersales")
y = prescrip
z = beersales

#------------------------- QUESTION 1 -----------------------------------# 
#making the data ts data
x = ts(c(-1, 6, 1, 20, 28, 14, 30, 21, 18, 20))

#quick visual check
tsdisplay(x)

#Fitting AR 1 with drift (intercept != 0)
model_1 = arima(x, order=c(1,0,0), include.mean = TRUE)

# we notice the values converge to the drift term 
# estimated as information goes down
fc1 = forecast(model_1, h=10)



#------------------------- QUESTION 2 -----------------------------------# 

tsdisplay(y)
#ADF shows stationary therefore must be a linear trend we observe
adf.test(y, k = 0)


n=length(y)
tt=2:n # convenience vector of time indices, 67 in length
y_diff=diff(y) # first difference of the series # very first obsesrvation missing, 67
fit=lm(y_diff~tt+prescrip[-n]) # estimate alpha, omega x[t-1], beta (alpha intercept
# beta by tt, omega by prescrip[-n])
yhat=fitted(fit)


SSE=sum((y-yhat)^2) 
SSM =sum((yhat)^2)
dofm = 3
dofe = n - 3 - 1

phi2 = (SSM/dofm)/(SSE/dofe)

#the test stat is the same
summary(ur.df(y, type = 'trend', lags = 0))



#------------------------- QUESTION 3 -----------------------------------# 
tsdisplay(z)
acf(z, lag.max = 12)

#not stationary, seasonality
adf.test(z)
# 12 period smoother? annual sales least jagged
TC = ma(z, 12)
plot(TC)

#additive model decomp
linear_tc = lm(TC~time(z))

tsdisplay(z-TC)


# Adjusting seasonality
pseudo_s = z - TC
matrix_s = matrix(pseudo_s,nrow=12)

# trying to estimate the avergae of each month though the yrs
S = rowMeans(matrix_s,na.rm=TRUE)

#centering the seasonal pattern, for a full cycle, the mean must be 1
S = S - mean(S)

#dividing the series by the linear trend and seasonal component
R = z - TC - rep(S,16)
acf(na.omit(R), lag.max = 20)
pacf(na.omit(R), lag.max= 20)

tsdisplay(R)



fit=Arima(R,order=c(3,0,4), include.mean = T)

coeftest(fit)
res=residuals(fit)
tsdisplay(res)
Box.test(res,lag=10,t='Ljung-Box')

linear_tc = lm(TC~time(z))
ftime=seq(1991,length=24,by=deltat(y)) 
d_fy = ts((linear_tc$coef[1]+linear_tc$coef[2]*ftime) + rep(S[1:12],2))
plot(d_fy, type = 'l')


fy=forecast(fit,h=30) # forecasts 6 missing months plus 24 months 

asef=d_fy + fy$mean[-c(1:6)]

plot(z, xlim = c(time(z)[1], ftime[length(ftime)]))
lines(ftime, asef, col= 4)



