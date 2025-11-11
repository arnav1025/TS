library(forecast)
library(lmtest)

#----------------- PART 2: ARIMA Simulation -----------------------------#
x=arima.sim(list(ar=0.5), 200)
x=arima.sim(list(ar=0.5, order=c(1,0,0)), 200)


#----------------- PART 3: ARIMA ACF ------------------------------------#
ARMAacf(x, lag.max = 5)
ARMAacf(ar=0.5, ma=0.5, lag.max=5)

#checking the ACF of custom model just to understnd how the flipping worked


# Generate theoretical PACF values
pacf_values <- ARMAacf(ar=0, ma=-0.8, lag.max=20, pacf=TRUE)

# Create a vector of lag numbers (excluding lag 0 if you prefer)
lags <- seq(0, length(pacf_values) - 1)

# Plot as a line graph
plot(lags, pacf_values, type="h", lwd=2, pch=16, col="blue",
     main="Theoretical PACF of ARMA(1,1)",
     xlab="Lag", ylab="Partial Autocorrelation")

# Add a horizontal zero line for reference
abline(h=0, col="black", lwd=1.2)


x= arima.sim(list(ar=0.5, order = c(1,0,0)), 200)
plot(x)

ARMAacf(ar = 0.5, ma = 0.5, lag.max = 5)
acf(arima.sim(n= 1e2, model = list(ar = 0.5, ma= 0.5)), lag.max = 5)




#----------------- PART 4: Parameter Estimation ------------------------#
# fitting a model to data for lab2
lab2 =  read.csv("lab2.csv")
acf(lab2$Series7, lag.max = 10)
pacf(lab2$Series7, lag.max = 5)
model7 = arima(lab2$Series7, order = c(2,0,0))
coeftest(model7)

#since the mean was insignificant we can test out case without mean
model7_nomean = arima(lab2$Series7, order = c(2,0,0), include.mean = FALSE)
coeftest(model7_nomean)





#----------------------- PART 5: DIAGNOSTICS----------------------------#

# Plotting the residuals over time
plot(residuals(model7))

# qq-plot of the residuals
qqnorm(residuals(model7))
qqline(residuals(model7))
shapiro.test(residuals(model7))
#Ljung-box test on the residuals
Box.test(residuals(model7), type = 'Ljung-Box', lag = 12, fitdf = 2)

# plotting the residuals against fitted values
plot(fitted(model7), residuals(model7))
abline(h=0, col="red", lwd=1.2)

acf(residuals(model7), lag.max = 25)
pacf(residuals(model7), lag.max = 25)




#------------- practise exercises --------------------------# 
y=arima.sim(model=list(ar=0.5,order=c(1,0,0)),n=200)
tsdisplay(y) # autocorrelated
y=arima.sim(model=list(ar=0.1,order=c(1,0,0)),n=200)
tsdisplay(y) # looks like it could be white noise
y=arima.sim(model=list(ar=0.9,order=c(1,0,0)),n=200)
tsdisplay(y) # looks like it could be non-stationary; highly autocorrelated


y=arima.sim(model=list(ar=c(0.5,0.45),order=c(2,0,0)),n=200)
tsdisplay(y) # looks like it could be non-stationary; highly autocorrelated
y=arima.sim(model=list(ar=c(0.6,0.5),order=c(2,0,0)),n=200) # doesn’t run be-cause non-stationary

y=arima.sim(model=list(ma=-0.9,order=c(0,0,1)),n=200)
tsdisplay(y) #

y=arima.sim(model=list(order=c(0,1,0)),n=200)
tsdisplay(y) # linear decay of ACF

y=arima.sim(model=list(ma=0.9,order=c(0,1,1)),n=200)
tsdisplay(y) # linear decay of ACFs
y=arima.sim(model=list(ma=-1,order=c(0,1,1)),n=200)
tsdisplay(y) # looks like white noise! This is because we get cancellation of the unit root term on both sides of the ARIMA equation


y=arima.sim(model=list(ar=0.5,ma=0.5,order=c(1,1,1)),n=200)
tsdisplay(y) #



