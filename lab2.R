library(forecast)
x=arima.sim(list(ar=0.5), 200)
x=arima.sim(list(ar=0.5, order=c(1,0,0)), 200)
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
