install.packages("TSA")
require(TSA)
getwd()
#read the csv file for the lab
lab1data = read.csv("lab1.csv", header=TRUE)
#check the data of the second column
lab1data[,2]
# check the names of the columns
names(lab1data)

# analysing series 1
plot(lab1data[,1], type="l")

par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,1])
pacf(lab1data[,1])

# analysing series 2 
plot(lab1data[,2], type="l")


par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,2])
pacf(lab1data[,2])

# analysing series 3, guessing it to be AR(1)

plot(lab1data[,3], type="l")

par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,3])
pacf(lab1data[,3])

# analysing series 4, guessing it to be AR(1)
plot(lab1data[,4], type="l")

par(mfrow=c(2,1)) 
acf(lab1data[,4])
pacf(lab1data[,4])

# analysing series 5, guessing it to be AR(1)
plot(lab1data[,5], type="l")

par(mfrow=c(2,1)) 
acf(lab1data[,5])
pacf(lab1data[,5])


# analysing series 6, guessing it to be AR(1)
plot(lab1data[,6], type="l")

par(mfrow=c(2,1)) 
acf(lab1data[,6])
pacf(lab1data[,6])

#------------- Trying to difference the series and observe their behaviours --------------# 
series1diff =  diff(lab1data[,1], lag=1)
series2diff =  diff(lab1data[,2], lag=1)
series3diff =  diff(lab1data[,3], lag=1)
series4diff =  diff(lab1data[,4], lag=1)
series5diff =  diff(lab1data[,5], lag=1)
series6diff =  diff(lab1data[,6], lag=1)

plot(series1diff, type="l")
plot(series2diff, type="l")
plot(series3diff, type="l")
plot(series4diff, type="l")
plot(series5diff, type="l")
plot(series6diff, type="l")

#------------- Trying to create a linear model for 4 --------------# 

tt=1:nrow(lab1data)
TC=lm(lab1data[,4] ~ tt)

y=lab1data[,4]-fitted(TC)
plot(y,type ="l")
acf(y)
pacf(y)

