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
series1diff = diff(lab1data[,1], lag=1)

par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,1])
pacf(lab1data[,1])

# analysing series 2 
plot(lab1data[,2], type="l")
series2diff = diff(lab1data[,2], lag=1)

par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,2])
pacf(lab1data[,2])

# analysing series 3 guessing it to be AR(1)

plot(lab1data[,3], type="l")
series2diff = diff(lab1data[,3], lag=1)

par(mfrow=c(2,1)) # par sets plotting options; here we set 2 plots
acf(lab1data[,3])
pacf(lab1data[,3])
