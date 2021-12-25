sp <-  read.csv(file.choose(), header = TRUE)
sp
colSums(is.na(sp))#no missing values
head(sp)
tail(sp)
class(sp) #data.frame
sps<- ts(sp[,2],start=2000, frequency=12)
sps 
class(sps) #time series
plot(sps, xlab= "Year", ylab= "Sales", main = "Sales vs Year (Original Dataset)") #Little seasonality 
plot(decompose(sps)) #can find seasonality

#mean (changing mean)
abline(reg = lm(sps~time(sps)))

# general Trend (upward Trend)
plot(aggregate(sps,FUN = mean), xlab= "Year", ylab= "Sales", main = "Sales vs Year (Original Dataset)" )

#sesonality 
boxplot(sps~cycle(sps),  xlab= "Year", ylab= "Sales", main = "Sales vs Year (Original Dataset)")

#differencing using log
#1st degree
p<- diff(log(sps))
plot(p,xlab= "Year", ylab= "Sales", main = "Sales vs Year")
abline(reg = lm(p~time(p))) 

#2nd degree
d.sp<-diff(p)
plot(d.sp,  xlab= "Year", ylab= "Sales", main = "Sales vs Year")
abline(reg = lm(d.sp~time(d.sp))) 

#summary 
summary(sps) #diff of range btw max and min - 562.7
summary(p) #diff of range btw max and min - 1.61
summary(d.sp) #diff of range btw max and min - 3.06

#Install and load the time series package
install.packages("tseries")
library(tseries)

#Dickey FUller Test for stationarity, to check if alternative is either stationary or explosive
#k is for additional lags
adf.test(sps, alternative="stationary")
#p-value is high, so we accept the null and reject the alternate. So, data is non-stationary
#The adf test confirms that the series is non-stationary for the original dataset.


adf.test(p)
adf.test(d.sp)
#The adf test for the differenced series is done. The null hypothesis states that the series is non stationary whereas the alternate hypothesis states that the series is stationary. The p-value is 0.01 which is less than 0.1, hence we can reject the null hypothesis and the series is stationary now.
#We have a stationary series now and we can proceed ahead with fitting the models.w


#acf and pacf for finding arima orders
acf(sps, main = "Original Seires")
pacf(sps, main = "Original Seires")
acf(p , main = "1st differenced Seires")
pacf(p, main = "1st differenced Seires")
acf(d.sp , main = "2nd differenced Seires")
pacf(d.sp , main = "2nd differenced Seires")

arima(sps,order=c(0,0,1))#aic = 457.17
arima(sps,order=c(1,1,1))#aic = 406.83
arima(sps,order=c(1,2,2))#aic = 392.15
arima(sps,order=c(0,1,2))#aic = 399.53
arima(sps,order=c(1,2,1))#aic = 400.12
arima(sps,order=c(1,0,5))#aic = 420.12
arima(sps,order=c(0,1,1))#aic = 413.16

library("forecast")
a <-auto.arima(sps) #arima(1,1,1)&arima(0,0,1) with 12 drifts
a #aic=399.53
b <-forecast(a,h=12,level=95)
b
accuracy(b)#mape-17.43117

#Split into train and test
sptrain <- ts(log(sps[1:24]),start= 2000, frequency=12 ) 
fit1 <-arima(sptrain, order = c(1, 2, 2))
pred1 <- 2.718^predict(fit1,6)$pred
pred1

tst <-sp[25:36,2]
stst<-ts(tst,start = 2002,frequency = 12)
stst
class(stst)

MAPE(stst,pred1) #0.1296529

# forecast of sales count of shampoo for the next 12 months
fit <-arima(sps, order = c(1, 2, 2))
nextpred <-predict(fit,n.ahead = 12)
nextpred
plot(forecast(fit,h=12), xlab = "Year", ylab = "Sales", main = "Forecast")

