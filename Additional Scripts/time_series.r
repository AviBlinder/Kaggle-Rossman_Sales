train <- read.csv("./Data/train.csv")
test <- read.csv("./Data/test.csv")

t1 <- aggregate(Date ~ Store ,data=train,function(x) length(x))
t2 <- aggregate(Date ~ Store ,data=test,function(x) length(x))


table(t1$Date)
table(t2$Date)
t1[order(t1$Date),]

train$logSales <- log(train$Sales+1)
store1115 <- train[train$Store ==1115,]
test_period = 48
library(forecast)
y <- ts(store1115$logSales, frequency=7)
z <- fourier(ts(store1115$logSales, frequency=365.25), K=5)
zf <- fourierf(ts(store1115$logSales, frequency=365.25), K=5, h=test_period)
fit <- auto.arima(y, xreg=cbind(z), seasonal=FALSE)
fit <- auto.arima(y, xreg=cbind(z,holiday,promo), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,holidayf,promof), h=test_period)
plot(fc)