setwd('C:/Users/seoju/Documents/ws/Pproject')
setwd('D:/Desktop/Itwill ws/Pproject')
setwd('d:/Pproject')
getwd()

library(TTR)
library(forecast)
library(data.table)
library(readxl)
library(tseries)
library(ggplot2)
library(gcookbook)


######## SKT 4G
skt4 <- read.csv('4g_sk.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt4 <- skt4[2,]
skt4 <- as.numeric(skt4)
df_sk <- data.frame(Date,skt4)
ts_skt4 <- ts(df_sk$skt,start=c(2013,1),frequency = 12)

auto.arima(ts_skt4)
summary(ts_skt4)

dev.new()
plot(decompose(ts_skt4),)
adf.test(ts_skt4,alternative = 'stationary',k=0)

dev.new()
par(mfrow=c(2,2))
plot(ts_skt4)
plot(log(ts_skt4))
plot(diff(log(ts_skt4),1))
plot(diff(log(ts_skt4),2))

dev.new()
par(mfrow=c(2,2))
acf(log(ts_skt4))
pacf(log(ts_skt4))
acf(diff(log(ts_skt4)))
pacf(diff(log(ts_skt4)))

auto.arima(diff(log(ts_skt4)))
fit <- arima(log(ts_skt4),c(0,1,1),seasonal = list(order=c(0,0,1), period=12),)
summary(fit)
Box.test(fit$residuals)

pred <- predict(fit, n.ahead = 12)
dev.new()
ts.plot(ts_skt4,
        exp(pred$pred),
        log='y',
        lty=c(1,3))
