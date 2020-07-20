setwd('C:/Users/seoju/Documents/ws/Pproject')
setwd('D:/Desktop/Itwill ws/Pproject')
setwd('d:/Pproject')
getwd()

install.packages('urca')

library(TTR)
library(forecast)
library(data.table)
library(readxl)
library(tseries)
library(ggplot2)
library(gcookbook)
library(urca)

######## SKT 4G TS
skt4 <- read.csv('4g_sk.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt4 <- skt4[2,]
skt4 <- as.numeric(skt4)
df_sk <- data.frame(Date,skt4)
ts_skt4 <- ts(df_sk$skt,start=c(2013,1),frequency = 12)


####
ts_skt4 %>% ur.kpss() %>% summary()

ndiffs(ts_skt4)
ts_skt4 %>% log() %>% diff() %>%  diff() %>%  ur.kpss() %>% summary()

nsdiffs(ts_skt4)

dev.new()
autoplot(ts_skt4)

####
fit <- auto.arima(ts_skt4,seasonal = F,)
fit

dev.new()
fit %>% forecast(h=12) %>% autoplot()

fit2 <- auto.arima(ts_skt4,seasonal = F, approximation = F, stepwise=F)
fit2
ggAcf(ts_skt4,lag=12)
ggPacf(ts_skt4,lag=12)
ggtsdisplay(diff(log(ts_skt4)))

fit4 <- auto.arima(diff(log(ts_skt4)),seasonal = F, approximation = F, stepwise=F)
fit4_m <- arima(ts_skt4,c(2,1,2),)

dev.new()
fit4_m %>% forecast(h=12) %>% autoplot()

summary(fit4_m)
residuals(fit4)
checkresiduals(fit4)


dev.new()
fit2 %>% forecast(h=12) %>% autoplot()

ndiffs(log(ts_skt4))
lambda <- BoxCox.lambda(ts_skt4)
lambda
autoplot(BoxCox(ts_skt4,lambda))
fit3 <- Arima(log(ts_skt4),c(2,2,0),)
fit3

fit3 %>% forecast(h=12) %>% autoplot()


############
dev.new()
ts_skt4 %>% 
  #log() %>% 
  Arima(c(0,1,2),seasonal = list(order=c(0,0,1),period=12)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 4G 예측 모델 AR(0,1,2)')