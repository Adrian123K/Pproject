setwd('C:/Users/seoju/Documents/ws/Pproject')
getwd()
library(TTR)
library(forecast)
library(data.table)
library(readxl)
library(tseries)
library(ggplot2)

## Data 붙이기
skt4 <- read.csv('4g_sk.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt4 <- skt4[2,]
skt4 <- as.numeric(skt4)

df_sk <- data.frame(Date,skt4)
str(df_sk)

## skt data 분기별 freq
ts_skt4 <- ts(df_sk$skt,start=c(2013,1),frequency = 12)
end(ts_skt4)

dev.new()
plot.ts(ts_skt4)
plot(decompose(ts_skt4))

## 각 plot 확인
dev.new()
ggseasonplot(ts_skt4)
ggsubseriesplot(diff(ts_skt4, differences = 1))
gglagplot(ts_skt4)

## acf, pacf 확인
dev.new()
par(mfrow=c(2,1))
acf(diff(ts_skt4,differences = 1),lag=40)
pacf(diff(ts_skt4,differences = 1),lag=40)
acf(diff(ts_skt4,differences = 2))
pacf(diff(ts_skt4,differences = 2))
ggAcf(diff(ts_skt4,differences = 1),lag=40) + ggtitle('차분 1회 시행 시 ACF')
ggAcf(diff(ts_skt4,differences = 2),lag=40) + ggtitle('차분 2회 시행 시 ACF')
ggPacf(diff(ts_skt4, differences = 1),lag=40) + ggtitle('차분 1회 시행 시 PACF')
ggPacf(diff(ts_skt4, differences = 1),lag=40) + ggtitle('차분 2회 시행 시 PACF')

##### skt 4g
auto.arima(ts_skt4,seasonal = F, stepwise=F, approximation = F)
dev.new()
ggtsdisplay(ts_skt4)
dev.new()
ggtsdisplay(diff(log(ts_skt4),differences = 2))

skt4_fit <- arima(diff(log(ts_skt4),differences = 2),c(4,2,0))
summary(skt4_fit)
dev.new()
checkresiduals(skt4_fit)

dev.new()
ts_skt4 %>%
  Arima(order=c(4,2,0)) %>%
  forecast() %>%
  autoplot() + ggtitle('SKT 4G 예측 모델')
# par(new=T)  

##### skt 5g
skt5 <- read.csv('5g_sk.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt5 <- as.numeric(skt5)

df_skt5 <- data.frame(Date,skt5)

ts_skt5 <- ts(df_skt5$skt5,start=c(2013,1),frequency = 12)
end(ts_skt5)

dev.new()
plot.ts(ts_skt5)

ggAcf(ts_skt5,lag=30)
ggPacf(ts_skt5,lag=30)

auto.arima(ts_skt5,seasonal = F, stepwise=F, approximation = F)
dev.new()
ggtsdisplay(diff(ts_skt5,differences = 2))


skt5_test <- arima(diff(ts_skt5,differences = 2),c(5,2,0))
dev.new()
checkresiduals(skt5_test)

dev.new()
ts_skt5 %>%
  Arima(order=c(5,2,0)) %>%
  forecast() %>%
  autoplot() + ggtitle('SKT 5g 예측 모델')




dev.new()
ts_skt4 %>%
  Arima(order=c(4,2,0)) %>%
  forecast() %>%
  autoplot()+ ggtitle('SKT 4g 예측 모델')


##### kt 4g

kt4 <- read.csv('4g_kt.csv',header=F,stringsAsFactors = F)
kt4 <- as.numeric(kt4)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
df_kt <- data.frame(Date, kt4)
ts_kt4 <- ts(df_kt$kt4,start=c(2013,1),frequency = 12)

# 원본데이터 확인
dev.new()
ggtsdisplay(ts_kt4)
dev.new()
ggtsdisplay(diff(log(ts_kt4),differences = 2),lag=10)

# auto arima로 확인
auto.arima(ts_kt4,seasonal = F, stepwise=F, approximation = F)
# ARIMA(0,2,1)

kt4_fit <- arima(diff(log(ts_kt4),differences = 2),c(2,2,0),)

adf.test(diff(ts_kt4,differences = 2),alternative='stationary',k=0)
summary(kt4_fit)
dev.new()
checkresiduals(kt4_fit)
autoplot(ts_kt4)

dev.new()
ggtsdisplay(diff(ts_kt4,differences = 2))

dev.new()
ts_kt4 %>%
  Arima(order=c(2,2,0)) %>%
  forecast() %>%
  autoplot() + ggtitle('KT 4G 예측 모델')

##### kt 5g
kt5 <- read.csv('5g_kt.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
kt5 <- as.numeric(kt5)

df_kt5 <- data.frame(Date,kt5)

ts_kt5 <- ts(df_kt5$kt5,start=c(2013,1),frequency = 12)
end(ts_kt5)

dev.new()
ggtsdisplay(ts_kt5)

ggAcf(ts_kt5,lag=30)
ggPacf(ts_kt5,lag=30)

auto.arima(ts_kt5,seasonal = F, stepwise=F, approximation = F)
ggtsdisplay(diff(ts_kt5,differences = 2))

kt5_test <- arima(diff(ts_kt5,differences = 2),c(5,2,0))
checkresiduals(kt5_test)

ts_kt5 %>%
  Arima(order=c(5,2,0)) %>%
  forecast() %>%
  autoplot()+ ggtitle('KT 5g 예측 모델')
