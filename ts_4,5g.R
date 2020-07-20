setwd('C:/Users/seoju/Documents/ws/Pproject')
setwd('D:/Desktop/Itwill ws/Pproject')
getwd()

library(TTR)
library(forecast)
library(data.table)
library(readxl)
library(tseries)
library(ggplot2)
library(gcookbook)

## Data 붙이기
skt4 <- read.csv('4g_sk.csv',header=F,stringsAsFactors = F)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt4 <- skt4[2,]
skt4 <- as.numeric(skt4)
df_sk <- data.frame(Date,skt4)

str(df_sk)

## skt data 분기별 freq
ts_skt4 <- ts(df_sk$skt,start=c(2013,1),frequency = 12)

plot(df_sk$skt4)

start(ts_skt4)
end(ts_skt4)

dev.new()
plot.ts(ts_skt4)
abline(reg=lm(ts_skt4~time(ts_skt4)))
plot(decompose(ts_skt4))

## 각 plot 확인
dev.new()
plot(decompose(ts_skt4))
ggseasonplot(ts_skt4)
ggsubseriesplot(diff(ts_skt4, differences = 1))
gglagplot(ts_skt4)

## acf, pacf 확인
dev.new()
par(mfrow=c(2,1))
acf(diff(ts_skt4,differences = 1),lag=40)
pacf(diff(ts_skt4,differences = 1),lag=40)

dev.new()
acf(diff(ts_skt4,differences = 2))
pacf(diff(ts_skt4,differences = 2))

dev.new()
ggAcf(diff(ts_skt4,differences = 1),lag=40) + ggtitle('SKT 4G 차분 1회 시행 시 ACF')
ggPacf(diff(ts_skt4, differences = 1),lag=40) + ggtitle('SKT 4G 차분 1회 시행 시 PACF')

dev.new()
ggAcf(diff(ts_skt4,differences = 2),lag=40) + ggtitle('SKT 4G 차분 2회 시행 시 ACF')
ggPacf(diff(ts_skt4, differences = 2),lag=40) + ggtitle('SKT 4G 차분 2회 시행 시 PACF')

dev.new()
ggAcf(diff(log(ts_skt4),differences = 2),lag=40) + ggtitle('SKT 4G 로그변환 차분 2회 시행 시 ACF')
ggPacf(diff(log(ts_skt4),differences = 2),lag=40) + ggtitle('SKT 4G 로그변환 차분 2회 시행 시 PACF')


##### skt 4g auto arima
skt4g_auto <- auto.arima(ts_skt4,seasonal = F, stepwise=F, approximation = F)
dev.new()
ggtsdisplay(ts_skt4)
adf.test(ts_skt4,alternative = 'stationary',k=0)
summary(skt4g_auto)

dev.new()
ts_skt4 %>%
  log() %>% 
  Arima(order=c(5,2,0)) %>% 
  forecast(h=12) %>%
  autoplot() + ggtitle('SKT 4G 예측 모델 Auto ARIMA')

# skt 4g 차분데이터 확인
dev.new()
ggtsdisplay(diff(log(ts_skt4),differences = 2),lag=12)
adf.test(diff(log(ts_skt4),differences = 2),alternative='stationary',k=0)

# 로그+1회 차분 
skt4_fit_a <- auto.arima(diff(log(ts_skt4)))
skt4_fit_a

dev.new()
ggAcf(diff(log(ts_skt4)))
ggPacf(diff(log(ts_skt4)))
ggtsdisplay(diff(log(ts_skt4)))
skt4_fit <- arima(diff(log(ts_skt4)),c(0,1,2),seasonal=list(order=c(0,0,1),period=12))
summary(skt4_fit)

skt4_modifit <- arima(diff(log(ts_skt4)),order=c(0,1,2),seasonal = list(order=c(0,0,1),period=12))
summary(skt4_modifit)

skt4_modipred <- predict(skt4_modifit, n.ahead=5*12)

dev.new()
ts_skt4 %>% 
  log() %>% 
  Arima(c(0,1,2),seasonal = list(order=c(0,0,1),period=12)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 4G 예측 모델 log,diff=1 ARIMA')


# 로그+2회 차분 
skt4_autofit <- auto.arima(diff(log(ts_skt4)),2)
summary(skt4_autofit)

skt4_modifit <- arima(log(ts_skt4),order=c(2,2,1),)
summary(skt4_modifit)

skt4_modipred <- predict(skt4_modifit, n.ahead=5*12)

dev.new()
ts_skt4 %>% 
  log() %>% 
  Arima(c(2,2,1),) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 4G 예측 모델 log,diff=2 ARIMA')
  


#skt4_fit <- Arima(diff(log(ts_skt4),2),c(2,2,0))
summary(skt4_fit)

pred <- predict(skt4_fit,n.ahead = 12*3)
exp(exp(pred$pred))

dev.new()
ts.plot(ts_skt4,exp(pred$pred),log='y',lty=c(1,3))

dev.new()
ts_skt4 %>%
  # log() %>%
  Arima(order=c(4,2,0)) %>% 
  forecast(h=12) %>%
  autoplot() + ggtitle('SKT 4G 예측 모델 diff=2 ARIMA')


# stlf 
dev.new()
ts_skt4 %>% mstl() %>%
  autoplot() + xlab("통화량")
forecast <- (stlf(ts_skt4))
summary(forecast)

dev.new()
ts_skt4 %>% 
  stlf() %>% 
  forecast(h=12) %>% 
  autoplot()  + ggtitle('SKT 4G 예측 모델 STLF')
# https://otexts.com/fppkr/forecasting-decomposition.html

dev.new()
ts_skt4 %>%
  Arima(order=c(4,2,0)) %>% 
  forecast(h=12) %>%
  autoplot() + ggtitle('SKT 4G 예측 모델 ARIMA')
# par(new=T)  


# ETS 모델 적합도 확인 필요
dev.new()
ets_forecast <- ets(ts_skt4)
summary(ets_forecast)
ts_skt4 %>% ets() %>% forecast(h=12) %>% autoplot() + ggtitle('SKT 4G 예측 모델 ETS')

dev.new()
autoplot(diff(ts_skt4,differences = 2), series="데이터") +
  autolayer(ma(diff(ts_skt4,differences = 2), 12), series="12-MA") +
  xlab("연도") + ylab("신규 가입자") +
  ggtitle("SKT  4G 점유율") +
  scale_colour_manual(values=c("데이터"="grey","12-MA"="red"),
                      breaks=c("데이터","12-MA")) +
  guides(colour=guide_legend(title=" "))


##### skt 5g
skt5 <- read.csv('5g_sk.csv',header=F,stringsAsFactors = F)
#skt5 <- skt5[76:89]
#Date <- seq(as.Date('2019-04-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
skt5 <- as.numeric(skt5)

df_skt5 <- data.frame(Date,skt5)
ts_skt5 <- ts(df_skt5$skt5,start=c(2013,1),frequency = 12)

dev.new()
plot.ts(ts_skt5,main='SKT 5G 가입자 현황')

dev.new()
ggtsdisplay(ts_skt5,lag=15)
#ggAcf(ts_skt5,lag=15)
#ggPacf(ts_skt5,lag=15)

adf.test(ts_skt5,alternative='stationary',k=0)
adf.test(diff(ts_skt5),alternative='stationary',k=0)
auto.arima(ts_skt5,seasonal = F, stepwise=F, approximation = F)

skt5_fit_a <- auto.arima(ts_skt5,seasonal = F, stepwise=F, approximation = F)
summary(skt5_fit_a)
summary(forecast(skt5_fit_a))

dev.new()
ts_skt5 %>% 
  Arima(c(1,2,1)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 5G Arima(1,2,1) auto 예측 모델')

ggtsdisplay(diff(ts_skt5),main='5G TS ACF, PACF 그래프')

adf.test(diff(ts_skt5),alternative='stationary',k=0)

# 1번 auto
skt5_fit_a1 <- auto.arima(diff(ts_skt5))
summary(skt5_fit_a1)
fa_forecast <- forecast(skt5_fit_a1,h=12)
summary(fa_forecast)

# 2번 0,1,10
skt5_fit_a2 <- arima(diff(ts_skt5),c(0,1,10))
summary(skt5_fit_a2)
f2_forecast <- forecast(skt5_fit_a2,h=12)
summary(f2_forecast)

# 3번 0,1,6
skt_fit_a3 <- arima(diff(ts_skt5),c(0,1,6))
summary(skt_fit_a3)
f3_forecast <- forecast(skt_fit_a3,h=12)
summary(f3_forecast)

# 4번 5,2,0
skt5_fit_a4 <- arima(diff(ts_skt5),c(5,2,0))
summary(skt5_fit_a4)
f4_forecast <- forecast(skt5_fit_a4,h=12)
summary(f4_forecast)

# 5번 ets
skt5_ets <- window(ts_skt5, start=c(2013,1))
skt5_fit_ets <- ets(skt5_ets,damped=T, model='ZZZ')
f5_forecast <- forecast(skt5_fit_ets,h=12,level=c(80,95))
summary(f5_forecast)

# 2번 모델 예측 그래프
dev.new()
ts_skt5 %>% 
  Arima(c(0,1,10)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 5G 예측 모델 c(0,1,10)')

# 3번 모델 예측 그래프
dev.new()
ts_skt5 %>% 
  Arima(c(0,1,6)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('SKT 5G 예측 모델 c(0,1,6)')

# 4번 모델 예측 그래프
dev.new()
ts_skt5 %>%
  Arima(order=c(5,2,0)) %>%
  forecast(h=12) %>%
  autoplot() + ggtitle('SKT 5g MA(5,2,0) 예측 모델 ')

# ets 모델 예측 그래프
dev.new()
skt5_fit_ets %>% 
  forecast(h=12,level=c(80,95)) %>% 
  autoplot() + ggtitle('SKT 5G ETS 예측 모델')


### 모델 묶어서 보기

ts_skt55 <- window(ts_skt5, start=c(2019,4),end=c(2020,5),frequency=12)
ts_skt5_test <- window(ts_skt5,start=c(2019,4),end=c(2020,5),frequency=12)

models <- list (
  mod_arima = auto.arima(ts_skt5, ic='aicc', stepwise=FALSE),
  mod_exponential = ets(ts_skt5, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(ts_skt5, p=12, size=25),
  mod_tbats = tbats(ts_skt5, ic='aicc'),
  mod_bats = bats(ts_skt5, ic='aicc'),
  mod_stl = stlm(ts_skt5, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(ts_skt5)
)

forecasts <- lapply(models, forecast, 12)
forecasts$naive <- naive(ts_skt5, 12)

dev.new()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(ts_skt5_test, col='red')
}

models

acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)



##### 5G 예측 모델 선정해야함


##### 발표용 데이터 정리
df_skk <- data.frame(Date, skt4, skt5)
head(df_skk,30)
tail(df_skk,30)

df_ktt <- data.frame(Date, kt4, kt5)
head(df_ktt,30)
tail(df_ktt,30)


##### kt 4g

kt4 <- read.csv('4g_kt.csv',header=F,stringsAsFactors = F)
kt4 <- as.numeric(kt4)
Date <- seq(as.Date('2013-01-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),'months')
df_kt <- data.frame(Date, kt4)
ts_kt4 <- ts(df_kt$kt4,start=c(2013,1),frequency = 12)


# 원본데이터 확인
dev.new()

ts_kt4 %>% diff(lag=6) %>% ggtsdisplay()

ggtsdisplay(ts_kt4,lag=24)
ggtsdisplay(diff(ts_kt4),lag=24)
ggtsdisplay(diff(log(ts_kt4)))
plot(decompose(ts_kt4))

adf.test(ts_skt4,alternative = 'stationary',k=0)

dev.new()
ggtsdisplay(diff(log(ts_kt4),differences = 2),lag=10,main='KT 4G diff=2 ACF, PACF plot')

# auto arima로 확인
auto.arima(ts_kt4,seasonal = F, stepwise=F, approximation = F)
# ARIMA(0,2,1)

kt4g_auto <- auto.arima(ts_kt4,seasonal = F, stepwise=F, approximation = F)
kt4g_auto

summary(kt4g_auto)
write.csv(forecast(kt4g_auto),file = 'kt4g_forecast.csv')

dev.new()
ts_kt4 %>%
  #log() %>% 
  Arima(order=c(0,2,1)) %>% 
  forecast(h=12) %>%
  autoplot() + ggtitle('KT 4G 예측 모델 Auto ARIMA')


###################

kt4_fit <- arima(diff(log(ts_kt4),differences = 2),c(2,2,0),)
summary(kt4_fit)

ggAcf(diff(log(ts_kt4)))

kt4_fit_a <- auto.arima(log(ts_kt4))
kt4_fit_a

#kt4_modifit <- arima(diff(log(ts_skt4)),order=c(0,1,2),seasonal = list(order=c(0,0,1),period=12))
#summary(kt4_modifit)

#kt4_fit <- predict(kt4_fit, n.ahead=12)
kt4_fit_f <- arima(ts_kt4,order=c(0,2,1),seasonal=list(order=c(0,0,1),period=12))
summary(kt4_fit_f)

dev.new()
ts_kt4 %>% 
  #log() %>% 
  Arima(c(0,2,1),seasonal = list(order=c(0,0,1),period=12)) %>% 
  forecast(h=12) %>% 
  autoplot() + ggtitle('KT 4G 예측 모델 log,diff=1 ARIMA')

fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
kt_e1 <- tsCV(ts_kt4,fets,h=1)
kt_e2 <- tsCV(ts_kt4,farima,h=1)
mean(kt_e1^2, na.rm=T)
mean(kt_e2^2, na.rm=T)

dev.new()
ts_kt4 %>% ets() %>% forecast(h=12) %>% autoplot() + ggtitle('KT 4G 예측 모델 ets')
summary(kt_e1)
forecast(ets(ts_kt4),h=12)
summary(ets(ts_kt4))

summary(kt4_fit_a)
dev.new()
ggtsdisplay(diff(log(ts_kt4)))

kt4_fit_a2 <- arima(diff(log(ts_skt4)),c(0,1,2),seasonal=list(order=c(0,0,1),period=12))
forecast(kt4_fit_a2)
summary(kt4_fit_a2)
adf.test(diff(log(ts_kt4)),alternative = 'stationary',k=0)
Box.test(diff(log(ts_kt4)))
plot(diff(log(ts_kt4)))
kt4_pred <- predict()
auto.arima(log(ts_kt4))
fit_test <- arima(log(ts_kt4),c(1,2,2),seasonal = list(order=c(0,0,0),period=12))
summary(fit_test)
Box.test(fit_test$residuals)
test_pred <- predict(fit_test,n.ahead=12)

########################################################################

kt4_fit_ff <- arima()




########################################################################
par(mfrow=c(2,1))
ts.plot(ts_kt4,
        exp(test_pred$pred),
        log='y',
        lty=c(1,3),
        main='KT 4G 수요 예측 plot')
plot(ts_kt4, main='KT 4G 가입자 현황 plot')

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

dev.new()
ggtsdisplay(ts_kt5)

ggAcf(ts_kt5,lag=30)
ggPacf(ts_kt5,lag=30)

auto.arima(ts_kt5,seasonal = F, stepwise=F, approximation = F)
dev.new()
ggtsdisplay(diff(ts_kt5,differences = 2))

kt5_test <- arima(diff(ts_kt5,differences = 2),c(5,2,0))
dev.new()
checkresiduals(kt5_test)

dev.new()
ts_kt5 %>%
  Arima(order=c(5,2,0)) %>%
  forecast(h=12) %>%
  autoplot()+ ggtitle('KT 5g MA(5,2,0) 예측 모델')

kt5_fit <- arima(diff(ts_kt5,2),c(5,2,0))
summary(kt5_fit)
summary(kt5_test)
forecast(kt5_fit)
forecast(kt5_test)
