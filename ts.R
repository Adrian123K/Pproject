setwd('d:/Pproject')
setwd('d:/Desktop/Itwill ws/Pproject')
setwd('C:/Users/seoju/Documents/ws/Pproject')

install.packages('TTR')
install.packages('forecast')
install.packages('readxl')
install.packages('tseries')
library(TTR)
library(forecast)
library(data.table)
library(readxl)
library(tseries)
library(ggplot2)

sample <- read.csv('5g 가입자.csv',header=F, stringsAsFactors=F)

SKT <- t(sample[2,])
KT <- t(sample[3,])
LG <- t(sample[4,])
MVNO <- t(sample[5,])

Date <- seq(as.Date('2019-04-01','%Y-%m-%d'),as.Date('2020-05-01','%Y-%m-%d'),by='month')
SKT <- as.numeric(SKT)
KT <- as.numeric(KT)
LG <- as.numeric(LG)
MVNO <- as.numeric(MVNO)
st <- data.frame(Date,SKT, KT,LG, MVNO)
st

#date <- seq(as.Date('2019-04','%Y-%m'),as.Date('2020-05','%Y-%m'),'month')



ts_skt <- ts(st$SKT,start=c(2019,4),frequency = 12)
end(ts_skt)

dev.new()
par(mfrow=c(2,1))
plot.ts(ts_skt)
abline(reg=lm(ts_skt~time(ts_skt)))
plot(decompose(ts_skt))

adf.test(ts_skt,alternative='stationary',k=0)
adf.test(diff(log(ts_skt)),alternative='stationary',k=0)
dev.new()
plot.ts(diff(log(ts_skt)))

skt_d1 <- diff(log(ts_skt),differences = 1)
skt_d2 <- diff(log(ts_skt),differences = 2)
dev.new()
par(mfrow=c(2,2))
plot.ts(ts_skt)
plot.ts(diff(log(ts_skt)))
plot.ts(skt_d1)
plot.ts(skt_d2)

dev.new()
par(mfrow=c(2,1))
acf(log(ts_skt))
pacf(log(ts_skt))
ggAcf(diff(log(ts_skt)),lag=7)
ggPacf(diff(log(ts_skt)),lag=7)

ggAcf(ts_skt,lag=14)
ggPacf(ts_skt,lag=14)

dev.new()
par(mfrow=c(2,2))
acf(ts_skt)







###


ts_kt <- ts(st[,3])
ts_lg <- ts(st[,4])
ts_mvno <- ts(st[,5])



plot.ts(ts_kt)
plot.ts(ts_lg)
plot.ts(ts_mvno)

skt <- SMA(ts_st1, n=3)
kt <- SMA(ts_st2, n=3)
lg <- SMA(ts_st3, n=3)
mvno <- SMA(ts_st4, n=3)

par(mfrow=c(4,1))
plot.ts(skt)
plot.ts(kt)
plot.ts(lg)
plot.ts(mvno)

skt_diff <- diff(ts_st1, differences = 1)
kt_diff <- diff(ts_st2, differences = 1)
lg_diff <- diff(ts_st3, differences = 1)
mvno_diff <- diff(ts_st4, differences = 1)

par(mfrow=c(4,1))
plot.ts(skt_diff)
plot.ts(kt_diff)
plot.ts(lg_diff)
plot.ts(mvno_diff)

dev.new()
par(mfrow=c(4,1))
acf(skt_diff, lag.max = 20)
acf(kt_diff, lag.max = 20)
acf(lg_diff, lag.max = 20)
acf(mvno_diff, lag.max = 20)
