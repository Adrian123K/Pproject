setwd('d:/Pproject')

install.packages('TTR')
install.packages('forecast')
library(TTR)
library(forecast)

sample <- read.csv('5g 가입자.csv',head=F)
str(sample)
st <- t(sample)

colnames(st) <- c('Date','SKT','KT','LG','MVNO')
st[,2:5] <- as.integer(st[,2:5])
st[,1] <- as.Date(st[,1],'%Y-%m')
st

str(st)


par(mfrow=c(2,1))
dev.new()
ts_st1 <- ts(st[,1])
ts_st2 <- ts(st[,2])
ts_st3 <- ts(st[,3])
ts_st4 <- ts(st[,4])
plot.ts(ts_st1)
plot.ts(ts_st1)
plot.ts(ts_st1)
plot.ts(ts_st1)

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
