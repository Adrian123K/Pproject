setwd('D:/Desktop/Itwill ws/Pproject/연습')

# https://otexts.com/fppkr/graphics-exercises.html

install.packages('ggfortify')
library(forecast)
library(ggfortify)
library(ggplot2)
library(ggplot)


tute1 <- read.csv('tute1.csv',header=T)
View(tute1)

myts <- ts(tute1[,-1],start=1981,frequency = 4)
autoplot(myts,facets=T)

retail <- readxl::read_excel('retail.xlsx',skip=1)
View(retail)

myts_r <- ts(retail[,'A3349873A'],frequency = 12,start=c(1982,4))
autoplot(myts_r)
ggseasonplot(myts_r)
ggsubseriesplot(myts_r)
gglagplot(myts_r)
ggAcf(myts_r)

