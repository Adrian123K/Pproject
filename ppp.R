setwd('d:/Pproject')
getwd()

library(dplyr)
library(googleVis)
library(devtools)
library(lubridate)
library(stringr)
library(ggplot2)
library(rJava)
library(RColorBrewer)
library(wordcloud)
library(KoNLP)
library(data.table)

hd <- readLines("headline1Q.txt")
hd_exn <- sapply(hd, extractNoun, USE.NAMES=F)

hd_unl <- unlist(hd_exn)

hd_unl <- Filter(function(x){
  nchar(x)<=10
  nchar(x)>=2
}, hd_unl)

hd_unl <- gsub("\\d+","",hd_unl)
hd_unl <- gsub(" ","",hd_unl)
hd_unl <- gsub("조원태","대한항공",hd_unl)
hd_unl <- gsub("조현아","대한항공",hd_unl)
hd_unl <- gsub("한진","대한항공",hd_unl)
hd_unl <- gsub("회장","대한항공",hd_unl)
hd_unl <- gsub("확진자","코로나",hd_unl)
hd_unl <- gsub("신종","코로나",hd_unl)
hd_unl <- gsub("추가","코로나",hd_unl)
hd_unl <- gsub("확진","코로나",hd_unl)
hd_unl <- gsub("환자","코로나",hd_unl)
hd_unl <- gsub("우한","코로나",hd_unl)
hd_unl <- gsub("감염","코로나",hd_unl)
hd_unl <- gsub("중국","코로나",hd_unl)
hd_unl <- gsub("확진자","코로나",hd_unl)

hd_gsub <- readLines('head_gsub2.txt')
cnt <- length(hd_gsub)
for (i in 1:cnt){
  hd_unl <- gsub((hd_gsub[i]),"",hd_unl)
}
write(unlist(hd_unl),"headline_unlist.txt")
hd_list <- read.table("headline_unlist.txt")

wc <- table(hd_list)
head(sort(wc,decreasing=T),15)
write(unlist(wc_rs),'headline_wc.csv')

pal=brewer.pal(5,"Set3")
dev.new()
wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=1, random.order=F, random.color=T, colors=pal)
  #legend(0.3,1,"1Q News Head", cex=0.8, fill=NA, border=NA, bg='white', text.col='red', text.font=2, box.col='red')
