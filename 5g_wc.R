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
library(wordcloud2)

fg <- readLines("5gsearching.txt")
fg_exn <- sapply(fg, extractNoun, USE.NAMES=F)
fg_unl <- unlist(fg_exn)

head(fg_unl,20)

fg_unl <- Filter(function(x){
  nchar(x)<=5
  nchar(x)>=2
}, fg_unl)

fg_unl <- gsub("\\\"titleGtitle","",fg_unl)
fg_unl <- gsub("titleGtitle","",fg_unl)
fg_unl <- gsub("titleGtitle","",fg_unl)
fg_unl <- gsub("\\d+","",fg_unl)
fg_unl <- gsub(" ","",fg_unl)


hd_gsub <- readLines('head_gsub2.txt')
cnt <- length(hd_gsub)
for (i in 1:cnt){
  hd_unl <- gsub((hd_gsub[i]),"",hd_unl)
}

head(fg_unl,25)

write(unlist(fg_unl),"5g_unlist.txt")
fg_list <- read.table("5g_unlist.txt")

wc <- table(fg_list)
wc <- sort(wc,decreasing=T)

pal=brewer.pal(5,"Set3")
dev.new()
wordcloud2(data=wc,color = pal,size=3)

wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=1, random.order=F, random.color=T, colors=pal)
#legend(0.3,1,"1Q News Head", cex=0.8, fill=NA, border=NA, bg='white', text.col='red', text.font=2, box.col='red')
