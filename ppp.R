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

hd <- readLines("test.txt")
hd_exn <- sapply(hd, extractNoun, USE.NAMES=F)

hd_unl <- unlist(hd_exn)

hd_unl <- Filter(function(x){
  nchar(x)<=10
  nchar(x)>=2
}, hd_unl)

hd_unl <- gsub("\\d+","",hd_unl)
hd_unl <- gsub(" ","",hd_unl)
hd_unl <- gsub("LG","",hd_unl)
hd_unl <- gsub("G","",hd_unl)
hd_unl <- gsub("전자","",hd_unl)
hd_unl <- gsub("속보","",hd_unl)
hd_unl <- gsub("전자","",hd_unl)
hd_unl <- gsub("s","",hd_unl)

hd_txt <- readLines('head_gsub.txt')
cnt <- length(hd_txt)
for(i in 1:cnt){
  hd_unl=gsub((hd_txt[i]),"",hd_unl)
}

write(unlist(hd_unl),"headline_unlist.txt")
hd_list <- read.table("headline_unlist.txt")

wc <- table(hd_list)
head(sort(wc,decreasing=T),15)

pal=brewer.pal(5,"Set3")
dev.new()
wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=1, random.order=F, random.color=T, colors=pal)
  #legend(0.3,1,"1Q News Head", cex=0.8, fill=NA, border=NA, bg='white', text.col='red', text.font=2, box.col='red')
