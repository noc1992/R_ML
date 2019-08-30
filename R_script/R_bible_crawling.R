library(rvest)
library(dplyr)
library(KoNLP)
library(RColorBrewer)
library(ggplot2)
library(wordcloud2)
library(wordcloud)
library(tm)
useSejongDic()

base_url ='https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=rev&chap='
res_url ='&sec=1&range=all&keyword=#focus'
page = 1:22

txt = c()

for (page_num in page){
  bible_url <- paste0(base_url,page_num,res_url)
  html <- read_html(bible_url)
  temp <- repair_encoding(html_text(html_nodes(html,'#tdBible1 span')), from='utf-8')
  txt <- c(txt, temp)
}

text_bi <- as.data.frame(txt)
View(text_bi)

pword <- sapply(txt,extractNoun,USE.NAMES = F)
View(pword)
data <- Filter(function(x){nchar(x)>=2},data)
data
data <- unlist(pword)
View(data)
data <- gsub("\\d+","",data)
data <- gsub("\\n","",data)
data <- gsub(" ","",data)
data <- gsub("\n","",data)
data
data_cnt <- table(data)

x <- head(sort(data_cnt, decreasing=T), 50)
y <- head(sort(data_cnt, decreasing=T), 15)

y_char <- barplot(y, las=2,col="black" , ylim=c(0,65))
text(x=y_char, y=y-0.5, labels=paste(y,"회"), col='white',cex=0.7)

wordcloud(x, size=0.3, color = "random-light",backgroundColor = 'black',rotateRatio = 0.75)

wordcloud2(x, size=0.3, color = "random-light",backgroundColor = 'black',rotateRatio = 0.75)






