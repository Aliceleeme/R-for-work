
#### 과제 20180529 #### 
1. 애플스토어 리뷰 크롤링 
# https://stackoverflow.com/questions/4731620/crawl-data-on-the-app-store
# https://hjlog.me/post/45

#installing text analytics packages 
#참고: https://statkclee.github.io/text/nlp-book.html

install.packages("tm")          # text analysis package
install.packages("SnowballC")
install.packages("stringr")

#importing text 

library(rJava)
# 자바 깔면 "JAVA_HOME cannot be determined from the Registry" 해결됨 
library(KoNLP)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(stringr)
library(wordcloud2)

useNIADic(which_dic = c("woorimalsam", "insighter"), backup = T)


#
#read.lines; text file should be saved with utf-8 encoding 
data <- readLines("Androidreview_20180529.txt", encoding="UTF-8")

#특수문자, 기호 전처리 
data <- str_replace_all(data,"[^[:graph:]]", " ")
#data <- str_replace_all(data,"[^[ㄱ-흫]]", " ") #어미를 날리는 용도인듯
data <- data[which(data != "")]

#data_corpus <- sapply(data, function(x) {paste(extractNoun(x), collapse=" ")}, USE.NAMES=F)

#library(tm)
data_corpus <- Corpus(VectorSource(data_corpus))
data_corpus <- Corpus(VectorSource(data))
data_corpus <- tm_map(data_corpus, removePunctuation)
data_corpus <- tm_map(data_corpus, removeNumbers)
data_corpus <- tm_map(data_corpus, tolower) #한국어에는 lower 없음 
data_corpus_test <- tm_map(data_corpus, stripWhitespace)

#data_stopwords <- c(stopwords("korean"), "되", "하면", "것", "그것", "음", "수", "들이", "한", "할", "있", "년", "하기", "하지", "하")

#데이터 정제 경과 확인 
inspect(data_corpus[1])
inspect(data_corpus)
inspect(docs)
summary(docs)
class(docs)

data_corpus_test <- tm_map(data_corpus, removeWords, data_stopwords)
data_corpus_test <-tm_map(data_corpus, PlainTextDocument)

#tdm utf-8 깨짐 방지 필요 
data_tdm <- TermDocumentMatrix(data_corpus_test, control=list(wordLengths=c(2,Inf)))
Encoding(data_tdm$dimnames$Terms) = 'UTF-8'
as.matrix(data_tdm)

########### 

#parameter list 
#ref: https://rdrr.io/github/haven-jeon/KoNLP/src/R/manageDic.R
#' @param category_dic_nms character vectors. category dictionary will be used. default is 'all' which means all categories.
#'    \itemize{
#'  \item general
#'  \item chemical
#'  \item language
#'  \item music
#'  \item history
#'  \item education
#'  \item society in general
#'  \item life
#'  \item physical
#'  \item information and communication
#'  \item medicine
#'  \item earth
#'  \item construction
#'  \item veterinary science
#'  \item business
#'  \item law
#'  \item plant
#'  \item buddhism
#'  \item engineering general
#'  \item folk
#'  \item administration
#'  \item economic
#'  \item math
#'  \item korean medicine
#'  \item military
#'  \item literature
#'  \item clothes
#'  \item religion normal
#'  \item animal
#'  \item agriculture
#'  \item astronomy
#'  \item transport
#'  \item natural plain
#'  \item industry
#'  \item medium
#'  \item political
#'  \item geography
#'  \item mining
#'  \item hearing
#'  \item fishing
#'  \item machinery
#'  \item catholic
#'  \item book title
#'  \item named
#'  \item electrical and electronic
#'  \item pharmacy
#'  \item art, music and physical
#'  \item useless
#'  \item ocean
#'  \item forestry
#'  \item christian
#'  \item craft
#'  \item service
#'  \item sports
#'  \item food
#'  \item art
#'  \item environment
#'  \item video
#'  \item natural resources
#'  \item industry general
#'  \item smoke
#'  \item philosophy
#'  \item health general
#'  \item proper names general
#'  \item welfare
#'  \item material
#'  \item humanities general
#' }
#' @export
#' @examples
#' \dontrun{

########### 


