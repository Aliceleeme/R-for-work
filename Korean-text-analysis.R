
# Written by Jihye Lee
# 2018-05-29 

####Installing text analytics packages 
#참고: https://statkclee.github.io/text/nlp-book.html

#install.packages("tm")    # text mining 
#install.packages("SnowballC")
#install.packages("stringr")

####importing text 
library(rJava)
#자바 설치하면 "JAVA_HOME cannot be determined from the Registry" 해결됨 
library(KoNLP)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(stringr)
library(wordcloud2)

#library(KoNLP)와 연계해서 사용하는 useNIADic 
useNIADic(which_dic = c("woorimalsam", "insighter"), backup = T)
#튜토리얼 참고; https://kbig.kr/portal/kbig/knowledge/files/bigdata_report.page?bltnNo=10000000016451

#read.lines; text file should be saved with utf-8 encoding (not ASNI)
data <- readLines("Androidreview_20180611.txt", encoding="UTF-8")

####특수문자, 기호 전처리 
data <- str_replace_all(data,"[^[:graph:]]", " ")
#data <- str_replace_all(data,"[^[ㄱ-흫]]", " ") #어미를 날리는 용도인듯
data <- data[which(data != "")]

inspect(data[1])

####명사 추출해서 데이터 상황 알아보기 
extractNoun(data[1])
extractNoun(data[2])
extractNoun(data[3])


####오타교정하기(특정 한국어 단어, 문장 바꾸기) 
for (j in seq(docs))
{
  docs[[j]] <- gsub("not answered", "not_answered", docs[[j]])     # pattern, replacement, dataname 순서로 작성 
  docs[[j]] <- gsub("not solved", "not_solved", docs[[j]])
  docs[[j]] <- gsub("qpp", "app", docs[[j]])  }     # 상황에 따라 gsub 통해서 오타 교정(그러나 비추천)


#data_corpus <- sapply(data, function(x) {paste(extractNoun(x), collapse=" ")}, USE.NAMES=F)

####Text mining 
#Vcorpus로 고민해보는 것도 좋지 않을까 

#library(tm)
data_corpus <- Corpus(VectorSource(data_corpus))
data_corpus <- Corpus(VectorSource(data))
data_corpus <- tm_map(data_corpus, removePunctuation)
data_corpus <- tm_map(data_corpus, removeNumbers)
data_corpus <- tm_map(data_corpus, tolower) #한국어에는 lower 없음 
data_corpus_test <- tm_map(data_corpus, stripWhitespace)
data_stopwords <- c(stopwords("english"), "되", "하면", "것", "그것", "음", "수", "들이", "한", "할", "있", "년", "하기", "하지", "하")
#stopwords를 korean으로 바꾸면 작동 안함

#데이터 정제 경과 확인 
#inspect(data_corpus[1])
#inspect(data_corpus)
#inspect(docs)
#summary(docs)
#class(docs)

data_corpus_test <- tm_map(data_corpus, removeWords, data_stopwords)
data_corpus_test <-tm_map(data_corpus, PlainTextDocument)

#tdm utf-8 깨짐 방지를 위해서는 일단 메모장에서 utf-8로 텍스트를 저장할 것 
data_tdm <- TermDocumentMatrix(data_corpus_test, control=list(wordLengths=c(2,Inf)))

#data_tdm이 오류가 나는 경우, TDM이 인자로 Corpus만 받기 때문임을 기억하고 corpus 변환 후 tdm으로 재변환 요망
#ref: http://lightblog.tistory.com/52 
doc <- Corpus(VectorSource(data_corpus_test))
tdm <- TermDocumentMatrix(doc)

#Encoding(data_tdm$dimnames$Terms) = 'UTF-8'

inspect(tdm)
tdm <- as.matrix(tdm)
tdm <- rowSums(tdm)
tdm <- tdm3[order(tdm, decreasing = T)]
#str(tdm)
tdm

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

