# ===== TFIDF程式 : 臉書
# ===== 處理柯文哲1月至5月的文章
# ------------------------------------------
# 
# 匯入套件
library(tibble)
library(dplyr)
library(NLP)
library(tm)
library(stats)
library(proxy)
library(jiebaRD)
library(jiebaR)
library(magrittr)
library(slam)
library(Matrix)
library(tidytext)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

# ===== 資料清理
# 匯入資料組
setwd("/Users/Weber/Documents/GitHub/Weber1234/HW_5")
data <- read.csv("Ko_report.csv")
data <- data %>% na.omit()
# 切開時間
data <- data %>% separate(time, c("year","month","day"),"-")
data <- data %>% separate(day, c("date","time"), "T")
# 排序
data <- data[with(data, order(year ,month, date)), ]
# 清理重複資料
data <- data[!duplicated(data$post), ]
# 移除2017資料
data <- data[data$year == "2018",]
row.names(data) = c(1:138) # 由資料數重新編排號碼
# 依月份建立子資料組

# ===== 切詞
# ---- Jan
docs <- Corpus(VectorSource(data$post))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)
# 匯入自定義字典
mixseg = worker()
segment <- c("柯文哲","姚文智","丁守中","台北市長","選舉","候選人","台灣","選票","柯市長","民進黨","國民黨","台北市民","市民")
new_user_word(mixseg,segment)
# 有詞頻之後就可以去畫文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
# 清除單字
for(i in c(1:length(freqFrame$Var1))){
  if((freqFrame$Var1[i] %>% as.character %>% nchar) == 1){
    freqFrame[i,] <- NA
  }
}
freqFrame <- na.omit(freqFrame)
d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
tf <- as.matrix(tdm)
DF <- tidy(tf)
head(DF, 10)

# ===== 建立TF-IDF
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)

doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}
findZeroId = as.matrix(apply(doc.tfidf, 1, sum))
# tfidfnn = doc.tfidf[-which(findZeroId == 0),]
head(doc.tfidf, 10)

# 畫個助畫柱狀圖
freq=rowSums(as.matrix(doc.tfidf))
high.freq=tail(sort(freq),n=20)
high.freq
hfp.df=as.data.frame(sort(high.freq))
names(hfp.df) <- "frequence"
hfp.df$names <- rownames(hfp.df) 
hfp.df <- hfp.df[-c(14,16,17,19,20),]
hfp.df <- hfp.df[order(hfp.df$frequence),]
rownames(hfp.df) <- c(1:15) 


ggplot(hfp.df, aes(hfp.df$names, hfp.df$frequence)) +
  geom_bar(stat="identity", fill="olivedrab", colour="black") +
  xlab("Frequency") + ylab("Terms") +
  ggtitle("Term frequencies")

# 順便畫一個文字雲來看看結果
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=30,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

