# ===== 文字雲程式 : 蘋果文字雲
# ===== 處理 柯文哲1月至5月的文章
# ------------------------------------------
# 
# 匯入套件
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(magrittr)
library(RColorBrewer)
library(wordcloud)

# ---- 資料清理 --------------------------------------------------------------
# 匯入資料組
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/Apple")
data <- read.csv("Ko_Appnews.csv")


# bind all
# Di_all <- rbind(Di_data[,2:4], Di_data2[,2:4], Di_data3[,2:4]) 
data <- data %>% na.omit()

#切開時間
# Di_all <- Di_all %>% separate(V1, c("year","month","day"),"-")
# Di_all <- Di_all %>% separate(day, c("date","time"), " ")
# Di_all <- Di_all[with(Di_all, order(year ,month, date)), ]
data <- data[!duplicated(data$content), ]
# Di_all<- Di_all[Di_all$year == "2018",]
# row.names(Di_all) = c(1:687) # 由資料數重新編排號碼

# 依月份建立子資料組
Ko1<- subset(data , data$month == "1", select = content)
Ko2<- subset(data , data$month == "2", select = content)
Ko3<- subset(data , data$month == "3", select = content)
Ko4<- subset(data , data$month == "4", select = content)
Ko5<- subset(data , data$month == "5", select = content)

# ==== 丁守中
# ---- Jan
docs <- Corpus(VectorSource(Ko1$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_Jan.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=100,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()


# ---- Feb
docs <- Corpus(VectorSource(Ko2$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_Feb.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=60,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()


# ---- Mar
docs <- Corpus(VectorSource(Ko3$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_Mar.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=100,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()


# ---- Apr
docs <- Corpus(VectorSource(Ko4$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_Apr.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=90,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()



# ---- May
docs <- Corpus(VectorSource(Ko5$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_May.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=170,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()



# ---- 2018
docs <- Corpus(VectorSource(data$content))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；",">","<","＜","＞")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_doc(docs)
clean_word_doc <- function(docs){
  clean_words <- c("分享","記者","攝影","提及","表示","報導","我們","他們","的","也","都","就","與","但","是","在","和","及","為","或","且","有","含")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}
docs <- clean_word_doc(docs)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
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
# 畫出文字雲
# 儲存文字雲圖片
png("Ko_2018.png", width = 400, height = 400 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=400,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()

】