# ===== 文字雲程式 : 中時文字雲
# ===== 處理丁守中1月至5月的文章
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

# 匯入資料組
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/CT")
data <- read.csv("Ding_CTnews.csv")
# 清除NA
data <- data %>% na.omit()
# 切開時間
# data <- data %>% separate(time, c("year","month","day"),"-")
# data <- data %>% separate(day, c("date","time"), "T")
# 排序
# data <- data[with(data, order(year ,month, date)), ]
# 清理重複資料
# data <- data[!duplicated(data$post), ]
# 移除2017資料
# data <- data[data$year == "2018",]
# row.names(data) = c(1:260) # 由資料數重新編排號碼
# 依月份建立子資料組
Di1<- subset(data , data$month == "1", select = content)
Di2<- subset(data , data$month == "2", select = content)
Di3<- subset(data , data$month == "3", select = content)
Di4<- subset(data , data$month == "4", select = content)
Di5<- subset(data , data$month == "5", select = content)
Di6<- subset(data , data$month == "6", select = content)
# Ko_data <- read.csv("Ko_report.csv")
# Yao_data <- read.csv("Yao_report.csv")

# Di_text <- matrix(data = NA, nrow = 67,ncol = 5 )
# Di_text <- cbind(data_1$bindtext, data_2$bindtext, data_3$bindtext, data_4$bindtext, data_5$bindtext)

# ----------------------------------------------------------------------------------------------------------
# 清理文本內容，刪除文本中不需要的雜訊
# data_i$bindtext <- substr(data$bindtext, 69, (nchar(as.vector(data$bindtext))-183))
# data$bindtext <- substr(data$bindtext, 69, (nchar(as.vector(data$bindtext))-183))
# 切詞
# data_1$bindtext[1:3] <- substr(data_1$bindtext[1:3], 69, (nchar(as.vector(data_1$bindtext[1:3]))-176))
# ----------------------------------------------------------------------------------------------------------
# 以上內容有待開發


# ==== 丁守中
# ---- Jan
docs <- Corpus(VectorSource(Di1$content))
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
png("Di_Jan.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=20,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()


# ---- Feb
docs <- Corpus(VectorSource(Di2$content))
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
png("Di_Feb.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=20,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()


# ---- Mar
docs <- Corpus(VectorSource(Di3$content))
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
png("Di_Mar.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=20,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()


# ---- Apr
docs <- Corpus(VectorSource(Di4$content))
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
png("Di_Apr.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=10,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()



# ---- May
docs <- Corpus(VectorSource(Di5$content))
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
png("Di_May.png", width = 300, height = 300 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=40,
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
png("Di_2018.png", width = 400, height = 400 )

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=80,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
dev.off()


# 報導量
Di_textNum <- rbind(nrow(Di1),nrow(Di2),nrow(Di3),nrow(Di4),nrow(Di5))
Di_textNum %>% as.data.frame()
colnames(Di_textNum) <- "丁守中報導量"
rownames(Di_textNum) <- c("Jan","Feb","Mar","Apr","May")
Di_textNum
# 輸出
write.table(Di_textNum, file = "Di_textNum.CSV", sep = ",")