# ===== 文字雲程式
# ===== 處理丁守中1月至5月的文章
# ------------------------------------------
# 遇到問題#1 : 
# 每篇首尾都是規律的廢話，不過有少部分沒有廢話，所以無法用substr()一次清乾淨
# 解決方法#1 :
# 用toSpace函數與tm_map功能換掉廢話，不過又衍生新的問題(已解決)


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
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/LTN/Di")
data_1 <- read.csv("Di_JanLtnNews", encoding = "big5")
data_2 <- read.csv("Di_FebLtnNews", encoding = "big5")
data_3 <- read.csv("Di_MarLtnNews", encoding = "big5")
data_4 <- read.csv("Di_AprLtnNews", encoding = "big5")
data_5 <- read.csv("Di_MayLtnNews", encoding = "big5")

# 清除格式有誤的資料
# data_1<- data_1[-4,]
# data_2<- data_2[-8,]
# data_3<- data_3[-34,]
# data_5<- data_5[-1,]
# data_5<- data_5[-57,]

# 重遺漏資訊
data_1 <- data_1 %>% na.omit()
data_2 <- data_2 %>% na.omit()
data_3 <- data_3 %>% na.omit()
data_4 <- data_4 %>% na.omit()
data_5 <- data_5 %>% na.omit()
# 移除重複性資料
data_1 <- data_1[!duplicated(data_1$bindtext), ]
data_2 <- data_2[!duplicated(data_2$bindtext), ]
data_3 <- data_3[!duplicated(data_3$bindtext), ]
data_4 <- data_4[!duplicated(data_4$bindtext), ]
data_5 <- data_5[!duplicated(data_5$bindtext), ]

Di_all <- rbind(data_1, data_2, data_3, data_4, data_5)

Di <- subset(Di_all, select = c(V1, bindtext))
Media <- c()
text <- c("LTN")
for( i in 1:length(Di$bindtext)){
  Media <- rbind(Media, text)
}
Di <- cbind(Media, Di)
write.table(Di, file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/NewsCleaning/Di_ltn.csv", sep = ",")

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


#====== 1月切詞
docs <- Corpus(VectorSource(data_1$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()


#====== 2月切詞
docs <- Corpus(VectorSource(data_2$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
png("Di_Feb.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

#====== 3月切詞
docs <- Corpus(VectorSource(data_3$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
png("Di_Mar.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

#====== 4月切詞
docs <- Corpus(VectorSource(data_4$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
png("Di_Apr.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

#====== 5月切詞
docs <- Corpus(VectorSource(data_5$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
png("Di_May.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

#====== 2018
# bind

docs <- Corpus(VectorSource(Di_all$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})
# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs, toSpace, " 為達最佳瀏覽效果，建議使用 Chrome、Firefox 或 Internet Explorer 10")
docs <- tm_map(docs, toSpace, "新聞送上來！快加入自由電子報APP、LINE好友  2018年6月13日‧星期三‧戊戌年四月卅 注目新聞 1 中國卡車罷工延燒 司機高喊「打倒共產黨」！ 2 新版身分證票選出爐 設計師魯少綸奪首獎！ 3 洋腸又來！夜店外帶18歲妹 網站取名「台女很容易」... 4 侯友宜妻1塊土地99個門牌 蘇貞昌：太荒唐 5 連俞涵超正弟媳曝光…根本巨乳林志玲！")
docs <- tm_map(docs, toSpace, "以上版本的瀏覽器。 爆")
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
png("Di_2018.png", width = 300, height = 300)

wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=200,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

# ==== 報導量
Di_textNum <- rbind(nrow(data_1),nrow(data_2),nrow(data_3),nrow(data_4),nrow(data_5) )
Di_textNum %>% as.data.frame()
colnames(Di_textNum) <- "丁守中報導量"
rownames(Di_textNum) <- c("Jan","Feb","Mar","Apr","May")
Di_textNum
# 輸出
write.table(Di_textNum, file = "Di_textNum.CSV", sep = ",")


