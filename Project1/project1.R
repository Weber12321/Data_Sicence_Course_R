# ===== 文字雲程式 : 聯合報文字雲
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
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/UDN/爬完的結果!!")
Ko_data <- read.csv("ko_Output.csv", encoding = "big5")
Ko_data2 <- read.csv("ko_Output2.csv", encoding = "big5")

# bind all
ko_all <- rbind(Ko_data[,3:5], Ko_data2[,2:4])
# 清除NA
ko_all <- ko_all %>% na.omit()

#切開時間
ko_all <- ko_all %>% separate(V2, c("year","month","day"),"-")
ko_all <- ko_all %>% separate(day, c("date","time"), " ")
ko_all <- ko_all[with(ko_all, order(year, month, date)), ]
ko_all <- ko_all[!duplicated(ko_all$bindtext), ]
row.names(ko_all) = c(1:3112) # 由資料數重新編排號碼
Ko <- subset(ko_all, select = c(month, date, V1, bindtext))
Media <- c()
text <- c("UDN")
for( i in 1:length(Ko$bindtext)){
  Media <- rbind(Media, text)
}
Ko <- cbind(Media, Ko)
write.table(Ko , file = "C:/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/NewsCleaning/Ko_udn.csv", sep = ",")


ko1<- subset(ko_all, ko_all$month == "01", select = bindtext)
ko2<- subset(ko_all, ko_all$month == "02", select = bindtext)
ko3<- subset(ko_all, ko_all$month == "03", select = bindtext)
ko4<- subset(ko_all, ko_all$month == "04", select = bindtext)
ko5<- subset(ko_all, ko_all$month == "05", select = bindtext)

# ==== 柯文哲
# ---- Jan
docs <- Corpus(VectorSource(ko1$bindtext))
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