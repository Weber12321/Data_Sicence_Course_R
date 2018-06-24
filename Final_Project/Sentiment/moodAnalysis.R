library(jiebaRD)
library(jiebaR)

#### 使用情感字典 <NTUSD> ####
pos<-read.csv('NTUSD_positive_unicode.csv',header=F,stringsAsFactors=FALSE,encoding = "unicode")
weight <- rep(1, length(pos[,1])) #正面情感詞語權重為1
pos <- cbind(pos, weight)
neg<-read.csv('NTUSD_negative_unicode.csv',header=F,stringsAsFactors=FALSE,encoding = "unicode")
weight <- rep(-1, length(neg[,1])) #負面情感詞語權重為-1
neg <- cbind(neg, weight)
posneg<-rbind(pos,neg)
colnames(posneg)<-c('term','weight')
# 關閉pos、neg及weight
rm(pos)
rm(neg)
rm(weight)

# 建立切分詞字典<NTUSD加入字典>及環境
user<-posneg[,'term']
w1<-worker()
new_user_word(w1,user)


#### 文字清理 ####

# 使用測試資料
# 要算情緒的檔案從這邊丟入
Data <- read.csv("FB_result/Di_report.csv")

# 套件引用
library(NLP)
library(stringr)
library(tm)
library(plyr)

# 文字清理
# 選擇post欄位(輸入要做情緒分析的資料) >> 更改 Data$ ???
docs <- Corpus(VectorSource(Data$post))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})

clean_doc <- function(docs){
  clean_words <- c("[A-Za-z0-9]","、","《","『","』","【","】","／","，","。","！","「","（","」","）","\n","；")
  for(i in 1:length(clean_words)){
    docs <- tm_map(docs,toSpace, clean_words[i])
  }
  return(docs)
}

docs <- clean_doc(docs)


# 開始切詞

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], w1))
}

seg = lapply(docs, jieba_tokenizer)

# 計算情感分數
sentiment_point <- sapply(seg,function(d){
  res <- d
  temp<-data.frame()
  temp[c(1:length(res)),1]<-rep('1.text' ,length(res)) #id
  temp[c(1:length(res)),2]<-res[1:length(res)]#term
  colnames(temp)<-c('id','term')
  temp<-join(temp,posneg,by='term')
  temp<-temp[!is.na(temp$weight),]
  Ct_pos <- temp[temp$weight==1,3] %>% length()
  Ct_neg <- temp[temp$weight==-1,3] %>% length()
  return(Ct_pos/(Ct_pos+Ct_neg))
})

# 算分時候可能出現NA(此文章沒有正面也沒有負面)將NA改成0.5
sentiment_point[sentiment_point %>% is.na] = 0.5

# 算完分數的資料放回dataframe
Data <- cbind(Data,sentiment_point)

# 此時可以輸出Data這個結果


