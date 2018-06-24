library(stringr)
library(jiebaR)
<<<<<<< HEAD:Finalproject/moodAnalysis.R
pos<-read.csv('NTUSD_positive_UTF-8.csv',header=F,stringsAsFactors=FALSE,encoding = "unicode")
weight <- rep(1, length(pos[,1])) #正面情感词语赋权重为1
=======
library(jiebaRD)

#### 使用情感字典 <NTUSD> ####
pos<-read.csv('NTUSD_positive_unicode.csv',header=F,stringsAsFactors=FALSE,encoding = "unicode")
weight <- rep(1, length(pos[,1])) #正面情感詞語權重為1
>>>>>>> d5c79457dd098ee4a380af3bc21e5e581fa45e4d:Finalproject/Sentiment/moodAnalysis.R
pos <- cbind(pos, weight)
neg<-read.csv('NTUSD_negative_unicode.csv',header=F,stringsAsFactors=FALSE,encoding = "unicode")
weight <- rep(-1, length(neg[,1])) #負面情感詞語權重為-1
neg <- cbind(neg, weight)
posneg<-rbind(pos,neg)
colnames(posneg)<-c('term','weight')
# 關閉pos 及 neg
rm(pos)
rm(neg)

# 建立切分詞字典<NTUSD加入字典>及環境
user<-posneg[,'term']
w1<-worker()
new_user_word(w1,user)



#### 文字清理 ####

# 使用測試資料
Data <- read.csv("Di_JanFebNews.csv")

library(NLP)
library(tm)

docs <- Corpus(VectorSource(Data$V3))
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


# 開始切詞及計算情感分數

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], w1))
}
seg = lapply(docs, jieba_tokenizer)

sapply(seg,function(d){
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



