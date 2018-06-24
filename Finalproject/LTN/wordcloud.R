# ===== 文字雲程式
# 文字清理
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(magrittr)
library(RColorBrewer)
library(wordcloud)
# load data csv
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/LTN/Yao")
data <- read.csv("Yao_JanLtnNews", encoding = "big5")

# 清理文本內容，刪除文本中不需要的雜訊
data$bindtext <- substr(data$bindtext, 69, (nchar(as.vector(data$bindtext))-183))
# text <- data$bindtext[1]

# 切詞
docs <- Corpus(VectorSource(data$bindtext))
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern," ",x))
})

# 刪去單詞贅字、英文字母、標點符號、數字與空格
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, toSpace, "、")
docs <- tm_map(docs, toSpace, "，")
docs <- tm_map(docs, toSpace, "。")
docs <- tm_map(docs, toSpace, "！")
docs <- tm_map(docs, toSpace, "「")
docs <- tm_map(docs, toSpace, "（")
docs <- tm_map(docs, toSpace, "」")
docs <- tm_map(docs, toSpace, "）")
docs <- tm_map(docs, toSpace, "\n")
docs <- tm_map(docs, toSpace, "；")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "及")
docs <- tm_map(docs, toSpace, "為")
docs <- tm_map(docs, toSpace, "或")
docs <- tm_map(docs, toSpace, "且")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)

# 匯入自定義字典
mixseg = worker()
segment <- c("柯文哲","姚文智","丁守中","台北市長","選舉","候選人","台灣","選票","柯市長","民進黨","國民黨","台北市民","市民")
new_user_word(mixseg,segment)

#有詞頻之後就可以去畫文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
# 畫出文字雲
# 儲存文字雲圖片
png("Yao_Jan.png", width = 500, height = 500 )
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=50,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

dev.off()

