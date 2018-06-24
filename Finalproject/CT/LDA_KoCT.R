rm(list = ls())

#載入套件
library(jiebaR)
library(dplyr)
library(topicmodels)
library(tidyr)
library(stringr)
library(tidytext)

#查看路徑
getwd()

#讀檔的地方
Ko_CTdoc <- read.csv("Ko_CTnews")
Ko_CTdoc <- na.omit(Ko_CTdoc)

#斷詞
cutter <- worker() #叫出斷詞函數
#加入自定義詞，使其不要被刪掉
new_user_word(cutter, c("柯文哲","悠遊卡","陳景峻","賴清德","丁守中","轉型正義","賴揆","習近平","林佳龍","呂秀蓮","涂醒哲", "世大運", "段宜康", "蔡英文", "馬英九", "韓國瑜" , "中執會" , "三立" , "中天", "姚文智" , "柯P" , "陳佩琪" , "台北市長", "前總統", "前副總統", "副總統" , "蔡璧如", "金溥聰"))
Ko_CTdoc$words <- sapply(Ko_CTdoc$content %>% as.character() , function(x){tryCatch({cutter[x]}, error=function(err){})})

#讀取stop words檔
fin <- file("../Appnews/stopwords_tw.txt", open = "r")
stopwords <- readLines(fin , encoding = "UTF8")
stopwords <- c(stopwords,"表示", "報導")#stopwords 加上表示、報導
stopwords <- unique(stopwords) #刪去重複的stopwords

library(tidyr) # for unnest() 展開每一個被切開的詞
library(stringr)
word_token <- Ko_CTdoc %>%
  unnest() %>%
  select(title, words) %>% #選新聞標題與切字的欄位
  filter(!(words %in% stopwords)) %>%
  filter(!str_detect(words, "\\d")) %>% #將某一些不符合正規表達式\\d的字挑掉
  filter(nchar(words) > 1) #留下出現次數大於1的詞
# unnest words and filter words

library(tidytext)
dtm <- word_token %>%
  count(title, words) %>% 
  #count的語法是說數相同出現的詞出現幾次，然後最後會整理成有title,words和數數欄位的df
  cast_dtm(title, words, n)
#cast_dtm的語法是將word_token轉置成每一篇文章是一列，然後每一欄是每個詞出現的次數

raw.sum=apply(dtm,1,FUN=sum) #sum by raw each raw of the table #統計每一列出現幾次詞
dtm=dtm[raw.sum!=0,] #將只有0的列刪掉

#弄lDA的模型
library(topicmodels)
dtm_lda <- LDA(dtm, k = 8, control = list(seed = 1234)) #主題數4個

#畫圖
library(ggplot2)
dtm_topics <- tidy(dtm_lda, matrix = "beta") 
#將dtm_lda轉換成tidy的df(???
#beta值是表示該主題是由多少比例的那個字所構成

top_terms <- dtm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#選出每一個topic的前十個數字

# View(top_terms)
#畫圖
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text.y=element_text(colour="black", family="Heiti TC Light"))





# #以下是比較
# perplexity(dtm_lda)
# perplexity(dtm_lda4)
# 
# -(0.6*log2(0.6) + 0.4*log2(0.4))
# -(0.9*log2(0.9) + 0.1*log2(0.1))
# 
# 
# ks <- c(2, 4, 8, 14, 16, 18, 20, 24)
# #我有30個doc 我不會說我有30個的topic
# perplex <- sapply(ks, function(k){
#   lda.temp <- LDA(dtm, k =k, control = list(seed = 1109))
#   perplexity(lda.temp)
# })
# 
# 
# data_frame(k=ks, perplex=perplex) %>%
#   ggplot(aes(k, perplex)) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Evaluating LDA topic models",
#        subtitle = "Optimal number of topics (smaller is better)",
#        x = "Number of topics",
#        y = "Perplexity")
# 
# 
# 
# library(tidyr)
# 
# beta_spread <- dtm_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta) %>%
#   select(term, topic1, topic2) %>%
#   filter(topic1 > .001 | topic2 > .001) %>%
#   mutate(logratio = log2(topic1 / topic2)) %>%
#   arrange(desc(logratio))
# 
# beta_spread
# 
# beta_spread %>%
#   group_by(logratio > 0) %>%
#   top_n(20, abs(logratio)) %>%
#   ungroup() %>%
#   mutate(term = reorder(term, logratio)) %>%
#   ggplot(aes(term, logratio, fill = logratio < 0)) +
#   geom_col() +
#   coord_flip() +
#   ylab("Topic2/Topic1 log ratio") +
#   scale_fill_manual(name = "", labels = c("topic2", "topic1"),
#                     values = c("red", "lightblue")) + 
#   theme(axis.text.y=element_text(colour="black", family="Heiti TC Light"))