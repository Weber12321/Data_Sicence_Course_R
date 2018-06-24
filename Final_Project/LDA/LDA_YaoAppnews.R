#LDA的部分，以姚文智為例

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
setwd("/Users/sunny/Documents/GitHub/courseR/final_project")

#讀檔的地方
testdoc <- read.csv("./Appnews/Yao_Appnews")
testdoc <- na.omit(testdoc)

#斷詞
cutter <- worker()
new_user_word(cutter, c("柯建銘",
                        "吳敦義",
                        "吳茂昆",
                        "吳音寧",
                        "管中閔",
                        "拔管",
                        "挺管",
                        "深澳",
                        "燃煤",
                        "蔣萬安",
                        "張顯耀",
                        "柯文哲",
                        "悠遊卡",
                        "陳景峻",
                        "賴清德",
                        "丁守中",
                        "轉型正義",
                        "賴揆",
                        "習近平",
                        "林佳龍",
                        "呂秀蓮",
                        "涂醒哲",
                        "世大運",
                        "段宜康",
                        "蔡英文",
                        "馬英九",
                        "韓國瑜",
                        "中執會",
                        "三立",
                        "中天",
                        "姚文智",
                        "柯P",
                        "陳佩琪",
                        "台北市長",
                        "前總統",
                        "前副總統",
                        "副總統",
                        "蔡璧如",
                        "金溥聰",
                        "台灣價值",
                        "勤政清廉愛鄉土",
                        "意識形態",
                        "月票",
                        "雙城論壇",
                        "陳昭南",
                        "侯友宜",
                        "重陽",
                        "敬老",
                        "建管處",
                        "深澳電廠",
                        "陳彥伯",
                        "燈節",
                        "燈節廠商",
                        "綠營",
                        "藍營",
                        "蘋果",
                        "蘋果日報"
))
testdoc$words <- sapply(testdoc$content %>% as.character() , function(x){tryCatch({cutter[x]}, error=function(err){})})

#讀取stop words檔
fin <- file("./Appnews/stopwords_tw.txt", open = "r")
stopwords <- readLines(fin , encoding = "UTF8") #stopwords 加上表示、報導
stopwords <- c(stopwords,"表示", "報導", "新聞", "重點")#stopwords 加上表示、報導
stopwords <- unique(stopwords) #刪去重複的stopwords

library(tidyr) # for unnest() 展開每一個被切開的詞
library(stringr)

word_token <- testdoc %>%
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
dtm_lda <- LDA(dtm, k = 8, control = list(seed = 1234)) #主題數8個


#畫圖
library(ggplot2)
dtm_topics <- tidy(dtm_lda, matrix = "beta") 
#beta值是指該詞所佔某一個主題的比例。

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
