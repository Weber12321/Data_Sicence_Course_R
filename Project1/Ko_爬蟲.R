# #搜尋頁面網址爬蟲
# rm(list = ls())
# 
 library(httr)
 library(rvest)
# 
# udn_head_url <- "https://udn.com/search/result/2/%E6%9F%AF%E6%96%87%E5%93%B2/"
# url.link <- c()
# for( i in 7:169) {
#   url <- paste0(udn_head_url , i )
#   url.link <- rbind(url.link , url )
# }
# 
# 
# #爬每一篇新聞的網址
# links <- c()
# for ( n in 1:length(url.link)) {
#   print(n)
#   doc <- read_html(url.link[n])
#   css <- "#search_content > dt > a"
#   node <- html_nodes(doc, css)
#   link <- html_attrs(node) 
#   links <- rbind(links, link)
#   Sys.sleep(sample(1:5, 1))
# }
# 
# #跑到151筆資料出現HTTP error 503
# 
# for ( r in 152:163) {
#   print(r)
#   doc <- read_html(url.link[r])
#   css <- "#search_content > dt > a"
#   node <- html_nodes(doc, css)
#   link <- html_attrs(node) 
#   links <- rbind(links, link)
#   Sys.sleep(sample(1:5, 1))
# }
# 
# news_links <- links[1:3240] 
# news_links <- unlist(news_links)
# news_links <- matrix(news_links , byrow = T , ncol = 2)
# news_links <- news_links[ , 1 ] 
# 
# write.csv(news_links, file = "newspage_links")

#爬每一篇新聞的標題、時間、內文
#這邊要寫成function

rm(list = ls())

library(httr)
library(rvest)
library(yaml)
library(magrittr)

getwd()
setwd("/Users/sunny/Documents/GitHub/courseR/final_project")

news_links = read.csv( file = "Ko_udnlinks")
colnames(news_links) <- c("ID","URL")
news_links$URL <- news_links$URL %>% as.character()

#爬每一篇新聞的標題、時間、內文
#這邊要寫成function

length(news_links$URL)
unique(news_links$URL)


title <- c()
time <- c()
text <- c()


FindNews <- function(URL){
  
  t.doc <-  read_html(URL %>% as.character()) 
  title_node <- html_node(t.doc, "#story_art_title")
  t_title <- html_text(title_node)
  title <- rbind(title, t_title)
  
  time_node <- html_node(t.doc, "#story_bady_info > div > span")
  t_time <- html_text(time_node)
  time <- rbind(time, t_time)
  
  text_node <- html_nodes(t.doc, "#story_body_content > p")
  t_text <- html_text(text_node)
  bindtext <- ""
  for(i in c(1:length(t_text))){
    bindtext <- paste(bindtext,t_text[i])  
  }
  
  return(cbind(title, time, bindtext))
  
}


## 只爬500筆
Output <- data.frame()
for( i in 1:500){
  print(i)
  data <- FindNews(news_links[i,2])
  Output <- rbind(Output,data)
  Sys.sleep(sample(1:10,1))
  
}

Output5 <- data.frame()
for( i in 2238:3240){
  print(i)
  data <- FindNews(news_links[i,2])
  Output5 <- rbind(Output5,data)
  Sys.sleep(sample(1:10,1))
  
}

## 2237 掛掉

#持續少量分次爬

write.csv(Output5,file="Output5")
# 跑到1886
#爬完後cbind標題、時間、內文和連結
Ko_udnnews <- cbind(Output, news_links$URL)

#存檔
write.csv(Ko_udnnews, file = "Ko_udnnews")

data1 <- read.csv("Output1")
data2 <- read.csv("Output2")
data3 <- read.csv("Output3")
data4 <- read.csv("Output4")
data <- rbind(data1,data2,data3,data4)
write.csv(data,"ko_Output.csv")
write.csv(Output5,"ko_Output2.csv")

s1 <- read.csv("ko_Output.csv")
s2 <- read.csv("ko_Output2.csv")
test <- data[,c(2,3,4)] %>% unique

