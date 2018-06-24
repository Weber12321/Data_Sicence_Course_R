# #搜尋頁面網址爬蟲
# rm(list = ls())
# 
# library(httr)
# library(rvest)
# getwd()
# setwd( "/Users/sunny/Documents/GitHub/courseR/final_project")
# 
# Ding_head_url <- "https://udn.com/search/result/2/%E4%B8%81%E5%AE%88%E4%B8%AD/"
# 
# FindUdnPage <- function(x){
#   url <- paste0(Ding_head_url , x )
#   Di_udnlink <- c()
#   Di_udnlink <- rbind(Di_udnlink, url)
# }
# 
# #五月新聞網頁的連結
# Di_Maylink <- data.frame()
# for( i in 4:22){
#   print(i)
#   data <- FindUdnPage(i)
#   Di_Maylink <- rbind(Di_Maylink , data)
# }
# 
# #三月、四月新聞網頁的連結
# Di_MarAprlink <- data.frame()
# for( i in 22:30){
#   print(i)
#   data <- FindUdnPage(i)
#   Di_MarAprlink <- rbind(Di_MarAprlink , data)
# }
# 
# #二月、一月新聞網頁的連結
# Di_JanFeblink <- data.frame()
# for( i in 30:39){
#   print(i)
#   data <- FindUdnPage(i)
#   Di_JanFeblink <- rbind(Di_JanFeblink , data)
# }
# 
# #爬網頁裡每一篇新聞的網址的function
# FindUdnNews <- function(URL){
#   #URL <- Di_JanFeblink[1,] 
#   URL <- URL %>% as.character()
#   doc <- read_html(URL)
#   css <- "#search_content > dt > a"
#   node <- html_nodes(doc, css)
#   link <- html_attrs(node) 
#   news_links <- unlist(link)
#   news_links <- matrix(news_links , byrow = T , ncol = 2)
#   news_links <- news_links[ , 1 ] 
#   news_links <- news_links %>% data.frame() %>% return()
# }
# 
# #一二月每篇新聞的網址
# output <- data.frame()
# for ( i in 1:length(Di_JanFeblink$V1)) {
#   print(i)
#   run_links <- FindUdnNews(Di_JanFeblink[ i , 1])
#   output <- rbind(output, run_links)
# }
# 
# write.csv(output, file = "Di_JanFeblink")
# 
# #三四月每篇新聞的網址
# output2 <- data.frame()
# for ( i in 1:length(Di_MarAprlink$V1)) {
#   print(i)
#   run_links <- FindUdnNews(Di_MarAprlink[ i , 1])
#   output2 <- rbind(output2, run_links)
# }
# 
# write.csv(output2, file = "Di_MarAprlink")
# 
# #五月每篇新聞的網址
# output3 <- data.frame()
# for ( i in 1:length(Di_Maylink$V1)) {
#   print(i)
#   run_links <- FindUdnNews(Di_Maylink[ i , 1])
#   output3 <- rbind(output3, run_links)
#   Sys.sleep(sample(1:5, 1))
# }
# 
# write.csv(output3, file = "Di_Maylink")

library(httr)
library(rvest)
library(magrittr)

#清空workspace
rm(list = ls())
#讀每篇新聞連結的檔案
Di_JanFeblink <- read.csv("Di_JanFeblink")
Di_MarAprlink <- read.csv("Di_MarAprlink")
Di_Maylink <- read.csv("Di_Maylink")

#開始來爬每一篇新聞的標題、內文和時間
title <- c()
time <- c()
text <- c()
#先寫個function
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
  text <- rbind(text, bindtext)
  
  return(cbind(time, title ,text))
  
}


#爬一二月的新聞內文標題時間
Output1 <- data.frame()
for( i in 1:length(Di_JanFeblink$.)){
  print(i)
  data <- FindNews(Di_JanFeblink[i,2])
  Output1 <- rbind(Output1,data)
  Sys.sleep(sample(1:10, 1))
}

Output1 <- cbind(Output1, Di_JanFeblink$.)

#存丁的一二月新聞檔
write.csv(Output1 , file = "Di_JanFebNews")


#爬三四月的新聞內文標題時間
Output2 <- data.frame()
for( i in 1:length(Di_MarAprlink$.)){
  print(i)
  data <- FindNews(Di_MarAprlink[i,2])
  Output2 <- rbind(Output2,data)
  Sys.sleep(sample(1:10, 1))
}

Output2 <- cbind(Output2, Di_MarAprlink$.)

#存丁的三四月新聞檔
write.csv(Output2 , file = "Di_MarAprNews")

#爬五月的新聞內文標題時間
Output3 <- data.frame()
for( i in 1:length(Di_Maylink$.)){
  print(i)
  data <- FindNews(Di_Maylink[i,2])
  Output3 <- rbind(Output3,data)
  Sys.sleep(sample(1:10, 1))
}

Output3 <- cbind(Output3, Di_Maylink$.)


#存丁的五月新聞檔
write.csv(Output3 , file = "Di_MayNews")




# output <- sapply(Di_JanFeblink[, 1], FindUdnNews)
# 
# data = FindUdnNews(Di_JanFeblink[1,]) 
# data2 = FindUdnNews(Di_JanFeblink[2,])
# 
# rbind(data,data2)
# URLframe <- data_frame()
# sapply(Di_JanFeblink$V1,function(x){
#   data = FindUdnNews(x) 
#   URLframe <- rbind(URLframe,data)
# })
# #五月新聞的連結
# 
# Di_Maynews <- FindUdnNews(  , Di_Maylink)
# 
# 
# for ( n in 1:length(url.link)) {
#   print(n)
#   doc <- read_html(url.link[n])
#   css <- "#search_content > dt > a"
#   node <- html_nodes(doc, css)
#   link <- html_attrs(node) 
#   links <- rbind(links, link)
#   Sys.sleep(sample(1:5, 1))
# }


