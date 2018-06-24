#爬自由時報新聞，每月每月的爬，以柯文哲的為例。
#清除工作區檔案
rm(list = ls())

#下載套件
library(httr)
library(rvest)
library(magrittr)

#爬柯文哲五月新聞的網頁網址
ltn_may_url <- "http://news.ltn.com.tw/search/?keyword=%E6%9F%AF%E6%96%87%E5%93%B2&conditions=and&SYear=2018&SMonth=5&SDay=1&EYear=2018&EMonth=5&EDay=31&page="
may_ltnurl <- c()
#55頁只是5/1-5/31
for( i in 1: 55) {
  print(i)
  url <- paste0(ltn_may_url , i )
  may_ltnurl <- rbind(may_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#檢視工作路徑以便存檔
getwd()
setwd("\Users\Weber\Documents\GitHub\NTU-CSX-DataScience--Group5\Finalproject\LTN")

#存柯文哲五月新聞的網頁網址
write.csv(may_ltnurl , "Ko_mayltnpage")
Ko_mayltnpage <-read.csv("Ko_mayltnpage") 
Ko_mayltnpage <- Ko_mayltnpage[ , 2] #只有第二欄是網址需要

#爬每頁網址的每一篇新聞的網址
#先做一個可以爬每頁網址的每篇新聞的function
FindLtnNews <- function(URL){
  URL <- URL %>% as.character()
  doc <- read_html(URL)
  css <- "#newslistul > li > a.tit"
  node <- html_nodes(doc, css)
  link <- html_attrs(node) 
  #將news_links變成一個只有每篇新聞連結的list
  news_links <- unlist(link)
  news_links <- matrix(news_links , byrow = T , ncol = 3)
  news_links <- news_links[ , 2 ] 
  news_links <- news_links %>% data.frame() %>% return()
  }

#製作一個可以貼出完整每篇新聞網址的Function
PasteLtnlinks <- function(URL){
  pre <-  "http://news.ltn.com.tw/"
  URL <-   paste0(pre , URL) #%>% return() # Why不行?
  return(URL)
}

#用上面的Function爬五月的每篇新聞的網址
output1 <- c()
for ( i in 1:length(Ko_mayltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Ko_mayltnpage[i])
  output1 <- rbind(output1, runlinks)
  Sys.sleep(sample(1:5,1))
}

Ko_MayLtnlink <- c()

for ( n in 1:length(output1$.)) {
  print(n)
  URL <- PasteLtnlinks(output1$.[n])
  Ko_MayLtnlink <- rbind( Ko_MayLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Ko_MayLtnlink, file = "Ko_MayLtnlink")
#讀檔
Ko_MayLtnlink <- read.csv("Ko_MayLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()
#先寫個Function
FindNews <- function(URL){
  URL <- URL %>% as.character()
  t_title <- read_html(URL) %>% html_nodes(.,"h1") %>% html_text()
  title <- rbind(title, t_title)
  
  t_time <- read_html(URL) %>% html_node(.,".date") %>% html_text()
  time <- rbind(time, t_time)
  
  t_text <- read_html(URL) %>% html_nodes(.,"p") %>% html_text()
  s <- length(t_text) - 5
  ss <- t_text[1:s]
  bindtext <- ""
  for(i in c(1:length(ss))){
    bindtext <- paste(bindtext,ss[i])  
  }
  
   return(cbind(title, time ,bindtext))
  
}

#開始爬五月每篇新聞的標題時間內文
Ko_MayLtnNews <- data.frame()
for ( m in 1:length(Ko_MayLtnlink$V1)) {
  print(m)
  output <- FindNews(Ko_MayLtnlink$V1[m])
  output <- cbind(output, Ko_MayLtnlink$V1[m])
  Ko_MayLtnNews <- rbind(Ko_MayLtnNews, Ko_MayLtnlink$V1[m])
  Sys.sleep(sample(1:10, 1))
}

#存五月每篇新聞的標題時間內文連結
write.csv(Ko_MayLtnNews, "Ko_MayLtnNews")

#==========================================
#爬柯文哲四月新聞的網頁網址
ltn_apr_url <- "http://news.ltn.com.tw/search/?keyword=%E6%9F%AF%E6%96%87%E5%93%B2&conditions=and&SYear=2018&SMonth=4&SDay=1&EYear=2018&EMonth=4&EDay=30&page="
apr_ltnurl <- c()
#34頁只是4/1-4/30
for( i in 1: 34) {
  print(i)
  url <- paste0(ltn_apr_url , i )
  apr_ltnurl <- rbind(apr_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存柯文哲四月新聞的網頁網址
write.csv(apr_ltnurl , "Ko_aprltnpage")
Ko_aprltnpage <-read.csv("Ko_aprltnpage") 
Ko_aprltnpage <- Ko_aprltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬四月的每篇新聞的網址
output2 <- c()
for ( i in 1:length(Ko_aprltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Ko_aprltnpage[i])
  output2 <- rbind(output2, runlinks)
  Sys.sleep(sample(1:5,1))
}

Ko_AprLtnlink <- c()

for ( n in 1:length(output2$.)) {
  print(n)
  URL <- PasteLtnlinks(output2$.[n])
  Ko_AprLtnlink <- rbind( Ko_AprLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Ko_AprLtnlink, file = "Ko_AprLtnlink")
#讀檔
Ko_AprLtnlink <- read.csv("Ko_AprLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬四月每篇新聞的標題時間內文
Ko_AprLtnNews <- data.frame()
for ( m in 1:length(Ko_AprLtnlink$V1)) {
  print(m)
  output <- FindNews(Ko_AprLtnlink$V1[m])
  output <- cbind(output, Ko_AprLtnlink$V1[m])
  Ko_AprLtnNews <- rbind(Ko_AprLtnNews, Ko_AprLtnlink$V1[m])
  Sys.sleep(sample(1:10, 1))
}

#存四月每篇新聞的標題時間內文連結
write.csv(Ko_AprLtnNews, "Ko_AprLtnNews")


#==========================================
#爬柯文哲三月新聞的網頁網址
ltn_mar_url <- "http://news.ltn.com.tw/search/?keyword=%E6%9F%AF%E6%96%87%E5%93%B2&conditions=and&SYear=2018&SMonth=3&SDay=1&EYear=2018&EMonth=3&EDay=31&page="
mar_ltnurl <- c()
#35頁只是3/1-3/31
for( i in 1: 35) {
  print(i)
  url <- paste0(ltn_mar_url , i )
  mar_ltnurl <- rbind(mar_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存柯文哲三月新聞的網頁網址
write.csv(mar_ltnurl , "Ko_marltnpage")
Ko_marltnpage <-read.csv("Ko_marltnpage") 
Ko_marltnpage <- Ko_marltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬三月的每篇新聞的網址
output3 <- c()
for ( i in 1:length(Ko_marltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Ko_marltnpage[i])
  output3 <- rbind(output3, runlinks)
  Sys.sleep(sample(1:5,1))
}

Ko_MarLtnlink <- c()

for ( n in 1:length(output3$.)) {
  print(n)
  URL <- PasteLtnlinks(output3$.[n])
  Ko_MarLtnlink <- rbind( Ko_MarLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Ko_MarLtnlink, file = "Ko_MarLtnlink")
#讀檔
Ko_MarLtnlink <- read.csv("Ko_MarLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬三月每篇新聞的標題時間內文
Ko_MarLtnNews <- data.frame()
for ( m in 1:length(Ko_MarLtnlink$V1)) {
  print(m)
  output <- FindNews(Ko_MarLtnlink$V1[m])
  output <- cbind(output, Ko_MarLtnlink$V1[m])
  Ko_MarLtnNews <- rbind(Ko_MarLtnNews, Ko_MarLtnlink$V1[m])
  Sys.sleep(sample(1:10, 1))
}

#存三月每篇新聞的標題時間內文連結
write.csv(Ko_MarLtnNews, "Ko_MarLtnNews")

#==========================================
#爬柯文哲二月新聞的網頁網址
ltn_feb_url <- "http://news.ltn.com.tw/search/?keyword=%E6%9F%AF%E6%96%87%E5%93%B2&conditions=and&SYear=2018&SMonth=2&SDay=1&EYear=2018&EMonth=2&EDay=28&page="
feb_ltnurl <- c()
#22頁只是2/1-2/28
for( i in 1: 22) {
  print(i)
  url <- paste0(ltn_feb_url , i )
  feb_ltnurl <- rbind(feb_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存柯文哲二月新聞的網頁網址
write.csv(feb_ltnurl , "Ko_febltnpage")
Ko_febltnpage <-read.csv("Ko_febltnpage") 
Ko_febltnpage <- Ko_febltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬二月的每篇新聞的網址
output4 <- c()
for ( i in 1:length(Ko_febltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Ko_febltnpage[i])
  output4 <- rbind(output4, runlinks)
  Sys.sleep(sample(1:5,1))
}

Ko_FebLtnlink <- c()

for ( n in 1:length(output4$.)) {
  print(n)
  URL <- PasteLtnlinks(output4$.[n])
  Ko_FebLtnlink <- rbind( Ko_FebLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Ko_FebLtnlink, file = "Ko_FebLtnlink")
#讀檔
Ko_FebLtnlink <- read.csv("Ko_FebLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬二月每篇新聞的標題時間內文
Ko_FebLtnNews <- data.frame()
for ( m in 1:length(Ko_FebLtnlink$V1)) {
  print(m)
  output <- FindNews(Ko_FebLtnlink$V1[m])
  output <- cbind(output, Ko_FebLtnlink$V1[m])
  Ko_FebLtnNews <- rbind(Ko_FebLtnNews, Ko_FebLtnlink$V1[m])
  Sys.sleep(sample(1:10, 1))
}

#存二月每篇新聞的標題時間內文連結
write.csv(Ko_FebLtnNews, "Ko_FebLtnNews")


#==========================================
#爬柯文哲一月新聞的網頁網址
ltn_jan_url <- "http://news.ltn.com.tw/search/?keyword=%E6%9F%AF%E6%96%87%E5%93%B2&conditions=and&SYear=2018&SMonth=1&SDay=1&EYear=2018&EMonth=1&EDay=31&page="
jan_ltnurl <- c()
#29頁只是1/1-1/31
for( i in 1: 29) {
  print(i)
  url <- paste0(ltn_jan_url , i )
  jan_ltnurl <- rbind(jan_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存柯文哲ㄧ月新聞的網頁網址
write.csv(jan_ltnurl , "Ko_janltnpage")
Ko_janltnpage <-read.csv("Ko_janltnpage") 
Ko_janltnpage <- Ko_janltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬ㄧ月的每篇新聞的網址
output5 <- c()
for ( i in 1:length(Ko_janltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Ko_janltnpage[i])
  output5 <- rbind(output5, runlinks)
  Sys.sleep(sample(1:5,1))
}

Ko_JanLtnlink <- c()

for ( n in 1:length(output5$.)) {
  print(n)
  URL <- PasteLtnlinks(output5$.[n])
  Ko_JanLtnlink <- rbind( Ko_JanLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Ko_JanLtnlink, file = "Ko_JanLtnlink")
#讀檔
Ko_JanLtnlink <- read.csv("Ko_JanLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬ㄧ月每篇新聞的標題時間內文
Ko_JanLtnNews <- data.frame()
for ( m in 1:length(Ko_JanLtnlink$V1)) {
  print(m)
  output <- FindNews(Ko_JanLtnlink$V1[m])
  output <- cbind(output, Ko_JanLtnlink$V1[m])
  Ko_JanLtnNews <- rbind(Ko_JanLtnNews, Ko_JanLtnlink$V1[m])
  Sys.sleep(sample(1:10, 1))
}

#存ㄧ月每篇新聞的標題時間內文連結
write.csv(Ko_JanLtnNews, "Ko_JanLtnNews")


