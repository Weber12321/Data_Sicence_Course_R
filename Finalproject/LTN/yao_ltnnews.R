rm(list = ls())

library(httr)
library(rvest)
library(magrittr)

start_time <- system.time() 

#爬姚文智五月新聞的網頁網址
ltn_may_url <- "http://news.ltn.com.tw/search/?keyword=%E5%A7%9A%E6%96%87%E6%99%BA&conditions=and&SYear=2018&SMonth=5&SDay=1&EYear=2018&EMonth=5&EDay=31&page="
may_ltnurl <- c()
#15頁只是5/1-5/31
for( i in 1: 15) {
  print(i)
  url <- paste0(ltn_may_url , i )
  may_ltnurl <- rbind(may_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#檢視工作路徑以便存檔
getwd()
setwd("/Users/Weber/Documents/GitHub/NTU-CSX-DataScience--Group5/Finalproject/LTN/Yao")

#存姚文智五月新聞的網頁網址
write.csv(may_ltnurl , "Yao_mayltnpage")
Yao_mayltnpage <-read.csv("Yao_mayltnpage") 
Yao_mayltnpage <- Yao_mayltnpage[ , 2] #只有第二欄是網址需要

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
for ( i in 1:length(Yao_mayltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Yao_mayltnpage[i])
  output1 <- rbind(output1, runlinks)
  Sys.sleep(sample(1:5,1))
}

Yao_MayLtnlink <- c()

for ( n in 1:length(output1$.)) {
  print(n)
  URL <- PasteLtnlinks(output1$.[n])
  Yao_MayLtnlink <- rbind(Yao_MayLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Yao_MayLtnlink, file = "Yao_MayLtnlink")
#讀檔
Yao_MayLtnlink <- read.csv("Yao_MayLtnlink")

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
Yao_MayLtnNews <- data.frame()
for ( m in 1:length(Yao_MayLtnlink$V1)) {
  print(m)
  output <- FindNews(Yao_MayLtnlink$V1[m])
  output <- cbind(output, Yao_MayLtnlink$V1[m])
  Yao_MayLtnNews <- rbind(Yao_MayLtnNews, output)
  Sys.sleep(sample(1:10, 1))
}

#存五月每篇新聞的標題時間內文連結
write.csv(Yao_MayLtnNews, "Yao_MayLtnNews")

#==========================================
#爬姚文智四月新聞的網頁網址
ltn_apr_url <- "http://news.ltn.com.tw/search/?keyword=%E5%A7%9A%E6%96%87%E6%99%BA&conditions=and&SYear=2018&SMonth=4&SDay=1&EYear=2018&EMonth=4&EDay=30&page="
apr_ltnurl <- c()
#9頁只是4/1-4/30
for( i in 1: 9) {
  print(i)
  url <- paste0(ltn_apr_url , i )
  apr_ltnurl <- rbind(apr_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存姚文智四月新聞的網頁網址
write.csv(apr_ltnurl , "Yao_aprltnpage")
Yao_aprltnpage <-read.csv("Yao_aprltnpage") 
Yao_aprltnpage <- Yao_aprltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬四月的每篇新聞的網址
output2 <- c()
for ( i in 1:length(Yao_aprltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Yao_aprltnpage[i])
  output2 <- rbind(output2, runlinks)
  Sys.sleep(sample(1:5,1))
}

Yao_AprLtnlink <- c()

for ( n in 1:length(output2$.)) {
  print(n)
  URL <- PasteLtnlinks(output2$.[n])
  Yao_AprLtnlink <- rbind(Yao_AprLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Yao_AprLtnlink, file = "Yao_AprLtnlink")
#讀檔
Yao_AprLtnlink <- read.csv("Yao_AprLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬四月每篇新聞的標題時間內文
Yao_AprLtnNews <- data.frame()
for ( m in 1:length(Yao_AprLtnlink$V1)) {
  print(m)
  output <- FindNews(Yao_AprLtnlink$V1[m])
  output <- cbind(output, Yao_AprLtnlink$V1[m])
  Yao_AprLtnNews <- rbind(Yao_AprLtnNews, output)
  Sys.sleep(sample(1:10, 1))
}

#存四月每篇新聞的標題時間內文連結
write.csv(Yao_AprLtnNews, "Yao_AprLtnNews")


#==========================================
#爬姚文智三月新聞的網頁網址
ltn_mar_url <- "http://news.ltn.com.tw/search/?keyword=%E5%A7%9A%E6%96%87%E6%99%BA&conditions=and&SYear=2018&SMonth=3&SDay=1&EYear=2018&EMonth=3&EDay=31&page="
mar_ltnurl <- c()
#5頁只是3/1-3/31
for( i in 1: 5) {
  print(i)
  url <- paste0(ltn_mar_url , i )
  mar_ltnurl <- rbind(mar_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存姚文智三月新聞的網頁網址
write.csv(mar_ltnurl , "Yao_marltnpage")
Yao_marltnpage <-read.csv("Yao_marltnpage") 
Yao_marltnpage <- Yao_marltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬三月的每篇新聞的網址
output3 <- c()
for ( i in 1:length(Yao_marltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Yao_marltnpage[i])
  output3 <- rbind(output3, runlinks)
  Sys.sleep(sample(1:5,1))
}

Yao_MarLtnlink <- c()

for ( n in 1:length(output3$.)) {
  print(n)
  URL <- PasteLtnlinks(output3$.[n])
  Yao_MarLtnlink <- rbind( Yao_MarLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Yao_MarLtnlink, file = "Yao_MarLtnlink")
#讀檔
Yao_MarLtnlink <- read.csv("Yao_MarLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬三月每篇新聞的標題時間內文
Yao_MarLtnNews <- data.frame()
for ( m in 1:length(Yao_MarLtnlink$V1)) {
  print(m)
  output <- FindNews(Yao_MarLtnlink$V1[m])
  output <- cbind(output, Yao_MarLtnlink$V1[m])
  Yao_MarLtnNews <- rbind(Yao_MarLtnNews, output)
  Sys.sleep(sample(1:10, 1))
}

#存三月每篇新聞的標題時間內文連結
write.csv(Yao_MarLtnNews, "Yao_MarLtnNews")

#==========================================
#爬姚文智二月新聞的網頁網址
ltn_feb_url <- "http://news.ltn.com.tw/search/?keyword=%E5%A7%9A%E6%96%87%E6%99%BA&conditions=and&SYear=2018&SMonth=2&SDay=1&EYear=2018&EMonth=2&EDay=28&page="
feb_ltnurl <- c()
#5頁只是2/1-2/28
for( i in 1: 5) {
  print(i)
  url <- paste0(ltn_feb_url , i )
  feb_ltnurl <- rbind(feb_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存姚文智二月新聞的網頁網址
write.csv(feb_ltnurl , "Yao_febltnpage")
Yao_febltnpage <-read.csv("Yao_febltnpage") 
Yao_febltnpage <- Yao_febltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬二月的每篇新聞的網址
output4 <- c()
for ( i in 1:length(Yao_febltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Yao_febltnpage[i])
  output4 <- rbind(output4, runlinks)
  Sys.sleep(sample(1:5,1))
}

Yao_FebLtnlink <- c()

for ( n in 1:length(output4$.)) {
  print(n)
  URL <- PasteLtnlinks(output4$.[n])
  Yao_FebLtnlink <- rbind( Yao_FebLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Yao_FebLtnlink, file = "Yao_FebLtnlink")
#讀檔
Yao_FebLtnlink <- read.csv("Yao_FebLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬二月每篇新聞的標題時間內文
Yao_FebLtnNews <- data.frame()
for ( m in 1:length(Yao_FebLtnlink$V1)) {
  print(m)
  output <- FindNews(Yao_FebLtnlink$V1[m])
  output <- cbind(output, Yao_FebLtnlink$V1[m])
  Yao_FebLtnNews <- rbind(Yao_FebLtnNews, output)
  Sys.sleep(sample(1:10, 1))
}

#存二月每篇新聞的標題時間內文連結
write.csv(Yao_FebLtnNews, "Yao_FebLtnNews")


#==========================================
#爬姚文智一月新聞的網頁網址
ltn_jan_url <- "http://news.ltn.com.tw/search/?keyword=%E5%A7%9A%E6%96%87%E6%99%BA&conditions=and&SYear=2018&SMonth=1&SDay=1&EYear=2018&EMonth=1&EDay=31&page="
jan_ltnurl <- c()
#3頁只是1/1-1/31
for( i in 1: 3) {
  print(i)
  url <- paste0(ltn_jan_url , i )
  jan_ltnurl <- rbind(jan_ltnurl , url )
  Sys.sleep(sample(1:5 , 1))
}

#存姚文智ㄧ月新聞的網頁網址
write.csv(jan_ltnurl , "Yao_janltnpage")
Yao_janltnpage <-read.csv("Yao_janltnpage") 
Yao_janltnpage <- Yao_janltnpage[ , 2] #只有第二欄是網址需要

#用上面的Function爬ㄧ月的每篇新聞的網址
output5 <- c()
for ( i in 1:length(Yao_janltnpage)) {
  print(i)
  runlinks <- FindLtnNews(Yao_janltnpage[i])
  output5 <- rbind(output5, runlinks)
  Sys.sleep(sample(1:5,1))
}

Yao_JanLtnlink <- c()

for ( n in 1:length(output5$.)) {
  print(n)
  URL <- PasteLtnlinks(output5$.[n])
  Yao_JanLtnlink <- rbind(Yao_JanLtnlink, URL)
}


#將每篇新聞的網址存檔
write.csv(Yao_JanLtnlink, file = "Yao_JanLtnlink")
#讀檔
Yao_JanLtnlink <- read.csv("Yao_JanLtnlink")

#讀每篇新聞的標題時間內文
title <- c()
time <- c()
text <- c()

#開始爬ㄧ月每篇新聞的標題時間內文
Yao_JanLtnNews <- data.frame()
for ( m in 1:length(Yao_JanLtnlink$V1)) {
  print(m)
  output <- FindNews(Yao_JanLtnlink$V1[m])
  output <- cbind(output, Yao_JanLtnlink$V1[m])
  Yao_JanLtnNews <- rbind(Yao_JanLtnNews, output)
  Sys.sleep(sample(1:10, 1))
}


finish_time <- system.time()
print(finish_time - start_time)
#存ㄧ月每篇新聞的標題時間內文連結
write.csv(Yao_JanLtnNews, "Yao_JanLtnNews")


