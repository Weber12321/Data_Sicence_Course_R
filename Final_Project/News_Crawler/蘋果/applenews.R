#爬取蘋果日報新聞，該搜尋頁面屬於動態網頁，以POST的方式得到網頁資訊。
#以下爬取方式以丁守中為例，1月至5月按月爬。其他候選人的蘋果日報文本爬蟲方式相同。

#下載套件
library(rvest)
library(magrittr)
library(httr)

# #爬丁守中五月的新聞
# #測試：爬一頁的搜尋結果
# url <- "https://tw.appledaily.com/search"
# form <- list(searchType = "text",
#              keyword = "丁守中",
#              totalpage = "236",
#              page = "1",
#              sorttype = "1",
#              rangedate = "[20180501 TO 20180531999:99]")
# 
# res <- httr::POST(url, body = form)
# doc.str <- content(res, "text")
# doc <- read_html(doc.str)
# 
# css <- "#result li > div > h2 > a"
# links <- html_attr(html_nodes(doc, css), "href")
# links


#爬丁守中全部五月新聞搜尋頁面的連結
url <- "https://tw.appledaily.com/search"
Di_MayApplinks <- c()

#爬丁守中五月新聞全部連結的迴圈
for(p in 1:24){
  form <- list(searchType = "text",
               keyword = "丁守中",
               totalpage = "236",
               page = sprintf("%d" , p ),
               sorttype = "1",
               rangedate = "[20180501 TO 20180531999:99]")
  res <- POST(url, body = form)
  doc.str <- content(res, "text")
  doc <- read_html(doc.str)
  css <- "#result > li > div > h2 > a"
  links <- html_attr(html_nodes(doc,css), "href")
  Di_MayApplinks <- c(Di_MayApplinks, links)
  message(p)
}

#test是否抓到重複的網址
test <- Di_MayApplinks[?duplicated(Di_MayApplinks)]

#爬五月連結的每一篇新聞
#先寫可以爬每一篇新聞時間標題內文的function
FindAppnews <- function(URL){
  doc <- read_html(URL %>% as.character())

  title <- html_text(html_node(doc,  "#article > div.wrapper > div > main > article > hgroup > h1"))
  text <- html_text(html_node(doc, "#article > div.wrapper > div > main > article > hgroup > div.ndArticle_creat"))
  time <- html_text(html_node(doc, "#article > div.wrapper > div > main > article > div > div.ndArticle_contentBox > article > div > p"))
  
  output <- data.frame( title = title,
                        text = text,
                        time = time)
  return(output)
}

#爬五月的每一篇新聞的時間標題內文
Di_MayAppnews <- data.frame()
for ( t in 1:length(Di_MayApplinks)) {
  print(t)
  output1 <- FindAppnews(Di_MayApplinks[t]) 
  Di_MayAppnews <- rbind(Di_MayAppnews, output1)
  Sys.sleep(sample(1:10, 1))
  
}

#存檔
write.csv(Di_MayAppnews, "Di_MayAppnews")

#爬丁守中四月新聞全部連結
Di_AprApplinks <- c()

#爬丁守中四月新聞全部連結的迴圈
for(p in 1:6){
  form <- list(searchType = "text",
               keyword = "丁守中",
               totalpage = "53",
               page = sprintf("%d" , p ),
               sorttype = "1",
               rangedate = "[20180401 TO 20180430999:99]")
  res <- POST(url, body = form)
  doc.str <- content(res, "text")
  doc <- read_html(doc.str)
  css <- "#result > li > div > h2 > a"
  links <- html_attr(html_nodes(doc,css), "href")
  Di_AprApplinks <- c(Di_AprApplinks, links)
  message(p)
}

#test是否抓到重複的網址
test <- Di_AprApplinks[?duplicated(Di_AprApplinks)]

#爬四月連結的每一篇新聞
#爬四月的每一篇新聞的時間標題內文
Di_AprAppnews <- data.frame()
for ( t in 1:length(Di_AprApplinks)) {
  print(t)
  output2 <- FindAppnews(Di_AprApplinks[t]) 
  Di_AprAppnews <- rbind(Di_AprAppnews, output2)
  Sys.sleep(sample(1:10, 1))
  
}

#存檔
write.csv(Di_AprAppnews, "Di_AprAppnews")


#爬丁守中三月新聞全部連結
#爬丁守中三月新聞全部連結的迴圈
Di_MarApplinks <- c()

for( p in 1:5){
  form <- list(searchType = "text",
               keyword = "丁守中",
               totalpage = "41",
               page = sprintf("%d" , p ),
               sorttype = "1",
               rangedate = "[20180301 TO 20180331999:99]")
  res <- POST(url, body = form)
  doc.str <- content(res, "text")
  doc <- read_html(doc.str)
  css <- "#result > li > div > h2 > a"
  links <- html_attr(html_nodes(doc,css), "href")
  Di_MarApplinks <- c(Di_MarApplinks, links)
  message(p)
}

#test是否抓到重複的網址
test <- Di_MarApplinks[?duplicated(Di_MarApplinks)]

#爬三月連結的每一篇新聞
#爬三月的每一篇新聞的時間標題內文
Di_MarAppnews <- data.frame()
for ( t in 1:length(Di_MarApplinks)) {
  print(t)
  output3 <- FindAppnews(Di_MarApplinks[t]) 
  Di_MarAppnews <- rbind(Di_MarAppnews, output3)
  Sys.sleep(sample(1:10, 1))
  
}



#存檔
write.csv(Di_MarAppnews, "Di_MarAppnews")

#爬丁守中二月新聞全部連結
#爬丁守中二月新聞全部連結的迴圈
Di_FebApplinks <- c()

for( p in 1:6){
  form <- list(searchType = "text",
               keyword = "丁守中",
               totalpage = "57",
               page = sprintf("%d" , p ),
               sorttype = "1",
               rangedate = "[20180201 TO 20180228999:99]")
  res <- POST(url, body = form)
  doc.str <- content(res, "text")
  doc <- read_html(doc.str)
  css <- "#result > li > div > h2 > a"
  links <- html_attr(html_nodes(doc,css), "href")
  Di_FebApplinks <- c(Di_FebApplinks, links)
  message(p)
}

#test是否抓到重複的網址
test <- Di_FebApplinks[?duplicated(Di_FebApplinks)]

#爬二月連結的每一篇新聞
#爬二月的每一篇新聞的時間標題內文
Di_FebAppnews <- data.frame()
for ( t in 1:length(Di_FebApplinks)) {
  print(t)
  output4 <- FindAppnews(Di_FebApplinks[t]) 
  Di_FebAppnews <- rbind(Di_FebAppnews, output4)
  Sys.sleep(sample(1:10, 1))
  
}

#存檔
write.csv(Di_FebAppnews, "Di_FebAppnews")

#爬丁守中一月新聞全部連結
#爬丁守中一月新聞全部連結的迴圈
Di_JanApplinks <- c()

for( p in 1:6){
  form <- list(searchType = "text",
               keyword = "丁守中",
               totalpage = "56",
               page = sprintf("%d" , p ),
               sorttype = "1",
               rangedate = "[20180101 TO 20180131999:99]")
  res <- POST(url, body = form)
  doc.str <- content(res, "text")
  doc <- read_html(doc.str)
  css <- "#result > li > div > h2 > a"
  links <- html_attr(html_nodes(doc,css), "href")
  Di_JanApplinks <- c(Di_JanApplinks, links)
  message(p)
}

#test是否抓到重複的網址
test <- Di_JanApplinks[?duplicated(Di_JanApplinks)]

#爬二月連結的每一篇新聞
#爬二月的每一篇新聞的時間標題內文
Di_JanAppnews <- data.frame()
for ( t in 1:length(Di_JanApplinks)) {
  print(t)
  output5 <- FindAppnews(Di_JanApplinks[t]) 
  Di_JanAppnews <- rbind(Di_JanAppnews, output5)
  Sys.sleep(sample(1:10, 1))
  
}

#存檔
write.csv(Di_JanAppnews, "Di_JanAppnews")



