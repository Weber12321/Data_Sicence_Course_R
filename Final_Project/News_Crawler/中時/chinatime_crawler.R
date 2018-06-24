#爬中時資料庫的新聞新聞文本清理，以柯文哲為例，其他候選人的中時新聞爬取方式相同。
#清除工作區檔案
rm(list = ls())

#下載Rselenium套件
install.packages("devtools")
library(devtools)
install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
library(RSelenium)

# 連接 Selenium 伺服器，選用 chrome 瀏覽器
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

# 開啟瀏覽器
remDr$open()

#開啟知識贏家的網頁
remDr$navigate("http://kmw.chinatimes.com/News/NewsSearch.aspx?searchkind=s")

# 依據 CSS 選擇器取得網頁元素
login <- remDr$findElement(using = 'id', value = "lbInfotimesLogin")
login$clickElement()
keyword <- "柯文哲" #輸入搜尋柯文哲
SearchBox <- remDr$findElement(using='id', value='txtKeyword') 
SearchBox$sendKeysToElement(list(keyword, key = 'enter'))

SearchBox <- remDr$findElement(using='id', value='ddlRange')
SearchBox$sendKeysToElement(list("最近 6 個月", key = 'enter')) #輸入搜尋的時間範圍

#根據顯示出來的搜尋結果，得知共100頁筆柯文哲資料，因此爬取這100頁的網頁連結。
all_links <- c()

for( i in 1:100) {
  links <- remDr$findElements(using='class', value='NewsContetn')
  result <- lapply(links, function(e) { e$getElementAttribute("href") })
  all_links <- rbind(all_links, result)
  message(i)
  
  SearchBox <- remDr$findElement(using='id', value='ctl00_ContentPlaceHolder1_UCPage1_lbtnPageNext')
  SearchBox$clickElement()
  }

Ko_CTlinks <- unlist(all_links)

#存取連結
getwd()
setwd("/Users/sunny/Documents/GitHub/courseR/final_project/CTnews")
write.csv(Ko_CTlinks, "Ko_CTlinks")

#打開連結
Ko_CTlinks <- read.csv("Ko_CTlinks")

#寫爬取每一頁每一筆新聞內文、時間、標題的Function
FindCTnews <- function(URL){
  remDr$navigate(URL)
  title_list <- remDr$findElement(using = 'css selector', value = "#aspnetForm > div.page_container.clear-fix > article > header > span > h1")
  title <- unlist(title_list$getElementText()) #爬標題
  
  content_list <- remDr$findElement(using = 'css selector', value = "#dvContainer")
  content <- unlist(content_list$getElementText()) #爬內文
  
  time_list <- remDr$findElement(using = 'css selector', value = "#ctl00_ContentPlaceHolder1_UCNewsContent1_lbldateAuth")
  time <- unlist(time_list$getElementText()) #爬時間
  
  output <- cbind(title, time, content)
  
  return(output)
}

#開始爬每一頁每一筆新聞標題、時間、內文
Ko_CTnews <- data.frame()

for( i in 1:length(Ko_CTlinks$x)){
  print(i)
  output <- FindCTnews(Ko_CTlinks[i,2])
  Ko_CTnews <- rbind(Ko_CTnews, output)
  Sys.sleep(sample(1:5,1))
}  

#爬下來的內容有包含投書和評論，不屬於報導，在觀察之後，發現不含「報導」兩字的內文不屬於新聞報導，
#因此有以下步驟，將投書和評論的文章刪掉。

Ko_CTnews <- cbind(Ko_CTnews, Ko_CTlinks$x)
colnames(Ko_CTnews)[4] <- "URL"
mergenum <- c(1:1000) #建立一個mergenum的欄位，使其能和後面動作做聯結
colnames(mergenum)[1] <- "mergenum"
Ko_CTnews <- cbind(Ko_CTnews, mergenum)

#將含有報導兩個字的資料取出來，並保留原先標示排序的欄位，使其能跟上述的mergenum做聯結。
l <- sapply(colnames(Ko_CTnews), function(x) grep("報導", Ko_CTnews$content))
ll <- data.frame(l[ , 1])
colnames(ll)[1] <- "mergenum"

#merge完之後，留下含有「報導」兩字的資料，屬於新聞
Ko_RCTnews <- merge(Ko_CTnews, ll)
Ko_RCTnews <- Ko_RCTnews[ , 2:5]

#存檔
write.csv(Ko_RCTnews, "Ko_RCTnews") 

#關掉瀏覽器
remDr$quit()  

#待補
#1.內文要清掉報導兩個字與後面文字
#2.按照時間排序，留下2018/01/01~2018/05/31

#清除爬下來的雜訊
#打開爬下來的Ko_RCTnews  
Ko_RCTnews <- read.csv("Ko_RCTnews")

#清理第一步：整理時間欄位
#原先的時間欄位屬於時間加版面，如「2018/06/18- [中時/2018百里侯之戰特別報導/A3版]」，
#故希望去掉後面的版面資訊，留下時間。
library(magrittr)
newtime <- c()
for(i in 1:length(Ko_RCTnews$time)){
  tt <- strsplit(Ko_RCTnews$time[i] %>% as.character(), "-") #從符號“-”以後，分隔出時間和版面兩個資訊
  ttime <- matrix(unlist(tt), ncol = 2, byrow = T)
  newtime <- rbind(newtime, ttime)
  print(i)
}

#將分隔出來的時間欄位和版面欄位cbind回原先的Ko_RCT檔
newtime <- data.frame(newtime)
Ko_RCTnews <- cbind(Ko_RCTnews, newtime)

#清除每篇新聞後面的雜訊，即"本新聞、圖片內容由--時報資訊股份有限公司--取得合法授權。除經本公司同意，本網站僅供您個人及非商業目的之使用。您不得修改、拷貝、散佈、傳送、展示、執行、授權、製作衍生著作、移轉或銷售取自於本網站之任何資料、軟體、產品或服務。"
library(tm)
newcontent <- c()
for( n in 1:length(Ko_RCTnews$content)){
  docs <- Corpus(VectorSource(Ko_RCTnews$content[n]))
  toSpace <- content_transformer(function(x,pattern){
    return(gsub(pattern," ",x))
  })
  
  print(n)
  
  docs <- tm_map(docs,toSpace,"本新聞、圖片內容由--時報資訊股份有限公司--取得合法授權。除經本公司同意，本網站僅供您個人及非商業目的之使用。您不得修改、拷貝、散佈、傳送、展示、執行、授權、製作衍生著作、移轉或銷售取自於本網站之任何資料、軟體、產品或服務。")
  ncontent <- content(docs[1]) #取出doc[1]的content，即是新聞內文！
  newcontent <- rbind(newcontent, ncontent)
  
}


#重新製造一個新的中時柯文哲資料檔
Ko_CTnews <- c()
title <- data.frame(Ko_RCTnews$title)
time <- data.frame(Ko_RCTnews$X1)
content <- data.frame(newcontent)
Ko_CTnews <- cbind(title, time, content)
colnames(Ko_CTnews) <- c("title", "time", "content") #這次只要有標題、時間和內文就好

#整理好後存檔！
write.csv(Ko_CTnews, "Ko_CTnews")

#原先爬下來的是半年的新聞，但我們只要0101-0531區間的新聞文本
#所以決定先處理時間的排序，再去頭去尾，刪去不在0101-0531區間的文本
library(tibble)
library(dplyr)
library(tidyr)
library(data.table)

#將時間欄位切割成年月日
Ko_CTnews <- Ko_CTnews %>% separate(time, c("year","month","day"),"/") #將time的欄位切成年月日三欄
Ko_CTnews <- Ko_CTnews[with(Ko_CTnews, order(year, month, day )), ]
Ko_CTnews <- Ko_CTnews[69:725, ]#去掉六月的資料
Ko_CTnews <- Ko_CTnews[1:582, ]#去掉一月以前的資料


#這邊是為了要做情緒分析要用的各候選人四大報文本的df所寫的，所以在整理下來的文本裡面要加入一欄"Media"的欄位。
Media <- c()
text <- c("CT")
for( i in 1:length(Ko_CTnews$content)){
  Media <- rbind(Media, text)
}

Ko_CTnews <- cbind(Media, Ko_CTnews)

write.csv(Ko_CTnews, "Ko_CTnews")



#以下為Rselenium的練習
# # 瀏覽 Google 首頁
# remDr$navigate("https://www.google.com.tw/")
# # 瀏覽 Yahoo 首頁
# remDr$navigate("https://tw.yahoo.com/")
# # 回到上一頁
# remDr$goBack()
# # 前往下一頁
# remDr$goForward()
# # 取的目前網頁的網址
# remDr$getCurrentUrl()
# # 重新整理
# remDr$refresh()
# # 用 Google 搜尋 office 關鍵字
# remDr$navigate("https://www.google.com.tw/search?q=office")
