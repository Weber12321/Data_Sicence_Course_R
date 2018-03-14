#輸入網址
rm(list = ls())
library(rvest)

# Set url
url <- "https://www.dcard.tw/f"
# Get response
res <- read_html(url)

# 爬取低卡標題、主題、學校來源、讚數與回應
titles <- res %>% html_nodes(".PostEntry_unread_2U217")
cates <- res %>% html_nodes(".PostEntry_forum_1m8nJ")
schools <- res %>% html_nodes(".PostAuthor_root_3vAJf") 
likes <- res %>% html_nodes(".Like_counter_1enlP")
responses <- res %>% html_nodes(".PostEntry_comments_2iY8V")

# 標題
dcard.article.titles <- titles %>% html_text()
# 主題
dcard.article.cates <- cates %>% html_text()
# 學校來源
dcard.article.schools <- schools %>% html_text() 
# 讚數
dcard.article.likes <- likes %>% html_text()
# 回應
dcard.article.responses <- responses %>% html_text()

# 建立資料框框
dcard.df <- data.frame(dcard.article.titles, dcard.article.cates, dcard.article.schools, dcard.article.likes, dcard.article.responses)

# 建立欄位名稱
dcard.df.col.names <- c("title", "category", "school", "like", "response")
colnames(dcard.df) <- dcard.df.col.names
View(dcard.df)



