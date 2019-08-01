# === This file is a cheat sheet for doing ETL
# 0. load data
# 1. Missing value
# 2. duplicated data
# 3. split
# 4. remove spaces
# 5. append
# 6. sort & filter
# 7. combine
# 8. format
# 9. replace
# 10.export data
# 11.ggplot x reorder
# 12.geocode
# 13.data shaping(pivot)
# 14.detect
# 15.regex
# 16.Difference from datetime
# 17.trouble shooting
# ===
# system language
Sys.setlocale(category="LC_ALL",locale="cht")

# load packages
library(xlsx)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)
library(splitstackshape)
library(data.table)

# getwd
getwd()
setwd()

# 0. load data
file <- read.csv("", stringsAsFactors = FALSE, na.strings = c("NA", ""))
file <- xlsx::read.xlsx(file="PChome.xlsx",sheetIndex=1,encoding = "UTF-8")
file <- readxl::read_excel("file.xlsx","sheet name or index")

# 1. Missing value
# detect NA
sapply(file, function(x) {sum(is.na(x))})
# missing value tratment
# 1.1 drop NA
file = file %>% na.omit()
# 1.2 others
'http://r-statistics.co/Missing-Value-Treatment-With-R.html'

# 2. duplicated data
file = file %>% distinct()


# 3. split
# split column
# a
file = str_split_fixed(file$col, " ", 2)
# b
file = file %>% separate(col, c("new_col","new_col_2"),"")
# c:if ya don't know how many columns will be generated
file <- cbind(splitstackshape::cSplit(file, 'column name', ','))

# 4. remove spaces
file$col = file$col %>% str_trim()

# 5. append
file$col = file$col %>% paste("what ya wanna paste", sep = " ")

# 6. sort & filter
file = file %>% filter(col=="what ya wanna find out")
file = file %>% arrange(desc(col1),desc(col2))

# 7. combine
# merge() for dataset have same id
file = file %>% unite("new column name", c("col1", "col2"),sep="_",remove=F)

# 8. format
# time e.g 2018/01/02 22:22:22, only extract to hour
file$col = as.POSIXct(file$col)
file$col = format(file$col, format='%Y-%m-%d %H')
file = file %>% 
  separate(col, c("date","hour"), " ")
file$date = file$date %>% as.Date()

# 9. replace
# whole
file$col[file$col == "what value wanna replace"] <- "new value" 
# specific words
file$col = gsub("word","new word",file$col)
# Add a column to recognize if there is a unit x in column.
file <- file %>% mutate(result = stri_detect_regex(value,x))

# 10.export data
write.csv(file, file = "name_of_the_file.csv", row.names = FALSE, na = "")

# 11.ggplot x reorder
file$col <- factor(file$col, levels =file$col[order(file$col2)])

# 12.geocode
require(ggmap)
# load file
file <- read.csv(file = "", na.strings = c("", "NA"),stringsAsFactors=FALSE)
# data cleaning
# === google map api key
register_google(key = "AIzaSyD6-_3G3Par9D8aWVYb-jQVMxzAR86kWIQ")
# geocoding
for(i in 1:nrow(file))
{
  # Print("Working...")
  if (!(is.na(file$col[i]))) {
    result <- geocode(file$col[i], output = "latlona", source = "google")
    file$lon[i] <- as.numeric(result[1])
    file$lat[i] <- as.numeric(result[2])
    # origAddress$geoAddress[i] <- as.character(result[3])
  } else{
    file$lon[i] <- NA
    file$lat[i] <- NA
  }
}
write.csv(file, file = "", row.names = FALSE, na = "")

# 13.data shaping
file <- data.table::melt(file, id=1:25)
# columns after index 25 will be shape as colname in 26 and column value in 27.

# 14.detect
# detect if there is a pattern in file$col, return true-false
stringi::stri_detect_regex(file$col,pattern)
# count how many value in column, return integer
stringr::str_count(file$col, value)
# detect by index, return value in the range
stringi::stri_sub(file$col, start, end)
# detect letter's location
gregexpr(pattern = 'x', file$col)
stringr::str_locate_all(pattern = 'x',file$col)

# 15.regex
'https://blog.yjtseng.info/post/regexpr/'

# 16.Difference from datetime (dplyr)
file$datetime = as.Date(file$datetime, format='%Y/%m/%d  # original style')
file = file %>% 
  group_by(category) %>% arrange(category,datetime) %>% 
  mutate(Diff = value - lag(value)) # lag is method that finds the "previous" values in a vector.
# if u wanna find the "next" values, use lead()

# 17.trouble shooting
# load file
# Q1 : Error in make.names(col.names, unique = TRUE) : invalid multibyte string 1
# A1 : Re-saving file as "CSV (comma delimited)(*.csv)" from "CSV UTF-8 (comma delimited)(*.csv)"
# Q2 : Warning message of input string 1 is invalid in this locale
# A2 : This is a default language problem, since R's default language is English,
#      if u wanna change default language, for example change English to Mandarin Chinese,
#      add a line Sys.setlocale(category="LC_ALL",locale="cht") at the top of the script.
#      Since this way isn't permanant, make sure add the line to the script what u wanna deal.