### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris)

# 使用tail() 回傳iris的後六列
tail(iris)

# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)

for(i in c(1:9)){
  for(j in c(1:9)){
    print(paste(i , "x", j, "=", i*j ))
  }
}
  




########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
nums <- sample( x = 10:100, size = 10)

# 查看nums
nums

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。

for(i in nums){
  # 1.
  if(i >= 51 && i%%2 == 0){
    print(i)
  }
  # 2.
  if(i == 66)break
    print("太66666666666了")
}

  
  
  
  



########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年

year <- function(y)
{ 
  if(( y %% 400 == 0 )){
    return("為閏年")
  }else if ((( y %% 4 == 0 )&&( y %% 100 != 0 ))){
    return("為閏年")
  }else
    return("不是閏年")
}
print(year(2018))

########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 1. 
'
answer <- sample(1000:9999, 1)
time <- 0
repeat{
  guess <- cat("請輸入一組四位數字","\n")
  enter <- scan()
  a <- 0
  b <- 0
  for(i in 1:4){
    if(enter[i] == answer[i]){
      a <- a + 1
    }else
      for(j in 1:4){
        if(enter[j] == answer[i] ){
          b <- b + 1
        }
      }
  }
  cat(a,"A",b,"B","\n")
  if(a == 4){print("猜對")}
  break
}'

check <- function(num)
{
  num <- cat("請輸入一組四位數","\n")
  time <- time + 1
  a <- 0
  b <- 0
  i <- 1
    for(n in num){
      if(n == answer[i]){
        a <- a + 1
        i <- i + 1
      } else if(n == answer){
        b <- b + 1
      }
    }
  return(a)
  return(b)
} 

answer <- sample(1000:9999, 1)
while(TRUE){
  guess <- cat("請輸入一組四位數","\n")
  a = check(guess)
  b = check(guess)
  if(a == 4){
    print(paste("猜對了，答案是: ", answer, "共猜了 ", time))
  } else{
    print(paste("第", time, "猜數字, ", a, "A", b, "B" ))
  }
  break
}














