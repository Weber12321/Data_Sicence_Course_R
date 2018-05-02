library(magrittr)
library(ggplot2)
library(dplyr)

# Read the titanicTrain data and store it in titanic

train <- read.csv("titanicTrain.csv")
train <- train[c(1:1000),]

# Train data 上面的NA
str(train)
sapply(train, function(x) {sum(is.na(x))})


## embarked ##
train$embarked <- train$embarked %>% as.factor
summary(train$embarked[train$survived==0])
summary(train$embarked[train$survived==1])
embark <- cbind(summary(train$embarked[train$survived==0]),summary(train$embarked[train$survived==1]))
embark <- embark[-1,] %>% t
rownames(embark) <- c("0","1")

barplot(embark,col=c("gray","black"),main="embarked variable",beside=TRUE,ylab="counts")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)

## family = sibsp + parch + 1  ##

family <- train$parch + train$sibsp + 1
train <- cbind(train,family)

ps0 <- train$family[train$survived==0] %>% as.factor %>% summary
ps1 <- train$family[train$survived==1] %>% as.factor %>% summary %>% c(.,0)
family <- rbind(ps0,ps1)
rm(ps0)
rm(ps1)

barplot(family,col=c("gray","black"),main="family",beside=TRUE,ylab="counts",xlab="Number of people")
legend("topright", inset=.02,title="Survive",
       c("0","1"), fill=c("gray","black"), horiz=TRUE, cex=0.8)


## boat variable ##
train$survived <- as.factor(train$survived)
train$boat <- train$boat %>% as.character()
train$boat[train$boat == "5 7"] <- "17"
train$boat[train$boat == "5 9"] <- "18"
train$boat[train$boat == "8 10"] <- "19"
train$boat[train$boat == "13 15"] <- "20"
train$boat[train$boat == "13 15 B"] <- "21"
train$boat[train$boat == "15 16"] <- "22"
train$boat[train$boat == "A"] <- "23"
train$boat[train$boat == "B"] <- "24"
train$boat[train$boat == "C"] <- "25"
train$boat[train$boat == "D"] <- "26"
train$boat[train$boat == "C D"] <- "27"
train$boat[is.na(train$boat)] <- "28"
train$boat[train$boat == ""] <- "28"
#escape = 1 refer to the refugee who successfully took on boat
train$escape[train$boat != "28"] <- "1"
train$escape[train$boat == "28"] <- "0"
#see if taking boat is highly related survived

b1 <- ggplot(train$survived, aes(x = escape, fill = survived)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'escape from ship') +
  geom_label(stat='count', aes(label=..count..))

#subset of those who took on boat
survived_boat <- subset(train, train$boat != "28")
#see if there anyone who took on boat and didn't survive 
b2 <- ggplot(survived_boat, aes(x = boat, fill = survived)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'boat_survive') + theme_grey()

