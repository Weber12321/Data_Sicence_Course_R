library(e1071)
library(magrittr)

train <- read.csv("titanicTrain2.csv", stringsAsFactors = F, na.strings = c("NA", ""))
test <- read.csv("titanicQuestion.csv", stringsAsFactors = F, na.strings = c("NA", ""))
all <- rbind(train, test)

#survived
all$survived <- all$survived %>% as.factor
all$sex <- all$sex %>% as.factor
all$pclass <- all$pclass %>% as.ordered

#PclassSex
all$pclassSex[all$pclass=='1' & all$sex=='male'] <- 'P1Male'
all$pclassSex[all$pclass=='2' & all$sex=='male'] <- 'P2Male'
all$pclassSex[all$pclass=='3' & all$sex=='male'] <- 'P3Male'
all$pclassSex[all$pclass=='1' & all$sex=='female'] <- 'P1Female'
all$pclassSex[all$pclass=='2' & all$sex=='female'] <- 'P2Female'
all$pclassSex[all$pclass=='3' & all$sex=='female'] <- 'P3Female'
all$pclassSex <- as.factor(all$pclassSex)

#family
all$family <- all$parch + all$sibsp + 1

#boat
all$boat[is.na(all$boat)] <- "0"
all$boat[all$boat != "0"] <- "1"
all$boat <- all$boat %>% as.factor()

#body
all$body[is.na(all$body)] <- "0"
all$body[all$body != "0"] <- "1"
all$body <- all$body %>% as.factor()

#embark
all$embarked[is.na(all$embarked)] <- "X"
all$embarked <- all$embarked %>% as.factor()

#children
all$age[all$age <= 14] <- 1
all$age[all$age != 1] <- 0
all$age[is.na(all$age)] <- 0
all$age <- all$age %>% as.factor()

clean_test <- all[is.na(all$survived),]
clean_train <- all[!is.na(all$survived),]





#SVM
set.seed(1001)
titanic_svm <- train(survived~ pclass + sex + family + body + boat, data = clean_train, method='svmRadial', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number = 5))
titanic_svm

solution_svm <- predict(titanic_svm, clean_test)
solution_svm

test$survived <- solution_svm
write.csv(test, file = "answer.csv")

#GBM
#set.seed(1001)
#titanic_boost <- train(survived~ pclass + sex + family + body + boat, data = clean_train, method='gbm', preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5), verbose=FALSE)
#print(titanic_boost)

#titanic_boost <- predict(titanic_boost, clean_test)
#titanic_boost
