#project intro
#=========================
#1. this is a project for practicing the apriori ML method 
#   as the project#4 for the data science class
#2. this is complished together by the members of group 5
#   so you might receive same version of everyone in group 5

#3. in this case we used data from dataset::iris for variables
#   and through process the dataset to find out the outcome of prediction 
#=========================

#load the required package
rm(list=ls())
library(magrittr)

#build the dataset, and summarize it
datasets::iris
str(iris)
data <- iris
summary(data)

#as factor species 
data$Species <- data$Species %>% as.factor()


#use a function to turn the numeric value of both petal and sepal into
# "small", "median", "large" interval
Class<- function(Ary){
  DATA = c()
  Min = min(Ary)
  Max = max(Ary)
  Inter = (Max - Min)/3
  
  Output <- sapply(Ary,function(x){
    if(Min<= x && x < Min+Inter){
      DATA=c(DATA,"small")
    }else if(Min+Inter<= x && x < Min+Inter*2){
      DATA=c(DATA,"median")
    }else if(Min+Inter*2<= x && x <= Min+Inter*3){
      DATA=c(DATA,"large")
    }
  })
  return(Output)
}  

#as factor all value of petal and sepal
data$Sepal.Length <- Class(data$Sepal.Length) %>% as.factor()
data$Sepal.Width <- Class(data$Sepal.Width) %>% as.factor()
data$Petal.Length <- Class(data$Petal.Length) %>% as.factor()
data$Petal.Width <- Class(data$Petal.Width) %>% as.factor()

#load the arules method
require(arules)

#apriori data
rule <- apriori(data, 
  parameter = list(minlen=4, supp=0.1, conf=0.7),
  appearance = list(default = "lhs",
                    rhs = c("Species=setosa", "Species=versicolor", "Species=virginica")))
#observe data by lift
inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

#load the plotting method and plot those outcomes
require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")

