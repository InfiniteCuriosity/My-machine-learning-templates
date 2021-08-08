library(tidyverse)
library(ISLR)
library(MASS)

#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('~/Heart.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- iris
names(df)[names(df)==names(df[ncol(df)])] <- 'last'
#df <- df[,2:ncol(df)]
# nlevels(df$last)
length(levels(df$last))
# 
# for (i in 1:nlevels(df$last)){
#   print(i)
#   print(levels(df$last[i]))
# }
# 
# 
# 
# table1.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))

levels(df$last)[3]
  
str(df)

x =length(levels(df$last))
x
x^2
data1 <- seq(1:x^2)
data1


newmatrix <- matrix()

newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
newmatrix
newmatrix <- as.table(newmatrix)
newmatrix

rownames(table1.df) = c("No", "Yes")

randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]

accuracy <- 0
results <- data.frame(accuracy)
lda.table.df <- data.frame()


#for(i in 1:100){
  ratio <- round(runif(1, 0.05, 0.95),2)
  train <- sort(sample(nrow(df), nrow(df)*ratio))
  df.train <- df[train,]
  df.test <- df[-train,]
  
  df.lda <-  lda(last~., data = df.train)
  df.pred <- predict(df.lda, df.test)
  
  pred = predict(df.lda, df.test)
  
  Pred.Class <- pred$class
  
  Table1 <- table(Pred.Class, df.test$last)
  lda.table.df <- data.frame(Table1)
  lda.table.df <- rbind(Table1)
  accuracy <- sum(diag(Table1)) / sum(Table1)
  accuracy # highest = 0.8057554
  results <- rbind(results, accuracy)  
#}

Table1

results <- results$accuracy[-(1)]
min(results)
mean(results)
max(results)
hist(results,breaks = 50)
Table1
lda.table.df
