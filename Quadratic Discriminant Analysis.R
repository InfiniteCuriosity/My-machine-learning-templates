library(tidyverse)
library(ISLR)
library(MASS)

#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('~/Heart.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- Smarket
df$Direction <- ifelse(df$Direction=="Up", 1, 0)
names(df)[names(df)==names(df[ncol(df)])] <- 'last'
df$last <- as.factor(df$last)
#df <- df[,2:ncol(df)]
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]

accuracy <- 0
accuracytemp <- 0
results <- data.frame(accuracy)
x =length(levels(df$last))
data1 <- seq(1:x^2)
newmatrix <- matrix()
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
table1.df <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1.df))
results.table <- data.frame(table1.df)

sumdiagtmp <- 0

for(i in 1:100){
  ratio <- round(runif(1, 0.25, 0.75),2)
  train <- sort(sample(nrow(df), nrow(df)*ratio))
  df.train <- df[train,]
  df.test <- df[-train,]
  
  df.qda <-  qda(last~., data = df.train)
  df.pred <- predict(df.qda, df.test)
  
  pred = predict(df.qda, df.test)
  
  Pred.Class <- pred$class
  
  Table1 <- table(Pred.Class, df.test$last)
  table1.df <- data.frame(unclass(Table1))
  
  accuracy <- sum(diag(Table1)) / sum(Table1)
  results <- rbind(results, accuracy)
  if(sum(diag(Table1))>sumdiagtmp){
    if(accuracy>=accuracytemp){
      results.table <- rbind(results.table, table1.df)
      accuracytemp <-  accuracy
      sumdiagtmp <-  sum(diag(Table1))
      saveRDS(df.qda, file = "/tmp/qda.max.accuracy.rda")
    }
  }
  
}

######## ------- display results to the user ------------ ###############
cat("\014") # clears Console in R
#results <- results$accuracy[-(1)]
min(results)
mean(results$accuracy)
max(results)
hist(results$accuracy,breaks = 50)
results.table[(nrow(results.table)-1):nrow(results.table),]

print("The model with the highest accuracy is:")
bestqda <- readRDS(file = "/tmp/qda.max.accuracy.rda")
bestqda