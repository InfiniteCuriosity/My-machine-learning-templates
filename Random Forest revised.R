library(tidyverse)
library(randomForest)
library(ISLR)
df <- Carseats
df <-  df %>% dplyr::relocate(ShelveLoc, .after = last_col())

names(df)[names(df)==names(df[ncol(df)])] <- 'last'
# add in a results data frame:
accuracy <- 0
accuracy.df <- data.frame(accuracy)
results <- data.frame(accuracy)

table1.df <- data.frame(table1)

# randomize the rows
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]
# isolate the TRUE labels IN THE ORDER OF THE RANDOM DATA SET
dfLabels = df[,ncol(df)]

accuracytmp <- 0

x =nlevels(df$last)
data1 <- rep(x = 0,0, x^2)
newmatrix <- matrix(rep(x = 0), nrow <- nlevels(df$last))
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
table1.df <- as.table(newmatrix)
temptable <- table1.df
table1.df <- data.frame(unclass(table1.df))
table1.df

sumdiag <- 0


for(i in 1:10000){
  print(i)
  ratio <- round(runif(1, 0.25, 0.75),2)
  Train <- sort(sample(nrow(df), nrow(df)*ratio))
  df.train <- df[Train,]
  df.test <- df[-Train,]
  dfLabelsTrain <- dfLabels[Train]
  dfLabelsTest <- dfLabels[-Train]
  
  df.rf = randomForest(df$last~., data = df, importance = TRUE, subset = Train)
  matrix1 <- df.rf$confusion
  matrix1 <- matrix1[,1:ncol(matrix1)-1]
  accuracy <- sum(diag(matrix1)) / sum(matrix1)
  results <- rbind(results, accuracy)
  if(accuracy >= accuracytmp){
      accuracytmp <- accuracy
      accuracy.df <- rbind(accuracy.df, accuracy)
      table1 <- as.data.frame.matrix(matrix1)
      table1.df <- rbind(table1.df, table1)
      sumdiag <- sum(matrix1)
    }
  }

accuracy.df
which(results$accuracy==1)

results <- results$accuracy[-(1)]
min(results)
mean(results)
max(results)
hist(results,breaks = 100)

df.rf
