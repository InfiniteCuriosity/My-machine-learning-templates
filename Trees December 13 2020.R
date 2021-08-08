rm(list = ls())
start.time <- Sys.time()

#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
# df <- read.csv('~/Heart.csv', header = TRUE, stringsAsFactors = TRUE)
# df$AHD <- ifelse(df$AHD=="Yes", 1, 0)
# df <- df[,2:ncol(df)]

#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- iris
#df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
library(ISLR)
library(tidyverse)
library(tree)
attach(Auto)
library(gtools)
# df <- Auto
# df <- Auto[,1:(ncol(df)-1)]


#df <- select(df, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Survived)
names(df)[names(df)==names(df[ncol(df)])] <- 'last'

#df <- df[,2:ncol(df)] use if row numbers are the first column
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]
df$last <- as.factor(df$last)

last <- df$last
df <- df[,1:ncol(df)-1]
df
last

accuracy <- 0
accuracytmp <- 0
results <- data.frame(accuracy)

for (i in 1:1000) {
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
  combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      cat("\014")
      besttable <- readRDS("/tmp/besttreetable.rda")
      print(besttable)
      print(sprintf("Iteration %.1f of 1000", i))
      print(sprintf("Column %.1f of %.1f", j, ncol(df)))
      print(sprintf("Row %.1f of %.1f", k, nrow(combin)))
      print(sprintf("accuracytmp %f", accuracytmp))
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
  ratio <- round(runif(1, 0.25, 0.75),2)
  train <- sort(sample(nrow(df), nrow(df)*ratio))
  df.test <- df[train,]
  last.test <- last[train]
  tree.df <- tree(last~., data = df, subset = train)
  tree.pred <- predict(tree.df, df.test, type = c("class"))
  table1 <- table(tree.pred, last.test)
  accuracy <- sum(diag(table1)) / sum(table1)
  if(accuracy>accuracytmp){
    results <- rbind(results, accuracy)
    accuracytmp <- accuracy
    finaltree <- saveRDS(object = tree.df,file = "/tmp/besttree.rda")
    finaltable <- saveRDS(object = table1,file = "/tmp/besttreetable.rda")
      }
    }
  }
}


#### -------------- output results to the user ---------------------------- ####

results <- results$accuracy[-(1)]
min(results)
mean(results)
max(results)
plot(tree.df)
text(tree.df, pretty = 0)
hist(results,breaks = 100) # Convert using GGPlot2
besttable <- readRDS("/tmp/besttreetable.rda")
besttable
besttree <- readRDS("/tmp/besttree.rda")
summary(besttree)

