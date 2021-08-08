library(tidyverse)
library(randomForest)
library(ISLR)
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
df <- Carseats
df <-  df %>% dplyr::relocate(ShelveLoc, .after = last_col())
names(df)[names(df)==names(df[ncol(df)])] <- 'last'
last <- df$last
# add in a results data frame:
accuracy <- 0
accuracytmp <- 0
results <- data.frame(accuracy)
accuracy.df <- data.frame(table1)

x =nlevels(last)
data1 <- rep(x = 0,0, x^2)
newmatrix <- matrix(rep(x = 0), nrow <- nlevels(last))
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(last))
colnames(newmatrix) <- c(levels(last))
table1 <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1))
results.table <- data.frame(table1.df)
accuracy.table <- data.frame()
accuracy.df <- data.frame(accuracy)
temptable <- table1
temptable <- as.table(table1)
temptable.df <- data.frame()
table2.df <- data.frame()


# randomize the rows
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]
# isolate the TRUE labels IN THE ORDER OF THE RANDOM DATA SET
dfLabels = df[,ncol(df)]

for(i in 1:1){
  
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      print(i)
      print(j)
      print(k)
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      
      
      ratio <- round(runif(1, 0.25, 0.75),2)
      Train <- sort(sample(nrow(df), nrow(df)*ratio))
      df.train <- df[Train,]
      df.test <- df[-Train,]
      dfLabelsTrain <- dfLabels[Train]
      dfLabelsTest <- dfLabels[-Train]
      
      df.rf = randomForest(df$last~., data = df, importance = TRUE, subset = Train)
      table1 = df.rf$confusion
      table1 = table1[,1:ncol(table1)-1]
      accuracy = sum(diag(table1)) / sum(table1)
      results = rbind(results, accuracy)
      table1.df = rbind(table1.df, table1)
      
      #### ---- save the tables in increasing order of accuracy ---- ####
      if(accuracy>=accuracytmp){
        accuracytmp = accuracy
        accuracy.df <- rbind(accuracy.df, accuracy)
        temptable = table1
        accuracy.table <- rbind(accuracy.table, table1)
        max.accuracy <- saveRDS(object = df.rf,file = "/tmp/maxrandomforest.rda")
      }
    }
  }
}

######## ----------- display results to the user ------------- ##################3

which(results$accuracy==1)
results <- results[2:nrow(results),]
min(results)
mean(results)
max(results)
tail(accuracy.table,n = nlevels(last))
hist(results,breaks = 100)
max.accuracy <- readRDS("/tmp/maxrandomforest.rda")
max.accuracy
max.accuracy.terms <- readRDS("/tmp/maxaccuracyterms.rda")
max.accuracy.terms
