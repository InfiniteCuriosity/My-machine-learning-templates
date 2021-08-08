library(tidyverse)
library(ISLR)
library(MASS)
library(gtools)

#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('~/Heart.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- iris
#names(df)[names(df)==names(df[ncol(df)])] <- 'last'
df <- diamonds
df %>% dplyr::select(df, carat, cut, color, depth, table, price, x,  y, z, clarity)
df <-  df <-  dplyr::select(df, carat, cut, color, depth, table, price, x, y, z, clarity)
names(df)[names(df)==names(df[ncol(df)])] = 'last'
df <- as.data.frame(df)

#### randomize the rows ######
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]
last <- df$last

#### Initialize variables ####
accuracy <- 0
accuracytemp <- 0
results <- data.frame(accuracy)

#### Make basic confusion matrix ####
x =nlevels(df$last)
data1 <- seq(1:x^2)
newmatrix <- matrix()
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
table1.df <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1.df))
results.table <- data.frame(table1.df)

sumdiagtmp <- 0

for(i in 1:10){
  print(i)
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)

      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.05, 0.95),2)
      train <- sort(sample(nrow(df), nrow(df)*ratio))
      df.train <- as.data.frame(df[train,])
      df.test <- as.data.frame(df[-train,])
    
      
      #### Run linear discriminant analysis ####
      df.lda <-  lda(last~., data = df.train)
      df.pred <- predict(df.lda, df.test)
      pred = predict(df.lda, df.test)
      Pred.Class <- pred$class
  
      
      # Output results table
      Table1 <- table(Pred.Class, df.test$last)
      table1.df <- data.frame(unclass(Table1))

      
      # Calculate accuracy
      accuracy <- sum(diag(Table1)) / sum(Table1)

      # save accuracy data
      if(sum(diag(Table1))>sumdiagtmp){
        if(accuracy>=accuracytemp){
          results.table <- rbind(results.table, table1.df)
            accuracytemp <-  accuracy
            sumdiagtmp <-  sum(diag(Table1))
            results <- rbind(results, accuracy)
            saveRDS(df.lda, file = "/tmp/lda.max.accuracy.rda")
        }
      }
    }
  }
}

######## ------- display results to the user ------------ ###############

results <- results[2:nrow(results),]
results.table[(nrow(results.table)-(nlevels(df$last))+1):nrow(results.table),]
min(results)
mean(results)
max(results)
hist(results,breaks = 50)



