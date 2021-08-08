library(ISLR)
library(gtools)
library(caret)
library(ROCR)
library(MASS)
library(class)
library(tidyverse)
library(tree)
attach(Auto)
df <- diamonds
df <-  df %>% dplyr::relocate(cut, .after = last_col())
df <- as.data.frame(df)


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set
last = df[,ncol(df)]


#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor


i <- 0
accuracy <- 0
accuracytmp <- 0
accuracy.df <- data.frame(accuracy)
accuracy2 <- 0
accuracy2.df <- data.frame(accuracy2)
accuracy.best.k <- 0
accuracy.best.k.df <- data.frame(accuracy.best.k)

x =nlevels(last)
data1 <- rep(x = 0,0, x^2)
newmatrix <- matrix(rep(x = 0), nrow <- nlevels(last))
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(last))
colnames(newmatrix) <- c(levels(last))
table1 <- as.table(newmatrix)
table1.df <- as.table(newmatrix)

results.df <- as.data.frame(accuracy)
accuracy.table <- data.frame()
temptable <- table1
temptable <- as.table(table1)
results.table <- data.frame(table1.df)


for (i in 1:10){ # i measures the number of times we will update optimal.accuracy.rate.
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      cat("\014")
      print(sprintf("This is iteration is %s out of %s", i,10))
      print(sprintf("This is column %s out of %s", j,ncol(df)))
      print(sprintf("This is row %s out of %s", k,nrow(combin)))
      print(sprintf("Accuracy is %s", accuracytmp))
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.25, 0.75),2)
      dfsize <- as.integer((nrow(newdf))*ratio)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.data.frame(train.df)
      test.df <- newdf[-train,]
      test.df <- as.data.frame(test.df)
      
      
      #### Actual tree analysis is here ####
        ratio <- round(runif(1, 0.25, 0.75),2)
        train <- sort(sample(nrow(df), nrow(df)*ratio))
        df.test <- df[train,]
        last.test <- last[train]
        tree.df <- tree(last.test~., data = df.test)
        tree.pred <- predict(tree.df, df.test, type = c("class"))
        table1 <- table(tree.pred, last.test)
        accuracy <- sum(diag(table1)) / sum(table1)
        results.df <- rbind(results.df, accuracy)
        tree.final <- saveRDS(object = tree.df, file = ('/tmp/maxaccuracy.rda'))

      
      # #### Save table if it is valid
      # if(nrow(table1) == ncol(table1)){
      #   if(ncol(table1) == 2){
      #     table1 <- as.data.frame.matrix(table1)
      #     tables.df <- rbind(tables.df, table1)
      #   }
      # }
      
      
      #### - Measure overall accuracy of the tree model ####
      table1 <- table(tree.pred, last.test)
      if(nrow(table1) == ncol(table1)){
        accuracy = sum(diag(table1)) / sum(table1)
        if (accuracy>accuracytmp){
          if(sum(diag(table1))>sum(diag(temptable))){
            temptable <- table1
            table1 <- data.frame(unclass(table1))
            accuracytmp = accuracy
            table1 <- as.data.frame.matrix(table1)
            # accuracy.table = rbind(accuracy.table, table1)
            # accuracy.df <- rbind(accuracy.df, accuracy)
            saveRDS(tree.df, file = "/tmp/tree.max.accuracy.rda")            
          }
        }
      }
    }
  }
}


# ############ ----------- display results to the user ---------------- ####################
cat("\014")


print("The highest overall accuracy:")
accuracytmp
#tail(accuracy.table,n = nlevels(last))
max.accuracy <- readRDS(file = "/tmp/tree.max.accuracy.rda")
plot(max.accuracy)
text(max.accuracy, pretty = 0)
max.accuracy
