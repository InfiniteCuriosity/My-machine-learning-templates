# test of support vector classifier

library(e1071)
library(ISLR)
library(tidyverse)
df <- Auto
df <- df[,1:ncol(df)-1]
str(df)
#df <-  df %>% dplyr::relocate(ShelveLoc, .after = last_col())

accuracy <- 0
accuracytemp <- 0
results <- data.frame(accuracy)

sumdiagtmp <- 0

results.table <- data.frame()

#### randomize the rows ######
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]

# Name the last column as "Last", we will use it later in the analysis
names(df)[names(df)==names(df[ncol(df)])] = 'last'
df <- as.data.frame(df)

# Set up our set of true values, based on the randomized rows
last <- df$last
last <- as.factor(last)

#### Analysis starts here ####
for (i in 1:1){#accuracy increases as the 2nd number is raised: 1:10 gives higher accuracy results than 1:1 or 1:2, but this increases run time.
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      print(i)
      print(j)
      print(k)
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
      
      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.25, 0.75),2)
      train <- sort(sample(nrow(df), nrow(df)*ratio))
      df.train <- as.data.frame(df[train,])
      df.test <- as.data.frame(df[-train,])
      
      
      svm.fit <- svm(last[train]~., data = df.train, type = "C-classification", kernel = 'linear')
      y_pred <- predict(svm.fit, newdata = df.test)
      cm <- table(last[-train],y_pred)
      accuracy <- sum(diag(cm))/sum(cm)
      
        if(accuracy>=accuracytemp){
          cm.matrix <- as.data.frame.matrix(cm)
          results.table <- rbind(results.table, cm.matrix)
          accuracytemp <-  accuracy
          results <- rbind(results, accuracy)
          saveRDS(svm.fit, file = "/tmp/svm.max.accuracy.rda")
      }
    }
  }
  }

readRDS('/tmp/svm.max.accuracy.rda')

max(results)

tail(x = results.table, n = nlevels(last))


