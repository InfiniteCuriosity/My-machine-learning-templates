library(tidyverse)
library(ISLR)
library(MASS)
library(gtools)

# Import the data
df <- iris

# Clean the data
# df %>% dplyr::select(df, carat, cut, color, depth, table, price, x,  y, z, clarity)
# df <-  df <-  dplyr::select(df, carat, cut, color, depth, table, price, x, y, z, clarity)

# Name the last column as "Last", we will use it later in the analysis
names(df)[names(df)==names(df[ncol(df)])] = 'last'
df <- as.data.frame(df)

#### randomize the rows ######
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]

# Set up our set of true values, based on the randomized rows
last <- df$last

#### Initialize variables ####
accuracy <- 0
accuracytemp <- 0
results <- data.frame(accuracy)
sumdiagtmp <- 0

#### Make basic confusion matrix, based on the dimensions of the feature we are studying ####
x =nlevels(df$last)
data1 <- rep(x = 0,0, x^2)
newmatrix <- matrix(rep(x = 0), nrow <- nlevels(df$last))
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
table1.df <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1.df))
results.table <- data.frame(table1.df)
results.table


#### Analysis starts here ####
for (i in 1:1000){#accuracy increases as the 2nd number is raised: 1:10 gives higher accuracy results than 1:1 or 1:2, but this increases run time.
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
      
      
      #### Run linear discriminant analysis ####
      df.qda <-  qda(last~., data = df.train)
      df.pred <- predict(df.qda, df.test)
      
      # Make the predictions based on unseen (new) data
      pred = predict(df.qda, df.test)
      Pred.Class <- pred$class
      
      
      # Output results table
      Table1 <- table(Pred.Class, df.test$last)
      table1.df <- data.frame(unclass(Table1))
      
      
      # Calculate accuracy
      accuracy <- sum(diag(Table1)) / sum(Table1)
      
      # Save accuracy data, only add rows if accuracy increases
      if(sum(diag(Table1))>sumdiagtmp){
        if(accuracy>=accuracytemp){
          results.table <- rbind(results.table, table1.df)
          accuracytemp <-  accuracy
          sumdiagtmp <-  sum(diag(Table1))
          results <- rbind(results, accuracy)
          saveRDS(df.qda, file = "/tmp/qda.max.accuracy.rda")
        }
      }
    }
  }
  
}

######## ------- display results to the user ------------ ###############

results <- results[2:nrow(results),]
tail(results.table,n = nlevels(last))
min(results)
mean(results)
max(results)
results <- as.data.frame(results)
ggplot(data = results,mapping = aes(x = results)) +
  geom_histogram(binwidth = .001)
max.accuracy <- readRDS(file = "/tmp/qda.max.accuracy.rda")
max.accuracy

