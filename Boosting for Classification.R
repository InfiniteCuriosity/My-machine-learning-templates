# Boosting - linear regression
library(gbm)
library(ISLR)
library(tidyverse)
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
names(df)[names(df)==names(df[ncol(df)])] <- 'last'
#df$last <- as.factor(df$last)
last <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
mean.squared.error <- 1000
mean.squared.error.df <- data.frame(mean.squared.error)
min.MSE <-  1000
minMSE.df <- data.frame(min.MSE)
for (m in 1:10){
  for (i in 1:ncol(df)){ # this creates all possible permutations of the columns
    print (c(m, "of 10"))
    print (c(i, "of", c(ncol(df))))
    combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
    for (j in 1:nrow(combin)){
      colvals <- c(combin[j,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
      newdf <- as.data.frame(newdf)

      ratio <- round(runif(1, 0.25, 0.75),2)
      train <- sort(sample(nrow(df), nrow(df)*ratio))
      df.train <- as.data.frame(df[train,])
      df.test <- as.data.frame(df[-train,])
      
      gbm.fit <- gbm(last[train]~., data = df.train, distribution = "bernoulli",n.trees = 5000, shrinkage = 0.2)
      boost.predict <- predict(gbm.fit, newdata = df.test, n.trees = 5000, type = "link")
      mean.squared.error <- mean((boost.predict - last[-train])^2)
      mean.squared.error.df <- rbind(mean.squared.error.df, mean.squared.error)
      
      if(mean.squared.error <= min.MSE){
        min.MSE = mean.squared.error
        minMSE.df <- rbind(minMSE.df, min.MSE)
        best.boost <- saveRDS(object = boost.df, file = "/tmp/best.boost.rda")
      }
    }
  }
  

}

sprintf("The best model predicts within %f miles per gallon of the true value", sqrt(min(minMSE.df)))

best.boost <- readRDS(file = "/tmp/best.boost.rda")
best.boost


minMSE.df
