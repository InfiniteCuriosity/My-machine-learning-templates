# Boosting - linear regression
library(gbm)
library(ISLR)
library(tidyverse)
library(gtools)
library(MASS)
# df <- Auto[,1:ncol(Auto)-1]
# df <-  df %>% dplyr::relocate(mpg, .after = last_col())
# label <- colnames(df)[ncol(df)]

# df <- Carseats
# df <-  df %>% dplyr::relocate(Price, .after = last_col())
# label <- colnames(df)[ncol(df)]

df <- Boston
df <-  df %>% dplyr::relocate(medv, .after = last_col())
label <- colnames(df)[ncol(df)]

names(df)[names(df)==names(df[ncol(df)])] <- 'last'

mean.squared.error <- 10000
min.MSE <- 10000
mean.squared.error.df <- data.frame(mean.squared.error)
minMSE.df <- data.frame(min.MSE)


last <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)


# for (i in 1:ncol(df)){
#   df[,i] =  poly(df[,i],4, raw = TRUE)
# }


for (m in 1:10){
  for (i in 1:ncol(df)){ # this creates all possible permutations of the columns
    print (c(m, "of 10"))
    print (c(i, "of", c(ncol(df))))
    combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
    for (j in 1:nrow(combin)){
      print(sprintf("The best model predicts %s within %f of the true value", label, sqrt(min(minMSE.df))))
      colvals <- c(combin[j,])
      print(colvals)
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
      newdf <- as.data.frame(newdf)
      ratio <- round(runif(1, 0.25, 0.75),2)
      train <- sort(sample(nrow(df), nrow(df)*ratio))
      df.train <- as.data.frame(df[train,])
      df.test <- as.data.frame(df[-train,])
      
      boost.df <- gbm(last[train]~., data = df.train, distribution = "gaussian",n.trees = 100, shrinkage = 0.2)
      boost.predict <- predict(boost.df, newdata = df.test, n.trees = 50)
      #mean.squared.error <- mean((boost.predict - df.test)^2)
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

sprintf("The best model predicts %s within %f miles per gallon of the true value", label, sqrt(min(minMSE.df)))

best.boost <- readRDS(file = "/tmp/best.boost.rda")
best.boost

summary(boost.df)
summary(best.boost)
# boost.df$initF
# boost.df$var.names
# boost.df$Terms
# boost.df$m
