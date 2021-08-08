rm(list = ls())
start.time <- Sys.time()

#### Load packages we will use ####
library(tidyverse)
library(gtools)
library(Metrics)
library(ISLR)
library(MASS)
library(caret)

#### Read in the data frame, ALWAYS save with the name 'df' ####
# Note - all data frames MUST be Tidy before reading in

df <- Carseats
df <-  df %>% dplyr::relocate(Price, .after = last_col())
df$Urban <- ifelse(df$Urban=="Yes", 1,0)
df$US <- ifelse(df$US=="Yes", 1, 0)
df$ShelveLoc <- ifelse(df$ShelveLoc=="Bad", 1, ifelse(df$ShelveLoc == "Medium", 2,3))


# df <- mtcars
# df <-  df %>% dplyr::relocate(mpg, .after = last_col())


# df <- diamonds
# df <- as.data.frame(df)
# df <- df[,c(1,5:ncol(df))]
# df <-  df %>% dplyr::relocate(price, .after = last_col())


#### If needed, save the data frame as a data frame ####
df.final <- as.data.frame(df)

desired.feature <- colnames(df)[length(colnames(df))]


#### Initialize variables we will be measuring
adj.r.squared <-0
Mean.Squared.Error <- 0
adj.r.squared.df <- data.frame(adj.r.squared)
MSE.df <- data.frame(Mean.Squared.Error)
max.adj.r.squared <- 0
min.MSE <- 10000000000000
min.mae <- 100000
mae.value <- 0
mae.df <- data.frame(mae.value)
min.k.mae <- 1000000
max.k.adj.r.squared <- 0
min.k.min.MSE <- 10000
difference <- 1000
difference1 <- 1000000000000
adj.r.squared.test <-  1
adj.r.squared.train <- 1
max.adj.r.squared.test <- 0
number <- 0
m <- 0



#### Use all permutations of columns, and run the analysis ####
i = 1
last <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
for (i in 1:1){
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    print (c(i, "of", c(ncol(df))))
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE)
    for (k in 1:nrow(combin)){

      for (l in 1:100) {
        
        m = m+1
        cat("\014")
        print(sprintf("Iteration %0.1f of 10", i))
        print(sprintf("Column %.1f of %.1f", j, ncol(df)))
        print(sprintf("Row %.1f of %.1f", k, nrow(combin)))
        print(sprintf("Max Adjusted r-squared %f", max.adj.r.squared))
        print(sprintf("Difference = %f", difference))
        print(sprintf("Min Mean Squared Error %f", min.MSE))
        print(sprintf("The total number of models = %0.0f", m))
        print(sprintf("The total run time so far = %f", Sys.time() - start.time))
        # print(sprintf("Min Mean Absolute Error %f", min.mae))
        
        colvals <- c(combin[k,])
        newdf <- data.frame(df[,colvals])
        newdf <- newdf^(l/10)
        newdf <- cbind(newdf, last)
        newdf <- as.data.frame(newdf)
        findLinearCombos(newdf)$remove
        
        
        
        # Break the data set into random amounts of test and train
        ratio <- round(runif(1, 0.25, 0.75),2)
        dfsize <- as.integer((nrow(newdf))*ratio)
        train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
        train.df <- newdf[train,]
        train.df <- as.data.frame(train.df)
        test.df <- newdf[-train,]
        test.df <- as.data.frame(test.df)
        
        newdf.lm = lm(last~.*., data = train.df)
        predict <- as.numeric(newdf.lm$fitted.values)
        mae.value <- mae(predicted = predict,actual = last[train])
        adj.r.squared.train <- summary(newdf.lm)$adj.r.squared
        Mean.Squared.Error <- mean(newdf.lm$residuals^2)
        
        newdf.lm1 = lm(last~.*., data = test.df)
        predict <- as.numeric(newdf.lm1$fitted.values)
        mae.value <- mae(predicted = predict,actual = last[-train])
        adj.r.squared.test <- summary(newdf.lm1)$adj.r.squared
        Mean.Squared.Error1 <- mean(newdf.lm1$residuals^2)

        if (abs(adj.r.squared.train - adj.r.squared.test) < difference){
          if(adj.r.squared.test > max.adj.r.squared){
              model1 <- saveRDS(newdf.lm, file = "/tmp/max.r.squared.rda")
              model2 <- saveRDS(newdf.lm1, file = "/tmp/max.r.squared2.rda")
              max.adj.r.squared = max(adj.r.squared.test, adj.r.squared.train)
              difference = abs(adj.r.squared.train - adj.r.squared.test)
          }
        }              
        if (abs(Mean.Squared.Error - Mean.Squared.Error1) < difference1){
            model3 <- saveRDS(newdf.lm, file = "/tmp/min.mse1.rda")
            model4 <- saveRDS(newdf.lm1, file = "/tmp/min.mse2.rda")
            difference1 <- abs(Mean.Squared.Error - Mean.Squared.Error1)
            min.MSE <- min(Mean.Squared.Error, Mean.Squared.Error1)
        }
      }
    }
  }
}




#### Output the results for the user ####

cat("\014") # clears the R console

r.squared.final <- adj.r.squared.df[2:nrow(adj.r.squared.df),]
# max(r.squared.final)
# min(r.squared.final)
# mean(r.squared.final)
# sort(r.squared.final,decreasing = TRUE)[1:10]

# MSE <- MSE.df[2:nrow(MSE.df),]
# max(MSE)
# min(MSE)
# mean(MSE)
# sort(MSE, decreasing = FALSE)[1:10]
# 
# mae <- as.numeric(mae.df$mae.value)
# mae <- mae[2:length(mae)]
# max(mae)
# min(mae)
# mean(mae)
# sort(mae, decreasing = FALSE)[1:10]
# hist(mae,breaks = 100)
# 
# par(mfrow = c(1,3))
# hist(r.squared.final,breaks = 100)
# hist(MSE.df$Mean.Squared.Error, breaks = 100)
# hist(mae, breaks = 100)

print("The model with the highest adjusted r-squared:")
highest.r.squared.model <- readRDS(file = "/tmp/max.r.squared.rda")
summary(highest.r.squared.model)

highest2 <- readRDS(file = "/tmp/max.r.squared2.rda")
summary(highest2)

print("The model with the lowest MSE for test and train:")
lowest.mse.model <- readRDS(file ="/tmp/min.mse1.rda")
summary(lowest.mse.model)
lowest.mse.model_01 <- readRDS(file ="/tmp/min.mse2.rda")
summary(lowest.mse.model_01)
sprintf("This model predicts %s within %s", desired.feature, round(sqrt(mean(lowest.mse.model$residuals^2)),2))
print("To verify the lowest MSE")
mean(lowest.mse.model$residuals^2)
# 
# print("The model with the lowest MAE:")
# lowest.mae.model <- readRDS(file = "/tmp/min.mae.rda")
# summary(lowest.mae.model)

#Check that the mean of the residuals is close to zero:
mean(highest.r.squared.model$residuals)

# Check the model diagnostics
# gvlma::gvlma(highest.r.squared.model)
# plot(highest.r.squared.model)

last.df <- data.frame(last)
ggplot(data = last.df,mapping = aes(y = last)) +
  geom_boxplot()


sqrt(mean(lowest.mse.model$residuals^2))
sqrt(mean(lowest.mse.model_01$residuals^2))
endtime <- system.time()
duration <- endtime - start.time
print(duration)
