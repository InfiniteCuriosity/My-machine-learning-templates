library(tidyverse)
library(glmnet)
library(ISLR)
library(gtools)
# df <- mtcars                                                             
# df <-  df %>% dplyr::relocate(mpg, .after = last_col())
# names(df)[names(df)==names(df[ncol(df)])] <- 'last'



df <- Hitters
df <- na.omit(df)
last <- df$Salary
df$League <- ifelse(df$League=="A", 1, 0)
df$Division <- ifelse(df$Division == "E", 1, 0)
df$NewLeague <- ifelse(df$NewLeague == "A", 1, 0)
df <- df %>% dplyr::relocate(Salary, .after = last_col())
df <- df[,1:ncol(df)-1]
df <- scale(df)



#### Initialize variables we will be using ####

MSEtemp <- 10000
MSE <- 10000
MSE.df <- data.frame(MSE)
MSEtemp.df <- data.frame(MSEtemp)
y_predicted <- 0
y_predicted.df <- data.frame(y_predicted)
colnames(y_predicted.df) = "1"
r.squared <- 0
r.squared.df <- data.frame(r.squared)


i = 1
y <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
for (i in 1:ncol(df)){ # this creates all possible permutations of the columns
  print (c(i, "of", c(ncol(df))))
  combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
  for (j in 1:nrow(combin)){
    colvals <- c(combin[j,])
    newdf <- data.frame(df[,colvals])
    newdf <- cbind(newdf, last)
    newdf <- as.data.frame(newdf)
    newdf <- as.matrix(df)
    grid=10^seq(10,-2,length=100)
    model <- glmnet(newdf, y, alpha = 0,lambda = grid)
    cv_model <- cv.glmnet(newdf, y, alpha = 0)
    best_lambda <- cv_model$lambda.min

#find coefficients of best model
best_model <- glmnet(newdf, y, alpha = 0, lambda = best_lambda)

#calculate R-squared of model on training data
y_predicted <- predict(model, s = best_lambda, newx = newdf)


# Calculate MSE:
MSE <- mean(y - y_predicted)^2
if(MSE<MSEtemp){
  MSEtemp.df <- rbind(MSEtemp.df, MSEtemp)
  MSEtemp <- MSE
  MSE.df <- rbind(MSE.df, MSE)
  y_predicted.df <- rbind(y_predicted.df, y_predicted)
  best.ridge.model <- saveRDS(best_model, "/tmp/best.ridge.model.rda")
  sst <- sum((y - mean(y))^2)
  sse <- sum((y_predicted - y)^2)
  r.squared <- 1 - sse/sst
  r.squared.df <- rbind(r.squared.df, r.squared)
    }
  }
}


###### ------ output results to the user ------------- ###########
best.ridge.model <- readRDS('/tmp/best.ridge.model.rda')
best.ridge.model

print(tail(MSE.df))
print(tail(MSEtemp.df))
print(tail(r.squared.df))



###########  --------- Lasso Regression -------------- ################

library(tidyverse)
library(glmnet)
library(ISLR)
# df <- mtcars                                                             
# df <-  df %>% dplyr::relocate(mpg, .after = last_col())
# names(df)[names(df)==names(df[ncol(df)])] <- 'last'
# df <- scale(df)

df <- Hitters
df <- na.omit(df)
last <- df$Salary
df$League <- ifelse(df$League=="A", 1, 0)
df$Division <- ifelse(df$Division == "E", 1, 0)
df$NewLeague <- ifelse(df$NewLeague == "A", 1, 0)
df <- df %>% dplyr::relocate(Salary, .after = last_col())
df <- df[,1:ncol(df)-1]
df <- scale(df)



#### Initialize variables we will be using ####

MSEtemp <- 10000
MSE.df <- data.frame(MSE)
MSEtemp.df <- data.frame(MSEtemp)
y_predicted <- 0
y_predicted.df <- data.frame(y_predicted)
colnames(y_predicted.df) = "1"
r.squared <- 0
r.squared.df <- data.frame(r.squared)


i = 1
y <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
for (i in 1:ncol(df)){ # this creates all possible permutations of the columns
  print (c(i, "of", c(ncol(df))))
  combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
  for (j in 1:nrow(combin)){
    colvals <- c(combin[j,])
    newdf <- data.frame(df[,colvals])
#    newdf <- cbind(newdf, last)
    newdf <- as.data.frame(newdf)
    newdf <- as.matrix(df)
    grid=10^seq(10,-2,length=100)
    model <- glmnet(newdf, y, alpha = 0,lambda = grid)
    cv_model <- cv.glmnet(newdf, y, alpha = 1)
    best_lambda <- cv_model$lambda.min
    
    #find coefficients of best model
    best_model <- glmnet(newdf, y, alpha = 1, lambda = best_lambda)
    
    #calculate R-squared of model on training data
    y_predicted <- predict(model, s = best_lambda, newx = newdf)
    
    
    # Calculate MSE:
    MSE <- mean(y - y_predicted)^2
    if(MSE<MSEtemp){
      MSEtemp.df <- rbind(MSEtemp.df, MSEtemp)
      MSEtemp <- MSE
      MSE.df <- rbind(MSE.df, MSE)
      y_predicted.df <- rbind(y_predicted.df, y_predicted)
      best.ridge.model <- saveRDS(best_model, "/tmp/best.ridge.model.rda")
      sst <- sum((y - mean(y))^2)
      sse <- sum((y_predicted - y)^2)
      r.squared <- 1 - sse/sst
      r.squared.df <- rbind(r.squared.df, r.squared)
    }
  }
}


###### ------ output results to the user ------------- ###########
best.ridge.model <- readRDS('/tmp/best.ridge.model.rda')
summary(best.ridge.model)
coef(best.ridge.model)

print(tail(MSE.df))
print(tail(MSEtemp.df))
print(tail(r.squared.df))
