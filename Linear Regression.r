#### Load packages we will use ####
library(tidyverse)
library(gtools)
library(Metrics)
library(ISLR)
library(MASS)
library(caret)

#### Read in the data frame, ALWAYS save with the name 'df' ####
# Note - all data frames MUST be Tidy before reading in

df <- mtcars[,1:11]
df <-  df %>% dplyr::relocate(mpg, .after = last_col())

#### If needed, save the data frame as a data frame ####
df.final <- as.data.frame(df)

desired.feature <- colnames(df)[length(colnames(df))]


#### Initialize variables we will be measuring
adj.r.squared <-0
Mean.Squared.Error <- 0
adj.r.squared.df <- data.frame(adj.r.squared)
MSE.df <- data.frame(Mean.Squared.Error)
max.adj.r.squared <- 0
min.MSE <- 100000
min.mae <- 100000
mae.value <- 0
mae.df <- data.frame(mae.value)
min.k.mae <- 1000000
max.k.adj.r.squared <- 0
min.k.min.MSE <- 10000



#### Use all permutations of columns, and run the analysis ####
i = 1
last <- df[, ncol(df)] # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
for (i in 1:ncol(df)){ # this creates all possible permutations of the columns
  print (c(i, "of", c(ncol(df))))
  combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
  for (j in 1:nrow(combin)){
    cat("\014")
    print(sprintf("Column %.1f of %.1f", i, ncol(df)))
    print(sprintf("Row %.1f of %.1f", j, nrow(combin)))
    print(sprintf("Max Adjusted r-squared %f", max.adj.r.squared))
    print(sprintf("Min Mean Squared Error %f", min.MSE))
    print(sprintf("Min Mean Absolute Error %f", min.mae))
    for (k in 1:100) {

      colvals <- c(combin[j,])
      newdf <- data.frame(df[,colvals])
      newdf <- newdf^(k/10)
      newdf <- cbind(newdf, last)
      newdf <- as.data.frame(newdf)
      findLinearCombos(newdf)$remove
      newdf.lm = lm(last~.*., data = newdf)
      predict <- as.numeric(newdf.lm$fitted.values)
      mae.value <- mae(predicted = predict,actual = last)
      adj.r.squared <- summary(newdf.lm)$adj.r.squared
      Mean.Squared.Error <- mean(newdf.lm$residuals^2)
        
        if ((adj.r.squared >= max.adj.r.squared | adj.r.squared == 1) & adj.r.squared != "NaN"){
          saveRDS(newdf.lm, file = "/tmp/max.r.squared.rda")
          max.adj.r.squared = adj.r.squared
        }
        
        if((Mean.Squared.Error <= min.MSE) & Mean.Squared.Error != "NaN"){
          saveRDS(newdf.lm, file = "/tmp/min.mse.rda")
          min.MSE = Mean.Squared.Error
        }
        
        if(mae.value <= min.mae & min.mae != "NaN"){
          saveRDS(newdf.lm, file = "/tmp/min.mae.rda")
          min.mae = mae.value
        }
      }
    }
  }

      

#### Output the results for the user ####

r.squared.final <- adj.r.squared.df[2:nrow(adj.r.squared.df),]
# max(r.squared.final)
# min(r.squared.final)
# mean(r.squared.final)
# sort(r.squared.final,decreasing = TRUE)[1:10]

MSE <- MSE.df[2:nrow(MSE.df),]
max(MSE)
min(MSE)
mean(MSE)
sort(MSE, decreasing = FALSE)[1:10]

mae <- as.numeric(mae.df$mae.value)
mae <- mae[2:length(mae)]
max(mae)
min(mae)
mean(mae)
sort(mae, decreasing = FALSE)[1:10]
hist(mae,breaks = 100)

par(mfrow = c(1,3))
hist(r.squared.final,breaks = 100)
hist(MSE.df$Mean.Squared.Error, breaks = 100)
hist(mae, breaks = 100)

print("The model with the highest adjusted r-squared:")
highest.r.squared.model <- readRDS(file = "/tmp/max.r.squared.rda")
summary(highest.r.squared.model)

print("The model with the lowest MSE:")
lowest.mse.model <- readRDS(file ="/tmp/min.mse.rda")
summary(lowest.mse.model)
sprintf("This model predicts %s within %s", desired.feature, round(sqrt(mean(lowest.mse.model$residuals^2)),2))
print("To verify the lowest MSE")
mean(lowest.mse.model$residuals^2)

print("The model with the lowest MAE:")
lowest.mae.model <- readRDS(file = "/tmp/min.mae.rda")
summary(lowest.mae.model)

#Check that the mean of the residuals is close to zero:
mean(highest.r.squared.model$residuals)

# Check the model diagnostics
# gvlma::gvlma(highest.r.squared.model)
# plot(highest.r.squared.model)

