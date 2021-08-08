rm(list = ls())
start.time <- Sys.time()

#Error in if (abs(adj.r.squared.train - adj.r.squared.test) < 0.01) { : 
#missing value where TRUE/FALSE needed


#### Load packages we will use ####
library(tidyverse)
library(gtools)
library(Metrics)
library(ISLR)
library(MASS)
library(caret)

#### Read in the data frame, ALWAYS save with the name 'df' ####
# Note - all data frames MUST be Tidy before reading in
# 
# df <- Carseats
# df <-  df %>% dplyr::relocate(Price, .after = last_col())
# df$Urban <- ifelse(df$Urban=="Yes", 1,0)
# df$US <- ifelse(df$US=="Yes", 1, 0)
# df$ShelveLoc <- ifelse(df$ShelveLoc=="Bad", 1, ifelse(df$ShelveLoc == "Medium", 2,3))
# df <- df[,1:ncol(df)-1]

# df <- mtcars[,2:ncol(mtcars)]
# df <- df %>% dplyr::relocate(mpg, .after = last_col())


x1 <- rnorm(10000)
x2 <- rnorm(10000)
x3 <- rnorm(10000)
x4 <- rnorm(10000)
x5 <- rnorm(10000)
x6 <- rnorm(10000)
x7 <- rnorm(10000)
x8 <- rnorm(10000)
x9 <- rnorm(10000)
x10 <- rnorm(10000)
last <- rnorm(10000)
df <- data.frame(x1, x2, x3, x4, x5, x6, x7, x9, x9, x10)


#df <- Boston[,1:ncol(Boston)-1]

# df <- diamonds
# df <- as.data.frame(df)
# df <-  df %>% dplyr::relocate(price, .after = last_col())
# df <- df[,1:ncol(df)-1]
# df$cut <- as.numeric(df$cut)
# df$color <- as.numeric(df$color)
# df$clarity <- as.numeric(df$clarity)



# for (i in 1:ncol(df)){
#   df[,i] =  poly(df[,i], 4, raw = TRUE)
# }



#### If needed, save the data frame as a data frame ####
df.final <- as.data.frame(df)

desired.feature <- y


#### Initialize variables we will be measuring
adj.r.squared <-0
Mean.Squared.Error <- 0
Mean.Squared.Error1 <- 0
Mean.Squared.Error2 <- 0
Mean.Squared.Error3 <- 0
Mean.Squared.Error4 <- 0
Mean.Squared.ErrorFull <- 0
adj.r.squared.df <- data.frame(adj.r.squared)
MSE.df <- data.frame(Mean.Squared.Error)
max.adj.r.squared <- 0
min.MSE <- 100000000000
min.MSE1 <- 10000000000000


mse.train <- 10000000000000
mse.test <- 10000000000000
mse.validation <- 10000000000000

difference <- 1000
difference1 <- 1000000000000
difference2 <- 1000000000000
difference3 <- 1000000000000
difference4 <- 1000000000000
adj.r.squared.test <-  1
adj.r.squared.train <- 1
adj.r.squared.validation <- 1
max.adj.r.squared.test <- 0
number <- 0
m <- 0
adj.rsquared1 <- 0
adj.rsquared2 <- 0
adj.rsquared3 <- 0
adj.r.squared4 <- 0
adj.rsquared4 <- 0
adj.r.squared.full <- 0
adj.r.squared.df <- 0
colvals <- 0
best.r.squared <- 0



#### Use all permutations of columns, and run the analysis ####
i = 1
# This is our target, we will use this to measure the success of our analysis




for (i in 1:10){
  
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    print (c(i, "of", c(ncol(df))))
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE)
    for (k in 1:nrow(combin)){
      
      
      m = m+1
      cat("\014")
      print(sprintf("Iteration %0.1f of 10", i))
      print(sprintf("Column %.1f of %.1f", j, ncol(df)))
      print(sprintf("%s", colnames(df)[colvals]))
      print(sprintf("Row %.1f of %.1f", k, nrow(combin)))
      print(sprintf("Max Adjusted r-squared train %f", adj.rsquared1))
      print(sprintf("Max Adjusted r-squared test %f", adj.rsquared2))
      print(sprintf("Max Adjusted r-squared validation %f", adj.rsquared3))
      print(sprintf("Max Adjusted r-squared full data set %f", adj.rsquared4))
      print(sprintf("Min Mean Squared Error train %f", mse.test))
      print(sprintf("Min Mean Squared Error test %f", mse.train))
      print(sprintf("Min Mean Squared Error validation %f", mse.validation))
      print(sprintf("The total number of models = %0.0f", m))
      print(sprintf("Run time so far %f", Sys.time() - start.time))
      
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- as.data.frame(newdf)
      findLinearCombos(newdf)$remove
      
      idx <- sample(seq(1, 3), size = nrow(newdf), replace = TRUE, prob = c(nrow(newdf)/3, nrow(newdf)/3, nrow(newdf)/3))
      train.df <- as.data.frame(newdf[idx == 1,])
      test.df <- as.data.frame(newdf[idx == 2,])
      validation.df <- as.data.frame(newdf[idx == 3,])
      
      newdf.lm = lm(last[idx == 1]~., data = train.df)
      predict <- as.numeric(newdf.lm$fitted.values)
      adj.r.squared.train <- summary(newdf.lm)$adj.r.squared
      Mean.Squared.Error <- mean(newdf.lm$residuals^2)
      
      newdf.lm1 = lm(last[idx == 2]~., data = test.df)
      adj.r.squared.test <- summary(newdf.lm1)$adj.r.squared
      Mean.Squared.Error1 <- mean(newdf.lm1$residuals^2)
      
      newdf.lm2 = lm(last[idx==3]~., data = validation.df)
      predict <- as.numeric(newdf.lm2$fitted.values)
      adj.r.squared.validation <- summary(newdf.lm2)$adj.r.squared
      Mean.Squared.Error2 <- mean(newdf.lm2$residuals^2)
      
      newdf.lm3 <- lm(last~., data = newdf)
      adj.r.squared.full <- summary(newdf.lm3)$adj.r.squared
      Mean.Squared.ErrorFull <- mean(newdf.lm3$residuals^2)
      
      if (is.finite(adj.r.squared.test && adj.r.squared.train && adj.r.squared.validation)){
        if(adj.r.squared.train > (min(adj.rsquared1 , max.adj.r.squared)) &
           adj.r.squared.test > (min(adj.rsquared2 , max.adj.r.squared)) &
           adj.r.squared.validation > (min(adj.rsquared3 , max.adj.r.squared)) &&
           (abs(adj.r.squared.train - adj.r.squared.test) < difference1 &&
            abs(adj.r.squared.test - adj.r.squared.validation) < difference1 &&
            abs(adj.r.squared.train - adj.r.squared.validation) < difference1)){
          max.adj.r.squared <- min(adj.r.squared.train, adj.r.squared.test,  adj.r.squared.validation)
          adj.rsquared1 <- adj.r.squared.train
          adj.rsquared2 <- adj.r.squared.test
          adj.rsquared3 <- adj.r.squared.validation
          adj.rsquared4 <- adj.r.squared.full
          difference1 <- max(abs(adj.r.squared.train - adj.r.squared.test), abs(adj.r.squared.test - adj.r.squared.validation),
                             abs(adj.r.squared.train - adj.r.squared.validation))
          
          
          model1 <- saveRDS(newdf.lm, file = "/tmp/max.r.squared.rda")
          model2 <- saveRDS(newdf.lm1, file = "/tmp/max.r.squared2.rda")
          model3 <- saveRDS(newdf.lm2, file = "/tmp/max.r.squared3.rda")
        }
      } 
      
      if (Mean.Squared.Error < mse.test && Mean.Squared.Error1 < mse.train && Mean.Squared.Error2 < mse.validation){
        model4 <- saveRDS(newdf.lm, file = "/tmp/min.mse1.rda")
        model5 <- saveRDS(newdf.lm1, file = "/tmp/min.mse2.rda")
        model6 <- saveRDS(newdf.lm1, file = "/tmp/min.mse3.rda")
        min.MSE <-  min(Mean.Squared.Error, Mean.Squared.Error1, Mean.Squared.Error2)
        mse.test <- Mean.Squared.Error
        mse.train <- Mean.Squared.Error1
        mse.validation <- Mean.Squared.Error2        
      }
      # ||
      #  (abs(Mean.Squared.Error - Mean.Squared.Error1 < difference2) &&
      # (abs(Mean.Squared.Error - Mean.Squared.Error2 < difference2)) &&
      # (abs(Mean.Squared.Error1 - Mean.Squared.Error2 < difference2)))){
      
      # difference2 <- max((abs(Mean.Squared.Error - Mean.Squared.Error1) &&
      #                        abs(Mean.Squared.Error - Mean.Squared.Error2) &&
      #                        abs(Mean.Squared.Error1 - Mean.Squared.Error2)))
      
      # mse.train <- Mean.Squared.Error
      # mse.test <- Mean.Squared.Error1
      # mse.validation <- Mean.Squared.Error2
      # 
    }
  }
}
#}



#### Output the results for the user ####




# last.df <- data.frame(last)
# ggplot(data = last.df,mapping = aes(y = last)) +
#   geom_boxplot()
# 
# sqrt(mean(lowest.mse.model$residuals^2))
# sqrt(mean(lowest.mse.model_01$residuals^2))

end.time <- Sys.time()
duration <- end.time - start.time
options(scipen=1)
best.model <- c("Train", "Test", "Validation", "Full Data Set", "Duration", "Number of Models")
max.adjusted.r.squared <- c(adj.rsquared1,adj.rsquared2,adj.rsquared3, adj.rsquared4, duration, m)
best.min.mse <- c(mse.train, mse.test, mse.validation,0,0,0)
table.final <- data.frame(best.model, max.adjusted.r.squared, best.min.mse)
#cat("\014")

highest.r.squared.model <- readRDS(file = "/tmp/max.r.squared.rda")
summary(highest.r.squared.model)

lowest.mse.model <- readRDS(file ="/tmp/min.mse1.rda")
summary(lowest.mse.model)

table.final

