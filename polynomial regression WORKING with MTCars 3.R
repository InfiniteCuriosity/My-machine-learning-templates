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

# df <- Carseats
# df <-  df %>% dplyr::relocate(Price, .after = last_col())
# df$Urban <- ifelse(df$Urban=="Yes", 1,0)
# df$US <- ifelse(df$US=="Yes", 1, 0)
# df$ShelveLoc <- ifelse(df$ShelveLoc=="Bad", 1, ifelse(df$ShelveLoc == "Medium", 2,3))



# cyl <- poly(mtcars[,c(2)],4,raw = TRUE)
# disp <- poly(mtcars[,c(3)], 4, raw = TRUE)
# hp <- poly(mtcars[,c(4)],4,raw = TRUE)
# drat <- poly(mtcars[,5],4, raw = TRUE)
# wt <- poly(mtcars[,6],4, raw = TRUE)
# qsec <- poly(mtcars[,7], 4, raw = TRUE)
# vs <- poly(mtcars[,8], 4, raw = TRUE)
# am <-  poly(mtcars[,9], 4, raw = TRUE)
# gear <- poly(mtcars[,10], 4, raw = TRUE)
# carb <- poly(mtcars[,11], 4, raw = TRUE)
# df <- data.frame(cyl, disp, hp,drat, wt, qsec, vs, am, gear, carb)

df <- diamonds
df <- as.data.frame(df)
df <-  df %>% dplyr::relocate(price, .after = last_col())
df$cut <- as.numeric(df$cut)
df$color <- as.numeric(df$color)
df$clarity <- as.numeric(df$clarity)
carat <- poly(df[,c(1)],4, raw = TRUE)
cut <- poly(df[,c(2)],4, raw = TRUE)
color <- poly(df[,c(3)],4, raw = TRUE)
clarity <- poly(df[,c(4)],4, raw = TRUE)
depth <- poly(df[,c(5)], 4, raw = TRUE)
table <- poly(df[, c(6)],4, raw = TRUE)
x1 <- poly(df[,c(7)],4, raw = TRUE)
y1 <- poly(df[,c(8)],4, raw = TRUE)
z1 <- poly(df[,c(9)],4,raw = TRUE)
df <- data.frame(carat, cut, color, clarity, depth, table, x1, y1, z1)
# 
# 
# 
colnames(df) <- c("carat1", "carat2", "carat3", "carat4",
                  "cut1", "cut2", "cut3", "cut4",
                  "color1", "color2", "color3", "color4",
                  "clarity1", "clarity2", "clarity3", "clarity4",
                  "depth1", "depth2", "depth3", "depth4",
                  "table1", "table2", "table3", "table4",
                  "x11", "x12", "x13", "x14",
                  "y11", "y12", "y13", "y14",
                  "z11", "z12", "z13", "z14")
# str(df)


#### If needed, save the data frame as a data frame ####
df.final <- as.data.frame(df)

desired.feature <- diamonds$price


#### Initialize variables we will be measuring
adj.r.squared <-0
Mean.Squared.Error <- 0
Mean.Squared.Error1 <- 0
adj.r.squared.df <- data.frame(adj.r.squared)
MSE.df <- data.frame(Mean.Squared.Error)
max.adj.r.squared <- 0
min.MSE <- 10000000000000
min.MSE1 <- 10000000000000
min.mae <- 100000
mae.value <- 0
mae.df <- data.frame(mae.value)
min.k.mae <- 1000000
max.k.adj.r.squared <- 0
min.k.min.MSE <- 10000
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
colvals <- 0




#### Use all permutations of columns, and run the analysis ####
i = 1
last <- diamonds$price # This is our target, we will use this to measure the success of our analysis
df <- df[,1:(ncol(df))-1] #Remove the last column (we just saved it)
for (i in 1:10){
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    print (c(i, "of", c(ncol(df))))
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE)
    for (k in 1:nrow(combin)){

      
 #     for (l in 1:100) {
        
        m = m+1
        cat("\014")
        print(sprintf("Iteration %0.1f of 10", i))
        print(sprintf("Column %.1f of %.1f", j, ncol(df)))
        print(sprintf("Columns %0.0f", colvals))
        print(sprintf("Row %.1f of %.1f", k, nrow(combin)))
        print(sprintf("Max Adjusted r-squared %f", adj.rsquared1))
        print(sprintf("Max Adjusted r-squared %f", adj.rsquared2))
        print(sprintf("Max Adjusted r-squared %f", adj.rsquared3))
        #      print(sprintf("Difference = %f", difference))
        print(sprintf("Min Mean Squared Error %f", (min.MSE)))
        print(sprintf("Difference1 = %f", difference1))
        print(sprintf("The total number of models = %0.0f", m))
        print(sprintf("Run time so far %f", Sys.time() - start.time))
        # print(sprintf("Min Mean Absolute Error %f", min.mae))
        
        colvals <- c(combin[k,])
        newdf <- data.frame(df[,colvals])
 #       newdf <- newdf^(l/10)
        newdf <- cbind(newdf, last)
        newdf <- as.data.frame(newdf)
        findLinearCombos(newdf)$remove
        
        
        idx <- sample(seq(1, 3), size = nrow(df), replace = TRUE, prob = c(nrow(df)/3, nrow(df)/3, nrow(df)/3))
        train.df <- df[idx == 1,]
        test.df <- df[idx == 2,]
        validation.df <- df[idx == 3,]
        
        
        # Break the data set into random amounts of test and train
        # ratio <- round(runif(1, 0.25, 0.75),2)
        # dfsize <- as.integer((nrow(newdf))*ratio)
        # train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
        # train.df <- newdf[train,]
        # train.df <- as.data.frame(train.df)
        # test.df <- newdf[-train,]
        # test.df <- as.data.frame(test.df)
        
        newdf.lm = lm(last[idx == 1]~.*., data = train.df)
        predict <- as.numeric(newdf.lm$fitted.values)
#        mae.value <- mae(predicted = predict,actual = last[train])
        adj.r.squared.train <- summary(newdf.lm)$adj.r.squared
        Mean.Squared.Error <- mean(newdf.lm$residuals^2)
        
        newdf.lm1 = lm(last[idx == 2]~.*., data = test.df)
        predict <- as.numeric(newdf.lm1$fitted.values)
#        mae.value <- mae(predicted = predict,actual = last[-train])
        adj.r.squared.test <- summary(newdf.lm1)$adj.r.squared
        Mean.Squared.Error1 <- mean(newdf.lm1$residuals^2)
        
        newdf.lm2 = lm(last[idx==3]~.*., data = validation.df)
        predict <- as.numeric(newdf.lm2$fitted.values)
 #       mae.value <- mae(predicted = predict,actual = last[-train])
        adj.r.squared.validation <- summary(newdf.lm2)$adj.r.squared
        Mean.Squared.Error2 <- mean(newdf.lm2$residuals^2)
        
        
        if (is.finite(adj.r.squared.test && adj.r.squared.train && adj.r.squared.validation)){
          if ((abs(adj.r.squared.train - adj.r.squared.test) <= difference) &&
              (abs(adj.r.squared.train - adj.r.squared.validation) <= difference1) &&
              (abs(adj.r.squared.test - adj.r.squared.validation) <= difference2)){
            if(adj.r.squared.test >= max.adj.r.squared ||
               adj.r.squared.train >= max.adj.r.squared ||
               adj.r.squared.validation >= max.adj.r.squared){
              model1 <- saveRDS(newdf.lm, file = "/tmp/max.r.squared.rda")
              model2 <- saveRDS(newdf.lm1, file = "/tmp/max.r.squared2.rda")
              model3 <- saveRDS(newdf.lm2, file = "/tmp/max.r.squared3.rda")
#              max.adj.r.squared = min(adj.r.squared.test, adj.r.squared.train, adj.r.squared.validation)
              max.adj.r.squared <- max(adj.r.squared.test, adj.r.squared.train, adj.r.squared.validation)
              adj.rsquared1 <- adj.r.squared.test
              adj.rsquared2 <- adj.r.squared.train
              adj.rsquared3 <- adj.r.squared.validation
              difference = abs(adj.r.squared.train - adj.r.squared.test)
              difference1 <- abs(adj.r.squared.train - adj.r.squared.validation)
              difference2 <- abs(adj.r.squared.test - adj.r.squared.validation)
            }
          } 
        }
 
        if ((Mean.Squared.Error < min.MSE && Mean.Squared.Error1 < min.MSE1)){
          if ((abs(Mean.Squared.Error - Mean.Squared.Error1)) < difference4){
            model3 <- saveRDS(newdf.lm, file = "/tmp/min.mse1.rda")
            model4 <- saveRDS(newdf.lm1, file = "/tmp/min.mse2.rda")
            difference4 <- abs(Mean.Squared.Error - Mean.Squared.Error1)
            min.MSE <- Mean.Squared.Error
            min.MSE1 <- Mean.Squared.Error1
          }
        }
     # }
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

highest3 <- readRDS(file = "/tmp/max.r.squared3.rda")
summary(highest3)


print("The model with the lowest MSE for test and train:")
lowest.mse.model <- readRDS(file ="/tmp/min.mse1.rda")
summary(lowest.mse.model)
lowest.mse.model_01 <- readRDS(file ="/tmp/min.mse2.rda")
summary(lowest.mse.model_01)
#sprintf("This model predicts %s within %s", desired.feature, round(sqrt(mean(lowest.mse.model$residuals^2)),2))
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

end.time <- Sys.time()
duration <- end.time - start.time
duration


