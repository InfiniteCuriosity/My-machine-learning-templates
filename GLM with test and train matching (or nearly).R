rm(list = ls()) # clears out the global environment
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)
library(parallel)


#### Part 1: Import the data ####
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- df[,2:ncol(df)]


#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)

# df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
# df <-  df %>% dplyr::relocate(Survived, .after = last_col())
# df <- dplyr::select(df,PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Survived)
# df$Sex <- as.numeric(df$Sex)
# df$Embarked <- as.numeric(df$Embarked)
# df <- df[complete.cases(df),]


#df <- read.csv('/Users/russellconte/Downloads/titanic-passengers.csv', header = TRUE, stringsAsFactors = TRUE, sep = ';')
#df <- select(df,PassengerId, Pclass, Sex, Age, SibSp, Parch, Ticket, Fare, Embarked, Survived)


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set

#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.numeric(last) ######
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor


#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
accuracytmp1 <- 0
table1.df <- data.frame("No" = c(0, 0), "Yes" = c(0, 0))
rownames(table1.df) = c("No", "Yes")
colnames(table1.df) = c("X0", "X1")
accuracy.table <- as.data.frame(table1.df) #### fix this so it generalizes!!
accuracy.df <- data.frame(accuracytmp)
optimal.accuracy.rate <- 0.5
optimal.accuracy.df <- data.frame()
accuracy.train <- 0
accuracy.test <- 0
best.train.accuracy <- 0
best.train.accuracy.df <- data.frame(accuracy.train)
best.test.accuracy <- 0
best.test.accuracy.df <- data.frame(accuracy.test)
diff <- 10000000
diff.df <- data.frame(diff)
accuracy1.df <- data.frame(0)
accuracy2.df <- data.frame(0)

optimal.accuracy1.rate <- 0.5
optimal.accuracy1.df <- data.frame(optimal.accuracy1.rate)

optimal.accuracy2.rate <- 0.5
optimal.accuracy2.df <- data.frame(optimal.accuracy2.rate)

tables.df <- data.frame()


#### Analysis begins here ####
for (i in 1:1000){ # i measures the number of times we will update optimal.accuracy.rate.
  print(sprintf("Run %.01f of 1,000", i))
  print(tail(accuracy1.df,n = 1))
  print(tail(accuracy2.df, n = 1))
  print(sprintf("diff = %f", diff))
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.25, 0.75),2)
      dfsize <- as.integer((nrow(newdf))*ratio)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.data.frame(train.df)
      test.df <- newdf[-train,]
      test.df <- as.data.frame(test.df)
      test.df$last <- as.factor(test.df$last)
      
      #### Actual analysis is here ####
      
      glm.fits1 <- glm(last~., data = train.df, family = binomial)
      glm.probs <- predict(glm.fits1, test.df, type = "response")
      glm.pred <- rep("No", nrow(test.df))
      glm.pred[glm.probs > optimal.accuracy1.rate] = "Yes"
      table1 <- table(glm.pred, test.df$last)
      accuracy.train <- sum(diag(table1))/sum(table1)
      prediction <- prediction(predictions = glm.probs,labels = last[-train])
      cost_perf = performance(prediction, "cost")
      optimal.accuracy1.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]

      
      glm.probs2 <- predict(glm.fits1, train.df, type = "response")
      glm.pred2 <- rep("No", nrow(train.df))
      glm.pred2[glm.probs2 > optimal.accuracy2.rate] = "Yes"
      table2 <- table(glm.pred2, train.df$last)
      table2.df <- data.frame(unclass(table2))
      accuracy.test <- sum(diag(table2))/sum(table2)
      prediction <- prediction(predictions = glm.probs2,labels = last[train])
      cost_perf = performance(prediction, "cost")
      optimal.accuracy2.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]

      
      if(accuracy.test>max(accuracy1.df) | accuracy.train> max(accuracy2.df)){
        if(nrow(table1) == ncol(table1)){
          if(ncol(table1) == 2){
            if(abs(accuracy.train-accuracy.test) < 0.02){
              diff = abs(accuracy.train - accuracy.test)
              diff.df <- rbind(diff.df, diff)
              if(diff == 0) break
              best.train <- saveRDS(object = table1,file = "/tmp/best.train.rda")
              best.train.accuracy.df <- rbind(best.train.accuracy.df, accuracy.train)
              best.test <- saveRDS(object = table2.df, file = "/tmp/best.test.rda")
              best.test.accuracy.df <- rbind(best.test.accuracy.df, accuracy.test)
              best.model <- saveRDS(object = glm.fits1, file = "/tmp/best.glm.model")
              accuracy.df <- rbind(accuracy.df, accuracytmp)
              accuracy1.df <- rbind(accuracy1.df, accuracy.test)
              accuracy2.df <- rbind(accuracy2.df, accuracy.train)
              optimal.accuracy1.df <- rbind(optimal.accuracy1.df, optimal.accuracy1.rate)              
              optimal.accuracy2.df <- rbind(optimal.accuracy2.df, optimal.accuracy2.rate)
              print(table1)
              print(tail(accuracy.test,n = 1))
            } 
          }
        }
      }
    }
  }
}

best.train <- readRDS(file = "/tmp/best.train.rda")
best.train
tail(best.train.accuracy.df,n = 1)
best.test <- readRDS(file = "/tmp/best.test.rda")
best.test
tail(best.test.accuracy.df, n = 1)
best.model <- readRDS(file = "/tmp/best.glm.model")
summary(best.model)

