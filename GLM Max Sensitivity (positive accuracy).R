# GLM Sensitivity (?)

rm(list = ls()) # clears out the global environment
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)

# df <- Caravan
# df$Purchase <- ifelse(df$Purchase == "Yes", 1, 0)


#### Part 1: Import the data ####
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- select(df, sbp, tobacco, ldl, adiposity, famhist, typea, obesity, alcohol, age, chd) # deletes the first column
               
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)

# df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
# df <-  df %>% dplyr::relocate(Survived, .after = last_col())
# df <- dplyr::select(df,PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Survived)
# df$Sex <- as.numeric(df$Sex)
# df$Embarked <- as.numeric(df$Embarked)

#df <- read.csv('/Users/russellconte/Downloads/titanic-passengers.csv', header = TRUE, stringsAsFactors = TRUE, sep = ';')
#df <- select(df,PassengerId, Pclass, Sex, Age, SibSp, Parch, Ticket, Fare, Embarked, Survived)


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set

#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.factor(last)
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor


#### Set up to measure and report the sensitivity (positive predictions) of the analysis ####
sensitivity <- 0
sensitivitytmp <- 0
sensitivity.table <- data.frame()
sensitivity.df <- data.frame(sensitivity)
optimal.sensitivity.rate <- 0.5
optimal.sensitivity.df <- data.frame()

#### Set up temp tables to aid in the calculation of accuracy, sensitivity, and specificity
table1.df <- data.frame()
tables.df <- data.frame()

temptable <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable) = c("No", "Yes")
temptable <- as.table(temptable)

temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
sensitivity.table.df <- temptable1
rownames(temptable1) = c("No", "Yes")
temptable1 <- as.table(temptable1)

#### ---- Actual analysis starts here ---- ####

for (i in 1:10000){ # i measures the number of times we will update optimal.sensitivity.rate.
 print(sprintf("Run %1.0f of 10,000", i))
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    print(sprintf("Run %1.0f of %1.0f", j, ncol(df)))
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      cat("\014")
      print(sprintf("Run %1.0f of 10,000", i))
      print(sprintf("Run %1.0f of %1.0f columns", j, ncol(df)))
      print(sprintf("Run %1.0f of %1.0f rows", k, nrow(combin)))
      print(sprintf("TempSensitivity %f", sensitivitytmp))
      print(sensitivity.table.df)
      print("temptable")
      print(temptable)
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      newdf <- cbind(newdf, last)
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.1, 0.9),2)
      dfsize <- as.integer((nrow(newdf))*ratio)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.data.frame(train.df)
      test.df <- newdf[-train,]
      test.df <- as.data.frame(test.df)
      test.df$last <- as.factor(test.df$last)
      
      #### Actual analysis is here ####
      glm.fits <- glm(last~., data = train.df, family = binomial)
      glm.probs <- predict(glm.fits, test.df, type = "response")
      glm.pred <- rep("No", nrow(test.df))
      glm.pred[glm.probs > optimal.sensitivity.rate] = "Yes"
      table1 <- table(glm.pred, test.df$last)
      table1.df <- data.frame(unclass(table1))
      prediction <- prediction(predictions = glm.probs,labels = last[-train])
      cost_perf = performance(prediction, "cost")
      optimal.sensitivity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
      
      glm.pred[glm.probs > optimal.sensitivity.rate] = "Yes"
      table1 <- table(glm.pred, test.df$last)
      table1.df <- data.frame(unclass(table1))
      prediction <- prediction(predictions = glm.probs,labels = last[-train])
      cost_perf = performance(prediction, "cost")
      optimal.sensitivity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
      

      #### Measure the Sensitivity (positive accuracy) of the GLM
      if (nrow(table1) == ncol(table1)){
        sensitivity <- table1[4] / sum(table1[2]+table1[4])
        if(sensitivity>sensitivitytmp | sensitivity == 1){
          if(table1[2] == 0 & table1[4:4]>temptable[4:4]){
            sensitivity.table.df <- as.data.frame.matrix(table1)
            sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
            sensitivity.df <- rbind(sensitivity.df, sensitivity)
            sensitivitytmp = sensitivity
            temptable = table1
            print(sprintf("sensitivity %f", sensitivity))
            print(sprintf("sensitivitytmp %f", sensitivitytmp))
            saveRDS(glm.fits, file = "/tmp/glm.max.sensitivity.rda")
            saveRDS(table1, file = "/tmp/table1.rda")
            prediction <- prediction(predictions = glm.probs,labels = last[-train])
            cost_perf = performance(prediction, "cost")
            optimal.sensitivity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
          }
        }
      }
    }
  }
}


# ############ ----------- display results to the user ---------------- ####################



print("The highest sensitivity (positive accuracy)")
max(sensitivity.df)
sensitivity.table.df
max.sensitivity <- readRDS(file = "/tmp/glm.max.sensitivity.rda")
summary(max.sensitivity)
print(optimal.sensitivity.rate)
tail(sensitivity.df)
sensitivity.table
besttable <- readRDS(file = "/tmp/table1.rda")
besttable

