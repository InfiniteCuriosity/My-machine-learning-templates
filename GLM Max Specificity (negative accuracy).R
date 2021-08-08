rm(list = ls()) # clears out the global environment
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)


#### Part 1: Import the data ####
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- df[,2:ncol(df)]

# df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)

df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- select(df, sbp, tobacco, ldl, adiposity, famhist, typea, obesity, alcohol, age, chd) # deletes the first column




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

#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
table1.df <- data.frame("No" = c(0, 0), "Yes" = c(0, 0))
rownames(table1.df) = c("No", "Yes")
colnames(table1.df) = c("X0", "X1")
accuracy.table <- as.data.frame(table1.df) #### fix this so it generalizes!!
accuracy.df <- data.frame(accuracy)
optimal.accuracy.rate <- 0.5
optimal.accuracy.df <- data.frame()


#### Set up to measure and report the sensitivity (positive predictions) of the analysis ####
sensitivity <- 0
sensitivitytmp <- 0
sensitivity.table <- data.frame()
sensitivity.df <- data.frame(sensitivity)
optimal.sensitivity.rate <- 0.5
optimal.sensitivity.df <- data.frame()


sumtable1 <- 0
dummy1.df <- data.frame(table1.df)
j <- 0
pos.percentage.df <- data.frame(j)
maxyes <- 0

#### set up to measure and report the specificity (negative predictions) of the analysis ####
specificity <- 0
specificitytmp <- 0
specificity.table <- data.frame()
specificity.df <- data.frame(specificity)
optimal.specificity.rate <- 0.5
optimal.specificity.df <- data.frame()

sumtable1 <- 0
dummy1.df <- data.frame(table1.df)
j <- 0
neg.percentage.df <- data.frame(j)



#### Set up to report misclassification rate
misclassification.error <- 0
misclassificationtmp <- 0
misclass <- data.frame(misclassification.error)

total <- data.frame(accuracy, sensitivity, specificity, misclass)

#### Set up temp tables to aid in the calculation of accuracy, sensitivity, and specificity
table1.df <- data.frame()
tables.df <- data.frame()

temptable <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable) = c("No", "Yes")
temptable <- as.table(temptable)

temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable1) = c("No", "Yes")
temptable1 <- as.table(temptable1)

temptable <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable) = c("No", "Yes")
temptable <- as.table(temptable)
specificity.table.df <- temptable

temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable1) = c("No", "Yes")
temptable1 <- as.table(temptable1)

for (i in 1:10000){ # i measures the number of times we will update optimal.specificity.rate
  print("Specificity")
  print(i)
  print(tail(specificity.table.df, n = 2))
  print(tail(specificity.df,n = 1))
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
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
      glm.pred[glm.probs > optimal.specificity.rate] = "Yes"
      table1 <- table(glm.pred, test.df$last)
      table1.df <- data.frame(unclass(table1))
      prediction <- prediction(predictions = glm.probs,labels = last[-train])
      cost_perf = performance(prediction, "cost")
      optimal.specificity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
      
      glm.pred[glm.probs > optimal.specificity.rate] = "Yes"
      table1 <- table(glm.pred, test.df$last)
      table1.df <- data.frame(unclass(table1))
      table1
      
      #### Measure the Specificity (negative accuracy) of the GLM
      if (nrow(table1) == ncol(table1)){
        specificity <- table1[1] / sum(table1[c(1,3)])
        if(specificity>specificitytmp | table1[3]==0){
          if(specificity == 1 & table1[1]>temptable1[1]){
            specificity.table.df <- as.data.frame.matrix(table1)
            specificity.table <- rbind(specificity.table, specificity.table.df)
            specificity.df <- rbind(specificity.df, specificity)
            specificitytmp = specificity
            temptable1 = table1
            saveRDS(glm.fits, file = "/tmp/glm.max.specificity.rda")
            prediction <- prediction(predictions = glm.probs,labels = last[-train])
            cost_perf = performance(prediction, "cost")
            optimal.specificity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
            optimal.specificity.df <- rbind(optimal.specificity.df, optimal.specificity.rate)
          }
        }
      }
    }
  }
}




# ############ ----------- display results to the user ---------------- ####################



print("The highest specificity (negative accuracy")
print(max(specificity.df))
max.specificity <- readRDS(file = "/tmp/glm.max.specificity.rda")
summary(max.specificity)
specificity.table[(nrow(specificity.table)-1):nrow(specificity.table),]
print(optimal.specificity.rate)
tail(specificity.df)

