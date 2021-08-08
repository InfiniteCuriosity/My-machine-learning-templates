rm(list = ls()) # clears out the global environment
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)
library(MASS)

#### Part 1: Import the data ####
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- df[,2:ncol(df)]

#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)
df <- iris
#df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)

#df <- read.csv('/Users/russellconte/Downloads/titanic-passengers.csv', header = TRUE, stringsAsFactors = TRUE, sep = ';')
#df <- select(df,PassengerId, Pclass, Sex, Age, SibSp, Parch, Ticket, Fare, Embarked, Survived)


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set

#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.factor(last)

#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
accuracy.table <- data.frame()
accuracy.df <- data.frame()
results <- data.frame(accuracy)
x =length(levels(df$last))
data1 <- seq(1:x^2)
newmatrix <- matrix()
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(df$last))
colnames(newmatrix) <- c(levels(df$last))
table1.df <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1.df))
results.table <- data.frame(table1.df)
sumdiagtmp <- 0


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



#### Analysis begins here ####
for (i in 1:10){ # i measures the number of times we will update optimal.accuracy.rate.
  print(i)
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
      lda.fit <- lda(last~.,data = train.df)
      lda.pred <- predict(object = lda.fit,newdata = test.df)
      lda.class <- lda.pred$class
      table1 <- table(lda.class, last[-train])
      table1.df <- data.frame(unclass(table1))
      #### Save table if it is valid
      if(nrow(table1) == ncol(table1)){
        if(ncol(table1) == 2){
          tables.df <- rbind(tables.df, table1.df)
        }
      }
      
      #### - Measure overall accuracy of the LDA ####
      if(nrow(table1) == ncol(table1)){
        accuracy = sum(diag(table1)) / sum(table1)
        if (accuracy>accuracytmp){
          accuracytmp = accuracy
          accuracy.table = rbind(accuracy.table, table1.df)
          accuracy.df <- rbind(accuracy.df, accuracy)
          saveRDS(lda.fit, file = "/tmp/lda.max.accuracy.rda")
        }
      }
      #### Measure the Sensitivity (positive accuracy) of the GLM
      if (nrow(table1) == ncol(table1)){
        sensitivity <- table1[4:4] / sum(table1[3:4])
        if (sensitivity != 'NaN'){
          if(sensitivity>sensitivitytmp | sensitivity == 1){
            if (sum(diag(table1)) > sum(diag(temptable))){
              sensitivity.table.df <- as.data.frame.matrix(table1)
              sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
              sensitivity.df <- rbind(sensitivity.df, sensitivity)
              sensitivitytmp = sensitivity
              temptable = table1
              saveRDS(lda.fit, file = "/tmp/lda.max.sensitivity.rda") 
            }
            }
          }
        }

  

       #### Measure the Specificity (negative accuracy) of the GLM
         if (nrow(table1) == ncol(table1)){
           specificity <- table1[1:1] / sum(table1[1:2])
           if (specificity != 'NaN'){
             if(specificity>specificitytmp | specificity == 1){
               if(table1[1:1]> temptable1[1:1]){
                 specificity.table.df <- as.data.frame.matrix(table1)
                 specificity.table <- rbind(specificity.table, specificity.table.df)
                 specificity.df <- rbind(specificity.df, specificity)
                 specificitytmp = specificity
                 temptable1 = table1
                 saveRDS(lda.fit, file = "/tmp/lda.max.specificity.rda")
            }
          }
        }
      }   
    }
  }
}

# ############ ----------- display results to the user ---------------- ####################


print("The highest overall accuracy: (accurate rate and value of i)")
max(accuracy.df)
table1.df
max.accuracy <- readRDS(file = "/tmp/lda.max.accuracy.rda")
summary(max.accuracy)


print("The highest sensitivity (positive accuracy)")
max(sensitivity.df)
tail(sensitivity.table, n = nlevels(df$last))
max.sensitivity <- readRDS(file = "/tmp/lda.max.sensitivity.rda")
summary(max.sensitivity)



print("The highest specificity (negative accuracy")
print(max(specificity.df))
max.specificity <- readRDS(file = "/tmp/lda.max.specificity.rda")
summary(max.specificity)
specificity.table[(nrow(specificity.table)-2):nrow(specificity.table),]
tail(specificity.df)


