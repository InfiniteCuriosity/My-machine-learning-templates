#### Need to update the contruction of Table1 so it is automatically the dimensions of the factor in the last column####

rm(list = ls()) # clears out the global environment
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)
library(MASS)
library(class)

#### Part 1: Import the data ####
df <- Smarket
df <- as.data.frame(df)


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set
dfLabels = df[,ncol(df)]



#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.factor(last)
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor


#### Part 1C convert all character columns to factors to numeric ####
for (i in 1:ncol(df)){
  if(class(df[,i]) == 'character'){
    df[,i] <- as.factor(df[,i])
    df[,i] <- as.numeric(df[,i])
  }
}




#### Create a function to convert factors to numerical values, and then normalize the data frame
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
df <- as.data.frame(lapply(X = df, FUN = normalize))


#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
accuracy.df <- data.frame(accuracy)
x =nlevels(last)
data1 <- seq(1:x^2)
newmatrix <- matrix(0, nrow = x, ncol = x)
rownames(newmatrix) <- c(levels(last))
colnames(newmatrix) <- c(levels(last))
table1.df <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1.df))
tables.df <- data.frame(table1.df)

temptable <- table1.df
results.table <- data.frame(table1.df)
accuracy.table <- data.frame(table1.df)


#### Set up to measure and report the sensitivity (positive predictions) of the analysis ####
sensitivity <- 0
sensitivitytmp <- 0
sensitivity.table <- data.frame()
sensitivity.df <- data.frame(sensitivity)
optimal.sensitivity.rate <- 0.5
optimal.sensitivity.df <- data.frame()


#### set up to measure and report the specificity (negative predictions) of the analysis ####
specificity <- 0
specificitytmp <- 0
specificity.table <- data.frame()
specificity.df <- data.frame(specificity)
optimal.specificity.rate <- 0.5
optimal.specificity.df <- data.frame()


#### Set up to report misclassification rate
misclassification.error <- 0
misclassificationtmp <- 0
misclass <- data.frame(misclassification.error)

#### convert data to a matrix
df <- as.matrix(df)


#### Analysis begins here ####
for (i in 1:1){ # i measures the number of times we will update optimal.accuracy.rate.
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = 3, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      print(i)
      print(j)
      print(k)
      colvals <- c(combin[3,])
      newdf <- data.frame(df[,colvals])
#      newdf <- cbind(newdf, last)
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.1, 0.9),2)
      dfsize <- as.integer((nrow(newdf))*.5)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.matrix(train.df)
      test.df <- newdf[-train,]
      test.df <- as.matrix(test.df)
#      test.df$last <- as.factor(test.df$last)
      
      #### Actual knn analysis is here ####
      for (l in 1:10){
        knn.pred <- knn(train.df, test.df, cl = dfLabels[train], k = l)
        table1.df <- table(knn.pred, last[-train])
        }

      #### Save table if it is valid
      if(nrow(table1.df) == ncol(table1.df)){
        table2.df <- as.data.frame.matrix(table1.df)
          tables.df <- rbind(tables.df, table2.df)
        }
      
      
      #### - Measure overall accuracy of the knn model ####
      if(nrow(table1.df) == ncol(table1.df)){
        accuracy = sum(diag(table1.df)) / sum(table1.df)
        if (accuracy>accuracytmp){
          accuracytmp = accuracy
          table3.df <- as.data.frame.matrix(table1.df)
          accuracy.table = rbind(accuracy.table, table3.df)
          accuracy.df <- rbind(accuracy.df, accuracy)
          saveRDS(knn.pred, file = "/tmp/knn.max.accuracy.rda")
        }
      }
      #### Measure the Sensitivity (positive accuracy) of the knn model
      if (nrow(table1.df) == ncol(table1.df)){
        sensitivity <- table1.df[4:4] / sum(table1.df[3:4])
        if(sensitivity>sensitivitytmp | sensitivity == 1){
          if(table1.df[2:2]> temptable[2:2]){
            sensitivity.table.df <- as.data.frame.matrix(table1.df)
            sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
            sensitivity.df <- rbind(sensitivity.df, sensitivity)
            sensitivitytmp = sensitivity
            temptable = table1.df
            saveRDS(knn.pred, file = "/tmp/knn.max.sensitivity.rda")
          }
        }
      }
      
      
      #### Measure the Specificity (negative accuracy) of the knn model
      if (nrow(table1.df) == ncol(table1.df)){
        specificity <- table1.df[1:1] / sum(table1.df[1:2])
        if(specificity>specificitytmp | specificity == 1){
          if(table1.df[1:1]> temptable1[1:1]){
            specificity.table.df <- as.data.frame.matrix(table1.df)
            specificity.table <- rbind(specificity.table, specificity.table.df)
            specificity.df <- rbind(specificity.df, specificity)
            specificitytmp = specificity
            temptable1 = table1.df
            saveRDS(knn.pred, file = "/tmp/knn.max.specificity.rda")
          }
        }
      }   
    }
  }
}

# ############ ----------- display results to the user ---------------- ####################


print("The highest overall accuracy:")
max(accuracy.df)
max.accuracy <- readRDS(file = "/tmp/knn.max.accuracy.rda")


print("The highest sensitivity (positive accuracy)")
max(sensitivity.df)
sensitivity.table
max.sensitivity <- readRDS(file = "/tmp/knn.max.sensitivity.rda")
summary(max.sensitivity)
tail(sensitivity.df)


print("The highest specificity (negative accuracy")
print(max(specificity.df))
max.specificity <- readRDS(file = "/tmp/knn.max.specificity.rda")
summary(max.specificity)
specificity.table[(nrow(specificity.table)-1):nrow(specificity.table),]
tail(specificity.df)

temptable[3:1]
dim(temptable)
