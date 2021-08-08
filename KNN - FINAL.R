#### Need to update the contruction of Table1 so it is automatically the dimensions of the factor in the last column####

rm(list = ls()) # clears out the global environment
#library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)
library(MASS)
library(class)
library(tidyverse)

#### Part 1: Import the data ####
# df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
# df <- df[,2:ncol(df)] # remove the first column if it's jsut row numbers
df <- Carseats
df <-  df %>% dplyr::relocate(ShelveLoc, .after = last_col())
df

#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set
dfLabels = df[,ncol(df)]



#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.factor(last)
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor
str(df)


#### Part 2: Convert factors to numeric ####
for(i in 1:ncol(df)){
  if(class(df[,i]) == "factor"){
    df[,i] <- as.numeric(df[,i])
  }
}


#### Create a function to convert factors to numerical values, and then normalize the data frame
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
df <-normalize(df)


#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
accuracy.df <- data.frame(accuracy)
accuracy2 <- 0
accuracy2.df <- data.frame(accuracy2)
accuracy.best.k <- 0
accuracy.best.k.df <- data.frame(accuracy.best.k)

#### Set up to measure and report the sensitivity (positive predictions) of the analysis ####
sensitivity <- 0
sensitivitytmp <- 0
sensitivity.df <- data.frame(sensitivity)
sensitivity.best.k <- 0
sensitivity.best.k.df <- data.frame(sensitivity.best.k)


#### set up to measure and report the specificity (negative predictions) of the analysis ####
specificity <- 0
specificitytmp <- 0
specificity.df <- data.frame(specificity)
specificity.best.k <- 0
specificity.best.k.df <- data.frame(specificity.best.k)


#### Set up to report misclassification rate
misclass <- 0
misclasstmp <- 0
misclass.df <- data.frame(misclass)
misclass.table <- data.frame()
misclass.table.df <- data.frame()
misclass.best.k <- 0
misclass.best.k.df <- data.frame(misclass.best.k)

total <- data.frame(accuracy, sensitivity, specificity, misclass)

#### Set up temp tables to aid in the calculation of accuracy, sensitivity, and specificity
x =nlevels(last)
data1 <- rep(x = 0,0, x^2)
newmatrix <- matrix(rep(x = 0), nrow <- nlevels(last))
newmatrix <- matrix(data = data1, nrow = x)
rownames(newmatrix) <- c(levels(last))
colnames(newmatrix) <- c(levels(last))
table1 <- as.table(newmatrix)
table1.df <- data.frame(unclass(table1))
results.table <- data.frame(table1.df)
accuracy.table <- data.frame()
sensitivity.table <- data.frame()
specificity.table <- data.frame()
misclass.df <- data.frame()


temptable <- table1
temptable <- as.table(table1)

temptable1 <- table1
temptable1 <- as.table(table1)

temptable2 <- table1
temptable2 <- as.table(table1)

temptable3 <-table1
temptable3 <- as.table(table1)

tables.df <- data.frame()

#### Analysis begins here ####
for (i in 1:1){ # i measures the number of times we will update optimal.accuracy.rate.
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      print(i)
      print(j)
      print(k)
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.1, 0.9),2)
      dfsize <- as.integer((nrow(newdf))*ratio)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.data.frame(train.df)
      test.df <- newdf[-train,]
      test.df <- as.data.frame(test.df)
      
      
      #### Actual knn analysis is here ####
      for (l in 1:25){
        knn.df <- knn(train.df, test.df, cl = dfLabels[train], k = l)
      }
      table1 <- table(knn.df, last[-train])
      accuracy2 <- sum(diag(table1))/sum(table1)
      accuracy2.df <- rbind(accuracy2.df, accuracy)
      
      
      
      #### Save table if it is valid
      if(nrow(table1) == ncol(table1)){
        if(ncol(table1) == 2){
          table1 <- as.data.frame.matrix(table1)
          tables.df <- rbind(tables.df, table1)
        }
      }
      
      
      #### - Measure overall accuracy of the knn model ####
      table1 <- table(knn.df, last[-train])
      if(nrow(table1) == ncol(table1)){
        accuracy = sum(diag(table1)) / sum(table1)
        if (accuracy>accuracytmp){
          table1 <- data.frame(unclass(table1))
          accuracytmp = accuracy
          table1 <- as.data.frame.matrix(table1)
          accuracy.table = rbind(accuracy.table, table1)
          accuracy.df <- rbind(accuracy.df, accuracy)
          temptable <- table1
          accuracy.best.k <- l
          accuracy.best.k.df <- rbind(accuracy.best.k.df, accuracy.best.k)
          saveRDS(knn.df, file = "/tmp/knn.max.accuracy.rda")
        }
      }
      # #### Measure the Sensitivity (positive accuracy) of the knn model
      table1 <- table(knn.df, last[-train])
      if (nrow(table1) == ncol(table1)){
        sensitivity <- table1[4] / sum(table1[3:4])
        if(sensitivity>sensitivitytmp | sensitivity == 1){
          if(table1[4]> temptable2[2:2]){
            sensitivity.table.df <- as.data.frame.matrix(table1)
            sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
            sensitivity.df <- rbind(sensitivity.df, sensitivity)
            sensitivitytmp = sensitivity
            temptable2 = table1
            saveRDS(knn.df, file = "/tmp/knn.max.sensitivity.rda")
            sensitivity.best.k <- l
            sensitivity.best.k.df <- rbind(sensitivity.best.k.df, sensitivity.best.k)
          }
        }
      }
      
      # 
      # #### Measure the Specificity (negative accuracy) of the knn model
      table1 <- table(knn.df, last[-train])
      if (nrow(table1) == ncol(table1)){
        specificity <- table1[[1]] / sum(table1[,1])
        if(specificity>specificitytmp | specificity == 1){
          if(table1[1:1]> temptable2[1:1]){
            specificity.table.df <- as.data.frame.matrix(table1)
            specificity.table <- rbind(specificity.table, specificity.table.df)
            specificity.df <- rbind(specificity.df, specificity)
            specificitytmp = specificity
            temptable2 = table1
            saveRDS(knn.df, file = "/tmp/knn.max.specificity.rda")
            specificity.best.k <- l
            specificity.best.k.df <- rbind(specificity.best.k.df, specificity.best.k)
          }
        }
      }
      ###### Measure the lowest amount of misclassification error #####
      table1 <- table(knn.df, last[-train])
      if (nrow(table1) == ncol(table1)){
        misclass <- (sum(table1[2:3]))/(sum(table1))
        if(misclass<misclasstmp | misclass == 0){
          misclass.df <- rbind(misclass.df, misclass)
          misclass.table <- as.data.frame.matrix(table1)
          misclass.table.df <- rbind(misclass.table.df, misclass.table)
          misclasstmp = misclass
          temptable3 = table1
          saveRDS(knn.df, file = "/tmp/knn.max.misclass.rda")
          misclass.best.k <- l
          misclass.best.k.df <- rbind(misclass.best.k.df, misclass.best.k)
        }
      }
    }
  }
  
}

# ############ ----------- display results to the user ---------------- ####################


print("The highest overall accuracy:")
max(accuracy.df)
tail(accuracy.table,n = nlevels(last))
"The value of K that resulted in best accuracy:"
tail(accuracy.best.k.df,n = 1)
max.accuracy <- readRDS(file = "/tmp/knn.max.accuracy.rda")



print("The highest sensitivity (positive accuracy)")
max(sensitivity.df)
max.sensitivity <- readRDS(file = "/tmp/knn.max.sensitivity.rda")
tail(sensitivity.table, n = nlevels(last))
print("the value of k that resulted in the highest sensitivity:")
tail(sensitivity.best.k.df,1)


print("The highest specificity (negative accuracy")
print(max(specificity.df))
tail(specificity.table, n = nlevels(last))
print("The value of k that resulted in the highest specificity:")
tail(specificity.best.k.df,1)


print("The lowest misclassification")
#print(min(misclass.df))
print(misclass.table.df)
print("the value of k that resulted in the lowest misclass rate:")
tail(misclass.best.k.df,1)


