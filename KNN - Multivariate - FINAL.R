#### Need to update the contruction of Table1 so it is automatically the dimensions of the factor in the last column####

rm(list = ls()) # clears out the global environment
#library(tidyverse)
library(MASS)
library(class)
library(tidyverse)
library(ISLR)
library(gtools)
library(caret)
library(ROCR)




#### Part 1: Import the data ####
#df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- df[,2:ncol(df)] # remove the first column if it's jsut row numbers
#df <- iris

df <- Carseats
df <-  df %>% dplyr::relocate(Price, .after = last_col())
df$Urban <- ifelse(df$Urban=="Yes", 1,0)
df$US <- ifelse(df$US=="Yes", 1, 0)
df$ShelveLoc <- ifelse(df$ShelveLoc=="Bad", 1, ifelse(df$ShelveLoc == "Medium", 2,3))
df <- df[,1:ncol(df)-1]


#### Part 1a: Set the data up so that the desired feature is the last column
#df <- select(df, cyl,  disp,  hp, drat,   wt, qsec, vs, am, gear, carb, value)
names(df)[names(df)==names(df[ncol(df)])] = 'last' # Set the name of the last column to 'last'
df <- df[sample(nrow(df)),] # Randomize the rows of the data set
dfLabels = df[,ncol(df)]



#### Part 1b: Separate the last column, which is only 1 or 0, so we can use it in our analysis.
last <- df[,ncol(df)] # This is the true data, which we will use to make our determination of accuracy
last <- as.factor(last)
df <- df[,1:ncol(df)-1] # remove the last column, so the analysis is not impacted by this factor



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

for (i in 1:ncol(df)){
  df[,i] =  poly(df[,i],4, raw = TRUE)
}


#### Set up to measure and report accuracy of the analysis ####
i <- 0
accuracy <- 0
accuracytmp <- 0
accuracy.df <- data.frame(accuracy)
accuracy2 <- 0
accuracy2.df <- data.frame(accuracy2)
accuracy.best.k <- 0
accuracy.best.k.df <- data.frame(accuracy.best.k)



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

tables.df <- data.frame()

#### Analysis begins here ####
for (i in 1:1000){ # i measures the number of times we will update optimal.accuracy.rate.
  for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
    combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
    for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
      cat("\014")
      print(i)
      print(j)
      print(k)
      print(accuracytmp)
      colvals <- c(combin[k,])
      newdf <- data.frame(df[,colvals])
      
      # Break the data set into random amounts of test and train
      ratio <- round(runif(1, 0.25, 0.75),2)
      dfsize <- as.integer((nrow(newdf))*ratio)
      train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
      train.df <- newdf[train,]
      train.df <- as.data.frame(train.df)
      test.df <- newdf[-train,]
      test.df <- as.data.frame(test.df)
      
      
      #### Actual knn analysis is here ####
      for (l in 1:3){
        knn.df <- knn(train.df, test.df, cl = dfLabels[train], k = l)
      }
      table1 <- table(knn.df, last[-train])
      accuracy2 <- sum(diag(table1))/sum(table1)
#      accuracy2.df <- rbind(accuracy2.df, accuracy)
      
      
      #### - Measure overall accuracy of the knn model ####
      table1 <- table(knn.df, last[-train])
      if(nrow(table1) == ncol(table1)){
        accuracy = sum(diag(table1)) / sum(table1)
        if (accuracy>accuracytmp){
          if(sum(diag(table1))>sum(diag(temptable))){
            temptable <- table1
            table1 <- data.frame(unclass(table1))
            accuracytmp = accuracy
            table1 <- as.data.frame.matrix(table1)
            accuracy.table = rbind(accuracy.table, table1)
            accuracy.df <- rbind(accuracy.df, accuracy)
            accuracy.best.k <- l
            accuracy.best.k.df <- rbind(accuracy.best.k.df, accuracy.best.k)
            saveRDS(knn.df, file = "/tmp/knn.max.accuracy.rda")            
          }
        }
      }
     }
    }
  }

# ############ ----------- display results to the user ---------------- ####################
cat("\014")

print("The highest overall accuracy:")
max(accuracy.df)
tail(accuracy.table,n = nlevels(last))
"The value of K that resulted in best accuracy:"
tail(accuracy.best.k.df,n = 1)
max.accuracy <- readRDS(file = "/tmp/knn.max.accuracy.rda")