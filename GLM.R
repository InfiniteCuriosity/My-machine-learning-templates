rm(list = ls())
library(tidyverse)
library(ISLR)
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)
#df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
names(df)[names(df)==names(df[ncol(df)])] = 'last'
df <- df[,2:ncol(df)]
#summary(df)
#df$last <-  as.numeric(df$last)
accuracy <- 0
accuracytmp <- 0
table1.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))
rownames(table1.df) = c("No", "Yes")
colnames(table1.df) = c("0", "1")
accuracy.table <- data.frame(table1.df)
accuracy.df <- data.frame(accuracy)

sensitivity <- 0
sensitivitytmp <- 0
sensitivity.table <- data.frame()
sensitivity.df <- data.frame(sensitivity)

sumtable1 <- 0
dummy1.df <- data.frame(table1.df)
j <- 0
pos.percentage.df <- data.frame(j)
maxyes <- 0

specificity <- 0
specificitytmp <- 0
specificity.table <- data.frame()
specificity.df <- data.frame(specificity)
sumtable1 <- 0
dummy1.df <- data.frame(table1.df)
j <- 0
neg.percentage.df <- data.frame(j)
maxno <- 0


misclassification.error <- 0
misclassificationtmp <- 0
misclass <- data.frame(misclassification.error)

total <- data.frame(accuracy, sensitivity, specificity, misclass)

table1.df <- data.frame()
tables.df <- data.frame()

temptable <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable) = c("No", "Yes")
temptable <- as.table(temptable)

temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
rownames(temptable1) = c("No", "Yes")
temptable1 <- as.table(temptable1)


for(i in 1:10){
  for(j in 1:999){
    randomnumbers <- runif(nrow(df))
    df <- df[order(randomnumbers),]
    ratio <- round(runif(1, 0.25, 0.75),2)
    train <- sample(nrow(df), nrow(df)*ratio,replace = FALSE)
    train.df <- df[train,]
    test.df <- df[-train,]
    glm.fit <- glm(train.df$last~., data = train.df, family = binomial)
    glm.probs <- predict(glm.fit, test.df, type = "response")
    glm.pred <- rep("No", nrow(test.df))
    glm.pred[glm.probs > .001*j] = "Yes"
    table1 <- table(glm.pred, test.df$last)
    table1.df <- data.frame(unclass(table1))
    if(nrow(table1.df == ncol(table1.df))){
      tables.df <- rbind(tables.df, table1.df)
    }
    
    #### - Measure overall accuracy of the GLM ####
    if(nrow(table1) == ncol(table1)){
      accuracy = sum(diag(table1)) / sum(table1)
      if (accuracy>accuracytmp){
        accuracytmp = accuracy
        accuracy.table = rbind(accuracy.table, table1.df)
        accuracy.df = rbind(accuracy.df, accuracy)
        saveRDS(glm.fit, file = "/tmp/glm.max.accuracy.rda")      
        }
      }
    
### Measure the Sensitivity (positive accuracy) of the GLM
    if (nrow(table1) == ncol(table1)){
       sensitivity <- table1[4:4] / sum(table1[3:4])
       if(sensitivity>sensitivitytmp | sensitivity == 1){
         if(table1[4:4]> temptable[4:4]){
           sensitivity.table.df <- as.data.frame.matrix(table1)
           sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
           sensitivity.df <- rbind(sensitivity.df, sensitivity)
           sensitivitytmp = sensitivity
           temptable = table1
           pos.percentage.df <- rbind(pos.percentage.df,j)
           saveRDS(glm.fit, file = "/tmp/glm.max.sensitivity.rda")
         }
       }
    }

    if (nrow(table1) == ncol(table1)){
      specificity <- table1[1:1] / sum(table1[1:2])
      if(specificity>specificitytmp | specificity == 1){
        if(table1[1:1]> temptable1[1:1]){
          specificity.table.df <- as.data.frame.matrix(table1)
          specificity.table <- rbind(specificity.table, specificity.table.df)
          specificity.df <- rbind(specificity.df, specificity)
          specificitytmp = specificity
          temptable1 = table1
          pos.percentage.df <- rbind(pos.percentage.df,j)
          saveRDS(glm.fit, file = "/tmp/glm.max.specificity.rda")
        }
      }
    }

  }
}

############ ----------- display results to the user ---------------- ####################


print("The highest overall accuracy:")
print(max(accuracy.df))
max.accuracy <- readRDS(file = "/tmp/glm.max.accuracy.rda")
accuracy.table[(nrow(accuracy.table)-1):nrow(accuracy.table),]

print("The highest sensitivity (positive accuracy)")
print(max(sensitivity.df))
max.sensitivity <- readRDS(file = "/tmp/glm.max.sensitivity.rda")
summary(max.sensitivity)
sensitivity.table[(nrow(sensitivity.table)-1):nrow(sensitivity.table),]

print("The highest specificity (negative accuracy")
print(max(specificity.df))
max.specificity <- readRDS(file = "/tmp/glm.max.specificity.rda")
summary(max.specificity)
specificity.table[(nrow(specificity.table)-1):nrow(specificity.table),]

