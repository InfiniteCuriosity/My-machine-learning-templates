library(tidyverse)
library(ISLR)
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)
#df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
names(df)[names(df)==names(df[ncol(df)])] = 'last'
#summary(df)
#df$last <-  as.numeric(df$last)
accuracy <- 0
accuracytmp <- 0
table1.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))
rownames(table1.df) = c("No", "Yes")
accuracy.table <- data.frame(table1.df)
accuracy.df <- data.frame(accuracy)

sensitivity <- 0
sensitivitytmp <- 0
table1.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))
rownames(table1.df) = c("No", "Yes")
sensitivity.table <- data.frame(table1.df)
sensitivity.df <- data.frame(sensitivity)
sumtable1 <- 0
dummy1.df <- data.frame(table1.df)
j <- 0
pos.percentage.df <- data.frame(j)
maxyes <- 0

specificity <- 0
specificitytmp <- 0
specificity.table <- data.frame(table1.df)
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

for(i in 1:100){
  for(j in 1:99){
    randomnumbers <- runif(nrow(df))
    df <- df[order(randomnumbers),]
    ratio <- round(runif(1, 0.25, 0.75),2)
    train <- sample(nrow(df), nrow(df)*ratio,replace = FALSE)
    train.df <- df[train,]
    test.df <- df[-train,]
    glm.fit <- glm(train.df$last~., data = train.df, family = binomial)
    glm.probs <- predict(glm.fit, test.df, type = "response")
    glm.pred <- rep("No", nrow(test.df))
    glm.pred[glm.probs > .01*j] = "Yes"
    table1 <- table(glm.pred, test.df$last)
    table1.df <- data.frame(unclass(table1))
    if(nrow(table1.df) == ncol(table1.df)){
      if(table1.df[1,2]==0 | table1.df[2,1]==0)
        tables.df <- rbind(tables.df, table1.df)
    }
    
    #### - Measure overall accuracy of the GLM ####
    accuracy <- (sum(diag(table1)) / sum(table1))
    if (accuracy>accuracytmp){
      if(nrow(table1.df) == ncol(table1.df)){
        accuracytmp=accuracy
        if(table1.df$X1[2])
          accuracy.table <- rbind(accuracy.table, table1.df)
          accuracy.df <- rbind(accuracy.df, accuracy)
      }
      saveRDS(glm.fit, file = "/tmp/glm.max.accuracy.rda")
    }
    accuracy.df <- rbind(accuracy.df, accuracy)
    
    
    #### Measure the Sensitivity (positive accuracy) of the GLM
    dummytable.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))
    rownames(dummytable.df) = c("No", "Yes")
    if (nrow(table1.df) == ncol(table1.df)){
      sensitivity <- table1.df$X1[2] / sum(table1.df$X1[1:2])
      sensitivity.df <- rbind(sensitivity.df, sensitivity)
      dummy1.df <- rbind(dummy1.df, table1.df)
      if(sensitivity>sensitivitytmp | sensitivity == 1){
        if(table1.df$X1[2] > dummytable.df$X1[2]){
          sensitivity.table <- rbind(sensitivity.table, table1.df)
          sensitivitytmp = sensitivity
          dummytable.df = table1.df
          pos.percentage.df <- rbind(pos.percentage.df,j)
          saveRDS(glm.fit, file = "/tmp/glm.max.sensitivity.rda")
        }
      }    
    }
  }
  
  
  #### Measure the Specificity (negative accuracy) of the GLM
  dummytable.df <- data.frame("X0" = c(0, 0), "X1" = c(0, 0))
  rownames(dummytable.df) = c("No", "Yes")
  if (nrow(table1.df) == ncol(table1.df)){
    specificity <- table1[1] / sum(table1[1:2])
    specificity.df <- rbind(specificity.df, specificity)
    dummy1.df <- rbind(dummy1.df, table1.df)
    if(specificity>specificitytmp | specificity == 1){
      if(table1.df$X1[2] > dummytable.df$X1[2]){
        specificity.table <- rbind(specificity.table, table1.df)
        specificitytmp <- specificity
        dummytable.df = table1.df
        neg.percentage.df <- rbind(neg.percentage.df,j)
        saveRDS(glm.fit, file = "/tmp/glm.max.specificity.rda")
      }
    }    
  }
}
###################### ---- good up to this point #############################

print("The highest overall accuracy:")
print(max(accuracy.df))
max.accuracy <- readRDS(file = "/tmp/glm.max.accuracy.rda")
summary(max.accuracy)
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
