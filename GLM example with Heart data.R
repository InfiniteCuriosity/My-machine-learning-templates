final <- 0
results <- data.frame(final)
library(tidyverse)
heart <- read.csv(file = 'https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data')
heart$famhist = factor(heart$famhist)
heart.glm <- glm(chd~., data = heart, family = "binomial")
summary(heart.glm)
heart1 <- heart %>% select(tobacco, ldl, famhist, typea, age, chd)
heart1.glm <- glm(chd~.,data = heart1, family =  "binomial")
summary(heart1.glm)
glm.probs = predict(heart1.glm, type = "response")
glm.pred = rep("Down", nrow(heart))
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred, heart1$chd)
table01 <- table(glm.pred, heart1$chd)
accuracy <- function(table){
  sum(diag(table01))/sum(table01)
}
accuracy(table01)


####randomly split the data 75% train, 25% test, run the analysis ####

heart2 <- sort(sample(nrow(heart), nrow(heart)*0.70))
heart2.train <- heart[heart2,]
heart2.test <- heart[-heart2,]
heart2.train <- select(heart2.train, tobacco, ldl, famhist, typea, age, chd)
heart2.test <- select(heart2.test,tobacco, ldl, famhist, typea, age, chd)

heart2.train.glm <- glm(chd~., data = heart2.train, family = binomial)
summary(heart2.train.glm)

glm.probs = predict(heart2.train.glm, type = "response")
glm.pred = rep("Down", nrow(heart2.train))
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred, heart2.train$chd)
table1 <- table(glm.pred, heart2.train$chd)
accuracy <- function(table1){
  sum(diag(table1))/sum(table1)
}
accuracy(table1)

### using that glm from the testing data on the training data, and measuring the result:
for (i in 1:100) {
  glm.fit <- glm(chd~., data = heart2.test,family = "binomial")
  glm.probs <- predict(glm.fit, heart2.test, type = "response")
  glm.pred = rep("Down", nrow(heart2.test))
  glm.pred[glm.probs>0.5] = "Up"
  table(glm.pred, heart2.test$chd)
  table2 <- table(glm.pred, heart2.test$chd)
  accuracy <- function(table2){
    sum(diag(table2))/sum(table2)
  }
  final <- 100*accuracy(table2) # highest accuracy with logistic regression 82.01439%, lowest = 69.06475
  results <- rbind(results, final)
}
