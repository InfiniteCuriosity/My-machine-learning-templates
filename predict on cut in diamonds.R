library(tidyverse)
attach(diamonds)
diamonds <- dplyr::select(diamonds, carat, color, clarity, depth, table, price, x, y, z, cut)

TestTrain <- sort(sample(nrow(diamonds), nrow(diamonds)*0.70))
Test <- diamonds[TestTrain,]
Train <- diamonds[-TestTrain,]
diamonds.lda <- lda(cut~., data = Train)
lda.pred <- predict(object = diamonds.lda, newdata = Test)
table1 <- table(lda.pred$class,Test$cut)
table1
sum(diag(table1)) / sum(table1)

diamonds.qda <- qda(cut~., data = Train)
qda.pred <- predict(object = diamonds.qda, newdata = Test)
table1 <- table(qda.pred$class, Test$cut)
table1
sum(diag(table1)) / sum(table1)


#### KNN starts here ####

library(class)

normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
diamonds <- as.data.frame(lapply(X = diamonds, FUN = normalize))

#str(diamonds) # verifies that the entire data set is normalized

#summary(diamonds) # check that the numerical values are normalized

# set up test (30%) and train (70%) data sets:

TestTrain <- sort(sample(nrow(diamonds), nrow(diamonds)*0.70))
diamonds.train <- diamonds[TestTrain,]
diamonds.test <- diamonds[-TestTrain,]
diamondsLabelsTrain <- diamondsLabels[TestTrain]
diamondsLabelsTest <- diamondsLabels[-TestTrain]


# use K-Nearest Neighbors (requires the Class library, see line 4)
diamonds.knn = knn(train = diamonds.train,test = diamonds.test,cl = diamonds.train$cut , k = sqrt(nrow(diamonds)))

Accuracy <- table(diamonds.knn,diamondsLabelsTest)

Accuracy
Accuracy.diamonds <- sum(diag(Accuracy))/sum(Accuracy)
Accuracy.diamonds # Highest I've seen so far is 0.8201439, lowest I've seen is  0.6546763