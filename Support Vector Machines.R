Heart <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
library(e1071)

Heart$famhist = ifelse(Heart$famhist == "Absent", 0 ,1)

randomnumbers <- runif(nrow(Heart))
Heart <- Heart[order(randomnumbers),]
Heart <- Heart[,c(2:ncol(Heart))]
Heart$chd <- as.factor(Heart$chd)

TestTrain <- sort(sample(nrow(Heart), nrow(Heart)*0.70))
Heart.train <- Heart[TestTrain,]
Heart.test <- Heart[-TestTrain,]
HeartLabelsTrain <- Heart.train[TestTrain,]
HeartLabelsTest <- Heart.test[-TestTrain,]

x <- Heart.train[,1:ncol(Heart)-1]
y <- Heart.train[,ncol(Heart)]

Heart.svm <- svm(x = x, y = y)
Heart.pred <- predict(Heart.svm, x)
Table6 <- table(Heart.pred, y)

accuracy <- sum(diag(Table6)) / sum(Table6)
accuracy 

# need to finish this, and calculate MSE for this result.

# optimal SVM for the data set

set.seed(1)
x <-matrix(rnorm(200*2), ncol = 2)
x [1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1,150), rep(2,50))
dat = data.frame(x = x, y = as.factor(y))
plot(x, col = y)
train <-sample(200,100)
svmfit <- svm(y~.,data = dat[train,], kernel = "radial", gamma = 1, cost = 1 )
plot(svmfit, dat[train,])
summary(svmfit)
svmfit <- svm(y~.,data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
# use cross validation to select the best choice of gamma and cost for an SVM with a radial kernel

set.seed(1)
tune.out = tune(svm, y~., data = dat[train,], kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))
summary(tune.out)
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))
(67+21) / (67+21+2+10) # 88% - how can I use this in my package??? :)


# ROC curvers
library(ROCR)
rocplot = function(pred, truth,...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

svmfit.opt <- svm(y~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.opt, dat[train,], decision.values = TRUE))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
svmfit.flex = svm(y~., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = TRUE)
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "test data")
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")

# 9.6.4, SVM with multiple classes

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))
x[y == 0,2] = x[y == 0, 2]+2
dat = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
svmfit = svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

