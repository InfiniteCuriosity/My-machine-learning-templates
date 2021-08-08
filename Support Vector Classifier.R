# Heart <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
# library(e1071)
# 
# Heart$famhist = ifelse(Heart$famhist == "Absent", 0 ,1)
# 
# randomnumbers <- runif(nrow(Heart))
# Heart <- Heart[order(randomnumbers),]
# Heart <- Heart[,c(2:ncol(Heart))]
# Heart$chd <- as.factor(Heart$chd)
# 
# 
# HeartLabels = Heart[,ncol(Heart)]
# TestTrain <- sort(sample(nrow(Heart), nrow(Heart)*0.70))
# Heart.train <- Heart[TestTrain,]
# Heart.test <- Heart[-TestTrain,]
# HeartLabelsTrain <- HeartLabels[TestTrain]
# HeartLabelsTest <- HeartLabels[-TestTrain]
# svm.Heart <- svm(chd~., data = Heart, kernel = "linear", cost = 10, scale = FALSE)
# svm.Heart
# plot(x = svm.Heart,)


library(e1071)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y == 1,] = x[y==1,] + 1
plot(x, col = (3-y))

dat = data.frame(x = x, y = as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)
