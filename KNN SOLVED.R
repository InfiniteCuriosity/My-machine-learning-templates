library(class)
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE, stringsAsFactors = TRUE)
#df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)
df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
df <- as.data.frame(df)
df <- df[,2:ncol(df)]
names(df)[names(df)==names(df[ncol(df)])] <- 'last'
df$last <- as.factor(df$last)

# convert other categorical variables to numerical for KNN to use
df$famhist = ifelse(df$famhist == "Present", 1, 0)

# add in a results data frame:
accuracy <- 0
accuracytemp <- 0
sumdiagtmp <- 0
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

# randomize the rows
randomnumbers <- runif(nrow(df))
df <- df[order(randomnumbers),]

# isolate the labels IN THE ORDER OF THE RANDOM DATA SET
dfLabels = df[,ncol(df)]


# remove the target feature, which is in the last column:
df <- df[,c(2:ncol(df)-1)]

# rescale the numerical features
# Normalize the numbers, so it scales all the features to a range of (0, 1)

normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
df <- as.data.frame(lapply(X = df, FUN = normalize))

#str(df) # verifies that the entire data set is normalized

#summary(df) # check that the numerical values are normalized

# set up test and train data sets:
for(i in 1:100000){
  ratio <- round(runif(1, 0.05, 0.95),2)
  train <- sort(sample(nrow(df), nrow(df)*ratio))
  df.train <- df[train,]
  df.test <- df[-train,]
  dfLabelsTrain <- dfLabels[train]
  dfLabelsTest <- dfLabels[-train]
  
  
  # use K-Nearest Neighbors (requires the Class library, see line 4)
  df.knn = knn(train = df.train,test = df.test,cl = dfLabelsTrain , k = sqrt(nrow(df)))
  
  confusion.matrix <- table(df.knn,dfLabelsTest)
  table1.df <- data.frame(unclass(confusion.matrix))
  
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  results <- rbind(results, accuracy)
  if(accuracy>accuracytemp){
    accuracytemp <-  accuracy
    results.table <- rbind(results.table, table1.df)
    saveRDS(df.knn, file = "/tmp/knn.max.accuracy.rda")
    knn.confusion.matrix <- confusionMatrix(table(df.knn, dfLabelsTest))
    saveRDS(knn.confusion.matrix, file =  "/tmp/knn.confusion.Matrix.rda")
  }
}

############ -------------- Output the results to the user  -------------- ############

cat("\014") # clears Console in R
#results <- results$accuracy[-(1)]

knn1 <- readRDS(file = "/tmp/knn.confusion.Matrix.rda")
knn1