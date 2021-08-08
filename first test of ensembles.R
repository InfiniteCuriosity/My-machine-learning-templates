lapply(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost',
         'caretEnsemble', 'C50', 'earth'), require, character.only = TRUE)

# Import dataset
Orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')
Orange <- Orange[complete.cases(Orange),]
sum(is.na(Orange))


# See top 6 rows and 10 columns
head(Orange[, 1:10])

# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(Orange$Purchase, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- Orange[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- Orange[-trainRowNumbers,]

# Store X and Y for later use.
x = trainData[, 2:18]
y = trainData$Purchase

# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE,
                             index=createFolds(Orange$Purchase, 10))

algorithmList <- c('earth')

set.seed(100)
models <- caretList(Purchase ~ ., data=trainData, trControl=trainControl, methodList=algorithmList,continue_on_fail = TRUE)
results <- resamples(models)
summary(results)
