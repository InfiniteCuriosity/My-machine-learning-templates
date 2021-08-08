library(caret)
library(mlbench)
data(Sonar)

set.seed(107)
inTrain <- createDataPartition(
  y = Sonar$Class,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)
## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)
#>  int [1:157, 1] 1 2 3 4 5 7 10 11 12 13 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr "Resample1"

training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

nrow(training)
#> [1] 157
nrow(testing)
#> [1] 51

ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)


set.seed(123)
plsFit <- train(Class~.,
                data = training,
                method = "pls",
                preProc = c("center", "scale"),
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC"
                )
plsFit

ggplot(plsFit)

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

confusionMatrix(data = plsClasses, testing$Class)

# Fitting another model to the data

rdaGrid <- data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(
  Class~.,
  data = training,
  method = "rda",
  tuneGrid = rdaGrid,
  trControl = ctrl,
  metric = "ROC"
)
rdaFit
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps)
summary(diffs)
