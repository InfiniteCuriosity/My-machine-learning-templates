adj.r.squared <-0
colvals1 <- 0
coefficients1 <- 0
results.df <- data.frame(adj.r.squared)

library(tidyverse)
# df <- read.csv('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data', header = TRUE, stringsAsFactors = TRUE)
# df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/diabetes.csv', header = TRUE)
df <- read.csv('https://raw.githubusercontent.com/mlittmancs/great_courses_ml/master/data/ship.csv', header = TRUE, stringsAsFactors = TRUE)
df <- select(df, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked,Survived)
df <- as.data.frame(df)
library(gtools)
i = 1
last <- df[, ncol(df)]
df <- df[,1:(ncol(df))-1]
for (i in 1:ncol(df)){
  combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
  for (i in 1:nrow(combin)){
    colvals <- c(combin[i,])
    newdf <- data.frame(df[,colvals])
    newdf <- cbind(newdf, last)
    newdf <- as.data.frame(newdf)
    newdf.lm = lm(last~., data = newdf)
    adj.r.squared <- summary(newdf.lm)$adj.r.squared
    results.df <- rbind(results.df, adj.r.squared)
    }
}

max(results.df$adj.r.squared)
min(results.df$adj.r.squared)
mean(results.df$adj.r.squared)
hist(results.df$adj.r.squared,breaks = 100)
summary(newdf.lm)$adj.r.squared
sort(results.df$adj.r.squared,decreasing = TRUE)[1:10]

