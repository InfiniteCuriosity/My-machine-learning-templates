library(tidyverse)
library(ISLR)
library(pls)
df <- Carseats
df <-  df %>% dplyr::relocate(Price, .after = last_col())
names(df)[names(df)==names(df[ncol(df)])] <- 'last'


numbers <- seq(1:300)
df.test <- df[numbers,]
df.train <- df[-numbers,]

############ --- Principal Components regression ----- ############

pcr.df <- pcr(df.train$last~., data = df.train, scale = TRUE, validation = "CV")
summary(pcr.df)
validationplot(pcr.df,val.type="MSEP")

for (i in 1:ncol(df)){
  pcr.pred <- predict(pcr.df, df.test, ncomp = i)
  PCR.MSE <- mean((pcr.pred - df.test$last)^2)
  print(PCR.MSE)
}


############ --- Partial Least Squares regression ----- ############

pls.df <- plsr(df.train$last~., data = df.train, scale = TRUE, validation = "CV")
for (i in 1:ncol(df.test)){
  pls.pred <- predict(pls.df, newdata = df.test, ncomp = i)
  PLS.MSE <- mean((pls.pred - df.test$last)^2)
  print(PLS.MSE)
}

summary(pls.df)
