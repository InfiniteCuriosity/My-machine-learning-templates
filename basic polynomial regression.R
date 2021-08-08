df <- mtcars[,c(1,3:7)]

for (i in 1:ncol(df)) {
  for (j in 1:3) {
  df.lm <- lm(mpg~poly(df[,i],j), data = df)
  print(summary(df.lm))
  print(sprintf("i = %s, j = %0.0f", names(df)[i],j))
  }
}

