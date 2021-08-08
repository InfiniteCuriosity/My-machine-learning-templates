# df <- mtcars[,4:ncol(mtcars)]
# library(gtools)
# i = 1
# for (i in 1:ncol(df)){
#   combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
#   for (i in 1:nrow(combin)){
#       colvals <- c(combin[i,])
#       print(df[1,c(colvals)])
#     }
# }

#this prints all data frams
# df <- mtcars
# library(gtools)
# i = 1
# for (i in 1:ncol(df)){
#   combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
#   for (i in 1:nrow(combin)){
#     colvals <- c(combin[i,])
#     print(df[,c(colvals)])
#   }
# }
adj.r.squared <-0
results <- data.frame(adj.r.squared)
library(tidyverse)
df <- mtcars
df <- select(mtcars,cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, mpg)
names(df)[names(df)==names(df[ncol(df)])] = 'last'
df
library(gtools)
i = 1
last <- df$last
df <- df[,1:(ncol(df))-1]
print(df)
#for (i in 1:ncol(df)){
  combin <- combinations(n = ncol(df), r = i, repeats.allowed = FALSE)
  for (i in 1:nrow(combin)){
    colvals <- c(combin[i,])
    newdf <- df[,colvals]
    newdf <- cbind(newdf, true)
    newdf <- as.data.frame(newdf)
    newdf.lm = lm(newdf$last~., data = newdf)
    adj.r.squared <- summary(newdf.lm)$adj.r.squared
    results <- rbind(results, adj.r.squared)
    }
#}
newdf
newdf$true
newdf.lm = lm(newdf$true~., data = newdf)
