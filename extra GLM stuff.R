# #### - Measure overall accuracy of the GLM ####
# if(nrow(table1) == ncol(table1)){
#   accuracy = sum(diag(table1)) / sum(table1)
#   if (accuracy>accuracytmp){
#     accuracytmp = accuracy
#     accuracy.table = rbind(accuracy.table, table1.df)
#     accuracy.df <- rbind(accuracy.df, accuracy)
#     saveRDS(glm.fits, file = "/tmp/glm.max.accuracy.rda")
#     prediction <- prediction(predictions = glm.probs,labels = last[-train])
#     cost_perf = performance(prediction, "cost")
#     optimal.accuracy.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
#     optimal.accuracy.df <- rbind(optimal.accuracy.df, optimal.accuracy.rate)
#   }
}
}
}
#}   


# temptable <- matrix(data = c(0,0,0,0),nrow = 2)
# rownames(temptable) = c("No", "Yes")
# temptable <- as.table(temptable)
# 
# temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
# rownames(temptable1) = c("No", "Yes")
# temptable1 <- as.table(temptable1)
# 
# for (i in 1:10){ # i measures the number of times we will update optimal.sensitivity.rate.
#   print("Sensitivity")
#   print(i)
#   for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
#     combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
#     for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
#       colvals <- c(combin[k,])
#       newdf <- data.frame(df[,colvals])
#       newdf <- cbind(newdf, last)
#       # Break the data set into random amounts of test and train
#       ratio <- round(runif(1, 0.1, 0.9),2)
#       dfsize <- as.integer((nrow(newdf))*ratio)
#       train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
#       train.df <- newdf[train,]
#       train.df <- as.data.frame(train.df)
#       test.df <- newdf[-train,]
#       test.df <- as.data.frame(test.df)
#       test.df$last <- as.factor(test.df$last)
#       
#       #### Actual analysis is here ####
#       glm.fits <- glm(last~., data = train.df, family = binomial)
#       glm.probs <- predict(glm.fits, test.df, type = "response")
#       glm.pred <- rep("No", nrow(test.df))
#       glm.pred[glm.probs > optimal.sensitivity.rate] = "Yes"
#       table1 <- table(glm.pred, test.df$last)
#       table1.df <- data.frame(unclass(table1))
#       
#       #### Save table if it is valid
#       if(nrow(table1) == ncol(table1)){
#         if(ncol(table1) == 2){
#           tables.df <- rbind(tables.df, table1.df)
#         }
#       }     
#       
#       #### Measure the overall accuracy, minimze the difference between test and train ####
#       glm.fits <- glm(last~., data = train.df, family = binomial)
#       glm.probs <- predict(glm.fits, train.df, type = "response")
#       glm.pred <- rep("No", nrow(train.df))
#       glm.pred[glm.probs > optimal.sensitivity.rate] = "Yes"
#       table1 <- table(glm.pred, train.df$last)
#       table1.df <- data.frame(unclass(table1))
#       accuracy.train <- sum(diag(table1))/sum(table1)
#       
#       glm.fits <- glm(last~., data = train.df, family = binomial)
#       glm.probs <- predict(glm.fits, test.df, type = "response")
#       glm.pred <- rep("No", nrow(test.df))
#       glm.pred[glm.probs > optimal.accuracy.rate] = "Yes"
#       table2 <- table(glm.pred, test.df$last)
#       table2.df <- data.frame(unclass(table2))
#       accuracy.test <- sum(diag(table2))/sum(table2)
#       

# 
# 
#       
#       
#       
#       
#       
#       
#       #### Measure the Sensitivity (positive accuracy) of the GLM
#       if (nrow(table1) == ncol(table1)){
#         sensitivity <- table1[4:4] / sum(table1[3:4])
#         if(sensitivity>sensitivitytmp | sensitivity == 1){
#           if(table1[4:4]> temptable[4:4]){
#             sensitivity.table.df <- as.data.frame.matrix(table1)
#             sensitivity.table <- rbind(sensitivity.table, sensitivity.table.df)
#             sensitivity.df <- rbind(sensitivity.df, sensitivity)
#             sensitivitytmp = sensitivity
#             temptable = table1
#             saveRDS(glm.fits, file = "/tmp/glm.max.sensitivity.rda")
#             cost_perf = performance(prediction, "cost")
#             optimal.sensitivity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
#             optimal.sensitivity.df <- rbind(optimal.sensitivity.df, optimal.sensitivity.rate)
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# temptable <- matrix(data = c(0,0,0,0),nrow = 2)
# rownames(temptable) = c("No", "Yes")
# temptable <- as.table(temptable)
# 
# temptable1 <- matrix(data = c(0,0,0,0),nrow = 2)
# rownames(temptable1) = c("No", "Yes")
# temptable1 <- as.table(temptable1)
# 
# for (i in 1:10){ # i measures the number of times we will update optimal.specificity.rate
#   print("Specificity")
#   print(i)
#   for (j in 1:ncol(df)){ # this creates all possible permutations of the columns
#     combin <- combinations(n = ncol(df), r = j, repeats.allowed = FALSE) ### remember to change this!!!
#     for (k in 1:nrow(combin)){ # Loop to create all possible data sets by column, then we can do the analysis of each one
#       colvals <- c(combin[k,])
#       newdf <- data.frame(df[,colvals])
#       newdf <- cbind(newdf, last)
#       # Break the data set into random amounts of test and train
#       ratio <- round(runif(1, 0.1, 0.9),2)
#       dfsize <- as.integer((nrow(newdf))*ratio)
#       train <- sample(nrow(newdf), nrow(newdf)*ratio,replace = FALSE)
#       train.df <- newdf[train,]
#       train.df <- as.data.frame(train.df)
#       test.df <- newdf[-train,]
#       test.df <- as.data.frame(test.df)
#       test.df$last <- as.factor(test.df$last)
#       
#       #### Actual analysis is here ####
#       glm.fits <- glm(last~., data = train.df, family = binomial)
#       glm.probs <- predict(glm.fits, test.df, type = "response")
#       glm.pred <- rep("No", nrow(test.df))
#       glm.pred[glm.probs > optimal.specificity.rate] = "Yes"
#       table1 <- table(glm.pred, test.df$last)
#       table1.df <- data.frame(unclass(table1))
#       
#       #### Save table if it is valid
#       if(nrow(table1) == ncol(table1)){
#         if(ncol(table1) == 2){
#           tables.df <- rbind(tables.df, table1.df)
#         }
#       }     
#       
#       #### Measure the Specificity (negative accuracy) of the GLM
#       if (nrow(table1) == ncol(table1)){
#         specificity <- table1[1:1] / sum(table1[1:2])
#         if(specificity>specificitytmp | specificity == 1){
#           if(table1[1:1]> temptable1[1:1]){
#             specificity.table.df <- as.data.frame.matrix(table1)
#             specificity.table <- rbind(specificity.table, specificity.table.df)
#             specificity.df <- rbind(specificity.df, specificity)
#             specificitytmp = specificity
#             temptable1 = table1
#             saveRDS(glm.fits, file = "/tmp/glm.max.specificity.rda")
#             cost_perf = performance(prediction, "cost")
#             optimal.specificity.rate <- prediction@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]
#             optimal.specificity.df <- rbind(optimal.specificity.df, optimal.specificity.rate)
#           }
#         }
#       }
#     }
#   }
# }
# 


# ############ ----------- display results to the user ---------------- ####################


# print("The highest overall accuracy: (accurate rate and value of i)")
# max(accuracy.df)
# tail(accuracy.table,n = 2)
# max.accuracy <- readRDS(file = "/tmp/glm.max.accuracy.rda")
# summary(max.accuracy)
# print(optimal.accuracy.rate)
# tail(accuracy.df)


# print("The highest sensitivity (positive accuracy)")
# max(sensitivity.df)
# sensitivity.table.df
# max.sensitivity <- readRDS(file = "/tmp/glm.max.sensitivity.rda")
# summary(max.sensitivity)
# print(optimal.sensitivity.rate)
# tail(sensitivity.df)
# 
# 
# print("The highest specificity (negative accuracy")
# print(max(specificity.df))
# max.specificity <- readRDS(file = "/tmp/glm.max.specificity.rda")
# summary(max.specificity)
# specificity.table[(nrow(specificity.table)-1):nrow(specificity.table),]
# print(optimal.specificity.rate)
# tail(specificity.df)

#############

# glm.fits <- glm(last~., data = train.df, family = binomial)
# glm.probs <- predict(glm.fits, train.df, type = "response")
# glm.pred <- rep("No", nrow(train.df))
# glm.pred[glm.probs > optimal.sensitivity.rate] = "Yes"
# table1 <- table(glm.pred, train.df$last)
# table1.df <- data.frame(unclass(table1))
# table1
# 
# accuracy.train <- sum(diag(table1))/sum(table1)
# accuracy.train
# 
# glm.fits <- glm(last~., data = train.df, family = binomial)
# glm.probs <- predict(glm.fits, test.df, type = "response")
# glm.pred <- rep("No", nrow(test.df))
# glm.pred[glm.probs > optimal.accuracy.rate] = "Yes"
# table2 <- table(glm.pred, test.df$last)
# table2.df <- data.frame(unclass(table2))
# table2
# accuracy.test <- sum(diag(table2))/sum(table2)
# accuracy.test


# accuracytmp <- (abs(accuracy.train - accuracy.test))


# best.train.accuracy.df <- rbind(best.train.accuracy.df, accuracy.train)
# best.test <- saveRDS(object = table2, file = "tmp/best.test.rda")
# best.test.accuracy.df <- rbind(best.test.accuracy.df, accuracy.test)
# best.model <- saveRDS(object = glm.fits, file = "tmp/best.glm.model")
