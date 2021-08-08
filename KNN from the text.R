library(ISLR)
attach(Smarket)
train=(Year <2005)
Smarket.2005= Smarket[!train,]
Direction.2005=Direction[!train]
library(class)
train.X=cbind(Lag1 ,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction =Direction [train]
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)