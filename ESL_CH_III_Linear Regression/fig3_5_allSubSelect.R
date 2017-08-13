library(lm2)

data=read.table("D:/RProject/DataRepository/ESL/ProstateData.txt")
train=data[data$train=="TRUE",];test=data[data$train=="FALSE",];train$train=NULL;test$train=NULL
yTrain=train$lpsa;yTest=test$lpsa;train$lpsa=NULL;test$lpsa=NULL

out=ScaleData(Input = train,yTrain,test,yTest,type = "Standardize")
train=out$train;yTrain=out$respTrain;test=out$test;yTest=out$respTest
out1=lm2(train,yTrain,fit="Subset")
plot(out1$DoF,out1$errorVector,type='b',pch=19,ylab="RSS",xlab = "degrees of freedom")

