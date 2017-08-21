library(lc)
library(ESLMixtures)
vowelTr=read.table("D:/RProject/DataRepository/ESL/vowel.train",sep=',',header=TRUE);vowelTr$row.names=NULL
yTrain=vowelTr$y;train=vowelTr[,-1]
vowelTe=read.table("D:/RProject/DataRepository/ESL/vowel.test",sep=',',header=TRUE);vowelTe$row.names=NULL
yTest=vowelTe$y;test=vowelTe[,-1]
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

out0=ScaleData(train,Response = yTrain,test,yTest,type="Standardize")
train=out0$train;yTrain=out0$respTrain;test=out0$test;yTest=out0$respTest

rrda=lc(train,yTrain,fit="RRDA")
visualizeReduced(train,yTrain,rrda)
par(mfrow=c(2,2))
visualizeReduced(train,yTrain,rrda,coord1 = 1,coord2 = 3)
visualizeReduced(train,yTrain,rrda,coord1 = 2,coord2 = 3)
visualizeReduced(train,yTrain,rrda,coord1 = 1,coord2 = 7)
visualizeReduced(train,yTrain,rrda,coord1 = 9,coord2 = 10)
