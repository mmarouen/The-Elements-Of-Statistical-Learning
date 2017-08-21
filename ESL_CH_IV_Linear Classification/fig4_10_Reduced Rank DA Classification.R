library(lc)
vowelTr=read.table("D:/RProject/DataRepository/ESL/vowel.train",sep=',',header=TRUE);vowelTr$row.names=NULL
yTrain=vowelTr$y;train=vowelTr[,-1]
vowelTe=read.table("D:/RProject/DataRepository/ESL/vowel.test",sep=',',header=TRUE);vowelTe$row.names=NULL
yTest=vowelTe$y;test=vowelTe[,-1]
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

out0=ScaleData(train,Response = yTrain,test,yTest,type="Standardize")
train=out0$train;yTrain=out0$respTrain;test=out0$test;yTest=out0$respTest

p=ncol(train)
error=matrix(0,ncol=p,nrow=2)

for(i in 1:p){
  rrda=lc(train,yTrain,fit = "RRDA",sub=i)
  #yhatTrain=rrda$yhat
  yhatTrain=lcPredict(train,rrda)
  yhatTest=lcPredict(test,rrda)
  error[1,i]=mean(yTrain != yhatTrain)
  error[2,i]=mean(yTest != yhatTest)
}


plot(1:p,error[1,],type='b',col='blue',ylim=c(min(error[1,]),max(error[2,])),pch=19,xlab = "Dimension",
     main = "LDA and dimension reduction on the vowel data",ylab = "Misclassification rate")
lines(1:p,error[2,],type='b',col='orange',pch=19)
legend("topright",legend = c("train","test"),col = c("blue","orange"),lty = 1)
