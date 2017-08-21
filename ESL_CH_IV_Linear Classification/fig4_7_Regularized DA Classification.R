library(lc)
vowelTr=read.table("D:/RProject/DataRepository/ESL/vowel.train",sep=',',header=TRUE);vowelTr$row.names=NULL
yTrain=vowelTr$y;train=vowelTr[,-1]
vowelTe=read.table("D:/RProject/DataRepository/ESL/vowel.test",sep=',',header=TRUE);vowelTe$row.names=NULL
yTest=vowelTe$y;test=vowelTe[,-1]
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

out0=ScaleData(train,Response = yTrain,test,yTest,type="Standardize")
train=out0$train;yTrain=out0$respTrain;test=out0$test;yTest=out0$respTest

L=100
error=matrix(0,ncol=L,nrow=2)
alpha=seq(0,1,length=L)
for(i in 1:L){
  regda=lc(train,yTrain,fit ="RegDA",alpha =  alpha[i],gamma = 1)
  yhatTrain=regda$yhat
  yhatTest=lcPredict(test,regda)
  error[1,i]=mean(as.numeric(as.character(yTrain)) != yhatTrain)
  error[2,i]=mean(as.numeric(as.character(yTest)) != yhatTest)
}
plot(alpha,error[1,],type='b',col='blue',ylim=c(min(error[1,]),max(error[2,])),pch=19,
     main="Regularized Discriminant Analysis on the Vowel Data",ylab="Misclassification Rate",xlab="alpha")
lines(alpha,error[2,],type='b',col='orange',pch=19)
