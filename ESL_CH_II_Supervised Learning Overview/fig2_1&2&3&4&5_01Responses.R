library(ESLMixtures)
library(lm2)
source("D:/RProject/DataRepository/ESL/ESLMixtures/callMixtures.R")
### create data
out=callMixtures(chap = "2",order = "1")
out2=ScaleData(out$train,out$resp,out$test,out$respTest,shuffle = TRUE,type = "Standardize")
train=out2$train;yTrain=out2$respTrain;test=out2$test;yTest=out2$respTest
blue=out$BlueCentroids
orange=out$OrangeCentroids
###prepare grid
L=75#size of the grid
X=seq(min(train[,1]),max(train[,1]),length=L)
Y=seq(min(train[,2]),max(train[,2]),length=L)
XY=expand.grid(X,Y)

###figure2_1: linear model
Xtrain=as.matrix(cbind(1,train))
betaHat=solve(t(Xtrain)%*%Xtrain)%*%t(Xtrain)%*%as.numeric(as.character(yTrain))
yhat=as.matrix(cbind(1,XY))%*%betaHat
yhat=as.numeric(yhat>0.5)
a=-betaHat[2]/betaHat[3]
b=(0.5-betaHat[1])/betaHat[3]
plot(train,col=yTrain,xlab="X1",ylab="X2",xlim=range(train[,1]),ylim=range(train[,2]),type="n")
abline(b,a)
points(XY,col=yhat,pch=".")
points(train,col=yTrain)
title("Linear regression")

library(class)
###figure2_2: KNN-15
yhat=knn(train,XY,yTrain,k=15)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
points(XY,col=yhat, pch=".")
contour(X, Y, matrix(as.numeric(yhat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=yTrain)
title("knn-15")

###figure2_3: KNN-1
yhat=knn(train,XY,yTrain,k=1)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
points(XY,col=yhat, pch=".")
contour(X, Y, matrix(as.numeric(yhat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=yTrain)
title("knn-1")

###figure 2_4: misclassification curves for training & test 
###with linear regression and various KNN values
neighbors=c(1,2,3,4,5,6,7,9,10,11,15,20,30,37,45,60,70,85,100,125,150)
neighbors=c(1,2,3,4,5,6,7,9,10,11,15,20,22,24,26,28,30,35)
N=length(neighbors)
errorTrain=vector(length = N)
errorTest=vector(length = N)
dof=vector(length = N)
for(i in 1:N){
  yhatTrain=knn(train,train,k=neighbors[i],cl=yTrain)
  errorTrain[N-i+1]=mean(yhatTrain != yTrain)
  yhatTest=knn(train,test,k=neighbors[i],cl=yTrain)
  errorTest[N-i+1]=mean(yhatTest != yTest)
  dof[N-i+1]=200/neighbors[i]
}
yhatLinTrain=as.matrix(cbind(1,train))%*%betaHat
yhatLinTrain=as.numeric(yhatLinTrain>0.5)
yhatLinTest=as.matrix(cbind(1,test))%*%betaHat
yhatLinTest=as.numeric(yhatLinTest>0.5)
errLinTrain=mean(yhatLinTrain != yTrain)
errLinTest=mean(yhatLinTest != yTest)
plot(neighbors,errorTrain,type='l',col='blue',xlab="k-nearest neighbors",ylab = "test error",ylim = c(0,0.3),pch=19)
points(neighbors,errorTest,type='l',col='orange',pch=19)
points(10,errLinTrain,col='red')
points(10,errLinTest,col='green')

###figure 2_5: bayes decision boundary
out3=callMixtures(chap = "2",order = "2",input1 = XY,input2 = blue,input3 = orange)
yhat=out3$yhat_bayes
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
points(XY,col=yhat, pch=".")
contour(X, Y, matrix(as.numeric(yhat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=yTrain)
title("Bayes Decision Boundary")

