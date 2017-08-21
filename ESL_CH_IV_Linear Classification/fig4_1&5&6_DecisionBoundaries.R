library(lc)
library(ESLMixtures)

### create data
out=callMixtures(chap = "4")
train=out$train
yTrain=out$yTrain
##scale
out0=ScaleData(train,Response = yTrain,type="Standardize")
train=out0$train;yTrain=out0$respTrain
#plot(train,col=yTrain,pch=as.character(yTrain))#raw data plot
### prepare grid
L=100#size of the grid
X=seq(min(train[,1]),max(train[,1]),length=L)
Y=seq(min(train[,2]),max(train[,2]),length=L)
XY=expand.grid(X,Y)
XY=ScaleData(XY,type="Standardize")$train

### QDA
out=lc(train,yTrain,"QDA")
yHat=lcPredict(XY,out)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
contour(X, Y, matrix(as.numeric(yHat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=yTrain,pch=19)
title("QDA")

### LDA
out=lc(train,yTrain,fit = "LDA")
yHat=lcPredict(XY,out)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
contour(X, Y, matrix(as.numeric(yHat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=yTrain,pch=19)
title("LDA")

### LDA with quadratic terms
train1=cbind(train,train[,1]*train[,2],train[,1]^2,train[,2]^2)
XY1=cbind(XY,X*Y,X^2,Y^2)
XY1=ScaleData(XY1,type="Standardize")$train
out=lc(train1,yTrain,fit = "LDA")
yHat=lcPredict(XY1,out)
plot(train1, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n")
contour(X, Y, matrix(as.numeric(yHat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train1, col=yTrain,pch=19)
title("LDA with quadratic terms")
