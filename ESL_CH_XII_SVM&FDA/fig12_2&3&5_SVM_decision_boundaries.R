library(ESLMixtures)
library(rkhs)

### create data
out=callMixtures(chap = "2",order = "1")
out2=ScaleData(out$train,out$resp,out$test,out$respTest,shuffle = TRUE,type = "Normalize",symm = FALSE)
train=out2$train;yTrain=out2$respTrain;test=out2$test;yTest=out2$respTest
blue=out$BlueCentroids
orange=out$OrangeCentroids
yTrain=2*(as.numeric(as.character(yTrain))-0.5)
yTest=2*(as.numeric(as.character(yTest))-0.5)
### prepare grid
L=100#size of the grid
X=seq(min(train[,1]),max(train[,1]),length=L)
Y=seq(min(train[,2]),max(train[,2]),length=L)
XY=expand.grid(X,Y)
### bayesian boudaries
out=callMixtures(chap = "2",order = "2",input1 = XY,input2 = blue,input3 = orange)
yhatB=out$yhat_bayes

#For all below figures, the results obtained using our implementation 
#and using e1071 are both very different from book results
#perhaps this means that our implementation of the mixture is not correct :)

### figure 12.2
out1=RKHS(train,yTrain,kernel = 'linear',C = 0.01)
yhat=predictRkhs(XY,out1)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), 
     type="n",main="C=10000")
points(XY,col=as.factor(0.5*yhat+0.5), pch=".")
contour(X, Y, matrix(yhat,L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=as.factor(0.5*yTrain+0.5))

### figure 12.3 a :polynomial kernel
out1=RKHS(train,yTrain,kernel = 'polynomial',C = 1,degree = 3)
yhat1=predictRkhs(XY,out1)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), 
     type="n",main="C=1")
points(XY,col=as.factor(0.5*yhat1+0.5), pch=".")
contour(X, Y, matrix(yhat1,L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=as.factor(0.5*yTrain+0.5))

### figure 12.3 b :gaussian kernel, gamma=1
out1=RKHS(train,yTrain,kernel = 'gaussian',C = 1,gamm = 1)
yhat1=predictRkhs(XY,out1)
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), 
     type="n",main="C=1")
points(XY,col=as.factor(0.5*yhat1+0.5), pch=".")
contour(X, Y, matrix(yhat1,L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(train, col=as.factor(0.5*yTrain+0.5))
