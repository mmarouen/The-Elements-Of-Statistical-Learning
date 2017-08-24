library(lm2)
library(ESLMixtures)
library(splines)

out=callMixtures()
train=out$train
yTrain=out$resp
orange=out$OrangeCentroids
blue=out$BlueCentroids
dat0=cbind(yTrain,train)
###prepare grid
L=100#size of the grid
X=seq(min(train[,1]),max(train[,1]),length=L)
Y=seq(min(train[,2]),max(train[,2]),length=L)
XY=expand.grid(X1=X,X2=Y)
###Additive NS fit
fit=glm(yTrain~ns(X1,4)+ns(X2,4),data=dat0,family = binomial())
prob=predict(fit,newdata = XY,type="response")
yhat=rep(0,length=nrow(XY))
yhat[prob>0.5]=1
yhat=factor(yhat)
###Tensor Product fit
v=ns(dat0$X1,4)*ns(dat0$X2,4)
fit2=glm(yTrain~ns(X1,4)*ns(X2,4),data=dat0,family = binomial())
prob2=predict(fit2,newdata = XY,type="response")
yhat2=rep(0,length=nrow(XY))
yhat2[prob2>0.5]=1
yhat2=factor(yhat2)
#################Plots
out=callMixtures(order = "2",input1 = XY,input2 = blue,input3 = orange)
yhat_bayes=out$yhat_bayes

par(mfrow=c(1,2))
###Top Plot
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n",main="Additive Natural Cubic Splines")
points(XY,col=yhat, pch="-")
contour(X, Y, matrix(as.numeric(yhat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
contour(X, Y, matrix(yhat_bayes,L,L), levels=c(0,1), add=TRUE, drawlabels=FALSE,col="blue")
points(train, col=yTrain,pch=20)
###Bottom Plot
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n",main="Natural Cubic Splines Tensor Product")
points(XY,col=yhat2, pch=".")
contour(X, Y, matrix(as.numeric(yhat2),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE)
contour(X, Y, matrix(yhat_bayes,L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE,col="purple",lty=2)
points(train, col=yTrain,pch=20)
###Tensor VS Additive Splines
plot(train, xlab="X1", ylab="X2", xlim=range(train[,1]), ylim=range(train[,2]), type="n",main="Additive Natural Cubic Splines")
points(XY[yhat2=="0",],col='orange',pch='.')
points(XY[yhat2=="1",],col='blue',pch='.')
contour(X, Y, matrix(as.numeric(yhat),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE,col="black",cex=1.5)
contour(X, Y, matrix(as.numeric(yhat2),L,L), levels=c(1,2), add=TRUE, drawlabels=FALSE,col="purple",cex=1.5,lty=2)
points(train[yTrain=="0",], col='orange')
points(train[yTrain=="1",],col='blue')
