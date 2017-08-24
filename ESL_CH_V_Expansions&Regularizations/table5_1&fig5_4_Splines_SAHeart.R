library(lm2)
library(nnet)
library(splines)

SA=read.table("D:/RProject/DataRepository/ESL/SAheart.data",sep=',',header = TRUE)
train=SA[,-c(1,11)];yTrain=SA$chd### result=76.57%
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain
# test=train[1:100,];yTest=yTrain[1:100];train=train[101:462,];yTrain=yTrain[101:462]
levels(train$famhist)=c(0,1)
train$famhist=as.numeric(as.character(train$famhist))

out0=ScaleData(train,yTrain,type="Standardize")
train=out0$train;yTrain=out0$respTrain

###model 1
X=cbind(ns(train$sbp,4),ns(train$tobacco,4),ns(train$ldl,4),train$famhist,ns(train$obesity,4),ns(train$age,4))
df=data.frame(yTrain,X);logis=multinom(yTrain~.,data=df)
beta=coef(logis)
weights=as.vector(logis$fitted.values*(1-logis$fitted.values))
H=cbind(1,X)
sigma=solve(t(H)%*%(weights*H))
###Generate table
pval=beta/sqrt(diag(sigma))
###plot
par(mfrow=c(3,2))
##sbp variable
N=cbind(1,ns(train$sbp,df=4))
yhat=N%*%beta[1:5]
SigmaHat=solve(t(N)%*%(weights*N))
ycov=N%*%SigmaHat%*%t(N)
yvar=diag(ycov)
m=min(yhat-2*sqrt(yvar))
M=max(yhat+2*sqrt(yvar))
vv=order(train$sbp)
plot(sort(train$sbp),yhat[vv],type='l',ylim=c(m,M),pch=19,xlab = 'sbp',ylab='f(sbp)')
lines(sort(train$sbp),yhat[vv]+2*sqrt(yvar[vv]),col='red',type='l',pch=19)
lines(sort(train$sbp),yhat[vv]-2*sqrt(yvar[vv]),col='red',type='l',pch=19)
##tobacco
N=cbind(1,ns(train$tobacco,df=4))
yhat=N%*%beta[c(1,6:9)]
SigmaHat=solve(t(N)%*%(weights*N))
ycov=N%*%SigmaHat%*%t(N)
yvar=diag(ycov)
m=min(yhat-2*sqrt(yvar))
M=max(yhat+2*sqrt(yvar))
vv=order(train$tobacco)
plot(sort(train$tobacco),yhat[vv],type='l',ylim=c(m,M),pch=19,xlab='tobacco',ylab='f(tobacco)')
lines(sort(train$tobacco),yhat[vv]+2*sqrt(yvar[vv]),col='red',type='l',pch=19)
lines(sort(train$tobacco),yhat[vv]-2*sqrt(yvar[vv]),col='red',type='l',pch=19)
##ldl
N=cbind(1,ns(train$ldl,df=4))
yhat=N%*%beta[c(1,10:13)]
SigmaHat=solve(t(N)%*%(weights*N))
ycov=N%*%SigmaHat%*%t(N)
yvar=diag(ycov)
m=min(yhat-2*sqrt(yvar))
M=max(yhat+2*sqrt(yvar))
vv=order(train$ldl)
plot(sort(train$ldl),yhat[vv],type='l',ylim=c(m,M),pch=19,xlab="ldl",ylab='f(ldl)')
lines(sort(train$ldl),yhat[vv]+2*sqrt(yvar[vv]),col='red',type='l',pch=19)
lines(sort(train$ldl),yhat[vv]-2*sqrt(yvar[vv]),col='red',type='l',pch=19)
##obesity
N=cbind(1,ns(train$obesity,df=4))
yhat=N%*%beta[c(1,15:18)]
SigmaHat=solve(t(N)%*%(weights*N))
ycov=N%*%SigmaHat%*%t(N)
yvar=diag(ycov)
m=min(yhat-2*sqrt(yvar))
M=max(yhat+2*sqrt(yvar))
xx=train$obesity[order()]
vv=order(train$obesity)
plot(sort(train$obesity),yhat[vv],type='l',ylim=c(m,M),pch=19,xlab='Obesity',ylab='f(Obesity)')
lines(sort(train$obesity),yhat[vv]+2*sqrt(yvar[vv]),col='red',type='l',pch=19)
lines(sort(train$obesity),yhat[vv]-2*sqrt(yvar[vv]),col='red',type='l',pch=19)
##age
N=cbind(1,ns(train$age,df=4))
yhat=N%*%beta[c(1,19:22)]
SigmaHat=solve(t(N)%*%(weights*N))
ycov=N%*%SigmaHat%*%t(N)
yvar=diag(ycov)
m=min(yhat-2*sqrt(yvar))
M=max(yhat+2*sqrt(yvar))
vv=order(train$age)
plot(sort(train$age),yhat[vv],type='l',ylim=c(m,M),pch=19,xlab='Age',ylab='f(Age)')
lines(sort(train$age),yhat[vv]+2*sqrt(yvar[vv]),col='red',type='l',pch=19)
lines(sort(train$age),yhat[vv]-2*sqrt(yvar[vv]),col='red',type='l',pch=19)
