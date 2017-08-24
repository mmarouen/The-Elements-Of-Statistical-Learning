library(lc)

laozone=read.table("D:/RProject/DataRepository/ESL/LAozone.data",sep=',',header=TRUE)
train=laozone[,-1];yTrain=laozone$ozone;
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

smoother1=smooth.spline(train$dpg,yTrain,df=5,tol = 0.01)
smoother2=smooth.spline(train$dpg,yTrain,df=11,tol=0.01)
#########################1. Top plot
###smooth response
M=max(smoother1$y,smoother2$y,yTrain)
m=min(smoother1$y,smoother2$y,yTrain)
plot(train$dpg, yTrain,ylim=c(m-1,M+1),type='p',pch=20,
     xlab="Daggot Pressure Gradient",ylab="Ozone Concentration")
points(smoother1$x,smoother1$y,col="blue",type='l')
points(smoother1$x,smoother2$y,col="red",type='l')
#########################2. eigen decomposition plots & Banded Matrix effect
x=unique(sort(train$dpg))
N=length(x)
tmp=diag(N)
S1=apply(tmp,2,function(yy) smooth.spline(x,yy,df=5)$y)
S2=apply(tmp,2,function(yy) smooth.spline(x,yy,df=15,tol=1)$y)
aux=eigen(S1,symmetric = TRUE)
values=aux$values
vec=aux$vectors
aux2=eigen(S2,symmetric = TRUE)
values2=aux2$values
vec2=aux2$vectors
#########eigen values
M=max(values)
plot(values[1:25],pch=20,type='b',col='red',xlab="order",ylab = "Eigenvalues",ylim=c(0,M))
abline(h=c(1,0),lty=2,col=1)
points(values2[1:25],pch=20,type='b',col='blue')
#########eigen vectors
par(mfrow=c(2,2))
plot(x,vec[,3],type='l',col='blue',xlab="",ylab = "")
lines(x,vec2[,3],col='pink')
plot(x,vec[,4],type='l',col='blue',xlab="",ylab = "")
lines(x,vec2[,4],col='pink')
plot(x,vec[,5],type='l',col='blue',xlab="",ylab = "")
lines(x,vec2[,5],col='pink')
plot(x,vec[,6],type='l',col='blue',xlab="",ylab = "")
lines(x,vec2[,6],col='pink')
dev.off()
#########Rows of smoothing  matrix
par(mfrow=c(6,1))
for(i in seq(1,101,len=6)){
  plot(x,S1[i,],ylab=paste(i,"th row of S",sep=""),type="b")
}
dev.off()
#########Banded Matrix
tmp=S1;
for(i in 1:N) tmp[i,]=S1[(N-i+1),];
image(t(tmp), col=topo.colors(12))#rainbow(12), heat.colors(12), terrain.colors(12)

