##generate data
n=100
x = sort(runif(n,0,1))
eps=rnorm(n,0,1)
fy=sin(12*(x+0.2))/(x+0.2)
y = sin(12*(x+0.2))/(x+0.2) + eps
tmp =diag(n)

## fitted pplots df=5, df=9,df=15
smooth_5=smooth.spline(x, y, df=5)
S5 <- apply(tmp,1,function(yy) smooth.spline(x,yy,df=5)$y)
var_5=diag(S5%*%t(S5))
min_5=smooth_5$y-2*sqrt(var_5)
max_5=smooth_5$y+2*sqrt(var_5)

smooth_9=smooth.spline(x, y, df=9)
S9 <- apply(tmp,1,function(yy) smooth.spline(x,yy,df=9)$y)
var_9=diag(S9%*%t(S9))
min_9=smooth_9$y-2*sqrt(var_9)
max_9=smooth_9$y+2*sqrt(var_9)

smooth_15=smooth.spline(x, y, df=15)
S15 <- apply(tmp,1,function(yy) smooth.spline(x,yy,df=15)$y)
var_15=diag(S15%*%t(S15))
min_15=smooth_15$y-2*sqrt(var_15)
max_15=smooth_15$y+2*sqrt(var_15)

par(mfrow=c(2,2))
plot(x,y,ylim=c(min(y)-1,max(y)+1),col="red",main = "lambda= 5")
lines(x,fy,col="red")
points(x,smooth_5$y,col="blue",type='l')
points(x,max_5,type='l')
points(x,min_5,type='l')
polygon(c(x,rev(x)),c(min_5,rev(max_5)),col="orange")

plot(x,y,ylim=c(min(y)-1,max(y)+1),col="purple",main = "lambda= 9",pch=20)
lines(x,fy,col="purple")
lines(x,smooth_9$y,col="green")
lines(x,max_9,col='orange')
lines(x,min_9,col='orange')

plot(x,y,ylim=c(min(y)-1,max(y)+1),main="lambda=15")
lines(x,fy,col="purple")
lines(x,smooth_15$y,col="green")
lines(x,max_15,col='orange')
lines(x,min_15,col='orange')
dev.off()
##cross validation
epe=rep(0,length=16)
cverr=rep(0,length=16)
for(i in 5:20){
  smooth=smooth.spline(x,y,df=i)
  S=apply(tmp,1,function(yy) smooth.spline(x,yy,df=i)$y)
  epe[i-4]=mean((y-smooth$y)^2)
  cverr[i-4]=mean(((y-smooth$y)/(1-diag(S)))^2)
}
plot(5:20,epe,pch=19,col="blue",xlab="df(lambda)",ylim=c(min(epe,cverr),max(epe,cverr)),
     ylab="EPE(lambda) & CV(lambda)")
points(5:20,cverr,pch=19,col="orange")
abline(v = 9,lty=2)
