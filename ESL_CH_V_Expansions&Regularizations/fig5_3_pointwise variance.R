library(splines)
n=50
x0=runif(n,0,1)
x0=sort(x0)
X=cbind(1,x0,x0^2,x0^3)
eps=rnorm(length(x0))
beta <- c(4.3965,1.9407,-0.1215,0.1535)#assumed true coefficients
Y=X%*%beta+eps
#Y=cos(x0)+eps

###cubic spline 
N=bs(x0,knots=c(0.33,0.66),degree =6)
N=cbind(1,N)
H=N%*%solve(t(N)%*%N)%*%t(N)
yhat_1=H%*%Y
sigma2_1=(1/(n-ncol(N)))*sum((yhat_1-Y)^2)
var1=diag(H)
###natural cubic spline
v=seq(0.1,0.9,length=6)
N2=ns(x = x0,knots = v[2:5],Boundary.knots = c(0.1,0.9),df = 6)
H2=N2%*%solve(t(N2)%*%N2)%*%t(N2)
yhat_2=H2%*%Y
sigma2_2=(1/(n-ncol(N2)))*sum((yhat_2-Y)^2)
var2=diag(H2)
#ploynomial fit
P=poly(x0,3)
H3=P%*%solve(t(P)%*%P)%*%t(P)
yhat_3=H3%*%Y
sigma2_3=(1/(n-ncol(P)))*sum((yhat_3-Y)^2)
var3=diag(H3)
#global linear
X1=cbind(1,x0)
H4=X1%*%solve(t(X1)%*%X1)%*%t(X1)
yhat_4=H4%*%Y
sigma2_4=(1/(n-ncol(X1)))*sum((yhat_4-Y)^2)
var4=diag(H4)

minY=min(var1,var2,var3,var4)
maxY=max(var1,var2,var3,var4)
plot(x0,var1,pch=20,type='b',ylim=c(minY,maxY+0.1),col="green",
     xlab = "X",ylab = "pointwisevariance",main = "Pointwise variance curves")
lines(x0,var2,pch=20,type='b',col="blue")
lines(x0,var3,pch=20,type='b',col="red")
lines(x0,var4,pch=20,type='b',col="orange")
legend("top",legend=c("cubic spline","natural cubic spline","polynomial fit","global linear"),
       col=c("green","blue","red","orange"),lty=1)
