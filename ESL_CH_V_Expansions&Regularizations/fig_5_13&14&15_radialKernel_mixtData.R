
nu=1
gaussKernel<-function(x,y,nu=1){
  return(exp(-nu*sum((x-y)^2)))
}
yvalues=sort(train[,1])

#figure 5.13: 5 samples from kernel function
xm=sort(sample(yvalues,size=5,replace = FALSE))
xvalues=seq(floor(range(train[,1])[1]),ceiling(range(train[,1])[2]),length.out = 100)
kernels=matrix(ncol=5,nrow=length(xvalues))
for(j in 1:length(xm)){
  kernels[,j]=apply(as.matrix(xvalues),1,function(xx) gaussKernel(xx,xm[j]))
}
plot(xvalues,kernels[,1],type='l',xlab = "x",ylab = "K(,xm)",main = "Radial Kernel in R")
segments(xm[1],0,x1=xm[1],y1=max(kernels[,1]),lty=2)
for(i in 2:length(xm)){
  lines(xvalues,kernels[,i],col=i)
  segments(xm[i],0,x1=xm[i],y1=max(kernels[,i]),lty=2)
}

#figure 5.14&15 kernel matrix eigenvalues & eigenvectors
kernelMatrix=matrix(ncol=nrow(train),nrow = nrow(train))
for(i in 1:nrow(kernelMatrix)){
  kernelMatrix[i,]=apply(as.matrix(yvalues),1,function(xx) gaussKernel(xx,yvalues[i]))
}
out=eigen(kernelMatrix)
eigenvalues=out$values
vectors=out$vectors
scaledVector=vectors%*%diag(sqrt(abs(eigenvalues)))

#eigenvalue plot
plot(eigenvalues[1:50],pch=19,col="purple",log = "y",xlab="",ylab="Eigenvalues")
#eigen vectors plot
M=max(scaledVector[,1:16])
m=min(scaledVector[,1:16])
plot.new()
par(mfrow = c(4,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
for(i in 1:16){
  plot(vectors[,i],ylim=c(m,M),type='l',col="blue",xaxt='n',yaxt='n',xlab = "",ylab = "")
}

plot.new()
par(mfrow = c(4,4),oma = c(0,0,0,0),mar = c(0,0,0,0))
for(i in 1:16){
  plot(scaledVector[,i],ylim=c(m,M),type='l',col="red",xaxt='n',yaxt='n',xlab = "",ylab = "")
}

