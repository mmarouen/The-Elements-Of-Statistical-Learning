library(MASS)
library(class)
tabTrain= read.table("D:/RProject/DataRepository/ESL/zip.train")
tabTest=read.table(gzfile("D:/RProject/DataRepository/ESL/zip.test.gz"))
L1= tabTrain$V1== 2 | tabTrain$V1==3
L2= tabTest$V1== 2 | tabTest$V1==3
train= tabTrain[L1,]
test=tabTest[L2,]
Y=as.numeric(train[,1])
Ytest=as.numeric(test[,1])

#linear model
X=as.matrix(cbind(1,train[,-1]))
Xtest=as.matrix(cbind(1,test[,-1]))
betaHat=ginv(t(X)%*%X)%*%t(X)%*%Y
YhatTrain=X%*%betaHat
YhatTest=Xtest%*%betaHat
GLinTrain=rep(3,length(YhatTrain))
GLinTrain[YhatTrain<2.5]=2
GLinTest=rep(3,length(YhatTest))
GLinTest[YhatTest<2.5]=2
LinErrTrain=mean(GLinTrain != Y)#0.431%
LinErrTest=mean(GLinTest != Ytest)# 3.57%
#knn
neighbors =seq(1:14)
N=length(neighbors)
knnErrTrain=vector(length = N)
knnErrTest=vector(length = N)
for (i in 1:N){
  GknnTrain=knn(train,train,cl=Y,k=neighbors[i])
  knnErrTrain[i]=mean(GknnTrain != Y)
  GknnTest=knn(train,test,cl=Y,k=neighbors[i])
  knnErrTest[i]=mean(GknnTest != Ytest)
}
#plot
plot(neighbors, knnErrTest, xlab = "k-nearest neighbors",ylab = "error rate",type='b',col='blue',ylim = c(0,0.05))
points(neighbors, knnErrTrain,type='b',col='red')
abline(h=LinErrTest, col='blue')
abline(h=LinErrTrain,col='red')

