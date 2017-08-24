library(lc)
library(splines)

phoneme=read.table("D:/RProject/DataRepository/ESL/phoneme.data",sep=',',header=TRUE)
train=phoneme[(phoneme$g %in% c("aa","ao")) & grepl('^train', phoneme$speaker),2:257]
test=phoneme[(phoneme$g %in% c("aa","ao")) & grepl('^test', phoneme$speaker),2:257]
yTrain=as.numeric(phoneme$g[(phoneme$g %in% c("aa","ao")) & grepl('^train', phoneme$speaker)])-1
yTest=as.numeric(phoneme$g[(phoneme$g %in% c("aa","ao")) & grepl('^test', phoneme$speaker)])-1
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

#-------------#
#1. Upper plot#
#-------------#
inds0=sample(1:nrow(train[yTrain==0,]),15)
inds1=sample(1:nrow(train[yTrain==1,]),15)
M=max(train[yTrain==0,][inds0,],train[yTrain==1,][inds1,])
plot(1:256,train[yTrain==0,][inds0[1],],type='l',col="green",ylim=c(0,M),
     xlab = "frequency",ylab = "Log-periodogram",main="Phoneme Examples")
lines(1:256,train[yTrain==1,][inds1[1],],type='l',col="orange")
for(i in 2:15){
  lines(1:256,train[yTrain==0,][inds0[i],],type='l',col="green")
  lines(1:256,train[yTrain==1,][inds1[i],],type='l',col="orange")
}
legend("topright",legend = c("aa","ao"),col=c("green","orange"),lty=c(1,1),cex = 0.5)
#--------------------------------------#
#2. Lower plot & classifiers comparison#
#--------------------------------------#
###2.1 models
#raw logistic
df=data.frame(yTrain,train)
fit1=glm(yTrain~.,data=df,family=binomial)
coef1=coefficients(fit1)[-1]#raw logisitic regression coefficients
#regularized natural spline
freq=seq(1,256)
N=ns(freq,df=12)
trainStar=as.matrix(train)%*%N;dfStar=data.frame(yTrain,trainStar)
fit2=glm(yTrain~.,data=dfStar,family=binomial)#regularized model
coefStar=coefficients(fit2)  
coef2=N%*%coefStar[-1]#regularized coefficient
###2.2 lower plot
plot(coef1,ylim=c(min(coef1),max(coef1)), type="l", xlab="Frequency", ylab="Logistic Regression Coefficients")
lines(coef2, col="red")
abline(h=0,lty=2)
###3. Classification
##we'll extend to other classifiers QDA,LDA,RRDA,RDA
results=matrix(0,ncol=6,nrow=2)
colnames(results)=c("Raw","Regularized","QDA","LDA","RDA","RRDA")
rownames(results)=c("Training Error","Testing Error")
#3.1 raw logistic
yTrainProb=predict(fit1,train,type="response");yTrainHat=as.double(yTrainProb>0.5)
yTestProb=predict(fit1,test,type="response");yTestHat=as.double(yTestProb>0.5)
results[,1]=c(round(mean(yTrainHat!=yTrain),digits = 3),mean(yTestHat!=yTest))
#3.2 regularized natural spline
testStar=data.frame(as.matrix(test)%*%N);trainStar=data.frame(trainStar)
yTrainProb=predict(fit2,trainStar,type="response");yTrainHat=as.double(yTrainProb>0.5)
yTestProb=predict(fit2,testStar,type="response");yTestHat=as.double(yTestProb>0.5)
results[,2]=c(mean(yTrainHat!=yTrain),mean(yTestHat!=yTest))
#3.3 QDA
fitQ=lc(train,yTrain,fit = "QDA")
yHatTrain=fitQ$yhat-1
yHatTest=lcPredict(test,fitQ)-1
results[,3]=c(mean(yHatTrain!=yTrain),mean(yHatTest!=yTest))
#3.4 LDA


print(results,digits = 3)
