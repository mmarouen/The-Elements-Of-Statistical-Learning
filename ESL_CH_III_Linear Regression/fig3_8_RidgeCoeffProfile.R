library(lm2)
data=read.table("D:/RProject/DataRepository/ESL/ProstateData.txt")
train=data[data$train=="TRUE",];test=data[data$train=="FALSE",];train$train=NULL;test$train=NULL
yTrain=train$lpsa;yTest=test$lpsa;train$lpsa=NULL;test$lpsa=NULL

out0=ScaleData(train,Response = yTrain,type="Standardize")
train=out0$train;yTrain=out0$respTrain

out=Ridge(train,yTrain)
DF=out$DoF
beta=out$betaMat

plot(DF,beta[2,],col=1,pch=19,type='l',xlim = range(1:round(1.075*nrow(beta))),
     main = "Ridge Coefficients Profile",xlab = "df(Lambda)",ylab = "Coefficients",ylim=c(min(beta),max(beta[2:nrow(beta),])))
text(1.075*nrow(beta), 1.02*beta[2,1], rownames(beta)[2])
for (j in 3:(ncol(train)+1)){
  lines(DF,beta[j,],type='l',col=j,pch=19)
  text( 1.075*nrow(beta), 1.02*beta[j,1], rownames(beta)[j] )
}

