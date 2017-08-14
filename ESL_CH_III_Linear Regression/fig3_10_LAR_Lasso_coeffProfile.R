library(lm2)
#This graph plots LAR coefficient profile instead of Lasso

##scale
data=read.table("D:/RProject/DataRepository/ESL/ProstateData.txt")
train=data[data$train=="TRUE",];test=data[data$train=="FALSE",];train$train=NULL;test$train=NULL
yTrain=train$lpsa;yTest=test$lpsa;train$lpsa=NULL;test$lpsa=NULL

out0=ScaleData(train,Response = yTrain,test,yTest,type="Unit")
train=out0$train;yTrain=out0$respTrain;test=out0$test;yTest=out0$respTest

train1=apply(train,2,function(x) x/sqrt(sum(x^2))) #convert to unit norm
test1=apply(test,2,function(x) x/sqrt(sum(x^2))) #convert to unit norm

out1=lm2(train1,yTrain,fit = "LAR")
betaMat=out1$betaMat
L2=ncol(betaMat)

plot(1:L2,betaMat[2,],main = "LAR Coefficients Profile",xlab = "iterations",
         ylab = "Coefficients",type='l',ylim=c(min(betaMat),max(betaMat)),col=1)
for (j in 1:(nrow(betaMat)-1)){
    lines(1:L2,betaMat[j+1,],type='l',col=j)
}

