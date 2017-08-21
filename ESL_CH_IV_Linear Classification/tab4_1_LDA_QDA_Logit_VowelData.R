library(lc)

vowelTr=read.table("D:/RProject/DataRepository/ESL/vowel.train",sep=',',header=TRUE);vowelTr$row.names=NULL
yTrain=vowelTr$y;train=vowelTr[,-1]
vowelTe=read.table("D:/RProject/DataRepository/ESL/vowel.test",sep=',',header=TRUE);vowelTe$row.names=NULL
yTest=vowelTe$y;test=vowelTe[,-1]
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

out0=ScaleData(train,Response = yTrain,test,yTest,type="Standardize")
train=out0$train;yTrain=out0$respTrain;test=out0$test;yTest=out0$respTest


results=data.frame()
#1. LDA
linda=lc(train,yTrain,fit = "LDA")
yhatTest=lcPredict(test,linda);yhatTrain=linda$yhat
results[1,1]=mean(as.numeric(as.character(yTrain)) != yhatTrain)
results[1,2]=mean(as.numeric(as.character(yTest)) != yhatTest);results[2,3]="NA"
#3. QDA
qda=lc(train,yTrain,fit="QDA")
yhatTest=lcPredict(test,qda)
yhatTrain=qda$yhat
results[2,1]=mean(as.numeric(as.character(yTrain)) != yhatTrain)
results[2,2]=mean(as.numeric(as.character(yTest)) != yhatTest);results[3,3]="NA"
#4. Regularized Discriminant
regda=lc(train,yTrain,fit = "RegDA",alpha = 0.9,gamma = 1)
yhatTest=lcPredict(test,regda)
yhatTrain=regda$yhat
results[3,1]=mean(as.numeric(as.character(yTrain)) != yhatTrain)
results[3,2]=mean(as.numeric(as.character(yTest)) != yhatTest);results[4,3]="alpha= 0.9, gamma= 1"
#4. Reduced Rank Discriminant
rrda=lc(train,yTrain,fit = "RRDA",sub = 2)
yhatTest=lcPredict(test,rrda)
yhatTrain=rrda$yhat
results[4,1]=mean(as.numeric(as.character(yTrain)) != yhatTrain)
results[4,2]=mean(as.numeric(as.character(yTest)) != yhatTest)
results[4,3]="2nd order"

#results
colnames(results)=c("Train Err","Test Err","optimization")
rownames(results)=c("LDA","QDA","RegDA", "RRDA")
print(results)
