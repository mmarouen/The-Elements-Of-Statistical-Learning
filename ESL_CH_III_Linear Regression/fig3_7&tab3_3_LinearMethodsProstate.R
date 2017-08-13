library(lm2)
##scale
data=read.table("D:/RProject/DataRepository/ESL/ProstateData.txt")
train=data[data$train=="TRUE",];test=data[data$train=="FALSE",];train$train=NULL;test$train=NULL
yTrain=train$lpsa;yTest=test$lpsa;train$lpsa=NULL;test$lpsa=NULL
#NB: the book seems to have scaled data prior to processing
#In practice, this is not recommended !

out0=ScaleData(train,Response = yTrain,type="Standardize",shuffle = TRUE)
train=out0$train;yTrain=out0$respTrain
####Ridge regression
out1=lm2(train,yTrain,fit = "Ridge",CV = TRUE,cvScale = FALSE)
err=out1$errorVector;df=out1$DoF;stds=out1$stdDev
CVplot(err,stds,DoF = df,stepSize = 4)
####Best subset selection
out1=lm2(train,yTrain,fit = "Subset",CV = TRUE,cvScale = FALSE)
err=out1$errorVector;df=out1$DoF;stds=out1$stdDev
CVplot(err,stds,DoF = df)
####PCR regression
out1=lm2(train,yTrain,fit = "PCR",CV = TRUE,cvScale = FALSE)
err=out1$errorVector;df=out1$DoF;stds=out1$stdDev
CVplot(err,stds,df)
####LAR regression
#unit norm
train1=apply(train,2,function(x) x/sqrt(sum(x^2)))
out1=lm2(train1,yTrain,fit = "LAR",CV = TRUE,cvScale = FALSE)
err=out1$errorVector;sd=out1$stdDev;DoF=out1$DoF
CVplot(err,sd,DoF)
####PLS regression
#something is wrong with cv plot, I'm working on it.
out1=lm2(train,yTrain,fit = "PLS",CV = TRUE,cvScale = FALSE)
err=out1$errorVector;df=out1$DoF;stds=out1$stdDev
CVplot(err,stds,df)

