library(lc)

bone=read.table("D:/RProject/DataRepository/ESL/bone.data",header = TRUE)
train=bone[,-4];yTrain=bone$spnbmd
out00=ScaleData(train,yTrain,shuffle = TRUE);train=out00$train;yTrain=out00$respTrain

males=train$gender=="male"
females=train$gender=="female"
boneMaleSmooth = smooth.spline( train$age[males], yTrain[males], df=12 )
boneFemaleSmooth = smooth.spline( train$age[females], yTrain[females], df=12 )

plot(boneMaleSmooth$x,boneMaleSmooth$y,type='l',ylim=c(min(yTrain),max(yTrain)),
     col="blue", xlab="Age", ylab="relative change in Spinal BMD")
points(train$age[males],yTrain[males],col="blue",pch=20)
lines(boneFemaleSmooth$x,boneFemaleSmooth$y,col="red",type='l')
points(train$age[females],yTrain[females],col="red",pch=20)
legend("topright",legend = c("Male","Female"),col=c("blue","red"),lty=c(1,1),cex = 0.7)
