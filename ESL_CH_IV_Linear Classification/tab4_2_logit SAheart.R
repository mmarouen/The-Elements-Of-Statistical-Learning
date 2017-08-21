source("E:/R Project/toolkits/Classification.R")
source("E:/R Project/toolkits/Routines.R")
data=read.table("E:/R Project/DataRepository/SAheart.data",sep = ",",header=T)
data$row.names=NULL
yTrain=as.factor(data$chd)
train=data
train$chd=NULL
train[,5]=as.factor(as.numeric(train[,5]))
train=train[,-c(4,6)]

out0=ScaleData(train,Response = yTrain,trainingScale = TRUE)
train=out0[[1]]
yTrain=out0[[2]]
dat=cbind(yTrain,train)

fit=glm(yTrain~.,data = dat,family=binomial())
prob1=predict(fit,dat,type = "response")
summary(prob1)

out=Logis(train,yTrain)
beta=out[[1]]
likelihood=unlist(out[[2]])
probs=out[[3]]
