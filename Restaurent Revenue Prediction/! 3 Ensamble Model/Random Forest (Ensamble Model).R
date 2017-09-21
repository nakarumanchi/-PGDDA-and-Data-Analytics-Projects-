require(ggplot2)
require(caret)
require(randomForest)
require(doParallel)
cl=makeCluster(detectCores())
registerDoParallel(cl)

getwd()
setwd('!!Regression/ML Process/!2 Restaurent Revenue Prediction/! 3 Ensamble Model/')
getwd()
training=read.csv('train.csv',header=T,sep=",",na.strings=c("NA",""," "))
dim(training)
str(training)
names(training)
head(training)
summary(training$revenue)

summary(head(training))
str(head(training))

levels(training$City)
levels(training$City.Group)
levels(training$Type)

summary(training$P1)
x11()
xtabs(revenue~City,training)
plot(training$City.Group,training$revenue)
dotplot(revenue~Open.Date,training)

?trainControl
set.seed(1000)
control1=trainControl(method="cv",number=30,repeats=10)
control2=trainControl(method="boot",number=30,repeats=10)
control3=trainControl(method="repeatedcv",number=30,repeats=5)

set.seed(10001)
Rf_model1=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rf",trControl=control2)
Rf_model1
Rf_model1$method
Rf_model1$finalModel

Rf_model2=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rf",trControl=control2,tuneLength=36,ntree=1000)
Rf_model2
Rf_model2$method
Rf_model2$finalModel

cp=expand.grid(.mtry=c(1:50))

set.seed(100000)
Rf_model3=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rf",trControl=control2,tuneLength=36,ntree=1000,tuneGrid=cp)
Rf_model3
Rf_model3$method
Rf_model3$finalModel
x11()
varImpPlot(Rf_model3$finalModel)
Rf_model3$modelInfo



set.seed(100001)
Rf_model4=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rf",trControl=control2,tuneLength=36,ntree=2500,tuneGrid=cp)
Rf_model4$finalModel

test=read.csv('test.csv',header=T,sep=",",na.strings=c("NA"," ",""))
dim(test)
dim(training)
names(test)
names(training)

levels(training$Type)=levels(test$Type)

test$Prediction=predict(Rf_model4,test[,-c(1,2,3,4,5,43)])
summary(test$Prediction)
write.csv(test[,c("Id","Prediction")],file="Rf_model4.csv",row.names=F)


stopCluster(cl)


set.seed(100000)
gbm_model3=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="gbm",trControl=control2,tuneLength=36)
gbm_model3
gbm_model3$method
gbm_model3$finalModel
x11()
varImpPlot(Rf_model3$finalModel)
Rf_model3$modelInf