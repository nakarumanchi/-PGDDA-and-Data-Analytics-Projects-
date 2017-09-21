require(ggplot2)
require(caret)


getwd()
setwd('!!Regression/ML Process/!2 Restaurent Revenue Prediction/! 1 Single Tree Model/')
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
control2=trainControl(method="boot",number=10,repeats=5)
control3=trainControl(method="repeatedcv",number=20,repeats=5)

set.seed(10001)
cart_model1=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rpart",trControl=control1,tuneLength=20)
cart_model1
summary(cart_model1)
cart_model1$method
cart_model1$modelInfo
cart_model1$modelType
cart_model1$results
cart_model1$resample
cart_model1$bestTune
cart_model1$call
cart_model1$metric
cart_model1$control
cart_model1$finalModel
varImp(cart_model1)
X11()
plot(varImp(cart_model1))
cart_model1$finalModel$variable.importance
plot(cart_model1$finalModel$variable.importance,type='both')
cart_model1$yLimits
cart_model1$perfNames
var(cart_model1$results)
  
set.seed(10002)
cart_model2=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rpart",trControl=control2,tuneLength=35)
cart_model2
cart_model2$results
cart_model2$resample
X11()
varImp(cart_model2) 
plot(varImp(cart_model2))

set.seed(10003)
cart_model3=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="rpart",trControl=control3)
cart_model3
x11()
plot(cart_model3)
cart_model3$results


test=read.csv('test.csv',header=T,sep=",",na.strings=c("NA"," ",""))
dim(test)
dim(training)
names(test)
names(training)

levels(training$Type)=levels(test$Type)

test$Prediction=predict(cart_model1,test[,-c(1,2,3,4,5,43)])
summary(test$Prediction)
x11()
write.csv(test[,c("Id","Prediction")],file="rpartmodel.csv",row.names=F)






