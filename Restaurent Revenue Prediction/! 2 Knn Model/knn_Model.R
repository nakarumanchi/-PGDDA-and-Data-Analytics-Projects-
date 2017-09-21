require(ggplot2)
require(caret)


getwd()
setwd('!!Regression/ML Process/!2 Restaurent Revenue Prediction/! 2 Knn Model/')
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
control3=trainControl(method="repeatedcv",number=30,repeats=5)

set.seed(10001)
knn_model1=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="knn",trControl=control1,tuneLength=37)
knn_model1
knn_model1$finalModel
varImp(knn_model1)
x11()
plot(varImp(knn_model1))

set.seed(10002)
knn_model2=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="knn",trControl=control3,tuneLength=37)
knn_model2




test=read.csv('test.csv',header=T,sep=",",na.strings=c("NA"," ",""))
dim(test)
dim(training)
names(test)
names(training)

levels(training$Type)=levels(test$Type)

test$Prediction=predict(knn_model2,test[,-c(1,2,3,4,5,43)])
summary(test$Prediction)
x11()
write.csv(test[,c("Id","Prediction")],file="rpartmodel.csv",row.names=F)

