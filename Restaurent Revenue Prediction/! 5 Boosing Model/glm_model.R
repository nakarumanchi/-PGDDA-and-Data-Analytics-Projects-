require(ggplot2)
require(caret)
require(reshape2)
require(rpart)
require(rpart.plot)
require(Amelia)


require(doParallel)
cl=makeCluster(detectCores())
registerDoParallel(cl)

getwd()
setwd('!!Regression/ML Process/!2 Restaurent Revenue Prediction/! 4 Equation based Learning/')
getwd()
training=read.csv('train.csv',header=T,sep=",",na.strings=c("NA",""," "))
dim(training)
str(training)
names(training)
head(training)
summary(training$revenue)
training$Type=factor(training$Type)

?histogram

ggplot(training)+geom_histogram(aes(x=revenue))

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

gbmGrid=expand.grid(interaction.depth=3:7,n.trees=seq(100,500,50),shrinkage=c(0.01,0.1),n.minobsinnode=5)


set.seed(10001)

gbm_model1=train(training[,-c(1,2,3,4,5,43)],training[,c("revenue")],method="gbm",trControl=control2,tuneGrid=gbmGrid)
gbm_model1







test=read.csv('test.csv',header=T,sep=",",na.strings=c("NA"," ",""))
dim(test)
dim(training)
names(test)
names(training)

levels(training$Type)=levels(test$Type)

test$Prediction=predict(lm_model1,test[,-c(1,2,3,4,5,43)])
summary(test$Prediction)
write.csv(test[,c("Id","Prediction")],file="lm_model1.csv",row.names=F)


stopCluster(cl)