### Required libreary 
require(excel.link)
require(Hmisc)
require(VIM)
require(ggplot2)
require(graphics)
require(caret)
require(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
options(max.print = .Machine$integer.max)

### Set the Path
getwd()


denials <- xl.read.file('Profile_ Evaluation data_04 01 17.xlsb',header=TRUE, top.left.cell="A1",xl.sheet="Denials")

### export data into CSV files
write.csv(denials,file = "denials_report.csv",row.names=F)

### Importing the CSV file in R.

denials_report = read.csv('denials_report.csv',header=T,sep=",",na.strings=c("NA"," ",""),stringsAsFactors=F)
dim(denials_report)
str(denials_report)
contents(denials_report)
describe(denials_report)
#### Change the class type od variables based on the requirement
denials_report$Enc.Rendering = as.factor(denials_report$Enc.Rendering)
denials_report$Src.Type = as.factor(denials_report$Src.Type)
denials_report$Tran.Cd = as.factor(denials_report$Tran.Cd)
denials_report$Tran.Status = as.factor(denials_report$Tran.Status)
denials_report$Category = as.factor(denials_report$Category)
denials_report$Sub.Category = as.factor(denials_report$Sub.Category)
denials_report$Rsn.Cds.Remarks = as.factor(denials_report$Rsn.Cds.Remarks)
denials_report$CPT4 = as.factor(denials_report$CPT4)
denials_report$Sv.It = as.factor(denials_report$ Sv.It)
denials_report$Department = as.factor(denials_report$Department)
denials_report$Component = as.factor(denials_report$Component)
denials_report$Place.Of.Serv = as.factor(denials_report$Place.Of.Serv)
denials_report$Fin.Class = as.factor(denials_report$Fin.Class)
denials_report$Payer.Name = as.factor(denials_report$Payer.Name)

## Identify the Missing values in entaire data.

NA_values = which(colSums(sapply(denials_report,is.na)) > 0)
sort(colSums(sapply(denials_report[NA_values],is.na)), decreasing = TRUE)
x11()
aggr_plot <- aggr(denials_report, col=c('navyblue','red'), numbers = TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram for %missing data","Pattern"))


## inatial EDA for data underatanding the Data
### Identify the top5 denials in Provider Id
describe(denials_report$Enc.Rendering)
sort(xtabs( ~ Enc.Rendering, data=denials_report),decreasing = TRUE)
x11()
ggplot(denials_report, aes(x=reorder(Enc.Rendering, -table(Enc.Rendering)[Enc.Rendering]),fill=Enc.Rendering)) + geom_bar()+coord_flip()+
  labs (x = "Provider_id", y = "Number of Providers")

x11()
ggplot(denials_report, aes(x=reorder(Category, -table(Category)[Category]),fill=Category)) + geom_bar()+
  facet_wrap(~Enc.Rendering)+coord_flip()+
  labs (x = "Category", y = "Denials Count")

x11()
ggplot(denials_report, aes(x=reorder( Sub.Category, -table( Sub.Category)[ Sub.Category]),fill= Enc.Rendering)) + geom_bar()+
  coord_flip()+
  labs (x = "Sub.Category", y = " Denials Count")

x11()
ggplot(denials_report, aes(x=reorder(Category, -table(Sub.Category)[Sub.Category]),fill=Sub.Category)) + geom_bar()+
  facet_wrap(~Enc.Rendering)+coord_flip()+
  labs (x = "Category", y = "Sub Category Wise Denials Count")+
  theme(axis.text.x = element_text(angle=90))

### Identify the Top 5 denials in Payer and CPT4
## Count wise Top 5 denials for Provider ID
# (Provider 3, 11,9,10,4)
sort(xtabs(~ Enc.Rendering,data = denials_report),decreasing = TRUE)

Top5_Providers = subset(denials_report,denials_report$Enc.Rendering %in% c("Provider 3","Provider 11","Provider 9","Provider 10","Provider 4"))

x11()
ggplot(Top5_Providers, aes(x=reorder(Category, -table(Category)[Category]),fill=Category)) + geom_bar()+
  facet_wrap(~Enc.Rendering)+coord_flip()+
  labs (x = "Category", y = "Denials Count")

x11()
ggplot(Top5_Providers, aes(x=reorder(Fin.Class, -table(Fin.Class)[Fin.Class]),fill=Enc.Rendering)) + geom_bar()+
  coord_flip()+  labs (x = "FIN.Class", y = "Denials Count")

write.csv(Top5_Providers,file = "Top5_Providers.csv",row.names = F)


### Identify the Top5 CPT4 for Count WISE
# (cpt4 36415, 99214,99213,11720,G0439)

sort(xtabs(~ CPT4,data = denials_report),decreasing = TRUE)

Top5_CPT4_CPT = subset(denials_report,denials_report$CPT4 %in% c("36415","99214","99213","11720","G0439") )

x11()
ggplot(Top5_CPT4_CPT, aes(x=reorder(Category, -table(Category)[Category]),fill=CPT4)) + geom_bar()+
  coord_flip()+  labs (x = "Category", y = "Denials Count")


write.csv(Top5_CPT4_CPT,file = "Top5_CPT4.csv",row.names = F)

## Significant tests for top 5 denials with the Payer and CPT
##Null Hypothesis :  H0 = There is No Sig. between Payer and CPT4
##Alternative Hypothesis :  H1 = There is Sig. between Payer and CPT4
## Sig level 95% with 0.05

Top5_Provider_id = read.csv("Top5_Providers.csv")
str(Top5_Provider_id)
test_Top5_Providers = table(Top5_Provider_id$Enc.Rendering,Top5_Provider_id$CPT4)
print(test_Top5_Providers)
print(chisq.test(test_Top5_Providers))
# p-value < 2.2e-16
# Conclusion : strong sig. between Provider_id and CPT

Top5_CPT4 = read.csv('Top5_CPT4.csv')
str(Top5_CPT4)
test_Top_CPT4 = table(Top5_CPT4$CPT4,Top5_CPT4$Enc.Rendering)
print(test_Top_CPT4)
print(chisq.test(test_Top_CPT4))
# p-value < 2.2e-16
# Conclusion : strong sig. between  CPT and Provider_id

### creating the Decession tree Top_2 Providers,

PID3_PID11 = subset(denials_report,denials_report$Enc.Rendering %in% c( "Provider 3","Provider 11" ))
dim(PID3_PID11)

### EDA for Data analysis for building the TREE model
x11()
ggplot(PID3_PID11, aes(x=reorder(Category, -table(Category)[Category]),fill=Category)) + geom_bar()+
  facet_wrap(~Enc.Rendering) + coord_flip()+
  labs (x = "Category", y = "Denials Count")

x11()
ggplot(PID3_PID11, aes(x=reorder(Sub.Category, -table(Sub.Category)[Sub.Category]),fill = Category)) + geom_bar() +
  facet_wrap(~Enc.Rendering) + coord_flip() +
  labs(x = "Category & SubCategory", y = "Denials Count")

x11()
ggplot(PID3_PID11, aes(x=reorder(CPT4, -table(CPT4)[CPT4]),fill = CPT4)) + geom_bar() +
  facet_wrap(~Enc.Rendering) + coord_flip() +
  labs(x = "CPT4", y = "Denials Count")

x11()
ggplot(PID3_PID11, aes(x=reorder(Fin.Class, -table(Fin.Class)[Fin.Class]),fill = Fin.Class)) + geom_bar()+
  facet_wrap(~Enc.Rendering) + coord_flip() +
  labs(x = "FinClass", y = "Denials Count")

write.csv(PID3_PID11,file="PID3_PID11.csv",row.names=F)

#### Feature engineering for Model building using (Provider 3 and Provider 11)

top2_Providers_list = read.csv('PID3_PID11.csv',header=T,sep=",",stringsAsFactors=F)
x11()
aggr_plot <- aggr(top2_Providers_list, col=c('navyblue','red'), numbers = TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram for %missing data","Pattern"))

summary(top2_Providers_list$Ded.Amt)
top2_Providers_list$Ded.Amt = NULL ### Most of the data having "0" and Blanks and no Variable description

summary(top2_Providers_list$Sv.It)
top2_Providers_list$Sv.It = NULL ### Data is Duplicated with CPT4, and No varibale description

summary(top2_Providers_list$Sv.It.Desc)
top2_Providers_list$Sv.It.Desc = NULL ### Data is Duplicated with CPT4Desc, and No varibale description

summary(top2_Providers_list$Cob1.Amt)
top2_Providers_list$Cob1.Amt = NULL ### No Variable Description, and min values is (-219) and Max is (23220)

summary(top2_Providers_list$Cob2.Amt)
top2_Providers_list$Cob2.Amt = NULL ### No Variable Description, and min values is (-11030.000) and Max is (7506.000)

summary(top2_Providers_list$Cob3.Amt)
top2_Providers_list$Cob3.Amt = NULL  ### No Variable Description, and min values is (-76.51000) and Max is (8.43000)

summary(top2_Providers_list$Bad.Debt)
top2_Providers_list$Bad.Debt = NULL ### No Variable Description all values are "Zero"

summary(top2_Providers_list$Modality)
top2_Providers_list$Modality = NULL ### No Variable Description all values are blank

summary(top2_Providers_list$Component)
top2_Providers_list$Component = NULL ### No Variable description in data, identify 3 levels in data

summary(top2_Providers_list$Rv.Cd)
top2_Providers_list$Rv.Cd = NULL  ### No Variable Description all values are blank

top2_Providers_list$First.Bill.Dt= NULL ### No Variable Description
top2_Providers_list$Lst.Bill.Dt = NULL ### No Variable Description

dimnames(top2_Providers_list)
str(top2_Providers_list)
summary(top2_Providers_list)
top2_Providers_list$Enc.Rendering = as.factor(top2_Providers_list$Enc.Rendering )
top2_Providers_list$Src.Type = as.factor(top2_Providers_list$Src.Type )
top2_Providers_list$Category = as.factor(top2_Providers_list$Category)
top2_Providers_list$Sub.Category = as.factor(top2_Providers_list$Sub.Category)
top2_Providers_list$Fin.Class = as.factor(top2_Providers_list$Fin.Class)
top2_Providers_list$Department = as.factor(top2_Providers_list$Department)
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)

### Train Control ####################

fitControl <- trainControl(method = "cv", number = 5)

################## Tree Model 1 #########################################
set.seed(1000)
rpart_model = train(Enc.Rendering~ Category +CPT4,
  data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneLength=5)
rpart_model$finalModel
x11()
rpart.plot(rpart_model$finalModel)

rpart_model_pred = predict(rpart_model,top2_Providers_list)
confusionMatrix(rpart_model_pred,top2_Providers_list$Enc.Rendering)

rpart_model
# cp          Accuracy   Kappa    
# 0.01096606  0.6912434  0.3773218

#################### Tree Model 2 ############################
set.seed(1001)
rpart_model1 = train(Enc.Rendering~ Category +CPT4+ Fin.Class ,
  data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneLength=5)

rpart_model1
rpart_model1$finalModel
x11()
rpart.plot(rpart_model1$finalModel)

# cp          Accuracy   Kappa    
# 0.01096606  0.6887331  0.3729252

######################Tree Model 3#########################

set.seed(1002)
rpart_model2 = train(Enc.Rendering~ Category +CPT4+ Fin.Class+Billed.Amt+Aprv.Amt+Rsn.Cds.Remarks,
  data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneLength=5)
rpart_model2
rpart_model2$finalModel
x11()
rpart.plot(rpart_model1$finalModel)

# cp          Accuracy   Kappa    
# 0.01427328  0.7229520  0.4398885

###################Tree Model 4########################

set.seed(1003)
rpart_model3 = train(Enc.Rendering~ Category +CPT4+ Fin.Class+Billed.Amt+Aprv.Amt+Rsn.Cds.Remarks+
    Sub.Category +Department,
  data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneLength=5)

rpart_model3
rpart_model3$finalModel
x11()
rpart.plot(rpart_model3$finalModel)
plot(rpart_model3)
varImp(rpart_model3)

# cp          Accuracy   Kappa    
# 0.01444735  0.7171551  0.4286643

#################### Tree Model 5 #######################
set.seed(1004)
rpart_model4 = train(Enc.Rendering~Billed.Amt+Aprv.Amt+Tran.Cd+Tran.Status+Rsn.Cds.Remarks+
    Category+Sub.Category+CPT4+Diag.Desc.1+Payer.Name,data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneLength=5)
rpart_model4
rpart_model4$finalModel
X11()
rpart.plot(rpart_model4$finalModel)

rpart_model4_pred = predict(rpart_model4,top2_Providers_list)
confusionMatrix(rpart_model4_pred,top2_Providers_list$Enc.Rendering)

## cp          Accuracy   Kappa    
## 0.04281984  0.8273843  0.6499181
######################################################
?trainControl
tune_grid  = expand.grid(.cp = seq(0.01,0.1,0.01))

set.seed(1005)
rpart_model5 = train(Enc.Rendering~Billed.Amt+Aprv.Amt+Tran.Cd+Tran.Status+Rsn.Cds.Remarks+
    Category+Sub.Category+CPT4+Diag.Desc.1+Payer.Name,data = top2_Providers_list,
  method = "rpart", trControl = fitControl,tuneGrid=tune_grid)

rpart_model5
rpart_model5$finalModel
varImp(rpart_model5)
rpart_model5$results
rpart_model5$metric
rpart_model5$control$index[1]
rpart_model5$finalModel$parms
rpart_model5$finalModel$variable.importance
X11()
rpart.plot(rpart_model5$finalModel)

rpart_model5_pred = predict(rpart_model5,top2_Providers_list)
confusionMatrix(rpart_model5_pred,top2_Providers_list$Enc.Rendering)

#######################################################
### Create Cube for Billing details Vs ("Enc.Rendering","Fin.Class","CPT4","Category","Sub.Category","Payer.Name")

Billed_Amt_Cube <- tapply(denials_report$Billed.Amt,
  denials_report[,c("Enc.Rendering","Fin.Class","CPT4","Category","Sub.Category")], 
  FUN=function(x){return(sum(x))})

Provider_Billed = apply(Billed_Amt_Cube, c("Enc.Rendering"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})
sort(Provider_Billed,decreasing= T)

Fin_Billed = apply(Billed_Amt_Cube, c("Fin.Class"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})
sort(Fin_Billed,decreasing= T)

CPT4_Billed = apply(Billed_Amt_Cube, c("CPT4"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})
sort(CPT4_Billed,decreasing= T)

Category_Billed = apply(Billed_Amt_Cube, c("Category"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})
sort(Category_Billed,decreasing= T)

SCategory_Billed = apply(Billed_Amt_Cube, c("Sub.Category"),
  FUN=function(x) {return(sum(x, na.rm=TRUE))})
sort(SCategory_Billed,decreasing= T)

aggregate(denials_report$Billed.Amt,by=list(denials_report$Enc.Rendering),FUN = sum)
aggregate(denials_report$Billed.Amt,by=list(denials_report$CPT4),FUN = sum)
aggregate(denials_report$Billed.Amt,by=list(denials_report$Category),FUN = sum)
aggregate(denials_report$Billed.Amt,by=list(denials_report$Sub.Category),FUN = sum)
aggregate(denials_report$Billed.Amt,by=list(denials_report$Fin.Class),FUN = sum)
aggregate(denials_report$Billed.Amt,by=list(denials_report$Payer.Name),FUN = sum)

##### End of Code #####



