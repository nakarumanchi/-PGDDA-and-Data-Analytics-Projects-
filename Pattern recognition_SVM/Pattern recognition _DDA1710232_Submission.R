##### pattern recognition #####################
#===============================================

### Approach followed as per CRISP-DM methology ######

# 1. Business Understanding
#### A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
#### Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
#### The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

# 2. Data Understanding
  #### Data is collected from mnist database (mnist_training and mnist test)
  #### Over 60,000 data points and 785 Variables in traing and 10,000 data points and 785 Variables in mnist_test data
  #### The Range of the data is 0-255, and target Variable having 10 class (0-9)
  #### As we observed data set having no lables (No Headers), while importing we given the condiation it's automataclly taken(X1 to X785)
  #### For the Target variable, updated the Coloum Name as "Label"
  #### No Missing Values in mnist data

# 3. Data Preparation
  #### For doing the Preprocess, we merge the mnist_training , mnist_test using rbind() function
  #### after Merging the data we Run the PCA (Principal Component Analysis) to reduce variables
  #### we Comparing the multiple PCA, final we fix at 60% Variance with 64 Components
  #### segrate the the for traing and test

# 4. Model Building

    ### 4.1 Ksvm Model using C=1---- Model is given good Accuracy
    ### 4.2 Ksvm Model using C=10--- Model is given good Accuracy compare to 4.1

### 4.3 using Crossfold validation method = "SvmLiner" 
    ### 4.3.1 Using the crossvalidation and captured 5 folds
    ### 4.3.2 Using the crossvalidation with Tunelength 5

### 4.4 Using the Crossvalidation Method  = "SvmRadial"
    ### 4.4.1 Using the Crossfold Validation
    ### 4.4.2 Cross Fold Validation with TuneLength =5
    ### 4.4.3 Removed the TuneLength and add the tuneGrid with C = 1 to 5  
    ### 4.4.4 Removed the Tune Grid and added TuneLength =6
    ### 4.4.5 Removed the TuneLength and add the tuneGrid with C = 1 to 10

# 5. Model Evaluation
      ### Using the Confusion matrix to evaluate the model Accuracy

# 6. Model Deployment
### We feel that, Our Testing data is a unseen data and final model will deployed to testdata

##### Require package ###########

require(readr) ### Importing the files in R
require(DescTools)#### Check the distrubution of data
require(caret)#### For Preprocess and algortham
require(corrplot) ### For Correlation
library(doParallel) #### For Parallel Process
require(ggplot2) #### For Ploting the data
require(kernlab) ### For SVM model building


#### set the working directory ######
getwd()
setwd("D:/Narendra/IIITB Upgrad/4.0 Predictive Analytics II/4.3 Support Vector Machines - Assignment")


#### Importing the mnist (training and test) data base to R enronment
### Importing Training_data

mnist_training = read_csv('mnist_train.csv',col_names = FALSE,na="NA")
View(mnist_training)
dim(mnist_training)  #### 60000 rows and 785 coloums
str(mnist_training)

#### Importing Test_data

mnist_test = read_csv('mnist_test.csv',col_names = FALSE,na="NA")
View(mnist_test)
dim(mnist_test)  #### 10000 rows and 785 coloums
str(mnist_test)

### Check the target variables, all the levels are matched (or) not
summary(factor(mnist_training$X1)) ### Total 10 classes in the data (0-9)
summary(factor(mnist_test$X1)) ### ### Total 10 classes in the data (0-9)

### Check the Distrubution of Target Variable

ggplot(mnist_training,aes(x = factor(X1),fill=factor(X1)))+geom_bar()+
  coord_flip() #### Most of the classes are proportionate and having the 0-9 classes
ggplot(mnist_test,aes(x = factor(X1),fill=factor(X1)))+geom_bar()+
  coord_flip() #### Most of the classes are proportionate looks like same as Training and having 0-9 classes

#### Combine the training & test data sets for data Quality checking and Process purose

mnist_Merging = rbind(mnist_training,mnist_test)
dim(mnist_Merging) ### 70000 rows and 785 coloums

##### Check the summary of data #####

summary(mnist_Merging_1[,1:50]) 
summary(mnist_Merging_1[,51:100])
summary(mnist_Merging_1[,101:150])
summary(mnist_Merging_1[,151:200])
summary(mnist_Merging_1[,201:250])
summary(mnist_Merging_1[,251:300])
summary(mnist_Merging_1[,301:350])
summary(mnist_Merging_1[,351:400])
summary(mnist_Merging_1[,401:450])
summary(mnist_Merging_1[,451:500])
summary(mnist_Merging_1[,501:550])
summary(mnist_Merging_1[,551:600])
summary(mnist_Merging_1[,601:650])
summary(mnist_Merging_1[,651:750])
summary(mnist_Merging_1[,751:784])#### The range of the data is 0 to 255

##### Data Quality checking ####

# Headers names :-
### in mnist data "header names " not available..
### while importing defined all  variables as X1 to X785 both training and test
### Changed the Target Variables as "lable" for both training and test

colnames(mnist_Merging)[1] = "lable"

# Missing Data :-

colSums(is.na(mnist_Merging)) #### no Missing Values
sum(is.na(mnist_Merging)) #### No Missing Values

# Create the non lable (Removed the target variable) dataset  for Analysis

mnist_Merging_1 = mnist_Merging[,-1]
dim(mnist_Merging_1)

#### Distrubution of Target variable

Desc(factor(mnist_Merging$lable)) ### after Merging the Target variable Freq..


### in 785 Variable to do the Analysis is heavy 
### So reduce the dimension will go for PCA Analysis (or) Factor Analysis

##############################################################
### Factor Analysis using "factanal()" 
### Looks like Factor Analysis not working this data set due to more coloums..
    ### 1. trying to check the Correlaction to decide number of Factors it's generates "NA"
    ### 2. And try to Run the factor Analysis, it's data having some "NA" values
          # Here Factor Analysis works based on the Correlaction and Corelaction is generates "NA"
    ### 3. Before Factor Analysis, try to scale the data we will get "NAN" in data.
    ### 4. After removing the zero variance feature, it's Getting some system memory error, 
    ###  5.we try to do the analysis based on the error, that say's Factor Analysis not works properly on more the 100 Variable
##############################################################

#### Principal Component Analysis (PCA) using Caret Package
#### Before going to PCA need to remove the Zero Variance features
### check the Variance of features & "near zero variance"
### For this step we eliminate the feature which are in "Zerovariance"

var_obj = nearZeroVar(mnist_Merging_1,saveMetrics=TRUE, allowParallel = T)
var_obj$freqRatio
var_obj$percentUnique
var_obj$zeroVar
var_obj$nzv

table(var_obj$zeroVar)##### 65 Features are available in Zero Variance

mnist_Merging_2 = mnist_Merging_1[ ,var_obj$zeroVar==FALSE]

#### After elimaniting the "Zero Variance features" nearly 65 features are elimanated directly

dim(mnist_Merging_2) ### total 719 Coloums

#### implementing the PCA on "mnist_Merging_1"
#### When we Implemented the PCA using Preprocess finction in caret libreay it's defualt Scale and center the data

#### PCA With Cumalative Variance as 0.95%
pca_obj_1 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.95)
pca_obj_1 ### PCA needed 332 components to capture 95 percent of the variance

#### PCA With Cumalative Variance as 0.90%
pca_obj_2 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.90)
pca_obj_2 ### PCA needed 238 components to capture 90 percent of the variance

#### PCA With Cumalative Variance as 0.85%
pca_obj_3 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.85)
pca_obj_3 ### PCA needed 186 components to capture 85 percent of the variance


#### PCA With Cumalative Variance as 0.80%
pca_obj_4 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.80)
pca_obj_4 ### PCA needed 150 components to capture 80 percent of the variance

#### PCA With Cumalative Variance as 0.75%
pca_obj_5 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.75)
pca_obj_5 ### PCA needed 121 components to capture 75 percent of the variance

#### PCA With Cumalative Variance as 0.70%
pca_obj_6 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.70)
pca_obj_6 ### PCA needed 99 components to capture 70 percent of the variance

#### PCA With Cumalative Variance as 0.65%
pca_obj_7 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.65)
pca_obj_7 ### PCA needed 80 components to capture 65 percent of the variance

#### PCA With Cumalative Variance as 0.60%
pca_obj_8 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.60)
pca_obj_8 ### PCA needed 64 components to capture 60 percent of the variance

#### PCA With Cumalative Variance as 0.55%
pca_obj_9 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.55)
pca_obj_9 ###PCA needed 51 components to capture 55 percent of the variance

#### PCA With Cumalative Variance as 0.50%
pca_obj_10 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.50)
pca_obj_10 ###PCA needed XX components to capture 50 percent of the variance

### PCA -- 0.95 Variance ---332 components  58% (332) Variable reduced if we capture 95% of Variables
### PCA -- 0.90 Variance ---238 components  70% (238) Variables reduced if we capture the 90% of Variables
### PCA -- 0.85 Variance ---186 components  76% (186) Variables reduced if we capture the 85% of Variables
### PCA -- 0.80 Variance ---150 components  81% (150) Variables reduced if we capture the 80% of Variables
### PCA -- 0.75 Variance ---121 components  85% (121) Variables reduced if we capture the 75% of Variables
### PCA -- 0.70 Variance ---99 components   87% (99) Variables reduced if we capture the 70% of Variables
### PCA -- 0.65 Variance ---80 components   90% (80) Variables reduced if we capture the 65% of Variables
### PCA -- 0.60 Variance ---64 components   92% (64) Variables reduced if we capture the 60%  of Variables
### PCA -- 0.55 Variance ---51 components   93% (55) Variables reduced if we capture the 55% of Variables
### PCA -- 0.50 Variance ---39 components   95% (51) Variables reduced if we capture the 55% of Variables

#### looks like 65% and 60% Variance has not much difference 
#### Captured the minimam 60% Variance for predicting PCA, it will get 64 PCA

mnist_Merging_PCA_60 = predict(pca_obj_8,mnist_Merging_2)
dim(mnist_Merging_PCA_60)
View(mnist_Merging_PCA_60)
summary(mnist_Merging_PCA_60)

#### Merge the target coloum into "mnist_Merging_PCA_65"

mnist_Merging_PCA_60$label = mnist_Merging$lable

#### Change the target variables as Factors
mnist_Merging_PCA_60$label = as.factor(mnist_Merging_PCA_60$label)

#### Devide the data into Original mnist_train ad mnist_test 

mnist_training_1 = mnist_Merging_PCA_60[1:60000, ]
mnist_test_1 = mnist_Merging_PCA_60[60001:nrow(mnist_Merging_PCA_60), ]
dim(mnist_training_1) ### After PCA 60000(data points) X 65 (Variables include Target)
dim(mnist_test_1) #### After PCA 10000(datapoints) X 65 (Variables include Target)

### Model building using Support vector machines
#### Devide the data into training & validation
set.seed(1000)
mnist_training_1_sample = sample(2,nrow(mnist_training_1),replace = T,prob = c(0.7,0.3))
mnist_train =mnist_training_1[mnist_training_1_sample==1, ]
mnist_validation = mnist_training_1[mnist_training_1_sample==2, ]

dim(mnist_train)
dim(mnist_validation)


#### build the basic SVM model
#### if we are not given C value it's takes default 1 ##########################

kvsm_model_1<- ksvm(label ~ ., data = mnist_train)
kvsm_model_1
summary(kvsm_model_1)

pred_1 = predict(kvsm_model_1,mnist_validation)
confusionMatrix(pred_1,mnist_validation$label) #### Validation Accuracy : 0.9637  

### When we predit the Ksvm_model1, in Validation data Accuracy is 0.9637
### looks like sensitivy and specitivity also good most of the class
### Class 3 , Class 5 , Class 7 , Class 8 and Class 9 minor difference in sencitivy and specificity.

### Predit the data in mnist_test_1

mnist_test_1$pred_1 = predict(kvsm_model_1,mnist_test_1)
confusionMatrix(mnist_test_1$pred_1,mnist_test_1$label) #### Testing Accuracy : 0.9621

#### Predict the test data Accuracy is good at 0.9621  
### looks like sensitivy and specitivity also good most of the class

########## Give the condiation C=10 in same model #############################
kvsm_model_2<- ksvm(label ~ ., data = mnist_train,C=10)
summary(kvsm_model_2)
kvsm_model_2

pred_2 = predict(kvsm_model_2,mnist_validation)
confusionMatrix(mnist_validation$label,pred_2) #### Validation Accuracy : 0.9736 

### When we predit the Ksvm_model2, in Validation data Accuracy is 0.9736
### looks like sensitivy and specitivity also good most of the class

### Predit the data in mnist_test_1

mnist_test_1$pred_2 = predict(kvsm_model_2,mnist_test_1)
confusionMatrix(mnist_test_1$pred_2,mnist_test_1$label) #### testing Accuracy : 0.9715 


### When we predit the Ksvm_model2, in test data Accuracy is 0.9715
### looks like sensitivy and specitivity also good most of the class
#########################################################################################

#register cluster for parallel processing to reduce the model running time
### Parallelisam will work, it's captured all the momery and build the models Parallely

cl = makeCluster(detectCores())
registerDoParallel(cl)

###### Go for the cross validation Using SVM_Linear Algortham

tr_control = trainControl(method = "cv",number = 5)

#### Basic model with train control methods ###########################################

SVM_liner_1 = train(label ~ ., data = mnist_train,method="svmLinear",trControl=tr_control)
SVM_liner_1
SVM_liner_1$finalModel

#Accuracy   Kappa        C=1
#0.9296799  0.9218348  ## looks like it's moderate accuracy given
### Predicting the data in Validation Data

pred_3 = predict(SVM_liner_1 , mnist_validation)
confusionMatrix(pred_3,mnist_validation$label) ## Validation Accuracy : 0.9328 

### Predicting the test data
mnist_test_1$pred_3 = predict(SVM_liner_1 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_3,mnist_test_1$label) ## test Accuracy : 0.9353 
#######################################################################################
### Added the condiation as best 5 tunes

SVM_liner_2 = train(label~., data = mnist_train,method="svmLinear",
                    trcontrol=tr_control,tuneLength=5)
summary(SVM_liner_2)
SVM_liner_2

#Accuracy   Kappa    # C=1
#0.9269887  0.9188408 ### Looks like it's moderate Accuracy given

## Predicting the Validation data
pred_4 = predict(SVM_liner_2 , mnist_validation)
confusionMatrix(pred_4,mnist_validation$label) ## Validation Accuracy : 0.9328 

### Predicting the test data
mnist_test_1$pred_4 = predict(SVM_liner_2 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_4,mnist_test_1$label) ## test Accuracy : 0.9353 

# Class 8 , Class 5 , Sensitivity and  Specificity high difference

############################################################################################

#### Using the Cross Validation with SVMLinear algortham,it's not crossed the Accuracy on KSVM in normal model
#### Will try to svmRadial algotham for imporve the accuracy ############################

svmRadial_1 = train(label ~ ., data = mnist_train,method="svmRadial",trControl=tr_control)
svmRadial_1
svmRadial_1$finalModel
svmRadial_1$bestTune

# The final values used for the model were sigma = .01425389 and C = 1.
# C     Accuracy   Kappa    
# 1.00  0.9597187  0.9552259

### Predicting the Validation data
pred_5 = predict(svmRadial_1 , mnist_validation)
confusionMatrix(pred_5,mnist_validation$label) ## Validation Accuracy : 0.9637 

### Predicting the test data
mnist_test_1$pred_5 = predict(svmRadial_1 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_5,mnist_test_1$label) ## test Accuracy : 0.9621

### Looks like Model is Good, Not Overfitted.

#############################################################################################
#### Added the Condiation as TuneLength =5 , this will be find out best 5 

svmRadial_2 = train(label ~ ., data = mnist_train,method="svmRadial",
                    trControl=tr_control,tuneLength=5)
svmRadial_2$bestTune
svmRadial_2$finalModel

#C     Accuracy   Kappa    
#4.00  0.9679648  0.9643917
# The final values used for the model were sigma = 0.01441683 and C = 4.

### Predicting the Validation data
pred_6 = predict(svmRadial_2 , mnist_validation)
confusionMatrix(pred_6,mnist_validation$label) ## Validation Accuracy : 0.9714 

### Predicting the test data
mnist_test_1$pred_6 = predict(svmRadial_2 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_6,mnist_test_1$label) ## test Accuracy : 0.97 

### Looks like model works well on training , Validation and test datasets.

###########################################################################################

#### Added the tune grid to tune the algorthim

t_grid = expand.grid(.sigma = 0.01425389,.C = seq(1,5,1))
svmRadial_3 = train(label ~ ., data = mnist_train,method="svmRadial",
                    trControl=tr_control,tuneGrid=t_grid)
svmRadial_3$bestTune
svmRadial_3$finalModel

#C  Accuracy   Kappa
#5  0.9687493  0.9652638
# The final values used for the model were sigma = 0.01425389 and C = 5


### Predicting the Validation data
pred_7 = predict(svmRadial_3 , mnist_validation)
confusionMatrix(pred_7,mnist_validation$label) ## Validation Accuracy : 0.9722 

### Predicting the test data
mnist_test_1$pred_7 = predict(svmRadial_3 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_7,mnist_test_1$label) ## test Accuracy : 0.9703 

### Looks like model works well on training , Validation and test datasets.


############################################################################################
#### Added the tune length as tuneLength "6" to find out the best 6

svmRadial_4 = train(label ~ ., data = mnist_train,method="svmRadial",
                    trControl=tr_control,tuneLength=6)
svmRadial_4
svmRadial_4$bestTune
svmRadial_4$finalModel
#   C     Accuracy   Kappa 
#  8.00  0.9690586  0.9656074
# The final values used for the model were sigma =0.01449617 and C = 8.

### Predicting the Validation data
pred_8 = predict(svmRadial_4 , mnist_validation)
confusionMatrix(pred_8,mnist_validation$label) ## Validation Accuracy :  0.9738 

### Predicting the test data
mnist_test_1$pred_8 = predict(svmRadial_4 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_8,mnist_test_1$label) ## test Accuracy : 0.9713  

### Looks like model works well on training , Validation and test datasets.

###################################################################################

#### Added the tune grid, for C Values as 1 to 10 with sigma of 0.014
t_grid_1 = expand.grid(.sigma = 0.01425389,.C = seq(1,10,1))

svmRadial_5 = train(label ~ ., data = mnist_train,method="svmRadial",
                    trControl=tr_control,tuneGrid=t_grid_1)
svmRadial_5
svmRadial_5$finalModel
svmRadial_5$bestTune

# The final values used for the model were sigma = 0.01425389 and C = 9.
#  C   Accuracy   Kappa 
#  9  0.9705081  0.9672186

### Predicting the Validation data
pred_9 = predict(svmRadial_5 , mnist_validation)
confusionMatrix(pred_9,mnist_validation$label) ## Validation Accuracy :  0.9738 

### Predicting the test data
mnist_test_1$pred_9 = predict(svmRadial_5 , mnist_test_1)
confusionMatrix(mnist_test_1$pred_9,mnist_test_1$label) ## test Accuracy : 0.971

### Looks like model works well on training , Validation and test datasets.
#### Consider the final Model as "svmRadial_4" it works good in all unseen database

stopCluster(cl)

########################END OF CODE ################################################









