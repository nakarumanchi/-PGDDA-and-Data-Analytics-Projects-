###################Geely Auto Analysis #####################################
############################################################################

### Required library's

require(DescTools)  #### For Descriptive stats
require(ggplot2)  #### Ploting the data
require(gridExtra) #### arrange the grids
require(Hmisc) ### Data Quality and missing data
require(stringr) ### Splitting the Data
require(corrplot) #### Required Correlaction Plot
require(MASS) #### for StepAIC function
require(caret) ### For model buildings
require(car) ### For model Building

### set the working dir #####

getwd()
setwd('D:\\Narendra/IIITB Upgrad/3.0 Predictive Analytics I/3.5 Assignment- Linear Regression/3.5.1 Assignment- Linear Regression/')

### Impoting data set into R memory #####

geely_auto = read.csv('CarPrice_Assignment.csv')
dim(geely_auto)
colnames(geely_auto)
head(geely_auto)

### Describe Data

str(geely_auto)
summary(geely_auto)
describe(geely_auto)

#### Data Quality #####

sum(duplicated(geely_auto$car_ID))   #### No Duplicates found Car_ID Unique data
sum(is.na(geely_auto))  ### No Missing values in Data set

### Variable wise analysis ###
### Variable symboling :
table(factor(geely_auto$symboling))
describe(geely_auto$symboling) ### -2 having only 3 Variable (0.01%)

## CarName : 
### As per the Business problem CarName varible splitted into "CarName_NEW" and "CarModel_NEW" for data understanding 

summary(factor(geely_auto$CarName)) 
geely_auto$CarName = as.character(geely_auto$CarName)
CarModel = str_split_fixed(geely_auto$CarName, "[ ]", 2)
View(CarModel)
colnames = as.data.frame(CarModel)

### Created new Variables
geely_auto$CarName_NEW = colnames$V1
geely_auto$CarModel_NEW = colnames$V2 

View(geely_auto)

table(geely_auto$CarName_NEW) ### As we observed most of the names are missmatch.

### "nissan and Nissan
geely_auto$CarName_NEW = tolower(geely_auto$CarName_NEW)

###"toyota and toyouta"

geely_auto$CarName_NEW[which(geely_auto$CarName_NEW=="toyouta")] = "toyota"

###"porcshce and porsche" 

geely_auto$CarName_NEW[which(geely_auto$CarName_NEW=="porcshce")] = "porsche"

### "vokswagen,volkswagen and vw"

geely_auto$CarName_NEW[which(geely_auto$CarName_NEW=="vokswagen")] = "volkswagen"
geely_auto$CarName_NEW[which(geely_auto$CarName_NEW=="vw")] = "volkswagen"

#### maxda renamed to mazda, Because Carmodel maxda is matched with mazda

geely_auto$CarName_NEW[which(geely_auto$CarName_NEW=="maxda")] = "mazda"

geely_auto$CarName_NEW = as.factor(geely_auto$CarName_NEW)
Desc(geely_auto$CarName_NEW)
summary(factor(geely_auto$CarName_NEW))

class(geely_auto$CarModel_NEW)
class(geely_auto$CarName_NEW)

### fueltype :
table(geely_auto$fueltype) ### No Issue

### aspiration :
table(geely_auto$aspiration)  ### No Issue

### doornumber :

table(geely_auto$doornumber) ### Need to change numbers
geely_auto$doornumber = as.character(geely_auto$doornumber)
geely_auto$doornumber[which(geely_auto$doornumber=="four")] = 4
geely_auto$doornumber[which(geely_auto$doornumber=="two")] = 2
geely_auto$doornumber = as.integer(geely_auto$doornumber)

### carbody :
Desc(geely_auto$carbody) ### No Issue

### drivewheel
Desc(geely_auto$drivewheel) ### No Issue

### enginelocation
table(geely_auto$enginelocation) ### No Issue

### wheelbase
summary(geely_auto$wheelbase)
Desc(geely_auto$wheelbase) ### Data Having Outliers

quantile(geely_auto$wheelbase,probs = seq(0,1,0.1))

## Finding the Outliers wheelbase

Q1 = quantile(geely_auto$wheelbase,probs = c(0.25))
Q3 = quantile(geely_auto$wheelbase,probs = c(0.75))
IQR = Q3-Q1
Upper_outlier = Q3+(IQR*1.5)
range(geely_auto$wheelbase)
geely_auto[geely_auto$wheelbase >Upper_outlier, ] ## Total we have 3 outliers look likes not much effect

### carlength
Desc(geely_auto$carlength) ### No Issue


### carwidth
Desc(geely_auto$carwidth) ### Data Having Outliers

### carheight
Desc(geely_auto$carheight) ### No Issue

### curbweight
Desc(geely_auto$curbweight) ### No Issue

### enginetype
Desc(geely_auto$enginetype) ### dohcv Engine type is not available, We changed "dohcv to dohv"

geely_auto$enginetype = as.character(geely_auto$enginetype)
geely_auto$enginetype[which(geely_auto$enginetype=="dohcv")] = "dohc"
geely_auto$enginetype = as.factor(geely_auto$enginetype)

#### cylindernumber
Desc(geely_auto$cylindernumber)  #### Jagure XK having 12 cylenders
geely_auto$cylindernumber=as.character(geely_auto$cylindernumber)

geely_auto$cylindernumber[which(geely_auto$cylindernumber=="four")] = 4
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="six")] = 6
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="five")] = 5
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="eight")] = 8
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="two")] = 2
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="three")] = 3
geely_auto$cylindernumber[which(geely_auto$cylindernumber=="twelve")] = 12

geely_auto$cylindernumber=as.integer(geely_auto$cylindernumber)


### enginesize
Desc(geely_auto$enginesize)  #### Data Having Outliers
range(geely_auto$enginesize)
Q1ES = quantile(geely_auto$enginesize,probs = c(0.25))
Q3ES = quantile(geely_auto$enginesize,probs = c(0.75))
IQR = Q3ES-Q1ES
Upper_outlier_ES = Q3ES+(IQR*1.5)
geely_auto[geely_auto$enginesize >Upper_outlier_ES, ] ## Total we have 10 outliers look likes not much effect

###fuelsystem
Desc(geely_auto$fuelsystem) ### No Issue

### boreratio 
Desc(geely_auto$boreratio) ### No Issue

### stroke 
Desc(geely_auto$stroke) ### Data Having Outliers

### compressionratio
Desc(geely_auto$compressionratio) ### Data Having Outlier

### horsepower
Desc(geely_auto$horsepower) ### Data Having Outlier

### peakrpm
Desc(geely_auto$peakrpm) ### Data Having Outlier

### citympg
Desc(geely_auto$citympg) ### Data Having Outlier

###highwaympg
Desc(geely_auto$highwaympg) ### Data Having Outlier

###price(Dependent variable)
Desc(geely_auto$price) ### Data Having Outlier

### EDA For all Variables numerical Variables and Categorical Variables
### Check the Pattern for Category variable and depend Variables

geely_auto_Numeric = geely_auto[,sapply(geely_auto,is.numeric)]
dim(geely_auto_Numeric)
geely_auto_Category =  geely_auto[,sapply(geely_auto,is.factor)]
dim(geely_auto_Category)
str(geely_auto_Category)

### Check the Correlaction on numbers data
### Based on the correlation Plot we are able to identify the Positive & negitive Correlated variables
### Cylindernumber,horsepower,enginesize,price,boreratio,wheelbase,carwidth,carlength,curbweight are positive Correlaction and remaining Variables are negitive corelaction
corrplot(cor(geely_auto_Numeric),method = "circle",type="full",
         outline = T,addgrid.col = "darkgray",order="hclust",
         mar = c(2,0,1,0),title = "Numeric - Correlaction") 

### Check the relation between Categorical and Price using the boxplot.

colnames(geely_auto_Category)
colnames(geely_auto_Numeric)

#### Fueltype Vs Price
P1 = ggplot(geely_auto_Category,aes(x = fueltype,y = geely_auto_Numeric$price,fill=fueltype))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Fueltype vs Price")+ theme_gray()

#### aspiration Vs Price

P2 = ggplot(geely_auto_Category,aes(x = aspiration,y = geely_auto_Numeric$price,fill=aspiration))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Aspiration vs Price")+ theme_gray()

#### carbody Vs Price

P3 = ggplot(geely_auto_Category,aes(x = carbody,y = geely_auto_Numeric$price,fill=carbody))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Carbody vs Price")+ theme_gray()

#### drivewheel Vs Price

P4 = ggplot(geely_auto_Category,aes(x = drivewheel,y = geely_auto_Numeric$price,fill=drivewheel))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Drivewheel vs Price")+ theme_gray()

#### enginelocation Vs Price

P5 = ggplot(geely_auto_Category,aes(x = enginelocation,y = geely_auto_Numeric$price,fill=enginelocation))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginelocation vs Price")+ theme_gray()

##### enginetype Vs Price

P6 = ggplot(geely_auto_Category,aes(x = enginetype,y = geely_auto_Numeric$price,fill=enginetype))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Enginetype vs Price")+ theme_gray()

#### Fuelsystem Vs Price

P7 = ggplot(geely_auto_Category,aes(x = fuelsystem,y = geely_auto_Numeric$price,fill=fuelsystem))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "Fuelsystem vs Price")+ theme_gray()

#### CarName_NEW Vs Price

P8 = ggplot(geely_auto_Category,aes(x = CarName_NEW,y = geely_auto_Numeric$price,fill=CarName_NEW))+
  geom_boxplot(outlier.colour = "red",outlier.size = 1)+scale_y_continuous(name = "Price")+
  ggtitle(label = "CarName_NEW vs Price")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

grid.arrange(P1,P2,P3,P4,top="Categorical data Vs Price Pattren-1") ### Looks Like Variables are in Different Median, So These Variables are effecting the Price Variables
grid.arrange(P5,P6,P7,P8, top="Categorical data Vs Price Pattren-2") ### Looks Like Variables are in Different Median, So These Variables are effecting the Price Variables


#### ANova Testing for Sig of Categorical Variables

Anova_1= aov(price~fueltype+aspiration+carbody+drivewheel+
               enginelocation+enginetype+fuelsystem+CarName_NEW+CarModel_NEW,
             data =geely_auto)
summary(Anova_1) ### Looks Like all variables having the Sig. But Variable "CarModel_NEW" having the Less sig.

### Create the Duplicate Data set, and remove the "CarName" Variable.

geely_auto_1 = geely_auto
geely_auto_1$CarName = NULL ### As Per the Business Problem created New Variable
geely_auto_1$CarModel_NEW = NULL ### As Per the business Problem we Ignore the Varible

# fueltype : 

summary(factor(geely_auto_1$fueltype))
levels(geely_auto_1$fueltype)<-c(1,0)
geely_auto_1$fueltype<- as.numeric(levels(geely_auto_1$fueltype))[geely_auto_1$fueltype]
View(geely_auto_1)

# aspiration:

summary(factor(geely_auto_1$aspiration))
levels(geely_auto_1$aspiration)<-c(1,0)
geely_auto_1$aspiration<- as.numeric(levels(geely_auto_1$aspiration))[geely_auto_1$aspiration]
View(geely_auto_1)

### carbody:
summary(factor(geely_auto_1$carbody))
dummy_1 <- data.frame(model.matrix( ~carbody, data = geely_auto_1))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
### Combine the Dummys and dataset
geely_auto_2 <- cbind(geely_auto_1[,-6], dummy_1)
View(geely_auto_2)

###drivewheel : 

summary(factor(geely_auto_2$drivewheel))

dummy_2 <- data.frame(model.matrix( ~drivewheel, data = geely_auto_2))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
### Combine the Dummys and dataset
geely_auto_3 <- cbind(geely_auto_2[,-6], dummy_2)
View(geely_auto_3)

### enginelocation :

summary(factor(geely_auto_3$enginelocation))
levels(geely_auto_3$enginelocation)<-c(1,0)
geely_auto_3$enginelocation<- as.numeric(levels(geely_auto_3$enginelocation))[geely_auto_3$enginelocation]

#### enginetype :

summary(factor(geely_auto_3$enginetype))

dummy_3 <- data.frame(model.matrix( ~enginetype, data = geely_auto_3))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
View(geely_auto_3)

### Combine the Dummys and dataset
geely_auto_4 <- cbind(geely_auto_3[,-12], dummy_3)
View(geely_auto_4)


### fuelsystem : 
summary(factor(geely_auto_4$fuelsystem))

dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = geely_auto_4))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
View(geely_auto_4)

### Combine the Dummys and dataset
geely_auto_5 <- cbind(geely_auto_4[,-14], dummy_4)
View(geely_auto_5)


### CarName_NEW : 
summary(factor(geely_auto_5$CarName_NEW))

dummy_5 <- data.frame(model.matrix( ~CarName_NEW, data = geely_auto_5))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
View(geely_auto_5)

### Combine the Dummys and dataset
geely_auto_6 <- cbind(geely_auto_5[,-22], dummy_5)
View(geely_auto_6)
dim(geely_auto_6)

#### Check the Correlaction of data before building the model

corrplot(cor(geely_auto_6),method = "circle",type="full",
         outline = T,addgrid.col = "darkgray",
         title = "Numeric - Correlaction") 

corr = cor(car_training)
View(corr)
write.csv(corr,file = "correlaction.csv",row.names = T)

##### Data is splitting into training and testing
set.seed(1000)
sample_data = sample(2,nrow(geely_auto_6),replace = T,prob = c(0.7,0.3))

car_training = geely_auto_6[sample_data==1,]
dim(car_training)
car_testing = geely_auto_6[sample_data==2,]
dim(car_testing)

##### Creating the regression models using StepAIC.

lm_model1 = lm(price~.,data=car_training)
summary(lm_model1)

### Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9559 

# In stepAIC function, we pass our first model i.e lm_model1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

step_lm_model1<- stepAIC(lm_model1, direction="both")
step_lm_model1

####step_lm_model1 is identify the sig variables.
### lets build the model2

lm_model2 = lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 curbweight + cylindernumber + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda + CarName_NEWisuzu + CarName_NEWmazda + 
                 CarName_NEWmercury + CarName_NEWmitsubishi + CarName_NEWnissan + 
                 CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWsaab + CarName_NEWtoyota + CarName_NEWvolkswagen + 
                 CarName_NEWvolvo, data = car_training)
summary(lm_model2)
VIF(lm_model2)
### Multiple R-squared:  0.9701,	Adjusted R-squared:  0.9605 ####
### Before we go "VIF" Checking, Here few varible is not having the Sig.
### Removed "CarName_NEWisuzu","enginetypel"

lm_model3 = lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 curbweight + cylindernumber + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcf + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmercury + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWsaab + CarName_NEWtoyota + CarName_NEWvolkswagen + 
                 CarName_NEWvolvo, data = car_training)
summary(lm_model3)

#### Multiple R-squared:  0.9684,	Adjusted R-squared:  0.9591 
#### All Variables having the Sig.
#### Let's check the Multicoliniarty (VIF)

VIF(lm_model3)

#### Below Variables are facing the Multicoliniarty Problem, But there variables are highlysig.
### car_ID,curbweight,cylindernumber,enginesize,boreratio,carbodyhatchback,
### carbodysedan,enginetypeohcf,CarName_NEWnissan,CarName_NEWplymouth,CarName_NEWtoyota,CarName_NEWvolkswagen,CarName_NEWvolvo

### As per the Correlaction car_ID is highly correlated with
    #CarName_NEWtoyota,CarName_NEWvolkswagen,CarName_NEWvolvo

### if we removed "car_ID" in Model4

lm_model4 = lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 curbweight + cylindernumber + enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcf + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmercury + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWsaab + CarName_NEWtoyota + CarName_NEWvolkswagen + 
                 CarName_NEWvolvo, data = car_training)
summary(lm_model4)

### Multiple R-squared:  0.9588,	Adjusted R-squared:  0.9471
### After removed the car_ID below variables are not having Sig.
###cylindernumber,enginetypeohcf,CarName_NEWmercury,CarName_NEWsaab,CarName_NEWvolvo

VIF(lm_model4)

#### cylindernumber having no Sig with High VIF- Remove the variable in model5

lm_model5 = lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 curbweight +  enginesize + boreratio + stroke + 
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcf + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmercury + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWsaab + CarName_NEWtoyota + CarName_NEWvolkswagen + 
                 CarName_NEWvolvo, data = car_training)
summary(lm_model5)
### Multiple R-squared:  0.9579,	Adjusted R-squared:  0.9465
VIF(lm_model5) ### most of the VIF are Controlled, Still need to check few which are in above 10

#### Below variables are not having the Sig.Remove Model-6
    ## stroke
    ## peakrpm
    ## enginetypeohcf
    ## CarName_NEWmercury
    ## CarName_NEWsaab

lm_model6 = lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 curbweight +  enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWtoyota + CarName_NEWvolkswagen + 
                 CarName_NEWvolvo, data = car_training)
summary(lm_model6)

### Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9455 --- Not much Difference
VIF(lm_model6) ### most of the variable are controlled, Still need to chedk
### curbweight ,enginesize,carbodyhatchback,carbodysedan
### Below variables are not sig. Remove model 7
    ### aspiration
    ### CarName_NEWvolvo

lm_model7 = lm(formula = price ~  enginelocation + carwidth + 
                 curbweight +  enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model7)

### Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9456  -- Not Much difference
### all Variables having the Sig...
VIF(lm_model7)
### still we are facing VIF problem on below variables
  #### curbweight
  #### enginesize
  #### carbodyhatchback
  #### carbodysedan
#### looks like curbweight and enginesize is highly correlated
#### Removed curbweight variable in model-8

lm_model8 = lm(formula = price ~  enginelocation + carwidth + 
                   enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model8)

### Multiple R-squared:  0.9479,	Adjusted R-squared:  0.9383 
VIF(lm_model8)

### For Model 8 still we are Facing on VIF problem in Below Variables
   #### carbodyhatchback
  ##### carbodysedan

#### Remove "carbodysedan" Variable

lm_model9 = lm(formula = price ~  enginelocation + carwidth + 
                 enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model9)
VIF(lm_model9)

### Multiple R-squared:  0.9425,	Adjusted R-squared:  0.9325 
#### VIF Problem is almost resolved but below variables are not having the Sig.
  ## carbodyhardtop
  ## carbodyhatchback
  ## carbodywagon, So we removed the variables in model-10.

lm_model10 = lm(formula = price ~  enginelocation + carwidth + 
                 enginesize + boreratio + enginetypeohcv + 
                 enginetyperotor + CarName_NEWaudi + CarName_NEWbmw + CarName_NEWdodge + 
                 CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                 CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                 CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model10)
VIF(lm_model10)
plot(lm_model10)
### After seeing the model_10 Plots data having some outliers, do to business problem we don't need to take any action.

### Multiple R-squared:  0.9421,	Adjusted R-squared:  0.9336 
### Still "CarName_NEWaudi" having "*" Sig. removed model-11

lm_model11 = lm(formula = price ~  enginelocation + carwidth + 
                  enginesize + boreratio + enginetypeohcv + 
                  enginetyperotor + CarName_NEWbmw + CarName_NEWdodge + 
                  CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                  CarName_NEWnissan + CarName_NEWpeugeot + CarName_NEWplymouth + CarName_NEWrenault + 
                  CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model11)
VIF(lm_model11)
plot(lm_model11)

## Multiple R-squared:  0.9402,	Adjusted R-squared:  0.932
## VIF are controlled all the VIF are >5 Looks model is good

#### CarName_NEWpeugeot and CarName_NEWplymouth having "*" Sig removed model-12

lm_model12 = lm(formula = price ~  enginelocation + carwidth + 
                  enginesize + boreratio + enginetypeohcv + 
                  enginetyperotor + CarName_NEWbmw + CarName_NEWdodge + 
                  CarName_NEWhonda  + CarName_NEWmazda + CarName_NEWmitsubishi + 
                  CarName_NEWnissan + CarName_NEWrenault + 
                  CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model12)
VIF(lm_model12)
plot(lm_model11)
## Multiple R-squared:  0.9345,	Adjusted R-squared:  0.9267 
### Below variables are having "*" Sig removed model-13
### CarName_NEWdodge
### CarName_NEWhonda


lm_model13 = lm(formula = price ~  enginelocation + carwidth + 
                  enginesize + boreratio + enginetypeohcv + 
                  enginetyperotor + CarName_NEWbmw +  
                  CarName_NEWmazda + CarName_NEWmitsubishi + 
                  CarName_NEWnissan + CarName_NEWrenault + 
                  CarName_NEWtoyota + CarName_NEWvolkswagen,data = car_training)
summary(lm_model13)
VIF(lm_model13)
plot(lm_model13)

##### looks like from LM_Model_10 to lm_model_13 is good and VIF are under Controlled

## lm_model10 - Multiple R-squared:  0.9421,	Adjusted R-squared:  0.9336 with 18 Variables
## lm_model11 - Multiple R-squared:  0.9402,	Adjusted R-squared:  0.932  with 17 Variables
## lm_model12 - Multiple R-squared:  0.9345,	Adjusted R-squared:  0.9267 with 15 Variables
## lm_model13 - Multiple R-squared:  0.9304,	Adjusted R-squared:  0.9233 with 13 Variables

#### Predicting the testdat

car_testing$prediction = predict(lm_model13,car_testing)

lm_10R = cor(car_testing$price,car_testing$prediction) ###  0.9350736
lm_10AR = lm_10R^2

plot(car_testing$price,type = "l",col="red")
lines(car_testing$prediction ,type = "l",col="blue") #### Looks plots Prediction Plots Good


#### Do to small data we are going to take Bootstarp sample for better accuracy 
### Will run the below model using Complete data set.

require(caret)
ctrl = trainControl(method = "boot",number = 3)
set.seed(10000)
regression_1 = train(price ~  enginelocation + carwidth + 
                       enginesize + boreratio + enginetypeohcv + 
                       enginetyperotor + CarName_NEWbmw +  
                       CarName_NEWmazda + CarName_NEWmitsubishi + 
                       CarName_NEWnissan + CarName_NEWrenault + 
                       CarName_NEWtoyota + CarName_NEWvolkswagen, data = geely_auto_6,
                     method="lm",trControl=ctrl)
summary(regression_1)
geely_auto_6$pred = predict(regression_1,newdata = geely_auto_6)

plot(geely_auto_6$price,type = "l",col="red")
lines(geely_auto_6$pred, type = "l",col="blue")

regression_2 = train(price ~  enginelocation + carwidth + 
                       enginesize + boreratio + enginetypeohcv + 
                       enginetyperotor + CarName_NEWbmw +  
                       CarName_NEWmazda + CarName_NEWmitsubishi + 
                       CarName_NEWnissan + CarName_NEWrenault + 
                       CarName_NEWtoyota + CarName_NEWvolkswagen, data = geely_auto_6,
                     method="lm",trControl=ctrl)
summary(regression_2)
geely_auto_6$pred = predict(regression_2,newdata = geely_auto_6)

plot(geely_auto_6$price,type = "l",col="red")
lines(geely_auto_6$pred, type = "l",col="blue")

APE = abs(geely_auto_6$price-geely_auto_6$pred)/geely_auto_6$price
mean(APE)

##### Model Results Summary ########

#Verifying the multiple models,
#we Identified the following variables are most significant variables to predict the Car Price in US market.
    ### enginelocation
    ###carwidth 
    ###enginesize
    ###boreratio 
    ###enginetypeohcv
    ###enginetyperotor 
    ###CarName_NEWbmw  
    ###CarName_NEWmazda 
    ###CarName_NEWmitsubish
    ###CarName_NEWnissan 
    ###CarName_NEWrenault 
    ###CarName_NEWtoyota 
    ###CarName_NEWvolkswagen

### As we observed Model 10 to Model 13 R^2 and Adjusted R^2 we are Getting above 90% accuracy in Training data
### Same we Predit in the Test data and getting approximately 13% Error.... 


## Plese note we are not taken any decision in Outliers. based on the domine we feel no needto take any action in Outliers.
## If we take the action in Outliers may model accuracy will changed


##### End of Model Summary ######

































