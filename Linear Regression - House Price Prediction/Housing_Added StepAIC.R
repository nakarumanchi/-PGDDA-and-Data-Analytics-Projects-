##################Linear Regression Model  #############################
########################################################################

require(ggplot2)
require(car)
require(MASS)
require(DescTools)
require(gridExtra)
require(caret)

setwd("D:/IIITB/Regression/Linear Regression - House Price Prediction")

housing <- read.csv("Housing.csv",stringsAsFactors = T)

View(housing)

# datadictonary
# price represents the sale price of a house in Rs.
# area gives the total size of a property in square feet
# bedrooms represents the number of bedrooms
# bathrooms shows the number of bathrooms
# stories variable shows the number of stories excluding basement
# mainroad =1 if the house faces a main road
# livingroom   = 1 if the house has a separate living room or a drawing room for guests
# basement shows if the house has a basement
# hotwaterheating  = 1 if the house uses gas for hot water heating
# airconditioning    = 1 if there is central air conditioning
# parking shoes the number of cars that can be parked
# prefarea is 1 if the house is located in the preferred neighbourhood of the city


#Let us examine the structure of the dataset
str(housing)

### Describe data
colnames(housing)
Desc(housing$price,main = "Price Distrubution")
Desc(housing$area,main = "area Distrubution") ###### Data having Outliers and looks it's Right Skewed
Desc(factor(housing$bedrooms),main = "bedrooms Distrubution") #### Most of the Level is "3"
Desc(factor(housing$bathrooms),main = "bathrooms Distrubution")#### Most of the Level is "1"
Desc(factor(housing$stories),main = "stories Distrubution")#### Most of the Level is "2"
Desc(housing$mainroad,main = "mainroad Distrubution")### 80% of Houses having "YES-Main Road available"
Desc(housing$basement,main = "basement Distrubution")### 80% of Houses having "NO Basement"
Desc(housing$guestroom,main = "guestroom Distrubution")### 80% of Houses having "NO Guest Room"
Desc(housing$hotwaterheating,main = "hotwaterheating Distrubution")### above 90% of Houses having "NO hotwaterheating"
Desc(housing$airconditioning,main = "airconditioning Distrubution")### Nearly 70% of Houses having "NO airconditioning"
Desc(factor(housing$parking),main = "parking Distrubution")### Most of the Houses are "0 No Car parking"
Desc(housing$prefarea,main = "prefarea Distrubution")### above 70% Prefarea area is NO
Desc(factor(housing$furnishingstatus),main = "furnishingstatus Distrubution") ### 45% PLOTS semi-furnished


#### EDA to find out Pattrens based on the target variables

B1 = ggplot(data = housing,aes(x = area ,y = price))+geom_point(col="blue")+
  ggtitle("are wise Price distrubution")

B2= ggplot(data = housing,aes(x = factor(bedrooms) ,y = price,fill=factor(bedrooms)))+
  geom_boxplot(outlier.colour = "red")+xlab("Bedroom")+
  ggtitle("Bedrooms wise Price")+guides(fill=FALSE)

B3 = ggplot(data = housing,aes(x = factor(bathrooms) ,y = price,fill=factor(bathrooms)))+
  geom_boxplot(outlier.colour = "red")+xlab("bathrooms")+
  ggtitle("bathrooms wise Price distrubution")+guides(fill=FALSE)

B4 = ggplot(data = housing,aes(x = factor(stories) ,y = price,fill=factor(stories)))+
  geom_boxplot(outlier.colour = "red")+xlab("stories")+
  ggtitle("stories wise Price distrubution")+guides(fill=FALSE)

B5 = ggplot(data = housing,aes(x = factor(mainroad) ,y = price,fill=factor(mainroad)))+
  geom_boxplot(outlier.colour = "red")+xlab("mainroad")+
  ggtitle("mainroad wise Price distrubution")+guides(fill=FALSE)

B6 = ggplot(data = housing,aes(x = factor(basement) ,y = price,fill=factor(basement)))+
  geom_boxplot(outlier.colour = "red")+xlab("basement")+
  ggtitle("basement wise Price distrubution")+guides(fill=FALSE)

B7 = ggplot(data = housing,aes(x = factor(guestroom) ,y = price,fill=factor(guestroom)))+
  geom_boxplot(outlier.colour = "red")+xlab("guestroom")+
  ggtitle("guestroom wise Price distrubution")+guides(fill=FALSE)

B8 = ggplot(data = housing,aes(x = factor(hotwaterheating) ,y = price,fill=factor(hotwaterheating)))+
  geom_boxplot(outlier.colour = "red")+xlab("hotwaterheating")+
  ggtitle("hotwaterheating wise Price distrubution")+guides(fill=FALSE)

B9 = ggplot(data = housing,aes(x = factor(airconditioning) ,y = price,fill=factor(airconditioning)))+
  geom_boxplot(outlier.colour = "red")+xlab("airconditioning")+
  ggtitle("airconditioning wise Price distrubution")+guides(fill=FALSE)

B10 = ggplot(data = housing,aes(x = factor(parking) ,y = price,fill=factor(parking)))+
  geom_boxplot(outlier.colour = "red")+xlab("parking")+
  ggtitle("parking wise Price distrubution")+guides(fill=FALSE)

B11 = ggplot(data = housing,aes(x = factor(prefarea) ,y = price,fill=factor(prefarea)))+
  geom_boxplot(outlier.colour = "red")+xlab("prefarea")+
  ggtitle("prefarea wise Price distrubution")

B12 = ggplot(data = housing,aes(x = factor(furnishingstatus) ,y = price,fill=factor(furnishingstatus)))+
  geom_boxplot(outlier.colour = "red")+xlab("furnishingstatus")+
  ggtitle("furnishingstatus wise Price distrubution")+guides(fill=FALSE)

grid.arrange(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,
             top="Price Distrubution of all Variables") ### Most of the Variables median is Different, all variables are important


#DUMMY VARIABLE CREATION. 
#Let us see the structure of variable "mainroad".
str(housing$mainroad)
summary(factor(housing$mainroad))

# One simple way to convert mainroad variable to numeric is to replace the levels- Yes's and Nos's with 1 and 0 is:
levels(housing$mainroad)<-c(1,0)

# Now store the numeric values in the same variable
housing$mainroad<- as.numeric(levels(housing$mainroad))[housing$mainroad]

# Check the summary of mainroad variable
summary(housing$mainroad)

# Do the same for other such categorical variables
levels(housing$guestroom)<-c(1,0)
housing$guestroom <- as.numeric(levels(housing$guestroom))[housing$guestroom]

levels(housing$basement)<-c(1,0)
housing$basement <- as.numeric(levels(housing$basement))[housing$basement]

levels(housing$hotwaterheating)<-c(1,0) 
housing$hotwaterheating <- as.numeric(levels(housing$hotwaterheating))[housing$hotwaterheating]

levels(housing$airconditioning)<-c(1,0)
housing$airconditioning <- as.numeric(levels(housing$airconditioning))[housing$airconditioning]

levels(housing$prefarea)<-c(1,0)
housing$prefarea <- as.numeric(levels(housing$prefarea))[housing$prefarea]


# Now we come across variables having more than 3 levels. 
summary(factor(housing$furnishingstatus))

#Converting "furnishingstatus" into dummies . 
dummy_1 <- data.frame(model.matrix( ~furnishingstatus, data = housing))

#check the dummy_1 data frame.
View(dummy_1)

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
housing_1 <- cbind(housing[,-13], dummy_1)
View(housing_1)

######## Based on the requirement created the derived Matrix ######
# Let us create the new metric and assign it to "areaperbedroom"

housing_1$areaperbedroom <- housing_1$area/housing_1$bedrooms

# metric - bathrooms per bedroom
housing_1$bbratio <- housing_1$bathrooms/housing_1$bedrooms

###### Divide into training and test data set (70% and 30%) #########
#set the seed to 100, let's run it 

set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(housing_1), 0.7*nrow(housing_1))
# generate the train data set
train = housing_1[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = housing_1[-trainindices,]



##############

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1) 

### R-squared:  0.6878,	Adjusted R-squared:  0.675

# Check if the correlation matrix , and identify the independent Variable relation
corrs = cor(housing_1)
View(corrs)


###### #For the calculation of VIF you need to install the package "car",  ########

vif(model_1)

 
# remove bbratio variable based on High VIF and insignificance (p>0.05)
# Make a new model without bbratio variable

model_2 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished + areaperbedroom, data = train)


# check the accuracy of this model
summary(model_2)

# Repeat the process of vif calculation of model_2. 
vif(model_2)


# Remove the areaperbedroom variable based on high VIF and insignificance (p>0.05)
# Make a model without areaperbedroom variable
model_3 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished , data = train)

#Check the accuracy of this model
summary(model_3)

# Calculate the vif for model_3. 
vif(model_3)
# all VIFs are below 2, thus Multicollinearity is not a problem anymore


# Remove furnishingstatussemi.furnishingstatus based on insignificance. It has has the highest p value of 0.53 among the remaining variables
# Make a model without areaperbedroom variable

model_4 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

# Check the accuracy and p-values again
summary(model_4)


# bedrooms variable has a p value of 0.22, thus is insignificant
## Make a new model after removing bedrooms variable 

model_5 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_5)


# basement variable is having the highest p-value (lowest significance) among the remaining variables.
# Make a new model after removing basement variable 

model_6 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_6)


# mainroad variable has the highest p-value among the other variables in the model
# Make a new model after removing mainroad variable 

model_7 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
     hotwaterheating + airconditioning + parking + prefarea + 
       furnishingstatusunfurnished , data = train)

# Check the adjusted R-squared and p-values again
summary(model_7)


# hotwater heating variable is having the highest p-value among the other variables in the model, 
# Make a new model after removing hotwaterheating variable 

model_8 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
                airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

#Check the accuracy
summary(model_8)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_8,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared


### To identify the House Price Below Variables are importent
#area
#bathrooms
#stories
#guestroom
#airconditioning
#parking
#prefarea
#furnishingstatusunfurnished



#### Please note for this Casestudy we are following the Backward regression to identify the Sig.. Variable 
#### insted of Doing this We need to use StepAIC will get the same output.... See below code


step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables - Bedrooms, furnishingstatussemi.furnished, bbratio, 

# Let's execute this model here, 
model_2 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                guestroom + basement + hotwaterheating + airconditioning + 
                parking + prefarea + furnishingstatusunfurnished + areaperbedroom, 
              data = train)
# Let us look at the summary of the model
summary(model_2)


## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
vif(model_2)

# area has a VIF of 4.20 and areaperbedroom has a VIF of 4.09
# Let us check now their p values
# You can see that are variable has a very low p value, while areaperbedroom has a VIF of 0.14,
# and thus, we can remove areaperbedroom variable and run the model again

model_3 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                guestroom + basement + hotwaterheating + airconditioning + 
                parking + prefarea + furnishingstatusunfurnished, 
              data = train)

# Let us look at the VIFs now
vif(model_3)
# All variables have a VIF below 2
# Let us check now for variables which have high p values
summary(model_3)

# Although, all variables have a p value below 0.05, the number of variables is still too large.
# You could continue removing the variables till the significance level is 0.001
# Try the rest yourself and compare the results of the two, this one and the previous model.

model_4 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                guestroom + hotwaterheating + airconditioning + 
                parking + prefarea + furnishingstatusunfurnished, 
              data = train)
summary(model_4)

# Remove mainroad


model_4 <- lm(formula = price ~ area + bathrooms + stories + 
                guestroom + hotwaterheating + airconditioning + 
                parking + prefarea + furnishingstatusunfurnished, 
              data = train)
summary(model_4)

# Remove hotwaterheating
model_5 <- lm(formula = price ~ area + bathrooms + stories + 
                guestroom + airconditioning + 
                parking + prefarea + furnishingstatusunfurnished, 
              data = train)
summary(model_5)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_5,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared