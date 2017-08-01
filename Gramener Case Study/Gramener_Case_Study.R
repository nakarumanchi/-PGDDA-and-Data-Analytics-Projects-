###################################################################################################
#################################### Gramener Case Study Solution #################################
###################################################################################################
##### Business Understanding 

# Business model of lending club: https://en.wikipedia.org/wiki/Lending_Club

# Types of products - Credit card, debt consolidation, etc. (https://www.lendingclub.com/info/statistics.action)

###################################################################################################
# loading the required libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(scales)
library(Hmisc)

###################################################################################################
##### Data understanding 
#1. Loading "loan" data 
loan <- read.csv("loan.csv", header = T, stringsAsFactors = FALSE)

###################################################################################################
#checking first few records
head(loan)

# structure of data shows the data types
str(loan)

###################################################################################################
# Understanding data in term of business understanding #####

## Customer's Demographic Information: 

# emp_title 
# emp_length
# home_ownership
# annual_inc
# verification_status
# addr_state
# zip_code
# title
# purpose
# desc
# url

## Loan Characteristics Information

# loan amount
# funded amount
# funded amount invested
# interest rate
# loan status
# loan grade
# loan sub-grade
# dti
# loan issue date
# loan term
# installment

## Credit information: Customer Behaviour variables 

#  delinq_2yrs
#  earliest_cr_line
#  inq_last_6mths
#  open_acc
#  pub_rec
#  revol_bal
#  revol_util 
#  total_acc
#  out_prncp 
#  out_prncp_inv
# total_pymnt"             
# total_pymnt_inv
# total_rec_prncp
# total_rec_int 
# total_rec_late_fee 
# recoveries             
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# application_type       

##################################################################################################
## Meta Data ##
# If you glance the meta data,you could find three different types of variables such as 
# 1. Variables related to customers demographic and its characteristics
# 2. Variables related to the loan characteristics
# 3. Variables related to the customers behaviour characteristics.


# Business Objective
# The company wants to understand the driving factors behind the loan default. 
# If one is able to identify these risky loan applicants,
# then such loans can be reduced thereby cutting down the amount of credit loss. 
# Identification of such applicants using EDA is the aim of this case study. 

##################################################################################################

# Now,think of mapping business problem with the dataset.You could find that the variables related to the customer
# behaviour data cann't be collected at the time of applicantion. Thus analysing these variable could not solve 
# our business problem. In general,our company wants to understand the driving
# factors behind the loan default at the time of application stage. It obvious that not considering behaviour variables
# would results and help us solving business problem correctly

# So, going forward, we will do the analysis on the remaining two types of variables:

# 1. Variables related to customers demographic and its characteristics
# 2. Variables related to the loan characteristics

##################################################################################################
# So before start analysing the case study,we will remove the 3rd types of variables from the analysis # 

behaviour_var<- c( 
  "delinq_2yrs",
  "earliest_cr_line",
  "inq_last_6mths",
  "open_acc",
  "pub_rec",
  "revol_bal",
  "revol_util",
  "total_acc",
  "out_prncp",
  "out_prncp_inv",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_d",
  "last_pymnt_amnt",
  "next_pymnt_d",
  "last_credit_pull_d",
  "application_type")

# Let's remove the customer behaviour variables from the analysis. 
loan <- loan[,!(colnames(loan) %in% behaviour_var)]

##################################################################################################

# Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables

missing_values <- loan %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

# There are some features with redundant values need to be removed
# finding out relevant/good features where the missing % < 15%

good_features <- filter(missing_values,missing_percentage<0.15)

good_features <- (good_features$feature) #the reason for deleting remaining features
# we cannot impute missing values with more than 15% obs

###################################################################################################

# Removing all the column which have redundant information. 

loan <- loan[,(colnames(loan) %in% good_features)]

# Let's summarise the data

summary(loan)

# You could see that some of the variables such as "acc_now_delinq","chargeoff_within_12_mths","delinq_amnt"
# "pub_rec_bankruptcies","collections_12_mths_ex_med" & "tax_liens" have contains large number of zero observations.
# "policy_code", "initial_list_status" and "pymnt_plan" are having same value for all the observations.So these variables are not meaningful feature for the analysis
#Thus,let's get rid of these variables from the analysis.
# Also, removing meaningless variables such member_id, id, url,desc,emp_title, zip_code,addr_state,title 

red_variables <- c("member_id","id","acc_now_delinq","chargeoff_within_12_mths","pymnt_plan","initial_list_status","delinq_amnt","pub_rec_bankruptcies", "tax_liens","collections_12_mths_ex_med","policy_code",
                   "url","desc","emp_title","zip_code","addr_state","title")

loan <- loan[,!(colnames(loan) %in% red_variables)]

#count of NA values by column: just to verify if the NA values cleared
loan %>%
  summarise_all(funs(sum(is.na(.))))

sum(is.na(loan)) # 0 

##################################################################################################
##################################################################################################
# Q1. What is the average overall default rate?
##################################################################################################

# Let's separate out the current status from the dataset. 
current_loan <- filter(loan,loan_status %in% c("Current"))
current_loan$loan_status <- factor(current_loan$loan_status)

# Let's consider Fully Paid & Charged Off levels in the loan variable
loan <- filter(loan,loan_status %in% c("Fully Paid","Charged Off"))
loan$loan_status <- factor(loan$loan_status)

# Let's change "Charged Off" level to "1" and "Fully Paid" to "0"
loan$loan_status <- ifelse(loan$loan_status=="Charged Off",1,0)

# Let's convert all the character type of variables to factor

loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], 
                                           as.factor)
#####################################################################################################
# Distribution of factor/categorical variables with corresponding frequencies

# Function for distribution of categorical variables 
univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    ) 
}

#####################################################################################################
############## Univariate Analysis  ###############

# Average overall default rate
mean(loan$loan_status) # 14.6%

# Distribution
univariate_categorical(loan,loan$loan_status,"Default Distribution")

##################################################################################################
# Q2. What are the types of products for giving the loan? - Credit card, debt consolidation, etc.
# (https://www.lendingclub.com/info/statistics.action)
##################################################################################################
# Types of products 

univariate_categorical(loan,loan$purpose,"Types of Products offers by Lending club")
# 46.8% applicant applied for Debt Consolidation loan 
# 13% applicant applied for the credit card
# 7.5% applicant applied for Home improvement loan 
# 5.6% applicant applied for the major purchase loan
# and so on...

# Based on loan purpose distribution.
# We will only analyse the categories which contain more than 5% of observations.
# credit_card
# debt_consolidation
# home_improvement
# major_purchase
# other ## we don't know what kind of loan purpose comes under "other" category. Best is to get rid of this group for further analysis

# In the further analysis, we will only consider four types of loan purpose.

loan <- filter(loan,purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase"))
loan$purpose <- factor(loan$purpose)

##################################################################################################
# How have the loan application varied time for these four loan types?
##################################################################################################
#6. Application Distribution Year on year
##################################################################################################
# issue_date 
# Converting this variable to date format 
loan$issue_d <- paste("01-",loan$issue_d,sep="")
loan$issue_d <- as.Date(loan$issue_d,"%d-%B-%y")

# Let's first create a derived metric "year"
loan$year <- as.factor(format(loan$issue_d,"%Y"))
univariate_categorical(loan,loan$year,"Yearly Application Distribution for  top-4 loan types")

##################################################################################################
# How have the four products varied with time?
##################################################################################################
# Let's write a function for performing similar task for each variable wrt. purpose of the loan

##################################################################################################
# Let's perform univariate analysis on all the purpose types 

product_wise <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
    )+facet_wrap(~purpose) 
  
}

##################################################################################################
# Year-wise product distribution 
product_wise(loan,loan$year,"Product-wise Distribution:2007-2011 ")

# In 2007 & 2008, Lending club focused on approving loan for "major purpose" type loan 
# In 2009, they started lending money for home improvement purpose(7.7% applicant received this loan) along with lending for major purpose loan(3.7%)
# In 2010, they reduced home improvement loan by 68%. Also, they started lending loan for debt consolidation
# In 2011, They increased debt consolidation application by 34% along with started lending money 
# in new product- credit card 

##################################################################################################
#How have the four products varied with loan terms? 
##################################################################################################
product_wise(loan,loan$term,"Term Distribution within the four types of loans")
# Lending club has given out 36 month term period loan for major purpose and home improvement.

##################################################################################################
#How have the grades are distibuted for the top 4 lending products? 
##################################################################################################

product_wise(loan,loan$grade,"Grade Distribution: Top-4 products")

##################################################################################################
# What is the mean interest rate of for each grades per-product? 
##################################################################################################
# int_rate
str(loan$int_rate)

# Let's remove % from int_rate variable
loan$int_rate <- extract_numeric(loan$int_rate)

#3. Interest rate
describe(loan$int_rate) 

# Mean Interest rate accross each grade
tapply(loan$int_rate, loan$grade, mean) 

# Mean Interest rate accross each type of loan
tapply(loan$int_rate, loan$purpose, mean) 

##################################################################################################
#4. Home Ownership Distribution
product_wise(loan,loan$home_ownership,"Home Ownership Distribution: Top-4 products")

##################################################################################################

#5. verification status Distribution
product_wise(loan,loan$verification_status,"Verification Status Distribution: Top-4 products")

##################################################################################################
# Deriving metrics for the analysis
##################################################################################################
#1. Let's create a bin variable out of the loan_amount
options("scipen"=100, "digits"=4)

loan$bin_loan_amnt<-ifelse(loan$loan_amnt<=5000,"Small",
                           ifelse(loan$loan_amnt>5000 & loan$loan_amnt<=15000,"Medium",
                                  ifelse(loan$loan_amnt>15000 & loan$loan_amnt<=25000,"High","VeryHigh")))


##################################################################################################
#2. Funded Amount invested
describe(loan$funded_amnt_inv) 

# Let's creat a bin variable out of the funded amount invested variable

loan$bin_funded_amnt_inv<-ifelse(loan$funded_amnt_inv<=5000,"Small",
                                 ifelse(loan$funded_amnt_inv>5000 & loan$funded_amnt_inv<=15000,"Medium",
                                        ifelse(loan$funded_amnt_inv>15000 & loan$funded_amnt_inv<=25000,"High","VeryHigh")))

##################################################################################################

#3. Let's creat a bin variable out of the interest rate variable variable
loan$bin_int_rate<-ifelse(loan$int_rate<=10,"Low_rate",
                          ifelse(loan$int_rate>10 & loan$int_rate<=15,"Medium_rate","High_rate"))

##################################################################################################

#4. dti
describe(loan$dti) 

# Let's creat a bin variable out of the dti variable
loan$bin_dti<-ifelse(loan$dti<=10,"Low_dti",
                     ifelse(loan$dti>10 & loan$dti<=20,"Medium_dti","High_dti"))


##################################################################################################

#5. Funded amount
describe(loan$funded_amnt) 

# Let's creat a bin variable out of the funded_amount variable
loan$bin_funded_amnt<-ifelse(loan$funded_amnt<=5000,"Small",
                             ifelse(loan$funded_amnt>5000 & loan$funded_amnt<=15000,"Medium",
                                    ifelse(loan$funded_amnt>15000 & loan$funded_amnt<=25000,"High","VeryHigh")))


##################################################################################################

#6. Installment 
describe(loan$installment) 

# Let's creat a bin variable out of the installment variable
loan$bin_installment<-ifelse(loan$installment<=200,"Small",
                             ifelse(loan$installment>200 & loan$installment<=400,"Medium",
                                    ifelse(loan$installment>400 & loan$installment<=600,"High","VeryHigh")))

##################################################################################################

#7. Annual income
describe(loan$annual_inc) 

# Let's creat a bin variable out of the annual income variable

loan$bin_annual_inc<-ifelse(loan$annual_inc<=50000,"Small",
                            ifelse(loan$annual_inc>50000 & loan$annual_inc<=100000,"Medium",
                                   ifelse(loan$annual_inc>100000 & loan$annual_inc<=150000,"High","VeryHigh")))

##################################################################################################
#8. employement length

# emp_length: Let's extract numeric values 
loan$emp_length <- extract_numeric(loan$emp_length)

describe(loan$emp_length) # 50% loan applicants are less than or having 4 years of experience
# 23% loan applicants are having more than 10 years of experience

# Missing values in employement length 
sum(is.na(loan))/nrow(loan)*100 # 2.37% 

# Rather than treating these missing values, best is to get rid of these observations. 
loan <- data.frame(na.omit(loan))  

# Let's divide the employee experience in 4 levels such as "freshers","junior","senior"& "expert"
# employment <=1 year should be considered as "freshers"
#  1 years-3year of experience should be considered as "junior"
#  4- 7 years of experience should be considered as "senior"
#  more than 7 year of experience should be considered as "expert"

loan$emp_length <- as.factor(ifelse(loan$emp_length<=1,"freshers",ifelse(loan$emp_length>1&loan$emp_length<=3,"junior",ifelse(loan$emp_length>3&loan$emp_length<=7,"senior","expert"))))

# Let's see the distribution again: 
product_wise(loan,loan$emp_length,"Applicant Experience Distribution- Top-4 products")
# "Major purchase" and "home improvement" were majorly approved for freshers(less than 1 year of experience)
# Debt consolidation and credit card loans were majorly approved for experts(more than 10 years of experience)

#################################################################################################

# Converting character variables to factor type
loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], 
                                           as.factor)

##################################################################################################
# Let's see the correlation values of the numerical variables 
continous_var <- names(loan)[sapply(loan, class) != "factor"]

continous_data <- loan[,(colnames(loan) %in% continous_var)]

# Also, removing loan_status variable. it is a discrete variable
continous_data$loan_status <- NULL

# Also, removing date variable
continous_data$issue_d <- NULL

corr <- cor(continous_data)

# Plot the correlation matrix
corrplot(corr, method="number")

# Loan amount, funded amount and funded amount invested are hightly correlated. 
# Which is expected, because funded amount can be less than or eqaul to the loan amount applied by the applicant. 
# And most of the time, full amount has been granted by the company, Thus these variables are highly correlated. 
# One good insight is that the loan amount and annual income are having 26% positive correlation which implies the 
# loan amount requested by applicant has been decided on the basis of his/her earning i.e annual income

##################################################################################################

# Till the univariate analysis, We have seen the distribution of the each and every variables of loan dataset.
# 75% of applicants applied laon for 36 months term period
# We saw that loan applicants are increasing year on year, more than 50% of loan applicants received loans in 2011
# 30% of applicants loans comes under grade B 
# Interest rate increases as a grade level increases from A to B
# 48 % of applicants are living in rented home whereas 44% applicants were mortagaged their home.  
# Most importantly, 47% of applicants applied loan for paying their other loans. 

##################################################################################################
# One thing is clear from the univariate analysis is that the
# loan attributes and customer characteristics would be changed based on the purpose of the loan.
##################################################################################################
# How the loan amount, funded amount, dti and interest rate distributed for top-4 loan products
#########################################################################################

continuous_dist <- function(dataset,con_var,var_name){
  
  con_summary <- tapply(con_var, loan$purpose, summary) 
  
  P1 <- dataset %>% ggplot(aes(x=con_var)) + geom_line(stat = 'density',color='red')+facet_wrap(~purpose)+ggtitle(var_name)+xlab(var_name)
  
  return(list(con_summary,P1))
}

#########################################################################################

#1. loan_amnt # ignoring the rest amount as all the other variables are correlated to each.
continuous_dist(loan,loan$loan_amnt,"loan Distribution")
#########################################################################################

#2. int_rate
continuous_dist(loan,loan$int_rate,"Interest Rate Distribution")


#########################################################################################

#5. Annual income Distribution

continuous_dist(loan,loan$annual_inc,"Annual Income Distribution")


#################################################################################################

#5. DTI 

continuous_dist(loan,loan$dti,"DTI Distribution")

##################################################################################################
##################################################################################################
## On what basis are the loans approved or rejected? 
# And how are interest rates, installment amount etc. decided?


# Let's understand the variables relationship with loan_status variables 
##################################################################################################
# Bivariate Analysis #############################################################################

# Let's analyse all the categorical variables: 

segmented_defaults <- function(dataset,cat_var, var_name){
  
  a <- aggregate(loan_status~cat_var, dataset, mean)
  b <- data.frame(prop.table(table(cat_var))*100)
  b[,2] <- paste(round(b[,2], 2), "%", sep="")
  colnames(a)[1] <- var_name
  colnames(b)[1] <- var_name
  agg_default <- merge(a, b, by = var_name)
  agg_default <- data.frame(agg_default)
  colnames(agg_default) <- c(var_name, "Default","count")
  agg_default[, 2] <- round(agg_default[, 2], 2)
  agg_default <- arrange(agg_default, desc(Default))
  
  p.plot <- ggplot(agg_default, aes(agg_default[, 1], Default, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
  return(list((agg_default[1, 2] - agg_default[nrow(agg_default), 2]),p.plot))
  
}

##################################################################################################

# Let's take only categorical variables in the dataset
categorical_var_segments <- names(loan)[sapply(loan, class) == "factor"]

categorical_data_segments <- loan[,(colnames(loan) %in% categorical_var_segments)]

categorical_data_segments$loan_status <- loan$loan_status

# It contains very large number of categories, which mislead the analysis, So better is to get rid of this variable
categorical_data_segments$sub_grade <- NULL
###################################################################################################

###################################################################################################
### Let's understanding the incremental gain within the categories of categorical variables ###
###################################################################################################
# Creating a dataframe which contains the incremental values.
Increment_grain <- function(dataset,var_name){
  
  Increment <- data.frame(sapply(dataset, function(x) segmented_defaults(dataset,as.factor(x),"Plots")[[1]]))
  Increment <- cbind(variables =row.names(Increment),Increment)
  rownames(Increment) <- NULL
  colnames(Increment) <- c("Variables",var_name)
  Increment <- arrange(Increment, desc(Increment[,2]))
  Increment<- Increment[-which(Increment$Variables=="loan_status"),]
  
  ggplot(data=Increment, aes(x=reorder(Variables,Increment[,2]),y=Increment[,2])) +
    geom_bar(position="dodge",stat="identity") + 
    coord_flip() +
    ggtitle(var_name) +xlab("Variables")+ylab("default_rate")
  
}


### CREDIT CARD ####
# Filtering the credit card dataset
credit_card <- filter(categorical_data_segments,purpose =="credit_card")
credit_card$purpose <- NULL


# Credit card plots
####################################################################################
credit_card_plots <- list()
for (i in 1:13){
  credit_card_plots[[i]] <- segmented_defaults(credit_card,credit_card[,i],colnames(credit_card)[i])
}

# For Term 
credit_card_plots[[1]]
# Applicant having loan for 60 months term period defaults more than 30 months term period

# Grade
credit_card_plots[[2]]
# Grade contains 5% overall applicant among them 24% defaults 

# and so on, best is to see the maximum incremental gain within the category

#################################################################################
# Credit Card - Important variables 
###################################################################################
Increment_grain(credit_card,"Credit Card Incremental Difference")
###################################################################################
# Top-5 Variables: Credit Card
# Grade
# Term
# Bin interest rate
# Year
# Home Ownership



# debt_consolidation 
###################################################################################

debt_consolidation <- filter(categorical_data_segments,purpose =="debt_consolidation")
debt_consolidation$purpose <- NULL


# Debt Consolidation plots
####################################################################################

debt_consolidation_plots <- list()
for (i in 1:13){
  debt_consolidation_plots[[i]] <- segmented_defaults(debt_consolidation,debt_consolidation[,i],colnames(debt_consolidation)[i])
}

# Term 
debt_consolidation_plots[[1]] #27% default rate for 60 months term 

# Grade

debt_consolidation_plots[[2]]
# Total approved applicant in Grades E-G comprises of 12% of total applicant but these applicant
# are very risky for the business



#################################################################################
# Debt Consolidation - Important variables 
###################################################################################
Increment_grain(debt_consolidation,"Debt Consolidation Incremental Difference")
###################################################################################
# Top-5 Variables: Debt Consolidation
# Grade
# Bin interest rate
# Home Ownership
# Term
# Bin loan amount





# Home Improvement 
###################################################################################

home_improvement <- filter(categorical_data_segments,purpose =="home_improvement")
home_improvement$purpose <- NULL


# Home Improvement plots
####################################################################################
home_improvement_plots <- list()
for (i in 1:13){
  home_improvement_plots[[i]] <- segmented_defaults(home_improvement,home_improvement[,i],colnames(home_improvement)[i])
}

# Home Improvement- Important variables 
###################################################################################
Increment_grain(home_improvement,"Home Improvement Incremental Difference")
###################################################################################
# Top-5 variables: Home Improvement
# Grade
# Bin interest rate
# Home Ownership
# Term
# Bin Annual Income





# Major purchase 
###################################################################################

major_purchase <- filter(categorical_data_segments,purpose =="major_purchase")
major_purchase$purpose <- NULL

# Major Purchase Plots
####################################################################################
major_purchase_plots <- list()

for (i in 1:13){
  major_purchase_plots[[i]] <- segmented_defaults(major_purchase,major_purchase[,i],colnames(major_purchase)[i])
}

# Major Purchase - Important variables 
###################################################################################
Increment_grain(major_purchase,"Major Purchase Incremental Difference")

# Top-5 Major purchase loan variables: 
# Grade
# Bin interest rate
# Term
# Home Ownership
# Year

