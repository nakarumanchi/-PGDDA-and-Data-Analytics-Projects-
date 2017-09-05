# Load essential libraries
library(ggplot2)
library(lubridate)


# load the media company data
media <- read.csv("mediacompany.csv", stringsAsFactors = F)
str(media)


# convert the date to R date format
media$date <- as.Date(media$Date, "%m/%d/%Y")


# Deriving "days since the show started"
media$day<- difftime(media$date,as.Date("2017-02-28"), units="days")
media$day <- as.numeric(media$day)


# Scatter Plot (days vs Views_show)
ggplot(media, aes(day, Views_show)) + geom_line(aes(colour = "blue" )) 
+ scale_x_continuous(name = "days", breaks = seq(0,84,7), limits = c(0,84)) + 
  scale_y_continuous(name = "Views_show", breaks = seq(0,800000,100000), 
                     limits = c(0,800000))


# Scatter Plot (days vs Views_show and days vs Ad_impressions)
# bringing the Ad_impression to the scale of Views_show. Think how this could be done. 
ggplot(media, aes(day, Views_show)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "days", breaks = seq(0,84,7), limits = c(0,84)) 
+ scale_y_continuous(name = "Views_show", breaks = seq(0,800000,100000),limits = c(0,800000)) 
+ geom_line(aes(x=day, y=Ad_impression*8/30000, colour="red"))
# Although much better graphs could have been produced in Tableu, it serves the purpose of visualising variables


# Alternate- Using a library to generate a plot with 2 Y axis
install.packages("plotrix")
library(plotrix)
twoord.plot( lx=media$day,ly=media$Views_show,ry=media$Ad_impression, 
             rx = media$day, ylab="Views_show",rylab="Ad_impression")
# You could try and find more ways to do the same


# Derived Metrics
# Weekdays are taken such that 1 corresponds to Sunday and 7 to Saturday
# Generate the weekday variable
media$Weekday <- (media$day+3)%%7
media$Weekday[which(media$Weekday==0)] <- 7
View(media)


#model 1.1 Y vs Weekday & visitors 
model_1.1 <- lm(formula = Views_show~Visitors + Weekday, data = media)
summary(model_1.1)


# create Weekend variable, with value 1 at weekends and 0 at weekdays
media$Weekend <- ifelse((media$day%%7==5)|(media$day%%7==4),1,0)


#model 1.2 Y vs Weekend & visitors 
model_1.2 <- lm(formula = Views_show~Visitors + Weekend, data = media)
summary(model_1.2)


#model 2 Y vs Weekend, Character A & visitors
model_2 <- lm(formula = Views_show~Visitors + Weekend + Character_A, data = media)
summary(model_2)


# Create lag variable
media$Lag_Views <- c(0, media$Views_show[seq_along(media$Views_show) -1])
View(media)


#Model 3 Y vs Weekend, Character A, LagViews & visitors
model_3 <- lm(formula = Views_show~Visitors + Lag_Views + Weekend + 
                Character_A, data = media)
summary(model_3)


#model 4 Y vs Weekend, Character A & Views_platform
model_4 <- lm(formula = Views_show~ Weekend + Character_A + 
                Views_platform, data = media)
summary(model_4)


#model 5 Y vs Weekend, Character A & Visitors
model_5 <- lm(formula = Views_show~ Weekend + Character_A + 
                Visitors, data = media)
summary(model_5)


#model 6 Y vs Weekend, Character A, Visitors and Ad_Impression
model_6 <- lm(formula = Views_show~ Weekend + Character_A + Visitors + 
                Ad_impression, data = media)
summary(model_6)


#model 7 Y vs Weekend, Character A and Ad_Impression
model_7 <- lm(formula = Views_show~ Weekend + Character_A + 
                Ad_impression, data = media)
summary(model_7)


#Ad impression in million
media$ad_impression_million <- media$Ad_impression/1000000


#model 8 Y vs Weekend, Character A, Ad_Impression and Cricket_match_india
model_8 <- lm(formula = Views_show~ Weekend + Character_A + 
                ad_impression_million + Cricket_match_india, data = media)
summary(model_8)


#model 9 Y vs Weekend, Character A, Ad_Impression and Cricket_match_india
model_9 <- lm(formula = Views_show~ Weekend + ad_impression_million, data = media)
summary(model_9)


#predicted shows
media$Predicted_views <- predict(model_9, media)
media$error <-  media$Views_show - media$Predicted_views


# Plot - Actual vs Predicted Views Model9
ggplot(media, aes(day, Views_show)) + geom_line(aes(colour = "blue" )) +
scale_x_continuous(name = "days", breaks = seq(0,84,7), limits = c(0,84)) +
scale_y_continuous(name = "Views_show", breaks = seq(0,800000,100000), limits = c(0,800000)) +
geom_line(aes(x=day, y=Predicted_views, colour="red"))


# Plot Model9 errors
ggplot(media, aes(day, error)) + geom_point() + 
  scale_x_continuous(name = "days", breaks = seq(0,90,10), limits = c(0,84)) +
  scale_y_continuous(name = "Error", breaks = seq(-250000,250000,50000), limits = c(-250000,250000)) +
 geom_hline(yintercept = 0)


# Predicting values and calculating error
media$Predicted_views_model5 <- predict(model_5, media)
media$error_model5 <-  media$Views_show - media$Predicted_views_model5


# Actual vs predicted values from model5
ggplot(media, aes(day, Views_show)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "days", breaks = seq(0,84,7), limits = c(0,84)) + 
  scale_y_continuous(name = "Views_show", breaks = seq(0,800000,100000), limits = c(0,800000)) + geom_line(aes(x=day, y=Predicted_views_model5, colour="red"))


# Plot model5 errors
ggplot(media, aes(day, error_model5)) + geom_line() + 
  scale_x_continuous(name = "days", breaks = seq(0,90,10), limits = c(0,84)) + 
  scale_y_continuous(name = "Error", breaks = seq(-250000,250000,50000), limits = c(-250000,250000)) + geom_hline(yintercept = 0)

