
dataset<- Uber.request.data <- read.csv("Uber request data.csv")

library(stringr)
library(ggplot2)

#Check for duplicate values
sum(duplicated(Uber.request.data$Request.id))

#Check for NA values
sum(is.na(Uber.request.data$Request.id))
sum(is.na(Uber.request.data$Pickup.point))
sum(is.na(Uber.request.data$Status))
sum(is.na(Uber.request.data$Request.timestamp))

#Make the time separator consistent
dataset$req_dt <- str_replace_all(dataset$Request.timestamp, "[/]",  "-")
dataset$drop_dt <- str_replace_all(dataset$Drop.timestamp, "[/]",  "-")

# convert time columns to datetime oject
dataset$req_dt <- as.POSIXlt(dataset$req_dt, format = "%d-%m-%Y %H:%M")
dataset$drop_dt <- as.POSIXlt(dataset$drop_dt, format = "%d-%m-%Y %H:%M")


#Extract the hour and day data from the request time
dataset$reqhour <- format(dataset$req_dt, "%H")
dataset$day <- format(dataset$req_dt, "%d")

#plot the data
P <- ggplot(dataset, aes(x = as.factor(reqhour),fill = Status))+geom_bar(position = "dodge")
P + facet_wrap( ~ dataset$day, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Status" )

Q <- ggplot(dataset, aes(x = as.factor(reqhour),fill = Pickup.point))+geom_bar(position = "dodge")
Q + facet_wrap( ~ dataset$day, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )


#plotting the number of trips by hour
ggplot(dataset, aes(x = as.factor(reqhour),fill = Pickup.point))+geom_bar(position = "dodge")+labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )


#converting reqhour to numeric
dataset$reqhour <- as.numeric(dataset$reqhour)
#making slots by number of trips
dataset$time_slot = ifelse(dataset$reqhour < 5, "Pre_Morning", ifelse(dataset$reqhour < 10,"Morning_Rush",ifelse(dataset$reqhour < 17,"Day_Time",ifelse(dataset$reqhour < 22,"Evening_Rush","Late_Night"))))

#finding the number of trips made in each slot
nrow(subset(dataset, dataset$time_slot == "Pre_Morning"))
nrow(subset(dataset, dataset$time_slot == "Morning_Rush"))
nrow(subset(dataset, dataset$time_slot == "Day_Time"))
nrow(subset(dataset, dataset$time_slot == "Evening_Rush"))
nrow(subset(dataset, dataset$time_slot == "Late_Night"))

#plotting and identifying the most critical problems before Uber
ggplot(dataset, aes(x = as.factor(time_slot), fill= as.factor(dataset$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )

#problem 1 - High cancellations in the morning
dataset1 <- subset(dataset,time_slot=="Morning_Rush")
ggplot(dataset1, aes(x = as.factor(Pickup.point), fill= as.factor(dataset1$Status))) + geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )

#severity of problem by location
nrow(subset(dataset1, dataset1$Pickup.point == "Airport" & dataset1$Status == "Cancelled"))
nrow(subset(dataset1, dataset1$Pickup.point == "City" & dataset1$Status == "Cancelled"))

#percentage breakup
data_problem1 <- subset(dataset1, Pickup.point %in% "City")
ggplot(data_problem1, aes(x = data_problem1$Pickup.point, fill= as.factor(data_problem1$Status))) + geom_bar() + coord_polar(theta = "y", start=0)+ labs( y = "Number of Requests", x = "", fill = "Status")

#Supply and Demand
nrow(subset(dataset1, dataset1$Pickup.point == "City" & dataset1$Status == "Trip Completed"))
nrow(subset(dataset1, dataset1$Pickup.point == "City"))

#problem 2
dataset2 <- subset(dataset,time_slot=="Evening_Rush")
ggplot(dataset2, aes(x = as.factor(Pickup.point), fill= as.factor(dataset2$Status))) + geom_bar()+labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )


#severity of problem by location
nrow(subset(dataset2, dataset2$Pickup.point == "Airport" & dataset2$Status == "No Cars Available"))
nrow(subset(dataset2, dataset2$Pickup.point == "City" & dataset2$Status == "No Cars Available"))

#percentage breakup
data_problem2 <- subset(dataset2, Pickup.point %in% "Airport")
ggplot(data_problem2, aes(x = data_problem2$Pickup.point, fill= as.factor(data_problem2$Status))) + geom_bar() + coord_polar(theta = "y", start=0) + labs( y = "Number of Requests", x = "", fill = "Status")

#Supply and Demand
nrow(subset(dataset2, dataset2$Pickup.point == "Airport" & dataset2$Status == "Trip Completed"))
nrow(subset(dataset2, dataset2$Pickup.point == "Airport"))

