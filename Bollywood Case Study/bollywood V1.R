
#	Import the Bollywood data set in Rstudio
# Make sure that the data file is in the folder (directory) where you are currently working.

bollywood <- read.csv("bollywood.csv")
bollywood


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of data frame by using str()
str(bollywood)

# You can change the attribute 'Movie' from factor to character type using given command
bollywood$Movie <- as.character(bollywood$Movie)


#Q1.
#	Access the last 10 movies (from bottom of bollywood dataframe) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)

n <- nrow(bollywood)
last_10 <- bollywood$Movie[(n-9):n]
last_10

#Q2.
#	Find out the total number of  missing values (NA) in the bollywood dataframe.
# Store the result in na_bollywood vector

na_bollywood <- sum(is.na(bollywood))
na_bollywood

#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie

max_Tcol <- max(bollywood$Tcollection)
index_max_Tcol <- which(bollywood$Tcollection == max_Tcol)
top_movie <- bollywood$Movie[index_max_Tcol]
top_movie


#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

max_Tcol <- max(bollywood$Tcollection)
index_max_Tcol <- which(bollywood$Tcollection == max_Tcol)    #finding index of row with maximum Total Collections

max_2_Tcol <- max(bollywood$Tcollection[-index_max_Tcol])     #stripping index with index_max_Tcol to find second maximum
index_max_2_Tcol <- which(bollywood$Tcollection == max_2_Tcol)
top_2_movie <- bollywood$Movie[index_max_2_Tcol]
top_2_movie
  
  
# Now let's find out the movies shot by Shahrukh, Akshay & Amitabh separately.
# subset() function is used for that. The code has already been written for you. 

shahrukh <- subset(bollywood, Lead == "Shahrukh")
akshay <- subset(bollywood, Lead == "Akshay")
amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view how the above data frames look like

#Q5  
#	What is the total collection of Shahrukh, Akshay & Amitabh movies individually?

shahrukh_collection <- sum(shahrukh$Tcollection)
shahrukh_collection
akshay_collection <- sum(akshay$Tcollection)
akshay_collection
amitabh_collection <- sum(amitabh$Tcollection)
amitabh_collection


#Q6  
# Write command/s to find out how many movies are in flop,average, Hit & Superhit categories in entire bollywood dataset.

summary(bollywood$Verdict)


#You can use SAPPLY function if you want to apply a function specific columns in a data frame
#Command to find mean of Ocollection, Wcollection, Fwcollecion & Tcollection is illustrated below
sapply(bollywood[4:7], mean, na.rm = TRUE)

#Similarly you can find maximum value of Ocollection, Wcollection, Fwcollecion & Tcollection

#Q7 
# Write command to find name of movies which have maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector
row_index <- sapply(bollywood[4:7], which.max)
result <- bollywood$Movie[row_index]
result

