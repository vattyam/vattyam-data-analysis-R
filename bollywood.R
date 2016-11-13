
#Q-1. 
#	Import the Bollywood data set in Rstudio. Make sure that the data file is in the folder (directory) where
# you are currently working.

setwd("F:/Data Analytics/R")
bollywood <- read.csv("bollywood.csv",header = T)     

#Q0.
#	When you import a data set, R stores character vectors as factors (by default).
# Change the attribute 'Movie' from factor to character type. 
     
bollywood$Movie <- as.character(bollywood$Movie)


#Q1.
#	Create a vector "first_10" and store the first 10 movies (from top) from the bollywood dataset.
# Use the column bollywood$Movie
     

first_10 <- head(bollywood$Movie, 10)   
	
#Q2.
#	What is the name of 7th movie from the top?
     
movie_7 <- bollywood$Movie[7] 
     
	 
#Q3. 
#	How many rows and columns are present in the bollywood dataset? 
     
	rows <- nrow(bollywood)   
  columns <- ncol(bollywood) 

	 
#Q4.
#	Find out the total number of  missing values (NA) in the dataset.
  
  na_bollywood <- length(which((is.na.data.frame(bollywood) == "TRUE")))
  
 
#Q5.
#	How many variables are of type 'factor' in the dataset. Try using the str function 
# on bollywood. Assign the numeric value to fact_vars.
     
bollywood$Movie <- as.factor(bollywood$Movie)

fact_vars <- length(grep("Factor",capture.output(str(bollywood))))

#Sandeep : perfect solution - fact_vars <-sum(sapply(bollywood,is.factor))


#Q6.0 
#	Now let's find out the movies shot by Salman, SRK and Akshay separately.
# subset() function is used for that. The code has already been written for you. 
 
  salman <- subset(bollywood, Hero == "Salman")
  srk <- subset(bollywood,Hero == "SRK")
  akshay <- subset(bollywood,Hero == "Akshay_Kumar")
     
 
#Q6.1 
#	What is the total collection of Akshay, Salman and SRK movies individually?
# Look for a column named 'Tcollection'.
 
  
  akshay_collection <- sum(akshay$Tcollection)
	salman_collection <- sum(salman$Tcollection)
	srk_collection    <- sum(srk$Tcollection)

#Q6.2 
#	Among the three actors, who has done the most number of movies?
# Store "akshay", "salman" or "srk" as the variable name most_movies_actor.
# (only the correct answer)

actor_names <- c("akshay","salman","srk")

actor_movies_count <- c(nrow(akshay),nrow(salman),nrow(srk))

most_movies_actor <- actor_names[which(actor_movies_count == max(actor_movies_count))]  


#Q6.3
# Also, whose collection is the largest (total collection/Tcollection)? 

actor_names <- c("akshay","salman","srk")

actor_movies_collection <- c(sum(akshay$Tcollection),sum(salman$Tcollection),sum(srk$Tcollection))   

largest_collection_actor <- actor_names[which(actor_movies_collection == max(actor_movies_collection))]   
     
     
#Q7 Let's try to find out which actor has made the largest opening collection (Ocollection).
#	First, let's see which movie has made the largest opening collection.
# Find out the maximum collection amount first.

   
max_number <- max(bollywood$Ocollection)

# Great. You got a number as the output. Now you can use which() function to get the index
# of the row where max_number is located. This code is already written for you.
	
row_num <- which(bollywood$Ocollection == max_number)

	
#	We know the row number corresponding to maximum collection now. To get the actor,
#	extract the relevant column corresponding to row_num.

bollywood$Hero <- as.character(bollywood$Hero)

max_opening_actor <- bollywood[row_num,"Hero"]
	

#Q8 
#	In which month most movies were released? Note that we are talking about dates and months now.
# Let's first install the lubridate package.
    
	install.packages("lubridate")
    
#   After installing any package, we have to load it into the R environment.
	
	library(lubridate)
	
#   The month() function in lubridate extracts the month from a date format.
#   Apply month() to the date column of bollywood.
 
# Sandeep - convert the date values into one format as there are
# two different formats in the RDate column(some in DD-MM-YYYY and
# some in MM/DD/YYYY)		

	temp <- as.Date(bollywood$Rdate,"%m-%d-%Y")
	
	for(i in 1:length(temp))
	{
	  if(is.na(temp[i])==T)
	  {
	    temp[i] <- as.Date(bollywood$Rdate[i],"%d-%m-%Y") 
	  }
	} 
	
	bollywood$Rdate <- temp
##		
	    bollywood$month <- month(bollywood$Rdate)
	
#   as.factor() function converts months into factors.
#   Apply as.factor() to bollywood$month
    
	  bollywood$month <- as.factor(bollywood$month)
	
#   A great way to look at the summary of a data frame is the summary() function.
#   Look at the summary of bollywood data set.
 
    summary(bollywood$month)

       
#Q9 
#	How many movies are in flop,average, Hit & Superhit categories. Simply see the summary of 
#	the column 'Verdict'.

    summary(bollywood$Verdict)   

#Q10.1 
#	Find the date on which three movies had launched on the same day?

# Sandeep - convert the date values into one format as there are
# two different formats in the RDate column(some in DD-MM-YYYY and
# some in MM/DD/YYYY)		
    
    setwd("F:/Data Analytics/R")
    bollywood <- read.csv("bollywood.csv",header = T) 
    
    temp <- as.Date(bollywood$Rdate,"%m-%d-%Y")
    
    for(i in 1:length(temp))
    {
      if(is.na(temp[i])==T)
      {
        temp[i] <- as.Date(bollywood$Rdate[i],"%d-%m-%Y") 
      }
    } 
    
    bollywood$Rdate <- temp
##    
    release_date <- bollywood$Rdate
    
    temp <- as.data.frame(table(release_date), responseName ="Num_movies")
    
    date_three <- temp$release_date[which(temp$Num_movies == 3)]
    
    date_three <-  as.Date(date_three,"%Y-%m-%d")
    
#Q10.2 
#   Find the names of the movies released on date_three and store them in movie1, movie2 and movie3.
    
    bollywood$Movie <- as.character(bollywood$Movie)
    
	  temp <- bollywood$Movie[which(bollywood$Rdate == date_three)]	
	
    movie1 <- temp[1]
    movie2 <- temp[2]
    movie3 <- temp[3]
    
	
#Q11. 
#	Convert the Rdate column into date format.
    
# Sandeep - convert the date values into one format as there are
# two different formats in the RDate column(some in DD-MM-YYYY and
#some in MM/DD/YYYY)	
    
    setwd("F:/Data Analytics/R")
    bollywood <- read.csv("bollywood.csv",header = T) 
    
    temp <- as.Date(bollywood$Rdate,"%m-%d-%Y")
    
    for(i in 1:length(temp))
    {
      if(is.na(temp[i])==T)
      {
        temp[i] <- as.Date(bollywood$Rdate[i],"%d-%m-%Y") 
      }
    } 
    
    bollywood$Rdate <- temp
    ## 
    
	
# see structure to check if you got it right. 
	str(bollywood)
  

#Q12. 
#	Change all the characters of movie names into lower case 
# Hint: tolower()
    
	bollywood$Movie <- tolower(bollywood$Movie)

	
# Finally, view the data frame again
    
	View(bollywood)
	
	

   
    


    
    
    