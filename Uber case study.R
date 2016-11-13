#Read Uber request data into a dataframe named uber

setwd(("F:/Data Analytics/Assignment_August 7"))
uber <- read.csv("Uber request data.csv",header = T)

# There are two different formats in Date column, convert it into default date format

temp <- as.Date(uber$Date,"%m-%d-%Y")

for(i in 1:length(temp))
{
  if(is.na(temp[i])==T)
  {
    temp[i] <- as.Date(uber$Date[i],"%d-%m-%Y") 
  }
} 

uber$Date <- temp

# Plot frequency of requests for each hour and pick up point

requests_time <- data.frame(table(sub("\\:.*","",as.character(uber$Request.time)),uber$Pickup.point))
library(ggplot2)
requests_time_aes <- ggplot(requests_time,aes(x = requests_time$Var1, y = requests_time$Freq,fill = requests_time$Var2))
plot1 <- requests_time_aes +geom_bar(stat = "identity",position = "dodge") + xlab("Request Time") + ylab("Requests Frequency") + guides(fill=guide_legend(title="Pickup Point"))

# Divide the day into different Time slots based on requests frequency
# Pre_Morning = 00 to 03 hrs
# Morning_Rush = 04 to 09 hrs
# Day_Time = 10 to 16 hrs
# Evening_Rush = 17 to 21 hrs 
# Late_Night = 22 to 23 hrs 

uber["Time_Slot"] <- NA
time <- sub("\\:.*","",as.character(uber$Request.time)) 

for(i in 1 : length(uber$Request.id)){
  
  if(time[i] == "00" | time[i] == "01" | time[i] == "02" | time[i] == "03")
     {
       uber$Time_Slot[i] <- "Pre_Morning"
     }

else if(time[i] == "04" | time[i] == "05" | time[i] == "06" | time[i] == "07" | time[i] == "08" | time[i] == "09")   
{
     uber$Time_Slot[i] <- "Morning_Rush" 
}

else if(time[i] == "17" | time[i] == "18" | time[i] == "19" | time[i] == "20" | time[i] == "21" )
{
  uber$Time_Slot[i] <- "Evening_Rush"
}  

  else if(time[i] == "22" | time[i] == "23")
  {
    uber$Time_Slot[i] <- "Late_Night"
  }   
  
else 
{
  uber$Time_Slot[i] <- "Day_Time"
}
}

# Plot completed trips frequency based on Time Slot

uber_trip_completed <- subset(uber,Status == "Trip Completed")
time_slot_trips <- data.frame(table(uber_trip_completed$Time_Slot))
time_slot_trips_aes <- ggplot(time_slot_trips,aes(x = time_slot_trips$Var1, y = time_slot_trips$Freq, fill = time_slot_trips$Var1))
plot2 <- time_slot_trips_aes +geom_bar(stat = "identity") + xlab("Time Slot") + ylab("Trips Frequency") + guides(fill=guide_legend(title="Time Slot"))


# Plot frequency of requests based on Time Slot, also represent Request status

time_slot_requests <- data.frame(table(uber$Time_Slot,uber$Status))
time_slot_requests_aes <- ggplot(time_slot_requests,aes(x = time_slot_requests$Var1, y = time_slot_requests$Freq, fill = time_slot_requests$Var2 ))
plot3 <- time_slot_requests_aes +geom_bar(stat = "identity") + xlab("Time Slot") + ylab("Requests Frequency") + guides(fill=guide_legend(title="Requests Status"))


# problem 1 : Most requests are getting cancelled during Morning_Rush in City

requests_morning_rush <- subset(uber,Time_Slot == "Morning_Rush")
requests_morning_rush_cancelled <- subset(requests_morning_rush,Status == "Cancelled" )
requests_morning_rush_table <- data.frame(table(requests_morning_rush_cancelled$Pickup.point))
requests_morning_rush_aes <- ggplot(requests_morning_rush_table,aes(x = requests_morning_rush_table$Var1, y = requests_morning_rush_table$Freq, fill = requests_morning_rush_table$Var1))
plot4 <- requests_morning_rush_aes + geom_bar(stat = "identity") + xlab("Pickup Point") + ylab("Frequency of Cancelled requests") + guides(fill=guide_legend(title="Pickup Point"))

# Percentage breakup for problem 1 based on pick up point

percent_cancelled <- round(100*requests_morning_rush_table$Freq/sum(requests_morning_rush_table$Freq),1)
percent_cancelled_airport <- percent_cancelled[1]
percent_cancelled_city <- percent_cancelled[2]

# Demand and Supply gap for Problem 1

city_requests_morning_rush <- subset(requests_morning_rush,Pickup.point == "City")
count_requests_morning_rush <-  nrow(city_requests_morning_rush)
count_trips_made_morning_rush <- length(which(city_requests_morning_rush$Status == "Trip Completed"))

# Problem 2 : No cars are available problem is predominant during Evening_Rush hours at Airport

requests_evening_rush <- subset(uber,Time_Slot == "Evening_Rush")
requests_evening_rush_no_cars<- subset(requests_evening_rush,Status == "No Cars Available")
requests_evening_rush_table <- data.frame(table(requests_evening_rush_no_cars$Pickup.point))
requests_evening_rush_aes <- ggplot(requests_evening_rush_table,aes(x = requests_evening_rush_table$Var1, y = requests_evening_rush_table$Freq, fill = requests_evening_rush_table$Var1))
plot5 <- requests_evening_rush_aes + geom_bar(stat = "identity") + xlab("Pickup Point") + ylab("Frequency of No Cars Available requests") + guides(fill=guide_legend(title="Pickup Point"))

# Percentage breakup for problem 2 based on pick up point

percent_no_cars <- round(100*requests_evening_rush_table$Freq/sum(requests_evening_rush_table$Freq),1)
percent_no_cars_airport <- percent_no_cars[1]
percent_no_cars_city <- percent_no_cars[2]

# Demand and Supply gap for Problem 2

airport_requests_evening_rush <- subset(requests_evening_rush,Pickup.point == "Airport")
count_requests_evening_rush <-  nrow(airport_requests_evening_rush)
count_trips_made_evening_rush <- length(which(airport_requests_evening_rush$Status == "Trip Completed"))

