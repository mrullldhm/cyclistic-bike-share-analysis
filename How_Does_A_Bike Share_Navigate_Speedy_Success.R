install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(ggplot2)

#Upload all the 12 separate csv file into R and merge it into one
d1<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202212-divvy-tripdata.csv")
d2<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202301-divvy-tripdata.csv")
d3<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202302-divvy-tripdata.csv")
d4<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202303-divvy-tripdata.csv")
d5<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202304-divvy-tripdata.csv")
d6<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202305-divvy-tripdata.csv")
d7<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202306-divvy-tripdata.csv")
d8<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202307-divvy-tripdata.csv")
d9<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202308-divvy-tripdata.csv")
d10<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202309-divvy-tripdata.csv")
d11<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202310-divvy-tripdata.csv")
d12<-read.csv("D:/Tupa/Data Analyst/Case Study/How Does A Bike Share Navigate Speedy Success/202311-divvy-tripdata.csv")
trip <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
#Display the summary of the data
summary(trip)

#Delete the unnecessary variable
trip <- select(trip, -start_lat, -start_lng, -end_lat, -end_lng)
#Change the string format into time based format
trip$started_at <- ymd_hms(trip$started_at)
trip$ended_at <- ymd_hms(trip$ended_at)
#Calculate the duration in minutes.
difftime <- difftime(trip$ended_at, trip$started_at, units="mins")
duration <- as.numeric(difftime)
duration <- round(duration, digits = 0)
trip <- mutate(trip, duration)
#Identify day of week and month
trip <- mutate(trip, day_of_week = wday(started_at, label = TRUE))
trip <- mutate(trip, month = format(started_at, "%b"))
trip$month <- ordered(trip$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#Check for missing value in dataset
anyNA(trip)

View(trip)


                    #Visualize the data#

#The amount of user based on membership
user <- trip %>%
        group_by(member_casual) %>%
        summarize(total = n()) %>%
        mutate (total_user = sum(total)) %>%
        group_by(member_casual) %>%
        summarize(percentage_user = total/total_user)
        user$percentage_user <- as.numeric(user$percentage_user)
pie(x = user$percentage_user,           
        labels = c("Casual 36%","Annual 64%"), 
        main = "The Percentage of Membership",
        col = blues9, 
        clockwise = TRUE,  
        init.angle = 90,  
        border = "black",  
        radius = 1)

#The duration of the usage based on user membership

time <- trip %>%
        group_by(member_casual) %>%
        summarize (total = sum(duration)) %>%
        mutate(total_time = sum(total)) %>%
        group_by(member_casual) %>%
        summarize(percentage_time = total/total_time)
        time$percentage_time <- as.numeric(time$percentage_time)
pie(x = time$percentage_time,           
        labels = c("Casual 56%","Annual 44%"), 
        main = "Percentage Of Duration",
        col = blues9, 
        clockwise = TRUE,  
        init.angle = 90,  
        border = "black",  
        radius = 1)
  
#The day of week and month usage for casual member
#For marketing timing
dayofweekcasual <- trip %>%
             filter(member_casual =="casual") %>%
             group_by(member_casual, day_of_week) %>% 
             summarise(numberofrides = n(),.groups = "drop")
ggplot(data = dayofweekcasual, aes(x = day_of_week, y = numberofrides)) + 
       geom_bar(stat = "identity", fill = "blue", color = "black") +
       labs(x = "Day Of Week", y = "Number Of Rides", title = "Casual Membership Daily usage")
  
dayofweekannual <- trip %>%
             filter(member_casual =="member") %>%
             group_by(member_casual, day_of_week) %>% 
             summarise(numberofrides = n(),.groups = "drop")
ggplot(data = dayofweekannual, aes(x = day_of_week, y = numberofrides)) + 
       geom_bar(stat = "identity", fill = "blue", color = "black") +
       labs(x = "Day Of Week", y = "Number Of Rides", title = "Annual Membership Daily usage")

monthlycasual <- trip %>%
           filter(member_casual =="casual") %>%
           group_by(member_casual, month) %>% 
           summarise(numberofrides = n(),.groups = "drop")
ggplot(data = monthlycasual, aes(x = month, y = numberofrides)) + 
       geom_bar(stat = "identity", fill = "blue", color = "black") +
       labs(x = "Month", y = "Number Of Rides", title = "Casual Membership Monthly Usage")

monthlyannual <- trip %>%
           filter(member_casual =="member") %>%
           group_by(member_casual, month) %>% 
           summarise(numberofrides = n(),.groups = "drop")
ggplot(data = monthlyannual, aes(x = month, y = numberofrides)) + 
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(x = "Month", y = "Number Of Rides", title = "Annual Membership Monthly Usage")

#Preferred bike
#Discount approach for marketing
bikecasual <- trip %>%
        filter(member_casual =="casual") %>%
        group_by(member_casual, rideable_type) %>% 
        summarise(numberofrides = n(),.groups = "drop")
ggplot(data = bikecasual, aes(x = rideable_type, y = numberofrides)) + 
       geom_bar(stat = "identity", fill = "blue", color = "black") +
       labs(x = "Type Of Ride", y = "Nuymber Of Rides", title = "Casual Membership Bike Usage")

bikeannual <- trip %>%
       filter(member_casual =="member") %>%
       group_by(member_casual, rideable_type) %>% 
       summarise(numberofrides = n(),.groups = "drop")
ggplot(data = bikeannual, aes(x = rideable_type, y = numberofrides)) + 
       geom_bar(stat = "identity", fill = "blue", color = "black") +
       labs(x = "Type Of Ride", y = "Nuymber Of Rides", title = "Annual Membership Bike Usage")
