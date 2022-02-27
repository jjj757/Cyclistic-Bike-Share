library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)
#importing csv
tripdata_202102 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202102-divvy-tripdata.csv")
tripdata_202103 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202103-divvy-tripdata.csv")
tripdata_202104 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202104-divvy-tripdata.csv")
tripdata_202105 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202105-divvy-tripdata.csv")
tripdata_202106 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202106-divvy-tripdata.csv")
tripdata_202107 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202107-divvy-tripdata.csv")
tripdata_202108 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202112-divvy-tripdata.csv")
tripdata_202201 <- read.csv("/Users/jeremymassaquoi/Data project/Google Cert Project/CSV Bike/202201-divvy-tripdata.csv")

#joining csvs
#checking structure for any inaccurate datatypes and other data inaccuracies

str(tripdata_202102)
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)
str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)
str(tripdata_202201)

#double checking to make sure certain columns are character
tripdata_202102 <-  mutate(tripdata_202102, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202103 <-  mutate(tripdata_202103, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
                             
tripdata_202104 <-  mutate(tripdata_202104, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202105 <-  mutate(tripdata_202105, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202106 <-  mutate(tripdata_202106, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202107 <-  mutate(tripdata_202107, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202108 <-  mutate(tripdata_202108, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202109 <-  mutate(tripdata_202109, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202110 <-  mutate(tripdata_202110, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202111 <-  mutate(tripdata_202111, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
tripdata_202112 <-  mutate(tripdata_202112, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id))
                             
tripdata_202201 <-  mutate(tripdata_202201, start_station_id = as.character(start_station_id)
                              ,end_station_id = as.character(end_station_id))
#############
#join
#############
all_12_months<-bind_rows((tripdata_202102),
         (tripdata_202103),
         (tripdata_202104),
         (tripdata_202105),
         (tripdata_202106),
         (tripdata_202107),
         (tripdata_202108),
         (tripdata_202109),
         (tripdata_202110),
         (tripdata_202111),
         (tripdata_202112),
         (tripdata_202201))
#check columns
colnames(all_12_months)






all_12_months_v2 <-all_12_months
#looking for blanks values seen in importing
c<-all_12_months_v2%>%
  filter(all_12_months_v2$start_station_name == ""|all_12_months_v2$start_station_id == ""|all_12_months_v2$end_station_name == ""|all_12_months_v2$end_station_id == "")
#filtering for rows with no ""
c<-all_12_months_v2%>%
  filter(all_12_months_v2$start_station_name != ""|all_12_months_v2$start_station_id != ""|all_12_months_v2$end_station_name != ""|all_12_months_v2$end_station_id != "")

all_12_months_v2<-c
#rechecking
str(all_12_months_v2)
#creating date, day of week, month, day, and year of data
all_12_months_v2$date <- as.Date(all_12_months_v2$started_at) 
all_12_months_v2$weekday <- format(as.Date(all_12_months_v2$started_at), "%A")
all_12_months_v2$month <- format(as.Date(all_12_months_v2$started_at), "%m")
all_12_months_v2$day <- format(as.Date(all_12_months_v2$date), "%d")
all_12_months_v2$year <- format(as.Date(all_12_months_v2$date), "%Y")


#creating time difference in seconds
all_12_months_v2$ride_length <- difftime(all_12_months_v2$ended_at, all_12_months_v2$started_at)


#rechecking structure and names
str(all_12_months_v2)
colSums(is.na(all_12_months_v2))

#making sure ride_length is numeric
is.factor(all_12_months_v2$ride_length)
is.numeric(all_12_months_v2$ride_length)
all_12_months_v2$ride_length <- as.numeric(all_12_months_v2$ride_length)
is.numeric(all_12_months_v2$ride_length)


#checking for NAs in each column
apply(all_12_months_v2, 2, function(x) any(is.na(x)))
#checking certain column for outputs

table(all_12_months_v2$rideable_type)
table(all_12_months_v2$member_casual)
table(all_12_months_v2$weekday)
table(all_12_months_v2$month)
table(all_12_months_v2$year)


#Getting rid of HQ bikes and ride times less than zero

all_12_months_v2 <- all_12_months_v2[!(all_12_months_v2$rideable_type == "HQ QR" | all_12_months_v2$ride_length<0),]

#getting rid of any NAS in data
d<-na.omit(all_12_months_v2)
all_12_months_v2 <- d
colSums(is.na(all_12_months_v2)) #checking for NAs

#removing any observations where the start time is after the end time
sum1 <- nrow(d) 
d <- all_12_months_v2 %>% 
  filter(all_12_months_v2$started_at < all_12_months_v2$ended_at)

#checking observations
sum2<-nrow(d)
all_12_months_v2<-d

#adding a minutes column in case any stakeholder would prefer mins over seconds
all_12_months_v2 <- all_12_months_v2%>%
  mutate(ride_length_min = ride_length/60)


##############
#Analysis
##############
#summary of ride lengths in seconds
summary(all_12_months_v2$ride_length)


#getting statistics for ride lengths for each membership type
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual, FUN = mean)
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual, FUN = median)
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual, FUN = max)
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual, FUN = min)


#getting statistics for ride lengths for each membership type per day of the week
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual + all_12_months_v2$weekday, FUN = mean)

# ordering weekdays
all_12_months_v2$weekday <- ordered(all_12_months_v2$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

# Average run time per day; separated by membership
aggregate(all_12_months_v2$ride_length ~ all_12_months_v2$member_casual + all_12_months_v2$weekday, FUN = mean)

#Analyze ridership data by type and weekday
e<-all_12_months_v2 %>% 
  mutate(weekday = wday(started_at)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

  

#export for visualization on Tableau
counts <- all_12_months_v2
write.csv(counts, file = '~/Desktop/tripdata.csv')
