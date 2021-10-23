rm(list = ls())
install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(skimr) #get summary data
library
install.packages("skimr")
library(skimr)
library(ggplot2)

#STEP 1: COLLECT DATA
Trips_Apr20 <- read_csv('202004-divvy-tripdata.csv')
Trips_May20 <- read_csv('202005-divvy-tripdata.csv')
Trips_June20 <- read_csv('202006-divvy-tripdata.csv')
Trips_July20 <- read_csv('202007-divvy-tripdata.csv')
Trips_Aug20 <- read_csv('202008-divvy-tripdata.csv')
Trips_Sep20 <- read_csv('202009-divvy-tripdata.csv')
Trips_Oct20 <- read_csv('202010-divvy-tripdata.csv')
Trips_Nov20 <- read_csv('202011-divvy-tripdata.csv')
Trips_Dec20 <- read_csv('202012-divvy-tripdata.csv')
Trips_Jan21 <- read_csv('202101-divvy-tripdata.csv')
Trips_Feb21 <- read_csv('202102-divvy-tripdata.csv')
Trips_Mar21 <- read_csv('202103-divvy-tripdata.csv')

#STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
colnames(Trips_Apr20)
colnames(Trips_May20)
colnames(Trips_June20)
colnames(Trips_July20)
colnames(Trips_Aug20)
colnames(Trips_Sep20)
colnames(Trips_Oct20)
colnames(Trips_Nov20)
colnames(Trips_Dec20)
colnames(Trips_Jan21)
colnames(Trips_Feb21)
colnames(Trips_Mar21)

#we can compare column datatype across all dataframe by using compare_df_cols when we have large dataset, that would be more easy 
compare_df_cols(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20,
                Trips_Aug20, Trips_Sep20, Trips_Oct20, Trips_Nov20, Trips_Dec20,
                Trips_Jan21, Trips_Feb21, Trips_Mar21, return = "mismatch")

#Convert end_station_id and start_station_id to character so that they can stack correctly 
Trips_Apr20 <- mutate(Trips_Apr20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
Trips_May20 <- mutate(Trips_May20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
Trips_June20 <- mutate(Trips_June20, end_station_id =
                         as.character(end_station_id), start_station_id =
                         as.character(start_station_id))
Trips_July20 <- mutate(Trips_July20, end_station_id =
                         as.character(end_station_id), start_station_id =
                         as.character(start_station_id))
Trips_Aug20 <- mutate(Trips_Aug20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
Trips_Sep20 <- mutate(Trips_Sep20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
Trips_Oct20 <- mutate(Trips_Oct20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))
Trips_Nov20 <- mutate(Trips_Nov20, end_station_id =
                        as.character(end_station_id), start_station_id =
                        as.character(start_station_id))

compare_df_cols(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20,
                Trips_Aug20, Trips_Sep20, Trips_Oct20, Trips_Nov20, Trips_Dec20,
                Trips_Jan21, Trips_Feb21, Trips_Mar21, return = "mismatch")

#Stack individual data frames into one big data frame
all_trips <- bind_rows(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20,
                       Trips_Aug20, Trips_Sep20, Trips_Oct20, Trips_Nov20, Trips_Dec20,
                       Trips_Jan21, Trips_Feb21, Trips_Mar21)
#Remove unused column
all_trips<- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

#Rename Columns 
all_trips <- all_trips %>% rename(trip_id= ride_id ,ride_type =
                                    rideable_type
                                  ,start_time = started_at,end_time =ended_at
                                  ,from_station_name = start_station_name
                                  ,from_station_id = start_station_id
                                  ,to_station_name = end_station_name
                                  ,to_station_id = end_station_id
                                  ,usertype = member_casual)

#STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
colnames(all_trips) #List of column names
head(all_trips) #See the first 6 rows of data frame
summary(all_trips) #Statistical summary of data. Mainly for numerics
skim(all_trips) #get summary of data, check missing data 
glimpse(all_trips)

#Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$start_time) #The default format is yyyymm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
tripdata_cleaned$weekday <- format(as.Date(tripdata_cleaned$date), "%u")

#“ride_length” calculation
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#To remove negative values
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),] 
write.csv(all_trips_v2, "data.csv")

# Remove rows with missing values
colSums(is.na(all_trips))
tripdata_cleaned <- all_trips_v2[complete.cases(all_trips_v2), ]
skim(tripdata_cleaned)

#STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
summary(tripdata_cleaned$ride_length)

# average ride_length for members and casual riders
tripdata_cleaned %>% 
  group_by(usertype) %>% 
  summarize(mean(ride_length))

# average ride_length for day_of_week and usertype
tripdata_cleaned %>% 
  group_by(day_of_week,usertype) %>% 
  summarize(mean(ride_length))

#  average ride time by each day for members vs casual users
aggregate(tripdata_cleaned$ride_length ~ tripdata_cleaned$usertype + tripdata_cleaned$day_of_week, FUN = mean)



# analyze ridership data by type and weekday
tripdata_cleaned %>% 
  group_by(usertype, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)

# visualize number of rides by rider type
tripdata_cleaned %>% 
  group_by(usertype, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Number of Rides") +
  xlab("Day of Week")
