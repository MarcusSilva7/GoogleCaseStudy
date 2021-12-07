#Setting up environment
install.packages("tidyverse")
library(tidyverse)
library(data.table) #Saving the data with "data.table" package for a faster process  

#I put all CSV files into one file and set it as my working directory
#Merging all the data
csv_data <- list.files(path = getwd(), recursive = TRUE, full.names = TRUE)
cyclistic_data <- do.call(rbind, lapply(csv_data, read_csv))
head(cyclistic_data)

#Looking for duplicates in the data
sum(duplicated(cyclistic_data$ride_id))

#Removing duplicates
cyclistic_clean <- cyclistic_data[!duplicated(cyclistic_data$ride_id), ]
head(cyclistic_clean)


#Adjusting date: Year/Month
cyclistic_clean <- cyclistic_clean %>% 
  mutate(year_month = paste(strftime(cyclistic_clean$started_at, "%Y"),
                            "-", strftime(cyclistic_clean$started_at, "%m")))
unique(cyclistic_clean$year_month)

#Adjusting date: Weekday
cyclistic_clean <- cyclistic_clean %>% 
  mutate(weekday = paste(strftime(cyclistic_clean$started_at, "%a")))
unique(cyclistic_clean$weekday)

#Adjusting time: Hour of the day
cyclistic_clean <- cyclistic_clean %>% 
  mutate(hour_of_day = paste(strftime(cyclistic_clean$started_at, "%H")))
unique(cyclistic_clean$hour_of_day)

#Adjusting time: Duration in minutes
cyclistic_clean <- cyclistic_clean %>% 
  mutate(ride_time_in_m = as.numeric(cyclistic_clean$ended_at - cyclistic_clean$started_at) / 60)
summary(cyclistic_clean$ride_time_in_m)

#Saving the clean data
fwrite(cyclistic_clean, "cyclistic_clean.csv")


#Part 2 of the project
library(tidyverse)
df <- read_csv("cyclistic_clean.csv") # Using "df" for the sake of making the process easier

summary(df$ride_time_in_m)
sum(df$ride_time_in_m > 300)

#Removing unnecessary columns for analysis
df <- df %>%
  select(-c(...1, start_lat, end_lat, start_lng, end_lng))

#Creating a secondary version to keep the original data untouched 
fwrite(df, "cyclistic_clean_v2.csv")

df <- read_csv("cyclistic_clean_v2.csv")

#Removing negative and over 5 hours long rides for more accuracy
df_test <- df[!(df$ride_time_in_m < 0 | df$ride_time_in_m > 300),]


fwrite(df_test, "definitive_data.csv")

#Analysis of the clean data
df <- read_csv("definitive_data.csv")

# Compare casual x member
aggregate(df$ride_time_in_m ~ df$member_casual, FUN = mean)
aggregate(df$ride_time_in_m ~ df$member_casual, FUN = median)
aggregate(df$ride_time_in_m ~ df$member_casual, FUN = max)
aggregate(df$ride_time_in_m ~ df$member_casual, FUN = min)

# Average ride time
aggregate(df$ride_time_in_m ~ df$member_casual + df$weekday, FUN = mean)

# Putting the weekdays in order
df$weekday <- ordered(df$weekday, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# Analyze ridership data by type and weekday
df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time_in_m)) %>% 
  arrange(member_casual, weekday) 

# Command to show full numbers on the viz
options(scipen = 999)

# Let's visualize the number of rides
df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time_in_m)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides Throughout the Week", 
       x = "Day of the Week", y = "Number of Rides", 
       fill = "Member x Casual")

# Let's visualize the average ride duration
df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_time_in_m)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration Throughout the Week", 
       x = "Day of the Week", y = "Duration in Minutes", 
       fill = "Member x Casual")

# Let's visualize the time of the day the rides happen
df %>% 
  group_by(member_casual, hour_of_day) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, hour_of_day) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides Throughout the Day", 
       x = "Hour of the Day", y = "Number of Rides", 
       fill = "Member x Casual")

# Time of the day: Weekend x Weekday
df %>% 
  mutate(day = ifelse(weekday == "Sat" | weekday == "Sun", "weekend", "weekday")) %>% 
  group_by(member_casual, hour_of_day, day) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, hour_of_day) %>% 
  ggplot(aes(x = hour_of_day, y= number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  facet_grid(~ day) +
  labs(title = "Number of Rides: Weekday x Weekend", 
       x = "Hour of the Day", y = "Number of Rides", 
       fill = "Member x Casual")

# Let's visualize the month with the most rides
df %>% 
  group_by(member_casual, year_month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, year_month) %>% 
  ggplot(aes(x = year_month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Month", 
       x = "Year/Month", y = "Number of Rides", 
       fill = "Member x Casual") 



# Tibbles for later visualizations
counts <- aggregate(df$ride_time_in_m ~ df$member_casual + df$weekday, FUN = mean)

counts2 <- df %>% 
  group_by(member_casual, year_month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(year_month, member_casual)

counts3 <- df %>% 
  group_by(member_casual, hour_of_day) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, hour_of_day)

counts4 <- df %>% 
  group_by(member_casual, weekday,) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(weekday, member_casual)

fwrite(counts, "average_ride_time.csv")
fwrite(counts2, "rides_by_month.csv")
fwrite(counts3, "rides_by_hour.csv")
fwrite(counts4, "rides_by_weekday.csv")
