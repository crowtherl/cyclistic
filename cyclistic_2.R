### Divvy_Exercise_Full_Year_Analysis ###

# This script is adapted from https://docs.google.com/document/
  # d/1TTj5KNKf4BWvEORGm10oNbpwTRk1hamsWJGj6qRWpuI/edit

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and 
  # Polished’: Divvy and Data Visualization" written by Kevin Hartman (found 
  # here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose 
  # of this script is to consolidate downloaded Divvy data into a single 
  # dataframe and then conduct simple analysis to help answer the key question:
  # “In what ways do members and casual riders use Divvy bikes differently?”

# Install required packages.
library(tidyverse)  # for data import and wrangling
library(lubridate)  # for date functions

# Set working directory to simplify calls to data.
setwd("C:/Users/crowt/Documents/capstone/data") 

#=====================
# STEP 1: COLLECT DATA
#=====================
# Read csv files for the last year of Cyclistic data.
# In the variable names, the "c" stands for "Cyclistic," followed by four
  # digits for the year and two digits for the month.
c202106 <- read_csv("202106-divvy-tripdata.csv")
c202107 <- read_csv("202107-divvy-tripdata.csv")
c202108 <- read_csv("202108-divvy-tripdata.csv")
c202109 <- read_csv("202109-divvy-tripdata.csv")
c202110 <- read_csv("202110-divvy-tripdata.csv")
c202111 <- read_csv("202111-divvy-tripdata.csv")
c202112 <- read_csv("202112-divvy-tripdata.csv")
c202201 <- read_csv("202201-divvy-tripdata.csv")
c202202 <- read_csv("202202-divvy-tripdata.csv")
c202203 <- read_csv("202203-divvy-tripdata.csv")
c202204 <- read_csv("202204-divvy-tripdata.csv")
c202205 <- read_csv("202205-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare the column names for each file.
# While the names don't have to be in the same order, they DO need to match
  # perfectly before we can use a command to join them into one file.
colnames(c202106)
colnames(c202107)
colnames(c202108)
colnames(c202109)
colnames(c202110)
colnames(c202111)
colnames(c202112)
colnames(c202201)
colnames(c202202)
colnames(c202203)
colnames(c202204)
colnames(c202205)

# All column names are the same.

# Inspect the dataframes and look for incongruencies.
str(c202106)
str(c202107)
str(c202108)
str(c202109)
str(c202110)
str(c202111)
str(c202112)
str(c202201)
str(c202202)
str(c202203)
str(c202204)
str(c202205)

# Stack each month's data frame into one big data frame.
all_trips <- bind_rows(
  c202106, c202107, c202108, c202109, c202110, c202111, c202112, c202201,
  c202202, c202203, c202204, c202205)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  # List of column names
nrow(all_trips)  # How many rows are in data frame?
dim(all_trips)  # Dimensions of the data frame?
head(all_trips)  # See the first 6 rows of data frame.  Also tail(all_trips)
tail(all_trips)
str(all_trips)  # See list of columns and data types (numeric, character, etc)
summary(all_trips)  # Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
  # The data can only be aggregated at the ride-level, which is too granular. We
    # will want to add some additional columns of data -- such as day, month,
    # year -- that provide additional opportunities to aggregate the data.
  # We will want to add a calculated field for length of ride.
  # There are some rides where tripduration shows up as negative, including 
    # several hundred rides where Divvy took bikes out of circulation for 
    # quality control reasons. We will want to delete these rides.

# See how many observations fall under each user type in the member_casual
  # column. This is to make sure only two user types are used.
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride.
# This will allow us to aggregate ride data for each month, day, or year. Before
  # completing these operations, we could only aggregate at the ride level.
# For more info on date formats in R, check out the following link:
  # https://www.statmethods.net/input/dates.html
all_trips$date <- as.Date(all_trips$started_at)
  # The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds).
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert ride_length to numeric so we can run calculations on the data.
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length) # It is.

# Remove "bad" data.
# The dataframe includes a few hundred entries when bikes were taken out of 
  # docks and checked for quality by Divvy or ride_length was negative.
# We will create a new version of the dataframe (v2) since data is being
  # removed.
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(
  all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
    # Creates weekday field using wday()
  group_by(member_casual, weekday) %>%  # groups by usertype and weekday
  summarise(number_of_rides = n() 
            # Calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%  
            # Calculates the average duration
  arrange(member_casual, weekday) # Sorts

# Let's visualize the number of rides by rider type.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create csv files that we will visualize in Excel or Tableau.
# https://datatofish.com/export-dataframe-to-csv-in-r/
# Average ride length by weekday
weekday_counts <- aggregate(
  all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
    all_trips_v2$day_of_week, FUN = mean)
write.csv(weekday_counts, file = 'avg_ride_length_by_weekday.csv')

# Average ride length by month
month_counts <- aggregate(
  all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
    all_trips_v2$month, FUN = mean)
write.csv(month_counts, file = 'avg_ride_length_by_month.csv')

# https://r-coder.com/aggregate-r/
# Rides by start station
start_station_counts <- aggregate(
  all_trips_v2$ride_id ~ all_trips_v2$member_casual +
    all_trips_v2$start_station_name, FUN = length)
write_csv(start_station_counts, file = 'rides_by_start_station.csv')

# Rides by end station
end_station_counts <- aggregate(
  all_trips_v2$ride_id ~ all_trips_v2$member_casual +
    all_trips_v2$end_station_name, FUN = length)
write_csv(end_station_counts, file = 'rides_by_end_station.csv')

# Average start latitude
start_latitude_counts <- aggregate(
  all_trips_v2$start_lat ~ all_trips_v2$member_casual, FUN = mean)
write.csv(start_latitude_counts, file = 'avg_start_latitude.csv')

# Average start longitude
start_longitude_counts <- aggregate(
  all_trips_v2$start_lng ~ all_trips_v2$member_casual, FUN = mean)
write.csv(start_longitude_counts, file = 'avg_start_longitude.csv')

# Average end latitude
end_latitude_counts <- aggregate(
  all_trips_v2$end_lat ~ all_trips_v2$member_casual, FUN = mean)
write.csv(end_latitude_counts, file = 'avg_end_latitude.csv')

# Average end longitude
end_longitude_counts <- aggregate(
  all_trips_v2$end_lng ~ all_trips_v2$member_casual, FUN = mean)
write.csv(end_longitude_counts, file = 'avg_end_longitude.csv')

# Number of rides by month
month_sum_counts <- aggregate(
  all_trips_v2$ride_id ~ all_trips_v2$member_casual +
    all_trips_v2$month, FUN = length)
write_csv(month_sum_counts, file = 'ride_sums_by_month.csv')

# Number of rides by day of the week
weekday_sum_counts <- aggregate(
  all_trips_v2$ride_id ~ all_trips_v2$member_casual +
    all_trips_v2$day_of_week, FUN = length)
write_csv(weekday_sum_counts, file = 'ride_sums_by_weekday.csv')
