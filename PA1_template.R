#Load required libraries.
#library(dplyr)
#library(stringr)
library(ggplot2)
#library(scales)


#First set the directory where the data is located.
directory <- "/Users/brettaddison/Dropbox/Coursera_data_science/Reproducible_Research/Week1/project/"
filename_histo_total_steps <- paste0(directory, 'total_steps.png')
filename_histo_total_steps1 <- paste0(directory, 'total_steps1.png')
filename_line_plot_steps_interval <- paste0(directory, 'ave_steps_interval.png')
filename_histo_total_steps_no_NA <- paste0(directory, 'total_steps_na_replaced.png')
filename_histo_total_steps_no_NA1 <- paste0(directory, 'total_steps_na_replaced1.png')
filename_line_plot_steps_interval_day_type <- paste0(directory, 'ave_steps_interval_day_type.png')
html_file_loc <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
activity_data_zipfile <- paste0(directory, 'repdata-data-activity.zip')




#-----------Questions 1-3--------------#

#Download required activities file.
download.file(html_file_loc, activity_data_zipfile, method = 'curl')

#Unzip file.
activity_file <- unzip(activity_data_zipfile, exdir = directory)

#Read in unzipped activity data file.
activity_data_raw <- read.csv(activity_file, header = TRUE)

#Calculate the total number of steps taken per day (ignoring missing values).
#Sum by day using aggregate function. Missing values are ignored which will cause dates with all missing values
#to be removed from the dataframe!
activity_step_total <- aggregate(steps ~ date, data = activity_data_raw, FUN = "sum", na.rm=TRUE)

#Create a histogram of the total number of steps taken each day.
#Open the png device.
png(file = filename_histo_total_steps, width = 3000, height = 1000)

#geom_bar is used instead of geom_historgram since this is a discrete dataset, not a continuous one.
#Remove space between bars by setting width=1. This will simulate a histogram.
ggplot(data=activity_step_total, aes(x=date, y=steps)) + geom_bar(stat="identity", fill='red', width=1)  +
ggtitle("Total Steps Per Day")

#Close and writeout the plot.
dev.off()

#Another approach to creating a histogram with frequency.
#Open the png device.
png(file = filename_histo_total_steps1, width = 1000, height = 1000)

ggplot(data=activity_step_total, aes(steps)) + geom_histogram(fill='red', bins=length(activity_step_total$steps)/5)  +
  ggtitle("Frequency of the total steps taken per day")

#Close and writeout the plot.
dev.off()

#Calculate the mean and median of the total number of steps taken per day.
mean_steps = mean(activity_step_total$steps)
print(paste0("Mean number of total steps taken per day: ", mean_steps))
median_steps = median(activity_step_total$steps)
print(paste0("Median number of total steps taken per day: ", median_steps))


#-----------Questions 4-5--------------#
#Convert the interval time to just minutes. The current format (0000) is based on the 24 hour timescale
#i.e., hh:mm.
activity_data_raw_modulus <- activity_data_raw
for (i in 1:length(activity_data_raw_modulus$interval))
{
  activity_data_raw_modulus$interval[i] <- activity_data_raw_modulus$interval[i] - 
                                           ((45*floor(activity_data_raw_modulus$interval[i]/100)) - 
                                           floor(activity_data_raw_modulus$interval[i]/100)*5)
}

#Calculate the average number of steps for each five minute interval (ignoring missing values).
#Average by interval using aggregate function.
activity_data_interval_ave <- aggregate(steps ~ interval, data = activity_data_raw_modulus, FUN = "mean", na.rm=TRUE)

#Create a time series plot of the average number steps taken for each of the five minute intervals.
#Open the png device.
png(file = filename_line_plot_steps_interval, width = 1000, height = 1000)

ggplot(data=activity_data_interval_ave, aes(x=interval, y=steps)) + geom_line(col='red')  +
ggtitle("Average number of steps per five minute interval") + xlab("5 minute interval") + ylab("Average number of steps")

#Close and writeout the plot.
dev.off()

#Which 5-minute interval, averaged across all the days, contains the maximum number of steps?
#Use maximum function.
max_value <- max(activity_data_interval_ave$steps)
print(paste0("The maximum average step value is: ", max_value))

where_max_value <- which.max(activity_data_interval_ave$steps)
print(paste0("The location of the maximum average step value: ", where_max_value))

max_value_interval <- activity_data_interval_ave$interval[where_max_value]
print(paste0("The interval of the maximum average step value: ", max_value_interval))


#-----------Questions 6-7--------------#
#Determine the number of NAs in the data.
num_na <- sum(is.na(activity_data_raw$steps))
print(paste0("The number of NAs in the dataset: ", num_na))

#The strategy I propose for filling the NAs is to use the mean of each particular 5-minute interval.
interval_list <- unique(activity_data_raw_modulus$interval)

#Now we will loop through the different intervals and check each date for missing step values.
#Missing values will be filled with the corresponding median value.
activity_data_modulus_no_NA <- activity_data_raw_modulus
for (i in 1:length(interval_list))
{
  for (j in 1:length(activity_data_raw_modulus$interval))
  {
    if (is.na(activity_data_raw_modulus$steps[j]) && activity_data_raw_modulus$interval[j] == interval_list[i])
    {
      #Replace the NAs if the particular step in the dataset has an NA and the interval at the step
      #matches the interval_list. Rounding steps to the nearest whole number.
      activity_data_modulus_no_NA$steps[j] <- round(activity_data_interval_ave$steps[i])
    }
  }
}

activity_data_modulus_no_NA$steps <- as.numeric(activity_data_modulus_no_NA$steps)

activity_step_total_no_NA <- aggregate(steps ~ date, data = activity_data_modulus_no_NA, FUN = "sum", na.rm=TRUE)

#Create a histogram of the total number of steps taken each day with NAs replaced.
#Open the png device.
png(file = filename_histo_total_steps_no_NA, width = 3000, height = 1000)

#geom_bar is used instead of geom_historgram since this is a discrete dataset, not a continuous one.
#Remove space between bars by setting width=1. This will simulate a histogram.
ggplot(data=activity_step_total_no_NA, aes(x=date, y=steps)) + geom_bar(stat="identity", fill='red', width=1)  +
  ggtitle("Total Steps Per Day")

#Close and writeout the plot.
dev.off()

#Another approach to creating a histogram with frequency.
#Open the png device.
png(file = filename_histo_total_steps_no_NA1, width = 1000, height = 1000)

ggplot(data=activity_step_total_no_NA, aes(steps)) + geom_histogram(fill='red', bins=10)  +
  ggtitle("Frequency of the total steps taken per day")

#Close and writeout the plot.
dev.off()

#Calculate the mean and median of the total number of steps taken per day.
mean_steps_no_NA = mean(activity_step_total_no_NA$steps)
print(paste0("Mean number of total steps taken per day: ", mean_steps_no_NA))
median_steps_no_NA = median(activity_step_total_no_NA$steps)
print(paste0("Median number of total steps taken per day: ", median_steps_no_NA))


#-----------Questions 8-9--------------#
#Compare activity levels between weekdays and weekends.
#Convert the date column in dateset with NAs replaced.
activity_data_modulus_no_NA$date <- as.Date(activity_data_modulus_no_NA$date)

#Create new factor with day of the week.
activity_data_modulus_no_NA$day <- weekdays(activity_data_modulus_no_NA$date)

#Create new factor indicating whether the day is a weekday or weekend.
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

#Check if day matches the weekdays vector. If it does, then the day is a weekday.
#Otherwise the day is a weekend.
activity_data_modulus_no_NA$day_type <- c('weekend', 'weekday')[(activity_data_modulus_no_NA$day %in% weekdays)+1L]

#Calculate the average number of steps for each five minute interval for weekdays and weekends.
#Average by interval using aggregate function.
activity_data_interval_ave_day_type <- aggregate(steps ~ interval + day_type, data = activity_data_modulus_no_NA, FUN = "mean", na.rm=TRUE)

#Create a time series plot of the average number steps taken for each of the five minute intervals for weekdays and weekends.
#Open the png device.
png(file = filename_line_plot_steps_interval_day_type, width = 1000, height = 1000)

ggplot(data=activity_data_interval_ave_day_type, aes(x=interval, y=steps)) + geom_line(col='red')  +
  ggtitle("Average number of steps per five minute interval") + xlab("5 minute interval") + ylab("Average number of steps") +
  facet_wrap(~ day_type, ncol=1)

#Close and writeout the plot.
dev.off()