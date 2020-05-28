#1. Loading and preprocessing the data
##Show any code that is needed to
###1.1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
###1.2. Process/transform the data (if necessary) into a format suitable for your analysis
library(lubridate)
library(RCurl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(knitr)
FileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
ZipFile <- "./M5RR-Week2-Assignment/Data.zip"
FileDir <- "./M5RR-Week2-Assignment"
Path <- "./M5RR-Week2-Assignment/Data"
if (!file.exists(FileDir)){
  dir.create(FileDir)
}
download.file(FileUrl, file.path(ZipFile))
unzip(ZipFile, exdir = Path)
Data <- file.path(Path, "activity.csv")
activity <- read.csv(Data)

##For this part of the assignment, you can ignore the missing values in the 
##dataset.
###2. Calculate the total number of steps taken per day
TotalStepsPerDay <- activity %>%
  group_by(date) %>%
  summarise(stepsperday = sum(steps))
head(TotalStepsPerDay, 5)

#2.1. What is mean total number of steps taken per day?
summary(TotalStepsPerDay)
TotalStepsPerDay_Mean <- TotalStepsPerDay %>%
  summarise(mean = mean(stepsperday, na.rm = TRUE))
TotalStepsPerDay_Mean

###2.2 If you do not understand the difference between a histogram and a barplot,
####research the difference between them. Make a histogram of the total number of
####steps taken each day.
ggplot(data = TotalStepsPerDay, aes(x = TotalStepsPerDay$stepsperday), rm.na = TRUE) +
  geom_histogram(binwidth = 500, col = "black", fill = "red") +
  labs(x = "Total Daily Steps", y = "Frequency (binwidth 500)",
       title = "Total Number of Steps Taken Each Day")

###2.3 Calculate and report the mean and median of the total number of steps taken per day
TotalStepsPerDay_MeanMedian <- TotalStepsPerDay %>%
  summarise(mean = mean(stepsperday, na.rm = TRUE),
            median = median(stepsperday, na.rm = TRUE))
TotalStepsPerDay_MeanMedian

#3. What is the average daily activity pattern?
activitypattern <- activity %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))
summary(activitypattern)

###3.1 Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") 
####of the 5-minute interval (x-axis) and the average number of steps taken,
####averaged across all days (y-axis).
ggplot(data = activitypattern, aes(x = interval, y = mean_steps), rm.na = TRUE) +
  geom_line() +
  labs(x = "Interval (5 Minutes)", y = "Mean Steps",
       title = "Daily Activity Pattern")

###3.2 Which 5-minute interval, on average across all the days in the dataset, 
####contains the maximum number of steps?
activitypattern
MaxSteps <- activitypattern[order(-activitypattern$mean_steps),]
MaxSteps

#4 Imputing missing values
##Note that there are a number of days/intervals where there are missing values
##(coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce
##bias into some calculations or summaries of the data.
###4.1 Calculate and report the total number of missing values in the dataset 
####(i.e. the total number of rows with \color{red}{\verb|NA|}NAs).
summary(activity)

###4.1 Devise a strategy for filling in all of the missing values in the dataset.
####The strategy does not need to be sophisticated. For example, you could use
####the mean/median for that day, or the mean for that 5-minute interval, etc.
plot1 <- ggplot(activity, aes(activity$interval, activity$steps), rm.na = TRUE) +
  geom_boxplot()
plot2 <- ggplot(activity, aes(activity$steps), rm.na = TRUE) +
  geom_histogram()
grid.arrange(plot1, plot2, nrow = 2)
summary(activity)
###4.2 Create a new dataset that is equal to the original dataset but with the
####missing data filled in.

Na_to_Median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
activity2 <- activity %>% group_by(interval) %>% mutate(steps = Na_to_Median(steps))
summary(activity)
summary(activity2)
###4.3 Make a histogram of the total number of steps taken each day and Calculate 
####and report the mean and median total number of steps taken per day. Do these
####values differ from the estimates from the first part of the assignment? What
####is the impact of imputing missing data on the estimates of the total daily
####number of steps?
ggplot(data = activity2, aes(x = activity2$steps)) +
  geom_histogram(binwidth = 30, col = "black", fill = "yellow") +
  labs(x = "Total Daily Steps", y = "Frequency (binwidth 30)",
       title = "Total Number of Steps Taken Each Day")
summary(activity)
summary(activity2)
#5. Are there differences in activity patterns between weekdays and weekends?
##For this part the \color{red}{\verb|weekdays()|}weekdays() function may be 
##of some help here. Use the dataset with the filled-in missing values for this part.
###5.1 Create a new factor variable in the dataset with two levels – “weekday” 
####and “weekend” indicating whether a given date is a weekday or weekend day.
day_of_the_week <- wday(activity2$date)
f <- function(i) {
  day_to_category <- c('weekend',
                       'weekday',
                       'weekday',
                       'weekday',
                       'weekday',
                       'weekday',
                       'weekend')
  return (day_to_category[i])
}

activity2$time_of_week <- sapply(day_of_the_week, f)
str(activity2)
activity2

stepsperday_weekday <- activity2 %>%
  filter(time_of_week == 'weekday') %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))
summary(stepsperday_weekday)

stepsperday_weekend <- activity2 %>%
  filter(time_of_week == 'weekend') %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))
summary(stepsperday_weekend)

###5.2 Make a panel plot containing a time series plot 
####(i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval 
####(x-axis) and the average number of steps taken, averaged across all weekday
####days or weekend days (y-axis). See the README file in the GitHub repository
####to see an example of what this plot should look like using simulated data.
#Add a categorical variable for weekday vs weekend analysis
plot3 <- ggplot(data = stepsperday_weekday, aes(x = steps)) +
  geom_histogram(binwidth = 10, col = "black", fill = "white") +
  labs(x = "Total Daily Steps", y = "Frequency (binwidth 10)",
       title = "Total Steps Taken on Weekday")
plot4 <- ggplot(data = stepsperday_weekend, aes(x = steps)) +
  geom_histogram(binwidth = 10, col = "black", fill = "white") +
  labs(x = "Total Daily Steps", y = "Frequency (binwidth 10)",
       title = "Total Steps Taken on Weekend")
grid.arrange(plot3, plot4, nrow = 2)
