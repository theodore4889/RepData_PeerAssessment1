
## Load libraries and dataset  
library(dplyr)  
data <- read.table(unz("./R/R/activity.zip", "activity.csv"), header=T, quote="\"", sep=",")  


## Mean Total Number of steps` 
### Aggregate data by getting mean total number of steps in a day  
data.dateSum <- aggregate(x = data[c("steps")],  
                      FUN = sum,  
                      by = list(date = data$date))  

### Histogram of total steps  
hist(data.dateSum$steps, 15, xlab = "Total steps in a day", main = "Histogram of total steps in a day")  

### Get mean and median values of the total steps in a day  
steps_mean <- mean(data.dateSum$steps, na.rm=TRUE)  
steps_median <- median(data.dateSum$steps, na.rm=TRUE)  


## Average Daily Activity Pattern  
### Aggregate data by getting mean number of steps per interval (most active interval on average)  
data.timeMean <- aggregate(x = data[c("steps")],  
                          FUN = mean,  
                          na.rm = TRUE,  
                          by = list(interval = data$interval))  

### Line plot to show mean number of steps at given time intervals
plot(data.timeMean$interval, data.timeMean$steps, type = "l", xlab = "Time Interval", 
     ylab = "Mean Number of Steps", main = "Mean number of steps taken at measured time intervals")  

interval_stepsMax <- data.timeMean[data.timeMean$steps == max(data.timeMean$steps),]$interval  


## Replace Missing Data (NA values)  
### Strategy: separate data by day of the week and average the number of steps per given time  
###  interval.  Use these averaged time interval values by day to replace appropriate NA values

### Get count of NA values  
n_NA <- sum(is.na(data$steps))  

### Add Monday, Tuesday, Wednesday, etc specification to data points
data$day <- weekdays(as.Date(data$date))

### For non NA values, group data by day of the week and time interval, then find means of those groups
data.dayInt <- data[complete.cases(data),] %>%
  group_by(day, interval) %>%
  summarize(meanSteps = mean(steps))

### Copy raw data
data.replaceNa <- data

### Replace NA values with mean values grouped by day of week and time interval
data.replaceNa$steps[is.na(data.replaceNa$steps)] <- data.dayInt$meanSteps[
                                                match(data.replaceNa$day, data.dayInt$day)
                                              & match(data.replaceNa$interval, data.dayInt$interval)
                                             ]

### For some reason, this command needs to be run twice in order to get all of the NA values
data.replaceNa$steps[is.na(data.replaceNa$steps)] <- data.dayInt$meanSteps[
                                                match(data.replaceNa$day, data.dayInt$day)
                                              & match(data.replaceNa$interval, data.dayInt$interval)
                                             ]

### Aggregate data by getting mean number of steps per interval  
data.dateSumReplaceNa <- aggregate(x = data.replaceNa[c("steps")],
                          FUN = sum, 
                          by = list(date = data.replaceNa$date))

# Histogram of total steps
hist(data.dateSumReplaceNa$steps, 15, xlab = "Total steps per day", main = "Histogram of total steps per day")

### Get mean and median values of the total steps in a day  
stepsRepNa_mean <- mean(data.dateSumReplaceNa$steps, na.rm=TRUE)
stepsRepNa_median <- median(data.dateSumReplaceNa$steps, na.rm=TRUE)

#### Both mean and median values increased when replacing NA values with values averaged for 
####  intervals as a function of the day of the week.

### By putting in this missing data, we are strengthing the impact of days which contained the most NA values.
### Data will skew more towards those days
table(data[is.na(data$steps),]$day)


# New variable that indicates if the day is a weekday or a weekend
data.replaceNa$weekday <- ifelse(substr(weekdays(as.Date(data.replaceNa$date)), 0,1) != 'S', "weekday", "weekend")

# Split data from weekdays and weekends
data.weekdays <- data.replaceNa[data.replaceNa$weekday == "weekday",]
data.weekends <- data.replaceNa[data.replaceNa$weekday == "weekend",]

data.meanWeekdays <- aggregate(x = data.weekdays[c("steps")],
                           FUN = mean,
                           na.rm = TRUE,
                           by = list(interval = data.weekdays$interval))

data.meanWeekends <- aggregate(x = data.weekends[c("steps")],
                               FUN = mean,
                               na.rm = TRUE,
                               by = list(interval = data.weekends$interval))

main_text <- "Mean number of steps taken at measured time intervals"
xlab_text <- "Time Interval"
ylab_text <- "Mean Number of Steps"
y_lim <- c(-5, 250)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1), oma = c( 0, 0, 2, 0 )) 

plot(data.meanWeekdays$interval, data.meanWeekdays$steps, type = "l", xlab = xlab_text, 
     ylab = ylab_text, ylim = y_lim, main = "weekdays")

plot(data.meanWeekends$interval, data.meanWeekends$steps, type = "l", xlab = xlab_text, 
     ylab = ylab_text,ylim = y_lim, main = "weekends")

title( main_text, outer = TRUE )
