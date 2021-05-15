---
title: "Reproducible Research: Course Project 1"
output: 
  html_document:
    keep_md: true
---

Huy NGUYEN  
May 2021 - Brussels - Belgium

# Introduction

  A complete description of this "Course Project 1" can be found in doc/instructions.pdf  
  This file was created with Rstudio 1.4.1106  

## Loading and preprocessing the data
1. Load the data (i.e. read.csv())  


```r
# Load libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

# Import data
unzip("activity.zip")
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
  

2. Process/transform the data (if necessary) into a format suitable for your analysis  
No need to transform data for now.


 
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
# Calculate the total number of steps taken per day
totalStepsByDay<-aggregate(steps~date, activity, sum)
hist(totalStepsByDay$steps, xlab="Total Number of Steps per day", 
     ylab="Number of Days", main="Distribution of Number of Steps taken per day",
     ylim = c(0, 30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
  
  
2. Calculate and report the mean and median total number of steps taken
per day


```r
# mean total number of steps taken
mean(totalStepsByDay$steps)
```

```
## [1] 10766.19
```

```r
# median total number of steps taken
median(totalStepsByDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)


```r
#Calculate the average number of steps taken per interval of 5 min
averageStepsbyInterval<-aggregate(steps~interval, activity, mean)

#Plot this average 
plot(averageStepsbyInterval$interval, averageStepsbyInterval$steps,
     xlab="Interval of 5 minutes", ylab="Average number of steps",
     main="Average number of steps per interval",
     xlim=c(0,2500), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```r
averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)


```r
#Calculate the total number of NAs
sum(is.na(activity$steps))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Choose to replace the NAs by the mean number of steps per interval.
m<-mean(averageStepsbyInterval$steps)
m
```

```
## [1] 37.3826
```


3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.


```r
#Create a vector specifying if the element is a NA
vecNA<-is.na(activity[,1])

# Create new data set, and replace the NAs 
newActivity <- activity 
newActivity[vecNA,1] <- m
head(newActivity)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. 


```r
#New total number of steps taken each day
newTotStepsByDay<-aggregate(steps~date, newActivity, sum)

hist(newTotStepsByDay$steps, xlab="Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day with NAs replaced")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

  Do these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?


```r
#New mean number of steps per day  
mean(newTotStepsByDay$steps)
```

```
## [1] 10766.19
```

```r
#New median number of steps per day
median(newTotStepsByDay$steps)
```

```
## [1] 10766.19
```

The new mean number of steps per day (10766.19) is the same as before imputing the NAs values, which is normal since the NAs values are just replaced by the mean value.  

The new median number of steps taken per day (10766.19) is slightly different than before (10765)  
 

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.


```r
# Set system locale to English, so running the program in others countries get same result
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
# Format date
newActivity$date<-as.Date(newActivity$date)

# Create factor variable with two levels – “weekday” and “weekend”
dayType <- ifelse(weekdays(newActivity$date)=="Saturday" | weekdays(newActivity$date)=="Sunday", "weekend", "weekday")

# Add column dayType to activity
newActivity <- cbind(newActivity,dayType)

head(newActivity)
```

```
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis). The plot should look
something like the following, which was creating using simulated data:


```r
# Calculate the average number of steps taken by interval and dayType ("weekend" or "weekday")
averageByDayType <- aggregate(steps~interval + dayType, newActivity, mean, na.rm = TRUE)

# Plot it
ggplot(averageByDayType, aes(x = interval , y = steps, color = dayType)) +
  geom_line() +
  labs(title = "Average daily steps by weekday or weekend", x = "Interval", y = "Average number of steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#Close device
dev.off()
```

```
## null device 
##           1
```

