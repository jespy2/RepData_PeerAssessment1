Reproducible Research Project One
---------------------------------
##Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data [52K]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as *NA*)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken
* The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



##The Project
###Loading and preprocessing the data


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
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
maindata<-read.csv("./activity.csv")
maindata$date<-as.Date(maindata$date, format = "%Y-%m-%d")
```


###What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

**1. Calculate the total number of steps taken per day.  Make a histogram of the total number of steps taken each day**

```r
##remove nas
maindata.nona<-na.omit(maindata)
##create dataframe with total steps by date
sumdata<-maindata.nona %>%
  group_by(date) %>%
  summarize(dailysums = sum(steps))
##plot total steps with a histogram
hist(sumdata$dailysums, col="grey", main="Total Steps Per Day", xlab="Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)


**2. Calculate and report the mean and median of the total number of steps taken per day**

```r
mean(sumdata$dailysums)
```

```
## [1] 10766.19
```

```r
median(sumdata$dailysums)
```

```
## [1] 10765
```


###What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
##create dataframe with means of steps per interval
suminterval<-maindata.nona %>%
  group_by(interval) %>%
  summarize(intervalmeans = mean(steps))
##generate plot
plot(suminterval$interval, suminterval$intervalmeans, type="l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Number of Steps Per Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
suminterval[which.max(suminterval$intervalmeans),]
```

```
## # A tibble: 1 x 2
##   interval intervalmeans
##      <int>         <dbl>
## 1      835      206.1698
```

**Answer:**  The most steps were observed during interval 835 which had a mean of 206.1698 steps.

###Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

```r
##Generate new means data with nas replaced with means for that particular interval across dates
sum(is.na(maindata$steps))
```

```
## [1] 2304
```

**Answer:**  There are 2304 missing values (NAs) in the dataset

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
##identify obserations with nas
natrack<-is.na(maindata$steps)
##determine means per interval without nas
avg.inter<-tapply(maindata$steps, maindata$interval, mean, na.rm=TRUE)
##create new dataframe, then replace nas in an interval with means of that interval
data.nonas<-maindata
data.nonas$steps[natrack]<-avg.inter[as.character(maindata$interval[natrack])]
```

**Strategy:**  I used the suggested approach of determining the mean of steps taken in each interval across days, and then applied that in place of the NAs in each respective interval.

**3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
##create dataframe with sum of steps for each interval
sumdata.nonas<-data.nonas %>%
  group_by(date) %>%
  summarize(dailysums = sum(steps))
##plot data in a comparative histogram with original and new data
hist(sumdata.nonas$dailysums, col="red", main="Total Steps Per Day", xlab="Steps")
hist(sumdata$dailysums, col="grey", main="Total Steps Per Day", xlab="Steps", add=T)
legend("topright", c("new data", "original data"), fill = c("red", "grey"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)



```r
##Compare mean and median of new data
summary(sumdata$dailysums)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
summary(sumdata.nonas$dailysums)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```



```r
##Compare total steps of new data
sum(sumdata$dailysums)
```

```
## [1] 570608
```

```r
sum(sumdata.nonas$dailysums)
```

```
## [1] 656737.5
```

```r
sum(sumdata.nonas$dailysums)-sum(sumdata$dailysums)
```

```
## [1] 86129.51
```

**Conclusion:**  The strategy I choose had no impact on the mean and increased the median by one.  The total steps was increased by 86,129 

###Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
##create variable to track day of the week
day.data<-maindata
day.data$day<-weekdays(day.data$date)

##create variable to track weekday vs. weekend
weekenddays<-c("Saturday", "Sunday")
day.data$day.category = as.factor(ifelse(is.element(day.data$day, weekenddays), "weekend", "weekday"))
```


**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
##find average steps per interval across weekend days
wkndinterval<-day.data[which(day.data$day.category=="weekend"),] 
wkndinterval<-na.omit(wkndinterval)
wkndmeans<-wkndinterval%>%
  group_by(interval) %>%
  summarize(intervalmeans = mean(steps))

##find average steps per interval across weekdays
wkdyinterval<-day.data[which(day.data$day.category=="weekday"),] 
wkdyinterval<-na.omit(wkdyinterval)
wkdymeans<-wkdyinterval%>%
  group_by(interval) %>%
  summarize(intervalmeans = mean(steps))

##Create line plots to compare average steps per interval across weekend vs. weekday days
par(mfrow=c(2,1), mar=c(4,4,3,2))
plot(wkndmeans$interval, wkndmeans$intervalmeans, type="l", col="red", main = "Weekend Avg Steps Per Interval", xlab="Interval", ylab="Average Number of Steps", ylim=c(0, 230))
plot(wkdymeans$interval, wkdymeans$intervalmeans, type="l", col="blue", main = "Weekday Avg Steps Per Interval", xlab="Interval", ylab="Average Number of Steps", ylim=c(0, 230))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

**Conclusion:**  Overall, subjects were more active on weekends, though there is a marked spike of activity early in the day on weekdays.
