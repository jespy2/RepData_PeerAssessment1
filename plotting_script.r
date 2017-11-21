library(dplyr)

maindata<-read.csv("./activity.csv")
maindata$date<-as.Date(maindata$date, format = "%Y-%m-%d")

######
###### First Plot:  Total Steps Per Day
######

##remove nas
maindata.nona<-na.omit(maindata)
##create dataframe with total steps by date
sumdata<-maindata.nona %>%
  group_by(date) %>%
  summarize(dailysums = sum(steps))
##plot total steps with a histogram
png("Total_Steps_Per_Day.png", width = 480, height = 480)
hist(sumdata$dailysums, col="grey", main="Total Steps Per Day", xlab="Steps")
dev.off()

######
###### Second Plot:  Average steps by interval
######
suminterval<-maindata.nona %>%
  group_by(interval) %>%
  summarize(intervalmeans = mean(steps))
##generate plot
png("Avg_Steps_Per_Interval.png", width = 480, height = 480)
plot(suminterval$interval, suminterval$intervalmeans, type="l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Number of Steps Per Interval")
dev.off()

######
###### Third Plot:  Total Number of Steps Comparison--Imputed vs. Original Data
######

##Generate new means data with nas replaced with means for that particular interval across dates
sum(is.na(maindata$steps))
##identify obserations with nas
natrack<-is.na(maindata$steps)
##determine means per interval without nas
avg.inter<-tapply(maindata$steps, maindata$interval, mean, na.rm=TRUE)
##create new dataframe, then replace nas in an interval with means of that interval
data.nonas<-maindata
data.nonas$steps[natrack]<-avg.inter[as.character(maindata$interval[natrack])]
##create dataframe with sum of steps for each interval
sumdata.nonas<-data.nonas %>%
  group_by(date) %>%
  summarize(dailysums = sum(steps))
##plot data in a comparative histogram with original and new data
png("Imputed_vs_Original.png", width = 480, height = 480)
hist(sumdata.nonas$dailysums, col="red", main="Total Steps Per Day", xlab="Steps")
hist(sumdata$dailysums, col="grey", main="Total Steps Per Day", xlab="Steps", add=T)
legend("topright", c("new data", "original data"), fill = c("red", "grey"))
dev.off()

######
###### Fourth Plot:  Weekday vs. Weekend Activity
######

##create variable to track day of the week
day.data<-maindata
day.data$day<-weekdays(day.data$date)
##create variable to track weekday vs. weekend
weekenddays<-c("Saturday", "Sunday")
day.data$day.category = as.factor(ifelse(is.element(day.data$day, weekenddays), "weekend", "weekday"))
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
png("Weekend_vs_Weekday.png", width = 480, height = 480)
par(mfrow=c(2,1), mar=c(4,4,3,2))
plot(wkndmeans$interval, wkndmeans$intervalmeans, type="l", col="red", main = "Weekend Avg Steps Per Interval", xlab="Interval", ylab="Average Number of Steps", ylim=c(0, 230))
plot(wkdymeans$interval, wkdymeans$intervalmeans, type="l", col="blue", main = "Weekday Avg Steps Per Interval", xlab="Interval", ylab="Average Number of Steps", ylim=c(0, 230))
dev.off()