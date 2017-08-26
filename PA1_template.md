# Reproducible Research: Peer Assessment 1

The data for this assignment was downloaded from the course web site:
Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
## Loading library and data
```sh
library("knitr")
library(dplyr)
library(ggplot2)
library(impute)
setwd("C:/Users/Jirawech/Documents/#5assignment1")
mydata<-read.csv("activity.csv", header = TRUE)
```
## What is mean total number of steps taken per day?
1. Process/transform the data for answering the question
```sh
data1<- mydata %>% group_by(date) %>% summarise(steps=sum(steps,na.rm=TRUE))
data1<-transform(data1, date=strptime(date, format="%Y-%m-%d"))
qplot(data1$steps, geom = 'histogram', bins = 30 )+labs(x= "total number of steps taken each day")
```
[](https://github.com/nomiinee/RepData_PeerAssessment1/blob/assignment/directory/plot1.png?raw=true)

# What is the average daily activity pattern?
```sh
avgPattern<- mydata %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm=TRUE))
ggplot(data = avgPattern, aes(x=interval, y = steps)) + geom_line()+labs(x = "Average steps", y="5 minute interval")
avgPattern[avgPattern$steps == max(avgPattern$steps),]
```
[](https://github.com/nomiinee/RepData_PeerAssessment1/blob/assignment/directory/plot2.png?raw=true)
# Imputing missing values
```sh
missingVal <- is.na(mydata$steps)
meanPerDay <- mydata %>% group_by(date) %>% summarise(steps=mean(steps, na.rm=TRUE))
totalMean<-mean(mydata$steps,na.rm=TRUE)
newdata<-mydata
newdata[is.na(newdata)] <- totalMean
newTotal<-newdata %>% group_by(date) %>% summarise(steps=sum(steps, na.rm=TRUE))
ggplot(data= newTotal, aes(x=date,y=steps, fill= as.POSIXlt(date)$wday), width = 30, fill = date )+labs(x= "New total number of steps taken each day")+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
newmeanPDay<- mean(newTotal$steps)
newmedPDay<- median(newTotal$steps)
```
[](https://github.com/nomiinee/RepData_PeerAssessment1/blob/assignment/directory/plot3.png?raw=true)
# Are there differences in activity patterns between weekdays and weekends?
```
fdata<-newdata %>% mutate( weekends.or.weekdays = as.POSIXlt(newdata$date)$wday)
fdata$weekends.or.weekdays<-ifelse(fdata$weekends.or.weekdays %in% c(0,6), 'Weekends', "Weekdays")
result <- aggregate(steps ~ interval+ weekends.or.weekdays , data=fdata, mean)
ggplot(aes(interval, steps),data = result) +facet_grid( weekends.or.weekdays~.)+geom_line()
```
[](https://github.com/nomiinee/RepData_PeerAssessment1/blob/assignment/directory/plot4.png?raw=true)


