#Load data set
setwd("C:/Users/Jirawech/Documents/#5assignment1")
library(dplyr)
library(ggplot2)
library(impute)
mydata<-read.csv("activity.csv", header = TRUE)


#What is mean total number of steps taken per day?
data1<- mydata %>% group_by(date) %>% summarise(steps=sum(steps, na.rm=TRUE))
data1<-transform(data1, date=strptime(date, format="%Y-%m-%d"))
qplot(data1$steps, geom = 'histogram', bins = 30 )+labs(x= "total number of steps taken each day")

meanPDay<- mean(data1$steps)
medPDay<- median(data1$steps)
#meanPerDay <- mydata %>% group_by(date) %>% summarise(steps=mean(steps, na.rm=TRUE))
#medianPerDay<- mydata %>% group_by(date) %>% summarise(steps=median(unique(steps), na.rm=TRUE))
#qplot(meanPerDay$steps, geom = 'histogram', bins = 30 ) + 
#  geom_line(aes(y = step, colour = "var0"), data = meanPerDay) + 
#  geom_line(aes(y = step, colour = "var1"), data = meanPerDay)

#qplot(date, steps, data = meanPerDay) +scale_x_date(format = "%b-%Y")

#What is the average daily activity pattern?

avgPattern<- mydata %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm=TRUE))
ggplot(data = avgPattern, aes(x=interval, y = steps)) + geom_line()+labs(x = "Average steps", y="5 minute interval")
avgPattern[avgPattern$steps == max(avgPattern$steps),]

#Imputing missing values
missingVal <- is.na(mydata$steps)
table(missingVal)
mean(missingVal)

meanPerDay <- mydata %>% group_by(date) %>% summarise(steps=mean(steps, na.rm=TRUE))
totalMean<-mean(mydata$steps,na.rm=TRUE)

newdata<-mydata
newdata[is.na(newdata)] <- totalMean
newTotal<-newdata %>% group_by(date) %>% summarise(steps=sum(steps, na.rm=TRUE))
qplot(newTotal$steps, geom = 'histogram', bins = 30 )+labs(x= "New total number of steps taken each day")
newmeanPDay<- mean(newTotal$steps)
newmedPDay<- median(newTotal$steps)

#Are there differences in activity patterns between weekdays and weekends?
fdata<-newdata %>% mutate( weekends.or.weekdays = as.POSIXlt(newdata$date)$wday)
fdata$weekends.or.weekdays<-ifelse(fdata$weekends.or.weekdays %in% c(0,6), 'Weekends', "Weekdays")
#fdata$weekends.or.weekdays[fdata$weekends.or.weekdays == 5 | fdata$weekends.or.weekdays ==6 ] = "Weekends"
#fdata$weekends.or.weekdays[fdata$weekends.or.weekdays != 5 | fdata$weekends.or.weekdays !=6] = "Weekdays"
result <- aggregate(steps ~ interval+ weekends.or.weekdays , data=fdata, mean)
ggplot(aes(interval, steps),data = result) +facet_grid( weekends.or.weekdays~.)+geom_line()


