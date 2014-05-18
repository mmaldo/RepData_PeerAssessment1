data<-read.csv("activity/activity.csv")
data[,"date"]<-as.Date(data$date)
library(plyr)

## make scatter plot of steps vs date
plot<-ggplot(data,aes(x=date,y=steps))+ geom_point()
box <- ggplot(data, aes(x=weekdays(date), y=steps) + geom_boxplot()
              
## Count NA

sum(is.na(data))             
## Calculate mean total steps
mean(data$steps,na.rm=TRUE)
## SD
sd(data$steps,na.rm=TRUE)

sum(newdata$steps)/nrow(newdata)

## subset by intervals
interval5<-data[data$interval==5,]
interval5Monday<-interval5[weekdays(interval5$date)=="Monday",]




hist(data[weekdays(data$date)=="Monday",]$steps)hist(data[weekdays(data$date)=="Tuesday",]$steps)
hist(data[weekdays(data$date)=="Wednesday",]$steps)
hist(data[weekdays(data$date)=="Thursday",]$steps)
hist(data[weekdays(data$date)=="Friday",]$steps)
hist(data[weekdays(data$date)=="Saturday",]$steps)
hist(data[weekdays(data$date)=="Sunday",]$steps)

averageStepsPerDay<-ddply(data,.(date),summarize,mean(steps,na.rm=TRUE))
averageStepsPerinterval<-ddply(data,.(interval),summarize,meanSteps=mean(steps,na.rm=TRUE))

hist(averageStepsPerDay$meanSteps)
hist(averageStepsPerinterval$meanSteps)
hist(totalStepsPerDay$Total)
mean(totalStepsPerDay$Total)
median(totalStepsPerDay$Total)
summary(totalStepsPerDay$Total)
bin=range(totalStepsPerDay$Total)/7
ggplot(totalStepsPerDay,aes(x=Total))+geom_histogram(binwidth = bin[2])
ggplot(totalStepsPerDay,aes(x=Total))+geom_histogram(aes(fill = ..count..),binwidth = bin[2])

ggplot(averageStepsPerinterval, aes(x=interval, y=meanSteps))+geom_line(aes(colour = meanSteps),size=1)+ scale_colour_gradient(high="#56B4E9",low="#D55E00")