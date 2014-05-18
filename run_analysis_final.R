# Reproducible Research: Peer Assessment 1
library(plyr)



## Loading and preprocessing the data

data<-read.csv("activity/activity.csv")
data[,"date"]<-as.Date(data$date)
## Count NA
sum(is.na(data))   

## What is mean total number of steps taken per day?
averageStepsPerDay<-ddply(data,.(date),summarize,mean(steps,na.rm=TRUE))
totalStepsPerDay<-ddply(data,.(date),summarize,Total=sum(steps,na.rm=TRUE))
## Create Histogram
## Determine Number of Bins
class = nclass.Sturges(totalStepsPerDay$Total)
## Calculate binWidth
bin=range(totalStepsPerDay$Total)/class
## Plot Histogram 
ggplot(totalStepsPerDay,aes(x=Total))+geom_histogram(aes(fill = ..count..),binwidth = bin[2])
## mean
mean(totalStepsPerDay$Total)
## median
median(totalStepsPerDay$Total)

## What is the average daily activity pattern?
averageStepsPerinterval<-ddply(data,.(interval),summarize,meanSteps=mean(steps,na.rm=TRUE))
## max mean Steps interval
averageStepsPerinterval[which.max(averageStepsPerinterval$meanSteps),]

## Plot Mean Steps vs interval
ggplot(averageStepsPerinterval, aes(x=interval, y=meanSteps))+geom_line(aes(colour = meanSteps),size=1)+ scale_colour_gradient(high="#56B4E9",low="#D55E00")

## Imputing missing values
missing.pattern.plot(data, gray.scale = TRUE)
info <- mi.info(data)
info
mi.preprocess(data, info)
info$imp.formula
info.upd <- update(info, "imp.formula", list("steps" = "steps ~ date + interval^2","interval"="interval~steps^2+date",
                                             "date"="date~steps^2+interval"))
info.upd <- update(info, "imp.formula", list("steps" = "steps ~ date + interval^2","interval"="interval~steps+date", 
                                             "date"="date~steps+interval^2"))
info.upd <- update(info, "imp.formula", list("steps" = "steps ~ date + interval^3","interval"="interval~steps^3+date", 
                                             "date"="date~steps^3+interval"))
info.upd$imp.formula
plot(IMP)

md.pattern(data)
p <- md.pairs(data)
p
library(VIM)
marginplot(data, col=c("blue","red","orange"), cex=1.5,cex.lab=1.5, cex.numbers=1.3, pch=19)
pbox(data,pos=3,int=FALSE,cex=1.2)
imp <- mice(data, m = 5, maxit = 0)
complete(imp)[1:50,]
library(lattice)
stripplot(steps~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20,cex=1.4, xlab="Imputation number")
transformedData<-complete(imp)

data3 = data.frame(data,time=as.numeric(factor(data$date)))
data4 = data.frame(data3$steps,data3$time,data3$interval)
imp <- mice(data4, seed = 23109)
stripplot(imp, pch = 20, cex = 1.2)
xyplot(imp, data3.steps ~ data3.interval | .imp, pch = 20, cex = 1.4)
nedata=complete(imp)
hist(nedata$data3.steps)
datanew = data.frame(nedata$data3.steps,data$date,nedata$data3.interval)
## For the nonnegative variable type, mi.preprocess() creates two ancillary variables. One
## is an indicator for which values of the nonnegative variable are bigger than 0. The other
## ancillary variable takes the log of such a variable on any value that is bigger than 0

## Are there differences in activity patterns between weekdays and weekends?

