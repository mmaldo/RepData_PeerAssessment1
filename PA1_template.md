# Reproducible Research: Peer Assessment 1

```r
library(lattice)
library(ggplot2)
library(mice)
library(plyr)
library(knitr)
library(grid)
library(gridExtra)
```

## Loading and preprocessing the data

```r
data <- read.csv("activity/activity.csv")
data[, "date"] <- as.Date(data$date)
```

## Count NA

```r
sum(is.na(data))
```

```
## [1] 2304
```

## What is mean total number of steps taken per day?

```r
totalStepsPerDay <- ddply(data, .(date), summarize, Total = sum(steps, na.rm = TRUE))
totalStepsPerDay
```

```
##          date Total
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```


## Create Histogram
## Determine Number of Bins

```r
class = nclass.Sturges(totalStepsPerDay$Total)
```

## Calculate bin width

```r
bin = range(totalStepsPerDay$Total)/class
```

## Plot Histogram 

```r
ggplot(totalStepsPerDay, aes(x = Total)) + geom_histogram(aes(fill = ..count..), 
    binwidth = bin[2])
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


## What is mean total number of steps taken per day?
## mean

```r
mean(totalStepsPerDay$Total)
```

```
## [1] 9354
```

## median

```r
median(totalStepsPerDay$Total)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
averageStepsPerinterval <- ddply(data, .(interval), summarize, meanSteps = mean(steps, 
    na.rm = TRUE))
```

## max mean Steps interval

```r
averageStepsPerinterval[which.max(averageStepsPerinterval$meanSteps), ]
```

```
##     interval meanSteps
## 104      835     206.2
```

## Plot Mean Steps vs interval

```r
ggplot(averageStepsPerinterval, aes(x = interval, y = meanSteps)) + geom_line(aes(colour = meanSteps), 
    size = 1) + scale_colour_gradient(high = "#56B4E9", low = "#D55E00")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


## Imputing missing values
Data was imputed with multiple imputation using the mice package. The method used was predictive mean matching. An extra column was added where the date column is converted to a factor and then a numeric. I did this to make imputing with mice package work. Results seem reasonable. However, more diagnostics should probably be done. Extra iterations should also probably be done to decrese the noise. Only 5 iterations were run.

```r
timeData = data.frame(data, time = as.numeric(factor(data$date)))
miceData = data.frame(steps = timeData$steps, date = timeData$time, interval = timeData$interval)
```



```r
imp <- imp <- mice(miceData, m = 5, seed = 23109)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
```


```r
stripplot(imp, pch = 20, cex = 1.2)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 



```r
xyplot(imp, steps ~ interval | .imp, pch = 20, cex = 1.4)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 



```r
imputedData = complete(imp)
recombinedData = data.frame(steps = imputedData$steps, date = data$date, interval = imputedData$interval)
```

## What is mean total number of steps taken per day?

```r
imputedTotalStepsPerDay <- ddply(recombinedData, .(date), summarize, Total = sum(steps))
```


## Create Histogram
## Determine Number of Bins

```r
class = nclass.Sturges(imputedTotalStepsPerDay$Total)
```

## Calculate bin width

```r
bin = range(imputedTotalStepsPerDay$Total)/class
```

## Plot Histogram 

```r
ggplot(imputedTotalStepsPerDay, aes(x = Total)) + geom_histogram(aes(fill = ..count..), 
    binwidth = bin[2])
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 


## What is mean total number of steps taken per day?
## Mean

```r
mean(imputedTotalStepsPerDay$Total)
```

```
## [1] 10783
```

## Median

```r
median(imputedTotalStepsPerDay$Total)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
recombinedData <- transform(recombinedData, week = ifelse(weekdays(data$date) %in% 
    c("Sunday", "Saturday"), "Weekend", "Weekday"))
```

Mean Steps in an interval with Imputed Data

```r
newaverageStepsPerinterval <- ddply(recombinedData, .(interval, week), summarize, 
    meanSteps = mean(steps))
```



```r
ggplot(newaverageStepsPerinterval, aes(x = interval, y = meanSteps)) + geom_line(aes(colour = meanSteps), 
    size = 1) + scale_colour_gradient(high = "#56B4E9", low = "#D55E00")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26.png) 

Comparing Weekend and Weeday Patterns with Imputed Data

```r
weekday = ggplot(newaverageStepsPerinterval, aes(x = interval, y = meanSteps)) + 
    geom_line(aes(colour = meanSteps), size = 1, subset = .(week == "Weekday")) + 
    scale_colour_gradient(high = "#56B4E9", low = "#D55E00") + ggtitle("Weekday")
```


```r
weekend = ggplot(newaverageStepsPerinterval, aes(x = interval, y = meanSteps)) + 
    geom_line(aes(colour = meanSteps), size = 1, subset = .(week == "Weekend")) + 
    scale_colour_gradient(high = "#56B4E9", low = "#D55E00") + ggtitle("Weekend")
```


```r
grid.arrange(weekday, weekend, ncol = 1)
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29.png) 

