# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
First set the working directory.

```r
setwd("/Users/waterman/Documents/Data Science Courses/Reproducible Research/Project1/RepData_PeerAssessment1")
```
Now reading the data into a dataframe.

```r
activitydata <- read.csv("./activity.csv", stringsAsFactors = FALSE)
```

## What is mean total number of steps taken per day?

The number of steps taken each day.

```r
library(plyr)
stepsperdaydata <- ddply(activitydata, c("date"), summarize, steps = sum(steps), rm.na=TRUE)
```
Histogram of the "Number of Steps per Day"

```r
hist(stepsperdaydata$steps, 
     main="Steps per day",
     xlab="Number of Steps",
     col="grey"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
<br>Calculating and printing the mean number of steps per day.

```r
meanstepsperday <- mean(stepsperdaydata$steps, na.rm = TRUE)
print(meanstepsperday)
```

```
## [1] 10766.19
```
Calculating and printing the median number of steps per day.

```r
medianstepsperday <- median(stepsperdaydata$steps, na.rm = TRUE)
print(medianstepsperday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Calculate the mean steps per interval (for this section NA have been removed which will impact the interpretation of the data).

```r
meanstepsperintervaldata <- ddply(activitydata, c("interval"), summarize, meansteps = mean(steps, na.rm = TRUE))
```
In this section the Time Series plot is produced.

```r
plot(meanstepsperintervaldata$interval, meanstepsperintervaldata$meansteps,
     type="l",
     main="Mean Steps per Interval", 
     ylab="Mean Steps",
     xlab="Interval"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
<br>This section of code sorts the intervals by size in descending order and prints the interval that contains the largest number of steps.

```r
maxinterval <- meanstepsperintervaldata[order(-meanstepsperintervaldata$meansteps), ]
head(maxinterval, n=1)
```

```
##     interval meansteps
## 104      835  206.1698
```
Based on the results the interval that on average has the larges number of steps is 835.

## Imputing missing values
Determining the number of rows with missing values.

```r
countNAs <- (sum(is.na(activitydata)))
print(countNAs)
```

```
## [1] 2304
```
Now replacing NA values with the mean value of the interval period
First there a function is created to do the replacement.
Second the function is call.
Third the data frame is reordered by date then interval.

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newactivitydata <- ddply(activitydata, ~ interval, transform, steps = impute.mean(steps))
newactivitydata <- newactivitydata[order(newactivitydata$date), ]
```
Recreating the Histogram of the "Number of Steps per Day"

```r
newstepsperdaydata <- ddply(newactivitydata, c("date"), summarize, steps = sum(steps), rm.na=TRUE)
hist(newstepsperdaydata$steps, 
     main="Steps per day",
     xlab="Number of Steps",
     col="grey"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
<br>Calculating new mean and median steps per day.

```r
newmeanstepsperday <- mean(newstepsperdaydata$steps, na.rm = TRUE)
print(newmeanstepsperday)
```

```
## [1] 10766.19
```

```r
newmedianstepsperday <- median(newstepsperdaydata$steps, na.rm = TRUE)
print(newmedianstepsperday)
```

```
## [1] 10766.19
```
The newly calculated mean is 1.076619\times 10^{4} and the newly calculated median is 1.076619\times 10^{4}. Changing the NAs to the mean of the interval value does not change the reported mean, however the median is shifted and is now equal to the original reported mean. <br><br>
The impact of imputing missing data on the estimates of the total daily number of steps would be dependent upon the question be asked. Based on the question you are trying to answer, you would need to decide whether that was the most appropriate course of action and document your rational accordingly. 

## Are there differences in activity patterns between weekdays and weekends?
This part of the R scrip creates a vector containing the days in the weekend. The weekdays function and and the newly created weekendlist vector and then used to create a factor with two levels – “weekday” and “weekend”.

```r
weekendlist <- c('Saturday','Sunday')
newactivitydata$weekend <- factor((weekdays(as.Date(activitydata$date)) %in% weekendlist), 
                                   levels=c(TRUE, FALSE),
                                   labels=c('weekend','weekday'))
```
Here a new data frame is created that contains the mean number of steps for each interval grouped by the weekend or weekday.

```r
newmeanstepsperintervaldata <- ddply(newactivitydata, c("interval", "weekend"), 
                                     summarize, meansteps = mean(steps, na.rm = TRUE))
```
Now the lattice library is called and the plot is created.

```r
library(lattice)
xyplot(meansteps ~ interval | weekend, data = newmeanstepsperintervaldata, type="l", layout=c(1,2)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 
