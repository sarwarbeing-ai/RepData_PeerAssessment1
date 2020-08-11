---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data



```r
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip','./activity.zip')
```



```r
unzipfile<-unzip('./activity.zip')
```



```r
activity<-read.csv('./activity.csv')
activity$date <- as.Date(activity$date)
df_na_removed <- subset(activity, !is.na(activity$steps))
head(df_na_removed)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```


## What is mean total number of steps taken per day?



```r
library(ggplot2)
library(plyr)
df1<-ddply(df_na_removed,"date",summarize,total_steps_per_day=sum(steps,na.rm=T))

ggplot(df1, aes(x=total_steps_per_day))+
  geom_histogram(color="red", fill="blue")+ggtitle('The distribution of total steps per day (missing data ignored)')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


#mean and median total number of steps taken per day.



```r
print(mean(df1$total_steps_per_day))
```

```
## [1] 10766.19
```

```r
print(median(df1$total_steps_per_day))
```

```
## [1] 10765
```


## What is the average daily activity pattern?



```r
df_na_removed$interval<-factor(df_na_removed$interval)
df2<-ddply(df_na_removed,"interval",summarize,interval_average=mean(steps,na.rm=T))

plot(df2$interval,df2$interval_average,xlab='5-minute intervals',ylab='average steps in the interval across all days',type="l",lwd=4)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
max_steps <- max(df2$interval_average)
z<-df2[df2$interval_average == max_steps,] 

print(z)
```

```
##     interval interval_average
## 104      835         206.1698
```



## Imputing missing values


#the total number of missing values in the dataset 



```r
print(sum(is.na(activity$steps)))
```

```
## [1] 2304
```


#We use a simple strategy for filling in all of the missing values in the dataset.Filling missing values with the mean of 5-minute interval


```r
library(dplyr)
```

```
## Warning: As of rlang 0.4.0, dplyr must be at least version 0.8.0.
## * dplyr 0.7.4 is too old for rlang 0.4.5.
## * Please update dplyr to the latest version.
## * Updating packages on Windows requires precautions:
##   <https://github.com/jennybc/what-they-forgot/issues/62>
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
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
activity$interval<-factor(activity$interval)
df3 <-activity
missing <- is.na(df3$steps)
interval_avg <- tapply(df3$steps, df3$interval, mean, na.rm=TRUE, simplify=T)
df3$steps[missing] <- interval_avg[df3$interval[missing]]
head(df3)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
total_steps_per_day<-tapply(df3$steps, df3$date, sum, na.rm=T,simplify=T)

hist(total_steps_per_day,
     col="blue",
     breaks=22,
     xlab="steps per day ",
     ylab="frequency",
     main="The distribution of total steps per day (with missing data imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
print(mean(total_steps_per_day))
```

```
## [1] 10766.19
```

```r
print(median(total_steps_per_day))
```

```
## [1] 10766.19
```

#comment:Before imputing missing values the mean and median were  10766.19 and 
#10765 respectively  but after imputing missing values with some 5-minute interval
#values the mean is 10766.19 and median is 10766.19.Notice that the mean is same #before and after imputing values but the median has been increased a little.


#Also the due to imputing missing values the frequency count has increased around the mean.


## Are there differences in activity patterns between weekdays and weekends?


#Let's have a helper function to decide whether a day is weekday or weekend



```r
weekday <- function(d) {
    wd <- weekdays(d)
    if (wd=='Saturday' | wd=='Sunday'){
      
    return('weekend')
    
    }else{
           return('weekday')
  }
}

wdk<-sapply(df3$date, weekday)
df3$wdk<-as.factor(wdk)
head(df3)
```

```
##       steps       date interval     wdk
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

```r
df4<- aggregate(steps ~ wdk+interval,df3,mean)

library(lattice)
xyplot(steps ~ interval | factor(wdk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       data=df4)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


