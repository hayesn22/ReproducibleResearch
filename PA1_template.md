---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
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
```

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", as.is = TRUE)
activity <- data[complete.cases(data), ]
```


## What is mean total number of steps taken per day?


```r
stepsTaken <- aggregate(steps ~ date, activity, sum)
hist(stepsTaken$steps, main = "Total Steps Taken Per Day", xlab = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Mean of the total number of steps taken:

```r
mean(stepsTaken$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken:

```r
median(stepsTaken$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
activityPattern <- aggregate(steps~interval, activity, mean)
plot(activityPattern$interval, activityPattern$steps, type = "l",
     main = "Average Number of Steps by Interval", xlab = "Interval",
     ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
maximumSteps <- which.max(activityPattern$steps)
activityPattern[maximumSteps, ]
```

```
##     interval    steps
## 104      835 206.1698
```
The interval with the highest average steps is interval 835 with 206.1698 steps. 

## Imputing missing values

```r
sum(is.na(data))
```

```
## [1] 2304
```
There are 2304 rows with NA's


```r
for(i in 1:nrow(data)) {
  if(is.na(data$steps[i])) {
    imputed <- activityPattern$steps[which(activityPattern$interval==
                                             data$interval[i])]
    
    data$steps[i] <- imputed
  }
}
```


```r
stepsTakenImputed <- aggregate(steps ~ date, data, sum)

hist(stepsTakenImputed$steps, main = "Total Steps Taken Per Day (Imputed)", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean of the total number of steps taken:

```r
mean(stepsTakenImputed$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken:

```r
median(stepsTakenImputed$steps)
```

```
## [1] 10766.19
```

The median for the imputed data increased slightly. The distribution also changed slightly. 

## Are there differences in activity patterns between weekdays and weekends?


```r
weekDay <- function(date_val) {
  wd<- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if(!(wd == 'Saturday' || wd== 'Sunday')){
    day <- 'Weekday'
  }
  else {
    day <- 'Weekend'
  }
  day
}
```


```r
data$day_type <- as.factor(sapply(data$date, weekDay))

stepsTakenImputed <- aggregate(steps ~ interval + day_type, data, mean)

ggplot(stepsTakenImputed, aes(interval, steps)) + 
  geom_line(stat = "identity") + facet_grid(day_type ~., scales = "fixed",
  space = "fixed") + labs(x= "Interval", y= "Steps")+ 
  ggtitle("Steps Taken per Interval by Day Type")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
