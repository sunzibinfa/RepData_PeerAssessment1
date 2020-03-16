---
title: "Reproducible Research Week 2 Project"
author: "Xin Ha"
date: "3/16/2020"
output: 
  html_document: 
    keep_md: yes
---

## Load libraries


```r
library(ggplot2)
```
## Load and process the data
#### 1. Unzip and load data


```r
unzip("repdata_data_activity.zip")
activity <- read.csv("activity.csv")
```
#### 2. Calculate total number of steps and rename columns


```r
StepsbyDay <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=FALSE)
names(StepsbyDay)[1] <- "Date"
names(StepsbyDay)[2] <- "Steps"
```
#### 3. Disply Results


```r
head(StepsbyDay, 61)
```

```
##          Date Steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
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
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
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
## 61 2012-11-30    NA
```
#### 4. Plot historgram of total number of steps taken


```r
ggplot(StepsbyDay, aes(x = Steps)) +
  geom_histogram(fill = "Blue", binwidth = 1200) +
  labs(title = "Total Steps by Day", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 5. Calculate Mean and Median number of steps per day


```r
meansteps <- mean(StepsbyDay$Steps, na.rm=TRUE)
mediansteps <- median(StepsbyDay$Steps, na.rm=TRUE)
```
* Mean steps: 1.0766189\times 10^{4}
* Median steps: 10765

## Average daily activity pattern


```r
activityremoveNA <- subset(activity,activity$steps >= 0)
MeanStepsbyInterval <- aggregate(activityremoveNA$steps, by=list(activityremoveNA$interval), FUN=mean)
names(MeanStepsbyInterval)[1] ="Interval"
names(MeanStepsbyInterval)[2] ="Steps"
```

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(MeanStepsbyInterval, aes(x=Interval, y=Steps)) +
  geom_line(color="blue") +
  labs(title = "Mean of Steps by Interval", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- MeanStepsbyInterval[which.max(MeanStepsbyInterval$Steps),]
```
* Maximum mean interval (Interval, Steps): 835, 206.1698113

## Missing Values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numberofNA <- subset(activity,is.na(activity$steps))  
missingvalues <- nrow(numberofNA)
```
* Number of missing values: 2304

### 2. Devise a strategy to fill in Missing values.
#### Use the mean of interval to impute missing values.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
numberofNA <- numberofNA[,c('date','interval')]
numberofNA <- merge(numberofNA,MeanStepsbyInterval,by.x='interval',by.y='Interval')
names(numberofNA)[3] <- 'steps'
activity_imputeNA <- rbind(activityremoveNA,numberofNA)
activity_imputeNA <- activity_imputeNA[order(activity_imputeNA$date,activity_imputeNA$interval),]
```

### 4. Make a histogram of the total number of steps taken each day 


```r
StepsByDay2 <- aggregate(activity_imputeNA$steps, by=list(activity_imputeNA$date), FUN=sum, na.rm=FALSE)
names(StepsByDay2)[1] ="Date"
names(StepsByDay2)[2] ="Steps"
head(StepsByDay2,61)
```

```
##          Date    Steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
ggplot(StepsByDay2, aes(x = Steps)) +
  geom_histogram(fill = "Blue", binwidth = 1200) +
  labs(title = "Total Steps by Day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### 5. Calculate Mean and Median number of steps per day


```r
meansteps2 <- mean(StepsByDay2$Steps, na.rm=TRUE)
mediansteps2 <- median(StepsByDay2$Steps, na.rm=TRUE)
```
* Mean steps with imputation: 1.0766189\times 10^{4}
* Median steps with imputation: 1.0766189\times 10^{4}

### 6. Do these values differ from the estimates from the first part of the assignment?

Yes, the mean is the same but the median has risen by 1.19 steps.  

### 7. What is the impact of imputing missing data on the estimates of the total daily number of steps?

The effect of using mean data per interval as a data imputation method for missing values pushes overall data towards the mean.

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create new variable that gives day of week and whether it is on weekend or not


```r
activity_imputeNA$date <- as.Date(activity_imputeNA$date)
activity_imputeNA$DayofWeek <- weekdays(activity_imputeNA$date)
activity_imputeNA$IsWeekend <- ifelse(activity_imputeNA$DayofWeek == "Saturday" | 
                                        activity_imputeNA$DayofWeek == "Sunday", "Weekend", "Weekday")
```

#### Display results


```r
head(activity_imputeNA)
```

```
##        steps       date interval DayofWeek IsWeekend
## 1  1.7169811 2012-10-01        0    Monday   Weekday
## 10 0.3396226 2012-10-01        5    Monday   Weekday
## 17 0.1320755 2012-10-01       10    Monday   Weekday
## 29 0.1509434 2012-10-01       15    Monday   Weekday
## 33 0.0754717 2012-10-01       20    Monday   Weekday
## 45 2.0943396 2012-10-01       25    Monday   Weekday
```
### 2. Time series panel plot


```r
MeanbyIsWeekend <- aggregate(activity_imputeNA$steps, 
                             by=list(activity_imputeNA$IsWeekend, activity_imputeNA$interval), mean)
names(MeanbyIsWeekend)[1] ="IsWeekend"
names(MeanbyIsWeekend)[2] ="Interval"
names(MeanbyIsWeekend)[3] ="Steps"
ggplot(MeanbyIsWeekend, aes(x=Interval, y=Steps, color=IsWeekend)) +
  geom_line() +
  facet_grid(IsWeekend ~ .) +
  labs(title="Mean Steps by Interval by type of Day", x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Yes, there are differences between weekdays and weekend. On average, there is less number of steps on weekends. 


