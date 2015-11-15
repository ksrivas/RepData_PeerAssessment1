---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
---



```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

#Loading and preprocessing the data


```r
SrcFilename<-"repdata-data-activity.zip"
if (!file.exists(SrcFilename)){
  #  quit(save = "ask", status = 0,
  #       runLast = TRUE)
  print (paste("file ",SrcFilename,"File not found"))
}
```

```
## [1] "file  repdata-data-activity.zip File not found"
```

```r
unzip(SrcFilename)
```

```
## Warning in unzip(SrcFilename): error 1 in extracting from zip file
```

```r
ActivityData<-read.csv("activity.csv",header=TRUE,sep=",")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

#What is mean total number of steps taken per day


```r
SumDataActivity<-aggregate(steps~date,ActivityData,sum)
```

```
## Error in eval(expr, envir, enclos): object 'ActivityData' not found
```

1.) Plot the histogram of the total number of steps taken each day.


```r
p<- ggplot(SumDataActivity,aes(x = steps))+ 
  geom_histogram(fill="blue") +
  labs(x="Total number of steps per day", y = "Count")
```

```
## Error in ggplot(SumDataActivity, aes(x = steps)): object 'SumDataActivity' not found
```

```r
print(p)
```

```
## Error in print(p): object 'p' not found
```

2.) Mean and median of the number of steps

```r
MeanVal<-mean(SumDataActivity[,2])
```

```
## Error in mean(SumDataActivity[, 2]): object 'SumDataActivity' not found
```

```r
MedianVal<-median(SumDataActivity[,2])
```

```
## Error in median(SumDataActivity[, 2]): object 'SumDataActivity' not found
```

```r
print(MeanVal)
```

```
## Error in print(MeanVal): object 'MeanVal' not found
```

```r
print(MedianVal)
```

```
## Error in print(MedianVal): object 'MedianVal' not found
```

#What is the average daily activity pattern?


```r
AvgDailyIntervalPat<-aggregate(steps~interval,ActivityData,mean)
```

```
## Error in eval(expr, envir, enclos): object 'ActivityData' not found
```

```r
with(AvgDailyIntervalPat,{ 
  plot(interval,steps,type="l",xlab="time interval [sec]",ylab="Avg number of steps",lwd="2",main="Average activity",col="red") 
})
```

```
## Error in with(AvgDailyIntervalPat, {: object 'AvgDailyIntervalPat' not found
```

- Get the interval with max number of steps over all days


```r
indx<-which.max(AvgDailyIntervalPat[,2])
```

```
## Error in which.max(AvgDailyIntervalPat[, 2]): object 'AvgDailyIntervalPat' not found
```

```r
Intervalbegin<-AvgDailyIntervalPat[indx,1]
```

```
## Error in eval(expr, envir, enclos): object 'AvgDailyIntervalPat' not found
```

```r
Intervalend<-Intervalbegin+5
```

```
## Error in eval(expr, envir, enclos): object 'Intervalbegin' not found
```

```r
print(paste("Interval of max mean steps:",Intervalbegin,"-",Intervalend))
```

```
## Error in paste("Interval of max mean steps:", Intervalbegin, "-", Intervalend): object 'Intervalbegin' not found
```

#Imputing missing values
- Count Number of rows with NA


```r
NumCompCases<-sum(complete.cases(ActivityData))
```

```
## Error in complete.cases(ActivityData): object 'ActivityData' not found
```

```r
NumNas<-sum(!complete.cases(ActivityData))
```

```
## Error in complete.cases(ActivityData): object 'ActivityData' not found
```

```r
print(paste("Number of rows with NAs:",NumNas))
```

```
## Error in paste("Number of rows with NAs:", NumNas): object 'NumNas' not found
```

- Fill in the NAs
The strategy that I will use is to take replace the NAs by the mean for that day.
The new data set is called ActivityFullData.


```r
MeanByDay<-aggregate(steps~date,ActivityData,mean)
```

```
## Error in eval(expr, envir, enclos): object 'ActivityData' not found
```

```r
ActivityFullData<-transform(ActivityData,steps=ifelse(is.na(steps),
              MeanByDay[as.Date(MeanByDay$date)%in%as.Date(ActivityData$date),2],steps))
```

```
## Error in transform(ActivityData, steps = ifelse(is.na(steps), MeanByDay[as.Date(MeanByDay$date) %in% : object 'ActivityData' not found
```

- Make a new histogram  of the total number of steps taken each day

```r
SumDataActivityFull<-aggregate(steps~date,ActivityData,sum)
```

```
## Error in eval(expr, envir, enclos): object 'ActivityData' not found
```

```r
p1<- ggplot(SumDataActivityFull,aes(x = steps))+ 
  geom_histogram(fill="green") +
  labs(x="Total number of steps per day", y = "Count")
```

```
## Error in ggplot(SumDataActivityFull, aes(x = steps)): object 'SumDataActivityFull' not found
```

```r
print(p1)
```

```
## Error in print(p1): object 'p1' not found
```

2.)New Mean and median of the number of steps


```r
MeanValNew<-mean(SumDataActivityFull[,2])
```

```
## Error in mean(SumDataActivityFull[, 2]): object 'SumDataActivityFull' not found
```

```r
MedianValNew<-median(SumDataActivityFull[,2])
```

```
## Error in median(SumDataActivityFull[, 2]): object 'SumDataActivityFull' not found
```

```r
print(MeanValNew)
```

```
## Error in print(MeanValNew): object 'MeanValNew' not found
```

```r
print(MedianValNew)
```

```
## Error in print(MedianValNew): object 'MedianValNew' not found
```

The mean and median values are not affected by imputing the NAs with 
means over that corresponding day.

#Are there differences in activity patterns between weekdays and weekends?
- First create the grouped data and the factors weekends and weekdays


```r
ActivityFullData<-mutate(ActivityFullData,weekdays(as.Date(date)))
```

```
## Error in mutate_(.data, .dots = lazyeval::lazy_dots(...)): object 'ActivityFullData' not found
```

```r
names(ActivityFullData)[4]<-c("DayType")
```

```
## Error in names(ActivityFullData)[4] <- c("DayType"): object 'ActivityFullData' not found
```

```r
ActivityFullData<-transform(ActivityFullData,DayType=ifelse(DayType %in% c("Saturday","Sunday"),"weekend","weekday"))
```

```
## Error in transform(ActivityFullData, DayType = ifelse(DayType %in% c("Saturday", : object 'ActivityFullData' not found
```

```r
ActivityFullData$DayType<-as.factor(ActivityFullData$DayType)
```

```
## Error in is.factor(x): object 'ActivityFullData' not found
```

#Are there differences in activity patterns between weekdays and weekends?


```r
AvgFullData<-aggregate(steps~interval+DayType,ActivityFullData,mean)
```

```
## Error in eval(expr, envir, enclos): object 'ActivityFullData' not found
```

```r
p3<-ggplot(AvgFullData,aes(interval,steps,color=DayType)) +
  geom_line() +
  labs(x="Interval", y = "Number of steps") +
  facet_wrap(~DayType,nrow=2)
```

```
## Error in ggplot(AvgFullData, aes(interval, steps, color = DayType)): object 'AvgFullData' not found
```

```r
print(p3)
```

```
## Error in print(p3): object 'p3' not found
```

