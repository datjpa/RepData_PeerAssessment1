Reproducible Research - Coursera Peer Assessment 1
========================================================


This is an R markdown file for Coursera Reproducible Research Peer Assessment 1.

## Loading and preprocessing the data

### 1.Load the data

```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

### 2. Process/transform the data into a format suitable for your analysis

Start by checking characteristics of variables. 

```r
typeof(data$date)
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

```r
typeof(data$interval)
```

```
## Error in data$interval: object of type 'closure' is not subsettable
```

Change type of 'date' variable from character to date. 

```r
data$date <- as.Date(data$date)
```

```
## Error in data$date: object of type 'closure' is not subsettable
```

## What is mean total number of steps taken per day?

Note: For this part missing values in the dataset are ignored.

### 1.Calculate the total number of steps taken per day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
sumsteps <- aggregate(steps ~ date, data, FUN = sum, na.rm=TRUE)
```

```
## Error in as.data.frame.default(data, optional = TRUE): cannot coerce class ""function"" to a data.frame
```

```r
#sumsteps <- data %>% (group_by(date) %>% summarise(totalsteps = sum(steps))
```

### 2. Make a histogram of the total number of steps taken each day


```r
hist(sumsteps$steps, xlab = "Total Number of Steps Taken Each Day", main = "Histogram of Total Number of Steps Taken Each Day")
```

```
## Error in hist(sumsteps$steps, xlab = "Total Number of Steps Taken Each Day", : object 'sumsteps' not found
```

### 3.Calculate and report the mean and median of the total number of steps taken per day


```r
mean(sumsteps$steps)
```

```
## Error in mean(sumsteps$steps): object 'sumsteps' not found
```

```r
median(sumsteps$steps)
```

```
## Error in median(sumsteps$steps): object 'sumsteps' not found
```

The mean number of steps taken per day is 10766, the median number of steps/day taken is 10765.

## What is the average daily activity pattern?

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data1 <-na.omit(data)

interval2<-c(0,5,10,15,20,25,30,35,40,45,50,55)
data2 <- cbind(data1, interval2)

meansteps <- aggregate(data2["steps"], data2["interval2"], mean, na.rm=TRUE)
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
plot(meansteps$interval2, meansteps$steps, type="l", xlab="5-Minute Interval", ylab="Average number of steps taken across all days", main="Time Series Plot of 5-minute Interval and Average Number of Steps Taken")
```

```
## Error in plot(meansteps$interval2, meansteps$steps, type = "l", xlab = "5-Minute Interval", : object 'meansteps' not found
```

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxsteps <- meansteps[meansteps$steps==max(meansteps$steps),]
```

```
## Error in eval(expr, envir, enclos): object 'meansteps' not found
```

```r
maxsteps
```

```
## Error in eval(expr, envir, enclos): object 'maxsteps' not found
```
The interval at 15 minutes contains the maximum number of steps on average across all the days in the dataset.

## Imputing missing values

### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing <- is.na(data$steps)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
table(missing)[["TRUE"]]
```

```
## Error in unique.default(x, nmax = nmax): unique() applies only to vectors
```
There are 2304 missing values in the dataset.

### 2.Devise a strategy for filling in all of the missing values in the dataset. 


```r
data3<-data
data3 <- cbind(data, interval2)
for(i in 1:nrow(data3)){
        if(is.na(data3[i,1])){
                for(ii in 1:nrow(meansteps)){
                        if(data3[i,4]==meansteps[ii,1]){
                                data3[i,1]<-meansteps[ii,2]
                        }
                }
        }
}
```
### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

Data3 created in above step is the new data set with the missing values filled in.

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sumsteps2 <- aggregate(steps ~ date, data3, FUN = sum, na.rm=TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'steps' not found
```

```r
hist(sumsteps2$steps, xlab = "Total Number of Steps Taken Each Day", main = "Histogram of Total Number of Steps Taken Each Day")
```

```
## Error in hist(sumsteps2$steps, xlab = "Total Number of Steps Taken Each Day", : object 'sumsteps2' not found
```

```r
mean(sumsteps2$steps)
```

```
## Error in mean(sumsteps2$steps): object 'sumsteps2' not found
```

```r
median(sumsteps2$steps)
```

```
## Error in median(sumsteps2$steps): object 'sumsteps2' not found
```
While the mean value of 10766.19 is not changed by imputing missing values, the median value differs from the result when simply ommitting NAs (10765) and is now increased to 10766.19.

## Are there differences in activity patterns between weekdays and weekends?

### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
days <- weekdays(as.Date(data3$date, '%Y-%m-%d'))
```

```
## Error in as.Date.default(data3$date, "%Y-%m-%d"): do not know how to convert 'data3$date' to class "Date"
```

```r
weekend <- (days == "Saturday"|days=="Sunday")
```

```
## Error in eval(expr, envir, enclos): object 'days' not found
```

```r
dayfactor <- factor(weekend, labels = list("weekday","weekend"))
```

```
## Error in factor(weekend, labels = list("weekday", "weekend")): object 'weekend' not found
```

```r
data4 <- cbind(data3, dayfactor)
```

```
## Error in cbind(data3, dayfactor): object 'dayfactor' not found
```

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(lattice)
meansteps2 <- aggregate(data4$steps, list(Interval =data4$interval2, Dayfactor = data4$dayfactor), mean, na.rm=TRUE)
```

```
## Error in aggregate(data4$steps, list(Interval = data4$interval2, Dayfactor = data4$dayfactor), : object 'data4' not found
```

```r
xyplot(x ~ Interval | Dayfactor, data = meansteps2, layout = c(1,2), type = "l", xlab="5-minute Interval", ylab="Average Number of steps taken all weekday days or weekend days", main="Time Series Plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'meansteps2' not found
```
