# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### 1.Load the Data

```r
unzip("activity.zip",exdir = "data")
reprodata <- read.csv("data/activity.csv", stringsAsFactors=FALSE)
```

### 2.Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(lubridate,warn.conflicts = FALSE)
reprodata$date <- ymd(reprodata$date)
reprodatabackup <- reprodata
```

## What is mean total number of steps taken per day?

### 1.Calculate the total number of steps taken per day


```r
givendates <- as.Date(as.Date("2012-10-1"):as.Date("2012-11-30"), origin="1970-01-01")
sumlist <- lapply(givendates,function(x){
        a <- subset(reprodata, reprodata$date == x)
        sum(a$steps) # IF na.rm argument added returns the NA days as 0 decreasing the total mean
})
totalsteps <- unlist(sumlist)
```

### 2.Make a histogram of the total number of steps taken each day


```r
hist(totalsteps, col="purple",main="Total Steps per Day",xlab="Steps",breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 3.Calculate and report the mean and median of the total number of steps taken per day


```r
options(scipen = 1, digits = 2)
totalmean <- mean(totalsteps, na.rm = TRUE)
mediansteps <- median(totalsteps, na.rm = TRUE)
```
Daily mean is 10766.19 & median is 10765

## Lets continue with the barplot of totalsteps/day


```r
par(mfrow=c(1,1))
barplot(totalsteps, col="purple",main="Total Steps per Day",xlab="Steps",names.arg = unique(reprodata$date),cex.names = 0.4, las = 3)
abline( h = totalmean , lty = 2)
abline( h = mediansteps, lty = 2, col = "green")
text(x = 0,y=mediansteps,pos=3,labels = "mean & median", cex = 0.5)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## What is the average daily activity pattern?

### 1.Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgintervals <- reprodata$interval[1:288]
avglist <- lapply(avgintervals,function(x){
        b <- subset(reprodata, reprodata$interval == x)
        mean(b$steps,na.rm = TRUE)
})
avglist <- unlist(avglist) 

### Plotting type 'l' ###
plot(avgintervals,avglist , type = "l",
     xlab = "5min Intervals",
     ylab = "Averages Across all days",
     col = "purple",
     main = "Steps By Time Invertal"
     )
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxavgsteps <- avgintervals[which(avglist == max(avglist,na.rm = TRUE))] ;maxavgsteps
```

```
## [1] 835
```

## Imputing missing values

### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
NArows <- length(which(is.na(reprodata$steps))) ; NArows
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

For a given NA time interval corresponding mean for that same interval is going to be used.

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
for(i in seq(length(reprodata$steps))){
        if(is.na(reprodata$steps[i])){
        reprodata$steps[i] <- avglist[which(avgintervals == reprodata$interval[i])]  
        }
}
head(reprodatabackup,5) ;head(reprodata,5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```
##   steps       date interval
## 1 1.717 2012-10-01        0
## 2 0.340 2012-10-01        5
## 3 0.132 2012-10-01       10
## 4 0.151 2012-10-01       15
## 5 0.075 2012-10-01       20
```

### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sumlist2 <- lapply(givendates,function(x){
        a <- subset(reprodata, reprodata$date == x)
        sum(a$steps,na.rm = TRUE)
})

totalsteps2 <- unlist(sumlist2) ; totalmean2 <- mean(totalsteps2) ; mediansteps2 <- median(totalsteps2)
```
Daily mean with no NA data is 10766.19 & median is 10766.19

Creating the Na & No-NA histograms


```r
par(mfrow=c(1,2)) #Total steps per day w and w-out NA's side by side
hist(totalsteps, col="purple",main="Total Steps per Day",xlab="Steps",breaks = 10)
hist(totalsteps2, col="blue",main="Total Steps per Day no NA",xlab="Steps",breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(timeDate)
reprodatabackup$days <- isWeekday(reprodatabackup$date) # Added another column for days TRUE IF weekday FALSE weekend
reprodatabackup$days <- factor(reprodatabackup$days, labels = c("weekend", "weekday"))
```

### 2.Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avglistweek <- lapply(avgintervals,function(x){
        b <- subset(reprodatabackup, interval == x & days == "weekday")
        c <- subset(reprodatabackup, interval == x & days == "weekend") #CREATING A LIST each sub level like
        c(mean(b$steps,na.rm = TRUE),mean(c$steps,na.rm = TRUE))        # mean 25 min -> c(2,4) first weekday
})                                                                      # second weekend
avglistweekday <- unlist(avglistweek)[ c(TRUE,FALSE) ] # Dividing the list into two vectors
avglistweekend <- unlist(avglistweek)[ c(FALSE,TRUE) ]

library(ggplot2) ; library(gridExtra)
plot1 <- qplot(avgintervals,avglistweekday , geom = "line",
     xlab = "5min Intervals",
     ylab = "Steps",
     main = "Avg steps by minutes Weekdays",
     col = "purple"
) + theme(legend.position="none")
plot2 <- qplot(avgintervals,avglistweekend , geom = "line",
     xlab = "5min Intervals",
     ylab = "Steps",
     main = "Avg steps by minutes Weekend",
     col = "purple"
)+ theme(legend.position="none")
grid.arrange(plot1,plot2,nrow =2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

