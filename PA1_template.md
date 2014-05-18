
Reproducible Research: Peer Assessment 1
=============================================

# Introduction
This documents describes the work undertaken to answer the follow questions:
* What is the mean total number of steps taken per day?
* What is the average daily activity pattern?
* What are the differences in activity patterns between weekdays and weekends?

We begin by loading  and preprocessing the data. Ourdata will consist of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. This data is [freely available as a zip file](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).


# Section 1: Loading and Preprocessing the Data

We begin by unzipping the zip file which contains the activity data

```r
unzip("./activity.zip")
```

The zip file can be gotten easily by running

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")  #NOT RUN!
```


Next we load in the activity data as a data frame and take a look at the first 5 lines

```r
df <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

and also take a look at the last 5 lines

```r
tail(df)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


Also a quick summary of the data.

```r
summary(df)
```

```
##      steps           date              interval   
##  Min.   :  0.0   Length:17568       Min.   :   0  
##  1st Qu.:  0.0   Class :character   1st Qu.: 589  
##  Median :  0.0   Mode  :character   Median :1178  
##  Mean   : 37.4                      Mean   :1178  
##  3rd Qu.: 12.0                      3rd Qu.:1766  
##  Max.   :806.0                      Max.   :2355  
##  NA's   :2304
```


Note the only missing values occurs for **steps**.  

We will return to the question of the how to deal with NA's later, but first
we collect a little information on the existing data, ignoring missing values.




# Section 2 The Mean Total Number of Steps taken per Day

In this section we address the question of ***what is mean total number of steps taken per day?***

## Subsection 2.1 Plotting the total Number of Steps Each Day
To get the step count for each day (this treats each NA as 0),
we do

```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
df.daily.steps <- ddply(df[-3], "date", function(d) c(count = sum(d$steps, na.rm = T)))
head(df.daily.steps)
```

```
##         date count
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


Plotting the total number of steps per day we see

```r
barplot(df.daily.steps$count, col = "lightblue")
```

![Daily Total Number of Steps](figure/unnamed-chunk-7.png) 


## Subsection 2.2 Mean and Median

To get the mean number of steps per day

```r
mean.daily.steps <- mean(df.daily.steps$count)
mean.daily.steps
```

```
## [1] 9354
```


Alternatively we could get the mean by

```r
sum(df$steps, na.rm = T)/length(unique(df$date))
```

```
## [1] 9354
```


To get the median number of steps per day

```r
median.daily.steps <- median(df.daily.steps$count)
median.daily.steps
```

```
## [1] 10395
```


# Section 3 The Average Daily Activity Pattern

By ***average daily activity pattern***, we mean the pattern exhibited
of the average activity for the same time interval over all days. This pattern
is a 24-hour times series given in 5 minute intervals.
The data set represents the time intervals using starting time having a 24 hour format, for example
2430 is 11:30 pm.

## Subsection 3.1 A Plotting the Activity Pattern
Again use plyr we have

```r
df.ave.activity <- ddply(df[-2], "interval", function(d) c(average = mean(d$steps, 
    na.rm = T)))
head(df.ave.activity)
```

```
##   interval average
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```


and graphing 

```r
with(df.ave.activity, {
    plot(interval, average, type = "l")
})
```

![Activity Levels](figure/unnamed-chunk-12.png) 


## Subsection 3.3 The Interval with the Maximum Average Number of Steps
Here we see that there is a spike in the morning sometime before 10:00 am. When the spike occurs
is easily found to be

```r
peak <- df.ave.activity$interval[which(df.ave.activity$average == max(df.ave.activity$average))]
peak <- sprintf("%04d", as.numeric(peak))
peak <- paste(substr(peak, 1, 2), substr(peak, 3, 4), sep = ":")
peak
```

```
## [1] "08:35"
```


That is at 08:35.  Which is shortly before the usual offices hours.


# Section 4: Imputing missing values

## Subsection 4.1: The Amount of Missing Values (NA's)
As we saw in the first section, steps is the only field containing missing values, and by direct
calculation we see

```r
no.of.missing.values <- sum(is.na(df$steps))
no.of.missing.values
```

```
## [1] 2304
```

that there are 2304 many missing values. (argeeing with the summary in the first section)

To get the percent is more revealing 

```r
percentNA <- 100 * sum(is.na(df$steps))/length(df$steps)
percentNA
```

```
## [1] 13.11
```

Thus the precentage of NAs present in the **steps** column is 13.1148%.

## Subsection 4.2: A Strategy for filling The Missing NA's

We divide this section into parts, the first part an analysis to motivate our
approach which is present in the second part

### Part I: Prelimiary Analyisis of the Distribution of Missing Values

Before imputing the NA's it is useful to ask how are these
"NA's" distributed?

Since the rows are in chronological order: 

```r
identical(order(df$date) == 1:nrow)
```

```
## Error: NA/NaN argument
```


plotting   missing values as a function of time is easy:

```r
plot(which(is.na(df$steps)))
```

![Chronology of Missing Values ](figure/distribution_of_missing_values.png) 


From this it appears missing values seem to be clustered in groups.
Plotting the daily number of missing values shows:

```r
df.missing.steps <- ddply(df[-3], "date", function(d) c(count = sum(is.na(d$steps))))
barplot(df.missing.steps$count, col = "red")
```

![Total Daily Missing Values](figure/unnamed-chunk-17.png) 

It looks like there are only eight days where the values appear to be  missing.
Examing those days we see

```r
df.missing.days <- subset(df.missing.steps, count > 0)
kable(df.missing.days)
```

```
## |id  |date        |  count|
## |:---|:-----------|------:|
## |1   |2012-10-01  |    288|
## |8   |2012-10-08  |    288|
## |32  |2012-11-01  |    288|
## |35  |2012-11-04  |    288|
## |40  |2012-11-09  |    288|
## |41  |2012-11-10  |    288|
## |45  |2012-11-14  |    288|
## |61  |2012-11-30  |    288|
```

Since 

```r
no.daily.intervals <- 24 * (5/60)
no.daily.intervals
```

```
## [1] 2
```

we see that if a day has any missing value, the all values for that day are missing. 

### PART II: The Strategy

Clearly we can't impute a missing value by looking at it's neigbour,since that neighbor will almost always also be missing. 

Now we saw earlier that the steps values varied dramatically across time of day, with a spike at 08:35. So obviously a using a constant value, such as a mean,  to imputing the all the values for entire day would be a poor choice. 

A better choice IS to include the time of day as a factor in our imputation. Since we already have computed the daily average activity, this is easy to implement:

### OUR FINAL STRATEGY:
### ***For each missing with a given interval, replace each missing value with a predicted value given  by the average activity for that interval.*** 
We obtain this value from the **df.ave.activity** data frame previouly computed in section 3.3.


## Subsection 4.3 Creating the The Imputed Data Set

First we create a helper function that returns the average step value given the for a given
interval

```r
fn <- function(interval) {
    j <- which(df.ave.activity$interval == interval)  #find the index on activity
    df.ave.activity$average[j]  #return the ave step for that activity
}
```

Next we construct our new steps set

```r
i.steps <- sapply(1:nrow(df), function(i) {
    ifelse(is.na(df$steps[i]), fn(df$interval[i]), df$steps[i])
})
```

And finally we form a new data frame with the imputed steps

```r
df.imputed <- df  #copy
df.imputed$steps <- i.steps
```



## Subsection 4.4
Next we repeat the exercises in section 2, but with the new imputed data set


```r
require(plyr)
df.inputed.daily.steps <- ddply(df.imputed[-3], "date", function(d) c(count = sum(d$steps)))
head(df.inputed.daily.steps)
```

```
##         date count
## 1 2012-10-01 10766
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


Plotting the total number of steps per day we see

```r
barplot(df.inputed.daily.steps$count, col = "lightblue")
```

![Daily Total Number of Steps(inputed](figure/unnamed-chunk-24.png) 


And computing the mean and median for the inputed set we have

To get the mean number of steps per day

```r
mean.inputed.daily.steps <- mean(df.inputed.daily.steps$count)
mean.inputed.daily.steps
```

```
## [1] 10766
```


To get the median number of steps per day

```r
median.inputed.daily.steps <- median(df.inputed.daily.steps$count)
median.inputed.daily.steps
```

```
## [1] 10766
```


Clearly these values differ from those obtained in section 2.

# Section 5 Differences in Activity Patterns Between Weekdays and Weekends

## Adding a New Factor to our DataSet
To explore the differences in activity patterns between weekday and weekends
we add an new factor as an indicator for whether the observation was a weekend or weekday.
But first a helper function.

```r
week.day.end <- function(x) {
    y <- weekdays(as.Date(x))
    ifelse(y %in% c("Saturday", "Sunday"), "weekend", "weekday")
}
```

Next we add a new column to our data set

```r
df$wked <- sapply(df$date, week.day.end)
head(df$wked)
```

```
## [1] "weekday" "weekday" "weekday" "weekday" "weekday" "weekday"
```


## A Panel Plot Comparison

We do visual comparion of the activity patterns between weekdays and weekends, but first we need to recompute the average activities 
for  all intervals, togther with our new factors "weekend" and "weekday""


```r
df.ave.activity <- ddply(df[-2], c("interval", "wked"), function(d) c(average = mean(d$steps, 
    na.rm = T)))
```


Next we plot using the we load the ggplot package, to get a visual comparison.

```r
library(ggplot2)
qplot(interval, average, data = df.ave.activity, facets = wked ~ ., geom = "line")
```

![Visual Comparion between Weekends and Weekdays](figure/unnamed-chunk-30.png) 


Clearly, during the work week  there is a higher peak in the early morning (presumably people 
walking from their car to their office). However during the week less activity is exhibited 
for the rest of the day, presumably because the subjects were sitting behind a desk. 



