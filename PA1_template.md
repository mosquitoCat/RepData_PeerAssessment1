This is an R Markdown document for the peer assessment 1 of the
Reprducible Research course.

Loading and preprocessing the data
----------------------------------

Load the dataset from the course webset and transform the data into a
format suitable for the analysis if necessary.

    ## download the data file
    temp <- tempfile()
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
    ## read in data 
    data <- read.csv(unz(temp, "activity.csv"), header = TRUE)
    summary(data)

    ##      steps               date          interval   
    ##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
    ##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
    ##  Median :  0.0   2012-10-03:  288   Median :1178  
    ##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
    ##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
    ##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
    ##  NA's   :2304    (Other)   :15840

    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

    library(plyr)
    ## Make a histogram of the total number of steps taken each day
    ## subset dataframe by date and sum the steps
    data2 <- ddply(data, .(date), summarize, steps = sum(steps))
    ## Make a histogram with the new dataframe
    hist(data2$steps, main = "Histogram of the total number of steps taken each day", xlab = "steps", col = "cyan")

![plot of chunk
unnamed-chunk-2](./PA1_template_files/figure-markdown_strict/unnamed-chunk-2.png)

    ## Calculate and report the mean and median total numbe of steps taken per day
    mean <- mean(data2$steps, na.rm = TRUE)
    median <- median(data2$steps, na.rm = TRUE)
    cat("the mean total number of steps taken per day is: ", mean)

    ## the mean total number of steps taken per day is:  10766

    cat("the median total number of steps taken per day is:", median)

    ## the median total number of steps taken per day is: 10765

What is the average daily activity pattern?
-------------------------------------------

    ## Make a time series plot of the 5-minte interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    ## Sorting the dataframe by the 5-minte interval and calculate the average to generate a new data frame data3
    data3 <- ddply(data, .(interval), summarize, steps = mean(steps, na.rm = TRUE))
    ## time series plot
    plot(data3$interval, data3$steps, xlab = "Interval", ylab = "Average number of steps", type = "l", col = "blue")

![plot of chunk
unnamed-chunk-3](./PA1_template_files/figure-markdown_strict/unnamed-chunk-3.png)

    ## find the 5-minte interval on average across all the days contains the maximum number of steps
    intervalName <- data3$interval[which.max(data3$steps)]
    cat("the 5-minte interval on average across all the days contains the maximum number of steps is:", intervalName)

    ## the 5-minte interval on average across all the days contains the maximum number of steps is: 835

Imputing missing values
-----------------------

    data4 <- data
    ## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    nrow(data4[!complete.cases(data4), ])

    ## [1] 2304

    ## Devise a strategy for filling in all of the missing values in the dataset. The missing value will be filled with the mean for that 5-minute interval. 
    ## get index of NA values
    index <- which(is.na(data4) == TRUE)
    ## find the mean for the 5-minute interval where the NA is by searching the data3 dataset with the mean of steps according to the interval
    for (x in index) {
            data4[x, ]$steps = data3$steps[which(data3$interval == data4[x, ]$interval)]
    }
    ## Make a histogram of the total number of steps taken each day
    data5 <- ddply(data4, .(date), summarize, steps = sum(steps))
    hist(data5$steps, main = "Histogram of the total number of steps taken each day", xlab = "steps", col = "red")

![plot of chunk
unnamed-chunk-4](./PA1_template_files/figure-markdown_strict/unnamed-chunk-4.png)

    ## and calculate and report the mean and median total number of steps taken per doy
    mean <- mean(data5$steps, na.rm = TRUE)
    median <- median(data5$steps, na.rm = TRUE)
    cat("the mean total number of steps taken per day is: ", mean)

    ## the mean total number of steps taken per day is:  10766

    cat("the median total number of steps taken per day is:", median)

    ## the median total number of steps taken per day is: 10766

Are there differences in activity pattern between weekdays and weekends?
------------------------------------------------------------------------

    library(lattice)
    ## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part, which is data4.
    ## Creat a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
    data4$weekDay = weekdays(as.Date(data4$date))
    data4[which(data4$weekDay == "Sunday" | data4$weekDay == "Saturday"), ]$weekDay = "weekend"
    data4[which(data4$weekDay != "weekend"), ]$weekDay = "weekday"
    ## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
    data6 <- data4[which(data4$weekDay == "weekday"), ]
    data6 <- ddply(data6, .(interval), summarize, steps = mean(steps, na.rm = TRUE))
    data6$weekDay = "weekday"
    data7 <- data4[which(data4$weekDay == "weekend"), ]
    data7 <- ddply(data7, .(interval), summarize, steps = mean(steps, na.rm = TRUE))
    data7$weekDay = "weekend"
    data8 <- rbind(data6, data7)
    xyplot(steps ~ interval | weekDay, data = data8, group = weekDay, layout = c(1, 2), type = "l")

![plot of chunk
unnamed-chunk-5](./PA1_template_files/figure-markdown_strict/unnamed-chunk-5.png)

ß
