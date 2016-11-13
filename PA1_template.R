
## *****  Loading libraries ********
library(ggplot2)




## ********  1) Checking if file exists in our wd *************
name1 <- "activity.csv"
lname <- c(name1)
list_name <- dir(path=".")

verif1 <- lname %in% list_name

if (verif1[1] == TRUE ){
        print("Reading file 'activity.csv' ")
} else {
        print("file is missing in your working directory")
        stop()
}
##----------------------------------------------------------



##***********  2) Reading the file *************************
dades <- read.table(file= name1 , header=TRUE, sep=",")
##----------------------------------------------------------



##**** 3) What is mean total number of steps taken per day *****

### Calculate the total number of steps taken per day
sumstep <- aggregate(steps~date, dades, sum, na.rm=TRUE)

### histogram of the total number of steps taken each day
png("plot1.png", width=1000, height=600)
g <- ggplot(sumstep, aes(steps))
g + geom_histogram(color="blue", fill="cyan",binwidth = 1000) +
        labs(title="Total number of steps taken each day")
dev.off()

### mean and median of the total number of steps taken per day
mean1 <- mean(sumstep$steps)
median1 <- median(sumstep$steps)
##-----------------------------------------------------------------



##******** 4) What is the average daily activity pattern  *********

###  time series plot  of the 5-minute interval and 
###  the average number of steps taken (averaged)

#### subsetting data
sub2 <- aggregate(steps~interval, dades, mean, na.rm=TRUE)   

#### plotting
png("plot2.png", width=1000, height=1000)
g <- ggplot(sub2, aes(x=interval, y=steps))
g + geom_line(color="blue",size=1) +
        labs(title="Average number of steps taken by 5min interval")+
        theme(plot.title= element_text(size=30))+
        theme(axis.title.x = element_text(size=22), axis.text.x=element_text(size=16))+
        theme(axis.title.y = element_text(size=22), axis.text.y=element_text(size=16))
dev.off()

#### 5 minute interval containing the max. number of steps
maxaver <- max(sub2$steps)
maxint <- subset(sub2, steps==maxaver)$interval
##-----------------------------------------------------------------



##************ 5) imputing missing values  ****************

###  Calculate the number of NA values 
nalist <- is.na(dades$steps)


### Filling NA values: using the average for the interval and creating new data set
step2 <- replace(dades$steps, nalist, sub2$steps[match(dades$interval[nalist],sub2$interval)])
dades2 <- dades
dades2$steps <- step2


### histogram of the total number of steps taken each day with //modified data set//
sumstep2 <-aggregate(steps~date, dades2, sum)
png("plot3.png", width=1000, height=600)
g <- ggplot(sumstep2, aes(x=steps))
g + geom_histogram(color="red", fill="orange", binwidth = 1000) +
        labs(title="Total number of steps taken each day //modified//")
dev.off()


### mean and median of the total number of steps taken per day //modified data set//
mean2 <- mean(sumstep2$steps)
median2 <- median(sumstep2$steps)
##-----------------------------------------------------------------------------------



##***** 6) Are there differences in activity patterns between weekdays and weekends? ************

### Setting weekdays in English
Sys.setlocale("LC_TIME", "English")

### Redefining date variable as dates
dades2$date <- as.Date(as.character(dades2$date), "%Y-%m-%d")

### creating a weekday-weektype list
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")
weektype <- c( "weekday","weekday","weekday","weekday","weekday","weekend","weekend" )
weeklist <- cbind(day, weektype)

### Matching and defining weektype day in data set
dades2$weekday <- weekdays(dades2$date)
dades2$weekType <- weeklist[match(dades2$weekday, weeklist[,1]) ,2]

#### creating subset with average of steps by interval & plotting
sub3 <- aggregate(steps~interval+weekType, dades2, mean)
png("plot4.png", width=600, height=600)
g <- ggplot(sub3, aes(x=interval, y=steps))+ facet_grid(weekType~.)
g + geom_line(aes(color=weekType), size=1) +
        labs(title="Average number of steps taken by 5min interval")+
        theme(plot.title= element_text(size=20))+
        theme(axis.title.x = element_text(size=15), axis.text.x=element_text(size=11))+
        theme(axis.title.y = element_text(size=15), axis.text.y=element_text(size=11))
dev.off()
##-----------------------------------------------------------------------------------

