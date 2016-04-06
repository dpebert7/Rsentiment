# Ebert/Rider
# 3 April 2016
# Create visualizations for LA2014 and LA2016

# packages 
library(plyr)
library(dplyr)
# Does this zipfR package have anything we could use?

load("~/Desktop/Huang Research/Rsentiment/comTweetsLA.RData") # load LA2014 into memory as x


x$text<-as.character(x$text)
x$username<-as.character(x$username)
summary(x)
summary(x$username)

# Plot tweets per username
name.count<-data.frame(count(x,username))
# name.count$username<-as.character(name.count$username)
names(name.count)<-c("username","frequency")
name.count<-filter(name.count,username != "")
name.count<-arrange(name.count,desc(frequency))
hist(name.count$frequency)
plot(name.count$frequency,
     log = "y",
     xlab = "Unique Usernames",
     ylab = "Number of Tweets",
     type = "l")


# Plot time of day
hour.count<-data.frame(count(x,hour))
plot(hour.count$hour,hour.count$n)

# Plot month (not very interesting)
month.count<-data.frame(count(x,month))
plot(month.count$month,month.count$n)

# Plot day of month (not very interesting)
date.count<-data.frame(count(x,date))
plot(date.count$date,date.count$n)

# Plot tweets per day of the week.
dow.df <- data.frame(x$year,x$month,x$date)
names(dow.df) <- c("year","month","day")
dow.df$date <- paste(dow.df$year,dow.df$month,dow.df$day,sep = "-")
dow.df<-filter(dow.df, year != "NA")
dow.df<-filter(dow.df, month != "NA")
dow.df<-filter(dow.df, day != "NA")
dow.df$date <- as.Date(dow.df$date)
dow.df$wkday<-as.POSIXlt(dow.df$date)$wday
dow.count<-data.frame(count(dow.df,wkday))
plot(dow.count$wkday,dow.count$n,
     xlab = "day of week",
     ylab = "number of tweets")
