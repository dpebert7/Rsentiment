# Ebert/Rider
# 3 April 2016
# Create visualizations for LA2014 and LA2016

# packages 
library(plyr)
library(dplyr)

load(file = paste(storage.directory, "x.RData", sep = "")) # load (Spanish-free) LA2014 into memory as x

# Set date and time to POSIXlt form ----
  x$time = as.POSIXlt(paste(paste(x$year, x$month, x$date, sep = "-"), "  ", paste(x$hour, x$minute, x$second, sep = ":"), sep = ""))
  x$year = NULL
  x$month = NULL
  x$date = NULL
  x$hour = NULL
  x$minute = NULL
  x$second = NULL

# Plot tweets per username ----
  name.count = as.data.frame(table(x$username))
  names(name.count)<-c("username","nTweets")
  name.count<-arrange(name.count,desc(nTweets))
  hist(name.count$nTweets)
  plot(name.count$nTweets,
        log = "y",
        pch = 20,
        main = "Tweets per Username",
        xlab = "Unique Usernames",
        ylab = "Number of Tweets")


# Plot hour of day ----
  hour.count = as.data.frame(table(x$time$hour))
  names(hour.count) = c("hour", "frequency")
  hour.count$hour = as.integer(hour.count$hour)
  plot(x = hour.count$hour,
       y = hour.count$frequency,
       type = "p",
       main = "Time of Day",
       xlab = "hour",
       ylab = "number of tweets")

# Plot month (not very interesting) ----
  month.count = as.data.frame(table(x$time$mon))
  names(month.count) = c("month", "frequency")
  month.count$month = as.integer(month.count$month)
  plot(x = month.count$month,
       y = month.count$frequency,
       type = "p",
       main = "Time of Year",
       xlab = "Month",
       ylab = "Numer of Tweets")

# Skip day of month (not very interesting) ----

# Plot tweets per day of the week (dow) ----
  dow.count = as.data.frame(table(x$time$wday))
  names(dow.count) = c("dow", "frequency")
  dow.count$dow = as.integer(dow.count$dow)
  plot(x = dow.count$dow,
       y = dow.count$frequency,
       type = "p",
       main = "Time of Week",
       xlab = "Day of Week",
       ylab = "Numer of Tweets")


#try some junk in ggplot2 ----

library(ggplot2)
ggplot(name.count) +
  aes(nTweets) +
  geom_histogram(binwidth = 200) +
  scale_y_log10() +
  ggtitle("Frequency Histogram") +
  xlab("# of Tweets") +
  ylab("# of Usernames")

# This looks a little weird. I think it's right though. It might be better to switch x and y axes?

ggplot(month.count) + 
  geom_line(aes(x = month, y = frequency),
             colour = "blue", 
            size = 3)






