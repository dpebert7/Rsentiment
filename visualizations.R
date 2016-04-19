# Ebert/Rider
# 3 April 2016
# Create visualizations for LA2014 and LA2016

# packages 
library(plyr)
library(dplyr)

load(file = paste(storage.directory, "LA2014.RData", sep = "")) # load LA2014 into memory as LA2014

# Set date and time to POSIXlt form ----
  LA2014$time = as.POSIXlt(paste(paste(LA2014$year, LA2014$month, LA2014$date, sep = "-"), "  ", paste(LA2014$hour, LA2014$minute, LA2014$second, sep = ":"), sep = ""))
  LA2014$year = NULL
  LA2014$month = NULL
  LA2014$date = NULL
  LA2014$hour = NULL
  LA2014$minute = NULL
  LA2014$second = NULL
  LA2014$time = as.POSIXlt(LA2014$time - (7*3600)) # subtract 7 hours to change from UTC to PST
  LA2014$time = as.POSIXct(LA2014$time)
  
  
# May still need to +/- 3600's on time column to change time zone

# Plot tweets per username ----
  name.count = as.data.frame(table(LA2014$username))
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
  hour.count = as.data.frame(table(LA2014$time$hour))
  names(hour.count) = c("hour", "frequency")
  hour.count$hour = as.integer(hour.count$hour)
  plot(x = hour.count$hour,
       y = hour.count$frequency,
       type = "l",
       main = "Time of Day",
       xlab = "hour",
       ylab = "number of tweets")


# Plot month (not very interesting) ----
  month.count = as.data.frame(table(LA2014$time$mon))
  names(month.count) = c("month", "frequency")
  month.count$month = as.integer(month.count$month)
  plot(x = month.count$month,
       y = month.count$frequency,
       type = "l",
       main = "Time of Year",
       xlab = "Month",
       ylab = "Numer of Tweets") # Why is this so varied? It makes sense to have more tweets in the summer, but not 3 times more...

# Skip day of month (not very interesting) ----

# Plot tweets per day of the week (dow) ----
  dow.count = as.data.frame(table(LA2014$time$wday))
  names(dow.count) = c("dow", "frequency")
  dow.count$dow = as.integer(dow.count$dow)
  plot(x = dow.count$dow,
       y = dow.count$frequency,
       type = "l",
       main = "Time of Week",
       xlab = "Day of Week",
       ylab = "Numer of Tweets") # day 0 is sunday. Note that there may be problems with time zone still. May need to +/- 3600's to change hours


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


# plot rf.polarity scores
ggplot(LA2014) +
  aes(rf.polarity) +
  geom_histogram(binwidth = 0.04) +
  ggtitle("Random Forest Polarity Histogram") +
  xlab("Random Forest Polarity") +
  ylab("# of Tweets")

# plot AFINN.polarity scores
AFINN.count = as.data.frame(table(LA2014$AFINN.polarity))
colnames(AFINN.count) = c("AFINN.polarity", "Freq")

ggplot(AFINN.count) +
  aes(AFINN.polarity) +
  geom_bar(aes(weight = Freq)) +
  scale_y_log10() +
  ggtitle("AFINN Polarity Histogram") +
  xlab("AFINN Polarity") +
  ylab("# of Tweets")




# plot AFINN.polarity scores vs rf.polarity scores for LA2014 sample
index = sample(nrow(LA2014), size = 100000)
LAsample = LA2014[index,]
#LAsample = LAsample[LAsample$AFINN.polarity > (-30) & LAsample$AFINN.polarity < 30,] # this is kind of cheating...

ggplot(LAsample, aes(rf.polarity, AFINN.polarity)) +
  geom_point() +
  ggtitle("rf.polarity vs AFINN.polarity") +
  xlab("rf.polarity") +
  ylab("AFINN.polarity")


# plot AFINN.polarty scores vs rf.polarity scores for Sent140
# load sent140
load(file = paste(storage.directory, "sent140.RData", sep = ""))
sent140$rf.polarity = classify.polarity.machine(sent140$clean, chunk.size = 100)
sent140$AFINN.polarity = classify.sentiment(sent140$clean)
sent140$polarity = as.factor(sent140$polarity)

ggplot(sent140, aes(rf.polarity, AFINN.polarity, color = polarity)) +
  geom_point(size = 2) +
  ggtitle("Sent140 Classification") +
  xlab("rf.polarity") +
  ylab("AFINN.polarity")     # <-- this is the first really good plot!


