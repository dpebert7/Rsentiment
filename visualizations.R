library(plyr)
library(dplyr)

ComTweetsLA$text<-as.character(ComTweetsLA$text)
ComTweetsLA$username<-as.character(ComTweetsLA$username)
summary(ComTweetsLA)
summary(ComTweetsLA$username)

name.count<-data.frame(count(ComTweetsLA,username))
# name.count$username<-as.character(name.count$username)
names(name.count)<-c("username","frequency")
name.count<-filter(name.count,username != "")
name.count<-arrange(name.count,desc(frequency))
hist(name.count$frequency)

hour.count<-data.frame(count(ComTweetsLA,hour))
plot(hour.count$hour,hour.count$n)

month.count<-data.frame(count(ComTweetsLA,month))
plot(month.count$month,month.count$n)

date.count<-data.frame(count(ComTweetsLA,date))
plot(date.count$date,date.count$n)

dow.df <- data.frame(ComTweetsLA$year,ComTweetsLA$month,ComTweetsLA$date)
names(dow.df) <- c("year","month","day")
dow.df$date <- paste(dow.df$year,dow.df$month,dow.df$day,sep = "-")
dow.df<-filter(dow.df, year != "NA")
dow.df<-filter(dow.df, month != "NA")
dow.df<-filter(dow.df, day != "NA")
dow.df$date <- as.Date(dow.df$date)
dow.df$wkday<-as.POSIXlt(dow.df$date)$wday
dow.count<-data.frame(count(dow.df,wkday))
plot(dow.count$wkday,dow.count$n)
