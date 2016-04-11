# Ebert/Rider
# 9 April 2016
# Goals for this script:
# 1) Import AFINN, cleaning, and sentiment analysis functions from other scripts
# 2) Apply sentiment analysis to LA2014 and LA2016


# Pre-cleaning ComTweetsLA and saving as x ----

  # Import functions
    source("functions.R")

  # Import x
    x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", skip = 1, nrows = 9400000, header = FALSE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
    colnames(x) = c("text","username","tweet_id","lat","long","year","month","date","hour","minute","second")
  
  # remove 1479 tweets that don't have a date
    x = x[!is.na(x$date),]
    
  # Set date and time
    x$time = as.POSIXlt(paste(paste(x$year, x$month, x$date, sep = "-"), "  ", paste(x$hour, x$minute, x$second, sep = ":"), sep = ""))
    x$year = NULL
    x$month = NULL
    x$date = NULL
    x$hour = NULL
    x$minute = NULL
    x$second = NULL
    
  # remove very Spanish-sounding tweets
    x[,c("language", "isReliable")] = detectLanguage(x$text)[c("detectedLanguage", "isReliable")]
    x = x[x$language != "SPANISH" | x$isReliable == FALSE,]
    x$language = NULL
    x$isReliable = NULL
    9383334-9227873
    
  # Save x into storage directory
    save(x, file = paste(storage.directory, "x.RData", sep = "")) 

# Sentiment analysis of ComTweetsLA, loaded as x ----
  # load x from storage directory
    a = Sys.time()
    print(a)
    load(file = paste(storage.directory, "x.RData", sep = "")) 
    
  # clean x
    x[,"text"] = clean.tweets(x$text)

  # remove empty rows
    x = x[nchar(x$text)>2,]

  # AFINN sentiment
    x[,"AFINN.polarity"] = classify.sentiment(x$text, lexicon = AFINN_lexicon)

  #Skip rerunning Wiebe through sentiment2 because it's not as good and will run slower
    dim(x) # text now has 12 columns, 1 more than before
    head(x)
    
  # Random Forest Sentiment -- coming soon!

  # write table
    write.table(x, file = paste(storage.directory, "LA2014.csv", sep = ""), row.names = FALSE, sep = ",")
  # append table
    # write.table(x, file = paste(storage.directory, "LA2014.csv", sep = ""), col.names = FALSE, append = TRUE)

  rm(x)

  print(Sys.time() - a)


  
# read data back into R as y ----
  y = read.csv(file = paste(storage.directory, "LA2014.csv", sep = ""), skip = 0, nrows = 2000, header = TRUE, 
                 colClasses = c("character", "character", "character", "numeric", "numeric", "POSIXct", "integer"))
  y$time = as.POSIXlt(y$time)
  
  # Sample 100
  set.seed(100)
  idx = sample(nrow(y), 100)
  y.sample = y[idx,c("text", "username", "AFINN.polarity")]


# Check scores ----
dim(y) #8859604 rows
range(y$AFINN.polarity) # -135  88
mean(y$AFINN.polarity) # 0.4408979
hist(y$AFINN.polarity) # try to change y-axis to log scale. Maybe use ggplot
quantile(y$AFINN.polarity, c(0, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1))


#look at some of the extreme entries
hist(y$AFINN.polarity[y$AFINN.polarity > 25 | y$AFINN.polarity < (-25)]) #tails of AFINN.polarity

# VERY HAPPY:
y[y$AFINN.polarity>50, c("text", "AFINN.polarity")] #again, lots of repeated words. Fewer negation problems.

# VERY SAD
y[y$AFINN.polarity<(-75), c("text", "AFINN.polarity")] #again, lots of repeated words. Fewer negation problems.


#analyze sample of 100000 ----
set.seed(100)
idx = sample(nrow(y), 100000)
y.sample = y[idx,c("text", "username", "AFINN.polarity")]
