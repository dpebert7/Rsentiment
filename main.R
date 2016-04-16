# Ebert/Rider
# 9 April 2016
# Goals for this script:
# 1) Import AFINN, cleaning, and sentiment analysis functions from other scripts
# 2) Apply sentiment analysis to LA2014 and LA2016

##########################
# Import ComTweetsLA as x and remove junk tweets ----

  # Import functions
    source("functions.R")

  # Import x
    x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", skip = 1, nrows = 9400000, header = FALSE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
    colnames(x) = c("text","username","tweet_id","lat","long","year","month","date","hour","minute","second")
    
  # Save x into storage directory
    save(x, file = paste(storage.directory, "x.RData", sep = "")) 
    
  # loaded LA2014 as x
    load(file = paste(storage.directory, "x.RData", sep = "")) 
  
  # remove 1479 tweets that don't have a date
    x = x[!is.na(x$year),]
    
  # remove very Spanish-sounding tweets
    library(cldr)
    x[,c("language", "isReliable")] = detectLanguage(x$text)[c("detectedLanguage", "isReliable")]
    x = x[x$language != "SPANISH" | x$isReliable == FALSE,]
    x$language = NULL
    x$isReliable = NULL
    9383334-9227873 #155461 Spanish-sounding tweets are removed, about 1.5% of the tweets

##########################
# Cleaning and AFINN score ----
  # load LA2014 as x
    load(file = paste(storage.directory, "x.RData", sep = ""))     
  
  # split x into x1 and x2
    #x1 = x[1:4500000,]
    #rm(x)
    x2 = x[4500001:nrow(x),]
    rm(x)
    
  # Save x1  and x2 into storage directory
    #save(x1, file = paste(storage.directory, "x1.RData", sep = "")) 
    save(x2, file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # clean x
    names(x2)[names(x2)=="text"] <- "clean"
    x2$clean = clean.tweets(x2$clean)

  # remove empty rows
    dim(x2)
    x2 = x2[nchar(x2$clean)>2,]
    dim(x2) # remove 258906 empty rows (~2.8%)

  # AFINN sentiment
    x2$AFINN.polarity = classify.sentiment(x2$clean)

  # Check number of columns
    dim(x2) # x now has 12 columns, 1 more than before
    head(x2)

  # Save x1  and x2 into storage directory
    # load(file = paste(storage.directory, "x1.RData", sep = "")) 
    # save(file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # write table
    # write.table(x1, file = paste(storage.directory, "LA2014.csv", sep = ""), row.names = FALSE, sep = ",")
  # append table
    # write.table(x2, file = paste(storage.directory, "LA2014.csv", sep = ""), col.names = FALSE, append = TRUE)
    
    print(Sys.time() - a)
    
##########################
# Random Forest Classifier ----
    a = Sys.time()
    print(a)
    
  # Load x1  and x2 from storage directory
    #load(file = paste(storage.directory, "x1.RData", sep = "")) 
    load(file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # Random Forest Sentiment - too slow right now!
    x2$rf.polarity = classify.polarity.machine(x2$clean, model = rf.model)
    
  # Check number of columns
    dim(x2) # x now has 13 columns, 1 more than before
    head(x2)
    
  # Save x1  and x2 into storage directory
    # save(x1, file = paste(storage.directory, "x1.RData", sep = "")) 
    # save(x2, file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # write table
    # write.table(x1, file = paste(storage.directory, "LA2014.csv", sep = ""), row.names = FALSE, sep = ",")
    # append table
    # write.table(x2, file = paste(storage.directory, "LA2014.csv", sep = ""), col.names = FALSE, append = TRUE)
  
  Sys.time()-a
  beepr::beep(3)





  
# read data back into R as y ----
  y = read.csv(file = paste(storage.directory, "LA2014.csv", sep = ""), skip = 0, nrows = 9000000, header = TRUE, 
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
