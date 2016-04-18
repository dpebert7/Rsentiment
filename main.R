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
  
  # load LA2014 as x1 and x2
    load(file = paste(storage.directory, "x1.RData", sep = "")) 
    load(file = paste(storage.directory, "x2.RData", sep = "")) 
  
  # split x into x1 and x2
    #x1 = x[1:4500000,]
    #rm(x)
    #x2 = x[4500001:nrow(x),]
    #rm(x)
    
  # Save x1  and x2 into storage directory
    #save(x1, file = paste(storage.directory, "x1.RData", sep = "")) 
    #save(x2, file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # clean x
    names(x)[names(x)=="text"] <- "clean"
    x$clean = clean.tweets(x$clean)

  # remove empty rows
    dim(x)
    x = x[nchar(x2$clean)>2,]
    dim(x) # remove 258906 empty rows (~2.8%)

  # AFINN polarity
    x$AFINN.polarity = classify.sentiment(x2$clean)

  # Check number of columns
    dim(x) # x now has 12 columns, 1 more than before
    head(x)

  # Save x1  and x2 into storage directory
    #save(x1, file = paste(storage.directory, "x1.RData", sep = "")) 
    #save(x2, file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # write table
    # write.table(x1, file = paste(storage.directory, "LA2014.csv", sep = ""), row.names = FALSE, sep = ",")
  # append table
    write.table(x2, file = paste(storage.directory, "LA2014.csv", sep = ""), col.names = FALSE, append = TRUE)
    
    print(Sys.time() - a)
    
##########################
# Random Forest Classifier ----
    a = Sys.time()
    print(a)
    
  # Load x1  and x2 from storage directory
    load(file = paste(storage.directory, "x1.RData", sep = "")) 
    load(file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # Random Forest Sentiment - too slow right now!
    x$rf.polarity = classify.polarity.machine(x$clean, model = rf.model)
    
  # Check number of columns
    dim(x) # x now has 13 columns, 1 more than before
    head(x)
    
  # rf.polarity 
    x$rf.polarity = classify.polarity.machine(x$clean, chunk.size = 5000)
    
  # Save x1  and x2 into storage directory
    # save(x1, file = paste(storage.directory, "x1.RData", sep = "")) 
    # save(x2, file = paste(storage.directory, "x2.RData", sep = "")) 
    
  # write table
    # write.table(x1, file = paste(storage.directory, "LA2014_4-17-2016.csv", sep = ""), row.names = FALSE, sep = ",")
  # append table
    # write.table(x2, file = paste(storage.directory, "LA2014_4-17-2016.csv", sep = ""), col.names = FALSE, append = TRUE)
  
  Sys.time()-a
  beepr::beep(3)


##########################
  # Combine x1 and x2 and save as LA2014 ----
    load(file = paste(storage.directory, "x1.RData", sep = "")) 
    load(file = paste(storage.directory, "x2.RData", sep = ""))
    dim(x2)
    LA2014 = rbind(x1,x2)
    save(LA2014, file = paste(storage.directory, "LA2014.RData", sep = ""))

##########################
  # read LA2014 data back into R for analysis ----
    load(file = paste(storage.directory, "LA2014.RData", sep = ""))
  
  # Check scores ----
    dim(LA2014) #8968967 rows
    range(LA2014$AFINN.polarity) # -135  190, very wide!
    range(LA2014$rf.polarity) #       0    1, as expected
    
    mean(LA2014$AFINN.polarity) # 0.0608991
    mean(LA2014$rf.polarity)    # 0.4824181, interesting that it's below 0.5. Note that a blank tweet has polarity 0.3
    
    hist(LA2014$AFINN.polarity) # try to change y-axis to log scale. Maybe use ggplot
    hist(LA2014$rf.polarity)    # This looks nicer, but 
    
    quantile(LA2014$AFINN.polarity, c(0, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1))
    quantile(LA2014$rf.polarity,    c(0, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1))
    
  # VERY HAPPY:
    LA2014[LA2014$AFINN.polarity>50, c("clean", "AFINN.polarity", "rf.polarity")] #again, lots of repeated words. Fewer negation problems.
    dim(LA2014[LA2014$rf.polarity == 1, c("clean", "AFINN.polarity", "rf.polarity")]) #over 250 000 have polarity 1
  
  # VERY SAD
    LA2014[LA2014$AFINN.polarity<(-75), c("clean", "AFINN.polarity", "rf.polarity")] #again, lots of repeated words. Fewer negation problems.
    dim(LA2014[LA2014$rf.polarity == 0, c("clean", "AFINN.polarity", "rf.polarity")]) #over 730 000 have polarity 0
    
