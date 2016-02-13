# Importing large data text data set into R and running sentiment analysis

# ff ----

install.packages("ff")
library(ff)

# This method worked for getting the data into R,
# but I'm not sure what methods to call on the data once
# it's in R. :(



# Following 2 methods suggested at  http://www.mathfinance.cn/handling-large-csv-files-in-R/

# Scan, data.frame, cast ----


cat("TITLE extra line", "2 3 5 7", "11 13 17", file = "ex.data", sep = "\n")
pp <- scan("ex.data", skip = 1)
scan("ex.data", skip = 1)
scan("ex.data", skip = 1, nlines = 1) # only 1 line after the skipped one
scan("ex.data", what = list("","","")) # flush is F -> read "7"
scan("ex.data", what = list("","",""), flush = TRUE)
unlink("ex.data") # tidy up

scan("ComTweetsLA.csv", skip = 1, nlines = 10, what = list("","","",""))

install.packages("reshape")

# This might work, but I'm not sure

# Bigmemory ----


install.packages("bigmemory")
library(bigmemory)

read.big.matrix(filename = "ComTweetsLA.csv", header = TRUE, sep = ",")
help(read.big.matrix)

# bigmemory is part of a suite of packages designed to work with big data sets
# read.big.matrix("comTweetsLA.csv") crashed R the first time around.
# Not really designed for text data

# Read.csv ----

# Turns out that good old read.csv works well if you specify colClasses
# This works quickly up to 1 million entries, and is sufficient for our 10 million entries

x = read.csv(file = "ComTweetsLA.csv", nrows = 10000, header = TRUE)
str(x)

x = read.csv(file = "ComTweetsLA.csv", nrows = 94000, header = TRUE, colClasses = 
               c("character", "character", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
# This method reads 5 million rows in 64 seconds; ~9.4 million rows in 120 seconds.

# time is reduced when specifying colClasses ahead of time!
# system.time works when not assigning x

# Getting data into tm corpus ----

# This is a package for text mining. It will be useful once the data are in R.
install.packages("tm")
library(tm)

x = read.csv(file = "ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
               c("character", "character", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

# Problem: VCorpus treats each of the 11 attributes as a document, rather than the tweets themselves
# Solution: Start corpus by just taking text, in order to create the correct number of documents
dim(x)
my_corpus = VCorpus(VectorSource(x$text))
#str(my_corpus) #works, but long
#summary(my_corpus)
#met(my_corpus)
length(my_corpus) # This shows thare are the correct number of documents

# Then add attributes as necessary:
# username, latitude, longitude, and tweet_id
for (i in 1:length(my_corpus)) {
  attr(my_corpus[[i]], "username") <- as.character(x$username[i])
  attr(my_corpus[[i]], "lat") <- as.numeric(x$lat[i])
  attr(my_corpus[[i]], "long") <- as.numeric(x$long[i])
  attr(my_corpus[[i]], "tweet_id") <- as.numeric(x$tweet_id[i])
}

#VCorpus crashes when using the 9.4 million entries :(
#system.time is 9.3 seconds for 100 000 entries
#system.time is 93 seconds for 1 000 000 entries


# PCorpus rather than VCorpus 

library(tm)
# VCorpus stands for "volatile corpus." The corpus is lost upon quitting R
# What about using PCorpus ("permanent corpus"), which works with files stored outside of R

install.packages("filehash")
library(filehash)

x = read.csv(file = "ComTweetsLA.csv", nrows = 100, header = TRUE, colClasses = 
               c("character", "character", "numeric", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

my_corpus = PCorpus(VectorSource(x$text))
my_corpus # This shows thare are the correct number of documents

y = VCorpus(DirSource("thesource"))
# In order to use PCorpus, we need the package filehash

#SKIP THIS JUNK. JUST TREAT DATA AS IT IS!

# Applying sentiment function ----

library(dplyr) #contains select function
library(sentiment) #sentiment analysis

# Clean.data by removing punctuation, hyperlinks, etc and removing blank rows -----
clean.data = function(data_frame, column_name = "text"){
  require(dplyr)
  for (i in 1:nrow(data_frame)) {
    data_frame[i,column_name]<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", data_frame[i,column_name])
    data_frame[i,column_name]<-gsub("@\\w+", "", data_frame[i,column_name])
    data_frame[i,column_name]<-gsub("[[:punct:]]", "", data_frame[i,column_name])
    data_frame[i,column_name]<-gsub("[[:digit:]]", "", data_frame[i,column_name])
    data_frame[i,column_name]<-gsub("http\\w+", "", data_frame[i,column_name])
    data_frame[i,column_name]<-gsub("[^a-zA-Z]", " ", data_frame[i,column_name])
    data_frame[i,column_name]<-tolower(data_frame[i,column_name])
    data_frame[i,column_name]<-trimws(data_frame[i,column_name])
  }
  return(data_frame)
}

# Sentiment analysis ----
analyze.sentiment = function(data_frame, column_name = "text"){
  require(sentiment)
  for(i in 1:nrow(data_frame)){
    data_frame[i,"sentiment"]= classify_polarity2(data_frame[i,column_name])[3]
  }
  data_frame
}
# This runs very slowly! >1 second per row. Analyzing 1 million tweets at that rate would take conservatively >11 days.
# Note that the clean.data function works adequately fast, with 10000 rows requiring only 8 seconds

# clean.analyze.write ----
# Can we do a step-by-step process for analyzing and writing output to .csv file???
clean.analyze.write = function(begin, end, file_name, data_frame, append = FALSE, column_name = "text"){
    written = 0
    
    if(append == FALSE){
      #setup .csv file
      data_frame$sentiment = NA
      write.table(t(colnames(data_frame)), file_name, col.names = FALSE, row.names = FALSE, sep = ",")
    }
  
  while(begin < end){
    
      #clean
      ans = clean.data(data_frame[begin,], column_name)
      
      if(nchar(ans$text) > 2){
        #append polarity
        ans$sentiment = my.polarity(ans[,column_name])  
        
        #write
        newline <- as.matrix(ans)
        write.table(newline, file = file_name, col.names = FALSE, row.names = FALSE, append = TRUE, sep = ",")
        written = written + 1
      }
    
    if(begin %% 100000 == 0){
      print(paste(begin/1e6, "million entries complete; ", written, "entries written to file this cycle."))
    }
    begin = begin + 1 #increment
  }
  print(paste("Finished!", begin, "entries analyzed", written, "entries written to file this cycle."))
}


a = Sys.time()
print(a)
clean.analyze.write(begin = 1, end = 9384814, file_name = "cleanlatweets.csv", data_frame = x, append = FALSE)
print(Sys.time() - a)


# old clean.analyze.write takes 11 secs to do 10 rows; new one does it in 0.633 seconds
# old clean.analyze.write takes 2.179 mins to do 100 rows; new one does it in 3.3 seconds
# old clean.analyze.write takes 21.950 mins to do 1000 rows; new one does it in 34 seconds
# old clean.analyze.write takes +- 4 hours to do 9000 rows; new one does 10000 rows in a few minutes
# new clean.analyze.write takes 10.2 hours to do 1 million rows, writing 935398 rows to file
# new clean.analyze.write took only 7.1 hours to do 2nd million rows, writing 946884 rows to file
# new clean.analyze.write took 21.49 hours to do next 3 million rows, writing (over) 2828451 rows to file
# new clean.analyze.write took 1.323 days to do the final 4.38 million rows, writing 4147603 rows to file

# Switching to my.sentiment2.csv for final 4.39 million rows
# new clean.analyze.write



# made it through 1:9856 rows of the x data frame.
# the result in sentiment.csv has 9510 rows (of which one is header.)

newdf = read.csv("my.sentiment.csv")
dim(newdf) # should have 12 columns

# speedier sentiment analysis ----
# from www.r-bloggers.com/strategies-to-speedup-r-code/


# Here's the problem: 
mydata = x[1:25,]
system.time(analyze.sentiment(mydata))
# Time difference of 39.271 secs
# TOO SLOOOOW!


# Try loading lexicon before function
a = Sys.time()
analyze.sentiment(mydata)
Sys.time() - a
# Time difference of 32.7035 secs


#Load key facts about the lexicon first
a = Sys.time()
analyze.sentiment(mydata)
Sys.time() - a
# Time difference of 32.92407 secs

# Wiebe's lexicon ----
a = Sys.time()
Wiebe_lexicon = read.csv(system.file("data/subjectivity.csv.gz", 
                     package = "sentiment"), header = FALSE)
Sys.time() - a

dim(Wiebe_lexicon)

table(Wiebe_lexicon$V2)
table(Wiebe_lexicon$V3) # "impassive" is the only weakneg, for some reason
Wiebe_lexicon[which(Wiebe_lexicon$V3 == "both"),] # these are the 18 boths
# has unnecessary words like incredulously, indefatigable, incongruous and incontrovertible

# classify_polarity2 ----
Wiebe_lexicon <- read.csv(system.file("data/subjectivity.csv.gz", 
                                package = "sentiment"), header = FALSE)

classify_polarity2 = function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, 
          prior = 1, verbose = FALSE, lexicon = Wiebe_lexicon, ...) 
{
  matrix <- create_matrix(textColumns, ...)

  counts <- list(positive = 2324, 
                 negative = 4175,
                 total = 6518)
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) 
      print(paste("DOCUMENT", i))
    scores <- list(positive = 0, negative = 0)
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    for (word in words) {
      index <- pmatch(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        score <- pweak
        if (polarity == "strongsubj") 
          score <- pstrong
        if (algorithm == "bayes") 
          score <- abs(log(score * prior/count))
        if (verbose) {
          print(paste("WORD:", word, "CAT:", category, 
                      "POL:", polarity, "SCORE:", score))
        }
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio == 1) 
      best_fit <- "neutral"
    documents <- rbind(documents, c(scores$positive, scores$negative, 
                                    abs(scores$positive/scores$negative), best_fit))
    if (verbose) {
      print(paste("POS:", scores$positive, "NEG:", scores$negative, 
                  "RATIO:", abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  colnames(documents) <- c("POS", "NEG", "POS/NEG", "BEST_FIT")
  return(documents)
}

# my.polarity Looking up words in lexicon with pmatch and findFreqTerms ----

blurb = "Hello. How are you? I am having a super great day. Everything is not so terrible at all."



Wiebe_lexicon2 = cbind(as.character(Wiebe_lexicon$V1), as.numeric(2*(Wiebe_lexicon$V3 == "positive")-1))

my.polarity = function(document){
  sum(as.numeric(Wiebe_lexicon2[pmatch(unlist(strsplit(document, " ")[[1]]), Wiebe_lexicon2[,1], nomatch = 0),2]))
}

strsplit("hello world this is my favorite day", split = " ")
help(strsplit)

### THIS IS WAY FASTER!!!
# > system.time(my.polarity(blurb))
# user  system elapsed 
# 0.000   0.000   0.003 
# > system.time(classify_polarity(blurb))
# user  system elapsed 
# 1.464   0.000   1.462 

# Writing cleanlatweets.csv ----

x1 = read.csv(file = "my.sentiment.csv", nrow = 4800000, header = TRUE, colClasses = 
               c("character", "character", "character", "character", "character", 
                 "character", "character", "character", "character", "character", "character", "character"))

head(x1)
dim(x1)

x1$tweet_id = NULL
x1$sentiment = as.integer(x1$sentiment)
x1$sentiment = NULL
x1$long = as.numeric(x1$long)
x1$lat = as.numeric(x1$lat)
x1$year = as.integer(x1$year)
x1$month = as.integer(x1$month)
x1$date = as.integer(x1$date)
x1$hour = as.integer(x1$hour)
x1$minute = as.integer(x1$minute)
x1$second = as.integer(x1$second)


x2 = read.csv(file = "my.sentiment2.csv", nrow = 4200000, header = TRUE, colClasses = 
                c("character", "character", "character", "character", "character", 
                  "character", "character", "character", "character", "character", "character", "character"))

head(x2)
dim(x2)

x2$tweet_id = NULL
x2$sentiment = as.integer(x2$sentiment)
x2$sentiment = NULL
x2$long = as.numeric(x2$long)
x2$lat = as.numeric(x2$lat)
x2$year = as.integer(x2$year)
x2$month = as.integer(x2$month)
x2$date = as.integer(x2$date)
x2$hour = as.integer(x2$hour)
x2$minute = as.integer(x2$minute)
x2$second = as.integer(x2$second)

x = rbind(x1, x2)
dim(x) #cleaned data has 8.8 million rows
# 8861207/9384813 = 94.42% of latweets remains
help(write.csv)

write.csv(x, file = "cleanlatweets.csv", row.names = FALSE, quote = FALSE) 

# read cleanlatweets.csv back into R ----
x = read.csv(file = "cleanlatweets.csv", nrow = 8900000, header = TRUE, colClasses = c("character", "character", "numeric", "numeric", 
                      "integer", "integer", "integer", "integer", "integer", "integer"))

