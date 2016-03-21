# David Ebert
# 19 March 2016
# GOAL: Create a semi-supervised training set from within comtweetsLA

# read in ComTweetsLA.csv in +- 2 mins:
x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
               c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))


x$semiclean = gently.clean.data(x$text)
length(grep("\\bhome\\b:)", x$semiclean, value = FALSE)) # value = FALSE returns row number. "\\bhome\\b" searches for EXACTLY "home"
grep("\\bhome\\b|\\bhappy\\b", x$semiclean, value = TRUE) # value = TRUE returns the cell contents rather than the row number.


# HAPPY EMOTICONS
length(grep("\\:\\)", x$text, value = TRUE)) #67189 :)'s in the whole set. Takes about 11 sec. to run
length(grep("\\(\\:", x$text, value = TRUE)) #14401 (:'s in the whole set. 
length(grep("\\:-\\)", x$text, value = TRUE)) #14069 :-)'s in the whole set. 
length(grep("\\(-\\:", x$text, value = TRUE)) #1903 (-:'s in the whole set. 
length(grep("\\:D", x$text, value = TRUE)) #9724 :D's in the whole set SEEMS TOO MANY
length(grep("\\:-D", x$text, value = TRUE)) #20230 :-D's in the whole set
length(grep("<3", x$text, value = TRUE)) # 13287 <3's in the whole set (This is probably a bad one to use.)

happy_indices = c(
  grep("\\:\\)", x$text, value = FALSE),
  grep("\\(\\:", x$text, value = FALSE),
  grep("\\:-\\)", x$text, value = FALSE),
  grep("\\(-\\:", x$text, value = FALSE),
  grep("\\:D", x$text, value = FALSE),
  grep("\\:-D", x$text, value = FALSE)
)

length(happy_indices) #happy_indices has length 107767
x[happy_indices[duplicated(happy_indices)],] #461 entries have multiple distinct happy emoticons
dim(unique(x[happy_indices,])) #There are 107306 unique happy rows
x = unique(x[happy_indices,]) #Rewrite over x to avoid memory problems
dim(x) # Looks good
write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", row.names = FALSE)



#SAD EMOTICONS
length(grep("\\:\\(", x$text, value = TRUE)) #40065 :('s in the whole set. 
length(grep("\\:-\\(", x$text, value = TRUE)) #3485 :-('s in the whole set. 
length(grep("\\)\\:", x$text, value = TRUE)) #5489 ):'s in the whole set. 
length(grep("\\)-\\:", x$text, value = TRUE)) #355 )-:'s in the whole set. 
length(grep(":\\[", x$text, value = TRUE)) #70 :['s in the whole set.
length(grep(":\\{", x$text, value = TRUE)) #32 :{'s in the whole set.


sad_indices = c(
  grep("\\:\\(", x$text, value = FALSE),
  grep("\\:-\\(", x$text, value = FALSE),
  grep("\\)\\:", x$text, value = FALSE),
  grep("\\)-\\:", x$text, value = FALSE),
  grep(":\\[", x$text, value = FALSE),
  grep(":\\{", x$text, value = FALSE) 
)

length(sad_indices) #sad_indices has length 49496
dim(x[sad_indices[duplicated(sad_indices)],]) #92 entries have multiple distinct sad emoticons
dim(unique(x[sad_indices,])) #There are 49404 unique sad rows
x = unique(x[sad_indices,]) #Write over x to avoid memory problems
dim(x) # Looks good
write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", row.names = FALSE)


# Word cloud for sad and happy tweets ----
library(wordcloud)
library(tm)
# load happy and sad tweets
happy_tweets = read.csv(file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", nrows = 110000, header = TRUE, colClasses = 
               c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
dim(happy_tweets)

sad_tweets = read.csv(file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", nrows = 50000, header = TRUE, colClasses = 
                          c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
dim(sad_tweets)

#clean tweets 
# Note that tokenization was turned off for this step to avoid getting a hunge "USERNAME" in the middle of the 
happy_tweets$clean = clean.data(happy_tweets$text)
sad_tweets$clean = clean.data(sad_tweets$text)

#Create corpus using tm package
happy_corpus = Corpus(VectorSource(happy_tweets$clean))
sad_corpus = Corpus(VectorSource(sad_tweets$clean))

#Remove stop words
happy_corpus <- tm_map(happy_corpus, function(x)removeWords(x,stopwords())) # ~2 minutes
sad_corpus <- tm_map(sad_corpus, function(x)removeWords(x,stopwords())) # ~1 minute

#Make word cloud. Check reference for options
wordcloud(happy_corpus, max.words = 200) # ~4 minutes
wordcloud(sad_corpus, max.words = 200) # ~3 minutes



