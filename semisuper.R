# David Ebert
# 21 March 2016
# GOAL: Create a semi-supervised training set from within comtweetsLA

# READ IN DATA ComTweetsLA.csv in +- 2 mins ----
  x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
                c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

# HAPPY EMOTICONS ----
  happy_emoticons = c(":)", "(:", ":-)", "(-:", ":D", ":-D", "=)", "(=", "☺", "☻")

  grep("\\:\\)", x$text, value = TRUE) #67189 :)'s in the whole set. Takes about 11 sec. to run
  grep("\\(\\:", x$text, value = TRUE) #14401 (:'s in the whole set. 
  grep("\\:-\\)", x$text, value = TRUE) #14069 :-)'s in the whole set. 
  grep("\\(-\\:", x$text, value = TRUE) #1903 (-:'s in the whole set. 
  grep("\\:D", x$text, value = TRUE) #9724 :D's in the whole set SEEMS TOO MANY
  grep("\\:-D", x$text, value = TRUE) #20230 :-D's in the whole set
  grep("=\\)", x$text, value = TRUE) #1467 =)'s in the whole set
  grep("\\(=", x$text, value = TRUE) #195 (='s in the whole set
  grep("☺", x$text, value = TRUE) #38441 of them in the whole set
  grep("☻", x$text, value = TRUE) #130 these in the whole set
  grep("☀", x$text, value = TRUE) #7754 of these in the whole set. Leave these out, though

  happy_indices = c(
    grep("\\:\\)", x$text, value = FALSE),
    grep("\\(\\:", x$text, value = FALSE),
    grep("\\:-\\)", x$text, value = FALSE),
    grep("\\(-\\:", x$text, value = FALSE),
    grep("\\:D", x$text, value = FALSE),
    grep("\\:-D", x$text, value = FALSE),
    grep("=\\)", x$text, value = FALSE),
    grep("\\(=", x$text, value = FALSE),
    grep("☺", x$text, value = FALSE),
    grep("☻", x$text, value = TRUE)
  )

  length(happy_indices) #happy_indices has length 148000
  dim(x[happy_indices[duplicated(happy_indices)],]) #698 entries have multiple distinct happy emoticons
  dim(unique(x[happy_indices,])) #There are 147176 unique happy rows
  x = unique(x[happy_indices,]) #Rewrite over x to avoid memory problems
  dim(x) # Looks good
  write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", row.names = FALSE)

  rm(x)
  #reload x after this part. Sorry about the inconvenience :(

# SAD EMOTICONS ----
  happy_emoticons = c(":(", ":-(", "):", ")-:", ":[", "]:", ":{", "}:","=(", ")=", "☹")

  grep("\\:\\(", x$text, value = TRUE) #40065 :('s in the whole set. 
  grep("\\:-\\(", x$text, value = TRUE) #3485 :-('s in the whole set. 
  grep("\\)\\:", x$text, value = TRUE) #5489 ):'s in the whole set. 
  grep("\\)-\\:", x$text, value = TRUE) #355 )-:'s in the whole set. 
  grep(":\\[", x$text, value = TRUE) #70 :['s in the whole set.
  grep("\\]:", x$text, value = TRUE) #2636 ]:'s in the whole set.
  grep(":\\{", x$text, value = TRUE) #32 :{'s in the whole set.
  grep("\\}:", x$text, value = TRUE) #25 }:'s in the whole set.
  grep("=\\(", x$text, value = TRUE) #131 =('s in the whole set
  grep("\\)=", x$text, value = TRUE) #59 )='s in the whole set
  grep("☹", x$text, value = TRUE) #222 of these in the whole set

  
  sad_indices = c(
    grep("\\:\\(", x$text, value = FALSE),
    grep("\\:-\\(", x$text, value = FALSE),
    grep("\\)\\:", x$text, value = FALSE),
    grep("\\)-\\:", x$text, value = FALSE),
    grep(":\\[", x$text, value = FALSE),
    grep("\\]:", x$text, value = FALSE),
    grep(":\\{", x$text, value = FALSE),
    grep("\\}:", x$text, value = FALSE),
    grep("=\\(", x$text, value = FALSE),
    grep("\\)=", x$text, value = FALSE),
    grep("☹️", x$text, value = FALSE) #It appears twitter works with unicode characters. Not necessarily emoji
  )

  length(sad_indices) #sad_indices has length 52347
  dim(x[sad_indices[duplicated(sad_indices)],]) #100 entries have multiple distinct sad emoticons
  dim(unique(x[sad_indices,])) #There are 52247 unique sad rows
  x = unique(x[sad_indices,]) #Write over x to avoid memory problems
  dim(x) # Looks good
  write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", row.names = FALSE)
  
  rm(x)
  #reload x after this part. Sorry about the inconvenience :(
  

# WORD CLOUD for sad and happy tweets ----
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
  