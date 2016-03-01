# Purpose:
# Collect a semi-supervised set of tweets from LA county

# From 1/29/2016 to 3/1/2016 this script collected ~5 million tweets from LA county



library(streamR)
library(rjson)
library(ROAuth)
library(twitteR)


#install the necessary packages
library("twitteR")
library("wordcloud")
library("tm")

#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'A2X4Lbz4uvqOAiGQ3UEAGvmFn'
consumer_secret <- 'xa6unTGB4U6dgthoSExAyL2IXWqK7jVTB6sqaS55eUDBMcfiOy'
access_token <- '4501554494-msdK1BIdGvhYjCmTEzEgJd0TV8dN84Ygxo3F5a1'
access_secret <- 'RD10EsZOCfyAjpRXKrVuB8vBNSvMU390kL29VLvxo1t7Z'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)


# search for happy tweets near LA county after January 29th
happy <- searchTwitter(searchString = "#yes!",
                       n=10, lang = "en", locale = "los angeles", since = "2016-01-29", until = "2016-02-24")
happy


#happy search terms and emoticons:
happy_search_terms = c("#happy", "#joy", "#awesome", ":D", ":)", "#thrilled", "#love", "#great", "#sohappy")

#sad search terms and emoticons:
sad__search_terms = c("#sad", "#angry", "#sucks", ":(", ":-(", "#horrible", "#awful", "#terrible", "#sosad")
# using this method, happy_terms achieved 10000 #happy's and 10000 #joy's before reaching rate limit.


grabLAtweets = function(happy_terms = happy_search_terms, sad_terms = sad_search_terms, ntweets = 1000, waitMinutes = 30){
  
  startTime = Sys.time()
  happy_tweets = list()
  sad_tweets = list()
  a = Sys.time()-(waitMinutes*60+1) #start clock waitMinutes minutes ago 
  
  for(i in happy_terms){
    while(difftime(Sys.time(), a, units = "mins")<waitMinutes){
      Sys.sleep(10) #wait until time has passed...
    }
    #...then collect happy tweets.
    newtweets <- searchTwitter(searchString = i, 
                               n=ntweets, lang = "en", locale = "los angeles", since = "2016-01-29", until = "2016-02-24")
    a = Sys.time()
    happy_tweets = c(happy_tweets, newtweets)
    happy_tweets_df = twListToDF(happy_tweets)
    print(paste("happy_tweets has a length of",length(happy_tweets)))
    write.csv(happy_tweets_df, file = "happy_tweets.csv")
  }
  
  Sys.time()-startTime
  
  for(i in sad_terms){
    while(difftime(Sys.time(), a, units = "mins")<waitMinutes){
      Sys.sleep(10) #wait until time has passed...
    }
    #...then collect tweets
    newtweets <- searchTwitter(searchString = i, 
                               n=ntweets, lang = "en", locale = "los angeles", since = "2016-01-29", until = "2016-02-24")
    a = Sys.time()
    sad_tweets = c(sad_tweets, newtweets)
    sad_tweets_df = twListToDF(sad_tweets)
    print(paste("sad_tweets has a length of",length(sad_tweets)))
    write.csv(sad_tweets_df, file = "sad_tweets.csv")
  }
  
  print(paste("Task complete... happy_tweets has ", length(happy_tweets),
              "tweets and sad_tweets has", length(sad_tweets), "tweets."))
  Sys.time()-startTime
}


#still need to remove duplicates
newtweets <- searchTwitter(searchString = "#sad",
                           n=10000, 
                           lang = "en", 
                           locale = "los angeles", 
                           since = "2016-01-29", 
                           until = "2016-02-29")








r_tweets <- searchTwitter('patriots', geocode='42.375,-71.1061111,100mi')

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#alternative steps if you're running into problems 
r_stats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)















# streamR function
filterStream(file.name = "example.json", # Save tweets in a json file
             track = c(""), # Collect any tweets from stream; no search term
             language = "en",
             location = c(-119, 33, -117, 35), 
             timeout = 10, # Keep connection alive 1 hour
             #tweets = 10000, # how many tweets to collect
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

# latweets.json will NOT be overwritten; information is appended
# This method successfully streamed and parsed 100 tweets from 
# LA county in less than a minute



supervised_corpus = function(file.name, stop.time){
  require(streamR)
  require(rjson)
  i = 25
  a = Sys.time()
  print(a)
  
  while(1!=0){
    print(Sys.time())
    temp.file.json = paste(file.name, i, ".json", sep = "")    
    
    while (difftime(Sys.time(), a, units = "secs")<stop.time){
      filterStream(file.name = temp.file.json, # Save tweets in temporary .json file
                   track = c(""), # Collect any tweets from stream; no search term
                   language = "en",
                   location = c(-119, 33, -117, 35), 
                   timeout = 3600, # Keep connection alive for up to 30 minutes at a time
                   oauth = my_oauth) # Use my_oauth file as the OAuth credentials
      Sys.sleep(1) #pause briefly before reopening stream
    }
    
    # parse temp.json into R
    temp.df <- parseTweets(temp.file.json, simplify = FALSE) 
    
    #isolate correct tweet attributes
    keep.df = temp.df[,c("text","place_lat", "place_lon",  "screen_name", 
                         "id_str", "followers_count", "statuses_count", 
                         "favourites_count")]
    
    temp.file.csv = paste(file.name, i, ".csv",sep = "")
    write.csv(keep.df, temp.file.csv)
    
    i = i+1 #increment i
    a = Sys.time() #reset system time to 0
  }
}
# store tweets in .csv files called supervision1, supervision2, etc., changing file name
# every 12 hours
keepCapturingTweets(file.name = "LAData", stop.time = 86400)

read.csv("LAData1.csv", 
         colClasses = c("character", "character", "double", "double", 
                        "character", "character", "integer", "integer"),
         header = TRUE)


# making a small data frame for practice ----
example.df <- parseTweets("example.json", simplify = FALSE)
write.csv(example.df, file = "exampleLAtweets.csv", row.names = TRUE)
read.csv("newlatweets.csv")

# parse the json file and save to a data frame called tweets.df. 
# Simplify = FALSE ensures that we include lat/lon information in that data frame.


#filterstream function ----

filterStream
function (file.name = NULL, track = NULL, follow = NULL, locations = NULL, 
          language = NULL, timeout = 0, tweets = NULL, oauth = NULL, 
          verbose = TRUE) 
{
  if (!is.null(oauth)) {
    library(ROAuth)
  }
  open.in.memory <- FALSE
  if (all(is.null(c(track, follow, language, locations)))) {
    stop("No filter parameter was specified. At least one is necessary. \n    \t\tSee ?filterStream for more information about this error.")
  }
  if (all(is.null(c(track, follow, locations))) & !is.null(language)) {
    stop("Language parameter can only be used in combination with other filter parameters.")
  }
  if ((missing(file.name) || is.character(file.name) == FALSE)) {
    stop("The file where the tweets will be stored was not named properly.")
  }
  if (timeout < 0 || is.numeric(timeout) == FALSE || length(timeout) > 
      1) {
    stop("The specified time out was not properly formatted.")
  }
  if (is.null(oauth)) {
    stop("No authentication method was provided. \n   \t\tPlease use an OAuth token.")
  }
  if (!is.null(oauth)) {
    if (!inherits(oauth, "OAuth")) {
      stop("oauth argument must be of class OAuth")
    }
    if (!oauth$handshakeComplete) {
      stop("Oauth needs to complete its handshake. See ?filterStream.")
    }
  }
  params <- buildArgList(track, follow, language, locations, 
                         oauth = oauth)
  i <- 0
  if (!is.null(file.name)) {
    if (verbose == TRUE) 
      message("Capturing tweets...")
    if (nchar(file.name) == 0) {
      open.in.memory <- TRUE
      file.name <- tempfile()
    }
    conn <- file(description = file.name, open = "a")
    write.tweets <- function(x) {
      if (nchar(x) > 0) {
        i <<- i + 1
        writeLines(x, conn, sep = "")
      }
    }
    if (!is.null(tweets) && is.numeric(tweets) && tweets > 0) {
      write.tweets <- function(x) {
        if (i >= tweets) {
          break
        }
        if (nchar(x) > 0) {
          i <<- i + 1
          writeLines(x, conn, sep = "")
        }
      }
    }
  }
  init <- Sys.time()
  url <- "https://stream.twitter.com/1.1/statuses/filter.json"
  if (!is.null(oauth)) {
    output <- tryCatch(oauth$OAuthRequest(URL = url, params = params, 
                                          method = "POST", customHeader = NULL, timeout = timeout, 
                                          writefunction = write.tweets, cainfo = system.file("CurlSSL", 
                                                                                             "cacert.pem", package = "RCurl")), error = function(e) e)
  }
  if (!is.null(file.name)) {
    close(conn)
  }
  seconds <- round(as.numeric(difftime(Sys.time(), init, units = "secs")), 
                   0)
  if (open.in.memory == TRUE) {
    raw.tweets <- readLines(file.name, warn = FALSE, encoding = "UTF-8")
    if (verbose == TRUE) {
      message("Connection to Twitter stream was closed after ", 
              seconds, " seconds with up to ", length(raw.tweets), 
              " tweets downloaded.")
    }
    unlink(file.name)
    return(raw.tweets)
  }
  if (open.in.memory == FALSE) {
    if (verbose == TRUE) {
      message("Connection to Twitter stream was closed after ", 
              seconds, " seconds with up to ", i, " tweets downloaded.")
    }
  }
}


