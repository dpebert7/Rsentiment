# Purpose:
# Continually capture tweets from LA county

# From 1/29/2016 to 3/1/2016 this script collected ~5 million tweets from LA county
# The most up-to-date version of this script is running on the shared school computer.

library(streamR)
library(rjson)
library(ROAuth)
load("my_oauth.Rdata")
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



#keepCapturingTweets = function(file.name, stop.time){
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


# store tweets in .csv files called LAData1, LAData2, etc., changing file name
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


