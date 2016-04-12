# Ebert/Rider
# collectlatweets.R
# updated 23 March 2016

# This file runs the function keepCapturingTweets() continually on the school computer,
# collecting about 160k-180k tweets every 24 hours and storing it in a .json file 
# that is about 500MB in size.

library(ROAuth)
load("my_oauth.Rdata") # Note to avoid reusing credentials, "my_oauth.Rdata" should only be 
                       # used on the school computer

keepCapturingTweets = function(){
  require(streamR)
  require(rjson)
  i = Sys.Date()
  
  while(1!=0){
    print(Sys.time())
    temp.file.json = paste(Sys.Date(), ".json", sep = "")    
    
    while (Sys.Date()==i){
      filterStream(file.name = temp.file.json, # Save tweets in temporary .json file
                   # Note that this file isn't overwritten after timeout
                   track = c(""), # Collect any tweets from stream; no search term
                   language = "en", # English tweets
                   location = c(-119, 33, -117, 35), # LA county coordinates. 
                                                     # Note that some tweets just outside the location area may also be collected.
                   timeout = 1800, # Keep connection alive for up to 30 minutes at a time
                   oauth = my_oauth) # Use my_oauth file as the OAuth credentials
      Sys.sleep(1) #pause very briefly before reopening stream
    }
    
    # parse temp.json into R
    temp.df <- parseTweets(temp.file.json, simplify = FALSE) 
    
    i = Sys.Date() #increment i to new day
  }
}

# store tweets in .json files called <today's date>.json, changing file name within 
# 30 minutes after midnight

keepCapturingTweets()

