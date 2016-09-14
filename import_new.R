# David Ebert
# September 13, 2016

# Import NEW 2016 tweets for analysis.

library(streamR)
library(feather)
library(cldr)


new_data_path = "~/Desktop/Huang Research/September_LaPY_data/"
setwd(new_data_path)


a = Sys.time()
temp_df = parseTweets(tweets = "raw_data/2016-09-11.json")
Sys.time()-a
# ~2 minutes per script!

temp_df = temp_df[,c("text", "screen_name", "id_str", "place_lat", "place_lon", "created_at")]



#################################
#### big for loop below here ####
#################################

raw_file_path
feather_file_path
csv_file_path
max_tweets_per_file



import_new_tweets = function(raw_file_path = "~/Desktop/Huang Research/September_LaPY_data/raw_data/",
                             feather_file_path = "~/Desktop/Huang Research/September_LaPY_data/feather_data/",
                             csv_file_path = "~/Desktop/Huang Research/September_LaPY_data/csv_data/",
                             max_tweets_per_file = 1000000){
  require(streamR)
  require(feather)
  require(cldr)
  
  perm_df = data.frame()
  j=1
  
  for(i in list.files(path = raw_file_path)){
    print(paste("Reading ", i, "...", sep = ""))
    
    #Import one .json file into R as data frame
    temp_df = parseTweets(tweets = paste(raw_file_path, i, sep = ""))
    
    
    print(paste("Cleaning ", i, "...", sep = ""))
    #Remove unnecessary rows
    temp_df = temp_df[,c("text", "screen_name", "id_str", "place_lat", "place_lon", "created_at")]
    
    #Remove rows with no text
    temp_df = temp_df[temp_df$text!="",]
    
    #Remove rows with very Spanish-sounding tweets.
    temp_df[,c("language", "isReliable")] = detectLanguage(temp_df$text)[c("detectedLanguage", "isReliable")]
    temp_df = temp_df[temp_df$language != "SPANISH" | temp_df$isReliable == FALSE,]
    temp_df$language = NULL
    temp_df$isReliable = NULL 
    
    #Combine temp_df and perm_df
    perm_df = rbind(perm_df, temp_df)
    
    
    #If perm_df is too big OR if last file then write it to file and restart
    if(nrow(perm_df) >= max_tweets_per_file | i==tail(list.files(path = raw_file_path),1)){
      print("Writing to file...")
      write_feather(x = perm_df, path = paste(feather_file_path, j, ".feather", sep=""))
      write.csv(x = perm_df, file = paste(csv_file_path, j, ".csv", sep=""))
      j = j+1
      if (nrow(perm_df)!=max_tweets_per_file) {
        perm_df = perm_df[(max_tweets_per_file+ 1):nrow(perm_df),]
      }
      else {
        perm_df = data.frame() 
      }
    }
  }
}


