# use str_count command
# column names for lexicons
# str_count function
# AFINN rating = sum(n_term * afinn.score_term)
# this part is computationally expensive!


# word to vec
# create your own vector list
# handling negation





# Ebert/Rider

# Goals for this script:
# 1) Import lexicons
# 2) Import data using read.csv
# 3) Clean data efficiently, writing to .csv
# 4) Analyze sentiment efficiently, writing to .csv


# Import lexicons ----

#lexicons MUST be formatted so that the first column lists words and the second column gives the sentiment score of that word.

#Wiebe
Wiebe_lexicon = read.csv(system.file("data/subjectivity.csv.gz", 
                                     package = "sentiment"), header = FALSE, stringsAsFactors = FALSE)
Wiebe_lexicon = cbind(as.character(Wiebe_lexicon$V1), as.numeric(2*(Wiebe_lexicon$V3 == "positive")-1))
colnames(Wiebe_lexicon) = c("word", "score")

#AFINN
AFINN_lexicon = read.delim(file = "~/Desktop/Huang Research/Rladata/AFINN/AFINN-111.txt", stringsAsFactors = FALSE)
colnames(AFINN_lexicon) = c("word", "score")

#Negation Lexicon
negations = c("no", "not","none","no one","nobody","nothing","neither","nowhere","never","hardly",
              "scarcely","barely","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")

# Import data ----

# read in ComTweetsLA.csv in +- 2 mins:
x = read.csv(file = "~/Desktop/Huang Research/Rladata/ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
               c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

# Clean Data ----
clean.data = function(documents){
  require(plyr)
  require(dplyr)
  cleantext = laply(documents, function(documents)
    {
    documents<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", documents) #remove hyperlinks
    documents<-gsub("@\\w+", "", documents) #remove @
    documents<-gsub("[[:punct:]]", "", documents) #remove punctuation
    documents<-gsub("[[:digit:]]", "", documents) #remove numbers
    documents<-gsub("http\\w+", "", documents) #remove
    documents<-gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents<-tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents)
    documents<-trimws(documents) #remove leading and trailing whitespace
    }, .progress = "text")
  return(cleantext)
}

# Classify Sentiment ----
classify.sentiment = function(documents, lexicon){
  sentscorevec = laply(documents, function(documents, lex = lexicon)
  {
  words = unlist(strsplit(documents, " ")) #access words
  #eventually add words to term-document matrix?
  indices = pmatch(words, lexicon[,1], nomatch = 0, duplicates.ok = TRUE)
  vals = as.numeric(lexicon[indices,2])
  
  #fix negation
  if(length(words)>1){
    for(i in 2:length(words)){
      if(words[i-1] %in% negations){
        #print(words[(i-1):i])
        vals[length(vals)+1] = (-2)*as.numeric(lexicon[i,2])
      }
    }   
  }
  
  #return sum
  return(sum(vals))
  }, .progress = "text")
  return(sentscorevec)
}



# clean.analyze.write ----

# Import x

a = Sys.time()
print(a)
      x = read.csv(file = "~/Desktop/Huang Research/Rladata/ComTweetsLA.csv", skip = 5000000, nrows = 4400000, header = FALSE, colClasses = 
                     c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
      colnames(x) = c("text","username","tweet_id","lat","long","year","month","date","hour","minute","second")
      
      # clean x
      x[,"text"] = clean.data(x$text)
      
      # remove empty rows
      x = x[nchar(x$text)>2,]
      
      # Wiebe Sentiment x
      x[,"Wiebe.sentiment"] = classify.sentiment(x$text, lexicon = Wiebe_lexicon)
      
      # AFINN sentiment x
      x[,"AFINN.sentiment"] = classify.sentiment(x$text, lexicon = AFINN_lexicon)
      
      dim(x) # text now has 14 columns, 3 more than before
      head(x)
      
      # write table
      #write.table(x, file = "latweets.sentiment.csv", row.names = FALSE, sep = ",")
      # append table
      write.table(x, file = "latweets.sentiment.csv", row.names = FALSE, sep = ",", col.names = FALSE, append = TRUE)
      
      rm(x)

print(Sys.time() - a)



# read data back into R:
y = read.csv(file = "latweets.sentiment.csv", skip = 0, nrows = 8900000, header = TRUE, 
             colClasses = c("character", "character", "character", "numeric", "numeric", "integer", 
                            "integer", "integer", "integer", "integer", "integer", "integer", "integer"))
