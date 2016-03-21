#Ebert/Rider
#25 Feb 2016

#useful functions for other scripts

#AFINN_lexicon
AFINN_lexicon = read.delim(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE, header = F, quote = '')
names(AFINN_lexicon) <- c('word','score')
AFINN_lexicon$word.clean <- gsub('-',' ' , AFINN_lexicon$word)  #Replacing Hyphens with Spaces
AFINN_lexicon$word.clean <- gsub("[[:punct:]]", '', AFINN_lexicon$word.clean)  #Removing punctuation

clean.data = function(documents){
  require(plyr)
  require(dplyr)
  require(qdapRegex)
  cleantext = laply(documents, function(documents)
  {
    #documents<-gsub("n't", " not", documents) #replace contractions ending in n't with not. Might be better for Bayes to not use this.
    documents<-rm_url(documents, replacement = "url") #tokenize urls
    documents<-gsub("RT", "retweet", documents) # tokenize retweets
    documents<-gsub("@\\w+", "username", documents) #tokenize @
    documents<-gsub("#\\w+", "hash", documents) #tokenize #. Not necessary for tweets that haven't been classified yet.
    documents<-gsub("[[:punct:]]", "", documents) #remove punctuation
    documents<-gsub("[[:digit:]]", "", documents) #remove numbers
    documents<-gsub("http\\w+", "", documents) #remove hyperlinks
    documents<-gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents<-tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents) # limit character repeats to maximum 2
    documents<-trimws(documents) #remove leading and trailing whitespace
  }, .progress = "text")
  return(cleantext)
}


AFINN_lexicon.frequencies=function(x){
  str_count(x,AFINN_lexicon$word.clean)
}

ndsi.frequencies=function(x){
  str_count(x,freq.all$word[1:1024])
}

