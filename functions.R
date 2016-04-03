#Ebert/Rider
#Updated 23 March 2016


# Storage directory for loading R objects
storage.directory = "~/Desktop/Huang Research/Rsentiment/"

# Load libraries for other scripts
library(stringr) #library for str_count function
library(e1071) # for naive bayes model
library(ggplot2) #for graphs
library(caret) #for confusionMatrix
library(pROC) #ROC curves
library(randomForest)
library(rpart)
library(tm) # for building term frequency matrix from corpus


#AFINN_lexicon
  AFINN_lexicon = read.delim(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE, header = F, quote = '')
  names(AFINN_lexicon) <- c('word','score')
  AFINN_lexicon$word.clean <- gsub('-',' ' , AFINN_lexicon$word)  #Replacing Hyphens with Spaces
  AFINN_lexicon$word.clean <- gsub("[[:punct:]]", '', AFINN_lexicon$word.clean)  #Removing punctuation


clean.tweets = function(documents, 
                        usernameToken = "usernametoken", 
                        hashToken = " hashtoken ", 
                        happyToken = " happytoken ", 
                        sadToken = " sadtoken "){
  happy_emoticons = c("\\:\\)" , "\\(\\:", "\\:-\\)", "\\(-\\:", "\\:D", "\\:-D", "=\\)", "\\(=", "☺", "☻")
  sad_emoticons = c("\\:\\(", "\\:-\\(", "\\)\\:", "\\)-\\:", ":\\[", "\\]:", ":\\{", "\\}:","=\\(", "\\)=", "☹")
  require(plyr)
  require(dplyr)
  require(qdapRegex)
  cleantext = laply(documents, function(documents)
  {
    #documents = gsub("RT", "retweet", documents) # tokenize retweets. Ignore this since tweets aren't retweeets
    documents = rm_url(documents) #tokenize urls
    documents = gsub("@\\w+", usernameToken, documents) #tokenize @
    documents = gsub("\\#", hashToken, documents) #tokenize #. Not necessary for tweets that haven't been classified yet.
    documents = gsub(paste(happy_emoticons, collapse = "|"), happyToken, documents) #tokenize happy emoticons
    documents = gsub(paste(sad_emoticons, collapse = "|"), sadToken, documents) #tokenize sad emoticons
    documents = gsub("[[:punct:]]", "", documents) #remove punctuation
    documents = gsub("[[:digit:]]", "", documents) #remove numbers
    documents = gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents = tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents) # limit character repeats to maximum 2
    documents<-trimws(documents) #remove leading and trailing whitespace
  }, .progress = "text")
  return(cleantext)
}


word.freq <- function(document.vector, sparsity = .99){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords('english'),
                                               removeNumbers = T))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

AFINN_lexicon.frequencies=function(x){
  str_count(x,AFINN_lexicon$word.clean)
}

ndsi.frequencies=function(x){
  str_count(x,freq.all$word[1:1024])
}

classify.sentiment = function(documents, lexicon = AFINN_lexicon){
  sentscorevec = laply(documents, function(documents, lex = lexicon)
  {
    words = unlist(strsplit(documents, " ")) #access words
    #eventually add words to term-document matrix here?
    indices = pmatch(words, lexicon[,1], nomatch = 0, duplicates.ok = TRUE)
    vals = as.numeric(lexicon[indices,2])
    #print(c(words, indices, vals))
    
    #fix negation
    if(length(words)>1){
      for(i in 2:length(words)){
        #print(i)
        #print(words[i-1])
        if(words[i-1] %in% negations & words[i] != words[i-1]){
          #print(words[(i-1):i])
          #print("There's a negation here")
          vals[length(vals)+1] = (-2)*as.numeric(lexicon[pmatch(words[i], lexicon[,1], nomatch = NA),2])
        }
      }   
    }
    
    #return sum
    return(sum(na.omit(vals)))
  }, .progress = "text")
  return(sentscorevec)
}
