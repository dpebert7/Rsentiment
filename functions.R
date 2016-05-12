#Ebert/Rider
#Updated 23 March 2016


# Storage directory for loading R objects
storage.directory = "~/Desktop/Huang Research/Rsentiment/"

# Load libraries for other scripts
library(stringr) #library for str_count function
library(ggplot2) #for graphs
library(caret) #for confusionMatrix

#library(tm) # for building term frequency matrix from corpus
#library(cldr) # for detecting tweet language
#library(beepr) # for beeping, just use beepr::beep(3)


#library(e1071) # for naive bayes model
#library(pROC) #ROC curves
#library(randomForest)
#library(rpart)


#AFINN_lexicon
  AFINN_lexicon = read.delim(file = "Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE, header = F, quote = '')
  names(AFINN_lexicon) <- c('word','score')
  AFINN_lexicon = rbind(AFINN_lexicon, c("happytoken", 5), c("sadtoken", -5))
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



negations = c("no", "not","none","nobody","nothing","neither","never","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")



classify.sentiment = function(documents, lexicon = AFINN_lexicon){
  require(plyr)
  require(dplyr)
  sentscorevec = laply(documents, function(documents, lex = lexicon)
  {
    words = unlist(strsplit(documents, " ")) #access words
    #eventually add words to term-document matrix here?
    indices = match(words, lexicon[,1], nomatch = 0)
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



bin.maker = function(binsize, max){
  nbins = ceiling(max/binsize)
  result = as.list(1:nbins)
  for(i in 1:(nbins-1)){
    result[[i]]=((binsize*(i-1)+1):(binsize*(i)))
  }
  result[[nbins]] = ((binsize*(nbins-1)+1):max)
  return(result)
}



classify.polarity.machine = function(documents, chunk.size = 1000, model = rf.model){
  require(plyr)
  require(dplyr)
  require(randomForest)
  load(file = paste(storage.directory, "rf.model.RData", sep = ""))
  load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
  ndsi.lexicon = freq.all[1:1024,]
  
  if(length(documents)<chunk.size){stop("chunk.size must be less than length(documents). Also, length(documents) must be at least 2.")}
  
  column.names = paste("X", 1:1024, sep = "")
  result = NULL
  
  chunks = bin.maker(chunk.size, length(documents))
  
  for(i in 1:length(chunks)){
    print(paste((i-1)*chunk.size, "out of", length(documents), "rows analyzed:", 
                round((i-1)*100*chunk.size/length(documents), digits = 1), "percent complete"))
    
    term.freq <- t(apply(t(documents[chunks[[i]]]), 2, #MAY TAKE TIME!
                         ndsi.frequencies))
    
    colnames(term.freq) = column.names
    pred.sentiment = predict(model, newdata = term.freq, type = "prob")
    result = c(result, pred.sentiment[,1])
  }
  
  return(result)
}

# a = Sys.time()
#sent140$rf.polarity = classify.polarity.machine(sent140$text, chunk.size = 30)
# Sys.time()-a


#                                  Starting time: 1.539257 mins
#         Moving column_names to vector in setup: 1.49686 mins
#     Restrict ndsi.lexicon to 2^10 = 1024 terms: 1.488172 mins
#                   Fix the dang colames forever: 1.507144 mins
#               Put colnames back in their place: 1.446915 mins
#                    Doing term.freq all at once: 3.961535 SECS  <- Do this. Maybe 1000 rows at a time? Actually 5000




#Optimize cutoff value for classifier scores
optimize.cutoff = function(score.vec, polarity.vec, min = -10, max = 10, step = 1){
  require(caret)
  require(pROC)
  cutoffs = seq(min, max, by = step)
  accuracy.vec=1:length(cutoffs)
  for(i in 1:length(cutoffs)){
    accuracy.vec[i] = confusionMatrix(as.numeric(score.vec>=(cutoffs[i])),polarity.vec)$overall[1]
  }
  print(plot(roc(polarity.vec, score.vec)))
  print(accuracy.vec)
  optimal.cutoff = cutoffs[which.max(accuracy.vec)]
  print(confusionMatrix(as.numeric(score.vec>=(optimal.cutoff)),polarity.vec)$overall[1])
  return(optimal.cutoff)
}





# Additional functions
# confmatrix=function(y,predy){
#   matrix=table(y,predy)
#   accuracy=sum(diag(matrix))/sum(matrix)
#   return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
# }



# rand.which.max=function(x){
#   index=((1:length(x))[x==(max(x))])
#   return(sample(c(index,index),1))
# }



# old.classify.polarity.machine = function(documents, model = rf.model){
#   require(plyr)
#   require(dplyr)
#   require(randomForest)
#   load(file = paste(storage.directory, "rf.model.RData", sep = ""))
#   load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
#   ndsi.lexicon = freq.all[1:1024,]
#   column_names = paste("X", 1:1024, sep = "")
#   print("setup complete")
#   
#   sentpredvec = laply(documents, function(documents, mod = model)
#   {
#     term.freq <- t(apply(t(documents), 2,    #MAY TAKE TIME!
#                          ndsi.frequencies))
#     
#     colnames(term.freq) = column_names
#     pred.sentiment = predict(model, newdata = term.freq, type = "prob")
#     
#     return(pred.sentiment[1])
#   }, .progress = "text")
#   return(sentpredvec)
# }
