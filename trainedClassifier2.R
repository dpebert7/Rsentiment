#Ebert/Rider
#25 February 2016

#Purpose: Import semi-supervised training data to train a classifier


# libraries ----
library(stringr) #library for str_count function
library(e1071) # for naive bayes model
library(ggplot2) #for graphs
library(caret) #for confusionMatrix
library(pROC) #ROC curves
library(randomForest)
library(tm) # for building term frequency matrix from corpus

# setup ----

#import happy and sad tweets for semi-supervision
source("functions.R") #get cleaning function, AFINN_lexicon
happy = read.csv(file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", nrows = 110000, header = TRUE, colClasses = 
                          c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

sad = read.csv(file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", nrows = 50000, header = TRUE, colClasses = 
                        c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

#Clean tweets, remove blank tweets, then undersample from happy so that happy has as many tweets as sad does.
happy$text = clean.data(happy$text)
sad$text = clean.data(sad$text)

happy  = happy[happy$text!="",]
sad  = sad[sad$text!="",]

set.seed(127)
index = sample(nrow(happy),nrow(sad))
happy = happy[index,]
dim(happy)
dim(sad) #both should have 48124 rows


# Initialize polarity of happy and sad tweets
happy$polarity = 1
sad$polarity = -1
semisuper = rbind(as.data.frame(happy[,c("text", "polarity")]),sad[,c("text", "polarity")]) 
colnames(semisuper) = c("clean", "polarity") #note that cleaning already took place, so these column names are appropriate
dim(semisuper)
table(semisuper$polarity)

#Term Frequencies (May take a long time to run! 25 minutes for 100k tweets) ----
a = Sys.time()
term.freq <- t(apply(t(semisuper$clean), 2,
                     AFINN_lexicon.frequencies))
Sys.time()-a
dim(term.freq)
semisuper$AFINN.rating = as.vector(term.freq %*% AFINN_lexicon$score)

#Example ----
semisuper[1000,]
AFINN_lexicon[AFINN_lexicon$word == "enjoy",]
AFINN_lexicon$word[844]
term.freq[1000,844]  # the word "enjoy" occurs one time in the 20th tweet. Its corresponding AFINN score is +2

table(semisuper$AFINN.rating)




# building a term frequency matrix from the corpus ----
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
head(word.freq(semisuper$clean), 100)

# Normalized Sentiment Difference Index (NDSI) ----
#Word Frequencies for Positive and Negative Reviews in Training Data
word.freq.pos = word.freq(semisuper$clean[semisuper$polarity == 1],
                          sparsity=0.9999) #terms must occur in at least 1 out of 1000
word.freq.neg = word.freq(semisuper$clean[semisuper$polarity == -1],
                          sparsity=0.9999)
dim(word.freq.pos)
dim(word.freq.neg)
word.freq.pos[1:20,]
word.freq.neg[1:20,]


#Merge by word
freq.all = merge(word.freq.neg, word.freq.pos, by = 'word', all = T)

dim(word.freq.pos)
dim(word.freq.neg)
dim(freq.all)

word.freq.pos[1:20,]
word.freq.neg[1:20,]
freq.all[1:20,]

#Set NA's to 0
freq.all$freq.x[is.na(freq.all$freq.x)] = 0
freq.all$freq.y[is.na(freq.all$freq.y)] = 0

#Differences between Positive and Negative Frequencies
freq.all$diff = abs(freq.all$freq.x - freq.all$freq.y)
head(freq.all[order(-freq.all$diff), ])

#Smoothing term
alpha <- 2^7

#NDSI
freq.all$ndsi = abs(freq.all$freq.x -
                      freq.all$freq.y)/(freq.all$freq.x +
                                          freq.all$freq.y +
                                          2 * alpha)
#Sorting by NDSI
freq.all = freq.all[order(-freq.all$ndsi), ]
head(freq.all, 100)

#Convert word to a string
head(freq.all$word)
freq.all$word = as.character(freq.all$word)
head(freq.all$word)


# Term Frequencies and tfidf with NDSI ----

#AFINN Frequency Function (now used with ndsi lexicon)
freq.all$word = as.character(freq.all$word)
library(stringr)
ndsi.frequencies=function(x){
  str_count(x,freq.all$word[1:1024]) #1024 words with highest NDSI score
}

#Term Frequencies (Takes about 25 minutes to run with 100k tweets)
term.freq <- t(apply(t(semisuper$clean), 2,
                     ndsi.frequencies))

inv.doc.freq=log(nrow(semisuper)/colSums(sign(term.freq)))
range(inv.doc.freq)

inv.doc.freq[is.infinite(inv.doc.freq)]=0
range(inv.doc.freq)

tf.idf = term.freq %*% diag(inv.doc.freq)



# Random Forest Using NDSI tf.idf ----
semisuper$polarity=as.factor(semisuper$polarity)
rf.semisuper=data.frame(polarity=semisuper$polarity,tf.idf)

#Random Forest (Takes hours to run)
a = Sys.time()
rf.model=randomForest(polarity~.,data=rf.semisuper)
Sys.time()-a

pred.sentiment=predict(rf.model,
                       newdata=rf.movie.data[-train,])

confusionMatrix(pred.sentiment,semisuper$sentiment[-train])



#ROC Curve

phat=predict(rf.model,
             newdata=rf.movie.data[-train,],
             type="prob")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))
