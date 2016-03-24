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
dim(happy)

sad = read.csv(file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", nrows = 50000, header = TRUE, colClasses = 
                        c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
dim(sad)

#Clean tweets, remove blank tweets, then undersample from happy so that happy has as many tweets as sad does.
happy$text = clean.data(happy$text)
sad$text = clean.data(sad$text)

happy  = happy[happy$text!="",]
sad  = sad[sad$text!="",]

set.seed(127)
index = sample(nrow(happy),nrow(sad))
happy = happy[index,]
dim(happy)
dim(sad)


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


#70% training data ----
set.seed(945)
train = sample(1:nrow(semisuper), 0.7*nrow(semisuper))

#Naive Bayes Model
semisuper$polarity=as.factor(semisuper$polarity)
nb.model=naiveBayes(polarity~AFINN.rating,
                    data=semisuper[train,])

#plot reveals a lot of overlap. This doesn't look like it will be a good classifier
ggplot(semisuper[train,],
       aes(AFINN.rating, fill = as.factor(polarity))) +
  geom_density(alpha = .2)





#test accuracy
pred.polarity=predict(nb.model, newdata=semisuper[-train,])
confusionMatrix(pred.polarity,semisuper[-train,]$polarity)
# 67% accuracy. Pretty good.

# ROC curve
phat=predict(nb.model,
             newdata=semisuper[-train,],
             type="raw")
plot(roc(semisuper$polarity[-train],phat[,2]))
#AUC is 0.7262. Again, good, but also not very challenging

#apply Bayesian classifier to test data from sentiment140... This doesn't seem to be working very well.... ----
# we likely need to train data on the validation data, then cross validate
sentiment140 = read.csv(file = "testdata.manual.2009.06.14.csv", header = FALSE, stringsAsFactors = FALSE)
dim(sentiment140)
colnames(sentiment140) = c("polarity", "ID", "date", "query", "username", "text") # column names
sentiment140$polarity = as.factor(sentiment140$polarity) # set polarity to factor
sentiment140 = as.data.frame(cbind(sentiment140$polarity, sentiment140$text)) #throw away everything exept polarity and text
colnames(sentiment140) = c("polarity", "text")

sentiment140$polarity = c("negative", "neutral", "positive")[sentiment140$polarity]
table(sentiment140$polarity)

sentiment140$clean = clean.data(sentiment140$text)
sentiment140$AFINN.rating = classify.sentiment(sentiment140$clean)
sentiment140$Wiebe = classify.sentiment(sentiment140$clean, lexicon = Wiebe_lexicon)
sentiment140$NB = predict(nb.model, newdata = sentiment140, type = "raw")[,2]

head(sentiment140[c("polarity", "AFINN.rating", "Wiebe", "clean")])
sentiment140$predAFINN = as.factor(sign(sentiment140$AFINN.rating))
sentiment140$predWiebe = as.factor(sign(sentiment140$Wiebe))
sentiment140$predNB = as.factor(sign(round(sentiment140$NB*2)))
sentiment140$predAFINN = c("negative", "neutral", "positive")[sentiment140$predAFINN]
sentiment140$predWiebe = c("negative", "neutral", "positive")[sentiment140$predWiebe]
sentiment140$predNB = c("negative", "neutral", "positive")[sentiment140$predNB]

confusionMatrix(sentiment140$polarity, sentiment140$predAFINN)
#AFINN accuracy is 60%
confusionMatrix(sentiment140$polarity, sentiment140$predWiebe)
#Wiebe accuracy is 53.8%

# Next classifier: Random Forest Bag of Words ----
rf.semisuper=data.frame(polarity=semisuper$polarity,term.freq)

#Random Forest (Takes about 45 min to run with 6000 tweets and AFINN)
rf.model=randomForest(polarity~.,data=rf.semisuper[train,])

#Classification Accuracy
pred.polarity=predict(rf.model,
                       newdata=rf.semisuper[-train,])

confusionMatrix(pred.polarity,semisuper$polarity[-train])


#ROC Curve
phat=predict(rf.model,
             newdata=rf.semisuper[-train,],
             type="prob")

plot(roc(semisuper$polarity[-train],phat[,2]))
#AUC is an impressive 0.78

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
train.data=semisuper[train,]

word.freq.pos = word.freq(train.data$clean[train.data$polarity == 1],
                          sparsity=0.999)
word.freq.neg = word.freq(train.data$clean[train.data$polarity == -1],
                          sparsity=0.999)
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
  str_count(x,freq.all$word[1:1024])
}

#Term Frequencies (Takes about two minutes to run)
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


#Random Forest (Takes about 15 min to run)
rf.model=randomForest(polarity~.,data=rf.semisuper[train,])




pred.sentiment=predict(rf.model,
                       newdata=rf.movie.data[-train,])

confusionMatrix(pred.sentiment,semisuper$sentiment[-train])


#ROC Curve

phat=predict(rf.model,
             newdata=rf.movie.data[-train,],
             type="prob")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))
