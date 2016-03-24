##################
#MATH 5366 NOTES #
#TEXT MINING IN R#
##################

#################################################################################################
#This script explores text mining using data from the Kaggle data mining competition,           # 
#"Bag of Words Meets Bag of Popcorn".  Only about 10% of the training and test data are used    #
#in this script to reduce computation time.  The full competition and data can be found here:   #
#                                                                                               #
#https://www.kaggle.com/c/word2vec-nlp-tutorial                                                 #
#                                                                                               #
#Thanks to John Koo for writing a tutorial related to this project, which provided most of the  #
#material for this script.                                                                      #
#################################################################################################




#Obtaining Movie Review Data
movie.data = read.delim("http://faculty.tarleton.edu/crawford/documents/Math5364/MovieReviews.txt",
                        header = T, 
                        quote = '', 
                        stringsAsFactors = F)






########################################
#Text Cleaning with Regular Expressions#
########################################

#gsub Examples

gsub("ABC","123", "ABCDEFGHIJKLMNOP")



#A dot represents any single character

gsub("D.F","456","ABCDEFGHIJKLMNOP")

gsub("a.c","DOG","11111111abc22222222azc3333333ahc")



#An * indicates that the previous character can appear zero or more times.

gsub("ab*c","DOG","1111111abbbbbbbbbc2222222abc333333333333ac")

gsub("A.*C","DOG","wwwwwwwwwwwAqwertyuiopCwwwwww")

gsub("<.*>","","<h1>This is the Heading of a Wepage</h1>")



#A ? after an * causes the replacement to be as small as possible.

gsub("<.*?>","","<h1>This is the Heading of a Wepage</h1>")



#Removing HTML Tags from our Movie Review Data
movie.data$review.clean = gsub('<.*?>', '', movie.data$review)

#Removing Punctuation
movie.data$review.clean = gsub('[[:punct:]]','',movie.data$review.clean)

#Converting to Lower Case
movie.data$review.clean = tolower(movie.data$review.clean)


#More info on regular expressions:
#https://en.wikipedia.org/wiki/Regular_expression



################
#The AFINN List#
################

# Read in the AFINN list
afinn <- read.delim('http://faculty.tarleton.edu/crawford/documents/Math5364/AFINN.txt',
                    header = F, quote = '', stringsAsFactors = F)

names(afinn) <- c('word','score')

#Replacing Hyphens with Spaces
afinn$word.clean <- gsub('-',' ' , afinn$word)

#Removing punctuation
afinn$word.clean <- gsub("[[:punct:]]", '', afinn$word.clean)


#Histogram of Word Scores
hist(afinn$score)




##############################################
#Term Frequencies using the str_count Command#
##############################################

library(stringr)

#Examples

fruit.names=c("happy","banana","cantaloupe","date")

str_count("i want to eat an #ohhappyday or a banana, preferably an apple",fruit.names)



#AFINN Frequency Function
afinn.frequencies=function(x){
  str_count(x,afinn$word.clean)
}


#Term Frequencies (Takes about two minutes to run)
term.freq <- t(apply(t(movie.data$review.clean), 2,
                     afinn.frequencies))

dim(term.freq)


#Example
movie.data$review.clean[50]
afinn$word[1438]

term.freq[50,1438]




###############
#AFINN Ratings#
###############

movie.data$afinn.rating = as.vector(term.freq %*% afinn$score)


#Probability Densities for AFINN Ratings

library(ggplot2)
ggplot(movie.data,
       aes(afinn.rating, fill = as.factor(sentiment))) +
  geom_density(alpha = .2)


#########################
#Selecting Training Data#
#########################

source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

set.seed(256)
train=trainsample(movie.data,0.9)



###################
#Naive Bayes Model#
###################

install.packages("e1071")
library(e1071)

movie.data$sentiment=as.factor(movie.data$sentiment)

nb.model=naiveBayes(sentiment~afinn.rating,
                    data=movie.data[train,])


#Classification Accuracy
source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

pred.sentiment=predict(nb.model,
                       newdata=movie.data[-train,])

confmatrix(pred.sentiment,movie.data$sentiment[-train])


#ROC Curve
phat=predict(nb.model,
             newdata=movie.data[-train,],
             type="raw")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))





#Memory Cleanup
remove(nb.model,phat)


#List Objects in Order of Memory Usage in Bytes
sort( sapply(ls(),function(x){object.size(get(x))}))


#Total Memory Usage in Bytes
sum(sapply(ls(),function(x){object.size(get(x))}))




##################################
#Bag of Words Random Forest Model#
##################################

install.packages("randomForest")
library(randomForest)

rf.movie.data=data.frame(sentiment=movie.data$sentiment,term.freq)


#Random Forest (Takes about 15 min to run)
rf.model=randomForest(sentiment~.,data=rf.movie.data[train,])



#Classification Accuracy
source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

pred.sentiment=predict(rf.model,
                       newdata=rf.movie.data[-train,])

confmatrix(pred.sentiment,movie.data$sentiment[-train])


#ROC Curve

phat=predict(rf.model,
             newdata=rf.movie.data[-train,],
             type="prob")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))


##############################
#Inverse Document Frequencies#
##############################

View(term.freq)



inv.doc.freq=log(nrow(movie.data)/colSums(sign(term.freq)))
range(inv.doc.freq)

inv.doc.freq[is.infinite(inv.doc.freq)]=0
range(inv.doc.freq)


tf.idf = term.freq %*% diag(inv.doc.freq)


#Example
movie.data$review.clean[50]
afinn$word[1438]

term.freq[50,1438]
inv.doc.freq[1438]

term.freq[50,1438]*inv.doc.freq[1438]

tf.idf[50,1438]



############################
#Random Forest Using tf.idf#
############################

rf.movie.data=data.frame(sentiment=movie.data$sentiment,tf.idf)


#Random Forest (Takes about 15 min to run)
rf.model=randomForest(sentiment~.,data=rf.movie.data[train,])



#Classification Accuracy
source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

pred.sentiment=predict(rf.model,
                       newdata=rf.movie.data[-train,])

confmatrix(pred.sentiment,movie.data$sentiment[-train])


#ROC Curve

phat=predict(rf.model,
             newdata=rf.movie.data[-train,],
             type="prob")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))



##################################################
#Building a Term Frequency Matrix from the Corpus#
##################################################

library(tm)

corpus=Corpus(VectorSource(movie.data$review.clean))

tf = DocumentTermMatrix(corpus, control = list(stopwords = stopwords('english'),
                                               removeNumbers = T))



#Only include words that occur in at least 0.1% of reviews.
tf = removeSparseTerms(tf, .999)

#Convert to a matrix
tf = as.matrix(tf)

View(tf)
dim(tf)


#Total Word Frequencies
word.freq=colSums(tf)

head(word.freq)
head(names(word.freq))


#2 Column Format
word.freq=data.frame(word=names(word.freq),freq=word.freq)

head(word.freq)

#Remove extra row names
rownames(word.freq)=NULL

head(word.freq)


#Sorting by Frequency in Descending Order
head(word.freq[order(-word.freq$freq),])




#Writing a Word Frequency Function
word.freq <- function(document.vector, sparsity = .999)
{
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


head(word.freq(movie.data$review.clean))



##############################################
#Normalized Sentiment Difference Index (NDSI)#
##############################################


#Word Frequencies for Positive and Negative Reviews in Training Data
train.data=movie.data[train,]


word.freq.pos = word.freq(train.data$review.clean[train.data$sentiment == 1],
                          sparsity=0.99)

word.freq.neg = word.freq(train.data$review.clean[train.data$sentiment[train] == 0],
                          sparsity=0.99)

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


#Remove NA's
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
head(freq.all)


#Convert word to a string
head(freq.all$word)
freq.all$word = as.character(freq.all$word)
head(freq.all$word)


######################################
#Term Frequencies and tfidf with NDSI#
######################################



#AFINN Frequency Function
library(stringr)
ndsi.frequencies=function(x){
  str_count(x,freq.all$word[1:1024])
}


#Term Frequencies (Takes about two minutes to run)
term.freq <- t(apply(t(movie.data$review.clean), 2,
                     ndsi.frequencies))




inv.doc.freq=log(nrow(movie.data)/colSums(sign(term.freq)))
range(inv.doc.freq)

inv.doc.freq[is.infinite(inv.doc.freq)]=0
range(inv.doc.freq)


tf.idf = term.freq %*% diag(inv.doc.freq)


#################################
#Random Forest Using NDSI tf.idf#
#################################

movie.data$sentiment=as.factor(movie.data$sentiment)
rf.movie.data=data.frame(sentiment=movie.data$sentiment,tf.idf)


#Random Forest (Takes about 15 min to run)
rf.model=randomForest(sentiment~.,data=rf.movie.data[train,])



#Classification Accuracy
source("http://faculty.tarleton.edu/crawford/documents/Math5364/MiscRFunctions.txt")

pred.sentiment=predict(rf.model,
                       newdata=rf.movie.data[-train,])

confmatrix(pred.sentiment,movie.data$sentiment[-train])


#ROC Curve

phat=predict(rf.model,
             newdata=rf.movie.data[-train,],
             type="prob")

library(pROC)
plot(roc(movie.data$sentiment[-train],phat[,2]))




#########
#TwitteR#
#########

install.packages("twitteR")
library(twitteR)



#Trying to search for Verizon tweets.
verizon=searchTwitteR("Verizon",n=100,lang="en")



#We need to setup a twitteR app.
#https://apps.twitter.com/

#WARNING:  Don't use the keys and secrets below.  Create your own
#twitter app at https://apps.twitter.com/, and use the keys and secrets
#provided for your account.

setup_twitter_oauth(consumer_key="rf9Bym8zIRbxzwI8vEQTmLu5S",
                    consumer_secret ="BuN2d38QHcoH5PgxdUwIp6jps7poyQWxRxE2z2DbYf3N8oSISs",
                    access_token="4913426354-dWGlYAlEgr2xZVU0Z85P8xDLKUI4Guxq4RR9C2P",
                    access_secret="UTNWewqwm1kWc6OIEgscaJiKOJQRIx0wtHd8aRxqN5UFC")


#Searching Again.
verizon=searchTwitteR("Verizon",n=100,lang="en")


#Converting Search Results into a Data Frame
verizon=twListToDF(verizon)


#Cleaning the tweets
verizon$text.clean = gsub('<.*?>', '', verizon$text)
verizon$text.clean = gsub('[[:punct:]]','',verizon$text.clean)
verizon$text.clean = tolower(verizon$text.clean)




imdb=read.delim(file="C:\\Users\\jcrawford\\Downloads\\sentiment labelled sentences\\sentiment labelled sentences\\imdb_labelled.txt",
                header=F,
                sep="\n",
                quote='',
                stringsAsFactors=F)


amazon=read.table(file="C:\\Users\\jcrawford\\Downloads\\sentiment labelled sentences\\sentiment labelled sentences\\amazon_cells_labelled.txt",
                  header=F,
                  sep="\n",
                  quote='',
                  stringsAsFactors=F)

yelp=read.delim(file="C:\\Users\\jcrawford\\Downloads\\sentiment labelled sentences\\sentiment labelled sentences\\yelp_labelled.txt",
                header=F,
                sep="\n",
                quote='',
                stringsAsFactors=F)