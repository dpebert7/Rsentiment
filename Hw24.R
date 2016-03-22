#David Ebert
#21 March 2016
#Data Mining Homework 24

# libraries ----
library(stringr) #library for str_count function
library(e1071) # for naive bayes model
library(ggplot2) #for graphs
library(caret) #for confusionMatrix
library(pROC) #ROC curves
library(randomForest)
library(tm) # for building term frequency matrix from corpus
source("functions.R") #get cleaning function, AFINN_lexicon


# PRELIMINARIES ---- 

  # TARGET DATA: 9.4 million tweets from LA county in 2014
    target = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

  # TRAINING DATA: semisuper tweets from LA county in 2014 
    happy = read.csv(file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", nrows = 110000, header = TRUE, colClasses = 
                     c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

    sad = read.csv(file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", nrows = 50000, header = TRUE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

    # Clean tweets, remove blank tweets, then undersample from happy so that happy has as many tweets as sad does.
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
    happy$polarity = as.factor(1)
    sad$polarity = as.factor(0)
    train = rbind(as.data.frame(happy[,c("text", "polarity")]),sad[,c("text", "polarity")]) 
    colnames(train) = c("clean", "polarity") #note that cleaning already took place, so these column names are appropriate
    dim(train)
    table(train$polarity)


  # VALIDATION DATA: sentiment140
    test = read.csv("testdata.manual.2009.06.14.csv", header = FALSE, colClasses = 
                      c("character", "character", "character", "character", "character", "character"))
    colnames(test) = c("polarity", "not_sure", "created_at", "search_query", "username", "text")
  
    test[test$polarity == 0,]$polarity = 0
    test[test$polarity == 4,]$polarity = 1
    table(test$polarity)
  
    test = test[test$polarity !=2, c("polarity", "text")]
    test$polarity = as.factor(test$polarity)
    test$clean = clean.data(test$text)

    
  # Calculate AFINN scores for train and test using Crawford's method
    
    #AFINN term frequency for test data (5 sec)
    term.freq.test <- t(apply(t(test$clean), 2, AFINN_lexicon.frequencies))
    
    #AFINN.rating for test data
    test$AFINN.rating = as.vector(term.freq.test %*% AFINN_lexicon$score)
    test$pred = sign(test$AFINN.rating)
    table(test$pred)
    table(test$polarity, test$pred) #accuracy is (102+142)/(102+17+50+142) = 78% (excluding neutral tweets)
    
    #term frequency for training data (22 min)
    a = Sys.time()
    term.freq.train <- t(apply(t(train$clean), 2, AFINN_lexicon.frequencies))
    Sys.time()-a
    
    #AFINN.rating for training data
    train$AFINN.rating = as.vector(term.freq.train %*% AFINN_lexicon$score)
    train$pred = sign(train$AFINN.rating)
    table(train$pred)
    table(train$polarity, train$pred) #accuracy is (26640+16671)/(26640+7644+16671+16291) = 64% (excluding neutral tweets)
    
  # Calculate AFINN scores using classify.sentiment
    
    #test data
    test$AFINN.rating2 = classify.sentiment(test$clean)
    test$AFINN.rating2.pred = sign(test$AFINN.rating2)
    table(test$AFINN.rating2.pred)
    table(test$polarity, test$AFINN.rating2.pred) #accuracy is (119+124)/(119+124+39+33) = 77% (excluding neutral tweets)
    
    #train data
    train$AFINN.rating2 = classify.sentiment(train$clean)
    train$AFINN.rating2.pred = sign(train$AFINN.rating2)
    table(train$AFINN.rating2.pred)
    table(train$polarity, train$AFINN.rating2.pred) #accuracy is (22691+21023)/(22691+21023+9783+11464) = 67% (excluding neutral tweets)
    
    
  # Calculate AFINN scores using classify.sentiment2
    
    #test data
    test$AFINN.rating3 = classify.sentiment2(test$clean)
    test$AFINN.rating3.pred = sign(test$AFINN.rating3)
    table(test$AFINN.rating3.pred)
    table(test$polarity, test$AFINN.rating3.pred) #accuracy is (102+142)/(102+142+17+50) = 78% (excluding neutral tweets)
    
    #train data
    train$AFINN.rating3 = classify.sentiment2(train$clean) #(20 minutes)
    train$AFINN.rating3.pred = sign(train$AFINN.rating3)
    table(train$AFINN.rating3.pred)
    table(train$polarity, train$AFINN.rating3.pred) #accuracy is (uncalculated. This is too slow...)
    
  # Calculate WIEBE scores for train and test using classify.sentiment
    #test data
    test$WIEBE.rating = classify.sentiment(test$clean, lexicon = Wiebe_lexicon)
    test$WIEBE.rating.pred = sign(test$WIEBE.rating)
    table(test$WIEBE.rating.pred)
    table(test$polarity, test$WIEBE.rating.pred) #accuracy is (117+78)/(117+78+22+40) = 75% (excluding neutral tweets)
    
    #train data
    train$WIEBE.rating = classify.sentiment(train$clean, lexicon = Wiebe_lexicon)
    train$WIEBE.rating.pred = sign(train$WIEBE.rating)
    table(train$AFINN.rating2.pred)
    table(train$polarity, train$WIEBE.rating.pred) #accuracy is (20817+14342)/(20817+14342+7018+12775) = 64% (excluding neutral tweets)
    
    
  # Calculate EMOLEX scores for train and test using classify.sentiment
    #test data
    test$emolex.rating = classify.sentiment(test$clean, lexicon = emolex)
    test$emolex.rating.pred = sign(test$emolex.rating)
    table(test$emolex.rating.pred)
    table(test$polarity, test$emolex.rating.pred) #accuracy is (73+86)/(73+86+45+56) = 61.1% (excluding neutral tweets)
    
    #train data
    train$emolex.rating = classify.sentiment(train$clean, lexicon = emolex)
    train$emolex.rating.pred = sign(train$emolex.rating)
    table(train$AFINN.rating2.pred)
    table(train$polarity, train$emolex.rating.pred) #accuracy is (16222+16929)/(16222+16929+10310+9263) = 62.8% (excluding neutral tweets)
  
    
    
    
# MODELS INVOLVING AFINN SCORE (using only test data) ----
  a = Sys.time()
  term.freq <- t(apply(t(test$clean), 2, AFINN_lexicon.frequencies))
  Sys.time()-a
  
  dim(term.freq)
  test$AFINN.rating = as.vector(term.freq %*% AFINN_lexicon$score)

  test$pred = sign(test$AFINN.rating)
  table(test$pred)
  table(test$polarity, test$pred)
  #accuracy is (102+142)/(102+17+50+142) = 78% (excluding neutral tweets)

  
  #naive bayes model with AFINN score
  nb.model = naiveBayes(polarity ~ AFINN.rating, data = train)


# B BAG OF WORDS (random forest using term frequencies) ----
  # Make a lexicon from the training data
  
  
  # Apply lexicon 



# C TFIDF (random forest using term frequencies) ----




# D NORMALIZED SENTIMENT DIFFERENCE INDEX----
  print("hello world")

