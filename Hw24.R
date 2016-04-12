#David Ebert
#21 March 2016
#Data Mining Homework 24

# libraries, functions, and directory, cleaning function , AFINN_lexicon, etc----
source("functions.R") #get cleaning function, AFINN_lexicon


# PRELIMINARIES ---- 

  # TARGET DATA: 9.4 million tweets from LA county in 2014 -- don't worry about this here. It is dealt with in main.R

  # EMOTICON (emoticonING) DATA: semisuper tweets from LA county in 2014 
    load("~/Desktop/Huang Research/Rsentiment/emoticon.RData") # load emoticon/emoticon into memory as emoticon

  # TEST/VALIDATION DATA: sentiment140
    sent140 = read.csv("testdata.manual.2009.06.14.csv", header = FALSE, colClasses = 
                      c("character", "character", "character", "character", "character", "character"))
    colnames(sent140) = c("polarity", "not_sure", "created_at", "search_query", "username", "text")
  
    sent140[sent140$polarity == 0,]$polarity = 0
    sent140[sent140$polarity == 4,]$polarity = 1
    sent140(sent140$polarity)
  
    sent140 = sent140[sent140$polarity !=2, c("polarity", "text")]
    sent140$polarity = as.factor(sent140$polarity)
    sent140$clean = clean.tweets(sent140$text)

    
  # Calculate AFINN scores for emoticon and sent140 using Crawford's method
    #This has been removed. It was nearly as effective as classify.sentiment, but slower.
    
  # Calculate AFINN scores using classify.sentiment (which includes a negation stopper)
    #sent140 data
    sent140$AFINN.rating = classify.sentiment(sent140$clean)
    sent140$AFINN.rating.pred = sign(sent140$AFINN.rating)
    table(sent140$AFINN.rating.pred)
    ans = table(sent140$polarity, sent140$AFINN.rating.pred)
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 78.05% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 76.78%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 70.87%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 73.71%
      
    #emoticon data
    AFINN_lexicon = AFINN_lexicon[AFINN_lexicon$word != "sadtoken" & AFINN_lexicon$word != "happytoken",]
    #temporarily remove happytoken and sadtoken from dictionary, which unfairly yields accuracy of 97%
    
    emoticon$AFINN.rating = classify.sentiment(emoticon$clean)
    emoticon$AFINN.rating.pred = sign(emoticon$AFINN.rating)
    table(emoticon$AFINN.rating.pred)
    ans = table(emoticon$polarity, emoticon$AFINN.rating.pred)
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 68.09% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 67.46%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 48.47%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 56.41%

  # Calculate AFINN scores using classify.sentiment2 (similar to Crawford's method)
      # Removed. It didn't work great and was computationally expensive.
    
  # Calculate OpinionFinder (formerly known as WIEBE) scores for emoticon and sent140 using classify.sentiment
    #sent140 data
    sent140$OpinionFinder.rating = classify.sentiment(sent140$clean, lexicon = OpinionFinder)
    sent140$OpinionFinder.rating.pred = sign(sent140$OpinionFinder.rating)
    table(sent140$OpinionFinder.rating.pred)
    ans = table(sent140$polarity, sent140$OpinionFinder.rating.pred)
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 75.87% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 74.52%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 64.28%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 69.02%

    
    #emoticon data
    emoticon$OpinionFinder.rating = classify.sentiment(emoticon$clean, lexicon = OpinionFinder)
    emoticon$OpinionFinder.rating.pred = sign(emoticon$OpinionFinder.rating)
    table(emoticon$AFINN.rating.pred)
    ans = table(emoticon$polarity, emoticon$OpinionFinder.rating.pred) 
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 65.13% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 63.31%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 44.76%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 52.44%


  # Calculate NRC (formerly known as EmoLex) scores for emoticon and sent140 using classify.sentiment
    #sent140 data
    sent140$NRC.rating = classify.sentiment(sent140$clean, lexicon = NRC)
    sent140$NRC.rating.pred = sign(sent140$NRC.rating)
    table(sent140$NRC.rating.pred)
    ans = table(sent140$polarity, sent140$NRC.rating.pred) 
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 61.96% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 60.83%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 47.80%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 53.53%
    
      
    #emoticon data
    emoticon$NRC.rating = classify.sentiment(emoticon$clean, lexicon = NRC)
    emoticon$NRC.rating.pred = sign(emoticon$NRC.rating)
    table(emoticon$AFINN.rating.pred)
    ans = table(emoticon$polarity, emoticon$NRC.rating.pred) 
    ans 
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 65.57% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 66.03%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 38.28%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 48.46%  (Note that this is low but it is still an improvement over when pic]: was around...)
  
  # Calculate ANEW scores for emoticon and sent140 using classify.sentiment
    #sent140 data
    sent140$ANEW.rating = classify.sentiment(sent140$clean, lexicon = ANEW)
    sent140$ANEW.rating.pred = sign(sent140$ANEW.rating)
    table(sent140$ANEW.rating.pred)
    ans = table(sent140$polarity, sent140$ANEW.rating.pred) 
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 74.07% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 81.75%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 61.53%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 70.21%

    #emoticon data
    emoticon$ANEW.rating = classify.sentiment(emoticon$clean, lexicon = ANEW)
    emoticon$ANEW.rating.pred = sign(emoticon$ANEW.rating)
    table(emoticon$AFINN.rating.pred)
    ans = table(emoticon$polarity, emoticon$ANEW.rating.pred) 
    ans
      accuracy = (ans["0","-1"]+ans["1","1"])/(sum(ans[,"-1"])+sum(ans[,"1"])) 
      accuracy #is 64.79% (excluding neutral tweets)
      precision = ans["1","1"]/sum(ans[,"1"])
      precision # precision is 66.42%
      recall = ans["1","1"]/sum(ans["1",])
      recall # recall is 51.09%
      f1 = (2*precision*recall)/(precision + recall)
      f1 #is 57.71%


# MODELS INVOLVING AFINN SCORE (using only sent140 data) ----
    #I think this is junk now. But just in case...
                          a = Sys.time()
                          term.freq <- t(apply(t(sent140$clean), 2, AFINN_lexicon.frequencies))
                          Sys.time()-a
                          
                          dim(term.freq)
                          sent140$AFINN.rating = as.vector(term.freq %*% AFINN_lexicon$score)
                        
                          sent140$pred = sign(sent140$AFINN.rating)
                          table(sent140$pred)
                          table(sent140$polarity, sent140$pred)
                          #accuracy is (102+142)/(102+17+50+142) = 78% (excluding neutral tweets)
                        
                          
                          #naive bayes model with AFINN score
                          nb.model = naiveBayes(polarity ~ AFINN.rating, data = emoticon)

# B BAG OF WORDS (random forest using term frequencies) ----
  
  # Lexicon from the emoticoning (emoticon) data
    load(paste(storage.directory, "emoticon.RData", sep = "")) # load emoticon/emoticon into memory as emoticon
    table(emoticon$polarity)

    load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
    head(freq.all)
    
    #T load tf.idf created by emoticon.R
    load(paste(storage.directory,"tf.idf.RData", sep = "")) # load tf.idf lexicon into memory as tf.idf
    
    
    
    # load emoticon.tf.idf created by emoticon.R
    load(paste(storage.directory,"emoticon.tf.idf.RData", sep = "")) # load emoticon.tf.idf lexicon into memory as tf.idf
    
    happy_indices = 1:(nrow(emoticon.tf.idf)/2)
    sad_indices = (nrow(emoticon.tf.idf)/2+1):(nrow(emoticon.tf.idf))
    
    # Training and test data indices
    nTrain = 20000
    nTest = 20000
    
    train = c(sample(happy_indices,nTrain/2), sample(sad_indices,nTrain/2))
    test = c(sample(happy_indices,nTest/2), sample(sad_indices,nTest/2))  



  # Build a model using emoticon data and new dictionary
    # rpart tree (<10 minutes to run, even for lots of data) 
      a = Sys.time()
      tree.model = rpart(polarity~., data = emoticon.tf.idf[train,])
      Sys.time()-a
    
    #Train data results
      pred.sentiment = predict(tree.model, newdata = emoticon.tf.idf[train,], type = "class")
      confusionMatrix(pred.sentiment,emoticon.tf.idf[train,"polarity"])
      phat=predict(tree.model, newdata = emoticon.tf.idf[train,], type = "prob")
      plot(roc(emoticon.tf.idf[train,"polarity"],phat[,2]))
    
    #Test data results
      pred.sentiment = predict(tree.model, newdata = emoticon.tf.idf[test,], type = "class")
      confusionMatrix(pred.sentiment,emoticon.tf.idf[test,"polarity"])
      phat=predict(tree.model, newdata = emoticon.tf.idf[test,], type = "prob")
      plot(roc(emoticon.tf.idf[test,"polarity"],phat[,2]))
    
    # SVM
      a = Sys.time()
      svm.model=svm(polarity~.,data = emoticon.tf.idf[train,])
      Sys.time()-a
    
    #Train data results
      pred.sentiment = predict(svm.model, newdata = emoticon.tf.idf[train,], type = "class")
      confusionMatrix(pred.sentiment,emoticon.tf.idf[train,"polarity"])
      phat=predict(svm.model, newdata = emoticon.tf.idf[train,], type = "prob")
      plot(roc(emoticon.tf.idf[train,"polarity"],phat[,2]))  
    
    #Test data results
      pred.sentiment = predict(svm.model, newdata = emoticon.tf.idf[test,], type = "class")
      confusionMatrix(pred.sentiment,emoticon.tf.idf[test,"polarity"])
      phat=predict(svm.model, newdata = emoticon.tf.idf[test,], type = "prob")
      plot(roc(emoticon.tf.idf[test,"polarity"],phat[,2]))
    
    #Save model
      save(svm.model, file = paste(storage.directory, "svm.model.RData", sep = ""))
      
    
    # Random Forest (SLOW: Takes MANY HOURS to run)
      a = Sys.time()
      rf.model=randomForest(polarity~.,data = emoticon.tf.idf[train,])
      Sys.time()-a
    
      #Train data results
        pred.sentiment = predict(rf.model, newdata = emoticon.tf.idf[train,], type = "class")
        confusionMatrix(pred.sentiment,emoticon.tf.idf[train,"polarity"])
        phat=predict(rf.model, newdata = emoticon.tf.idf[train,], type = "prob")
        plot(roc(emoticon.tf.idf[train,"polarity"],phat[,2]))  
      
      #Test data results
        pred.sentiment = predict(rf.model, newdata = emoticon.tf.idf[test,], type = "class")
        confusionMatrix(pred.sentiment,emoticon.tf.idf[test,"polarity"])
        phat=predict(rf.model, newdata = emoticon.tf.idf[test,], type = "prob")
        plot(roc(emoticon.tf.idf[test,"polarity"],phat[,2]))
        
      #Save model
        save(rf.model, file = paste(storage.directory, "rf.model.RData", sep = ""))
      
      #sent140 results

    
    
    
  # Apply lexicon and random forest to test (sent140) data for outside validation
    
    # Import sent140
    load(file = paste(storage.directory, "sent140.RData", sep = ""))
    
    # Import rf.model
    load(file = paste(storage.directory, "rf.model.RData", sep = ""))
    
    # Import svm.model
    load(file = paste(storage.directory, "svm.model.RData", sep = ""))
    
    classify.polarity.machine = function(data, model, give.accuracy = FALSE, give.roc.curve = FALSE){
      source("functions.R")
      
      
      load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
      ndsi.lexicon = freq.all[freq.all$ndsi>0.05,]
      
      term.freq <- t(apply(t(data[,"clean"]), 2,    #MAY TAKE TIME!
                           ndsi.frequencies))
      
      load(file = paste(storage.directory, "inv.doc.freq.RData", sep = ""))
      
      bigmatrixforclassification = as.data.frame(term.freq %*% diag(inv.doc.freq))
      colnames(bigmatrixforclassification) = paste("X", 1:1024, sep = "") #hacky fix for column names
      pred.sentiment = predict(model, newdata = bigmatrixforclassification)
      
      if(give.accuracy != FALSE){
        require(caret)
        result = confusionMatrix(pred.sentiment, data$polarity) # Accuracy is a respectable 68%
        print(result$table)
        print(result$overall[1])
        print(result$byClass[1])
        print(result$byClass[2])
        #give F1 measure too
      }
      
      if(give.roc.curve != FALSE){
        require(pROC)
        phat=predict(model,
                     newdata = bigmatrixforclassification,
                     type = "prob")
        result = plot(roc(data$polarity, phat[,2]))
        print(result)
      }
      
      print("hello world!")
      pred.sentiment
    }
    
    
    
    

    
    # Import freq.all lexicon and limit to 1276 words with ndsi above 0.05
    load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
    ndsi.lexicon = freq.all[freq.all$ndsi>0.05,]
    dim(ndsi.lexicon)
    
    #Import inv.doc.freq
    load(file = paste(storage.directory, "inv.doc.freq.RData", sep = ""))
    
    # Apply freq.all to cleaned sent140 data (2 secs for sent140's 359 rows, but otherwise costly)
    term.freq <- t(apply(t(sent140[,"clean"]), 2,    #MAY TAKE TIME!
                         ndsi.frequencies))
    
    # Matrix Multiply by diag(inv.doc.freq) from emoticoning data
    bigmatrixforclassification = term.freq %*% diag(inv.doc.freq) #term.freq from before, diag(inv.doc.freq) from emoticon data

    # Prediction
    bigmatrixforclassification = as.data.frame(bigmatrixforclassification)
    colnames(bigmatrixforclassification) = paste("X", 1:1024, sep = "") #hacky fix for column names
    pred.sentiment=predict(rf.model, newdata = bigmatrixforclassification)
    
    #Accuracy
    confusionMatrix(pred.sentiment,sent140$polarity) # Accuracy is a respectable 68%
    
    # ROC curve
    phat=predict(rf.model,
                 newdata = bigmatrixforclassification,
                 type = "prob")
    plot(roc(sent140$polarity,phat[,2])) #Even more respectable 76.65%
    
    
    
    
    
    
  
  
# C TFIDF (random forest using term frequencies) ----

  
# D NORMALIZED SENTIMENT DIFFERENCE INDEX----
  print("hello world")
  
