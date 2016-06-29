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
    
    sent140 = sent140[sent140$polarity !=2, c("polarity", "text")]
    sent140$polarity = as.factor(sent140$polarity)
    sent140$clean = clean.tweets(sent140$text)

  # Calculate AFINN scores using classify.sentiment (which includes a negation stopper and uses MATCH instead of PMATCH)
    #sent140 data
    sent140$AFINN.score = classify.sentiment(sent140$clean)
    optimize.cutoff(score.vec = sent140$AFINN.score, polarity.vec = sent140$polarity)

    #emoticon data
    AFINN_lexicon = AFINN_lexicon[AFINN_lexicon$word != "sadtoken" & AFINN_lexicon$word != "happytoken",]
    emoticon$AFINN.score = classify.sentiment(emoticon$clean)
    optimize.cutoff(score.vec = emoticon$AFINN.score, polarity.vec = emoticon$polarity)


  # Calculate OpinionFinder (formerly known as WIEBE) scores for emoticon and sent140 using classify.sentiment
    #sent140 data
    sent140$OpinionFinder.score = classify.sentiment(sent140$clean, lexicon = OpinionFinder)
    optimize.cutoff(score.vec = sent140$OpinionFinder.score, polarity.vec = sent140$polarity)
    
    #emoticon data
    emoticon$OpinionFinder.score = classify.sentiment(emoticon$clean, lexicon = OpinionFinder)
    optimize.cutoff(score.vec = emoticon$OpinionFinder.score, polarity.vec = emoticon$polarity)
    
    
  # Calculate NRC (formerly known as EmoLex) scores for emoticon and sent140 using classify.sentiment
    #sent140 data
    sent140$NRC.score = classify.sentiment(sent140$clean, lexicon = NRC)
    optimize.cutoff(score.vec = sent140$NRC.score, polarity.vec = sent140$polarity)
    
    #emoticon data
    emoticon$NRC.score = classify.sentiment(emoticon$clean, lexicon = NRC)
    optimize.cutoff(score.vec = emoticon$NRC.score, polarity.vec = emoticon$polarity)
    

  # Calculate ANEW scores for emoticon and sent140 using classify.sentiment
  # Note that ANEW scores range from 1 to 9, so the cutoff needs to be calculated more carefully
    #sent140 data
    sent140$ANEW.score = classify.sentiment(sent140$clean, lexicon = ANEW)
    optimize.cutoff(score.vec = sent140$ANEW.score, polarity.vec = sent140$polarity, min = 1, max = 4, step = 0.005)
    
    #emoticon data
    emoticon$ANEW.score = classify.sentiment(emoticon$clean, lexicon = ANEW)
    optimize.cutoff(score.vec = emoticon$ANEW.score, polarity.vec = emoticon$polarity, min = 1, max = 4, step = 0.05)
   
    
  # Calculate rf.polarity scores for emoticon and sent140 using classify.sentiment
  # Note that rf.polarity scores range from 0 to 1, so the cutoff needs to be calculated more carefully
    #sent140 data
    sent140$rf.score = classify.polarity.machine(sent140$clean, chunk.size = 100, model = rf.model)
    optimize.cutoff(score.vec = sent140$rf.score, polarity.vec = sent140$polarity, min = 0.4, max = 0.8, step = 0.001)
    
    #emoticon data
    emoticon$rf.score = classify.polarity.machine(emoticon$clean, chunk.size = 10000, model = rf.model)
    optimize.cutoff(score.vec = emoticon$rf.score, polarity.vec = emoticon$polarity, min = 0.4, max = 0.7, step = 0.005)

    
  # Save data frames with scores
    save(sent140, file = paste(storage.directory,"sent140.with.lexicon.scores.RData", sep = "")) # save sent140 lexicon into memory
    save(emoticon, file = paste(storage.directory,"emoticon.with.lexicon.scores.RData", sep = "")) # save emoticon lexicon into memory
    
    
  # Example ggplot showing separation
    ggplot(sent140, aes(sent140[,"AFINN.score"], fill = as.factor(polarity))) + geom_density(alpha = .2, adjust = 1) + 
      theme(axis.text = element_text(size = 25),
            axis.title= element_text(size = 35),
            plot.title= element_text(size = 40),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 35),
            legend.position = "bottom") +
      scale_fill_manual(values = c(20,3),
                        labels = c(" negative   ", " positive"),
                        name = "Polarity:  ") +
      labs(x = "AFINN.score", y = "density") + ggtitle("AFINN.score densities over sent140")
    ggsave(filename = "afinn.sent140.png", plot = last_plot(), width = 12, height = 10)
    
    ggplot(emoticon, aes(emoticon[,"AFINN.score"], fill = as.factor(polarity))) + geom_density(alpha = .2, adjust = 7) +
      theme(axis.text = element_text(size = 25),
            axis.title= element_text(size = 35),
            plot.title= element_text(size = 40),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 35),
            legend.position = "bottom") +
      scale_fill_manual(values = c(20,3),
                        labels = c(" negative   ", " positive"),
                        name = "Polarity:  ") +
      labs(x = "AFINN.score", y = "density") + ggtitle("AFINN.score densities over emoticon") 
    ggsave(filename = "afinn.emoticon.png", plot = last_plot(), width = 12, height = 10)
    
    
    ggplot(sent140, aes(sent140[,"rf.score"], fill = as.factor(polarity))) + geom_density(alpha = .2) + 
      theme(axis.text = element_text(size = 25),
            axis.title= element_text(size = 35),
            plot.title= element_text(size = 40),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 35),
            legend.position = "bottom") +
      scale_fill_manual(values = c(20,3),
                        labels = c(" negative   ", " positive"),
                        name = "Polarity:  ") +
      labs(x = "rf.score", y = "density") + ggtitle("rf.score densities over sent140") 
    ggsave(filename = "rf.sent140.png", plot = last_plot(), width = 12, height = 10)
    
    ggplot(emoticon, aes(emoticon[,"rf.score"], fill = as.factor(polarity))) + geom_density(alpha = .2) + 
      theme(axis.text = element_text(size = 25),
            axis.title= element_text(size = 35),
            plot.title= element_text(size = 40),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 35),
            legend.position = "bottom") +
      scale_fill_manual(values = c(20,3),
                        labels = c(" negative   ", " positive"),
                        name = "Polarity:  ") +
      labs(x = "rf.score", y = "density") + ggtitle("rf.score densities over emoticon")
    ggsave(filename = "rf.emoticon.png", plot = last_plot(), width = 12, height = 10)
    
    
  # for loop iterating through all possible plots
    classifiers = c("AFINN", "OpinionFinder", "NRC", "ANEW", "rf")
    for(i in classifiers){
      model.name = paste(i, ".score", sep = "")
      print(ggplot(sent140, aes(sent140[,model.name], fill = as.factor(polarity))) + geom_density(alpha = .2) +
        labs(x = model.name, y = "density") + ggtitle(paste(model.name, "Densities over sent140")))
      print(ggplot(emoticon, aes(emoticon[,model.name], fill = as.factor(polarity))) + geom_density(alpha = .2) +
        labs(x = model.name, y = "density") + ggtitle(paste(model.name, "Densities over emoticon")))
    }


  
  # Storage - consider moving this to feather?
    load(paste(storage.directory, "emoticon.RData", sep = "")) # load emoticon/emoticon into memory as emoticon
    table(emoticon$polarity)
    
    load(paste(storage.directory,"ndsi.lexicon.RData", sep = "")) # load freq.all lexicon into memory as freq.all
    head(ndsi.lexicon)
    
    
    
    
# B BAG OF WORDS (random forest using term frequencies) ----
    
    # load emoticon.term.freq created by emoticon.R
    load(paste(storage.directory,"emoticon.term.freq.RData", sep = "")) # load emoticon.term.freq lexicon into memory as tf.idf
    
    # Training and test data indices
    nTrain = 5000
    nTest = 10000
    
    train_indices = c(sample(1:(nrow(emoticon.term.freq)/2),nTrain/2), 
                      sample((nrow(emoticon.term.freq)/2+1):(nrow(emoticon.term.freq)),nTrain/2))
    test_indices = c(sample(1:(nrow(emoticon.term.freq)/2),nTest/2), 
                     sample((nrow(emoticon.term.freq)/2+1):(nrow(emoticon.term.freq)),nTest/2))
    
    
  # Build a model using emoticon data and new dictionary
    
    # Caret Model
      library(doMC)
      registerDoMC(4)
      
      #  nTrain    1 core     2 cores     3 cores     4 cores
      #  100       3.13 mins  1.56 mins   2.39 mins   1.62 mins
      
      #  nTrain    1 core     2 cores     3 cores     4 cores
      #  500       41.4 mins  27.7 mins   32.9 mins   21.49 mins
      
      #  nTrain    1 core     2 cores     3 cores     4 cores
      #  1000      1.59 hrs   1.17 hrs    1.49 hrs    1.08 hrs
      
      #  nTrain    1 core     2 cores     3 cores     4 cores
      #  2000                                         6.08 hours
      
      #  nTrain    1 core     2 cores     3 cores     4 cores
      #  5000                                          hours
      
      
      Sys.time()
      a = Sys.time()
      rf.model = train(polarity~., data = emoticon.term.freq[train_indices,], method = "rf")
      Sys.time()-a
      beepr::beep(3)
    
    
    # Random Forest (SLOW: Takes MANY HOURS to run)
      library(randomForest)
      library(pROC)
      a = Sys.time()
      rf.model=randomForest(polarity~.,data = emoticon.term.freq[train_indices,])
      Sys.time()-a
    
      #Train data results
        pred.sentiment = predict(rf.model, newdata = emoticon.term.freq[train_indices,], type = "raw")
        confusionMatrix(pred.sentiment,emoticon.term.freq[train_indices,"polarity"])
        phat=predict(rf.model, newdata = emoticon.term.freq[train_indices,], type = "prob")
        plot(roc(emoticon.term.freq[train_indices,"polarity"],phat[,2]))  
      
      #Test data results
        pred.sentiment = predict(rf.model, newdata = emoticon.term.freq[test_indices,], type = "raw")
        confusionMatrix(pred.sentiment,emoticon.term.freq[test_indices,"polarity"])
        phat=predict(rf.model, newdata = emoticon.term.freq[test_indices,], type = "prob")
        plot(roc(emoticon.term.freq[test_indices,"polarity"],phat[,2]))
        
      #Save model
        save(rf.model, file = paste(storage.directory, "rf.model.RData", sep = ""))
        print("DONE! YAY!!!")
      

    
    
    
  # Apply lexicon and random forest to test (sent140) data for outside validation
    
    # Import sent140
    load(file = paste(storage.directory, "sent140.RData", sep = ""))
    
    # Import rf.model
    load(file = paste(storage.directory, "rf.model.RData", sep = ""))
    
    # Import svm.model
    load(file = paste(storage.directory, "svm.model.RData", sep = ""))

    
    classify.polarity.machine(documents = sent140$clean, model = rf.model)
    
    

    
    # Import freq.all lexicon and limit to 1276 words with ndsi above 0.05
    load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
    ndsi.lexicon = freq.all[freq.all$ndsi>0.05,]
    dim(ndsi.lexicon)
    
    #Import inv.doc.freq - no longer used
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








