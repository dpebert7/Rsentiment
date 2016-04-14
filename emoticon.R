# David Ebert
# 21 March 2016
# GOAL: Create a semi-supervised training set from within comtweetsLA


# READ IN DATA ComTweetsLA.csv in +- 2 mins ----
  load("~/Desktop/Huang Research/Rsentiment/x.RData") # load LA2014 (non-Spanish-ish) into memory as x

# HAPPY EMOTICONS ----
  grep("\\:\\)", x$text, value = TRUE) #67189 :)'s in the whole set. Takes about 11 sec. to run
  grep("\\(\\:", x$text, value = TRUE) #14401 (:'s in the whole set. 
  grep("\\:-\\)", x$text, value = TRUE) #14069 :-)'s in the whole set. 
  grep("\\(-\\:", x$text, value = TRUE) #1903 (-:'s in the whole set. 
  grep("\\:D", x$text, value = TRUE) #9724 :D's in the whole set SEEMS TOO MANY
  grep("\\:-D", x$text, value = TRUE) #20230 :-D's in the whole set
  grep("=\\)", x$text, value = TRUE) #1467 =)'s in the whole set
  grep("\\(=", x$text, value = TRUE) #195 (='s in the whole set
  grep("☺", x$text, value = TRUE) #38441 of them in the whole set
  grep("☻", x$text, value = TRUE) #130 these in the whole set
  grep("☀", x$text, value = TRUE) #7754 of these in the whole set. Leave these out, though
  
  happy_emoticons = c("\\:\\)" , "\\(\\:", "\\:-\\)", "\\(-\\:", "\\:D", "\\:-D", "=\\)", "\\(=", "☺", "☻")
  
  #grep everything all at once
  happy_indices = grep(paste(happy_emoticons, collapse = "|"),x$text, value = FALSE)

  length(happy_indices) #happy_indices has length 144678
  x = x[happy_indices,] #Rewrite over x to avoid memory problems
  dim(x) # Looks good
  write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/happy_tweets_2014", row.names = FALSE)

  rm(x)
  #reload x after this part. Sorry about the inconvenience :(


  

# SAD EMOTICONS ----
  grep("\\:\\(", x$text, value = TRUE) #40065 :('s in the whole set. 
  grep("\\:-\\(", x$text, value = TRUE) #3485 :-('s in the whole set. 
  grep("\\)\\:", x$text, value = TRUE) #5489 ):'s in the whole set. 
  grep("\\)-\\:", x$text, value = TRUE) #355 )-:'s in the whole set. 
  grep(":\\[", x$text, value = TRUE) #70 :['s in the whole set.
  #grep("\\]:", x$text, value = TRUE) #2636 ]:'s in the whole set. DON'T USE THIS ONE. IT'S A LIE!!! [pic]: 
  grep(":\\{", x$text, value = TRUE) #32 :{'s in the whole set.
  grep("\\}:", x$text, value = TRUE) #25 }:'s in the whole set.
  grep("=\\(", x$text, value = TRUE) #131 =('s in the whole set
  grep("\\)=", x$text, value = TRUE) #59 )='s in the whole set
  grep("☹", x$text, value = TRUE) #222 of these in the whole set
  
  sad_emoticons = c("\\:\\(", "\\:-\\(", "\\)\\:", "\\)-\\:", ":\\[", ":\\{", "\\}:","=\\(", "\\)=", "☹")
  
  #grep everything all at once
  sad_indices = grep(paste(sad_emoticons, collapse = "|"),x$text, value = FALSE)

  length(sad_indices) #sad_indices has length 47982
  x = x[sad_indices,] #Write over x to avoid memory problems
  dim(x) # Looks good
  write.csv(x,file = "~/Desktop/Huang Research/Rsentiment/sad_tweets_2014", row.names = FALSE)
  
  rm(x)
  #reload x after this part. Sorry about the inconvenience :(
  

# WORD CLOUD for sad and happy tweets ----
  library(wordcloud)
  library(tm)
  # load happy and sad tweets
  happy_tweets = read.csv(file = paste(storage.directory, "happy_tweets_2014", sep = ""), nrows = 150000, header = TRUE, colClasses = 
                 c("character", "character", "character", "numeric", "numeric", "POSIXct"))
  dim(happy_tweets)
  
  sad_tweets = read.csv(file = paste(storage.directory, "sad_tweets_2014", sep = ""), nrows = 50000, header = TRUE, colClasses = 
                          c("character", "character", "character", "numeric", "numeric", "POSIXct"))
  dim(sad_tweets)
  
  # clean tweets 
  #  ALL tokenization is turned off for this step to avoid getting a hunge "USERNAME" in the middle of the word cloud
  happy_tweets$clean = clean.tweets(happy_tweets$text, usernameToken = "", hashToken = "", sadToken = "", happyToken = "")
  sad_tweets$clean =   clean.tweets(sad_tweets$text,   usernameToken = "", hashToken = "", sadToken = "", happyToken = "")

  #Create corpus using tm package
  happy_corpus = Corpus(VectorSource(happy_tweets$clean))
  sad_corpus = Corpus(VectorSource(sad_tweets$clean))

  #Remove stop words
  happy_corpus <- tm_map(happy_corpus, function(x)removeWords(x,stopwords())) # ~2 minutes
  sad_corpus <- tm_map(sad_corpus, function(x)removeWords(x,stopwords())) # ~1 minute

  #Make word cloud. Check reference for options 
  wordcloud(happy_corpus, max.words = 200) # ~4 minutes
  wordcloud(sad_corpus, max.words = 200) # ~3 minutes
  

# WRITE HAPPY AND SAD TO A BALANCED DF OF 102 000 TWEETS ----
  happy_tweets = read.csv(file = paste(storage.directory, "happy_tweets_2014", sep = ""), nrows = 150000, header = TRUE, colClasses = 
                            c("character", "character", "character", "numeric", "numeric", "POSIXct"))

  sad_tweets = read.csv(file = paste(storage.directory, "sad_tweets_2014", sep = ""), nrows = 50000, header = TRUE, colClasses = 
                          c("character", "character", "character", "numeric", "numeric", "POSIXct"))
  
  # Clean tweets, remove blank tweets, then undersample from happy so that happy has as many tweets as sad does.
  happy_tweets$clean = clean.tweets(happy$text, happyToken = "", sadToken = "") # do NOT tokenize :)
  sad_tweets$clean = clean.tweets(sad$text, happyToken = "", sadToken = "")     # do NOT tokenize :(
  happy_tweets$text = NULL
  sad_tweets$text = NULL
  
  happy_tweets  = happy_tweets[happy_tweets$clean!="",]
  sad_tweets  = sad_tweets[sad_tweets$clean!="",]
  
  set.seed(127)
  index = sample(nrow(happy_tweets),nrow(sad_tweets))
  happy_tweets = happy_tweets[index,]
  dim(happy_tweets)
  dim(sad_tweets) #both should have same number of rows

  # Initialize polarity of happy and sad tweets
  happy_tweets$polarity = as.factor(1)
  sad_tweets$polarity = as.factor(0)
  emoticon = rbind(as.data.frame(happy_tweets[,c("clean", "polarity")]),sad_tweets[,c("clean", "polarity")]) 
  colnames(emoticon) = c("clean", "polarity") #note that cleaning already took place, so these column names are appropriate
  dim(emoticon)
  table(emoticon$polarity)
  save(emoticon, file = paste(storage.directory,"emoticon.RData", sep = "")) # save train/emoticon into memory as emoticon
  load(paste(storage.directory,"emoticon.RData", sep = "")) # load train/emoticon into memory as emoticon
  
  
# MAKE A LEXICON FROM THE TRAINING (EMOTICON) DATA ----
  load("~/Desktop/Huang Research/Rsentiment/emoticon.RData") # load train/emoticon into memory as emoticon
  table(emoticon$polarity)
  head(word.freq(head(emoticon$clean,1000)), 100) # create word frequency data frame for first 1000 tweets
  
  word.freq.pos = word.freq(emoticon$clean[emoticon$polarity == 1],
                            sparsity=0.9999) #terms must occur in at least 1 out of 1000 tweets
  word.freq.neg = word.freq(emoticon$clean[emoticon$polarity == 0],
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
  head(freq.all[order(-freq.all$diff), ],100) #this is somewhat puzzling
  
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
  head(freq.all$word,100)
  freq.all = freq.all[freq.all$ndsi>0,] # restrict to words with a nonzero ndsi score.
  
  freq.all$word = as.character(freq.all$word)
  
  dim(freq.all)
  head(freq.all)
  freq.all = freq.all[freq.all$word != "sadtoken" & freq.all$word != "happytoken",]
  save(freq.all, file = paste(storage.directory,"freq.all.RData", sep = "")) # save freq.all into memory as freq.all.RData
  load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all

  
  
# Build tf.idf model using emoticon data and new dictionary ----
  
  #Restrict term.freq to words with higher ndsi scores
  freq.all = freq.all[freq.all$ndsi>0.05,]
  dim(freq.all)
  
  
  #Term Frequencies (Takes about 25 minutes to run with 100k tweets)
  a = Sys.time()
  emoticon.term.freq <- t(apply(t(emoticon[,"clean"]), 2,    #TAKES TIME; 10 minutes for 100 000 tweets and 1276 terms 
                       ndsi.frequencies))
  Sys.time()-a
  
  emoticon.term.freq = data.frame(polarity=emoticon$polarity, emoticon.term.freq)
  
  save(emoticon.term.freq, file = paste(storage.directory,"emoticon.term.freq.RData", sep = "")) # save emoticon.term.freq lexicon into memory as tf.idf
  load(paste(storage.directory,"emoticon.term.freq.RData", sep = "")) # load emoticon.term.freq lexicon into memory as tf.idf

  
                #Try Skipping this for now
                inv.doc.freq=log(nrow(emoticon)/colSums(sign(term.freq)))
                range(inv.doc.freq)
                
                inv.doc.freq[is.infinite(inv.doc.freq)]=0
                range(inv.doc.freq)
                
                #SAVE inv.doc.freq for later use. In particular we care about its diagonal.
                save(inv.doc.freq, file = paste(storage.directory, "inv.doc.freq.RData", sep = "")) #save inv.doc freq for classifier
                load(file = paste(storage.directory, "inv.doc.freq.RData", sep = ""))
                
                tf.idf = term.freq %*% diag(inv.doc.freq)
                
                save(tf.idf, file = paste(storage.directory,"tf.idf.RData", sep = "")) # save tf.idf lexicon into memory as tf.idf
                load(paste(storage.directory,"tf.idf.RData", sep = "")) # load tf.idf lexicon into memory as tf.idf
  
                save(emoticon.term.freq, file = paste(storage.directory,"tf.idf.RData", sep = "")) # save tf.idf lexicon into memory as tf.idf
                load(paste(storage.directory,"tf.idf.RData", sep = "")) # load tf.idf lexicon into memory as tf.idf
              
                # emoticon.tf.idf for training and testing machine learning classifiers using EMOTICON data ----
                emoticon.tf.idf = data.frame(polarity=emoticon$polarity, tf.idf)
                
                save(emoticon.tf.idf, file = paste(storage.directory,"emoticon.tf.idf.RData", sep = "")) # save emoticon.tf.idf lexicon into memory as tf.idf
                load(paste(storage.directory,"emoticon.tf.idf.RData", sep = "")) # load emoticon.tf.idf lexicon into memory as tf.idf


