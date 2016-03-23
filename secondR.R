#TODO:
# use str_count command
# column names for lexicons
# str_count function
# AFINN rating = sum(n_term * afinn.score_term)
# this part is computationally expensive!
# word to vec
# create your own vector list
# handling negation


# Ebert/Rider
# Goals for this script:
# 1) Import lexicons
# 2) Import data using read.csv
# 3) Clean data efficiently, writing to .csv
# 4) Analyze sentiment efficiently, writing to .csv


# Import lexicons ----

#lexicons MUST be formatted so that the first column lists words and the second column gives the sentiment score of that word.

  #ANEW
    ANEW = read.csv(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/ANEW.csv", header = FALSE)
    colnames(ANEW) = c("word", "score")
    #ANEW$score = ANEW$score-(mean(ANEW$score)+1) #normalize ANEW scores to 0. This didn't work as well as the next line did.
    ANEW$score = ANEW$score - 5
    range(ANEW$score)
    head(ANEW[order(-ANEW$score),], 20) #happiest words
    head(ANEW[order(ANEW$score),], 20) #saddest words

  #EmoLex
    emolex = read.csv(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/EmoLex/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt",
                      sep = "\t", header = FALSE)
    colnames(emolex) = c("word", "emotion", "indicator")
    emolex = emolex[emolex$emotion == "negative"|emolex$emotion == "positive",]
    emolex = emolex[emolex$indicator == 1,]
    emolex[emolex$emotion == "negative",]$indicator = -1
    emolex = emolex[c("word", "indicator")]
    colnames(emolex) = c("word", "score")

  #Wiebe
    Wiebe_lexicon = read.csv(system.file("data/subjectivity.csv.gz", 
                                         package = "sentiment"), header = FALSE, stringsAsFactors = FALSE)
    #write.csv(Wiebe_lexicon, file = "Wiebe_lexicon.csv")
    Wiebe_lexicon = as.data.frame(cbind(as.character(Wiebe_lexicon$V1), as.integer(2*(Wiebe_lexicon$V3 == "positive")-1)))
    colnames(Wiebe_lexicon) = c("word", "score")
    Wiebe_lexicon$score = as.integer(Wiebe_lexicon$score)
    Wiebe_lexicon$score = (((Wiebe_lexicon$score-1)*2)-1)*-1

    #looking up Wiebe words
    Wiebe_lexicon[Wiebe_lexicon$score == -1,]
    Wiebe_lexicon[pmatch("asu", Wiebe_lexicon$word),] #the word "asu" matches "asunder" and gets a score of -1!!!
    Wiebe_lexicon[pmatch("you", Wiebe_lexicon$word),] 


  #AFINN
    AFINN_lexicon = read.delim(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE)
    colnames(AFINN_lexicon) = c("word", "score")

    #looking up AFINN words
    AFINN_lexicon[AFINN_lexicon$score == -5,]
    AFINN_lexicon[pmatch("am", AFINN_lexicon$word),2]
    AFINN_lexicon[pmatch("feeling", AFINN_lexicon$word),2]
    AFINN_lexicon[pmatch("creaking", AFINN_lexicon$word),2]
    AFINN_lexicon[pmatch("plague", AFINN_lexicon$word),2]

  #Negation Lexicon
    negations = c("no", "not","none","nobody","nothing","neither","never","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")

# Import 2014 ComtweetsLA.csv target data ----

  # read in ComTweetsLA.csv in +- 2 mins:
    x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", nrows = 100, header = TRUE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

# clean.data ----
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
    documents<-gsub("[[:punct:]]", "", documents) #remove punctuation
    documents<-gsub("[[:digit:]]", "", documents) #remove numbers
    #documents<-gsub("http\\w+", "", documents) #remove
    documents<-gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents<-tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents) # limit character repeats to maximum 2
    documents<-trimws(documents) #remove leading and trailing whitespace
  }, .progress = "text")
  return(cleantext)
}

# Test clean.data ----
testdocs = c(
  "   Heello world!!!      ", 
  "big      space",
  "HAAAAPPPIE",
  "<\r> formatting <\r>",
  "blahBlahblahblah",
  "@THESADDESTBAE fuck the haters",
  "Thanks! RT @GuardadoJesus: That's cool http://t.co/JqOMdiiFAl",
  "RT @FreeLaddin: Smash RT @daniebaybe: THATS SO FOUL! RT @NegroBendito: “@CHlLDHOODRUINER: RT TO RUIN SOMEONES NIGHT http://t.co/EyfCpDC")
cbind(testdocs, clean.data(testdocs))

# classify.sentiment ----
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

# classify.sentiment2 (modelled after Crawford's code. This didn't work as well as the original classify.sentiment and is not included in functions.R) ----
classify.sentiment2 = function(documents, lexicon = AFINN_lexicon){
  require(stringr)
  sentscorevec = laply(documents, function(documents, lex = lexicon)
  {
    return(str_count(documents,lexicon[,1])%*% lexicon[,2])
  }, .progress = "text")
  return(sentscorevec)
}

#compare sentiment classifiers. Note that this 2nd version is noticeably slower when working with 100 rows
x.cleaned = clean.data(x$text[1:100])
cbind(x.cleaned, classify.sentiment2(x.cleaned), classify.sentiment1(x.cleaned))

# Test classify.sentiment ----
testdocs = c(
  "hello world",
  "so sad",
  "not sad",
  "not happy",
  "happy",
  "disgusted",
  "dont stop",
  "cant stop",
  "wasnt stop",
  "cant be happy",
  "unhappy",
  "dont stop",
  "no no no no no no no no no no no no no no",
  "asu asu asu asu asu asu",
  "unhappy",
  "so happy",
  "sit down",
  "no one fucking notices if im gone",
  "lol same here but i barely get views",
  "putin is just trying to get his europe bonus by invading ukraine risktweet",
  "lmao oh my i gotta stop being so mean",
  "you are the creaking on my steps you are cancer you are plagueyou are regret you are diseasei wish that you would go away",
  "you are the creaking on my steps you are cancer you are plague you are regret you are disease i wish that you would go away",
  "im probs gonna die tomorrow bc i dont sleep",
  "bruh i fell down a flight of stairs tonjght",
  "solo tu me sabes envolver",
  "quote with a picture of us",
  "darryl sutter is thrilled to win another stanley cup staples center",
  "quando eu for em paris eu vou escrever meu nome e o nome da minha namorada em um cadeado e colocar naquela ponte l")
cbind(testdocs,classify.sentiment(testdocs, Wiebe_lexicon), classify.sentiment(testdocs, AFINN_lexicon))

# clean.analyze.write ----

  # Import x
    a = Sys.time()
    print(a)
    x = read.csv(file = "~/Desktop/Huang Research/Rsentiment/ComTweetsLA.csv", skip = 6000000, nrows = 3400000, header = FALSE, colClasses = 
                   c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
    colnames(x) = c("text","username","tweet_id","lat","long","year","month","date","hour","minute","second")

  # clean x
    x[,"text"] = clean.data(x$text)

  # remove empty rows
    x = x[nchar(x$text)>2,]

  # Wiebe sentiment - OBSOLETE
  #  x[,"Wiebe.sentiment"] = classify.sentiment(x$text, lexicon = Wiebe_lexicon)

  # AFINN sentiment
    x[,"AFINN.sentiment"] = classify.sentiment(x$text, lexicon = AFINN_lexicon)

  # AFINN sentiment2 - OBSOLETE
  #  x[,"AFINN.sentiment2"] = classify.sentiment2(x$text, lexicon = AFINN_lexicon)

  #Skip rerunning Wiebe through sentiment2 because it's not as good and will run slower
    dim(x) # text now has 15 columns, 4 more than before
    head(x)

  # write table
    #write.table(x, file = "~/Desktop/Huang Research/Rsentiment/latweets.sentiment2.csv", row.names = FALSE, sep = ",")
    # append table
    write.table(x, file = "~/Desktop/Huang Research/Rsentiment/latweets.sentiment2.csv", col.names = FALSE, append = TRUE)

  rm(x)

  print(Sys.time() - a)

  # read data back into R ----
    y = read.csv(file = "~/Desktop/Huang Research/Rsentiment/latweets.sentiment2.csv", skip = 0, nrows = 2000, header = TRUE, 
                 colClasses = c("character", "character", "character", "numeric", "numeric", "integer", 
                                "integer", "integer", "integer", "integer", "integer", "integer", "integer"))

  # Sample 100
  set.seed(100)
  idx = sample(nrow(y), 100)
  y.sample = y[idx,c("text", "username", "Wiebe.sentiment", "AFINN.sentiment")]

# Check scores ----
dim(y) #8859604 rows
range(y$AFINN.sentiment) # -135  88
range(y$Wiebe.sentiment) # -29   44

mean(y$AFINN.sentiment) # 0.4408979
mean(y$Wiebe.sentiment) # 0.512264


hist(y$AFINN.sentiment)
hist(y$Wiebe.sentiment)

quantile(y$AFINN.sentiment, c(0, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1))
quantile(y$Wiebe.sentiment, c(0, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 1))


#look at some of the extreme entries
hist(y$Wiebe.sentiment[y$Wiebe.sentiment > 10 | y$Wiebe.sentiment < (-10)]) #tails of Wiebe.sentiment
hist(y$AFINN.sentiment[y$AFINN.sentiment > 25 | y$AFINN.sentiment < (-25)]) #tails of AFINN.sentiment

# VERY HAPPY:
x_1 = y[y$Wiebe.sentiment>25, c("text", "Wiebe.sentiment", "AFINN.sentiment")] #lots of repeated words. Negations are handled poorly.
x_2 = y[y$AFINN.sentiment>50, c("text", "Wiebe.sentiment", "AFINN.sentiment")] #again, lots of repeated words. Fewer negation problems.
happy = unique(rbind(x_1, x_2))
# AFINN handles text words  like "yo" and "lmao" more robustly

# VERY SAD
w_1 = y[y$Wiebe.sentiment<(-25), c("text", "Wiebe.sentiment", "AFINN.sentiment")] #lots of repeated words. otherwise looks reasonable
w_2 = y[y$AFINN.sentiment<(-75), c("text", "Wiebe.sentiment", "AFINN.sentiment")] #again, lots of repeated words. Fewer negation problems.
sad = unique(rbind(w_1, w_2))

write.csv(happy, file = "happiesttweets.csv")
write.csv(sad, file = "saddesttweets.csv")

#analyze sample of 100000 ----
set.seed(100)
idx = sample(nrow(y), 100000)
y.sample = y[idx,c("text", "username", "Wiebe.sentiment", "AFINN.sentiment")]

#use Sentiment140 to check accuracy ----
testdata = read.csv(file = "testdata.manual.2009.06.14.csv", header = FALSE, stringsAsFactors = FALSE)
dim(testdata)
colnames(testdata) = c("polarity", "ID", "date", "query", "username", "text") # column names
testdata$polarity = as.factor(testdata$polarity) # set polarity to factor
testdata = as.data.frame(cbind(testdata$polarity, testdata$text)) #throw away everything exept polarity and text
colnames(testdata) = c("polarity", "text")

testdata$polarity = c("negative", "neutral", "positive")[testdata$polarity]
table(testdata$polarity)

testdata$clean = clean.data(testdata$text)
testdata$AFINN = classify.sentiment(testdata$clean)
testdata$Wiebe = classify.sentiment(testdata$clean, lexicon = Wiebe_lexicon)

head(testdata[c("polarity", "AFINN", "Wiebe", "clean")])
testdata$predAFINN = as.factor(sign(testdata$AFINN))
testdata$predWiebe = as.factor(sign(testdata$Wiebe))
testdata$predAFINN = c("negative", "neutral", "positive")[testdata$predAFINN]
testdata$predWiebe = c("negative", "neutral", "positive")[testdata$predWiebe]

library(caret)
confusionMatrix(testdata$polarity, testdata$predAFINN)
#AFINN accuracy is 60%
confusionMatrix(testdata$polarity, testdata$predWiebe)
#Wiebe accuracy is 53.8%
