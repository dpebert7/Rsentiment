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

#Wiebe
Wiebe_lexicon = read.csv(system.file("data/subjectivity.csv.gz", 
                                     package = "sentiment"), header = FALSE, stringsAsFactors = FALSE)
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
AFINN_lexicon[pmatch("happy", AFINN_lexicon$word),2]
AFINN_lexicon[pmatch("stop", AFINN_lexicon$word),2]
AFINN_lexicon[pmatch("creaking", AFINN_lexicon$word),2]
AFINN_lexicon[pmatch("plague", AFINN_lexicon$word),2]

#Negation Lexicon
negations = c("no", "not","none", "no one","nobody","nothing","neither","nowhere","never","hardly",
              "scarcely","barely","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")

# Import data ----

# read in ComTweetsLA.csv in +- 2 mins:
x = read.csv(file = "~/Desktop/Huang Research/Rladata/ComTweetsLA.csv", nrows = 9400000, header = TRUE, colClasses = 
               c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))

# clean.data ----
clean.data = function(documents){
  require(plyr)
  require(dplyr)
  cleantext = laply(documents, function(documents)
  {
    documents<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", documents) #remove hyperlinks
    documents<-gsub("@\\w+", "", documents) #remove @
    documents<-gsub("[[:punct:]]", "", documents) #remove punctuation
    documents<-gsub("[[:digit:]]", "", documents) #remove numbers
    documents<-gsub("http\\w+", "", documents) #remove
    documents<-gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents<-tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents)
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
  "blahBlahblahblah")
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
x = read.csv(file = "~/Desktop/Huang Research/Rladata/ComTweetsLA.csv", skip = 5000000, nrows = 4400000, header = FALSE, colClasses = 
               c("character", "character", "character", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer"))
colnames(x) = c("text","username","tweet_id","lat","long","year","month","date","hour","minute","second")

# clean x
x[,"text"] = clean.data(x$text)

# remove empty rows
x = x[nchar(x$text)>2,]

# Wiebe Sentiment x
x[,"Wiebe.sentiment"] = classify.sentiment(x$text, lexicon = Wiebe_lexicon)

# AFINN sentiment x
x[,"AFINN.sentiment"] = classify.sentiment(x$text, lexicon = AFINN_lexicon)

dim(x) # text now has 14 columns, 3 more than before
head(x)

# write table
#write.table(x, file = "latweets.sentiment.csv", row.names = FALSE, sep = ",")
# append table
#write.table(x, file = "latweets.sentiment.csv", row.names = FALSE, sep = ",", col.names = FALSE, append = TRUE)

rm(x)

print(Sys.time() - a)

# read data back into R ----
y = read.csv(file = "~/Desktop/Huang Research/Rsentiment/latweets.sentiment.csv", skip = 0, nrows = 8900000, header = TRUE, 
             colClasses = c("character", "character", "character", "numeric", "numeric", "integer", 
                            "integer", "integer", "integer", "integer", "integer", "integer", "integer"))

# Sample 100
set.seed(100)
idx = sample(nrow(y), 100)
y.sample = y[idx,c("text", "username", "Wiebe.sentiment", "AFINN.sentiment")]

# sanity tests ----
dim(y) #8859604 rows
range(y$AFINN.sentiment)
range(y$Wiebe.sentiment)

hist(y$AFINN.sentiment)
hist(y$Wiebe.sentiment)

#look at some of the extreme entries
hist(y$Wiebe.sentiment[y$Wiebe.sentiment > 12 | y$Wiebe.sentiment < (-12)]) #tails of Wiebe.sentiment
hist(y$AFINN.sentiment[y$AFINN.sentiment > 30 | y$AFINN.sentiment < (-30)]) #tails of AFINN.sentiment

# VERY HAPPY:
y[y$Wiebe.sentiment>25, c("text", "Wiebe.sentiment", "AFINN.sentiment")] #lots of repeated words. Negations are handled poorly.
y[y$AFINN.sentiment>50, c("text", "Wiebe.sentiment", "AFINN.sentiment")] #again, lots of repeated words. Fewer negation problems.
# AFINN handles text words  like "yo" and "lmao" more robustly

# VERY SAD
y[y$Wiebe.sentiment<(-25), c("text", "Wiebe.sentiment", "AFINN.sentiment")] #lots of repeated words. otherwise looks reasonable
y[y$AFINN.sentiment<(-50), c("text", "Wiebe.sentiment", "AFINN.sentiment")] #again, lots of repeated words. Fewer negation problems.

#analyze sample of 100000 ----
set.seed(100)
idx = sample(nrow(y), 100000)
y.sample = y[idx,c("text", "username", "Wiebe.sentiment", "AFINN.sentiment")]

