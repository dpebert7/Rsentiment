#Ebert/Rider
#import.R
#updated 21 March 2016
#Purpose: Import data into R

# Note that functions may be imported using source("functions.R")

# Import Tweets ----

  # LA2014
    load("~/Desktop/Huang Research/Rsentiment/comTweetsLA.RData") # load LA2014 into memory as x

  # LA2016

  #Sentiment140
    testdata = read.csv(file = "testdata.manual.2009.06.14.csv", header = FALSE, stringsAsFactors = FALSE)

# Import lexicons ----

  #List of negation words
    negations = c("no", "not","none","nobody","nothing","neither","never","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")

  #lexicons MUST be formatted so that the first column lists words and the second column gives the sentiment score of that word.
  #ANEW
    ANEW = read.csv(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/ANEW.csv", header = FALSE)
    colnames(ANEW) = c("word", "score")
    #ANEW$score = ANEW$score-(mean(ANEW$score)+1) #normalize ANEW scores to 0. This didn't work as well as the next line did.
    ANEW$score = ANEW$score - 5
    range(ANEW$score)
    head(ANEW[order(-ANEW$score),], 20) #happiest words
    head(ANEW[order(ANEW$score),], 20) #saddest words

  #NRC Word-Emotion Association Lexicon
    NRC = read.csv(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/EmoLex/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt",
                     sep = "\t", header = FALSE)
    colnames(NRC) = c("word", "emotion", "indicator")
    NRC = NRC[NRC$emotion == "negative"|NRC$emotion == "positive",]
    NRC = NRC[NRC$indicator == 1,]
    NRC[NRC$emotion == "negative",]$indicator = -1
    NRC = NRC[c("word", "indicator")]
    colnames(NRC) = c("word", "score")

  #OpinionFinder Lexicon
    OpinionFinder = read.csv(system.file("data/subjectivity.csv.gz", 
                                     package = "sentiment"), header = FALSE, stringsAsFactors = FALSE)
    #write.csv(OpinionFinder, file = "OpinionFinder")
    OpinionFinder = as.data.frame(cbind(as.character(OpinionFinder$V1), as.integer(2*(OpinionFinder$V3 == "positive")-1)))
    colnames(OpinionFinder) = c("word", "score")
    OpinionFinder$score = as.integer(OpinionFinder$score)
    OpinionFinder$score = (((OpinionFinder$score-1)*2)-1)*-1

    #looking up Wiebe words
    OpinionFinder[OpinionFinder$score == -1,]
    OpinionFinder[pmatch("asu", OpinionFinder$word),] #the word "asu" matches "asunder" and gets a score of -1!!!
    OpinionFinder[pmatch("you", OpinionFinder$word),] 


  #AFINN
    AFINN = read.delim(file = "~/Desktop/Documents/GitRepos/Rsentiment/Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE)
    colnames(AFINN) = c("word", "score")

    #looking up AFINN words
    AFINN[AFINN$score == -5,]
    AFINN[pmatch("am", AFINN$word),2]
    AFINN[pmatch("feeling", AFINN$word),2]
    AFINN[pmatch("creaking", AFINN$word),2]
    AFINN[pmatch("plague", AFINN$word),2]