<<<<<<< HEAD
## Rsentiment

Polarity analysis of Tweets from LA county for purpose of studying land use. Tweet collection and analysis completed in R using streamR, twitteR, and randomforest. Validation data from http://help.sentiment1.com/for-students/.

trainedClassifier.R creates trained classifiers for classifying LA tweets. The code is founded on crawfordCode.R

secondR.R is the most current script for the lexicon approach to classifying tweets.

functios.R includes commonly used functions and lexicons

semisuper.R creates a semi-supervised training set from previously collected LACounty tweets. It also creates a word cloud of happy and sad terms from the semi-supervised training set.

supervisedLAtweets.R is a failed attempt to collect old tweets from LA county. It is replaced by semisuper.R

crawfordCode.R is code from Dr. Crawford's Data Mining 2 class. It is the foundation of trainedClassifier.R

keepcapturingLAtweets.R continuously captures geotagged tweets from LA county. The most up-to-date version of this script is running on the shared school computer.
=======
## HAR
Human activity recognition using smartphone data.


#### Description
This repository includes code from a data mining project. The data set can be accessed from the  [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The goal of the project is to predict 6 classes of activity (walking, walking upstairs, walking downstairs, sitting, standing, and laying) based on data gathered from a cell phone accelerometer and gyroscope.


#### Results
SVM probably gives the best results. More coming.


#### To do list
 - [x] Trees
   - [x] basics
   - [x] picture of 2-category case
 - [x] SVM
   - [x] basic
   - [x] tune SVM
 - [x] naiveBayes
 - [x] Neural networks - Parker
 - [x] K nearest neighbor
 - [x] Silly walks
   - [x] basic comparing 2 walkers with trees
   - [x] cross validation of at least one model
   - [x] apply svm
   - [x] multiple walkers
 - [x] Understand more about accelerometers and gyroscopes

>>>>>>> 2d34ac832f7295638b4434e8cfff5fcaa3c724a5
