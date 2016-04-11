library(cldr)
library(textcat) #this one didn't work so well on tweets because they're so short
# language detection

#install from archive
url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
pkgFile<-"cldr_1.1.0.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
unlink(pkgFile)
# or devtools::install_version("cldr",version="1.1.0")

#usage
library(cldr)
demo(cldr)

x[,c("language", "isReliable")] = detectLanguage(x$text)[c("detectedLanguage", "isReliable")]

#non-English Tweets
dim(x[x$language != "ENGLISH" & x$isReliable == TRUE, c("text", "language")]) #The blank and SPANISH tweets look like they could be removed, but other tweets look pretty English
x[x$language != "ENGLISH" & x$isReliable == TRUE, c("text", "language")]


#Spanish Tweets
dim(x[x$language == "SPANISH" & x$isReliable == TRUE, c("text", "language")])
x[x$language == "SPANISH" & x$isReliable == TRUE, c("text", "language")]

#Unknown Tweets
dim(x[x$language == "Unknown" & x$isReliable == TRUE, c("text", "language")])
x[x$language == "Unknown" & x$isReliable == TRUE, c("text", "language")] #These are all blank tweets
