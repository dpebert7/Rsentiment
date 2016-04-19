#Ebert/Rider
#Created 16 April 2016

#Purpose: Create sentiment score map of LA2014 data.


#load a little bit of sample data from x
load(file = paste(storage.directory, "x1.RData", sep = "")) 

x1 = x1[x1$username != "GoldstarPgh",] #remove friggin Pittsburgh newspaper and its >3000 tweets
x1 = x1[x1$username != "GoldstarSA",] #remove friggin San Antonio newspaper and its >3000 tweets
x1 = x1[x1$username != "LoansHomes",] #remove 
x1 = x1[x1$username != "AAAcali11",] #remove
x1 = x1[x1$username != "Stompy_0487",] #remove
x1 = x1[x1$username != "Tony_Lien",] #remove friggin Tony lien's checkins
x1 = x1[x1$username != "CreativeGali",] #remove

which.max(table(x1$username))
dim(x1[x1$username == "ThatBoyJoy",])



# 2865 tweets; no real sentiment, but well travelled, apparently:
Tony_Lien = x1[x1$username == "Tony_Lien",]
save(Tony_Lien, file = paste(storage.directory, "Tony_Lien.RData", sep = ""))
Tony_Lien = Tony_Lien[,c("lat", "long", "AFINN.polarity")]

# 2821 tweets; more what I was expecting, finally:
ThatBoyJoy = x1[x1$username == "ThatBoyJoy",]
save(ThatBoyJoy, file = paste(storage.directory, "ThatBoyJoy.RData", sep = ""))
ThatBoyJoy = ThatBoyJoy[,c("lat", "long", "AFINN.polarity")]
load(file = paste(storage.directory, "ThatBoyJoy.RData", sep = ""))


library(ggmap)

