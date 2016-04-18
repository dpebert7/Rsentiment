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


library(ggmap) 

# The Tony_Lien travel map
Tony_Lien_map <- get_map(location = 'West Covina', zoom = 9)
ggmap(my_map)
ggmap(my_map)+
  geom_point(aes(x = long, y = lat), data = Tony_Lien,
             alpha = .5, color="darkred", size = 2)

# The ThatBoyJoy travel map
ThatBoyJoy_map <- get_map(location = 'Bellflower', zoom = 12)
ggmap(ThatBoyJoy_map)
ggmap(my_map)+
  geom_point(aes(x = long, y = lat), data = ThatBoyJoy,
             alpha = .5, color="darkred", size = 3)


# The Tony_Lien travel map
ThatBoyJoy_map <- get_map(location = 'Bellflower', zoom = 12)
ggmap(my_map)
ggmap(my_map)+
  geom_tile(aes(x = long, y = lat), data = ThatBoyJoy,
             alpha = .5, color="darkred", size = 3)

# Attempt at heat map using chicago crime map as template
ggmap(ThatBoyJoy_map) + 
  geom_tile(data = ThatBoyJoy, aes(x = long, y = lat, alpha = Frequency),
                           fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())





