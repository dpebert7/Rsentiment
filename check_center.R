# David Ebert
# September 13, 2016

# Are there any LA county tweets that are geotagged only from the marker of LA county and NOT actually a place out in LA county?

# Short answer. This might be a feasible way to tackle the problem of junk tweets, but it will take more work to make it work.

library(ggmap)
geocode("LA county")
# The Google center of LA county is:
#  lon      lat
# -118.2437 34.05223

# loaded LA2014 as x
load(file = paste(storage.directory, "x.RData", sep = ""))

head(x)


# find tweets that are close to center
target_lon = geocode("LA county")$lon
target_lat = geocode("LA county")$lat
dist = 0.001

table(x$lon>=(target_lon-dist) & x$lon<=(target_lon+dist) & x$lat>=(target_lat-dist) & x$lat<=(target_lat+dist))
x[x$lon>=(target_lon-dist) & x$lon<=(target_lon+dist) & x$lat>=(target_lat-dist) & x$lat<=(target_lat+dist),c("text","username")]


#Now check this against x:



#nrow(x) is 9 383 334
table(x$lon>=(target_lon-dist) & x$lon<=(target_lon+dist) & x$lat>=(target_lat-dist) & x$lat<=(target_lat+dist))[2]/nrow(x)

# dist     # % of tweets within dist
# 0.1      # 7.8%
# 0.01     # 0.2% 
# 0.001    # 0.01%  <- 1473 tweets
# 0.0005   # <0.01% <- Lots of advertisements in this set of 752 tweets
# 0.0001   # <0.01% <- Lots of advertisements in this set of 713 tweets