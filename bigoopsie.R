

#What do these tweets all have in common?

#[1654] "I'm at Ķempelīšu mājeles [pic]: http://t.co/Fw4Kp70Ee1"                                                                                     

#[2072] "What are we doing?! (@ McDonald's 麥當勞) [pic]: http://t.co/5aLrinYoKo" 

#[2013] "I'm at Eggberts (KapaaHI) [pic]: http://t.co/KcciBeSd8c http://t.co/ZsUKj00sl1"                                                             

#[2906] "I'm at 宜品福州乾拌麵（林口店） [pic]: http://t.co/9QdIvMND1B"                                                                              

#[2919] "Yummy (@ Olympic Cafe House of Breakfast) [pic]: http://t.co/p33BLvIwzI"                                                                    

#[2930] "I'm at The Original @FarmersMarketLa (Los Angeles CA) w/ 7 others [pic]: http://t.co/5sktSNS1CR"

#[2876] "#jwsabudhabi (@ ADNEC مركز أبوظبي الوطني للمعارض) [pic]: http://t.co/D0ZBplySru"                                                            

#[2946] "Smaller portion with brown rice. #HealthierChoices (@ Pei Wei Asian Diner) [pic]: http://t.co/ezUW3m1eFc"                                   

#[2964] "Youth RoundTable Meeting!!! (@ FCC United Church of Christ Of San Bernardino) [pic]: http://t.co/zefMp4QLeC" 



# Answer:
#  1) They all have "pic"
#  2) They're all "sad" because of ]:

# Solution:
#  Remove ]: from sad classifier
# I think this problem is from foursquare. many of the tweets start with "I'm at..."

# Magnitude:
# There WERE 52498 tweets in our sad_tweets_2014 training data.
# Of these, there were 2600 (5%) junk tweets that had "[pic]:" in them

# Note that the primary reason this glitch was discovered was by looking at the head
# of freq.all:

# > head(freq.all,25)
# word freq.x freq.y diff      ndsi
# 2937      pic   2600    155 2445 0.8120226  <--- this is the tip off!
# 3913    thank    120   1976 1856 0.7891156
# 3318      sad   1277     59 1218 0.7650754
# 3914   thanks    177   2137 1960 0.7626459
# 401  birthday    194   1698 1504 0.7001862
# 1724    happy    346   2458 2112 0.6901961
# 2544     miss   2380    453 1927 0.6238265
# 1634    great    172   1099  927 0.6070727
# 4354     wish   1008    182  826 0.5712310
# 4302  welcome     24    414  390 0.5619597
# 3630    sorry    771    140  631 0.5407027
# 4099      ugh    393     32  361 0.5301028
# 1888    hurts    317     10  307 0.5265866
# 2790   others    657    122  535 0.5169082
# 4261     want   2034    564 1470 0.5150666
# 4425      yay     28    357  329 0.5132605
# 124   amazing     93    552  459 0.5094340
# 1603     good    702   2423 1721 0.5090210
# 4260    wanna   1135    286  849 0.5062612
# 256   awesome     65    427  362 0.4839572
# 1739     hate    696    162  534 0.4793537
# 1580     glad     26    304  278 0.4744027
# 1317  excited     64    409  345 0.4732510
# 1420  finally     73    434  361 0.4731324
# 1881   hungry    280     24  256 0.4571429