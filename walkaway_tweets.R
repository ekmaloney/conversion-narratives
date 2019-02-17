#########################
# Emily Maloney #
# 2/16/2019 #
#######################

#libraries
library(tidyverse)
library(rtweet)
library(foreign)

#tokens
app_name<-"<YOUR INFO>"
consumer_key<-"<YOUR INFO>"
consumer_secret<-"<YOUR INFO>"
access_token<-"<YOUR INFO>"
access_token_secret<-"<YOUR INFO>"

create_token(app=app_name, consumer_key=consumer_key, consumer_secret=consumer_secret,
             access_token = access_token, access_secret = access_token_secret)

#download tweets
walkaway_tweets<-search_tweets("#WalkAway", n = 5000, include_rts = FALSE)

#save object
saveRDS(walkaway_tweets, "walkaway_tweets.rds")

#loop to get tweets from every user in walkaway_tweets df
users <- unique(walkaway_tweets$screen_name)

user_tweets<-as.data.frame(NULL)

for(i in 834:length(users)){
  
  #pull tweets
  tweets<-get_timeline(users[i], n=100)
  
  #populate dataframe
  user_tweets<-rbind(user_tweets, tweets)
  
  #pause for five seconds to further prevent rate limiting
  Sys.sleep(10)
  
  #print number/iteration for debugging/monitoring progress
  print(i)
}

saveRDS(user_tweets, "user_tweets.rds")
