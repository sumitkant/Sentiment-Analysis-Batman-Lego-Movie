# Reading tweets from Twitter API

setwd('D:/Data Analyst/Own Data/Sentiment Analysis Batman Lego Movie/')

library(twitteR)
library(ROAuth)
require(twitteR)
require(RCurl)

# Save all the keys in variables
consumerKey <- "G8iPPnWlNsSJpbHMHAb0JLoHh"
consumerSecret <- "w3KTC2TSBRKw9CShDeoWThFh96EBrH633X7j7zToNWtulaqhjV"
accessKey <- "449531980-uoNSdT1P5xgmeZfIyhrKjRvxyiFV7yBvJ0RqbWpZ"
accessSecret <- "BQOMQGbzUQmJi1q7pgZphIbF7MkHhqiwQTp9r7BmdJWLq"

setup_twitter_oauth(consumerKey,consumerSecret,accessKey,accessSecret)

search.string = "lego batman movie"
tweets<-searchTwitter(search.string, 3000, lang ="en")

# Saving tweetts in a csv file
name<-file(description="tweets.csv",open="w")
write.csv(twListToDF(tweets),file=name)
close(con = name)


