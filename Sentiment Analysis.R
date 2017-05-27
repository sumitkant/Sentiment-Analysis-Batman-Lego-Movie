#Loading the data
setwd('D:/Data Analyst/Own Data/Sentiment Analysis Batman Lego Movie/')
library(readr)
lbm = read_csv("tweets.csv")
windowsFonts(Lato.Regular = windowsFont('Lato Regular'))

#loading libraries
library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library(DT)
library(ggplot2)

#extracting relevant data
r1 = as.character(lbm$text)

#Data Preprocessing
sample = sample(r1, (length(r1)))
corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_up <- colSums(as.matrix(dtm_up))


#Calculating Sentiments - Approach 1
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

# plotting sentiments

## Positive Sentiments
ggplot(aes(x = freq_up, y = text, label = freq_up, color=freq_up), 
       data = sent_pos_up[sent_pos_up$freq_up>5,]) + 
  geom_point(size=4) +
  geom_text(size=3, position = position_nudge(x = 4), family = "Lato.Regular") +
  theme_gray(base_family = "Lato.Regular") +
  ylab("Positive Sentiments") +
  xlab("Frequencies")

## Wordcloud positive sentiments
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=5,colors=brewer.pal(6,"Dark2"))

## Negative Sentiments
ggplot(aes(x = freq_up, y = text, label = freq_up, color=freq_up), 
       data = sent_neg_up[sent_neg_up$freq_up>5,]) + 
  geom_point(size=4) +
  geom_text(size=3, position = position_nudge(x = 4), family = "Lato.Regular") +
  theme_gray(base_family = "Lato.Regular") +
  ylab("Negative Sentiments") +
  xlab("Frequencies")

## Wordcloud negative sentiments
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_neg_up$text,sent_neg_up$freq, min.freq=5,colors=brewer.pal(6 ,"Dark2"))

#Approach 2 - using the 'syuzhet' package
text = as.character(lbm$text)
##removing Retweets
some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
##let's clean html links
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
##let's remove people names
some_txt<-gsub("@\\w+","",some_txt)
##let's remove punctuations
some_txt<-gsub("[[:punct:]]"," ",some_txt)
##let's remove number (alphanumeric)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)  

#visual
library(ggplot2) # Data visualization
library(syuzhet)
mysentiment<-get_nrc_sentiment((some_txt))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")


sentis <- as.data.frame(colSums(mysentiment))
ggplot(data = sentis,
       aes(x = xAxis , 
           y = yAxis,  
           fill = xAxis, 
           label = yAxis)) + 
  geom_bar(stat = "identity") +
  geom_text(position = position_nudge(y = 30), family = "Lato.Regular") +
  xlab("Sentiments") +
  ylab("Count of sentiments") +
  ggtitle("Sentiments for Lego Batman movie") +
  theme_gray(base_family = "Lato.Regular")+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12,face="bold"))
 

ggsave(filename = "Sentiments.jpg", width=9, height = 6)
