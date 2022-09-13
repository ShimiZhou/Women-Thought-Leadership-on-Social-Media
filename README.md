# Women-Thought-Leadership-on-Social-Media
Applied sentiment analysis and natural language processing method with Python and R. 
library(rtweet)
library(dplyr)
install.packages("twitteR")
library(twitteR)
appname <- "Women Thought Leadership"

# Store API keys 
api_key <- "####"
api_secret_key <- "####"
access_token <- "####"
access_token_secret <- "####"

# Searching for tweets: can only extract from the past 6 to 9 days, the last 3200 tweets
Susan_tweets <- get_timeline("@SusanWojcicki", n= 3200)

extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}

dat = head(extract.hashes(vec1),50)
dat2 = transform(dat,tag = reorder(tag,freq))

# Remove retweets
Susan_tweets_organic <- Susan_tweets[Susan_tweets$is_retweet==FALSE, ] 
# Remove replies
Susan_tweets_organic <- subset(Susaan_tweets_organic, is.na(Susan_tweets_organic$reply_to_status_id)) 

#Looking at the variables: favorite_count (i.e. the number of likes) or retweet_count (i.e. the number of retweets)

Susan_tweets_organic <- Susan_tweets_organic %>% arrange(-favorite_count)
Susan_tweets_organic[1,5]
Susan_tweets_organic <- Susan_tweets_organic %>% arrange(-retweet_count)
Susan_tweets_organic[1,5]

#Show the Ratio of REPLIES/RETWEETS/ORGANIC TWEETS
# Keeping only the retweets
Susan_retweets <- Susan_tweets[Susan_tweets$is_retweet==TRUE,]

# Keeping only the replies
Susan_replies <- subset(Susan_tweets, !is.na(Susan_tweets$reply_to_status_id))

# Creating a data frame
library(dplyr)

data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856,192,120)
  )

# Adding columns 
library(ggplot2)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

# Rounding the data to two decimal points
data <- round_df(data, 2)

# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

#The frequency tweets by year
colnames(Bill_tweets)[colnames(Bill_tweets)=="screen_name"] <- "Twitter_Account"
ts_plot(dplyr::group_by(Bill_tweets, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Bill Gates",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Wordcloud of the most frequent used words 

install.packages("wordcloud")

library(tm)
library(slam)
library(wordcloud)
Susan_tweets_organic$hashtags <- as.character(Susan_tweets_organic$hashtags)
Susan_tweets_organic$hashtags <- gsub("c\\(", "", Susan_tweets_organic$hashtags)
set.seed(1234)
wordcloud(Bill_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(1234)
wordcloud(Bill_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

#sentiment analysis

install.packages("syuzhet")
library(syuzhet)

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(Bill_tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",Bill_tweets)

# removing mentions, in case needed
tweets <-gsub("@\\w+","",Bill_tweets)
ew_sentiment<-get_nrc_sentiment((Bill_tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

install.packages("tsbox")
library(tsbox)
library(ggplot2)
library(tidytext)
library(tidyverse)

  #Most retweeted tweet
Susan_tweets %>% 
  arrange(-retweet_count) %>%
  slice(1) %>% 
  select(created_at, text, retweet_count)
  #Tweets sample
  Susan_tweets %>% 
    sample_n(5) %>%
    select(created_at, text, favorite_count, retweet_count)

# Converting tweets to ASCII to trackle strange characters
  tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
  tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
  
# Removing mentions, in case needed
  tweets <-gsub("@\\w+","",tweets)
  ew_sentiment<-get_nrc_sentiment((tweets))
  sentimentscores<-data.frame(colSums(ew_sentiment[,]))
  names(sentimentscores) <- "Score"
  sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
  rownames(sentimentscores) <- NULL
  ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
    geom_bar(aes(fill=sentiment),stat = "identity")+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("Scores")+
    ggtitle("Total sentiment based on scores")+
    theme_minimal()
