# Load the libraries
library(rtweet)
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(stringi)
library(wordcloud)
library(tm)
library(devtools)
library(reshape)
library(qdapRegex)
library(qdap)
library(RColorBrewer)

# Extract 5000 tweets on "Parasite And Movie" in English
# Apply the '-filter' to remove retweets,replies and quote
tweets_parasite<- search_tweets("Parasite AND movie
                                -filter:retweets
                                -filter:quote
                                -filter:replies", n=5000, lang ="en")

# Order the tweets based on the favorite_count and get the 1000 most popular
tweets_parasite_ordered<-tweets_parasite[order(tweets_parasite$favorite_count, decreasing = TRUE),]
tweets_parasite_selection<-head(tweets_parasite_ordered,1000)

# Extract only the text of the tweets
tweets_parasite.df <- tweets_parasite_selection$text


# Extract 5000 tweets on "Joker And Movie" in English
# Apply the '-filter' to remove retweets,replies and quote
tweets_joker<- search_tweets("Joker AND movie
                                -filter:retweets
                                -filter:quote
                                -filter:replies", n=5000, lang ="en")

# Order the tweets based on the favorite_count and get the 1000 most popular
tweets_joker_ordered<-tweets_joker[order(tweets_joker$favorite_count, decreasing = TRUE),]
tweets_joker_selection<-head(tweets_joker_ordered,1000)

# Extract only the text of the tweets
tweets_joker.df <- tweets_joker_selection$text

# Create a time series object for parasite
parasite_ts<-ts_data(tweets_parasite_selection, by='hours')
names(parasite_ts)<- c("time","parasite_n")

# Create a time series object for joker
joker_ts<-ts_data(tweets_joker_selection, by='hours')
names(joker_ts)<- c("time","joker_n")

# Merge the two time series objects
merged_df<-merge(parasite_ts, joker_ts, by ='time', all=TRUE)
melt_df<-melt(merged_df, na.rm=TRUE, id.vars='time')

# Plot frequency of tweets on Joker and Parasite
ggplot(data = melt_df,
       aes(x = time, y = value, col = variable))+
       geom_line(lwd = 0.8)

######################################################################
# Processing / Cleaning


# Remove the tweet text URLs
tweets_parasite_clean<- rm_twitter_url(tweets_parasite.df)
tweets_joker_clean<- rm_twitter_url(tweets_joker.df)

# Remove special char,punctuations & number
tweets_parasite_clean<-gsub("[^A-Za-z]"," ",tweets_parasite_clean.df)
tweets_joker_clean<-gsub("[^A-Za-z]"," ",tweets_parasite_clean.df)

# Convert to corpus
tweets_parasite_corpus<-tweets_parasite_clean %>%
                     VectorSource() %>%
                     Corpus()
tweets_joker_corpus<-tweets_joker_clean %>%
                     VectorSource() %>%
                     Corpus()
# LowerCase
tweets_parasite_corpus<-tm_map(tweets_parasite_corpus, tolower)
tweets_joker_corpus<-tm_map(tweets_joker_corpus, tolower)

# Remove Stopwords
tweets_parasite_corpus<-tm_map(tweets_parasite_corpus, removeWords, stopwords("english"))
tweets_joker_corpus<-tm_map(tweets_joker_corpus, removeWords, stopwords("english"))

# Remove unecessary spaces
tweets_parasite_corpus<-tm_map(tweets_parasite_corpus, stripWhitespace)
tweets_joker_corpus<-tm_map(tweets_joker_corpus, stripWhitespace)
tweets_joker_corpus[[4]]$content


###########################################################################
# Extract popular terms


# Extract top 100 terms from corpus
term_counts_parasite <- freq_terms(tweets_parasite_corpus,100)
term_counts_joker <- freq_terms(tweets_joker_corpus,100)

# Removing custom stop words
custom_stop <- c("movie","parasite","s","t","m","y","re","get","re",
                       "also","can","amp","one","like","will","go",
                       "know","us","just","now","ve","got","haven",
                       "won","don","ho","get","say","even","didn")
tweets_parasite_corpus<- tm_map(tweets_parasite_corpus,removeWords,custom_stop)
tweets_joker_corpus<- tm_map(tweets_joker_corpus,removeWords,custom_stop)

term_counts_parasite <- freq_terms(tweets_parasite_corpus,50)
term_counts_joker <- freq_terms(tweets_joker_corpus,50)

# Create a subset dataframe
parasite_50<- subset(term_counts_parasite, FREQ > 50)
joker_50<-subset(term_counts_joker, FREQ > 50)

# Create a bar plot of frequent items
ggplot(parasite_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
       geom_bar(stat='identity', fill = 'blue') +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(joker_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
       geom_bar(stat='identity', fill = 'red') +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a WordCloud
wordcloud(tweets_parasite_corpus, min.freq = 20, colors = 'blue',
          scale = c(3,0.5), random.order = FALSE)

wordcloud(tweets_joker_corpus, min.freq = 20, colors = 'red',
          scale = c(3,0.5), random.order = FALSE)

# More colorful WordCloud
wordcloud(tweets_parasite_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)

wordcloud(tweets_joker_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)
############################################################################
# Sentiment Analysis
library(syuzhet)

sa_parasite.value<- get_nrc_sentiment(tweets_parasite_selection$text)
sa_joker.value<- get_nrc_sentiment(tweets_joker_selection$text)

# Calculating the sum of sentiment scores
score_parasite <- colSums(sa_parasite.value[,])
score_joker <- colSums(sa_joker.value[,])

# Convert into a dataframe
score_parasite_df <- data.frame(score_parasite)
score_joker_df <- data.frame(score_joker)

sa_parasite.score <- cbind(sentiment = row.names(score_parasite_df),
                           score_parasite_df, row.names = NULL)

sa_joker.score <- cbind(sentiment = row.names(score_joker_df),
                           score_joker_df, row.names = NULL)

ggplot(data = sa_parasite.score, aes(x = sentiment, y = score_parasite,
       fill = sentiment)) +
       geom_bar(stat = "identity") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = sa_joker.score, aes(x = sentiment, y = score_joker,
                                     fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  






