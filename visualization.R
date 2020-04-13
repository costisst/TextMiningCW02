# load the libraries
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(devtools)
library(stringi)
library(wordcloud)
library(scales)
library(sos)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)


##############################################################################
##############################################################################
#                                11
##############################################################################
##############################################################################

library(syuzhet)
library(plotly)

#import your dataset to analyse,
#ensure it is in the same directory as your code,
#otherwise you need to add the path
# remove emojis or special characters
tweet_clean = gsub('<.*>', '', enc2native(tweets.df))

emotions <- get_nrc_sentiment(tweet_clean)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

emo_sum <- emo_sum[1:8,]
emo_sum$percent<-(emo_sum$count/sum(emo_sum$count))*100

#Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""),  yaxis = list(title = "Emotion count"),
         showlegend=FALSE,title="Distribution of emotion categories") %>%
  layout(yaxis = list(ticksuffix = "%"))



##############################################################################
##############################################################################
#                                10
##############################################################################
##############################################################################


library(scales)
#Reading the Lexicon positive and negative words
pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

#function to calculate sentiment score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}
#sentiment score
scores_twitter <- score.sentiment(tweets.df, pos.txt, neg.txt, .progress='text')


View(scores_twitter)

#Summary of the sentiment scores
summary(scores_twitter)

scores_twitter$score_chr <- ifelse(scores_twitter$score < 0,'Negtive', ifelse(scores_twitter$score > 0, 'Positive', 'Neutral'))


View(scores_twitter)


#Convert score_chr to factor for visualizations
scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)
names(scores_twitter)[3]<-paste("Sentiment")  

#plot to show number of negative, positive and neutral comments
Viz1 <- ggplot(scores_twitter, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
Viz1






##############################################################################
##############################################################################
#                                Extra Visualization
##############################################################################
##############################################################################

# Most frequent words found
## SOS replace tweet.df with tweets_organic$tweet

tweets <- tweets_organic %>%
  dplyr::select(tweet) %>%
  unnest_tokens(word, tweet)
tweets <- tweets %>%
  anti_join(stop_words)


tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +
  labs(y = "Count",x = "Unique words",title = "Most frequent words found", subtitle = "Stop words removedt")