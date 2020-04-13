# load the libraries
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(devtools)
library(stringi)
library(wordcloud)
library(hunspell)
library(textclean)
library(syuzhet)
library(plotly)
library(scales)

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

# import your data set to analyse,
Dataset2 <- read.csv('D:/Msc_AI_UoM/Semester 2/Text mining/cw02/my_data.csv')
tweets.df <- Dataset2$tweet

# get rid of problem characters
tweets.df <- sapply(tweets.df,function(row) iconv(row, "latin1", "ASCII", sub=""))

# Data cleaning
tweets.df = gsub('https\\S+\\s*', '', tweets.df) ## Remove URLs
tweets.df = gsub('http\\S+\\s*', '', tweets.df) ## Remove URLs
tweets.df = gsub('pic\\S+\\s*', '', tweets.df) ## Remove URLs
tweets.df = gsub('www.\\S+\\s*', '', tweets.df) ## Remove URLs
tweets.df = gsub('\\b+RT', '', tweets.df) ## Remove RT
tweets.df = gsub('#\\S+', '', tweets.df) ## Remove Hashtags
tweets.df = gsub('@\\S+', '', tweets.df) ## Remove Mentions
tweets.df = gsub('[[:cntrl:]]', '', tweets.df) ## Remove Controls and special characters
tweets.df = gsub("\\d", '', tweets.df) ## Remove Controls and special characters
tweets.df = gsub('[[:punct:]]', '', tweets.df) ## Remove Punctuations
tweets.df = gsub("^[[:space:]]*","",tweets.df) ## Remove leading whitespaces
tweets.df = gsub("[[:space:]]*$","",tweets.df) ## Remove trailing whitespaces
tweets.df = gsub(' +',' ',tweets.df) ## Remove extra whitespaces
tweets.df = gsub('<.*>', '', enc2native(tweets.df)) #remove emojis
tweets.df <- str_replace_all(tweets.df," "," ") # get rid of unnecessary spaces
tweets.df <- str_replace(tweets.df,"RT @[a-z,A-Z]*: ","") # take out the retweet header (there is only one)
tweets.df = stri_remove_empty(tweets.df, na_empty = FALSE)

#get all tweets as one string
oneTweet<-toString(tweets.df)
# get list of all words
words <- strsplit(oneTweet, " ")[[1]]

#get all slang/misspelled words
badWords<-hunspell(words)
badWords<-badWords[lapply(badWords,length)>0]
badWords<-tolower(badWords)

# convert text to lowercase
tweets.df<-tolower(tweets.df)

tweets.df = unique(tweets.df)

#split into groups as too large to remove all at once
group <- 100
n <- length(badWords)
r <- rep(1:ceiling(n/group),each=group)[1:n]
d <- split(badWords,r)

MyStopwords <- c("oscars","oscar","nominees","academyawards","theoscars","awards","movie", "night", "film", "carpet", "red","actor","actress")

# Additional data cleaning
for (i in 1:length(d)) {
  
  # remove punctuations
  tweets.df <- removePunctuation(tweets.df)
  # remove "bad words" and slang
  tweets.df <- removeWords(tweets.df, c(paste(d[[i]])))
  # remove stopwords
  tweets.df <- removeWords(tweets.df, stopwords("english"))
  # remove custom stop words
  tweets.df <- removeWords(tweets.df, MyStopwords)
  # remove all numbers
  tweets.df <- removeNumbers(tweets.df)
  # remove whirespaces
  tweets.df <- stripWhitespace(tweets.df)
  
}

# corpus will hold a collection of text documents
tweet_corpus <- Corpus(VectorSource(tweets.df)) 
# Create WordCloud
par(mar=c(1,1,1,1))
wordcloud(tweet_corpus, random.order=0.5,max.words=200, col=rainbow(50),min.freq = 5,  scale=c(4.0,0.99))


# Calculate and visualize the emotion bars
emotions <- get_nrc_sentiment(tweets.df)
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


# Calculate and visualize emotion bars

#Reading the Lexicon positive and negative words
pos <- readLines("D:/Msc_AI_UoM/Semester 2/Text mining/cw02/positive_words.txt")
neg <- readLines("D:/Msc_AI_UoM/Semester 2/Text mining/cw02/negative_words.txt")


#sentiment score
scores_twitter <- score.sentiment(tweets.df, pos.txt, neg.txt, .progress='text')


#Summary of the sentiment scores
summary(scores_twitter)

scores_twitter$score_chr <- ifelse(scores_twitter$score < 0,'Negtive', ifelse(scores_twitter$score > 0, 'Positive', 'Neutral'))

#Convert score_chr to factor for visualizations
scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)
names(scores_twitter)[3]<-paste("Sentiment")  

#plot to show number of negative, positive and neutral comments
score_visual <- ggplot(scores_twitter, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
score_visual