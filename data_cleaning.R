# load the libraries
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)

# import your data set to analyse,
# ensure it is in the same directory as your code, otherwise you need to add the path
Dataset2 <- read.csv("D:\\Msc_AI_UoM\\Semester 2\\Text mining\\cw02\\my_data.csv")
tweets.df <- Dataset2$tweet

# convert text to lowercase
tweets.df<-tolower(tweets.df)

# get rid of problem characters
tweets.df <- sapply(tweets.df,function(row) iconv(row, "latin1", "ASCII", sub=""))

# remove punctuation, digits, special characters etc
# tweets.df = gsub("&amp", "", tweets.df)
# tweets.df= gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df)
# tweets.df = gsub("@\\w+", "", tweets.df)
# tweets.df= gsub("[[:punct:]]", "", tweets.df)
# tweets.df = gsub("[[:digit:]]", "", tweets.df)
# tweets.df = gsub("http\\w+", "", tweets.df)
# tweets.df = gsub("[ \t]{2,}", "", tweets.df)
# tweets.df= gsub("^\\s+|\\s+$", "", tweets.df) 

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

# get rid of unnecessary spaces
tweets.df <- str_replace_all(tweets.df," "," ")

# get rid of URLs
tweets.df <- str_replace_all(tweets.df, "http://t.co/[a-z,A-Z,0-9]*{8}","")

# take out the retweet header (there is only one)
tweets.df <- str_replace(tweets.df,"RT @[a-z,A-Z]*: ","")

# get rid of hashtags
tweets.df <- str_replace_all(tweets.df,"#[a-z,A-Z]*","")

# get rid of references to other screen names
tweets.df <- str_replace_all(tweets.df,"@[a-z,A-Z]*","")  

View(tweets.df)