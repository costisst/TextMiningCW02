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