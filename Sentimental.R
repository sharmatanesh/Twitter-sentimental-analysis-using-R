#connect all libraries
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
api_key <- '***************'
api_secret <- '*************************************'
access_token <- "********************"
access_token_secret <- "*************************"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#the function for extracting and analyzing tweets
searchSentimentInTwitter <- function(searchcity)
{
  #extact tweets and create storage file
  
  list <- searchTwitter(searchcity, n=100,lang="en")
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d')
  if (file.exists(paste(searchcity, '_tweetInfo.csv'))==FALSE) write.csv(df, file=paste(searchcity, '_tweetInfo.csv'), row.names=F)
  
  #merge the last extraction with storage file and remove duplicates
  stack <- read.csv(file=paste(searchcity, '_tweetInfo.csv'))
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.csv(stack, file=paste(searchcity, '_tweetInfo.csv'), row.names=F)
  
  #tweets evaluation function
  score.sentiment <- function(sentences, positive.words, negative.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, positive.words, negative.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
      sentence<-str_replace_all(sentence,"[^[:graph:]]", " ") 
      sentence <- tolower(sentence)
      word.list <- str_split(sentence, '\\s+')
      words <- unlist(word.list)
      positive.matches <- match(words, positive.words)
      negative.matches <- match(words, negative.words)
      positive.matches <- !is.na(positive.matches)
      negative.matches <- !is.na(negative.matches)
      score <- sum(positive.matches) - sum(negative.matches)
      return(score)
    }, positive.words, negative.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  positive <- scan('C:/positive-words.txt', what='character', comment.char=';') #folder with positiveitive dictionary
  negative <- scan('C:/negative-words.txt', what='character', comment.char=';') #folder with negativeative dictionary
  positive.words <- c(positive, 'safe')
  negative.words <- c(negative, 'accident','injury','dash','casualty','death','fatality')
  Dataset <- stack
  Dataset$text <- as.factor(Dataset$text)
  scores <- score.sentiment(Dataset$text, positive.words, negative.words, .progress='text')
  write.csv(scores, file=paste(searchcity, '_Twitscores.csv'), row.names=TRUE) #save evaluation results
  
  #total score calculation: positiveitive / negativeative / neutral
  stat <- scores
  stat$created <- stack$created
  stat$created <- as.Date(stat$created)
  stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
  by.tweet <- group_by(stat, tweet, created)
  by.tweet <- summarise(by.tweet, number=n())
  write.csv(by.tweet, file=paste(searchcity, '_SentimentStatistics.csv'), row.names=TRUE)
  
  #chart
  ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
    geom_point(aes(group=tweet, color=tweet), size=4) +
    theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
    #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
    ggtitle(searchcity)
  
  ggsave(file=paste(searchcity, '_SentimentStatGraph.jpeg'))
  
}
searchSentimentInTwitter("Dublin") #enter keyword


