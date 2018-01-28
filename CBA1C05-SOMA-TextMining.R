load("twitter authentication.Rdata")
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")

library (plyr)
library (stringr)
library (ggplot2)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores=laply(sentences, function(sentence, pos.words, neg.words){
    sentence = gsub('[[:punct:]]', '', sentence) #Punctuation characte
    sentence = gsub('[[:cntrl:]]', '', sentence) #Control characters
    sentence = gsub('\\d+', '', sentence) # \\d ==> Digits
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+') #\\s ==> space
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Load sentiment word lists
hu.liu.pos = scan('~/tmp/positive-words.txt', what='character', comment.char=';') #All Comments are ignored with <;> in front in the text file
hu.liu.neg = scan('~/tmp/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = hu.liu.pos
neg.words = hu.liu.neg

#Import 3 csv
DatasetRangers <- read.csv("~/tmp/RangersTweets.csv")
DatasetRangers$text<-as.factor(DatasetRangers$text)

DatasetAthletics <- read.csv("~/tmp/AthleticsTweets.csv")
DatasetAthletics$text<-as.factor(DatasetAthletics$text)

DatasetMLB <- read.csv("~/tmp/MLBTweets.csv")
DatasetMLB$text<-as.factor(DatasetMLB$text)


#Score all tweets 
Rangers.scores = score.sentiment(DatasetRangers$text, pos.words,neg.words, .progress='text')
Athletics.scores = score.sentiment(DatasetAthletics$text, pos.words,neg.words, .progress='text')
MLB.scores = score.sentiment(DatasetMLB$text, pos.words,neg.words, .progress='text')

#Export scores to csv file, please take note, you need to submit  
#RangerScore.csv to OLIVE 
path<-"~/tmp/"
write.csv(Rangers.scores,file=paste(path,"RangersScores.csv",sep=""),row.names=TRUE)
write.csv(Athletics.scores,file=paste(path,"AthleticsScores.csv",sep=""),row.names=TRUE)
write.csv(MLB.scores,file=paste(path,"MLBScores.csv",sep=""),row.names=TRUE)

Rangers.scores$Team = 'Rangers'
Athletics.scores$Team = 'Athletics'
MLB.scores$Team = 'MLB'



#Visualizaing  

############################# 
#Chunk -5- Visualizing         
#############################

hist(Rangers.scores$score)
qplot(Rangers.scores$score, binwidth=0.5)

hist(Athletics.scores$score)
qplot(Athletics.scores$score, binwidth=0.5)

hist(MLB.scores$score)
qplot(MLB.scores$score, binwidth=0.5)


#################################
#Chunk -6- Comparing 3 data sets	              
#################################

all.scores = rbind(Rangers.scores, Athletics.scores, MLB.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=Team), width=0.5) +
  facet_grid(Team~.) + # make a separate plot for each hashtag
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

