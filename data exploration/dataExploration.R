
tweets <- read.csv('../data collection/Data/ModelData/historicTweets.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
accounts <- read.csv('../data collection/Data/ModelData/accountsComplete.csv',header = T,stringsAsFactors = F,sep = ';')
followers<- read.csv('../data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')

tweets[is.na(tweets)] <- 0


#library(dplyr)
tweetsPerCategory <- merge(tweets,accounts,by = 'Account',sort = F)
tweetsPerCategory <- select(tweetsPerCategory,-location)


#library(ggplot2)
#library(reshape)
tweetGraph <- melt(tweetsPerCategory,id = c('Account','Category'))
names(tweetGraph) <- c('Account','Category','Date','nTweets')


#tweetsEvolution_all.pdf

pdf('tweetsEvolution_all.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = tweetGraph, aes(x=Date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category)

dev.off()



tweetGraph <- tweetGraph[tweetGraph$Account != 'TV_TradingIdeas',]

pdf('tweetsEvolution_noOutlier.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = tweetGraph, aes(x=Date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category,scales = "free_y")

dev.off()

pdf('tweetsEvolution_noOutlier_freeY.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = tweetGraph, aes(x=Date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category)

dev.off()


