# CSP 571 Project
# Twitter Analysis: Data Exploration

# First, set working directory to Source File Location

myData <- read.csv('../data collection/Data/ModelData/modelData.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
accounts <- read.csv('../data collection/Data/ModelData/accountsComplete.csv',header = T,stringsAsFactors = F,sep = ';')
followers<- read.csv('../data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')

missing <- tapply(myData$nTweets,myData$Account,function(x) sum(is.na(x))) 
missing[missing != 0]

#TODO pensar que hacer con estas cuentas
myData[is.na(myData)] <- 0


library(dplyr)
fullData <- merge(myData,accounts,by = 'Account',sort = F)
fullData <- select(fullData,-location)


library(lubridate)

formatdate <- function(d){
  d <- as.character(d)
  d <- gsub('X','',d)
  d <- gsub('\\.','-',d)
  return(d)
}


fullData$date <- apply(fullData['date'],formatdate,MARGIN = 1)
fullData$date <- ymd(fullData$date)
fullData['DoW'] <- wday(fullData$date,label = TRUE)
fullData['IsWeekend'] <- ifelse(fullData$DoW %in% c('Sat','Sun'),1,0)
fullData$IsWeekend <- as.factor(fullData$IsWeekend)


#tweetsEvolution_all.pdf

pdf('tweetsEvolution_all.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = fullData, aes(x=date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category)

dev.off()



fullData <- fullData[fullData$Account != 'TV_TradingIdeas',]

pdf('tweetsEvolution_noOutlier.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = fullData, aes(x=date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category,scales = "free_y")

dev.off()

pdf('tweetsEvolution_noOutlier_freeY.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = fullData, aes(x=date,y=nTweets,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category)

dev.off()


#Seasonality analysis

pdf('WeekEnd_seasonality.pdf',paper = 'USr',width = 11,height=8.5)

ggplot(data = fullData, aes(factor(0),nTweets,group = IsWeekend))+
  geom_boxplot(aes(colour = IsWeekend))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  facet_wrap(~Category)
dev.off()

pdf('WeekEnd_seasonality_freeY.pdf',paper = 'USr',width = 11,height=8.5)
ggplot(data = fullData, aes(factor(0),nTweets,group = IsWeekend))+
  geom_boxplot(aes(colour = IsWeekend))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  facet_wrap(~Category, scales = "free_y")
dev.off()

#Followers

pdf('followersEvolution_freeY.pdf',paper = 'USr',width = 11,height=8.5)
ggplot(data = fullData, aes(x=date,y=Followers,group = Account))+
  geom_line(aes(color=Account))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Category,scales = "free_y")

dev.off()

#TODO include Tv-... ?
initialData <- fullData[fullData$date == fullData$date[1],]

ggplot(initialData, aes(x = Followers)) +
  geom_histogram()

ggplot(initialData, aes(factor(0),Followers)) +
  geom_boxplot()

View(initialData[c('Account','Followers')])







# Messing around to get pChange in followers vs. Followers to bin 

fullDataOrdered <- fullData[order(fullData$date),]

startday <-  fullData$date[1]
endday <- fullData$date[nrow(fullData)]

fullDataOrdered$pChange_followers[which(fullDataOrdered$date=="2018-02-24")]<-((fullDataOrdered$Followers[which(fullDataOrdered$date=="2018-02-24")]-followers$X2018.02.23)/followers$X2018.02.23)*100

a1 <- fullDataOrdered$Followers[which((fullDataOrdered$date> (startday-1)) & (fullDataOrdered$date< endday))]
b1 <- fullDataOrdered$Followers[which((fullDataOrdered$date> startday) & (fullDataOrdered$date<(endday+1)))]
fullDataOrdered$pChange_followers[fullDataOrdered$date>startday] <- (b1 - a1)/a1*100

avgFollowers <- tapply(fullDataOrdered$Followers, fullDataOrdered$Account, mean)
avgpChange <- tapply(fullDataOrdered$pChange_followers, fullDataOrdered$Account, mean)

graphDF <- data.frame(followers=avgFollowers, change=avgpChange)
graphDF<-graphDF[complete.cases(graphDF),]
summary(graphDF)
# d1 <- fullData$date[1]
# d2 <- d1+1
# 
# fullDataOrdered <- fullData[order(fullData$date),]
# 
# pChangeFirst <- (fullDataOrdered$Followers[fullDataOrdered$date==d2]-fullDataOrdered$Followers[fullDataOrdered$date==d1])/fullDataOrdered$Followers[fullDataOrdered$date==d1]
# change <- (fullDataOrdered$Followers[fullDataOrdered$date==d2]-fullDataOrdered$Followers[fullDataOrdered$date==d1])
# avgChange <- 
# 
# followerFirst <- fullDataOrdered$Followers[fullDataOrdered$date==d1]

# g <- data.frame(change=pChangeFirst, followers=followerFirst)
# g2 <- data.frame(change=change, followers=followerFirst)

library(ggplot2)
ggplot(data=graphDF, aes(x=followers,y=change))+
  geom_point(stat="identity")+
  xlim(0,7.5e6)

library(cluster)
library(factoextra)

pdf('ClusterErrors.pdf',paper = 'USr',width = 11,height=8.5)
fviz_nbclust(graphDF, kmeans, method="wss")
dev.off()

k <- kmeans(graphDF, centers=4)

pdf('Clusters.pdf',paper = 'USr',width = 11,height=8.5)
fviz_cluster(k, data=graphDF)
dev.off()

c <- k$cluster

df <- data.frame(Account=names(c),Cluster=c)
# write.csv(df, file="\AccountClusters.csv")

merged <- merge(fullDataOrdered, df, by='Account', all.x = TRUE)
stopifnot(nrow(fullDataOrdered)==nrow(merged))

pdf('followersEvolution2_freeY.pdf',paper = 'USr',width = 11,height=8.5)
ggplot(data = merged, aes(x=date,y=Followers,group = Account))+
  geom_line(aes(color=Category))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none')+
  facet_wrap(~Cluster,scales = "free_y")
dev.off()