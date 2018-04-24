# CSP 571 Project
# Twitter Analysis: Data Exploration

# First, set working directory to Source File Location

myData <- read.csv('../data collection/Data/ModelData/modelData2.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
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



# Bin followers to create categorical variable for type of account: small, medium, large, huge

fullDataOrdered <- fullData[order(fullData$date),]
dataTrain <- fullDataOrdered[which(fullDataOrdered$date<="2018-04-09"),]

startday <-  fullData$date[1]
endday <- fullData$date[nrow(fullData)]

dataTrain$pChange_followers[which(dataTrain$date=="2018-02-24")]<-((dataTrain$Followers[which(dataTrain$date=="2018-02-24")]-followers$X2018.02.23)/followers$X2018.02.23)*100

a1 <- dataTrain$Followers[which((dataTrain$date> (startday-1)) & (dataTrain$date< endday))]
b1 <- dataTrain$Followers[which((dataTrain$date> startday) & (dataTrain$date<(endday+1)))]
dataTrain$pChange_followers[dataTrain$date>startday] <- (b1 - a1)/a1*100

avgFollowers <- tapply(dataTrain$Followers, dataTrain$Account, mean)
avgpChange <- tapply(dataTrain$pChange_followers, dataTrain$Account, mean)

graphDF <- data.frame(followers=avgFollowers, change=avgpChange)
graphDF<-graphDF[complete.cases(graphDF),]
summary(graphDF)


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

# Clusters from low to high number of followers are: 3<1<2<4

c <- k$cluster

df <- data.frame(Account=names(c),Cluster=c)
write.csv(df, file="AccountClusters.csv")

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