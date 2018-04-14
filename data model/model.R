# We are going to start uploading the file that has all the information necessary for the analysis

#setwd("../project571/data model")
data<-read.csv('C:/Users/Carru/SoftwareRepositories/project571/data collection/Data/ModelData/modelData.csv', header=T, sep=";", na.strings='Null')
#TODO: No me deja acortar el path (investigar)

#We start observing the data
index<-which(complete.cases(data)==FALSE)
data[index, ]
#There are 3 accounts that stopped tweeting for a period of time/stopped functioning
#TODO: discuss what to do with these accounts

#Create variable isWeekend:
data[, "date"]<-as.Date(data[, "date"])
library('lubridate')
day<-wday(data[,"date"], week_start=1) #starts on Monday
data[, "isWeekend"]<-0
data$isWeekend[which(day>=5)]<-1 #Friday weekend?

#Change in the number of followers
#TODO: remove the line that gets the number of followers for 23/02 when we put these values in the 
#modelData.csv

followers<- read.csv('C:/Users/Carru/SoftwareRepositories/project571/data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "change_followers"]<-0
data$change_followers[which(data$date=="2018-02-24")]<-data$Followers[which(data$date=="2018-02-24")]-followers$X2018.02.23
