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

#TODO: Hacer la función genérica cuando tengamos todos los datos
startday <-  data$date[1]
endday <- data$date[nrow(data)]

a1 <- data$Followers[which((data$date> (startday-1)) & (data$date< endday))]
b1 <- data$Followers[which((data$date> startday) & (data$date<(endday+1)))]

data$change_followers[data$date>startday] <- b1 - a1

#We create a category column
account<-read.csv('C:/Users/Carru/SoftwareRepositories/project571/data collection/Data/ModelData/accountsComplete.csv', header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "category"]<-"a"
data$category[which(data$Account==account$Account)]<-account$Category
data$category<-as.factor(data$category)

#Regression Model
#MODEL------------------------------------------------------------------------------------------------------
#We start observing the data
index<-which(complete.cases(data)==FALSE)
data[index, ]
datano <- data[-index,]
which(is.na(datano))


xVars <- names(datano)
xVars <- xVars[-8]
xVars <- xVars[-1]
xVars <- xVars[-9]
#xVars <- xVars[-7] #removing Followers
targetVar <-  "change_followers"

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

#sum(datawithoutNULL$change_followers>0)

# Let's divide into training and testing
library('caret')
library('lattice')
library('ggplot2')
library('MASS')
library('olsrr')
library('stats')

set.seed(1234)
inTrain <- createDataPartition(y = datano[,targetVar], list = FALSE, p = 0.8)
train <- datano[inTrain,]
test <- datano[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(datano))
sum(train$change_followers)/nrow(train)
sum(test$change_followers)/nrow(test)

modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model <- lm(modelForm, data = train)
summary(model)

#model backwards

xVars2 <- c("Followers", "isWeekend")
targetVar <-  "change_followers"

modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars2, includeIntercept = TRUE)
model <- lm(modelForm, data = train)
summary(model)

#TODO: look this fuction better
#back<-ols_step_backward_aic(model, details=TRUE) 

smodel<-step(model, direction="both")
summary(smodel)
