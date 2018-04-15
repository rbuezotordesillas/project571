#setwd("C:/Users/Carru/SoftwareRepositories/project571/data model")
data<-read.csv('../data collection/Data/ModelData/modelData.csv', header=T, sep=";", na.strings='Null')

#We start observing the data
index<-which(complete.cases(data)==FALSE)
#data[index, ]
#There are 3 accounts that stopped tweeting for a period of time/stopped functioning
#TODO: discuss what to do with these accounts


#Create variable isWeekend:
data[, "date"]<-as.Date(data[, "date"])
library('lubridate')
day<-wday(data[,"date"], week_start=1) #starts on Monday
data[, "isWeekend"]<-0
data$isWeekend[which(day>=5)]<-1 #We consider that the weekend starts on Friday


#Create variable change in number of followers

#TODO: remove the line that gets the number of followers for 23/02 when we put these values in the 
#modelData.csv
followers<- read.csv('../data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "change_followers"]<-0
data$change_followers[which(data$date=="2018-02-24")]<-data$Followers[which(data$date=="2018-02-24")]-followers$X2018.02.23

#TODO: change this when we have all the data
startday <-  data$date[1]
endday <- data$date[nrow(data)]

a1 <- data$Followers[which((data$date> (startday-1)) & (data$date< endday))]
b1 <- data$Followers[which((data$date> startday) & (data$date<(endday+1)))]
data$change_followers[data$date>startday] <- b1 - a1


#We create a category column
account<-read.csv('../data collection/Data/ModelData/accountsComplete.csv', header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "category"]<-"a"
data$category[which(data$Account==account$Account)]<-account$Category
data$category<-as.factor(data$category)


#We remove the Accounts that have NAs
i<-which((data$Account=="sportbible")|(data$Account=="fabulousanimals")|(data$Account=="Earth_Pics"))
cat('The percentage of removed data is:', (length(i)/dim(data)[1])*100) #So we can delete them
data_clean<-data[-i, ]

#-----------------------------------------------------------------------------------------------
#Correlation between variables
type<-function(a, funct){
  if(sum(sapply(a, funct))==0){
    return(0)
  }else{
    return(names(which(sapply(a, funct))))
  }
}  
numVar<-type(data_clean, is.numeric)
catVar<-type(data_clean, is.factor)
dateVar<-type(data_clean, is.Date)
stopifnot((length(numVar)+length(catVar)+length(dateVar))==ncol(data_clean))


library('corrplot')
corMatrix <- cor(data_clean[, numVar])
corrplot(corMatrix, method = 'number', diag = TRUE)
#From this matrix we see that the values show no relevant linear correlation except for pRTs and
#pMentions and Followers with change_followers

#TODO: Modify this in the models

#-----------------------------------------------------------------------------------------------
  #MODELS
#-----------------------------------------------------------------------------------------------
#Logistic Regression Model
  
#We are going to create a variable 1-Increase 0-Decrease or stayed the same
data_clean[, "logic_change"]<-0
data_clean$logic_change[which(data_clean$change_followers>0)]<-1

#We use stratified sampling to divide the data into training and test
library('caret')
library('lattice')
library('ggplot2')

set.seed(1234)
ind<-createDataPartition(y=data_clean$logic_change, list=FALSE, p=0.8)
train<-data_clean[ind,]
test<-data_clean[-ind,]
stopifnot(nrow(train) + nrow(test) == nrow(data_clean))

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

#TODO: Remove highly correlated variables?
xVars<-numVar[-(which(numVar=="change_followers"))]
xVars<-xVars[-(which(numVar=="Followers"))] #We remove Followers because it was highly correlated
xVars<-c(xVars, "category")
modelForm <- createModelFormula(targetVar = "logic_change", xVars = xVars, includeIntercept = TRUE)

logmodel<-glm(modelForm, family=binomial(link='logit'), data=train)
summary(logmodel)

logpred<-predict(logmodel, test, type="response")
#Now we set a parameter to measure what we consider as a "Yes" and what we consider as "No"
threshold<-0.5
defaulted<-rep(0, length(test$logic_change))
defaulted[logpred>threshold]<-1
defaulted<-as.logical(defaulted)
table(defaulted, test$logic_change)

mean(test$logic_change) 
#By default it has a 86% of probability of increasing
mean(defaulted==test$logic_change)
#Our model has 86,% of probability of correctly saying whether it's going to increase or not
#so, it's not better than the default setting

#-------------
#Regression Model

#TODO: Remove highly correlated variables?
xVars<-c(xVars, "Followers") #maybe remove this
targetVar <-  "change_followers"

inTrain <- createDataPartition(y = data_clean[,targetVar], list = FALSE, p = 0.8)
train <- data_clean[inTrain,]
test <- data_clean[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(data_clean))
sum(train$change_followers)/nrow(train)
sum(test$change_followers)/nrow(test)

modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model <- lm(modelForm, data = train)
summary(model)

#If we use Followers we have a R_squared of 0.54 for both the training and the test
#If we don't use it, we have a R_squared of 0.21

#From the p-values we see that we can remove the nTweets
xVars<-xVars[-(which(numVar=="nTweets"))]
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model <- lm(modelForm, data = train)
summary(model)

pred<-predict(model, test)
pred<-as.data.frame(pred)
pred[, "actual"]<-test[, "change_followers"]
RSE<-sum((pred[,"actual"]-pred[, "pred"])**2)
ymean<-mean(test[, "change_followers"])
Rtot<-sum((test[, "change_followers"]-ymean)**2)
R_squared<-1-(RSE/Rtot)
cat("The value of the R squared for the test is", R_squared)

#When we remove nTweets we get worst result

#We try doing backward selection
library('MASS')
library('olsrr')
library('stats')

smodel<-step(model, direction="both")
summary(smodel)

#TODO: look this fuction better
#back<-ols_step_backward_aic(model, details=TRUE)


