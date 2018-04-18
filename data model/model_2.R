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
library('lattice')
library('ggplot2')
library('caret')

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
#TODO: Introduce the groups replacing the followers
xVars<-numVar[-(which(numVar=="change_followers"))]
xVars<-xVars[-(which(numVar=="Followers"))] #We remove Followers because it was highly correlated
xVars<-c(xVars, "category")
response<-"logic_change"
modelForm <- createModelFormula(targetVar = response, xVars = xVars, includeIntercept = TRUE)

logmodel<-glm(modelForm, family=binomial(link='logit'), data=train)
summary(logmodel)

#TODO: once all the variables are set, analyze the coefficients

logpred<-predict(logmodel, test, type="response")
#Now we set a parameter to measure what we consider as a "Yes" and what we consider as "No"
threshold<-0.5
defaulted<-rep(0, length(test$logic_change))
defaulted[logpred>threshold]<-1


mean(test$logic_change) 
#86% of our data corresponds to an increase of the number of followers, so there is a clear 
#class imbalance
confusionMatrix(defaulted, test$logic_change)
#Our model has 86,% of probability of correctly saying whether it's going to increase or not
#so, it's not better than the default setting.

#Since there is a clear class imbalance in our data set, there is a huge difference 
#between Specificity & Sensitivity

x<-sort(logpred)
plot(x, ylab='Probability', main='Logistic regression, full model')
#Since we barely have data for the lower part of the sigmoid curve, we can't draw it properly.

#Now we are going to proceed by running Stepwise selection to see if we obtain a better model

#Stepwise Selection
library(MASS)
library(stats)

simple<-glm(logic_change ~ 1,family=binomial(link='logit'), data=train)
full<-glm(modelForm,family=binomial(link='logit'), data=train)
logmodel2<-step(simple, direction="both", scope=list(upper=full, lower=simple))
summary(logmodel2)

#logmodel2$formula
#Applying stepwise selection we get that the best model is 
#logic_change ~ category + pMentions + pURLs + pMedia + nTweets + isWeekend
#This makes sense as pRTs was highly correlated to pMentions
#Hastags are simply not importan 

#Prediction:
logpred2<-predict(logmodel2, test, type="response")
threshold<-0.5
defaulted2<-rep(0, length(test$logic_change))
defaulted2[logpred2>threshold]<-1
confusionMatrix(defaulted2, test$logic_change)
#No changes between both models, we get the same results
#No change in the graph either

# #Lasso Regression:
# library(Matrix)
# library(foreach)
# library(glmnet)
# 
# x<-model.matrix(~ nTweets + pHashtags + pMentions + pURLs + pMedia + pRTs + isWeekend + category -1, train)
# y<-as.matrix(train$logic_change)
# #We use cross-validation to choose the tuning parameter (lambda)
# cv.lasso<-cv.glmnet(x, y, alpha=1)
# best.lasso<-cv.lasso$lambda.min #we get a lambda of 0.00024
# 
# xtest<-model.matrix(~ nTweets + pHashtags + pMentions + pURLs + pMedia + pRTs + isWeekend + category -1, test)
# lasso_mod<-glmnet(x, y, alpha=1, lambda=c(1,10,1))
# lasso.pre<-predict(lasso_mod, s=best.lasso, xtest)


#-------------
#Regression Model

#TODO: EXPLANATION OF THE COEFFICIENTS
#TODO: Remove highly correlated variables?
#xVars<-c(xVars, "Followers") 
targetVar <-  "change_followers"

#We have to partition the data again, since now we have a different targetVar
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
#If we don't use it, we have a R_squared of 0.2234
cat('The adjusted r squared is: ', summary(model)$adj.r.squared)

#Prediction:
pred<-predict(model, test)

#The following function calculates the R_squared of the prediction
RsquaredLM<-function(pred, test, targetVar){
  pred<-as.data.frame(pred)
  n<-as.character(names(pred))
  pred[, "actual"]<-test[, targetVar]
  RSE<-sum((pred[,"actual"]-pred[, n])**2)
  ymean<-mean(test[, targetVar])
  Rtot<-sum((test[, targetVar]-ymean)**2)
  R_squared<-1-(RSE/Rtot)
}
R_squared<-RsquaredLM(pred, test, targetVar)
cat("The value of the R squared for the test is", R_squared)

#We get an R_squared of 0.2077

#From the p-values we see that we can remove the nTweets
xVars<-xVars[-(which(numVar=="nTweets"))]
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model2 <- lm(modelForm, data = train)
summary(model2)
cat('The adjusted r squared is: ', summary(model2)$adj.r.squared) #0.2236

pred2<-predict(model2, test)
R_squared2<-RsquaredLM(pred2, test, targetVar)
cat("The value of the R squared for the test is", R_squared2)
#We get the same R squared as before

#plot(model2)
#TODO: EXPLANATION OF THE PLOTS 

#Stepwise Selection

simple_lm<-lm(change_followers ~ 1, data=train)
full_lm<-lm(modelForm, data=train)
lmStep<-step(simple_lm, direction="both", scope=list(upper=full_lm, lower=simple_lm))
summary(lmStep)
cat('The adjusted r squared is: ', summary(lmStep)$adj.r.squared) #0.2236
#summary(lmStep)$call
#We get the same formula as the previous case

#If I remove pRTs I get worse model

#TODO: look this fuction better
#back<-ols_step_backward_aic(model, details=TRUE)

#TODO: Ridge & Lasso (& Naive ?)

#Standarization of the variables




#------------------------------------------------------------------------------------------
#Polynomic Model
