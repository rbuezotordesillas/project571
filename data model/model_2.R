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
data$isWeekend[which(day>=6)]<-1 #Friday=5/Saturday=6
#TODO: discuss, works better when it is Friday...


#Create variable change in number of followers
followers<- read.csv('../data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "change_followers"]<-0
data$change_followers[which(data$date=="2018-02-24")]<-(data$Followers[which(data$date=="2018-02-24")]-followers$X2018.02.23)/followers$X2018.02.23

#TODO: change this when we have all the data
startday <-  data$date[1]
endday <- data$date[nrow(data)]

a1 <- data$Followers[which((data$date> (startday-1)) & (data$date< endday))]
b1 <- data$Followers[which((data$date> startday) & (data$date<(endday+1)))]
data$change_followers[data$date>startday] <- ((b1 - a1)/a1)*100
#We have made this as a percentage

#TODO: acceleration of increase in followers instead of velocity

#We create a category column
account<-read.csv('../data collection/Data/ModelData/accountsComplete.csv', header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "category"]<-"a"
data$category[which(data$Account==account$Account)]<-account$Category
data$category<-as.factor(data$category)


#We remove the Accounts that have NAs
i<-which((data$Account=="sportbible")|(data$Account=="fabulousanimals")|(data$Account=="Earth_Pics"))
cat('The percentage of removed data is:', (length(i)/dim(data)[1])*100, '\n') #So we can delete them
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

#TODO: If you put the change_followers as percentage it shows that there is no correlation
#with the number of followers

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
#Hashtags are simply not important 

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
plot(model)
#TODO: investigate how to export them automatically to a file

#If we use Followers we have a R_squared of 0.54 for both the training and the test
#If we don't use it, we have a R_squared of 0.2234
cat('The adjusted r squared is: ', summary(model)$adj.r.squared)
#change in %:0.0239

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
#change as percentage: 0.0184


#From the p-values we see that we can remove the nTweets
#TODO: if in % we don't remove nTweets

# xVars<-xVars[-(which(numVar=="nTweets"))]
# modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
# model2 <- lm(modelForm, data = train)
# summary(model2)
# cat('The adjusted r squared is: ', summary(model2)$adj.r.squared) #0.2236
# #change in followers as percentage: 0.0235
# plot(model2)
# 
# pred2<-predict(model2, test)
# R_squared2<-RsquaredLM(pred2, test, targetVar)
# cat("The value of the R squared for the test is", R_squared2)
# #We get the same R squared as before
# #change %: 0.0180
#TODO: if I use this don't forget to introduce nTweets in xVars if I create new formulas with it

#TODO: EXPLANATION OF THE PLOTS 

#Stepwise Selection

simple_lm<-lm(change_followers ~ 1, data=train)
full_lm<-lm(modelForm, data=train)
lmStep<-step(simple_lm, direction="both", scope=list(upper=full_lm, lower=simple_lm))
summary(lmStep)
cat('The adjusted r squared is: ', summary(lmStep)$adj.r.squared) #0.2236
plot(lmStep)
#change in %: 0.02438
#summary(lmStep)$call

#We get the same formula as the previous case
#change in %: change_followers ~ category + pMedia + pRTs + pURLs

anova(model, lmStep)
#TODO: explanation when all the data is loaded
#TODO: maybe remove this cause it is going to be explained with all the models
#We compare the two models. The null hypothesis is that the two models fit the data equally
#and the alternative is that lmStep is better. The p-value is 0.91 which means that we
#fail to reject the null hypothesis. If we look at the r squared is practically the same, being
#the one for the first model slightly better


#TODO: Ridge & Lasso

#Standarization of the variables
#TODO: change_followers as a percentage


#TODO: PCA

#------------------------------------------------------------------------------------------
#Interaction Terms
#We are going to investigate whether the model satisfies the additive assumption

#When we did the correlation matrix we found that there is a high correlation between pRTs and 
#pMentions
variables<-xVars[-(which((xVars=="pRTs")|(xVars=="pMentions")))]
newModel<-as.formula(paste(targetVar, "~", paste(variables, collapse = '+ '), "+","pRTs*pMentions"))

lm.int<-lm(newModel, train)
summary(lm.int)
cat('The adjusted r squared is: ', summary(lm.int)$adj.r.squared)
#change %: 0.02896
#The p-value for this new term is really low which indicates that there is a clear relationship
#which is not additive. This means that the changes in the predictor pRTs is related to the change
#in pMentions

plot(lm.int)
pred3<-predict(lm.int, test)
R_squared3<-RsquaredLM(pred3, test, targetVar)
cat("The value of the R squared for the test is", R_squared3)
#change in %: 0.01909

#TODO: ALL OF THIS IS DONE BY HAND, DO A FUNCTION
#I use an alpha of 0.5 to choose the p-values maybe too high
lmReduced.int<-lm(change_followers~nTweets + pURLs+ pMedia+ category+pRTs*pMentions, data=train)
summary(lmReduced.int)
cat('The adjusted r squared is: ', summary(lmReduced.int)$adj.r.squared)
#change in %:0.02929

plot(lmReduced.int)
pred4<-predict(lmReduced.int, test)
R_squared4<-RsquaredLM(pred4, test, targetVar)
cat("The value of the R squared for the test is", R_squared4)
#0.01875

#TODO: anova comparison of all the regression models?
anova(model, lmStep, lm.int, lmReduced.int) #The results of this show that the third model is much better than the first
#anova(model, lm.int) #lm.int best model so far

#------------------------------------------------------------------------------------------
#Polynomic Regression
#Now we are going to see if the model satisfies the linear assumption

#First we are going to choose the order of the polynomial regression and then we are going to 
#choose the optimal variables

#TODO: investigate further, it doesn't allow me to introduce category to the quadratic equation
#order 2
pol.reg2<-lm(change_followers ~ category +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=2, raw=TRUE), data=train)
#summary(pol.reg2)
#cat('The adjusted r squared is: ', summary(pol.reg2)$adj.r.squared)

#order 3
pol.reg3<-lm(change_followers ~ category +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=3, raw=TRUE), data=train)
#summary(pol.reg3)
#cat('The adjusted r squared is: ', summary(pol.reg3)$adj.r.squared)

#order 4
pol.reg4<-lm(change_followers ~ category +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=4, raw=TRUE), data=train)
#summary(pol.reg4)
#cat('The adjusted r squared is: ', summary(pol.reg4)$adj.r.squared)

#order 5
pol.reg5<-lm(change_followers ~ category +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=5, raw=TRUE), data=train)
#summary(pol.reg5)
#cat('The adjusted r squared is: ', summary(pol.reg5)$adj.r.squared)

anova(model, pol.reg2, pol.reg3, pol.reg4, pol.reg5)
#We compare it with the "basic" model to choose the order of the polynomial
#It is unusual to take values higher than 3/4 because the curve can become over flexible. So,
#we choose order 4
#TODO: look for the full explanation

#TODO: choose best coefficients looking at the p-value, automatically?¿
p<-summary(pol.reg4)$coefficients[,4]
alpha<-0.05
candidates<-p[p<alpha]

#TODO: The best model
#The best model would be: 
pol.reg.pruned<-lm(change_followers ~ category + poly(pURLs, pMedia, degree=2, raw=TRUE) + 
                     nTweets*pMentions+ pHashtags + pHashtags:pMentions + nTweets:pURLs+
                     pHashtags:pURLs+ pURLs:pMentions + pHashtags:pMedia +
                     pMedia:pMentions + pURLs:pRTs + pMedia:pRTs + pRTs, train)
summary(pol.reg.pruned)

#TODO: regression splines (if I have time)

#------------------------------------------------------------------------------------------
#Regression Tree




#------------------------------------------------------------------------------------------
#ARIMA

