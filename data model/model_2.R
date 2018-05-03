#setwd("C:/Users/Carru/SoftwareRepositories/project571/data model")

#We load the data that we are going to use in our models
data<-read.csv('../data collection/Data/ModelData/modelData2.csv', header=T, sep=";", na.strings='Null', stringsAsFactors = F)


#We start observing the data, saving the index of those observations that are NA
index<-which(complete.cases(data)==FALSE)


#We create the variable isWeekend, which takes into consideration whether the data was collected in a weekend or not
data[, "date"]<-as.Date(data[, "date"])
library('lubridate')
day<-wday(data[,"date"], week_start=1) #starts on Monday
data[, "isWeekend"]<-0
data$isWeekend[which(day>=6)]<-1 #Saturday=6


#We create the variable change in number of followers as the variation of the number of followers from the previous day divided by the number of followers that day. This will be our target variable in the regression models
followers<- read.csv('../data collection/Data/ModelData/historicFollowers.csv',header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "change_followers"]<-0
data$change_followers[which(data$date=="2018-02-24")]<-(data$Followers[which(data$date=="2018-02-24")]-followers$X2018.02.23)/followers$X2018.02.23

startday <-  data$date[1]
endday <- data$date[nrow(data)]

a1 <- data$Followers[which((data$date> (startday-1)) & (data$date< endday))]
b1 <- data$Followers[which((data$date> startday) & (data$date<(endday+1)))]
data$change_followers[data$date>startday] <- (b1 - a1)/a1
#This is the change in followers given as an absolute


#We import the variable category 
account<-read.csv('../data collection/Data/ModelData/accountsComplete.csv', header = T,stringsAsFactors = F,sep = ';',na.strings = 'Null')
data[, "category"]<-"a"
data$category[which(data$Account==account$Account)]<-account$Category
data$category<-as.factor(data$category)


#We remove the Accounts that have NAs
i<-which((data$Account=="sportbible")|(data$Account=="fabulousanimals")|(data$Account=="Earth_Pics"))
cat('The percentage of removed data is:', (length(i)/dim(data)[1])*100, '\n') 
#The percentage of the data being deleted compared to the amount of data that we have is so little that we can remove it
data_clean<-data[-i, ]


#We import the variable clusters that partitions the accounts into groups depending on their size.
clusterpartition<-read.csv('../data exploration/AccountClusters.csv', header=T, stringsAsFactors = F, sep=',', na.strings='Null')
clusterpartition<-clusterpartition[,c("Account", "Cluster")]

library('sqldf')
sqlStr<-'SELECT *
        FROM data_clean INNER JOIN clusterpartition ON data_clean.Account== clusterpartition.Account'
data_clean<-sqldf(sqlStr)
data_clean<-data_clean[,-13]

# #TODO: label the clusters
# #TODO: ask what each number represents
data_clean$Cluster<-factor(data_clean$Cluster, levels=c(1,2,3,4))


#We are going to start our analysis by doing a logistic regression, in order to do so, we are going to create a binary variable for our target, where "1" represents an increase in the variation of followers and "0" represents a decrease or no change
data_clean[, "logic_change"]<-0
data_clean$logic_change[which(data_clean$change_followers>0)]<-1
data_clean$logic_change<-as.logical(data_clean$logic_change)


#----------------------------------------------------------------------------------------------
#Correlation between variables
#We start by investigating the correlation between our numeric variables.
type<-function(a, funct){
  if(sum(sapply(a, funct))==0){
    return(0)
  }else{
    return(names(which(sapply(a, funct))))
  }
}
charVar<-type(data_clean, is.character)
numVar<-type(data_clean, is.numeric)
catVar<-type(data_clean, is.factor)
dateVar<-type(data_clean, is.Date)
logicVar<-type(data_clean, is.logical)
stopifnot((length(numVar)+length(catVar)+length(dateVar)+length(logicVar))+length(charVar)==ncol(data_clean))


library('corrplot')
corMatrix <- cor(data_clean[, numVar])
corrplot(corMatrix, method = 'number', diag = TRUE)
#From this matrix we can see that the values show no relevant linear correlation except for pRTs and pMentions


#----------------------------------------------------------------------------------------------
  #MODELS
#----------------------------------------------------------------------------------------------

#Logistic Regression Model
  
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


xVars<-numVar[-(which(numVar=="change_followers"))]
xVars<-xVars[-(which(numVar=="Followers"))] #We remove Followers
xVars<-c(xVars, catVar)
response<-logicVar
modelForm <- createModelFormula(targetVar = response, xVars = xVars, includeIntercept = TRUE)

logmodel<-glm(modelForm, family=binomial(link='logit'), data=train)
summary(logmodel)

#Prediction:
logpred<-predict(logmodel, test, type="response")

#Now we set a threshold for what we consider as a "1" and what we consider as "0"
threshold<-0.5
defaulted<-rep(0, length(test$logic_change))
defaulted[logpred>threshold]<-1
defaulted<-as.logical(defaulted)

mean(test$logic_change) 
#86% of our data corresponds to an increase of the number of followers, so there is a clear class imbalance
confusionMatrix(defaulted, test$logic_change)
#Our model is no better than the default 
#Since there is a clear class imbalance in our data set, there is a huge difference between Specificity & Sensitivity


#Now we are going to proceed by running Stepwise selection to see if we obtain a better model

#Stepwise Selection
library(MASS)
library(stats)

simple<-glm(logic_change ~ 1,family=binomial(link='logit'), data=train)
full<-glm(modelForm,family=binomial(link='logit'), data=train)
logmodel2<-step(simple, direction="both", scope=list(upper=full, lower=simple))
summary(logmodel2)

#logmodel2$formula
#Applying stepwise selection we get that the best model is logic_change ~ category + pMentions + Cluster + pURLs + pMedia + nTweets + isWeekend 
#This makes sense as pRTs was highly correlated to pMentions

#Prediction:
logpred2<-predict(logmodel2, test, type="response")
defaulted2<-rep(0, length(test$logic_change))
defaulted2[logpred2>threshold]<-1
defaulted2<-as.logical(defaulted2)
confusionMatrix(defaulted2, test$logic_change)
#No changes between both models, we get the same results. Applying Occams Razor, this model would be better than the previous one since it's simpler, however it still doesn't provide good enough results. 

#Interaction between pRTs and pMentions
int_log<-glm(logic_change~nTweets + pURLs+ pMedia+ Cluster+category+pRTs*pMentions,family=binomial(link='logit'), data=train)
summary(int_log)

#Prediction:
logpred3<-predict(int_log, test, type="response")
defaulted3<-rep(0, length(test$logic_change))
defaulted3[logpred3>threshold]<-1
defaulted3<-as.logical(defaulted3)
confusionMatrix(defaulted3, test$logic_change)
#We get the same accuracy as in the previous case but we get a slight increase in the false negative rate

#---------------------------------------------------------------------------------------------
#Regression Model

targetVar <-  "change_followers"

set.seed(1234)
#We have to partition the data again, since now we have a different target Variable
inTrain <- createDataPartition(y = data_clean[,targetVar], list = FALSE, p = 0.8)
train <- data_clean[inTrain,]
test <- data_clean[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(data_clean))
sum(train$change_followers)/nrow(train)
sum(test$change_followers)/nrow(test)

#We start by observing the relationship between followers and change in followers 
#Since we want to investigate the change in followers as a variation not as the absolute value we mentioned before, we create a new data set
simple_data<-data
simple_data$change_followers[which(data$date=="2018-02-24")]<-simple_data$change_followers[which(data$date=="2018-02-24")]*followers$X2018.02.23
simple_data$change_followers[simple_data$date>startday]<-simple_data$change_followers[simple_data$date>startday]*a1
train2<-simple_data[simple_data$date=="2018-02-26",]
test2<-simple_data[simple_data$date=="2018-02-27",]
train2<-train2[, c('Followers', 'change_followers')]
test2<-test2[, c('Followers', 'change_followers')]


modelFollower<-lm(change_followers~Followers, data=train2)
summary(modelFollower)

#Prediction
predFollower<-predict(modelFollower, test2)
#The following function calculates the adjusted R_squared of the prediction
RsquaredLM<-function(pred, test, targetVar){
  pred<-as.data.frame(pred)
  n<-as.character(names(pred))
  pred[, "actual"]<-test[, targetVar]
  RSE<-sum((pred[,"actual"]-pred[, n])**2)
  ymean<-mean(test[, targetVar])
  Rtot<-sum((test[, targetVar]-ymean)**2)
  R_squared<-1-(RSE/Rtot)
  adj_R<-1-((1-R_squared)*(dim.data.frame(test)[1]))/(dim.data.frame(test)[1]-length(colnames(test))-4)
  return(adj_R)
}

R_squaredF<-RsquaredLM(predFollower, test2, targetVar) 

ggplot(train2, aes(x = Followers, y = change_followers))+ geom_point() + 
  geom_abline(intercept=2.126e+02, slope=2.921e-04,color='red') + 
  ggtitle("Relation between Followers and change in followers") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Followers", y="change_followers")


#Model including all variables and change_followers as absolute change
modelForm <- createModelFormula(targetVar = targetVar, xVars = xVars, includeIntercept = TRUE)
model <- lm(modelForm, data = train)
summary(model)
cat('The adjusted r squared is: ', summary(model)$adj.r.squared)
#change in %:0.0192

plot(model)

cat('\n')
#Prediction:
pred<-predict(model, test)
adjR_squared<-RsquaredLM(pred, test, targetVar)
cat("The value of the R squared for the test is", adjR_squared, "\n")
#change as percentage: 0.0344


#Stepwise Selection

simple_lm<-lm(change_followers ~ 1, data=train)
full_lm<-lm(modelForm, data=train)
lmStep<-step(simple_lm, direction="both", scope=list(upper=full_lm, lower=simple_lm))
summary(lmStep)
cat('The adjusted r squared is: ', summary(lmStep)$adj.r.squared, "\n") #0.01941
plot(lmStep)
#change in %: 0.02438
#summary(lmStep)$call

lmStep.pred<-predict(lmStep, data=test)
R_squaredStep<-RsquaredLM(pred, test, targetVar)
cat("The value of the R squared for the test is", R_squaredStep, "\n")
#We get the same formula as the previous case

anova(model, lmStep)

#We compare the two models. The null hypothesis is that the two models fit the data equally and the alternative is that lmStep is better. The p-value is 0.709 which means that we fail to reject the null hypothesis. If we look at the r squared is practically the same, being the one for the first model slightly better


#------------------------------------------------------------------------------------------
#Interaction Terms

#We are going to investigate whether the model satisfies the additive assumption. When we did the correlation matrix we found that there is a high correlation between pRTs and pMentions

variables<-xVars[-(which((xVars=="pRTs")|(xVars=="pMentions")))]
newModel<-as.formula(paste(targetVar, "~", paste(variables, collapse = '+ '), "+","pRTs*pMentions"))

lm.int<-lm(newModel, train)
sumsummary(lm.int)
cat('The adjusted r squared is: ', summary(lm.int)$adj.r.squared, "\n")
#change %: 0.02189
#The p-value for this new term is really low which indicates that there is a clear relationship
#which is not additive. This means that the changes in the predictor pRTs is related to the change in pMentions

plot(lm.int)
pred3<-predict(lm.int, test)
adjR_squared3<-RsquaredLM(pred3, test, targetVar)
cat("The value of the R squared for the test is", adjR_squared3, "\n") 


#We use an alpha of 0.5 to choose the p-values that are significant for our next iteration
lmReduced.int<-lm(change_followers~pURLs+ pMedia+ category+pRTs*pMentions, data=train)
summary(lmReduced.int)
cat('The adjusted r squared is: ', summary(lmReduced.int)$adj.r.squared)
#change in %:0.01975

plot(lmReduced.int)
pred4<-predict(lmReduced.int, test)
adjR_squared4<-RsquaredLM(pred4, test, targetVar)
cat("The value of the R squared for the test is", adjR_squared4, "\n")
#0.04707

anova(model, lmStep, lm.int, lmReduced.int) 

#------------------------------------------------------------------------------------------
#Polynomial Regression
#Now we are going to see if the model satisfies the linear assumption

#We start by trying a second degree polynomial
pol.reg2<-lm(change_followers ~ category + Cluster +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=2, raw=TRUE), data=train)
#summary(pol.reg2)
#cat('The adjusted r squared is: ', summary(pol.reg2)$adj.r.squared)
pol.reg2.pred<-predict(pol.reg2, test)
adjR_squared.pol2<-RsquaredLM(pol.reg2.pred, test, targetVar)
cat("The value of the R squared for the test is", adjR_squared.pol2, "\n")

#Third degree polynomial
pol.reg3<-lm(change_followers ~ category +Cluster +polym(nTweets, pHashtags, pMentions, pURLs, pMedia, pRTs, isWeekend, degree=3, raw=TRUE), data=train)
#summary(pol.reg3)
#cat('The adjusted r squared is: ', summary(pol.reg3)$adj.r.squared)
pol.reg3.pred<-predict(pol.reg2, test)
adjR_squared.pol3<-RsquaredLM(pol.reg3.pred, test, targetVar)
cat("The value of the R squared for the test is", adjR_squared.pol3, "\n")


anova(model, lm.int, lmStep , lmReduced.int, pol.reg2)
#The null hypothesis in this case is that all models fit just as equally the data as the simplest model (model), and the alternative hypothesis is that one of the models is better. From the results of this analysis we can see that the rest of the models are better. From our analysis of the adjusted R^2 we can deduce that from all of them the best one was the second degree polynomial regression, although the values obtained with this model weren't high enough. 
#------------------------------------------------------------------------------------------
#Regression Tree
library(rpart)
library(rpart.plot)

set.seed(1234)
reg.tree<-rpart(modelForm, data=train)
summary(reg.tree)
#There is no need to prune the tree, since the smallest error we get it at CP =4

rpart.plot(reg.tree, type=4, digits=4 , main="Full Regression Tree")
#There is no need to prune since we can see that the tree chose 5 levels and that has the lowest error.

#Prediction:
reg.tree.pred<-predict(reg.tree, test)
reg.mse<-mean((reg.tree.pred-test$change_followers)**2)


adjR<-RsquaredLM(reg.tree.pred, test, targetVar)
cat('The adj R of this model is: ', adjR, "\n")
#We get an adjusted R of 0.030



#Random Forest

library(randomForest)
set.seed(1234)
rf.tree<-randomForest(modelForm, data=train, ntreeTry = 500,importance=T)

rf.pred<-predict(rf.tree, data=test)
print(rf.tree)
mtry<-tuneRF(x=train[,xVars], y=train[, targetVar], ntreeTry = 500, stepFactor = 1.5, improve=0.01, trace=TRUE, plot=TRUE)
#Since mtry is still 2 which is what it is used to create the randomForest we don't have to create a new one

adjR2<-RsquaredLM(rf.pred, test, targetVar)
cat('The adj R of this model is: ', adjR2, "\n")
#We get an adjusted R of 0.030

