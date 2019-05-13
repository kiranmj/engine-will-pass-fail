#Clearing the R environment
rm(list=ls())
#Setting the working directory
setwd("C:\\Users\\SSC\\Downloads\\phd-final\\Newfiles")
#reading train and test datasets into R that are previously preprocessed
train=read.csv("Train.new.csv")
test=read.csv("Test.new.csv")
train$X.1=NULL
train$TestA.1=NULL
train$TestB.1=NULL
#Split train data in 80/20 ration as validation data
size_<-floor(0.80*nrow(train))
set.seed(123)
split.t<-sample(seq_len(nrow(train)), size = size_)
train.new<-train[split.t,]
train.val<-train[-split.t,]
#Building a Logistic Regression model
log_reg <- glm(y ~ ., data = train.new, family = "binomial") #binomial because we only want two classes as output
#Call summary function upon the model to see the performance
summary(log_reg) # model is not significant 
rm(log_reg) #Delete the model because it contains too many insignificant data and low accuracy
##
#Building a new Logistic Regression model with only the significant variables from above P(values)
logreg<-glm(train.new$y~ Number.of.Cylinders + material.grade + Bearing.Vendor + Fuel.Type + Compression.ratio + Cylinder.deactivation + Direct.injection + Peak.Power + Liner.Design., data = train.new, family = "binomial")
#Calling the summary function to check the performance of our model
summary(logreg)
#Using 'Caret' package to use the function confusion matrix
library(caret)
pred<-predict(logreg, type="response", newdata=train.val)
pred<-ifelse(pred>0.5,"pass","fail")
pred<-as.factor(pred)
confusionMatrix(train.val$y,pred)
table(train.val$y)
#The sensitivity of our model is 72.04%, while Specificity is 76%, and accuracy of 74%
#In total our model had predicted 307 classes as 'fail' and 325 classes as 'pass'
##
#Building a Randomforest model to try and increase the accuracy
library(randomForest) #the package contains RF function
rfmodel<-randomForest(y~.,train.new)
#we check the performance by plotting the Randomforest model
plot(rfmodel)
attributes(rfmodel)
#Tune hyperparameters like number of trees etc, in order to obtain better accuracy
tuneinrf<-tuneRF(train.new[,-21], train.new[,21],ntreeTry = 500, stepFactor=0.5,improve = 0.05,trace = TRUE)
#building a new model with tuned parameters
rfmodel1<-randomForest(y~.,train.new,ntree=500,mtry=4)
plot(rfmodel1)
attributes(rfmodel1)
#Finally we test the train validation data with the model above
predrf1<-predict(rfmodel1,train.val)
confusionMatrix(predrf1,train.val$y)
table(predrf1)
#The sensitivity of our model is 80%, while Specificity is 88%, and accuracy of 84%

#Now we try to predict using logreg model the target classes on our test data
predtest<-predict(logreg,test)
predtest<-ifelse(predtest>0.5,"pass","fail")
predtest<-as.factor(predtest)
predtest<-data.frame(predtest)
finaltest<-cbind(predtest,test)
head(finaltest$predtest)
#We try to predict target classes on our test data using rF model
pred<-predict(rfmodel1,test)
confusionMatrix(pred,test$y)
table(pred)