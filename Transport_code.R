rm(list=ls())
setwd('C:/Users/ashwi/Downloads')
mydata=read.csv("Cars_edited.csv")
attach(mydata)
library(dplyr)
library(funModeling)
library(rpivotTable)



library(ggplot2)
library(tidyverse)
library(corrplot)
library(lmtest)
library(pscl)
library(car)
library(Epi)
library(ROCR)
library(caret)

###############To view the data insight 
str(mydata)
names(mydata)
summary(mydata)
head(mydata)
glimpse(mydata)
df_status(mydata)
profiling_num(mydata)
plot_num(mydata)
describe(mydata)

###################Convert to integer 
mydata$Salary<-as.integer(mydata$Salary)
mydata$Distance <-as.integer(mydata$Distance)
mydata$Gender<-as.integer(mydata$Gender)

glimpse(mydata)
summary(mydata)
str(mydata)
##########################Remove 1 data for NULL value 
mydata <- mydata[complete.cases(mydata), ]
na.omit(mydata)

##################Check for Zero variance 
nzv=nearZeroVar(mydata, saveMetrics = T)
nzv
mydata=mydata[,-nzv]

DataExplorer::create_report(mydata)

###Corelation plot 
corrplot(cor(mydata_nofactor),method='number', type = 'upper' )


############Various grapgh plot for studying data 
t = sapply(mydata,is.numeric)
temp=mydata[,names(mydata) %in% names(mydata)[t]]
ggplot(gather(temp), aes(value)) + geom_histogram() + facet_wrap(~key, scales = 'free')
ggplot(gather(temp), aes(key,value)) + geom_boxplot() + facet_wrap(~key, scales = 'free') 
ggplot(mydata, aes(Transport,Age,col=Transport))  + geom_point(position="jitter",size=3) + facet_grid(~license) + ggtitle("Transport vs Age graph with Licence")
##ggplot(mydata, aes(Transport,MBA,col=Transport))  + geom_point(position="jitter",size=3) + facet_grid(~license) + ggtitle("Transport vs Age graph with Licence")
ggplot(mydata, aes(Transport,..count.., fill = Transport ,col=Transport))  + geom_bar() + facet_grid(~Engineer) + ggtitle("Transport vs Age graph Engineer")
ggplot(mydata, aes(Transport,..count.., fill = Transport ,col=Transport))  + geom_bar() + facet_grid(~MBA) + ggtitle("Transport vs Age graph mba")
ggplot(mydata, aes(Transport,Work.Exp,col=Transport))  + geom_point(position="jitter",size=3) + facet_grid(~license)
ggplot(mydata, aes(Transport,Salary,col=Transport))  + geom_point(position="jitter",size=3) + facet_grid(~license)
ggplot(mydata, aes(Transport,Work.Exp,col=Transport))  + geom_point(position="jitter",size=3) + facet_grid(~Gender)
ggplot(mydata, aes(Transport,Distance,col=Transport))  + geom_boxplot() + facet_grid(~Gender)
ggplot(mydata, aes(Transport,Distance ,col = Transport))  + geom_point(position="jitter",size=3) + ggtitle("Transport vs Distance")

rpivotTable(mydata)


###########As objective was to predict Car Use  label it as 1 and 0 
mydata$CarUsage<-ifelse(mydata$Transport =='Car',1,0)
mydata = mydata[,-c(9)]

###############Percentage of car usage 
table(mydata$CarUsage)
v1 <- 61/(383+61)
View(v1)

mydata_new = mydata

names(which(apply(mydata_new, 2, anyNA))) #which variables  has where NA is TRUE

###################Convert to factor variable 
mydata_new$Engineer<-as.factor(mydata_new$Engineer)
mydata_new$MBA<-as.factor(mydata_new$MBA)
mydata_new$license<-as.factor(mydata_new$license)
mydata_new$CarUsage<-as.factor(mydata_new$CarUsage)



################Indentifying parameters needed for model and corelation  
step(glm(mydata$CarUsage ~ ., data=mydata, family=binomial(link="logit")))
summary(m1)
car::vif(m1)

m1=glm(mydata$CarUsage ~ . - Work.Exp, data=mydata, family=binomial(link="logit"))
summary(m1)
car::vif(m1)


mydata = mydata[,-c(5)]
mydata_new = mydata

DataExplorer::create_report(mydata_new)

######################Split train and TEST dataset 
set.seed(137)
samp=sample(2, nrow(mydata_new), replace = T, prob = c(0.7,0.3)) ####Generate the test and train data-set
train=mydata_new[samp==1,]
test=mydata_new[samp==2,]
nrow(train)
nrow(test)
head(test)
head(train)


table(mydata_new$CarUsage)
table(test$CarUsage)
table(train$CarUsage)

v1 = 61/(61+382)
v1
v2 = 19/(19+112)
v2
v3 = 42/(42+270)
v3
library(GGally)
ggpairs(mydata_new)

############Smote data preparation ---
test$CarUsage<-as.factor(test$CarUsage)
train$CarUsage<-as.factor(train$CarUsage)
smote_train <- SMOTE(CarUsage ~ ., train , perc.over = 200, k = 5, perc.under = 150)
table(smote_train$CarUsage)


smote_features_train<-as.matrix(smote_train[,1:7])
smote_label_train<-as.matrix(smote_train$CarUsage)


################Logistic Regression
step(glm(smote_train$CarUsage ~ . , data=smote_train, family=binomial(link="logit")))
summary(m1)

m1=glm(smote_train$CarUsage ~ . -Engineer -Salary -Gender , data=smote_train, family=binomial(link="logit"))
summary(m1)
pR2(m1)
lrtest(m1)

y1 = coef(m1)
y1
y2 =exp(y1)

y3 = ((y2/(1+y2)))
(y3)


library(caret)
library(e1071)

model_lr <- caret::train(CarUsage ~ . -Engineer -Salary -Gender ,
                         data = smote_train,
                         method = "glm",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))
summary(model_lr)



pred.prob=predict(model_lr, newdata = test, type = 'prob')
final <- data.frame(actual = test$CarUsage,
                    predict(model_lr, newdata = test, type = "prob"))
final$predict <- ifelse(final$X1 > 0.75, "1", "0")

conf.mat=table(final$predict, test$CarUsage)
View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]


cm_lr=confusionMatrix(test$CarUsage,as.factor(final$predict),cutoff=0.75)
cm_lr



library(ROCR)
pred_lr = prediction(as.numeric(final$predict),as.numeric(final$actual))
perf_lr.1=performance(pred_lr, "tpr", "fpr")
as.numeric(performance(pred_lr, "auc")@y.values)
perf = performance(pred_lr, "tpr","fpr")
plot(perf)
KS <- max(attr(perf_lr.1, 'y.values')[[1]]-attr(perf_lr.1, 'x.values')[[1]])
KS
auc <- performance(pred_lr,"auc"); 
auc <- as.numeric(auc@y.values)
auc
library(ineq)
gini.1 = ineq(final$predict, type="Gini")
cat('Gini:', gini.1, '\n')




conf.mat=table(final$predict, test$CarUsage)
#View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]

#accuracy
cat('Accuracy:', (TN+TP)/(TN+TP+FP+FN),'\n')
#error
cat('Error:', (FP+FN)/(TN+TP+FP+FN),'\n')
#sensitivity/recall/TPR
cat('Sensitivity/Recall/True Positive Rate:', TP/(TP+FN), '\n')
#specificity/TNR
cat('Specificity/False Positive Rate:', TN/(TN+FP),'\n')
#Predicted Positive rate/Precision
cat('Predicted Positive Rate/Precision:', TP/(TP+FP), '\n')
#Predicted Negative rate
cat('Predicted Negative Rate:', TN/(TN+FN), '\n')




####################Naive Bayes  

train_nv = smote_train
test_nv =  test 

train_nv$Engineer = as.factor(train_nv$Engineer)
train_nv$MBA = as.factor(train_nv$MBA)
train_nv$license  = as.factor(train_nv$license)
glimpse(train_nv)
train_nv$Salary = round(train_nv$Salary)
train_nv$Distance = round(train_nv$Distance)

train_nv$Salary=as.factor(ifelse(train_nv$Salary<median(train_nv$Salary),'0','1'))
train_nv$Distance=as.factor(ifelse(train_nv$Distance<median(train_nv$Distance),'0','1'))
train_nv$Age=as.factor(ifelse(train_nv$Age<median(train_nv$Age),'0','1'))
train_nv$Gender=as.factor(train_nv$Gender)


test_nv$Engineer = as.factor(test_nv$Engineer)
test_nv$MBA = as.factor(test_nv$MBA)
test_nv$license  = as.factor(test_nv$license)
glimpse(test_nv)
test_nv$Salary = round(test_nv$Salary)
test_nv$Distance = round(test_nv$Distance)
test_nv$Salary=as.factor(ifelse(test_nv$Salary<median(test_nv$Salary),'0','1'))
test_nv$Distance=as.factor(ifelse(test_nv$Distance<median(test_nv$Distance),'0','1'))
test_nv$Age=as.factor(ifelse(test_nv$Age<median(test_nv$Age),'0','1'))
test_nv$Gender=as.factor(test_nv$Gender)

nb.m1=naiveBayes(CarUsage~. -Engineer -Salary -Gender, data=train_nv)
nb.m1




temp_predicted = predict(nb.m1 , type = 'raw' , newdata = test_nv )[,2]
View(temp_predicted)

plot(test_nv$CarUsage, temp_predicted)
Status.Predicted_navi = ifelse(temp_predicted  < 0.95,"0","1")
table(test_nv$CarUsage, Status.Predicted_navi)
cm_nv = confusionMatrix(test_nv$CarUsage ,as.factor(Status.Predicted_navi),cutoff = .95)
cm_nv



library(ROCR)
pred_nv = prediction(as.numeric(Status.Predicted_navi),as.numeric(test_nv$CarUsage))
perf_nv.1=performance(pred_nv, "tpr", "fpr")
as.numeric(performance(pred_nv, "auc")@y.values)
perf = performance(pred_nv, "tpr","fpr")
plot(perf)
KS <- max(attr(perf_nv.1, 'y.values')[[1]]-attr(perf_nv.1, 'x.values')[[1]])
KS
auc <- performance(pred_nv,"auc"); 
auc <- as.numeric(auc@y.values)
auc
library(ineq)
gini.nv = ineq(Status.Predicted_navi, type="Gini")
cat('Gini:', gini.nv, '\n')



conf.mat=table(Status.Predicted_navi, test$CarUsage)
#View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]
#accuracy
cat('Accuracy:', (TN+TP)/(TN+TP+FP+FN),'\n')
#error
cat('Error:', (FP+FN)/(TN+TP+FP+FN),'\n')
#sensitivity/recall/TPR
cat('Sensitivity/Recall/True Positive Rate:', TP/(TP+FN), '\n')
#specificity/TNR
cat('Specificity/False Positive Rate:', TN/(TN+FP),'\n')
#Predicted Positive rate/Precision
cat('Predicted Positive Rate/Precision:', TP/(TP+FP), '\n')
#Predicted Negative rate
cat('Predicted Negative Rate:', TN/(TN+FN), '\n')






####KNN model solution 
seed(2)
train_knn = smote_train
test_knn = test
pp=preProcess(smote_train, method = list(range=c('Age','Gender','Engineer','MBA','Salary',
                                           'Distance','license')))

train.norm=predict(pp,smote_train)
test.norm=predict(pp,test)

fit.control=trainControl(method = 'cv',
                         number = 5)

knn.m=train(CarUsage~. , data=train.norm,
            method = 'knn',
            trControl = fit.control,
            metric = 'Accuracy',
            tuneLength = 10)

print(knn.m)
knn.m$bestTune
knn.m$results[order(knn.m$results$Accuracy, decreasing = T),]


knn.m1=knn(train = train.norm , test = test.norm , train.norm$CarUsage ,k = 5)

library(ROCR)
library(gplots)
library(class)

tabKNN = table(test.norm$CarUsage, knn.m1)
tabKNN



library(ROCR)
pred_bag = prediction(as.numeric(knn.m1),as.numeric(test_knn$CarUsage))
perf_bag.1=performance(pred_bag, "tpr", "fpr")
as.numeric(performance(pred_bag, "auc")@y.values)
perf = performance(pred_bag, "tpr","fpr")
plot(perf)
KS <- max(attr(perf_bag.1, 'y.values')[[1]]-attr(perf_bag.1, 'x.values')[[1]])
KS
auc <- performance(pred_bag,"auc"); 
auc <- as.numeric(auc@y.values)
auc
library(ineq)
gini.1 = ineq(final_bag$predict, type="Gini")
cat('Gini:', gini.1, '\n')




conf.mat=table(knn.m1, test$CarUsage)
#View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]
#accuracy
cat('Accuracy:', (TN+TP)/(TN+TP+FP+FN),'\n')
#error
cat('Error:', (FP+FN)/(TN+TP+FP+FN),'\n')
#sensitivity/recall/TPR
cat('Sensitivity/Recall/True Positive Rate:', TP/(TP+FN), '\n')
#specificity/TNR
cat('Specificity/False Positive Rate:', TN/(TN+FP),'\n')
#Predicted Positive rate/Precision
cat('Predicted Positive Rate/Precision:', TP/(TP+FP), '\n')
#Predicted Negative rate
cat('Predicted Negative Rate:', TN/(TN+FN), '\n')

##################################################
#######Bagging

train_bgg = smote_train
test_bgg =  test

library(gbm)          
library(xgboost)      
library(ggplot2)
library(caret)     
library(ipred)
library(rpart)

Transport.bagging <- bagging(CarUsage ~.,
                          data=train_bgg,
                          control=rpart.control(maxdepth=5, minsplit=4))


train_bgg$pred.class <- predict(Transport.bagging,train_bgg)

bagging_model = confusionMatrix(train_bgg$CarUsage,train_bgg$pred.class)
bagging_model


test_bgg$pred.class <- predict(Transport.bagging,test_bgg)
bagging_model_test = confusionMatrix(test_bgg$CarUsage,test_bgg$pred.class)
bagging_model_test




library(ROCR)
pred_bag = prediction(as.numeric(test_bgg$pred.class),as.numeric(test_bgg$CarUsage))
perf_bag.1=performance(pred_bag, "tpr", "fpr")
as.numeric(performance(pred_bag, "auc")@y.values)
perf = performance(pred_bag, "tpr","fpr")
plot(perf)
KS <- max(attr(perf_bag.1, 'y.values')[[1]]-attr(perf_bag.1, 'x.values')[[1]])
KS
auc <- performance(pred_bag,"auc"); 
auc <- as.numeric(auc@y.values)
auc
library(ineq)
gini.1 = ineq(test_bgg$pred.class, type="Gini")
cat('Gini:', gini.1, '\n')




conf.mat=table(test_bgg$pred.class, test$CarUsage)
#View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]
#accuracy
cat('Accuracy:', (TN+TP)/(TN+TP+FP+FN),'\n')
#error
cat('Error:', (FP+FN)/(TN+TP+FP+FN),'\n')
#sensitivity/recall/TPR
cat('Sensitivity/Recall/True Positive Rate:', TP/(TP+FN), '\n')
#specificity/TNR
cat('Specificity/False Positive Rate:', TN/(TN+FP),'\n')
#Predicted Positive rate/Precision
cat('Predicted Positive Rate/Precision:', TP/(TP+FP), '\n')
#Predicted Negative rate
cat('Predicted Negative Rate:', TN/(TN+FN), '\n')

################################################
############Boosting 

train_boost = train

train_boost1 = smote_train
test_boost = test

boostcontrol <- trainControl(number=10)

xgbGrid <- expand.grid(
  eta = 0.3,
  max_depth = 1,
  nrounds = 50,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1, subsample = 1
)


carsxgb <-  train(CarUsage ~ .,train_boost1,trControl = boostcontrol,tuneGrid = xgbGrid,metric = "Accuracy",method = "xgbTree")


predictions_xgb<-predict(carsxgb,train_boost1)
xgb_predict = confusionMatrix(predictions_xgb,train_boost1$CarUsage)
xgb_predict

predictions_xgb<-predict(carsxgb,test_boost)
xgb_predict = confusionMatrix(predictions_xgb,test_boost$CarUsage)
xgb_predict



library(ROCR)
pred_boost = prediction(as.numeric(predictions_xgb),as.numeric(test_boost$CarUsage))
perf_boost.1=performance(pred_boost, "tpr", "fpr")
as.numeric(performance(pred_boost, "auc")@y.values)
perf = performance(pred_boost, "tpr","fpr")
plot(perf)
KS <- max(attr(perf_boost.1, 'y.values')[[1]]-attr(perf_boost.1, 'x.values')[[1]])
KS
auc <- performance(pred_boost,"auc"); 
auc <- as.numeric(auc@y.values)
auc
library(ineq)
gini.1 = ineq(final_boost$predict, type="Gini")
cat('Gini:', gini.1, '\n')



conf.mat=table(predictions_xgb,test_boost$CarUsage)
#View(conf.mat)
TN=conf.mat[1]
TP=conf.mat[4]
FP=conf.mat[2]
FN=conf.mat[3]
#accuracy
cat('Accuracy:', (TN+TP)/(TN+TP+FP+FN),'\n')
#error
cat('Error:', (FP+FN)/(TN+TP+FP+FN),'\n')
#sensitivity/recall/TPR
cat('Sensitivity/Recall/True Positive Rate:', TP/(TP+FN), '\n')
#specificity/TNR
cat('Specificity/False Positive Rate:', TN/(TN+FP),'\n')
#Predicted Positive rate/Precision
cat('Predicted Positive Rate/Precision:', TP/(TP+FP), '\n')
#Predicted Negative rate
cat('Predicted Negative Rate:', TN/(TN+FN), '\n')

########################
