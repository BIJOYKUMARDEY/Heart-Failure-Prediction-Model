#Import the data of credit card
df<-read.csv(file.choose(),header=T)
#checking the data structure
str(df)
df$Class<-as.factor(df$Class)             
str(df)
# checking the summary of data
summary(df)
#checking the scatter plot of class and v5 variable as there is a huge difference of first quartile and min value
pairs(~df$Class+df$V5)
#deleting the outliers of v5
uv<-3*quantile(df$V5,0.01)
df$V5[df$V5<uv]<-uv
pairs(~df$Class+df$V5)
#spliting the data in 80:20
set.seed(0)
split<-sample(2,nrow(df),replace=T,prob=c(0.8,0.2))
train<-df[split==1,]
test<-df[split==2,]
str(train)
summary(train)
#fitting the logistic regression and take the final columns after deleting all insignificant variables
library(class)
logit<-glm(Class~Time+V1+V3+V4+V5+V6+V8+V9+V10+V12+V13+V14+V15+V16+V17+V20+V21+V22+V24+V25+V27+V28+Amount,data=train,family='binomial')
summary(logit)
train<-train[,c(-3,-8,-12,-19,-20,-24,-27)]
#finding the confusion matrix and accuracy rate of the train data
trainval<-predict(logit,train,type='response')
train.pred<-rep("no",227949)
train.pred[trainval>0.5]<-"yes"
tab1<-table(predicted=train.pred,actual=train$Class)
tab1
accuracy<-sum(diag(tab1))/sum(tab1)#accuracy rate is 99.9263%
accuracy
#finding the accuracy rate and confusion matrix of the test data
test<-test[,c(-3,-8,-12,-19,-20,-24,-27)]
testval<-predict(logit,test,type = 'response')
test.pred<-rep("no",56858)
test.pred[testval>0.5]<-"yes"
tab2<-table(predicted=test.pred,actual=test$Class)
tab2
accuracy<-sum(diag(tab2))/sum(tab2)#accuracy rate is 99.91382%
accuracy
