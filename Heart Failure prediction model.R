#import the dataset and the dataset is available in https://www.kaggle.com/andrewmvd/heart-failure-clinical-data/download
df<-read.csv(file.choose(),header = T)
View(df)

#observe the data structure
str(df)
df$anaemia<-as.factor(df$anaemia)
df$diabetes<-as.factor(df$diabetes)
df$high_blood_pressure<-as.factor(df$high_blood_pressure)
df$sex<-as.factor(df$sex)
df$smoking<-as.factor(df$smoking)
df$DEATH_EVENT<-as.factor(df$DEATH_EVENT)
str(df)

# view the summary of the data
summary(df)

#delete the outliers
boxplot(df$creatinine_phosphokinase)
uv<-quantile(df$creatinine_phosphokinase,0.9)
df$creatinine_phosphokinase[df$creatinine_phosphokinase>uv]<-uv
boxplot(df$creatinine_phosphokinase)
summary(df$creatinine_phosphokinase)
boxplot(df$platelets)
uf<-quantile(df$platelets,0.92)
df$platelets[df$platelets>uf]<-uf
boxplot(df$platelets)
summary(df$platelets)

#spliting the data in 70:30
set.seed(0)
ind<-sample(2,nrow(df),replace = T,prob = c(0.7,0.3))
train<-df[ind==1,]
test<-df[ind==2,]
summary(train)

#fit the LDA model in the data
library(MASS)
linear.fit<-lda(DEATH_EVENT~.,train)
linear.fit
attributes(linear.fit)

#Histogram
p<-predict(linear.fit,train)
ldahist(data = p$x[,1], g = train$DEATH_EVENT)

#compute confusion matrix and model accuracy for train data
p1<-predict(linear.fit,train)$class
pred<-table(predicted=p1,actual=train$DEATH_EVENT)
pred
accuracy<-sum(diag(pred))/sum(pred)
accuracy #model accuracy for train data is 86.11111%

#compute confusion matrix and model accuracy for test data
p2<-predict(linear.fit,test)$class
pred1<-table(predicted=p2,actual=test$DEATH_EVENT)
pred1
accuracy<-sum(diag(pred1))/sum(pred1)
accuracy # model accuracy for test data is 80.72289%
