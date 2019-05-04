library(dplyr)
library(plyr)
library(ggplot2)
library(nnet)
install.packages("irr")
library(irr)
setwd("E:/Data Science/R for Data Science/Datasets")
dat<-read.csv("iris.data")
colnames(dat)<-c("sepal_length_in_cm","sepal_width_in_cm","petal_length_in_cm","petal_width_in_cm","class")
head(dat)
summary(dat)
unique(dat$class)
boxplot(dat$sepal_length_in_cm)
x<-boxplot(dat$sepal_width_in_cm)
Out<-x$out
index<-which(dat$sepal_width_in_cm%in%x$out)
dat$sepal_width_in_cm[index]<-mean(dat$sepal_width_in_cm,na.rm=T)
boxplot(dat$sepal_width_in_cm)
boxplot(dat$petal_length_in_cm)
boxplot(dat$petal_width_in_cm)

index1<-sample(nrow(dat),0.7*nrow(dat),replace = F)
train<-dat[index1,]
test<-dat[-index1,]

train$class1<-ifelse(train$class=="Iris-setosa",1,ifelse(train$class=="Iris-versicolor",2,3))
table(train$class1)/nrow(train)
kappa2(data.frame(test$class1,pred))
test$class1<-ifelse(test$class=="Iris-setosa",1,ifelse(test$class=="Iris-versicolor",2,3))
test$class1<-as.factor(test$class1)
table(test$class1)/nrow(test)

mod<-multinom(train$class1~.,data=train)
summary(mod)

pred<-(predict(object = mod,newdata = test))
pred<-as.factor(pred)
kappa2
as.matrix(table(Actual=test$class1,Predicted=pred))
dat$class1<-ifelse(dat$class=="Iris-setosa",1,ifelse(dat$class=="Iris-versicolor",2,3))
dat$pred1<-predict(object = mod,newdata = dat)
as.matrix(table(Actual=dat$class1,Predicted=pred1))

