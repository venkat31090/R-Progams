library(dplyr)
library(plyr)
library(ggplot2)
library(irr)
library(caret)

setwd("E:/Data Science/R for Data Science/Datasets/K-Means")
data<-read.csv("K-Means.csv")
summary(data)
plot(data$Sepal.Width,plot(data$ï..Sepal.Length))

#standardising data

mydata<-as.data.frame(scale(data))


#No of CLusters

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for(i in 1:25)
  {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
  }

plot(1:25, wss, type="b", xlab="No. of Clusters", ylab="wss")

wss


#assuming 4 clusters
clus4<-kmeans(mydata,centers = 4,nstart = 30)
clus4

aggregate(mydata,by=list(clus4$cluster),FUN = mean)
mydata$cluster<-as.data.frame(clus4$cluster)

plot(mydata$Sepal.Width,mydata$ï..Sepal.Length,col=clus4$cluster)
