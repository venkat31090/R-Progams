library(dplyr)
library(plyr)
library(ggplot2)
library(irr)
library(caret)

#Input data
setwd("E:/Data Science/R for Data Science/Datasets/Loan Approval")
train<-read.csv("train.csv")
test<-read.csv("test.csv")

summary(train)
str(train)

#Convert variables 
train$ApplicantIncome<-as.numeric(train$ApplicantIncome)
train$LoanAmount<-as.numeric(train$LoanAmount)
train$Credit_History<-as.factor(train$Credit_History)
train$Loan_Status<-(ifelse(train$Loan_Status=="Y",1,0))
train$Loan_Status<-as.factor(train$Loan_Status)
str(train)

#Missing Value Imputation
ind<-which(is.na(train$LoanAmount))
train$LoanAmount[ind]<-mean(train$LoanAmount,na.rm = T)
ind3<-which(train$Gender=="")
train$Gender[ind3]<-"Male"

table(train$Dependents)
ind1<-which(train$Dependents=="")
train$Dependents[ind1]<-"0"

ind2<-which(train$Married=="")
train$Married[ind2]<-"Yes"

train$Credit_History[is.na(train$Credit_History)]<-"1"

table(train$Self_Employed,train$Loan_Status)
#since maximum self employed category is No for whom loan has been approved, we replace blanks with No
ind6<-which(train$Self_Employed=="")
train$Self_Employed[ind6]<-"No"

boxplot(train$LoanAmount)
boxplot(train$ApplicantIncome)
boxplot(train$CoapplicantIncome)
#log transformation
train$LoanAmount<-log(train$LoanAmount)
train$Total_LoanIncome<-log(train$ApplicantIncome,train$CoapplicantIncome)

train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)]<-mean(train$Loan_Amount_Term,na.rm = T)
summary(train)

mod1<-glm(Loan_Status~Gender+Married+Dependents++Education+Self_Employed+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+Total_LoanIncome,data = train,family = "binomial")
summary(mod1)

train$Married_yes<-ifelse(train$Married=="Yes",1,0)

train$Credit_History_0<-ifelse(train$Credit_History=="0",1,0)

train$Property_Area_semiurban<-ifelse(train$Property_Area=="Semiurban",1,0)

mod3<-glm(Loan_Status~Married_yes+Credit_History_0+Property_Area_semiurban,data = train,family = "binomial")
summary(mod3)


#test Prediction

test$ApplicantIncome<-as.numeric(test$ApplicantIncome)
test$LoanAmount<-as.numeric(test$LoanAmount)
test$Credit_History<-as.factor(test$Credit_History)
str(test)

#Missing Value Imputation
ind11<-which(is.na(test$LoanAmount))
test$LoanAmount[ind11]<-mean(test$LoanAmount,na.rm = T)
ind13<-which(test$Gender=="")
test$Gender[ind13]<-"Male"

table(test$Dependents)
ind14<-which(test$Dependents=="")
test$Dependents[ind14]<-"0"

ind12<-which(test$Married=="")
test$Married[ind12]<-"Yes"

test$Credit_History[is.na(test$Credit_History)]<-"1"
#since maximum self employed category is No for whom loan has been approved, we replace blanks with No
ind16<-which(test$Self_Employed=="")
test$Self_Employed[ind16]<-"No"
test$Loan_Amount_Term[is.na(test$Loan_Amount_Term)]<-mean(test$Loan_Amount_Term,na.rm = T)

test$Married_yes<-ifelse(test$Married=="Yes",1,0)
test$Credit_History_0<-ifelse(test$Credit_History=="0",1,0)
test$Property_Area_semiurban<-ifelse(test$Property_Area=="Semiurban",1,0)

summary(test)
test$pred<-predict(mod3,type = "response",newdata = test)
test$pred<-ifelse(test$pred>=0.68,1,0)
summary(test$pred)
table(train$Loan_Status)/nrow(train)
table(test$pred)
write.csv(test,file = "test_pred.csv")

#accuracy
pred1<-predict(mod3,type = "response",newdata = train)
pred1<-ifelse(pred1>=0.68,1,0)

kappa2(data.frame(train$Loan_Status,pred1))

table(pred1,train$Loan_Status)
library(ROCR)
pred2<-prediction(pred1,train$Loan_Status)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred2,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

