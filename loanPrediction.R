library(plyr)
library(caret)
library(car)
library(rpart)
library(e1071)
library(outliers)

setwd("D:/Raaghul/Data Science Complete/AV/Loan Prediction")
Loan_train <- data.table(read.csv("train.csv",header =TRUE))
Loan_test <- data.table(read.csv("test.csv",header =TRUE))



Missing_Values<-function(A)
{
  A$Gender=NULL
  A$Dependents=NULL
  A$Loan_ID=NULL
  A$Self_Employed=NULL
  A$Loan_Amount_Term=NULL
  # A$Gender[A$Gender==""]=NA
  # A$Gender = as.numeric(A$Gender)
   A$Married[A$Married==""]=NA
   A$Married = as.numeric(A$Married)
  # A$Dependents[A$Dependents==""]=NA
  # A$Dependents = as.numeric(A$Dependents)

  A$Education = as.numeric(A$Education)
  # A$Self_Employed[A$Self_Employed==""]=NA
  # A$Self_Employed = as.numeric(A$Self_Employed)
  A$Property_Area = as.numeric(A$Property_Area)  
  
  
# gender=as.character(A$Gender)
# gender[gender==""]="Others"
# A$Gender=factor(gender)
# 
# Married=as.character(A$Married)
# Married[Married==""]="Maybe"
# A$Married=factor(Married)
# 
# Dependents=as.character(A$Dependents)
# Dependents[Dependents==""]=0
# A$Dependents=factor(Dependents)
# 
# Self_Employed=as.character(A$Self_Employed)
# Self_Employed[Self_Employed==""]="No"
# A$Self_Employed=factor(Self_Employed)

  require(mice)
  set.seed(145)
  A=complete(mice(A))

A
}



# Ol<-scores(Train$ApplicantIncome, type="z", prob=0.95)
# Train[Ol,7]<-NA
# namean<- mean(Train[,7],na.rm = TRUE)
# Train[is.na(Train[,7]),7]=namean
# 
# Ol<-scores(Train$CoapplicantIncome, type="z", prob=0.95)
# Train[Ol,8]<-NA
# namean<- mean(Train[,8],na.rm = TRUE)
# Train[is.na(Train[,8]),8]=namean
# 
# Ol<-scores(Train$LoanAmount, type="z", prob=0.95)
# Train[!is.na(Train[Ol,9]),9]<-NA
# namean<- mean(Train[,9],na.rm = TRUE)
# Train[is.na(Train[,9]),9]=namean
# 
# Ol<-scores(Train$Loan_Amount_Term, type="z", prob=0.95)
# Train[!is.na(Train[Ol,10]),10]<-NA
# namean<- mean(Train[,10],na.rm = TRUE)
# Train[is.na(Train[,10]),10]=namean


#Train[Train[is.na(Train[,11]),13]=="Y",11]=1
#Train[Train[is.na(Train[,11]),13]=="N",11]=1


#Ol<-scores(Train$CoapplicantIncome, type="z", prob=0.95)

#Train[Ol,8]<-NA

Loan_Status=Loan_train$Loan_Status
Loan_train$Loan_Status<-NULL
Loan_train$isTest<-0
Loan_ID<-Loan_test$Loan_ID
Loan_test$isTest<-1


Comb<-rbind(Loan_train,Loan_test)

Comb<-Missing_Values(Comb)

Train<-subset(Comb,Comb$isTest==0)
Test<-subset(Comb,Comb$isTest==1)

Train$isTest<-NULL
Test$isTest<-NULL
#Train=Train[2:12]

#ch<-Train[,6:10]

#im<-mice(data = ch, m = 5, method = "pmm", maxit = 50, seed = 500)

#Train[is.na(Train[,10]),10]<-im$imp$Credit_History[,1]

Train$Loan_Status=Loan_Status
Test$Loan_ID<-Loan_ID

#model <- glm(Loan_Status ~.,family='binomial',data=Train)
#predicted= predict(model,Test,type="response")

#summary(model)
#predicted[predicted<0.5]="N"
#predicted[predicted>=0.5]="Y"
#loanid=Test$Loan_ID
#Output=data.frame(loanid,predicted)

summary(Train)

dt_model<-rpart(Loan_Status~.,method="class",data = Train,control=rpart.control(minsplit=50, cp=0.01))
#Testdata<-data.frame(Test$Credit_History,Test$LoanAmount,Test$Married,Test$Education,Test$CoapplicantIncome)

predict_dt=predict(dt_model,newdata=Test,type = "class")

Mysubmission = data.frame(Loan_ID = Loan_ID, Loan_Status = predict_dt)
write.csv(Mysubmission,"Result_tree.csv", row.names = FALSE)

svm=svm(Loan_Status~.,data=Train)
pred_svm=predict(svm,newdata=Test)

#Output=data.frame(Test$Loan_ID,predict_dt)

#Output=data.frame(Test$Loan_ID,svm_predict)

Mysubmission = data.frame(Loan_ID = Loan_ID, Loan_Status = pred_svm)
write.csv(Mysubmission,"Result_svm.csv", row.names = FALSE)



require(randomForest)
forest=randomForest(Loan_Status~.,data=Train,ntree=18,nodesize=10,cp=.19)
pred_rf=predict(forest,newdata = Test)
Mysubmission = data.frame(Loan_ID = Loan_ID, Loan_Status = pred_rf)
write.csv(Mysubmission,"Submission_rf.csv", row.names = FALSE)
