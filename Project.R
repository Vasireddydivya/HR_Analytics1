library(readr)
library(dplyr)
library(magrittr)
library(leaps)
library(lars)
library(caret)
library(ROCR)
library(rpart)
library(randomForest)
library(pROC)
library(e1071)

HR_comma_sep <- read.csv("C:/Users/vasir/Downloads/HR_comma_sep.csv")
HR_comma_sep<-data.frame(HR_comma_sep)

#2.Renaming the variables names
colnames(HR_comma_sep)[9]<-"Department"

#3.Add unique identifier for each employee
HR_comma_sep["ID"]<-seq.int(nrow(HR_comma_sep))
length(HR_comma_sep)
HR_comma_sep<-HR_comma_sep[colnames(HR_comma_sep)[c(11,1:10)]]

#10.finding the NA values in the table
sum(is.na(HR_comma_sep))

#8.Finding the distribution for numeric variables
par(mfrow=c(3,3))
for(i in 2:6){hist(HR_comma_sep[,i],xlab=names(HR_comma_sep)[i])}

#1.Coventing the variables to proper data type
HR_comma_sep$left=as.factor(HR_comma_sep$left)
HR_comma_sep$salary<-as.factor(HR_comma_sep$salary)
HR_comma_sep$Work_accident<-as.factor(HR_comma_sep$Work_accident)
HR_comma_sep$Department<-as.factor(HR_comma_sep$Department)
HR_comma_sep$promotion_last_5years<-as.factor(HR_comma_sep$promotion_last_5years)
#2 converting the salary to ordinal variable
HR_Comma$salary<-ordered(HR_Comma$salary,levels=c("low","medium","high"))

#15. finding the descriptive statistics
summary(HR_comma_sep)

#4.finding distributions for variables
ggplot(HR_comma_sep,aes(x=salary,y=satisfaction_level,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = "black")+xlab("salary")+ylab("Satisfaction Level")

ggplot(HR_comma_sep,aes(x=factor(time_spend_company),y=average_montly_hours,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = NA)+xlab("Time Spend Company")+ylab("Average Monthly Hours")


#5. Finding the correlation between variables
nums<-sapply(HR_comma_sep,is.numeric)
cor_matrix<-cor(HR_comma_sep[,nums])
corrplot(cor_matrix,method = 'number')
HR_Corr<-HR_comma_sep %>% select(satisfaction_level:promotion_last_5years)


#Cp model
model.mat<-model.matrix(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+Department+salary,data=HR_comma_sep)
sb<-leaps(x=model.mat[,2:19],y=HR_comma_sep[,7],method = 'Cp')
plot(sb$size,sb$Cp,pch=19)
sb$which[which(sb$Cp==min(sb$Cp)),]

#Forward selection and Backward Selection

forward_fit<-lm(left~1,data = HR_comma_sep)
fit.forward<-step(forward_fit,scope = list(lower=left~1,upper=left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+sales+salary),direction = 'forward')
summary(fit.forward)

fit.backward<-lm(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+sales+salary,data = HR_comma_sep)
fit.back<-step(fit.backward,scope = list(lower=left~1,upper=left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+sales+salary),direction = 'backward')
summary(fit.back)

#LASSO
Xvars<-HR_comma_sep[,c(1:6,8:10)]
YVars<-HR_comma_sep[,7]

fit.lasso<-lars(x=as.matrix(model.mat[,2:19]),y=as.matrix(YVars),type = 'lasso')
plot(fit.lasso)

#Exploration in Data

#Number of projects and average monthly hours are correlated. So find average time for spending time on single project

#creating new column for finding the effecient employees
HR_comma_sep['avg_hr_prj']<-(HR_comma_sep['average_montly_hours'] * 12)/HR_comma_sep['number_project']

#Dividing the variable into 3 parts
HR_comma_sep['avg_hr_prj_range']<-cut(HR_comma_sep$avg_hr_prj,3)

#Assigning a variable with labels 0, 1, 2 according to monthly hours spent range
HR_comma_sep['HR_Cat']<-cut(HR_comma_sep$avg_hr_prj,3,labels = c(0:2))

#plotting the left and average monthly hours
ggplot(HR_comma_sep,aes(factor(left),average_montly_hours))+geom_boxplot(outlier.colour = "green", outlier.size = 3)
ggplot(HR_comma_sep,aes(factor(left),time_spend_company))+geom_boxplot(outlier.colour = "green", outlier.size = 3)+xlab("Left")+ylab("Time Spend Company")

ggplot(HR_comma_sep,aes(sales))+geom_bar(aes(fill=factor(left)),position='dodge')
# we observe that the highest employees left from the company belong to departments 'Management' and 'RandD'

ggplot(HR_comma_sep,aes(sales))+geom_bar(aes(fill=factor(time_spend_company)),position='dodge')
#More number of employee from Management and sales are spending more than 8 years in the company compared to other departments. So we cannot
#remove outliers.

# There are few outliers in the data set. So we cannotignore these observations because employees who are spending more than
#8 years in the company are from sales and management. As 'sales' and 'management' are playing important role in this comapnay.
dropdata<-subset(HR_comma_sep,time_spend_company<8)
HR_comma_sep1<-dropdata

left=dropdata[(dropdata$left==1),]
non_left=dropdata[(dropdata$left==0),]
ggplot(left,aes(time_spend_company))+geom_histogram(binwidth = 0.5)+xlab("Time Spend at the company")+ylab("Number of Observations")+ggtitle("left")
ggplot(non_left,aes(time_spend_company))+geom_histogram(binwidth = 0.5)+xlab("Time Spend at the company")+ylab("Number of Observations")+ggtitle("Not left")

# 1. From the above plots we can say that, people who work more than 6 years and who work for 2 years are less likely to leave
# 2. People are more likely to leave when they spend 3 to 5 years
# 3. People with 5-years are more likely to leave
# 4. When the years people spent in the company lies in 3-5: the more they've been here, the more likely they leave.

ggplot(dropdata,aes(x=time_spend_company,y=left,fill=factor(promotion_last_5years),colour=factor(promotion_last_5years)))+geom_bar(position='stack', stat='identity')+xlab("Time Spend in Company")+ylab("promotion in last 5 years")

#Very less people got promoted even though they are spending more time in the office

ggplot(dropdata,aes(x=salary,y=time_spend_company,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = NA)+xlab("salary")+ylab("Time Spend Company")

# The low and medium income people are leaving the company

HR_comma_sep1['avg_hr_prj']<-(HR_comma_sep1['average_montly_hours'] * 12)/HR_comma_sep1['number_project']
HR_comma_sep1['avg_hr_prj_range']<-cut(HR_comma_sep1$avg_hr_prj,3)


#who are valuable employess??
#The evaluation criteria and Monthly hours spend in the company are considered as valuable. Here we are not considering the 
#promotion because very less people got promoted in last 5 years.

#For our analysis we are finding the average time an employee spent on each project. Then, we converted the variable into 3 levels.
#In general an employee must work for 160 hours per month. We have splitted this variable into 3 levels and then according to the level
#we have given categories as [0,1,2]

b1<-HR_comma_sep$last_evaluation > 0.5
b2<-HR_comma_sep$HR_Cat==1 | HR_comma_sep$HR_Cat==2
sum(b1 & b2)

#There are total of 4386 valuable employees

#Decide who all are valuable employees
HR_comma_sep['valuedEmployee']<-0
head(HR_comma_sep)

for (i in (1: nrow(HR_comma_sep))){
  b1<-(HR_comma_sep[i,'last_evaluation'] > 0.5)
  b2<-((HR_comma_sep[i,'HR_Cat']==1) | (HR_comma_sep[i,'HR_Cat']==2))
  if(b1 & b2){
    HR_comma_sep[i,'valuedEmployee'] = 1
  }   
}

#finding the number of employees in each department
lev<-levels(as.factor(HR_comma_sep$sales))
for(i in (1:length(lev))){
      lev[i]<-sum(grepl(lev[i],HR_comma_sep$sales))
}
lev_list['Department']<-as.data.frame(levels(as.factor(HR_comma_sep$sales)))
lev_list['number_of_employees']<-lev

left_employees<-HR_comma_sep[(HR_comma_sep$left==1),]
lev_left<-levels(as.factor(left_employees$sales))
for(i in (1:length(lev_left))){
       lev_left[i]<-sum(grepl(lev_left[i],left_employees$sales))
   }
lev_list['number_of_employees_left']<-lev_left

f<-function(x,y) as.numeric(x)/as.numeric(y)
p<-as.data.frame(mapply(f,lev_list[,2],lev_list[,3]))
lev_list['percent']<-p[,1]

lev_lis<-function(df,colname){
  df<-df %>% 
  lev<-levels(as.factor(dataframe['sales']))
  for(i in (1:length(lev))){
    lev[i]<-sum(grepl(lev[i],dataframe['sales']))
  }
}

#Fitting models

#Stratified sampling
xvars=c('satisfaction_level','last_evaluation','number_project','average_montly_hours','time_spend_company','Work_accident','promotion_last_5years','sales','salary')
yvars='left'
p1<-0.8
set.seed(12345)
inTrain<-createDataPartition(y=HR_comma_sep[,yvars],p=p1,list=FALSE)
train_HR<-HR_comma_sep[inTrain,]
test_HR<-HR_comma_sep[-inTrain,]
stopifnot(nrow(train_HR)+nrow(test_HR)==nrow(HR_comma_sep))

#Fitting GLM
glm.fit<-glm(left~.,data=train_HR,family = binomial(link="logit"))
summary(glm.fit)

plot(glm.fit)


#confusion matrix
test_HR[,'Yhat']<-predict(glm.fit,newdata=test_HR)
fitted.values<-test_HR[,'Yhat']
test_HR$Yhat<-ifelse( test_HR$Yhat>0.5,1,0)
conf<-confusionMatrix(test_HR$Yhat,test_HR$left)
conf

#ROC Curve
fit_values<-prediction(fitted.values,test_HR$left)
p<-performance(fit_values,measure = 'tpr',x.measure = 'fpr')
plot(p)
abline(0, 1, lty = 2)

#CART implementation
cart.fit<-rpart(left~.,data=train_HR,method='class')
summary(cart.fit)

#predicting using CART model
fit.values.cart<-predict(cart.fit,newdata = test_HR)
fit.val1<-ifelse(fit.values.cart[,1]>0.5,1,0)
fit.val2<-ifelse(fit.values.cart[,2]>0.5,1,0)

conf.cart<-confusionMatrix(fit.val2,test_HR$left)
conf.cart

p.cart<-prediction(fit.values.cart[,2],test_HR$left)
p.cart<-performance(p.cart,measure = 'tpr',x.measure = 'fpr')
plot(p.cart)
abline(0,1,lty=2)

#Fitting random forest
fit_rf<-randomForest(as.factor(left)~.,data=train_HR,importance=TRUE,ntree=1000)
fit_rf$confusion

#confusion matrix for random forest
fitted.values.rf<-predict(fit_rf,newdata = test_HR,type='class')
fitted.values.rf1<-predict(fit_rf,newdata = test_HR,type='prob')
conf.rf<-confusionMatrix(fitted.values.rf,test_HR$left)
conf.rf

#ROC curve for random forest
HR.rf<-roc(test_HR$left, fitted.values.rf1[,2])
plot(HR.rf, print.auc=TRUE, auc.polygon=TRUE)

#Fitting SVM algorithm
svm_model<-svm(left~.,data=train_HR,type='C-classification')
svm_model1<-svm(left~.,data=train_HR,type='C-classification',probability = TRUE)
summary(svm_model)

#predicting values and confusion matrix
pred<-predict(svm_model,newdata = test_HR)
pred.prob<-predict(svm_model1,newdata = test_HR,type='prob',probability = TRUE)
conf.svm<-confusionMatrix(pred,test_HR$left)
conf.svm

#ROC curve for SVM
p.svm<-prediction(attr(pred.prob,"probabilities")[,2],test_HR$left)
svm.perf<-performance(p.svm,measure = 'tpr',x.measure = 'fpr')
plot(svm.perf,add=TRUE,col=6)

#K-means clusttering
