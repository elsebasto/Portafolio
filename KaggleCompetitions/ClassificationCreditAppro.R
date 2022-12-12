#----------------------------------------------librerias-------------------------------------------------------#
library(readxl)
library(e1071)
library(EnvStats)
library(corrplot)
library(mvoutlier)
library(DMwR)
library(readxl)
library(readxl)
library(e1071)
library(EnvStats)
library(corrplot)
library(readxl)
library(Boruta)
library(corrplot)
library(DMwR)
library(psych)
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(caret)
library(pROC)
library(gam)
library(mgcv)
library(readxl)
library(splines)
library(glmnet)
library(mvoutlier)
library(e1071)
library(neuralnet)
library(ParBayesianOptimization)
library(xgboost)
TrainClas <- read_excel("C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/TrainClas.xlsx")
View(TrainClas)
TestClas <- read_excel("Competencia/TestClas.xlsx")
head(TestClas)

##Have a look at data

#Numer of rows
N=nrow(TrainClas)
#Number of columns
C=ncol(TrainClas)
#dimensionality
dim(TrainClas)
#Column names
names(TrainClas)
#Structure of data
str(TrainClas)
#attributes of data
attributes(TrainClas)



#LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
summary(TrainClas$LIMIT_BAL)
var(TrainClas$LIMIT_BAL) 
hist(TrainClas$LIMIT_BAL)
boxplot(TrainClas$LIMIT_BAL~TrainClas$default.payment.next.month)
skewness(TrainClas$LIMIT_BAL) #No box cox
sum(is.na(TrainClas$LIMIT_BAL))
sum(is.nan(TrainClas$LIMIT_BAL))


#SEX: Gender (1=male, 2=female)

pie(table(TrainClas$SEX))
barplot(table(TrainClas$SEX))
sum(is.na(TrainClas$SEX))
sum(is.nan(TrainClas$SEX))

#EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)

pie(table(TrainClas$EDUCATION))
barplot(table(TrainClas$EDUCATION))
sum(is.na(TrainClas$EDUCATION))
sum(is.nan(TrainClas$EDUCATION))
TrainClas$EDUCATION<-ifelse(TrainClas$EDUCATION==0,5,ifelse(TrainClas$EDUCATION==6,5,TrainClas$EDUCATION))
TestClas$EDUCATION<-ifelse(TestClas$EDUCATION==0,5,ifelse(TestClas$EDUCATION==6,5,TestClas$EDUCATION))
table(TrainClas$EDUCATION)

#MARRIAGE: Marital status (1=married, 2=single, 3=others)

pie(table(TrainClas$MARRIAGE))
barplot(table(TrainClas$MARRIAGE))
sum(is.na(TrainClas$MARRIAGE))
sum(is.nan(TrainClas$MARRIAGE))
TrainClas$MARRIAGE<-ifelse(TrainClas$MARRIAGE==0,3,TrainClas$MARRIAGE)
TestClas$MARRIAGE<-ifelse(TestClas$MARRIAGE==0,3,TestClas$MARRIAGE)
table(TrainClas$MARRIAGE)
table(TestClas$MARRIAGE)

#AGE: Age in years
summary(TrainClas$AGE)
var(TrainClas$AGE) 
hist(TrainClas$AGE)
boxplot(TrainClas$AGE~TrainClas$default.payment.next.month)
skewness(TrainClas$AGE) #No box cox
sum(is.na(TrainClas$AGE))
sum(is.nan(TrainClas$AGE))

#PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, � 8=payment delay for eight months, 9=payment delay for nine months and above)

pie(table(TrainClas$PAY_0))
barplot(table(TrainClas$PAY_0))
sum(is.na(TrainClas$PAY_0))
sum(is.nan(TrainClas$PAY_0))

#PAY_2: Repayment status in August, 2005 (scale same as above)

pie(table(TrainClas$PAY_2))
barplot(table(TrainClas$PAY_2))
sum(is.na(TrainClas$PAY_2))
sum(is.nan(TrainClas$PAY_2))

#PAY_3: Repayment status in July, 2005 (scale same as above)

pie(table(TrainClas$PAY_3))
barplot(table(TrainClas$PAY_3))
sum(is.na(TrainClas$PAY_3))
sum(is.nan(TrainClas$PAY_3))

#PAY_4: Repayment status in June, 2005 (scale same as above)

pie(table(TrainClas$PAY_4))
barplot(table(TrainClas$PAY_4))
sum(is.na(TrainClas$PAY_4))
sum(is.nan(TrainClas$PAY_4))

#PAY_5: Repayment status in May, 2005 (scale same as above)

pie(table(TrainClas$PAY_5))
barplot(table(TrainClas$PAY_5))
sum(is.na(TrainClas$PAY_5))
sum(is.nan(TrainClas$PAY_5))

#PAY_6: Repayment status in April, 2005 (scale same as above)

pie(table(TrainClas$PAY_6))
barplot(table(TrainClas$PAY_6))
sum(is.na(TrainClas$PAY_6))
sum(is.nan(TrainClas$PAY_6))

#BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)

summary(TrainClas$BILL_AMT1)
var(TrainClas$BILL_AMT1) 
hist(TrainClas$BILL_AMT1)
boxplot(TrainClas$BILL_AMT1~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT1) #box cox
sum(is.na(TrainClas$BILL_AMT1))
sum(is.nan(TrainClas$BILL_AMT1))

#BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)

summary(TrainClas$BILL_AMT2)
var(TrainClas$BILL_AMT2) 
hist(TrainClas$BILL_AMT2)
boxplot(TrainClas$BILL_AMT2~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT2) #box cox
sum(is.na(TrainClas$BILL_AMT2))
sum(is.nan(TrainClas$BILL_AMT2))

#BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)

summary(TrainClas$BILL_AMT3)
var(TrainClas$BILL_AMT3) 
hist(TrainClas$BILL_AMT3)
boxplot(TrainClas$BILL_AMT3~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT3) #box cox
sum(is.na(TrainClas$BILL_AMT3))
sum(is.nan(TrainClas$BILL_AMT3))

#BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)

summary(TrainClas$BILL_AMT4)
var(TrainClas$BILL_AMT4) 
hist(TrainClas$BILL_AMT4)
boxplot(TrainClas$BILL_AMT4~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT4) #box cox
sum(is.na(TrainClas$BILL_AMT4))
sum(is.nan(TrainClas$BILL_AMT4))

#BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)

summary(TrainClas$BILL_AMT5)
var(TrainClas$BILL_AMT5) 
hist(TrainClas$BILL_AMT5)
boxplot(TrainClas$BILL_AMT5~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT5) #box cox
sum(is.na(TrainClas$BILL_AMT5))
sum(is.nan(TrainClas$BILL_AMT5))

#BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)

summary(TrainClas$BILL_AMT6)
var(TrainClas$BILL_AMT6) 
hist(TrainClas$BILL_AMT6)
boxplot(TrainClas$BILL_AMT6~TrainClas$default.payment.next.month)
skewness(TrainClas$BILL_AMT6) #box cox
sum(is.na(TrainClas$BILL_AMT6))
sum(is.nan(TrainClas$BILL_AMT6))

#PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)

summary(TrainClas$PAY_AMT1)
var(TrainClas$PAY_AMT1) 
hist(TrainClas$PAY_AMT1) #esta cosa esta rara
boxplot(TrainClas$PAY_AMT1~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT1) #box cox
sum(is.na(TrainClas$PAY_AMT1))
sum(is.nan(TrainClas$PAY_AMT1))


#PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)

summary(TrainClas$PAY_AMT2)
var(TrainClas$PAY_AMT2) 
hist(TrainClas$PAY_AMT2) #esta cosa esta rara
boxplot(TrainClas$PAY_AMT2~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT2) #box cox
sum(is.na(TrainClas$PAY_AMT2))
sum(is.nan(TrainClas$PAY_AMT2))

#PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)

summary(TrainClas$PAY_AMT3)
var(TrainClas$PAY_AMT3) 
hist(TrainClas$PAY_AMT3)
boxplot(TrainClas$PAY_AMT3~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT3) #box cox
sum(is.na(TrainClas$PAY_AMT3))
sum(is.nan(TrainClas$PAY_AMT3))

#PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)

summary(TrainClas$PAY_AMT4)
var(TrainClas$PAY_AMT4) 
hist(TrainClas$PAY_AMT4)
boxplot(TrainClas$PAY_AMT4~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT4) #box cox
sum(is.na(TrainClas$PAY_AMT4))
sum(is.nan(TrainClas$PAY_AMT4))

#PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)

summary(TrainClas$PAY_AMT5)
var(TrainClas$PAY_AMT5) 
hist(TrainClas$PAY_AMT5)
boxplot(TrainClas$PAY_AMT5~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT5) #box cox
sum(is.na(TrainClas$PAY_AMT5))
sum(is.nan(TrainClas$PAY_AMT5))

#PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)

summary(TrainClas$PAY_AMT6)
var(TrainClas$PAY_AMT6) 
hist(TrainClas$PAY_AMT6)
boxplot(TrainClas$PAY_AMT6~TrainClas$default.payment.next.month)
skewness(TrainClas$PAY_AMT6) #box cox
sum(is.na(TrainClas$PAY_AMT6))
sum(is.nan(TrainClas$PAY_AMT6))

#default.payment.next.month: Default payment (1=yes, 0=no)

table(TrainClas$default.payment.next.month) #imbalanceo de clases 


#----------------------------------------------NA vlues, correlacion y variables a transformar---------------------------------------------------#

sum(is.na(xx)) # no hay na



xx<-cbind(TrainClas$PAY_AMT1,TrainClas$PAY_AMT2,TrainClas$PAY_AMT3,TrainClas$PAY_AMT4,TrainClas$PAY_AMT5,TrainClas$PAY_AMT6,
          TrainClas$BILL_AMT1,TrainClas$BILL_AMT2,TrainClas$BILL_AMT3,TrainClas$BILL_AMT4,TrainClas$BILL_AMT5,TrainClas$BILL_AMT6,
          TrainClas$AGE,TrainClas$LIMIT_BAL)
co=cor(xx)
corrplot(co) 
mm=0
Pcor<-matrix(c(rep(0,26*2)), ncol = 2)
for(i in 1: nrow(co))
{
  for(j in 1:ncol(co))
  {
    if(co[i,j]>0.9)
    {
      mm=mm+1
      Pcor[mm,1]=i
      Pcor[mm,2]=j
    }
  }
}
Pcor #8 y 7 ; 8 y 9 ; 10 y 9 ; 10 y 11 , 10 y 12 ; 11 y 12
#Predictores a eliminar: 8 10 12
xx1<-cbind(TrainClas$PAY_AMT1,TrainClas$PAY_AMT2,TrainClas$PAY_AMT3,TrainClas$PAY_AMT4,TrainClas$PAY_AMT5,TrainClas$PAY_AMT6,
          TrainClas$BILL_AMT1,TrainClas$BILL_AMT3,TrainClas$BILL_AMT5,
          TrainClas$AGE,TrainClas$LIMIT_BAL)
co=cor(xx1)
corrplot(co) 
#no se puede trasformar ningun predictor



#-------------------------------------------------Outlier analysis----------------------------------------------------------#

outlier.scores <- lofactor(xx1, k=500)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:50]
print(outliers)
row.names(xx1)[outliers]
#------------------------------------------------Box-Cox--------------------------------------------------------------------#
#TrainClas$BILL_AMT2
#TrainClas$BILL_AMT4
#TrainClas$BILL_AMT6
dataN<-TrainClas[-outliers,-c(13,15,17)]
TestClas<-TestClas[,-c(13,15,17)]

hist(dataN$LIMIT_BAL)
skewness(dataN$LIMIT_BAL)

hist(dataN$AGE)
skewness(dataN$AGE)

hist(dataN$PAY_AMT1)
skewness(dataN$PAY_AMT1)
summary(dataN$PAY_AMT1)

#deteccion no parametrica de otliers
q=quantile(dataN$PAY_AMT1,0.95)
dataN$PAY_AMT1<-ifelse(dataN$PAY_AMT1>=q,q,dataN$PAY_AMT1)

q=quantile(dataN$PAY_AMT2,0.95)
dataN$PAY_AMT2<-ifelse(dataN$PAY_AMT2>=q,q,dataN$PAY_AMT2)

q=quantile(dataN$PAY_AMT3,0.95)
dataN$PAY_AMT3<-ifelse(dataN$PAY_AMT3>=q,q,dataN$PAY_AMT3)

q=quantile(dataN$PAY_AMT4,0.95)
dataN$PAY_AMT4<-ifelse(dataN$PAY_AMT4>=q,q,dataN$PAY_AMT4)

q=quantile(dataN$PAY_AMT5,0.95)
dataN$PAY_AMT5<-ifelse(dataN$PAY_AMT5>=q,q,dataN$PAY_AMT5)

q=quantile(dataN$PAY_AMT6,0.95)
dataN$PAY_AMT6<-ifelse(dataN$PAY_AMT6>=q,q,dataN$PAY_AMT6)


#------------------------------------------------Boruta---------------------------------------------------------------------#
names(dataN)
dataN<-transform(dataN,
                  LIMIT_BAL=as.numeric(LIMIT_BAL),
                  SEX= as.factor(SEX),
                  EDUCATION=as.factor(EDUCATION),
                  MARRIAGE=as.factor(MARRIAGE),
                  AGE=as.numeric(AGE),
                  PAY_0=as.factor(PAY_0),
                  PAY_2=as.factor(PAY_2),
                  PAY_3=as.factor(PAY_3),
                  PAY_4=as.factor(PAY_4),
                  PAY_5=as.factor(PAY_5),
                  PAY_6=as.factor(PAY_6),
                  BILL_AMT1= as.numeric(BILL_AMT1),
                  BILL_AMT3= as.numeric(BILL_AMT3),
                  BILL_AMT5= as.numeric(BILL_AMT5),
                  PAY_AMT1= as.numeric(PAY_AMT1),
                  PAY_AMT2= as.numeric(PAY_AMT2),
                  PAY_AMT3= as.numeric(PAY_AMT3),
                  PAY_AMT4= as.numeric(PAY_AMT4),
                  PAY_AMT5= as.numeric(PAY_AMT5),
                  PAY_AMT6= as.numeric(PAY_AMT6),
                  default.payment.next.month= as.factor(default.payment.next.month))
TestClas<-transform(TestClas,
                 LIMIT_BAL=as.numeric(LIMIT_BAL),
                 #SEX= as.factor(SEX),
                 #EDUCATION=as.factor(EDUCATION),
                 MARRIAGE=as.factor(MARRIAGE),
                 AGE=as.numeric(AGE),
                 PAY_0=as.factor(PAY_0),
                 PAY_2=as.factor(PAY_2),
                 PAY_3=as.factor(PAY_3),
                 PAY_4=as.factor(PAY_4),
                 PAY_5=as.factor(PAY_5),
                 PAY_6=as.factor(PAY_6),
                 BILL_AMT1= as.numeric(BILL_AMT1),
                 BILL_AMT3= as.numeric(BILL_AMT3),
                 BILL_AMT5= as.numeric(BILL_AMT5),
                 PAY_AMT1= as.numeric(PAY_AMT1),
                 PAY_AMT2= as.numeric(PAY_AMT2),
                 PAY_AMT3= as.numeric(PAY_AMT3),
                 PAY_AMT4= as.numeric(PAY_AMT4),
                 PAY_AMT5= as.numeric(PAY_AMT5),
                 PAY_AMT6= as.numeric(PAY_AMT6))

dt = sort(sample(nrow(dataN), nrow(dataN)*.8))
trainN<-dataN[dt,]
testN<-dataN[-dt,]

bor=Boruta(default.payment.next.month~.,data=trainN,pValue=0.05,maxRuns=100)
plot(bor)
getSelectedAttributes(bor)

trainN<-trainN[,-c(2,3)]
testN<-testN[,-c(2,3)]
dataN<-dataN[,-c(2,3)]
TestClas<-TestClas[,-c(2,3)]

levels(TestClas$MARRIAGE) <- levels(dataN$MARRIAGE)
levels(TestClas$PAY_0) <- levels(dataN$PAY_0)
levels(TestClas$PAY_2) <- levels(dataN$PAY_2)
levels(TestClas$PAY_3) <- levels(dataN$PAY_3)
levels(TestClas$PAY_4) <- levels(dataN$PAY_4)
levels(TestClas$PAY_5) <- levels(dataN$PAY_5)
levels(TestClas$PAY_6) <- levels(dataN$PAY_6)

#------------------------------------------------Despliegue de modelos------------------------------------------------------#
head(trainN)
sapply(trainN,class)

#-------------------------------------------------------bagging-------------------------------------------------------------#
bag.car=randomForest(as.factor(default.payment.next.month)~.,dataN,mtry=dim(trainN)[2]-1)

imp=importance(bag.car)
imp
varImpPlot(bag.car)

head(testN)
predi=predict(bag.car,testN,type="prob")

estesi<-rep(0,length(predi[,2]))
thres=0.5
estesi=ifelse(predi[,2]>thres,1,0)
confusionMatrix(table(estesi,testN[,19]))

roc_bag=roc(testN[,19],predi[,2])
plot(roc_bag, main ="ROC bagging")
auc_bag=auc(roc_bag)
auc_bag

#-------------------------------------------------------Random forest-------------------------------------------------------#
table(dataN$PAY_3)
table(TestClas$PAY_3)
sapply(dataN,class)
sapply(TestClas,class)

bag.car=randomForest(as.factor(default.payment.next.month)~.,dataN,mtry=sqrt(dim(dataN)[2]-1))

imp=importance(bag.car)
imp
varImpPlot(bag.car)

  
names(TestClas)
names(dataN)
TestClas<-as.data.frame(TestClas)
predi=predict(bag.car,TestClas,type="prob")[,2]
index= seq(1,length(predi[,2]), by=1)
predi<-as.data.frame(predi)
library(writexl)
write_xlsx(predi,"C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/predi.xlsx")

estesi<-rep(0,length(predi[,2]))
thres=0.5
estesi=ifelse(predi[,2]>thres,1,0)
confusionMatrix(table(estesi,testN[,19]))

roc_bag=roc(testN[,19],predi[,2])
plot(roc_bag, main ="ROC bagging")
auc_bag=auc(roc_bag)
auc_bag


#----------------------------------------------boosting----------------------------------------------------------------#
head(trainN)

boost.car=gbm(default.payment.next.month~.,data=trainN,distribution="bernoulli",n.trees=1000,interaction.depth=3,shrinkage=0.01, cv.folds = 10)
summary(boost.car)

#gbm.perf(boost.car,oobag.curve=T,overlay=T)
names()
pred_boo=predict(boost.car,testN,type="response")

estesi<-rep(0,length(pred_boo))
thres=0.4
estesi=ifelse(pred_boo>thres,2,1)
confusionMatrix(table(estesi,testN$y))

roc_boo=roc(testN$y,pred_boo,n.trees=10000)
plot(roc_boo)
auc_boo=auc(roc_boo)
auc_boo


#------------------------------------------XGboost---------------------------------------------------------------------#
library("xgboost")

xgboost <- xgboost(data = as.matrix(trainN[,1:18]),label=trainN[,19], max.depth =5, eta=0.1,subsample=0.8,nrounds = 2, objective = "binary:logistic")

xgb.pred = predict(xgboost,as.matrix(data.test[,-5]))
xg_predi=rep(0,305)
xg_predi[xgb.pred>thres]=1

library(caret)
confusionMatrix(data=as.factor(xg_predi),reference=Falso.test)

roc_xgb=roc(Falso.test,xgb.pred)
plot(roc_xgb)
auc_boo=auc(roc_xgb)
auc_boo

