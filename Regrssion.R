library(readxl)
library(e1071)
library(EnvStats)
library(corrplot)
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
TrainReg <- read_excel("C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/TrainReg.xlsx")
TestReg <- read_excel("C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/TestReg.xlsx")

data=TrainReg[,3:61]
dataTest=TestReg[,3:60]

#---------------------------------------------Exploracion y vizualizacion de datos-------------------------------------------#

##Have a look at data

#Numer of rows
N=nrow(data)
#Number of columns
C=ncol(data)
#dimensionality
dim(data)
#Column names
names(data)
#Structure of data
str(data)
#attributes of data
attributes(data)

##Explore individual varibles

#Summary
summary(data)
#Variance and histogram

#Number of words in the title
summary(data$n_tokens_title)
var(data$n_tokens_title) 
hist(data$n_tokens_title)
plot(data$n_tokens_title,data$shares)
skewness(data$n_tokens_title) #No.Box-Cox

#Number of words in the content
summary(data$n_tokens_content)
var(data$n_tokens_content) 
hist(data$n_tokens_content)
plot(data$n_tokens_content,data$shares)
skewness(data$n_tokens_content) # Hay que hacer Box-Cox###########

q=quantile(data$n_tokens_content,0.98)
data$n_tokens_content = ifelse(data$n_tokens_content>q,q,data$n_tokens_content)


#Rate of unique words in the content

summary(data$n_unique_tokens)
var(data$n_unique_tokens) 
hist(data$n_unique_tokens)
plot(data$n_unique_tokens,data$shares)
skewness(data$n_unique_tokens)#No Box-cox


#Rate of non-stop words in the content

summary(data$n_non_stop_words)
var(data$n_non_stop_words) 
hist(data$n_non_stop_words) #este predictor es raro. Remover predictor puesto que los datos se concentran en 1
plot(data$n_non_stop_words,data$shares)
skewness(data$n_non_stop_words)

#Rate of unique non-stop words in the content

summary(data$n_non_stop_unique_tokens)
var(data$n_non_stop_unique_tokens) 
hist(data$n_non_stop_unique_tokens)
plot(data$n_non_stop_unique_tokens,data$shares)
skewness(data$n_non_stop_unique_tokens) #hay que hacer box-cox##########


#Number of links

summary(data$num_hrefs)
var(data$num_hrefs) 
hist(data$num_hrefs)
plot(data$num_hrefs,data$shares)
skewness(data$num_hrefs) #Box-cox

#Number of links to other articles published by Mashable

summary(data$num_self_hrefs)
var(data$num_self_hrefs) 
hist(data$num_self_hrefs)
plot(data$num_self_hrefs,data$shares)
skewness(data$num_self_hrefs) #Box cox

q=quantile(data$num_self_hrefs,0.98)
data$num_self_hrefs=ifelse(data$num_self_hrefs>q,q,data$num_self_hrefs)

#Number of images

summary(data$num_imgs)
var(data$num_imgs) 
hist(data$num_imgs)
plot(data$num_imgs,data$shares)
skewness(data$num_imgs) #box-cox

q=quantile(data$num_imgs,0.98)
data$num_imgs=ifelse(data$num_imgs>q,q,data$num_imgs)

#Number of videos

summary(data$num_videos)
var(data$num_videos) 
hist(data$num_videos)
plot(data$num_videos,data$shares)
skewness(data$num_videos) #box cox

q=quantile(data$num_videos,0.98)
data$num_videos=ifelse(data$num_videos>q,q,data$num_videos)


#Average length of the words in the content

summary(data$average_token_length)
var(data$average_token_length) 
hist(data$average_token_length)
plot(data$average_token_length,data$shares)
skewness(data$average_token_length) #box cox


#Number of keywords in the metadata

summary(data$num_keywords)
var(data$num_keywords) 
hist(data$num_keywords)
plot(data$num_keywords,data$shares)
skewness(data$num_keywords) #No box cox

#data chanel variables 13 to 18 

channel=c(rep("other",nrow(data)))
for(i in 1:nrow(data))
{
  if(data$data_channel_is_lifestyle[i]==1)
  {
    channel[i]="lifesty"
  }
  if(data$data_channel_is_entertainment[i]==1)
  {
    channel[i]="entert"
  }
  if(data$data_channel_is_bus[i]==1)
  {
    channel[i]="bus"
  }
  if(data$data_channel_is_socmed[i]==1)
  {
    channel[i]="socmed"
  }
  if(data$data_channel_is_tech[i]==1)
  {
    channel[i]="tech"
  }
  if(data$data_channel_is_world[i]==1)
  {
    channel[i]="world"
  }
}
data=cbind(data,channel)
data=as.data.frame(data)

channel=c(rep("other",nrow(dataTest)))
for(i in 1:nrow(dataTest))
{
  if(dataTest$data_channel_is_lifestyle[i]==1)
  {
    channel[i]="lifesty"
  }
  if(dataTest$data_channel_is_entertainment[i]==1)
  {
    channel[i]="entert"
  }
  if(dataTest$data_channel_is_bus[i]==1)
  {
    channel[i]="bus"
  }
  if(dataTest$data_channel_is_socmed[i]==1)
  {
    channel[i]="socmed"
  }
  if(dataTest$data_channel_is_tech[i]==1)
  {
    channel[i]="tech"
  }
  if(dataTest$data_channel_is_world[i]==1)
  {
    channel[i]="world"
  }
}
dataTest=cbind(dataTest,channel)
dataTest=as.data.frame(dataTest)
pie(table(channel))
barplot(table(channel))
boxplot(data$shares~data$channel)

#Worst keyword (min. shares) 

summary(data$kw_min_min)
var(data$kw_min_min) 
hist(data$kw_min_min)
plot(data$kw_min_min,data$shares)##este predictor esta raro se quita
skewness(data$kw_min_min) #Box-cox

#Worst keyword (max. shares)

summary(data$kw_max_min)
var(data$kw_max_min) 
hist(data$kw_max_min)##este predictor esta raro, se quita
plot(data$kw_max_min,data$shares)
skewness(data$kw_max_min) #Box-cox
q=quantile(data$kw_max_min,0.98)
data$kw_max_min=ifelse(data$kw_max_min>q,q,data$kw_max_min)

#Worst keyword (avg. shares)

summary(data$kw_avg_min)
var(data$kw_avg_min) 
hist(data$kw_avg_min)##este predictor esta raro
plot(data$kw_avg_min,data$shares)
skewness(data$kw_avg_min) #box-cox

q=quantile(data$kw_avg_min,0.98)
data$kw_avg_min=ifelse(data$kw_avg_min>q,q,data$kw_avg_min)

#Best keyword (min. shares)

summary(data$kw_min_max)
var(data$kw_min_max) 
hist(data$kw_min_max)##este predictor esta raro se quita
plot(data$kw_min_max,data$shares)
skewness(data$kw_min_max) #box-cox

q=quantile(data$kw_min_max,0.98)
data$kw_min_max=ifelse(data$kw_min_max>q,q,data$kw_min_max)
#which(data$kw_min_max>840000)

#Best keyword (max. shares) 

summary(data$kw_max_max)
var(data$kw_max_max) 
hist(data$kw_max_max)##este predictor esta raro
plot(data$kw_max_max,data$shares)
skewness(data$kw_max_max) #Box cox

#Best keyword (avg. shares)

summary(data$kw_avg_max)
var(data$kw_avg_max) 
hist(data$kw_avg_max)
plot(data$kw_avg_max,data$shares)
skewness(data$kw_avg_max) # No Box-Cox

#Avg. keyword (min. shares)

summary(data$kw_min_avg)
var(data$kw_min_avg) 
hist(data$kw_min_avg)
plot(data$kw_min_avg,data$shares)
skewness(data$kw_min_avg) #No-Box-Cox

#Avg. keyword (max. shares)

summary(data$kw_max_avg)
var(data$kw_max_avg) 
hist(data$kw_max_avg)##este predictor esta raro
plot(data$kw_max_avg,data$shares)
skewness(data$kw_max_avg) #Box-cox

q=quantile(data$kw_max_avg,0.98)
data$kw_max_avg=ifelse(data$kw_max_avg>q,q,data$kw_max_avg)

#Avg. keyword (avg. shares)

summary(data$kw_avg_avg)
var(data$kw_avg_avg) 
hist(data$kw_avg_avg)
plot(data$kw_avg_avg,data$shares)
skewness(data$kw_avg_avg) #Box-cox

#Min. shares of referenced articles in Mashable

summary(data$self_reference_min_shares)
var(data$self_reference_min_shares) 
hist(data$self_reference_min_shares)##este predictor esta raro
plot(data$self_reference_min_shares,data$shares)
skewness(data$self_reference_min_shares) #Box cox

q=quantile(data$self_reference_min_shares,0.98)
data$self_reference_min_shares=ifelse(data$self_reference_min_shares>q,q,data$self_reference_min_shares)

#Max. shares of referenced articles in Mashable

summary(data$self_reference_max_shares)
var(data$self_reference_max_shares) 
hist(data$self_reference_max_shares)##este predictor esta raro
plot(data$self_reference_max_shares,data$shares)
skewness(data$self_reference_max_shares) #box cox

q=quantile(data$self_reference_max_shares,0.98)
data$self_reference_max_shares=ifelse(data$self_reference_max_shares>q,q,data$self_reference_max_shares)

#Avg. shares of referenced articles in Mashable

summary(data$self_reference_avg_sharess)
var(data$self_reference_avg_sharess) 
hist(data$self_reference_avg_sharess)##este predictor esta raro
plot(data$self_reference_avg_sharess,data$shares)
skewness(data$self_reference_avg_sharess) #Box-cox

q=quantile(data$self_reference_avg_sharess,0.98)
data$self_reference_avg_sharess=ifelse(data$self_reference_avg_sharess>q,q,data$self_reference_avg_sharess)

#Day Published

day=c(rep(0,nrow(data)))
for(i in 1:nrow(data))
{
  if(data$weekday_is_monday[i]==1)
  {
    day[i]="mon"
  }
  if(data$weekday_is_tuesday[i]==1)
  {
    day[i]="tue"
  }
  if(data$weekday_is_saturday[i]==1)
  {
    day[i]="sat"
  }
  if(data$weekday_is_sunday[i]==1)
  {
    day[i]="sun"
  }
  if(data$weekday_is_thursday[i]==1)
  {
    day[i]="thurs"
  }
  if(data$weekday_is_wednesday[i]==1)
  {
    day[i]="wed"
  }
  if(data$weekday_is_friday[i]==1)
  {
    day[i]="fri"
  }
}
data=cbind(data,day)
data=as.data.frame(data)

day=c(rep(0,nrow(dataTest)))
for(i in 1:nrow(dataTest))
{
  if(dataTest$weekday_is_monday[i]==1)
  {
    day[i]="mon"
  }
  if(dataTest$weekday_is_tuesday[i]==1)
  {
    day[i]="tue"
  }
  if(dataTest$weekday_is_saturday[i]==1)
  {
    day[i]="sat"
  }
  if(dataTest$weekday_is_sunday[i]==1)
  {
    day[i]="sun"
  }
  if(dataTest$weekday_is_thursday[i]==1)
  {
    day[i]="thurs"
  }
  if(dataTest$weekday_is_wednesday[i]==1)
  {
    day[i]="wed"
  }
  if(dataTest$weekday_is_friday[i]==1)
  {
    day[i]="fri"
  }
}
dataTest=cbind(dataTest,day)
dataTest=as.data.frame(dataTest)

table(day)
pie(table(day))
barplot(table(day))
boxplot(data$shares~data$day)


#weekend?

table(data$is_weekend)
pie(table(data$is_weekend))
barplot(table(data$is_weekend))

#Closeness to LDA topic 0

summary(data$LDA_00)
var(data$LDA_00) 
hist(data$LDA_00)
plot(data$LDA_00,data$shares)
skewness(data$LDA_00) #box-cox

#Closeness to LDA topic 1

summary(data$LDA_01)
var(data$LDA_01) 
hist(data$LDA_01)
plot(data$LDA_01,data$shares)
skewness(data$LDA_01) #box-cox

# Closeness to LDA topic 2

summary(data$LDA_02)
var(data$LDA_02) 
hist(data$LDA_02)
plot(data$LDA_02,data$shares)
skewness(data$LDA_02) #box cox

#Closeness to LDA topic 3

summary(data$LDA_03)
var(data$LDA_03) 
hist(data$LDA_03)
plot(data$LDA_03,data$shares)
skewness(data$LDA_03) #Box-cox

#Closeness to LDA topic 4

summary(data$LDA_04)
var(data$LDA_04) 
hist(data$LDA_04)
plot(data$LDA_04,data$shares)
skewness(data$LDA_04) #box cox

#44. global_subjectivity: Text subjectivity

summary(data$global_subjectivity)
var(data$global_subjectivity) 
hist(data$global_subjectivity)
plot(data$global_subjectivity,data$shares)
skewness(data$global_subjectivity) #no box cox

#45. global_sentiment_polarity: Text sentiment polarity

summary(data$global_sentiment_polarity)
var(data$global_sentiment_polarity) 
hist(data$global_sentiment_polarity)
plot(data$global_sentiment_polarity,data$shares)
skewness(data$global_sentiment_polarity) #no box cox

#46. global_rate_positive_words: Rate of positive words in the content

summary(data$global_rate_positive_words)
var(data$global_rate_positive_words) 
hist(data$global_rate_positive_words)
plot(data$global_rate_positive_words,data$shares)
skewness(data$global_rate_positive_words) #no box-cox

#47. global_rate_negative_words: Rate of negative words in the content

summary(data$global_rate_negative_words)
var(data$global_rate_negative_words) 
hist(data$global_rate_negative_words)
plot(data$global_rate_negative_words,data$shares)
skewness(data$global_rate_negative_words) #box cox

q=quantile(data$global_rate_negative_words,0.99)
data$global_rate_negative_words=ifelse(data$global_rate_negative_words>q,q,data$global_rate_negative_words)

#48. rate_positive_words: Rate of positive words among non-neutral tokens

summary(data$rate_positive_words)
var(data$rate_positive_words) 
hist(data$rate_positive_words)
plot(data$rate_positive_words,data$shares)
skewness(data$rate_positive_words)#boxcox

#49. rate_negative_words: Rate of negative words among non-neutral tokens

summary(data$rate_negative_words)
var(data$rate_negative_words) 
hist(data$rate_negative_words)
plot(data$rate_negative_words,data$shares)
skewness(data$rate_negative_words)# no box cox

#50. avg_positive_polarity: Avg. polarity of positive words

summary(data$avg_positive_polarity)
var(data$avg_positive_polarity) 
hist(data$avg_positive_polarity)
plot(data$avg_positive_polarity,data$shares)
skewness(data$avg_positive_polarity) #No box cox

#51. min_positive_polarity: Min. polarity of positive words

summary(data$min_positive_polarity)
var(data$min_positive_polarity) 
hist(data$min_positive_polarity)
plot(data$min_positive_polarity,data$shares)
skewness(data$min_positive_polarity)#Box-Cox

#52. max_positive_polarity: Max. polarity of positive words

summary(data$max_positive_polarity)
var(data$max_positive_polarity) 
hist(data$max_positive_polarity)
plot(data$max_positive_polarity,data$shares)
skewness(data$max_positive_polarity)#no box cox

#53. avg_negative_polarity: Avg. polarity of negative words

summary(data$avg_positive_polarity)
var(data$avg_positive_polarity) 
hist(data$avg_positive_polarity)
plot(data$avg_positive_polarity,data$shares)
skewness(data$avg_positive_polarity)#no box-cox

#54. min_negative_polarity: Min. polarity of negative words

summary(data$min_positive_polarity)
var(data$min_positive_polarity) 
hist(data$min_positive_polarity)
plot(data$min_positive_polarity,data$shares)
skewness(data$min_positive_polarity) #box cox

#55. max_negative_polarity: Max. polarity of negative words

summary(data$max_negative_polarity)
var(data$max_negative_polarity) 
hist(data$max_negative_polarity)
plot(data$max_negative_polarity,data$shares)
skewness(data$max_negative_polarity) #box cox

#56. title_subjectivity: Title subjectivity

summary(data$title_subjectivity)
var(data$title_subjectivity) 
hist(data$title_subjectivity)
plot(data$title_subjectivity,data$shares)
skewness(data$title_subjectivity) #No box cox

#57. title_sentiment_polarity: Title polarity

summary(data$title_sentiment_polarity)
var(data$title_sentiment_polarity) 
hist(data$title_sentiment_polarity)
plot(data$title_sentiment_polarity,data$shares)
skewness(data$title_sentiment_polarity)#No box cox

#58. abs_title_subjectivity: Absolute subjectivity level

summary(data$abs_title_subjectivity)
var(data$abs_title_subjectivity) 
hist(data$abs_title_subjectivity)
plot(data$abs_title_subjectivity,data$shares)
skewness(data$abs_title_subjectivity) #no box cox

#59. abs_title_sentiment_polarity: Absolute polarity level

summary(data$abs_title_sentiment_polarity)
var(data$abs_title_sentiment_polarity) 
hist(data$abs_title_sentiment_polarity)
plot(data$abs_title_sentiment_polarity,data$shares)
skewness(data$abs_title_sentiment_polarity)#box cox

#60. Shares
summary(data$shares)
var(data$shares)
hist(data$shares)
plot(data$shares)


q=quantile(data$shares,0.98)
data$shares=ifelse(data$shares>q,q,data$shares)

#---------------------------------------------------Box-Cox transforation-----------------------------------------------------#

#data$LDA_00   -1.453983

data$LDA_00=boxcoxTransform(data$LDA_00, lambda = -1.12)
hist(data$LDA_00)
skewness(data$LDA_00)

#data$LDA_01

data$LDA_01=boxcoxTransform(data$LDA_01, lambda = -1.4)
hist(data$LDA_01)
skewness(data$LDA_01)

#data$LDA_02

data$LDA_02=boxcoxTransform(data$LDA_02, lambda = -1)
hist(data$LDA_02)
skewness(data$LDA_02)

#data$LDA_03

data$LDA_03=boxcoxTransform(data$LDA_03, lambda = -1.12)
hist(data$LDA_03)
skewness(data$LDA_03)

#data$LDA_04

data$LDA_04=boxcoxTransform(data$LDA_04, lambda = -0.7)
hist(data$LDA_04)
skewness(data$LDA_04)


#------------------------------------------------Correlacion, Outliers y NA------------------------------------------------------------#
#Best keyword (max. shares) y Worst keyword (min. shares) y Rate of non-stop words in the content
head(data)
data=data[,-c(30,31,32,33,34,35,36,37,12,13,14,15,16,17,19,18,4)]
dataTest=dataTest[,-c(30,31,32,33,34,35,36,37,12,13,14,15,16,17,19,18,4)]

sapply(data,class)
sapply(dataTest, class)
#-----------------------------------------------seleccion de variables------------------------------------------------------------------#



bor=Boruta(shares~.,data=data,pValue=0.05,maxRuns=500)
plot(bor)
att<-getSelectedAttributes(bor)
bor$timeTaken
bor$finalDecision


names(data)
data<-cbind(data[,-c(41,39,40,38,44,1)])
data=as.data.frame(data)  
#abs_title_sentiment_polarity  title_sentiment_polarity abs_title_subjectivity  title_subjectivity  day n_tokens_title
dataTest<-dataTest[,-c(43,1,38,40,39,41)]
data=as.data.frame(data)
dataTest=as.data.frame(dataTest)

names(dataTest)
names(data)
#----------------------------------------------Correlacion y outliers---------------------------------------------------------------------#

h
names(data)
View(data)
head(data)

xx<-data[,-c(38,37)]
co<-cor(xx)
?corrplot
co<-as.matrix(co)
corrplot(co)
View(co)
co=ifelse(co>0.9,0,co)

outlier.scores <- lofactor(xx, k=80)
plot(density(outlier.scores))
outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)
outliers<-as.vector(outliers)

data=data[-outliers,-3]
names(data)

data=data[,-29]
dataTest=dataTest[,-c(3,29)]

table(data$channel)
table(dataTest$channel)

levels(data$channel) <- levels(dataTest$channel)
#----------------------------------------Despliegue de modelos----------------------------------------------------------------------#
dt = sort(sample(nrow(data), nrow(data)*.8))
trainN<-data[dt,]
testN<-data[-dt,]

sapply(trainN,class)
sapply(testN,class)



#Random forest

bag.car=randomForest(shares~.,trainN,mtry=sqrt(dim(trainN)[2]-1))

imp=importance(bag.car)
imp
varImpPlot(bag.car)

head(testN)
predi=predict(bag.car,testN)

mseBag<-mean((predi-testN$shares)^2)
sqrt(mseBag)

library(writexl)
predict_gboost=as.data.frame(predict_gboost)
write_xlsx(predict_gboost,"C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/prediReg2882.xlsx")


#GBM
m2=gbm(shares~.,data=trainN,n.trees=1000,shrinkage=c(0.01,0.1,0.2),distribution="gaussian",cv.folds = 20)
summary(m2)
predict_gboost=predict(m2,newdata=testN)

mse_gboost=mean((predict_gboost-testN$shares)^2)
sqrt(mse_gboost)


library(gam)
library(mboost)
#Gamboost
names(trainN)
gamb <- gamboost(shares~n_tokens_content+n_unique_tokens+num_hrefs+num_self_hrefs+
                   num_imgs+num_videos+average_token_length+num_keywords+num_keywords+
                   kw_avg_min+kw_min_max+kw_max_max+kw_avg_max+
                   kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+
                   self_reference_max_shares+self_reference_avg_sharess+LDA_00+LDA_01+
                   LDA_02+LDA_03+LDA_04+global_subjectivity+
                   global_sentiment_polarity+global_rate_positive_words+global_rate_negative_words+rate_positive_words+
                   avg_positive_polarity+min_positive_polarity+max_positive_polarity+
                   avg_negative_polarity+min_negative_polarity+max_negative_polarity+bols(channel)
                   ,data = data, dfbase = 3, control = boost_control(mstop = 50))


?gamboost
predi_gamb<-predict(gamb,newdata = dataTest) # este modelooooooooooooooooooooooooooooooo



mseGAMboost<-mean((predi_gamb-testN$shares)^2)
sqrt(mseGAMboost)

predi_gamb<-as.data.frame(predi_gamb)
write_xlsx(predi_gamb,"C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/gamboost.xlsx")



#GAM
names(trainN)
gams.fit <- gam(shares~(n_tokens_content)+(n_unique_tokens)+s(num_hrefs)+(num_self_hrefs)+(num_keywords)+(kw_min_max)+
                  (kw_avg_max)+s(kw_avg_avg)+s(self_reference_min_shares)+s(self_reference_max_shares)+(self_reference_avg_sharess)+
                  (LDA_00)+(LDA_01)+s(LDA_02)+(LDA_03)+(LDA_04)+(global_sentiment_polarity)+(avg_negative_polarity)+(min_negative_polarity)+
                  channel,data=trainN)
summary(gams.fit)

prediGAM<-predict(gams.fit,testN)
mseGAM<-mean((testN$shares-prediGAM)^2)
sqrt(mseGAM)

#PCR
library(pls)

train2<-trainN[,-37]
test2<-testN[,-37]

lm2=pcr(shares~.,data=train2,scale=T,validation="CV",ncomp=35)
summary(lm2)
predpp=predict(lm2,test2)
msepp=mean((test2$shares-predpp)^2)
sqrt(msepp)


#PLS
library(pls)

lm3=plsr(shares~.,data=data[,-37],scale=T,validation="CV")
acc<-summary(lm3)

predpl=predict(lm3,dataTest[,-37])
predpl=as.data.frame(predpl)
length(predpl$`shares.2 comps`)

write_xlsx(predpl$`shares.2 comps`,"C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/elpoderoso.xlsx")


msepl=mean((test2$shares-predpl)^2)
sqrt(msepl)



predpl=as.data.frame(predpl$`shares.4 comps`)
library(writexl)
write_xlsx(predpl,"C:/Users/toros/OneDrive/Escritorio/Sebastian/Statistical Learning for Data Analytics/Competencia/prediReg22.xlsx")


#Support Vector Regression

names(trainN)
svm_reg=svm(shares~(n_tokens_content)+(n_unique_tokens)+(num_hrefs)+(num_self_hrefs)+(num_keywords)+(kw_min_max)+
              (kw_avg_max)+(kw_avg_avg)+(self_reference_min_shares)+(self_reference_max_shares)+(self_reference_avg_sharess)+
              (LDA_00)+(LDA_01)+(LDA_02)+(LDA_03)+(LDA_04)+(global_sentiment_polarity)+(avg_negative_polarity)+min_negative_polarity+
              (channel)+kw_avg_min,data=trainN)

rang=list(cost=c(0.01,0.05,0.1,1,2,5),gamma=c(0.1,0.5,1,2,4))

tune_svm_reg=tune(svm,shares~(n_tokens_content)+(n_unique_tokens)+(num_hrefs)+(num_self_hrefs)+(num_keywords)+(kw_min_max)+
                    (kw_avg_max)+(kw_avg_avg)+(self_reference_min_shares)+(self_reference_max_shares)+(self_reference_avg_sharess)+
                    (LDA_00)+(LDA_01)+(LDA_02)+(LDA_03)+(LDA_04)+(global_sentiment_polarity)+(avg_negative_polarity)+min_negative_polarity+
                    (channel)+kw_avg_min,data=trainN,ranges=rang)

tune_svm_reg


svm_fit_reg=svm(shares~(n_tokens_content)+(n_unique_tokens)+(num_hrefs)+(num_self_hrefs)+(num_keywords)+(kw_min_max)+
                  (kw_avg_max)+(kw_avg_avg)+(self_reference_min_shares)+(self_reference_max_shares)+(self_reference_avg_sharess)+
                  (LDA_00)+(LDA_01)+(LDA_02)+(LDA_03)+(LDA_04)+(global_sentiment_polarity)+(avg_negative_polarity)+min_negative_polarity+
                  (channel)+kw_avg_min,data=trainN, cost=2,gamma=0.1,probability=T)

predi_svr<-predict(svm_fit_reg, testN)
mseSvr<-mean((predi-testN$shares)^2)  
sqrt(mseSvr)    




#----------------------------------------------------------n-net-----------------------------------------------------------#


#dataprep
library(nnet)
library(neuralnet)
Xnn<-data[,-c(35,36)]
ynn<-data[,35]
testnn<-dataTest[,-35]



max_data <- apply(Xnn, 2, max)
min_data <- apply(Xnn, 2, min)
data_scaled <- scale(Xnn,center = min_data, scale = max_data - min_data)

datann<-cbind(data_scaled,ynn)
datann<-as.data.frame(datann)


max_data <- apply(testnn, 2, max)
min_data <- apply(testnn, 2, min)
data_scaled_test <- scale(testnn,center = min_data, scale = max_data - min_data)



dt = sort(sample(nrow(datann), nrow(datann)*.8))
trainNN<-datann[dt,]
testNN<-datann[-dt,]

shares

names(trainNN)
net_data = neuralnet(ynn~n_tokens_content+n_unique_tokens+num_hrefs+num_self_hrefs+
                       num_imgs+num_videos+average_token_length+num_keywords+num_keywords+
                       kw_avg_min+kw_min_max+kw_max_max+kw_avg_max+
                       kw_min_avg+kw_max_avg+kw_avg_avg+self_reference_min_shares+
                       self_reference_max_shares+self_reference_avg_sharess+LDA_00+LDA_01+
                       LDA_02+LDA_03+LDA_04+global_subjectivity+
                       global_sentiment_polarity+global_rate_positive_words+global_rate_negative_words+rate_positive_words+
                       avg_positive_polarity+min_positive_polarity+max_positive_polarity+
                       avg_negative_polarity+min_negative_polarity+max_negative_polarity,data=datann,hidden=10,linear.output=T)
plot(net_data)
predi_nn<-predict(net_data,data_scaled_test) #########este modelooooooooo



mse_nn<-mean((predi_nn-testNN$ynn)^2)
sqrt(mse_nn)

2/3*34
