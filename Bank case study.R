#Libraries and Code used for the Project:
rm(list=ls())
library(readxl)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
library(randomForest)
library(gains)
library(ineq)
library(InformationValue)
library(lift)
library(factoextra)
library(cluster)
library(NbClust)

ba=read_excel("bank.xlsx",sheet="Bank")
summary(ba)
ba=ba[,-c(1,5)]
ba=na.omit(ba)
summary(ba)
str(ba)
#Renaming variables/manipulation
names(ba)[1]="Age"
names(ba)[2]="Experience"
names(ba)[3]="Income"
names(ba)[4]="Family"
names(ba)[8]="PL"
names(ba)[5]="CCAvg"
names(ba)[9]="SA"
names(ba)[10]="Bonds"
names(ba)[11]="Online"
names(ba)[12]="CreditCard"
ba$Mortgage=ifelse(ba$Mortgage==0,0,1)
ba$Mortgage=as.factor(ba$Mortgage)
ba$Family=as.factor(ba$Family)
ba$Education=as.factor(ba$Education)
ba$PL=as.factor(ba$PL)
ba$SA=as.factor(ba$SA)
ba$Online=as.factor(ba$Online)
ba$CreditCard=as.factor(ba$CreditCard)
ba$Bonds=as.factor(ba$Bonds)
str(ba)

#Exploratory Data Analysis Codes:
bk=ggplot(ba,aes(x=Age,y=Income,color=PL))
bk+geom_point(aes(color=PL))+facet_grid(Online~Family)+ggtitle("Age vs Income against online and family")
table(ba$Online,ba$PL)
table(ba$PL,ba$Family)
table(ba$PL,ba$Family,ba$Online)
bk=ggplot(ba,aes(x=Age,y=Income,color=PL))
bk+geom_point(aes(color=PL))+facet_grid(Online~Education)+ggtitle("Age vs Income against online and education")
table(ba$PL,ba$Education,ba$Online)
bk=ggplot(ba,aes(x=CCAvg,y=Income,color=PL))
bk+geom_point(aes(color=PL))+facet_grid(Education~CreditCard)+ggtitle("CCavg vs Income against Education and Credit card")
m<-ggplot(ba,aes(x=Income))
m+geom_histogram(aes(fill=PL,position="dodge"),alpha=0.5)+facet_grid(SA~Bonds+Mortgage)+ggtitle("income against Savings account, Mortgage and Bonds")



#Clustering Code:
baa=read_excel("bank.xlsx",sheet="Bank")
summary(baa)
baa=baa[,-c(1,5)]
baa=na.omit(baa)
summary(baa)
str(baa)
nc=NbClust(baa[,c(1:12)],min.nc=4,max.nc=8,method="kmeans")
table(nc$Best.nc[1,])
ba1=baa
fviz_nbclust(ba1,kmeans,method="wss")+labs(subtitle = "Elbow method")
kme.cl=kmeans(ba1,4)
ba1$cluster=kme.cl$cluster
ag=aggregate(ba1[,-13],list(ba1$cluster),FUN=mean)
Cust.profile=data.frame(Cluster=ag[,1],Freq=as.vector(table(ba1$cluster)),ag[,-1])
Cust.profile[,c(1,2,10)]
tu=baa[ba1$cluster==3,]
ti=baa[ba1$cluster==4,]
tar=rbind(tu,ti)

#Decision Tree Code:
library(caTools)
set.seed(100)
dim(ba)
str(ba)
class(ba)
spl=sample.split(ba$PL,SplitRatio  = 0.7)
#subset to train and test
tr=subset(ba,spl==T)
te=subset(ba,spl==F)
dim(tr)
dim(te)
#calculate response rate
table(ba$PL)

table(tr$PL)
sum(tr$PL=="1")/nrow(tr)



#define paramneters
par(mfrow=c(1,1))
r.ctrl=rpart.control(minisplit=100,minbucket=10,cp=0,xval=10)
mm=rpart(formula= PL~.,data=tr,method="class",control = r.ctrl)
mm
fancyRpartPlot(mm)
printcp(mm)
plotcp(mm)
mm_prune=prune(mm,cp=0.0073)
mm_prune
fancyRpartPlot(mm_prune)
#used the pruned tree to do prediction on train and test data
tr$pred=predict(mm_prune,data=tr,type="class")
tr$prob=predict(mm_prune,data=tr,type="prob")[,"1"]
te$pr=predict(mm_prune,te,type="class")
te$prob=predict(mm_prune,te,type="prob")[,"1"]

#check performance on bothe test and train
library(ROCR)
pred_dt=prediction(tr$prob,tr$PL)
pref_dt=performance(pred_dt,"tpr","fpr")
plot(pref_dt,main="ROC CURVE")

pred_dt1=prediction(te$prob,te$PL)
pref_dt1=performance(pred_dt1,"tpr","fpr")
plot(pref_dt1,main="roc")
#check auc
auc_train_dt=performance(pred_dt,"auc")
auc_train_dt=as.numeric(auc_train_dt@y.values)
auc_train_dt
auc_test_dt1=performance(pred_dt1,"auc")
auc_test_dt1=as.numeric(auc_test_dt1@y.values)
auc_test_dt1

#Random Forest Code:
trf=subset(ba,spl==T)
ted=subset(ba,spl==F)
dim(trf)
dim(ted)
#build first RF model
rf=randomForest(PL~.,data=trf,ntree=501,mtry=4,nodesize=90,importance=T)
print(rf)
plot(rf)
#Tuning Random forest to reduce OOB error rate:
rf1=tuneRF(x=trf[,c(-8)],y=trf$PL,mtryStart = 4,stepFactor = 1.5,ntreeTry=51,improve=0.0001,nodesize=10,trace=T,plot=T,doBest = T,importance=T)
plot(rf1)
rf1=randomForest(PL~.,data=trf,ntree=51,mtry=6,nodesize=40,importance=T)
plot(rf1)
print(rf1)

#Using the Pruned tree to build a model on train and test data:
trf$pred=predict(rf1,data=trf,type="class")
trf$prob=predict(rf1,data=trf,type="prob")[,"1"]
ted$pred=predict(rf1,ted,type="class")
ted$prob=predict(rf1,ted,type="prob")[,"1"]
pred_dt0=prediction(trf$prob,trf$PL)
pref_dt0=performance(pred_dt0,"tpr","fpr")
plot(pref_dt0,main="ROC CURVE")
pred_dt2=prediction(ted$prob,ted$PL)
pref_dt2=performance(pred_dt2,"tpr","fpr")
plot(pref_dt2,main="roc")
auc_train_rf=performance(pred_dt0,"auc")
auc_train_rf=as.numeric(auc_train_rf@y.values)
auc_train_rf
auc_test_rf=performance(pred_dt2,"auc")
auc_test_rf=as.numeric(auc_test_rf@y.values)
auc_test_rf

#Model Performance Measures Codes:
#CART:
tr$pred=predict(mm_prune,data=tr,type="class")
tr$prob=predict(mm_prune,data=tr,type="prob")[,"1"]
te$pr=predict(mm_prune,te,type="class")
te$prob=predict(mm_prune,te,type="prob")[,"1"]

#Build confusion matrix
tr_cm=table(tr$PL,tr$pred)
tr_cm
te_cm=table(te$PL,te$pr)
te_cm

###Miss classification rate
(tr_cm[1,2]+tr_cm[2,1])/nrow(tr)
(te_cm[1,2]+te_cm[2,1])/nrow(te)

######Accuracy
ac=1-(tr_cm[1,2]+tr_cm[2,1])/nrow(tr)
ac
acte=1-(te_cm[1,2]+te_cm[2,1])/nrow(te)
acte

#area under the auc curve
library(ROCR)
pred_dt=prediction(tr$prob,tr$PL)
pref_dt=performance(pred_dt,"tpr","fpr")
plot(pref_dt,main="ROC CURVE")

pred_dt1=prediction(te$prob,te$PL)
pref_dt1=performance(pred_dt1,"tpr","fpr")
plot(pref_dt1,main="roc")
#check auc
auc_train_dt=performance(pred_dt,"auc")
auc_train_dt=as.numeric(auc_train_dt@y.values)
auc_train_dt
auc_test_dt1=performance(pred_dt1,"auc")
auc_test_dt1=as.numeric(auc_test_dt1@y.values)
auc_test_dt1


#Gains charts
library(gains)
gain_tr=performance(pred_dt,"tpr","rpp")
plot(gain_tr,col="orange",lwd=2)
lines(x=c(0,0.5,1),y=c(0,1,1),col="darkgreen",lwd=2)

gain_te=performance(pred_dt1,"tpr","rpp")
plot(gain_te,col="orange",lwd=2)
lines(x=c(0,0.5,1),y=c(0,1,1),col="darkgreen",lwd=2)

#kolgormonov-smirnov matrix
ks_tr=max(pref_dt@y.values[[1]]-pref_dt@x.values[[1]])
plot(pref_dt,main=paste0("KS=",round(ks_tr*100,5),"%"))
lines(x=c(0,1),y=c(0,1))

ks_te=max(pref_dt1@y.values[[1]]-pref_dt1@x.values[[1]])
plot(pref_dt1,main=paste0("KS=",round(ks_te*100,5),"%"))
lines(x=c(0,1),y=c(0,1))

#gini
library(ineq)
ineq(tr$prob,"gini")
ineq(te$prob,"gini")

#concordance
library(InformationValue)
Concordance(actuals=tr$PL,predictedScores = tr$prob)
Concordance(actuals=te$PL,predictedScores = te$prob)

#lift chart
library(lift)
plotLift(tr$prob,tr$PL,cumulative = T)
plotLift(te$prob,te$PL,cumulative = T)

#Random Forest:
rf_tr=table(trf$PL,trf$pred)
rf_tr
rf_te=table(ted$PL,ted$pred)
rf_te

#Missclassification rate
(rf_tr[1,2]+rf_tr[2,1])/nrow(trf)
(rf_te[1,2]+rf_te[2,1])/nrow(ted)

#Accuracy
actf=1-(rf_tr[1,2]+rf_tr[2,1])/nrow(trf)
actf
acte=1-(rf_te[1,2]+rf_te[2,1])/nrow(ted)
acte
#plot ROC and check AUC on train and test:
pred_dt0=prediction(trf$prob,trf$PL)
pref_dt0=performance(pred_dt0,"tpr","fpr")
plot(pref_dt0,main="ROC CURVE")

pred_dt2=prediction(ted$prob,ted$PL)
pref_dt2=performance(pred_dt2,"tpr","fpr")
plot(pref_dt2,main="Roc")

auc_train_rf=performance(pred_dt0,"auc")
auc_train_rf=as.numeric(auc_train_rf@y.values)
auc_train_rf
auc_test_rf=performance(pred_dt2,"auc")
auc_test_rf=as.numeric(auc_test_rf@y.values)
auc_test_rf

#gain chart
gain_trf=performance(pred_dt0,"tpr","rpp")
plot(gain_tr,col="orange",lwd=2)
lines(x=c(0,0.5,1),y=c(0,1,1),col="darkgreen",lwd=2)

gain_terf=performance(pred_dt2,"tpr","rpp")
plot(gain_terf,col="orange",lwd=2)
lines(x=c(0,0.5,1),y=c(0,1,1),col="darkgreen",lwd=2)

#Kolgomorov Plot for train and test:
ks_tr=max(pref_dt0@y.values[[1]]-pref_dt0@x.values[[1]])
plot(pref_dt0,main=paste0("KS=",round(ks_tr*100,5),"%"))
lines(x=c(0,1),y=c(0,1))

ks_te=max(pref_dt2@y.values[[1]]-pref_dt2@x.values[[1]])
plot(pref_dt2,main=paste0("KS=",round(ks_te*100,5),"%"))
lines(x=c(0,1),y=c(0,1))

library(ineq)

ineq(trf$prob,"gini")
ineq(ted$prob,"gini")

library(InformationValue)
Concordance(actuals=trf$PL,predictedScores = trf$prob)
Concordance(actuals=ted$PL,predictedScores = ted$prob)     
library(lift)
plotLift(tr$prob,trf$PL,cumulative = T)
plotLift(te$prob,ted$PL,cumulative = T)
