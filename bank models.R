library(tidyverse)
library(dslabs)
library(dplyr)
library(naniar)
library(caret)
library(pROC)
library(rpart.plot)
library(rpart)
library(lubridate)
library(ggplot2)
miss_var_summary(bank)
?miss_var_summary
#vars-> factor replace unknown with NA 
miss_scan_count(data=bank,search = "unknown")
bank1<-replace_with_na_all(data=bank,condition=~.x %in% c("unknown"))

miss_var_summary(bank1)
bank1$job<-as.factor(bank1$job)
levels(bank1$job)
bank1$marital<-as.factor(bank1$marital)
levels(bank1$marital)
bank1$education<-as.factor(bank1$education)
levels(bank1$education)
bank1$housing<-as.factor(bank1$housing)
levels(bank1$housing)
table(bank1$housing)
bank1$housing<-plyr::revalue(bank1$housing,c("1"="yes","2"="no"))
bank1$loan<-as.factor(bank1$loan)
bank1$loan<-plyr::revalue(bank1$loan,c("1"="yes","2"="no"))
table(bank1$loan)
bank1$contact<-as.factor(bank1$contact)
levels(bank1$contact)
table(bank1$contact)
bank1$poutcome<-as.factor(bank1$poutcome)
levels(bank1$poutcome)
table(bank1$poutcome)
hist(bank1$age)
bank1$y<-as.factor(bank1$y)
bank1$y<-plyr::revalue(bank1$y,c("1"="yes","2"="no"))
levels(bank1$y)
prop.table(table(bank1$y))

#month-> factor change order of levels
bank1$month<-as.factor(bank1$month)  
bank1$month<-factor(bank1$month,levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")) 

#days_of_week-> factor change order of levels
bank1$day_of_week<-as.factor(bank1$day_of_week)
bank1$day_of_week<-factor(bank1$day_of_week,levels = c("mon","tue","wed","thu","fri"))
str(bank1)

#delete lines pdays=999 and previous!=0 doesnt make sense
ind<-which(bank1$pdays==999 & bank1$previous!=0)
bank2<-bank1[-ind,]

#month-> factor change order of levels
bank2$month<-as.factor(bank2$month)   
bank2$month<-factor(bank2$month,levels = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")) 

#days_of_week-> factor change order of levels
bank2$day_of_week<-as.factor(bank2$day_of_week)
bank2$day_of_week<-factor(bank2$day_of_week,levels = c("mon","tue","wed","thu","fri"))
prop.table(table(bank2$y))


vis_miss(bank2,cluster = T)
+labs(title = "Bank2")

summary(bank2)

#summary tables

#count of yes,no by day
gia<-by(bank2$y,bank2$day_of_week,count)
#percent of yes,no by day
bank2%>%group_by(day_of_week)%>% summarise(percentno=mean(y=="no"),percentyes=mean(y=="yes"))

#absolute count of yes and no by day
count1<-bank2%>%group_by(day_of_week)%>% summarise(countno=sum(y=="no"),countyes=sum(y=="yes"),n=n())

# differense of proportio by day (pvalue<0.05) 
pp<-prop.test(count1$countyes,count1$n) 

#barplot of percentage of yes and no by days
barplot(t(prop.table(table(bank2$day_of_week,bank2$y),1)),beside = T,ylim = c(0,1))

#same with ggplot
gia<-data.frame(t(prop.table(table(bank2$day_of_week,bank2$y),1)))
ggplot(gia,aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat = "identity",position = "dodge")

#percentsyes of no ana job
bank2%>%group_by(job)%>% summarise(percent_no=mean(y=="no"),percent_yes=mean(y=="yes"))



#create train kai test sets same proportion of yes and no with the initial(success)
indtest<-createDataPartition(bank2$y,p=0.25,list = F)
train<-bank2%>%slice(-indtest)
test<-bank2%>%slice(indtest)
prop.table(table(train$y))
prop.table(table(test$y))
prop.table(table(bank2$y))

#completely random model
y_hat <- as.factor(sample(c("yes", "no"), dim(na.omit(test))[1], replace = TRUE)) 
class(y_hat)
levels(y_hat)
confusionMatrix(y_hat,na.omit(test)$y)

#knn model differen k withaout the default
fitknn1<-train(y~age+job+marital+education+housing+loan+contact+month+day_of_week+campaign:nr.employed,method="knn",data = train, na.action = na.omit,tuneGrid=data.frame(k=seq(100,200,5))) 
ggplot(fitknn1,highlight = T)               
fitknn1$bestTune
               
#knn model with optimal k without default day_of_week+
fitknn<-knn3(y~age+job+marital+education+housing+loan+contact+month+campaign:nr.employed,data = train,na.action = na.omit ,  k=135)
prknnpr<-predict(fitknn,na.omit(test),type = "prob")
prknn<-predict(fitknn,na.omit(test),type="class")
confusionMatrix(prknn,na.omit(test)$y)
length(prknn)
# ROC
ROC<-roc(na.omit(test)$y,prknnpr)
?roc
length(na.omit(test)$y)
length(prknn)

#logistic regression model +day_of_week  removed
train1<-mutate(train,y=as.numeric(y=="yes"))
miss_var_summary(train1)
test1<-mutate(test,y=as.numeric(y=="yes"))
fitlg<-glm(y~age+job+marital+education+loan+month+housing+contact+campaign:nr.employed, na.action = na.omit,data = train1,family = "binomial")
p_hat_log<-predict(fitlg,test1,type = "response") #propability of "yes"
y_hat_log <- ifelse(p_hat_log > 0.2, "yes", "no") %>% factor
a<-confusionMatrix(y_hat_log,test$y)
length(na.omit(y_hat_log))
bind_cols(p_hat_log,test$y,y_hat_log==test$y)

#Different cutpoints of p accuracy and sensitivity
y_hat_log<-c()
a<-c()
b<-c()

f<-function(p){
  y_hat_log <- ifelse(p_hat_log > p, "yes", "no") %>% factor
  a<-confusionMatrix(y_hat_log,test$y)$overall["Accuracy"]
  b<-confusionMatrix(y_hat_log,test$y)$byClass["Sensitivity"]
  return(c(p,a,b))
}

m<-data.frame(t(sapply(data.frame(t(seq(0.2,0.9,0.05))),f)))
names(m)<-c("p","Acc","Sens")
head(m)
class(m)
m<-gather(m,ind,value,-p)
m%>%ggplot(aes(x=p,y=value,col=ind))+geom_line()







#ftiaxnw montelo rforest
