
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
rm(list=ls())

log =read.csv(choose.files(), header=T, stringsAsFactors = TRUE)
head(log)
log1 = data.frame(log[,-c(1:6,11)])
attach(log1)
Paint.Touches = as.factor(Paint.Touches)
Change.Sides.of.Floor= as.factor(Change.Sides.of.Floor)


RNGkind(sample.kind = "default")
set.seed(2293347)
train.idx <- sample(x=1:nrow(log1), size=floor(.9*nrow(log1)))
train.df<-log1[train.idx,]
test.df<-log1[-train.idx,]

myforest<- randomForest(Result~., #remember the dangers of this short cut
                        data=train.df, ntree=1000,mtry=2, importance = TRUE)
#I set mtry = 2 because the square root of the # of x variables (5)
print(myforest)



##------------------------Tuning the Forest ---------------------------##
Btry<-c(15, 25, seq(from=50, to=1000, by=50))

for(ii in length(Btry)){
  tempforest<-randomForest(Successful.Possession~. , data=train.df, mtry=Btry)
  
}

#1 Choose what values of m actually make sense 
#for the tennis data set, since k=24, we can realy only tune m between 1 and 36

#THINK: what is possible

mtry<-c(1:5)

#need to make room for thepairs of m and OOB error
#create a dataframe

keeps<-data.frame(m=rep(NA, length(mtry)), OOB_error_rate=rep(NA,length(mtry)))

for(ii in 1:length(mtry)){
  print(paste0("This is tree: ", mtry[ii]))
  tempforest<-randomForest(Successful.Possession~. , data=train.df, ntree=1000, mtry=mtry[ii])
  
  keeps[ii, "m"]<- mtry[ii]
  keeps[ii, "OOB_error_rate"] <- mean(predict(tempforest)!=train.df$Successful.Possession)
  
}

rocCurve<-ggplot(keeps)+ geom_line(aes(x=m, y=OOB_error_rate))+ scale_x_continuous(breaks=mtry)+labs(x="m(mtry) value", y="OOB Error rate")
print(rocCurve)
#Use n=1 for final forest

finalforest<- randomForest(Successful.Possession~. , data=train.df, ntree= 1000, mtry=1, importance=TRUE)
print(finalforest)

#OOB accuracy
(482+201)/(482+201+244+93)
#0.6696078


#OOB error
#33.04%

pi_hat<- predict(finalforest, test.df, type="prob")[,"Yes"]

rocCurve<- roc(response=test.df$Successful.Possession, predictor= pi_hat, levels=c("No", "Yes"))
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)

#Specificity: .5
#Sensitivity: .821
#AUC: .694
#the second goal is interpretation: understanding how the x variable impact the y variable
# A random forest can help us understand which x variables are the most important. IT does this by giving a 
# variable importance plot. 
#NOTE: you must have importance = TRUE
# in the forest code so that R collects the necessary data

varImpPlot(finalforest, type=1)


m1 = glm(Successful.Possession~ Paint.Touches+Change.Sides.of.Floor+Player.Shot.Attempt
         +Shot.Type+Result,
         family= binomial(link="logit"))
print(m1)

m2 = glm(Successful.Possession~ Paint.Touches+Change.Sides.of.Floor+Player.Shot.Attempt
        +Shot.Type,
        family= binomial(link="logit"))
print(m2)

m3 = glm(Successful.Possession~ Paint.Touches+Player.Shot.Attempt
         +Shot.Type,
         family= binomial(link="logit"))
print(m3)


m4 = glm(Successful.Possession~ Paint.Touches
         +Shot.Type,
         family= binomial(link="logit"))
print(m4)

m5 = glm(Successful.Possession~ Shot.Type,
         family= binomial(link="logit"))
print(m5)

new = data.frame(Paint.Touches="2", Shot.Type= "Layup")
exp(predict(m4,new))/(exp(predict(m4,new))+1)

new1 = data.frame(Paint.Touches = "1", Shot.Type= "Three", 
                  Player.Shot.Attempt= "Tucker")
exp(predict(m3,new1))/(exp(predict(m3,new1))+1)


ggplot(data=log1)+
  geom_histogram(aes(x= Paint.Touches, fill = Shot.Type), binwidth = 1)+
  ggtitle("Histogram of Shot Types")+
  labs(x= "Paint Touches", y= "Possessions")

ggplot(data=log1)+
  geom_histogram(aes(x= Change.Sides.of.Floor, fill = Shot.Type), binwidth = .5)+
  ggtitle("Histogram of Shot Types")+
  labs(x= "Change Sides", y= "Possessions")

