
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
library(car)
library(gridExtra)
library(tidyverse)
rm(list=ls())

SLU =read.csv(choose.files(), header=T, stringsAsFactors = TRUE)
head(SLU)


SLU = SLU[(SLU$Shot.Type != "Team"),]
#head(SLU)

attach(SLU)

#-------------Data Visualization ----------------

ggplot(data=SLU)+
  geom_histogram(aes(x= Points.Scored, fill = Shot.Type), binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "Points Scored", y= "Possessions")

ggplot(data=SLU)+
  geom_histogram(aes(x= Points.Scored, fill = Push.Tempo), binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "Points Scored", y= "Possessions")

ggplot(data=SLU)+
  geom_histogram(aes(x= Points.Scored, fill = Player.Shot.Attempt), binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "Points Scored", y= "Possessions")

ggplot(data=SLU)+
  geom_bar(aes(x= Player.Shot.Attempt), binwidth = 1)+
  ggtitle("Points Scored by Shot Type")+
  labs(x= "Points Scored", y= "Possessions")

ggplot(data=SLU)+
  geom_bar(aes(x= Shot.Type, fill = Player.Shot.Attempt), binwidth = 1)+
  ggtitle("Shot Type Distribution")+
  labs(x= "Shot Type", y= "Possessions")

ggplot(data=SLU)+
  geom_bar(aes(x= Player.Shot.Attempt , fill = Shot.Type), binwidth = 1)+
  ggtitle("Possession Type Distribution")+
  labs(x= "Player", y= "Possessions")

Tucker = SLU[(SLU$Tucker == "Yes"),]
head(Tucker)
summary(Tucker)

ggplot(data=Tucker)+
  geom_bar(aes(x= Roman), binwidth = 1)+
  facet_wrap(vars(DJ,Sturtz))+
  ggtitle("Possession Type Distribution")+
  labs(x= "Player", y= "Possessions")

ggplot(data=Tucker)+
  geom_bar(aes(x= Roman, fill = DJ))+
  ggtitle("Tucker, Roman, DJ")+
  labs(x= "Roman", y= "Possessions")


ggplot(data=SLU)+
  geom_bar(aes(x= Shot.Type, fill = Successful.Possession))+
  facet_wrap(vars(Player.Shot.Attempt))+
  ggtitle("Tucker, Roman, DJ")+
  labs(x= "Shot Type", y= "Points Scored")


#--------------One Man ------------

m1 = lm(Points.Scored ~ Opponent + Paint.Touches + Player.Shot.Attempt + Shot.Type + 
          Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate)
summary(m1)

m0 = lm(Points.Scored ~ 1)

step(m0, scope=list(lower=m0, upper=m1, direction ="both"))



m2 = lm(Points.Scored ~ Shot.Type + Nate + Sardaar)
summary(m2)

m3 = lm(Points.Scored ~ Paint.Touches + Shot.Type)
anova(m2,m3)

.26739 + 1.96*.08732
#.4385372
.26739 - 1.96*.08732
#.0962428


RNGkind(sample.kind = "default")
set.seed(223428)
train.idx <- sample(x=1:nrow(SLU), size=floor(.8*nrow(SLU)))
train.df<-SLU[train.idx,]
test.df<-SLU[-train.idx,]


mtry<-c(1:14)


keeps<-data.frame(m=rep(NA, length(mtry)), OOB_error_rate=rep(NA,length(mtry)))

for(ii in 1:length(mtry)){
  print(paste0("This is tree: ", mtry[ii]))
  tempforest<-randomForest(Successful.Possession~ Opponent + Paint.Touches + Player.Shot.Attempt +
                             Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate, data=train.df, ntree=1000, mtry=mtry[ii])
  
  keeps[ii, "m"]<- mtry[ii]
  keeps[ii, "OOB_error_rate"] <- mean(predict(tempforest)!=train.df$Successful.Possession)
  
}

rocCurve<-ggplot(keeps)+ geom_line(aes(x=m, y=OOB_error_rate))+ scale_x_continuous(breaks=mtry)+labs(x="m(mtry) value", y="OOB Error rate")
print(rocCurve)
#Use n=5 for final forest

finalforest<- randomForest(Successful.Possession~ Opponent + Paint.Touches + Player.Shot.Attempt +
                             Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate , data=train.df, ntree= 1000, mtry=2, importance=TRUE)
print(finalforest)

#OOB accuracy
(355+76)/(355+65+179+76)
#0.6385185


#OOB error
#36.15%

pi_hat<- predict(finalforest, test.df, type="prob")[,"Yes"]

rocCurve<- roc(response=test.df$Successful.Possession, predictor= pi_hat, levels=c("No", "Yes"))
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)

#Specificity: .506 (True Positive)
#Sensitivity: .909 (True Negative)
#AUC: .766
#the second goal is interpretation: understanding how the x variable impact the y variable
# A random forest can help us understand which x variables are the most important. IT does this by giving a 
# variable importance plot. 
#NOTE: you must have importance = TRUE
# in the forest code so that R collects the necessary data

varImpPlot(finalforest, type=1)

mm1 = glm(Successful.Possession~ Paint.Touches
          +Eric + Sturtz,
          family= binomial(link="logit"))
summary(mm1)

mm1 = glm(Successful.Possession ~ Paint.Touches + Player.Shot.Attempt + 
          Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate,
          family = binomial(link = "logit"))

mm0 = glm(Successful.Possession ~ 1,
          family = binomial(link = "logit"))

step(mm0, scope=list(lower=mm0, upper=mm1, direction ="both"))

mm2 = glm(Successful.Possession ~ Paint.Touches + Player.Shot.Attempt + 
          Sardaar + Brodie  + Sturtz + Conor,
          family = binomial(link = "logit"))
summary(mm2)
#Sturtz statistically significant!!

exp(-1.1878+ .7081 + .5933)/(exp(-1.1878 + .7081 + .5933)+1)


#Sturtz!!!!!!


#----------Two Man -------------

SLU$Ro.DJ = ifelse(Roman == "Yes" & DJ == "Yes", "Yes", "No")
SLU$Ro.Tucker = ifelse(Roman == "Yes" & Tucker == "Yes", "Yes", "No")
SLU$Ro.Daar = ifelse(Roman == "Yes" & Sardaar == "Yes", "Yes", "No")
SLU$Ro.Sturtz = ifelse(Roman == "Yes" & Sturtz == "Yes", "Yes", "No")
SLU$Ro.Conor = ifelse(Roman == "Yes" & Conor == "Yes", "Yes", "No")
SLU$Ro.Brodie = ifelse(Roman == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Ro.Eric = ifelse(Roman == "Yes" & Eric == "Yes", "Yes", "No")

SLU$DJ.Tucker = ifelse(DJ == "Yes" & Tucker == "Yes", "Yes", "No")
SLU$DJ.Daar = ifelse(DJ == "Yes" & Sardaar == "Yes", "Yes", "No")
SLU$DJ.Sturtz = ifelse(DJ == "Yes" & Sturtz == "Yes", "Yes", "No")
SLU$DJ.Conor = ifelse(DJ == "Yes" & Conor == "Yes", "Yes", "No")
SLU$DJ.Brodie = ifelse(DJ == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$DJ.Eric = ifelse(DJ == "Yes" & Eric == "Yes", "Yes", "No")

SLU$Tuck.Daar = ifelse(Tucker == "Yes" & Sardaar == "Yes", "Yes", "No")
SLU$Tuck.Sturtz = ifelse(Tucker == "Yes" & Sturtz == "Yes", "Yes", "No")
SLU$Tuck.Conor = ifelse(Tucker == "Yes" & Conor == "Yes", "Yes", "No")
SLU$Tuck.Brodie = ifelse(Tucker == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Tuck.Eric = ifelse(Tucker == "Yes" & Eric == "Yes", "Yes", "No")

SLU$Daar.Sturtz = ifelse(Sardaar == "Yes" & Sturtz == "Yes", "Yes", "No")
SLU$Daar.Conor = ifelse(Sardaar == "Yes" & Conor == "Yes", "Yes", "No")
SLU$Daar.Brodie = ifelse(Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Daar.Eric = ifelse(Sardaar == "Yes" & Eric == "Yes", "Yes", "No")

SLU$Sturtz.Conor = ifelse(Sturtz == "Yes" & Conor == "Yes", "Yes", "No")
SLU$Sturtz.Brodie = ifelse(Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Sturtz.Eric = ifelse(Sturtz == "Yes" & Eric == "Yes", "Yes", "No")

SLU$Conor.Brodie = ifelse(Conor == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Conor.Eric = ifelse(Conor == "Yes" & Eric == "Yes", "Yes", "No")

attach(SLU)
model1 = lm(Points.Scored ~ Ro.DJ + Ro.Tucker + Ro.Daar + Ro.Sturtz + Ro.Conor + Ro.Brodie + Ro.Eric + DJ.Tucker + DJ.Daar+   
+ DJ.Sturtz  +  DJ.Conor + DJ.Brodie + DJ.Eric + Tuck.Daar + Tuck.Sturtz+Tuck.Conor+ Tuck.Brodie+Tuck.Eric+  Daar.Sturtz          
+ Daar.Conor+ Daar.Brodie+Daar.Eric+  Sturtz.Conor+ Sturtz.Brodie        
+ Sturtz.Eric+Conor.Brodie+  Conor.Eric )
summary(model1)

model1a = lm(Points.Scored ~ Daar.Eric)
summary(model1a)

model0 = lm(Points.Scored ~ 1)

step(model0, scope=list(lower=model0, upper=model1, direction ="both"))

model2 = lm(Points.Scored ~ Daar.Eric + Ro.Tucker + Ro.Eric)
summary(model2)

#Positive: 
#Negative: Sardaar and Eric


mo1 = glm(Successful.Possession ~ Ro.DJ + Ro.Tucker + Ro.Daar + Ro.Sturtz + Ro.Conor + Ro.Brodie + Ro.Eric + DJ.Tucker + DJ.Daar+   
            + DJ.Sturtz  +  DJ.Conor + DJ.Brodie + DJ.Eric + Tuck.Daar + Tuck.Sturtz+Tuck.Conor+ Tuck.Brodie+Tuck.Eric+  Daar.Sturtz          
          + Daar.Conor+ Daar.Brodie+Daar.Eric+  Sturtz.Conor+ Sturtz.Brodie        
          + Sturtz.Eric+Conor.Brodie+  Conor.Eric,
          family = binomial(link = "logit"))

mo0 = glm(Successful.Possession ~ 1,
          family = binomial(link = "logit"))

step(mo0, scope=list(lower=mo0, upper=mo1, direction ="both"))

mo2 = glm(Successful.Possession ~ Daar.Eric + Conor.Eric + 
            Ro.Brodie, family = binomial(link = "logit"))
summary(mo2)
#Positive: 
#Negative: Sardaar and Eric, Conor and Eric

SLU$Conor_no_Sturtz = ifelse(Sturtz == "No" & Conor == "Yes", "Yes", "No")

attach(SLU)
model3 = lm(Points.Scored ~ Conor_no_Sturtz + Paint.Touches)
summary(model3)

model4 = lm(Points.Scored ~ Sturtz.Conor + Paint.Touches)
summary(model4)
#No longer significant

#------Five Man -------

SLU$Starters = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Conor.Roman = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar.RomanDJ = ifelse(Conor == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Eric.Brodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Daar.Sturtz = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar.RomanSturtz = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Daar_Eric.DJBrodie = ifelse(Roman == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Daar.DJ = ifelse(Roman == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Daar_Eric.SturtzBrodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Daar.Tucker = ifelse(Roman == "Yes" & DJ == "Yes" & Sardaar == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar_Eric.RomanSturtzBrodie = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar_Eric.RomanTuckerBrodie = ifelse(Conor == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar_Eric.RomanDJBrodie = ifelse(Conor == "Yes" & DJ == "Yes" & Sturtz == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar.SturtzBrodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Conor == "Yes", "Yes", "No")
SLU$Starters_Conor_Daar.RomanTucker = ifelse(Conor == "Yes" & DJ == "Yes" & Sardaar == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")



attach(SLU)

colnames(SLU)


mo = lm(Points.Scored ~ Starters +                      
        Starters_Eric.Brodie    +                  
        Starters_Daar.Sturtz +Starters_Conor.Roman  + Starters_Conor_Daar.RomanSturtz   +        
      Starters_Daar_Eric.DJBrodie    +Starters_Daar.DJ          +           Starters_Daar_Eric.SturtzBrodie +          
        Starters_Conor_Daar_Eric.RomanSturtzBrodie+ Starters_Daar.Tucker  + 
        Starters_Conor_Daar.RomanTucker )
summary(mo)

mo1 = lm(Points.Scored ~ Starters_Eric.Brodie    +                  
           Starters_Conor.Roman )
summary(mo1)

step(m0, scope=list(lower=m0, upper=mo, direction ="both"))

mo1 = lm(Points.Scored ~ Starters_Conor.Roman + Starters_Eric.Brodie)
summary(mo1)

#Positive: Conor, DJ, Tucker, Sturtz, Brodie | Roman, DJ, Tucker, Sturtz, Eric
#Negative: 


moo = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.4 + Player.5)
summary(moo)

moo1 = lm(Points.Scored ~ Player.2 + Player.3 + Player.4 + Player.5)
anova(moo,moo1)
#.6439


moo2 = lm(Points.Scored ~ Player.1 + Player.3 + Player.4 + Player.5)
anova(moo,moo2)
#.5949

moo1 = lm(Points.Scored ~ Player.1 + Player.2 + Player.4 + Player.5)
anova(moo,moo1)
#.9137

moo3 = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.5)
anova(moo,moo3)
#.5674

moo4 = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.4)
anova(moo,moo4)
#.4253






