
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
library(car)
library(gridExtra)
rm(list=ls())

Defense =read.csv(choose.files(), header=T, stringsAsFactors = TRUE)
head(Defense)

str(Defense)

is.na(Defense)
colSums(is.na(Defense))

Defense = na.omit(Defense)

is.na(Paint.Touches)

Defense$Points.Scored = as.integer(Defense$Points.Scored)


attach(Defense)
m1 = lm(Points.Scored ~ Opponent + Paint.Touches + Defender.Shot.Allowed + Post + Double +
          Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate)
summary(m1)

m0 = lm(Points.Scored ~ 1)

step(m0, scope=list(lower=m0, upper=m1, direction ="both"))

m2 = lm(Points.Scored ~  Post + Okay)
summary(m2)

m3 = lm(Points.Scored ~ Post + DJ)
summary(m3)

m4 = lm(Points.Scored ~ Paint.Touches + Post + Double +
          Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce  + Conor + Nate)


anova(m4,m3)




RNGkind(sample.kind = "default")
set.seed(223428)
train.idx <- sample(x=1:nrow(Defense), size=floor(.9*nrow(Defense)))
train.df<-Defense[train.idx,]
test.df<-Defense[-train.idx,]

myforest<- randomForest(Successful.Possession~., #remember the dangers of this short cut
                        data=train.df, ntree=1000,mtry=5, importance = TRUE)
#I set mtry = 5 because the square root of the # of x variables (26)
print(myforest)



##------------------------Tuning the Forest ---------------------------##
Btry<-c(15, 25, seq(from=50, to=1000, by=50))

for(ii in length(Btry)){
  tempforest<-randomForest(Successful.Possession~ Paint.Touches + Defender.Shot.Allowed +
                             Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate , data=train.df, mtry=Btry)
  
}

#1 Choose what values of m actually make sense 
#for the tennis data set, since k=24, we can realy only tune m between 1 and 36

#THINK: what is possible

mtry<-c(1:13)

#need to make room for thepairs of m and OOB error
#create a dataframe

keeps<-data.frame(m=rep(NA, length(mtry)), OOB_error_rate=rep(NA,length(mtry)))

for(ii in 1:length(mtry)){
  print(paste0("This is tree: ", mtry[ii]))
  tempforest<-randomForest(Successful.Possession~ Paint.Touches + Defender.Shot.Allowed +
                             Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate, data=train.df, ntree=1000, mtry=mtry[ii])
  
  keeps[ii, "m"]<- mtry[ii]
  keeps[ii, "OOB_error_rate"] <- mean(predict(tempforest)!=train.df$Successful.Possession)
  
}

rocCurve<-ggplot(keeps)+ geom_line(aes(x=m, y=OOB_error_rate))+ scale_x_continuous(breaks=mtry)+labs(x="m(mtry) value", y="OOB Error rate")
print(rocCurve)
#Use n=3 for final forest

finalforest<- randomForest(Successful.Possession~ Paint.Touches + Defender.Shot.Allowed +
                             Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate , data=train.df, ntree= 1000, mtry=12, importance=TRUE)
print(finalforest)

#OOB accuracy
(355+76)/(355+65+179+76)
#0.6385185


#OOB error
#36.15%

pi_hat<- predict(finalforest, test.df, type="prob")[,"Yes"]

rocCurve<- roc(response=test.df$Successful.Possession, predictor= pi_hat, levels=c("No", "Yes"))
plot(rocCurve, print.thres=TRUE, print.auc=TRUE)

#Specificity: .560 (True Positive)
#Sensitivity: .577 (True Negative)
#AUC: .489
#the second goal is interpretation: understanding how the x variable impact the y variable
# A random forest can help us understand which x variables are the most important. IT does this by giving a 
# variable importance plot. 
#NOTE: you must have importance = TRUE
# in the forest code so that R collects the necessary data

varImpPlot(finalforest, type=1)


mm1 = glm(Successful.Possession ~ Paint.Touches + Defender.Shot.Allowed + 
            Roman + DJ + Tucker + Sardaar + Brodie  + Sturtz + Eric + Bryce + Okay + Conor + Nate,
          family = binomial(link = "logit"))

mm0 = glm(Successful.Possession ~ 1,
          family = binomial(link = "logit"))

step(mm0, scope=list(lower=mm0, upper=mm1, direction ="both"))

mm2 = glm(Successful.Possession ~ Paint.Touches + Eric,
          family = binomial(link = "logit"))
summary(mm2)
#Eric not statistically significant


#----------Two Man -------------

Defense$Ro.DJ = ifelse(Roman == "Yes" & DJ == "Yes", "Yes", "No")
Defense$Ro.Tucker = ifelse(Roman == "Yes" & Tucker == "Yes", "Yes", "No")
Defense$Ro.Daar = ifelse(Roman == "Yes" & Sardaar == "Yes", "Yes", "No")
Defense$Ro.Sturtz = ifelse(Roman == "Yes" & Sturtz == "Yes", "Yes", "No")
Defense$Ro.Conor = ifelse(Roman == "Yes" & Conor == "Yes", "Yes", "No")
Defense$Ro.Brodie = ifelse(Roman == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Ro.Eric = ifelse(Roman == "Yes" & Eric == "Yes", "Yes", "No")

Defense$DJ.Tucker = ifelse(DJ == "Yes" & Tucker == "Yes", "Yes", "No")
Defense$DJ.Daar = ifelse(DJ == "Yes" & Sardaar == "Yes", "Yes", "No")
Defense$DJ.Sturtz = ifelse(DJ == "Yes" & Sturtz == "Yes", "Yes", "No")
Defense$DJ.Conor = ifelse(DJ == "Yes" & Conor == "Yes", "Yes", "No")
Defense$DJ.Brodie = ifelse(DJ == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$DJ.Eric = ifelse(DJ == "Yes" & Eric == "Yes", "Yes", "No")

Defense$Tuck.Daar = ifelse(Tucker == "Yes" & Sardaar == "Yes", "Yes", "No")
Defense$Tuck.Sturtz = ifelse(Tucker == "Yes" & Sturtz == "Yes", "Yes", "No")
Defense$Tuck.Conor = ifelse(Tucker == "Yes" & Conor == "Yes", "Yes", "No")
Defense$Tuck.Brodie = ifelse(Tucker == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Tuck.Eric = ifelse(Tucker == "Yes" & Eric == "Yes", "Yes", "No")

Defense$Daar.Sturtz = ifelse(Sardaar == "Yes" & Sturtz == "Yes", "Yes", "No")
Defense$Daar.Conor = ifelse(Sardaar == "Yes" & Conor == "Yes", "Yes", "No")
Defense$Daar.Brodie = ifelse(Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Daar.Eric = ifelse(Sardaar == "Yes" & Eric == "Yes", "Yes", "No")

Defense$Sturtz.Conor = ifelse(Sturtz == "Yes" & Conor == "Yes", "Yes", "No")
Defense$Sturtz.Brodie = ifelse(Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Sturtz.Eric = ifelse(Sturtz == "Yes" & Eric == "Yes", "Yes", "No")

Defense$Conor.Brodie = ifelse(Conor == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Conor.Eric = ifelse(Conor == "Yes" & Eric == "Yes", "Yes", "No")

attach(Defense)
model1 = lm(Points.Scored ~ Ro.DJ + Ro.Tucker + Ro.Daar + Ro.Sturtz + Ro.Conor + Ro.Brodie + Ro.Eric + DJ.Tucker + DJ.Daar+   
              + DJ.Sturtz  +  DJ.Conor + DJ.Brodie + DJ.Eric + Tuck.Daar + Tuck.Sturtz+Tuck.Conor+ Tuck.Brodie+Tuck.Eric+  Daar.Sturtz          
            + Daar.Conor+ Daar.Brodie+Daar.Eric+  Sturtz.Conor+ Sturtz.Brodie        
            + Sturtz.Eric+Conor.Brodie+  Conor.Eric )
summary(model1)

model0 = lm(Points.Scored ~ 1)

step(model0, scope=list(lower=model0, upper=model1, direction ="both"))

model2 = lm(Points.Scored ~ Daar.Conor + DJ.Tucker + Conor.Eric + 
              Ro.Eric)
summary(model2)

model3 = lm(Points.Scored ~ Ro.Tucker + Ro.Sturtz  + Tuck.Daar +  Daar.Sturtz)
summary(model3)
#Positive: Conor and Sardaar, Roman and Sturtz, Tucker and Sardaar
#Negative: Roman and Tucker, Sardaar and Sturtz

mo1 = glm(Successful.Possession ~ Ro.DJ + Ro.Tucker + Ro.Daar + Ro.Sturtz + Ro.Conor + Ro.Brodie + Ro.Eric + DJ.Tucker + DJ.Daar+   
            + DJ.Sturtz  +  DJ.Conor + DJ.Brodie + DJ.Eric + Tuck.Daar + Tuck.Sturtz+Tuck.Conor+ Tuck.Brodie+Tuck.Eric+  Daar.Sturtz          
          + Daar.Conor+ Daar.Brodie+Daar.Eric+  Sturtz.Conor+ Sturtz.Brodie        
          + Sturtz.Eric+Conor.Brodie+  Conor.Eric,
          family = binomial(link = "logit"))

mo0 = glm(Successful.Possession ~ 1,
          family = binomial(link = "logit"))

step(mo0, scope=list(lower=mo0, upper=mo1, direction ="both"))

mo2 = glm(Successful.Possession ~ Daar.Conor + Conor.Eric + 
            Daar.Sturtz + Ro.Eric, family = binomial(link = "logit"))
summary(mo2)
#Positive: Conor and Sardaar
#Negative: Conor and Eric


Defense$Conor_no_Sturtz = ifelse(Sturtz == "No" & Conor == "Yes", "Yes", "No")

attach(Defense)
model3 = lm(Points.Scored ~ Conor_no_Sturtz + Paint.Touches)
summary(model3)
#Defense is worse

model4 = lm(Points.Scored ~ Sturtz.Conor + Paint.Touches)
summary(model4)
table(Conor_no_Sturtz)

#Net negative defense when Conor is in without Sturtz
#1.57 less points allowed per game!
#5.04 per game spread difference!



#------Five Man -------
Defense$Starters = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Conor.Roman = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar.RomanDJ = ifelse(Conor == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Eric.Brodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Daar.Sturtz = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar.RomanSturtz = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Daar_Eric.DJBrodie = ifelse(Roman == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Daar.DJ = ifelse(Roman == "Yes" & Sardaar == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Daar_Eric.SturtzBrodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Daar.Tucker = ifelse(Roman == "Yes" & DJ == "Yes" & Sardaar == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar_Eric.RomanSturtzBrodie = ifelse(Conor == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar_Eric.RomanTuckerBrodie = ifelse(Conor == "Yes" & Tucker == "Yes" & Sturtz == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar_Eric.RomanDJBrodie = ifelse(Conor == "Yes" & DJ == "Yes" & Sturtz == "Yes" & Sardaar == "Yes" & Eric == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar.SturtzBrodie = ifelse(Roman == "Yes" & DJ == "Yes" & Tucker == "Yes" & Sardaar == "Yes" & Conor == "Yes", "Yes", "No")
Defense$Starters_Conor_Daar.RomanTucker = ifelse(Conor == "Yes" & DJ == "Yes" & Sardaar == "Yes" & Sturtz == "Yes" & Brodie == "Yes", "Yes", "No")



attach(Defense)



mo = lm(Points.Scored ~ Starters+                      
          Starters_Conor_Daar.RomanDJ+Starters_Eric.Brodie    +                  
          Starters_Daar.Sturtz +Starters_Conor.Roman  + Starters_Conor_Daar.RomanSturtz   +        
          Starters_Daar_Eric.DJBrodie    +Starters_Daar.DJ          +           Starters_Daar_Eric.SturtzBrodie +          
          Starters_Conor_Daar_Eric.RomanSturtzBrodie+ Starters_Daar.Tucker          +             Starters_Conor_Daar_Eric.RomanTuckerBrodie+
          Starters_Conor_Daar.RomanTucker       +     Starters_Conor_Daar_Eric.RomanDJBrodie  +   Starters_Conor_Daar.SturtzBrodie )
summary(mo)

step(m0, scope=list(lower=m0, upper=mo, direction ="both"))

mo1 = lm(Points.Scored ~ Starters_Daar.Sturtz + Starters_Daar_Eric.SturtzBrodie + 
           Starters_Conor_Daar.RomanSturtz)
summary(mo1)

#Positive: Roman, DJ, Tucker, Sardaar, Brodie
#Negative: None


moo = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.4 + Player.5)
summary(moo)

moo1 = lm(Points.Scored ~ Player.2 + Player.3 + Player.4 + Player.5)
anova(moo,moo1)
#.2882

moo2 = lm(Points.Scored ~ Player.1 + Player.3 + Player.4 + Player.5)
anova(moo,moo2)
#.03476

moo5 = lm(Points.Scored ~ Player.1 + Player.2 + Player.4 + Player.5)
anova(moo,moo5)
#.7342

moo3 = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.5)
anova(moo,moo3)
#.3212

moo4 = lm(Points.Scored ~ Player.1 + Player.2 + Player.3 + Player.4)
anova(moo,moo4)
#.6414