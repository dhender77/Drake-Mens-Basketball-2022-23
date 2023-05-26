library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(randomForest)
library(car)
library(gridExtra)
rm(list=ls())


mm =read.csv(choose.files(), header=T, stringsAsFactors = TRUE)
head(mm)

elite = ifelse(mm$Seed <= 8 | mm$Tourney.Games.Won >=2, 1,
              0 )
mm$elite = elite 
head(mm)
attach(mm)

m1 = glm(elite ~ FG_pct + OFG_pct + X2P_pct + O2P_pct + X3PA + X3P_pct + O3PA + O3P_pct + FTA + FT_pct + OFTA + ORB+ DRB + OORB + AST+ STL+ BLK + TOV + OTOV + PF + OPF + PTS + OPTS + AdjO + AdjD + AdjT + NCSOS
         ,family = binomial(link="logit"))
summary(m1)
vif(m1)
#FG_pct, OFG_pct, X2P_pct, O2P_pct, OFTA, DRB, OTOV, PF, PTS, OPTS,ADjT

m2 = glm(elite ~ FG_pct + OFG_pct + X3PA + X3P_pct + O3PA + O3P_pct + FTA + FT_pct + OFTA + ORB+ DRB + OORB + AST+ STL+ BLK + TOV + OTOV + PF + OPF + PTS + OPTS + AdjO + AdjD + AdjT + NCSOS
         ,family = binomial(link="logit"))
summary(m2)
vif(m2)

ggplot(data=mm)+
  geom_histogram(aes(x= OORB), binwidth = .05)+
  ggtitle("Offensive Rebounds")+
  labs(x= "Percentile Nationally", y= "Team Count")

ggplot(data=mm)+
  geom_histogram(aes(x= X3PA), binwidth = .05)+
  ggtitle("Offensive Rebounds")+
  labs(x= "Percentile Nationally", y= "Team Count")

ggplot(data=mm)+
  geom_histogram(aes(x= AdjO), fill = "dark blue", binwidth = .05)+
  ggtitle("Offensive Efficiency")+
  xlim(.45,1)+
  ylim(0,20)+
  labs(x= "Percentile Nationally", y= "Team Count")


mm1 = subset(mm, AdjD >= .90)
mean(AdjO)

ggplot(data=mm1)+
  geom_histogram(aes(x= AdjO), fill = "dark blue", binwidth = .05)+
  ggtitle("Off Eff Among Elite Defensive Teams")+
  xlim(.45,1)+
  ylim(0,10)+
  labs(x= "Percentile Nationally", y= "Team Count")
mean(mm1$AdjO)

ggplot(data=mm)+
  geom_histogram(aes(x= AdjD), fill = "dark blue", binwidth = .05)+
  ggtitle("Defensive Efficiency")+
  xlim(.45,1)+
  ylim(0,20)+
  labs(x= "Percentile Nationally", y= "Team Count")

mm2 = subset(mm, AdjO >= .90)

ggplot(data=mm2)+
  geom_histogram(aes(x= AdjD), fill = "dark blue", binwidth = .05)+
  ggtitle("Def Eff: Elite Offensive Teams")+
  xlim(.45,1)+
  ylim(0,10)+
  labs(x= "Percentile Nationally", y= "Team Count")

mean(AdjD)
mean(mm2$AdjD)

mm4 = subset(mm, AdjO <= .90)
mean(mm4$AdjD)

mm3 = subset(mm, AdjD <= .90)
mean(mm3$AdjO)

ggplot(data=mm3)+
  geom_histogram(aes(x= AdjO), fill = "dark blue", binwidth = .05)+
  ggtitle("Off Eff: Non-Elite Defensive Teams")+
  xlim(.45,1)+
  ylim(0,10)+
  labs(x= "Percentile Nationally", y= "Team Count")
mean(mm3$AdjO)

ggplot(data=mm4)+
  geom_histogram(aes(x= AdjD), fill = "dark blue", binwidth = .05)+
  ggtitle("Def Eff: Non-Elite Offensive Teams")+
  xlim(.45,1)+
  ylim(0,10)+
  labs(x= "Percentile Nationally", y= "Team Count")
mean(mm4$AdjD)


ggplot(data=mm2)+
  geom_histogram(aes(x= TOV), fill = "dark blue", binwidth = .05)+
  ggtitle("Turnovers: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,10)+
  labs(x= "Percentile Nationally", y= "Team Count")

attach(mm2)

summary(TOV)
summary(Seed)
summary(X3PA)
summary(X3P_pct)
summary(O3PA)
summary(FTA)
summary(FT_pct)
summary(OFTA)
summary(ORB)
summary(DRB)
summary(OORB)
summary(AdjT)
summary(NCSOS)
summary(BLK)
summary(AST)
summary(OTOV)
summary(PF)
summary(OPF)

sum(mm2$elite == 1)
22/35

sum(X3P_pct >= .65)
sum(TOV >= .75)
sum(OORB >= .75)

sum(mm1$elite == 1)
34/48

g1 = ggplot(data=mm2)+
  geom_histogram(aes(x= X3P_pct), fill = "dark blue", binwidth = .05)+
  ggtitle("3P%: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,8)+
  labs(x= "Percentile Nationally", y= "Team Count")

g2 = ggplot(data=mm2)+
  geom_histogram(aes(x= X3PA), fill = "dark blue", binwidth = .05)+
  ggtitle("3PA/Game: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,8)+
  labs(x= "Percentile Nationally", y= "Team Count")


g3 = plot1 = ggplot(data=mm2)+
  geom_histogram(aes(x= FT_pct), fill = "dark blue", binwidth = .05)+
  ggtitle("FT%: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,8)+
  labs(x= "Percentile Nationally", y= "Team Count")

plot2 = ggplot(data=mm2)+
  geom_histogram(aes(x= TOV), fill = "dark blue", binwidth = .05)+
  ggtitle("Turnovers: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,8)+
  labs(x= "Percentile Nationally", y= "Team Count")

plot3 = ggplot(data=mm2)+
  geom_histogram(aes(x= OORB), fill = "dark blue", binwidth = .05)+
  ggtitle("ORB Allowed: Elite Offensive Mid-Majors")+
  xlim(0,1)+
  ylim(0,8)+
  labs(x= "Percentile Nationally", y= "Team Count")

grid.arrange(g1, g2, g3, ncol=3) #this allows us to put the graphs side by side
grid.arrange(plot2, plot3, ncol=2)






