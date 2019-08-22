rm(list=ls())

##Set wd
setwd("~Documents/Plovers")

library(ggplot2)
library(glmm)
library(MASS)
library(dplyr)
library(sjstats)
library(MuMIn)


#import data 
#brood attendance and defence
b<- read.csv("...Documents/Plovers/Broodbehaviour.csv")
#Species is the plover species: Whitefronted=WfP, Madagascar=Mp or Kittilitz=KP
#FamilyID is a number given to each individual family, each replicate of the same family has the same number 
#Family is the original codes given to each family, can have the same codes for different species 
#Observer is the person that recorded the behavioural observation
#Site is the area that the observation was taken in 
#BroodSize is the number of chicks the family contained 
#BroodAge is the age of the oldest chick in the brood 
#Dste is the date the observation was recorded
#Julian is the date transformed into julian date 
#StartTime is the time the observation was recorded
#AmbientTemp is the temperature at the nearest hour to the starttime of the observation 
#M is male brood care as a percentage (defence and attendance added together)
#F is female brood care as a percentage (defence and attendance added together)
#ASR is the asr at the nearest survey date to the date of observation 
#MT is male brood care to the nearest integer
#FT is female brood care to the nearest integer
#MB is percentage male brood attendance
#FB is percentage female brood attendance
#MD is percentage male brood defence 
#FD is percentage female brood defence 

#visualise data
head(b)
summary(b)


#plot brood attandance and defense against ASR
PlotA<-plot(b$MB~b$ASR)
PlotB<-plot(b$FB~b$ASR)
PLotC<-plot(b$MD~b$ASR)
PlotD<-plot(b$FD~b$ASR)
#no obvious correlation 

#plot per species (attendance and defence together as brood care)
#Whitefronted 
PLotA1<-plot(b$M[b$Species=="WfP"]~b$ASR[b$Species=="WfP"])
#no obvious correlation
PlotB1<-plot(b$F[b$Species=="WfP"]~b$ASR[b$Species=="WfP"])
#could be a slight positive correlation in females 

#madagascar
PLotA2<-plot(b$M[b$Species=="MP"]~b$ASR[b$Species=="MP"])
#seems like a possible positive correlation between ASR and male attandance
PlotB2<-plot(b$F[b$Species=="MP"]~b$ASR[b$Species=="MP"])
#no obvious correlation 

#Kittilitz
PLotA3<-plot(b$M[b$Species=="KP"]~b$ASR[b$Species=="KP"])
#negative correlation in males
PlotB3<-plot(b$F[b$Species=="KP"]~b$ASR[b$Species=="KP"])
#no obvious corrlation 

#test for normality 
#attendance
hist(b$MB)
hist(b$FB)
shapiro.test(b$MB)
shapiro.test(b$FB)
#non-normal distribution, histogram suggests a poisson distribution
hist(b$MD)
hist(b$FD)
shapiro.test(b$MD)
shapiro.test(b$FD)
#non-notrmal distribution, poisson again 


#ASR
ASR1<-glm(MB~ASR, family=poisson, data=b)
summary(ASR1)
#overall ASR does not seem to have an effect on the male BA 
ASR2<-glm(FB~ASR, family=poisson, data=b)
summary(ASR2)
#ASR does have a significant influence on female BA
ASR3<-glm(MD~ASR, family=poisson, data=b)
summary(ASR3)
#overall ASR does not seem to have an effect on the male BD
ASR4<-glm(FD~ASR, family=poisson, data=b)
summary(ASR4)
#ASR does have a significant influence on female BD

#Site 
Site1<-glm(M~Site, family=poisson, data=b)
summary(Site1)
#overall site does not seem to have an effect on the male behaviour 
Site2<-glm(F~Site, family=poisson, data=b)
summary(Site2)
#also does not effect female behaviour

#BroodSize
Size1<-glm(MB~BroodSize, family=poisson, data=b)
summary(Size1)
#overall BroodSize doesnt have an effect on behaviour, leave out of model?  
Size2<-glm(FB~BroodSize, family=poisson, data=b)
summary(Size2)
#no effect on female behaviour 

#BroodAge
Age1<-glm(MB~BroodAge, family=poisson, data=b)
summary(Age1)
#overall BroodAge does seem to have effect on the male behaviour, p=<2e-16
Age2<-glm(FB~BroodAge, family=poisson, data=b)
summary(Age2)
#also an effect on female behaviour also, p=<2e-16

#Julian date
Date1<-glm(M~Julian, family=poisson, data=b)
summary(Date1)
#overall BroodAge does seem to have effect on the male behaviour, p=<2e-16
Date2<-glm(F~Julian, family=poisson, data=b)
summary(Date2)
#also an effect on female behaviour also, p=<2e-16

#Start time 
Time1<-glm(M~StartTime, family=poisson, data=b)
summary(Time1)
#overall BroodAge does seem to have effect on the male behaviour, p=3.13e-06
Time2<-glm(F~StartTime, family=poisson, data=b)
summary(Time2)
#also an effect on female behaviour also, p=2.55e-15

#Temperature 
Temp1<-glm(MT~AmbientTemp, family=poisson, data=b)
summary(Temp1)
#overall temperature does seem to have effect on the male behaviour, p=0.00412
Temp2<-glm(F~AmbientTemp, family=poisson, data=b)
summary(Temp2)
#also has an effect on female behaviour, p=1.25e-14


#general linear fixed effect model for brood care (defence and attendance together) on all 3 species 
Model1<-glmmPQL(MT ~ ASR +BroodAge + Julian + StartTime + AmbientTemp + Species, ~1|FamilyID, data=b , family=poisson)
summary(Model1)
Model2<-glmmPQL(FT ~ ASR + BroodAge + Julian + StartTime + AmbientTemp + Species,~ 1|FamilyID, data=b , family=poisson)
summary(Model2)
#analyse model results 
#random effect variance 
r1Var<-VarCorr(Model1)
print(r1Var)
#marginal and conditional R^2
r.squaredGLMM(Model1)
r2Var<-VarCorr(Model2)
print(r2Var)
r.squaredGLMM(Model2)

#general linear mix effect models for brood attendance alone, all species 
Model1a<-glmmPQL(MB ~ ASR +BroodAge + Julian + StartTime + AmbientTemp + Species, ~1|FamilyID, data=b , family=poisson)
summary(Model1a)
Model2a<-glmmPQL(FB ~ ASR + BroodAge + Julian + StartTime + AmbientTemp + Species,~ 1|FamilyID, data=b , family=poisson)
summary(Model2a)
             
r1Vara<-VarCorr(Model1a)
print(r1Vara)
r.squaredGLMM(Model1a)
r2Vara<-VarCorr(Model2a)
r2Vara
r.squaredGLMM(Model2a)             
 
#general linear mixed effect models for brood defence alone, all species             
Model1b<-glmmPQL(MD ~ ASR +BroodAge + Julian + StartTime + AmbientTemp + Species, ~1|FamilyID, data=b , family=poisson)
summary(Model1b)
Model2b<-glmmPQL(FD ~ ASR + BroodAge + Julian + StartTime + AmbientTemp + Species,~ 1|FamilyID, data=b , family=poisson)
summary(Model2b)
             
r1Var<-VarCorr(Model1b)
print(r1Var)
r.squaredGLMM(Model1b) 
r2Var<-VarCorr(Model2b)
r2Var
r.squaredGLMM(Model2b) 

#subest data into individual species 
WF<-subset(b, b$Species=="WfP", select=c("Species","FamilyID", "Family", "Site", "BroodSize", "BroodAge", "Julian", "StartTime", "AmbientTemp", "M", "F", "ASR", "MT", "FT"))
#Madagascar Plovers
MP<-subset(b, b$Species=="MP", select=c("Species","FamilyID", "Family", "Site", "BroodSize", "BroodAge", "Julian", "StartTime", "AmbientTemp", "M", "F", "ASR", "MT", "FT"))
#Kittilitz Plovers
KP<-subset(b, b$Species=="KP", select=c("Species","FamilyID", "Family", "Site", "BroodSize", "BroodAge", "Julian", "StartTime", "AmbientTemp", "M", "F", "ASR", "MT", "FT"))

#Mix effect models for individual species 
#GLMM for WfP brood care, males and females separated 
model3<-glmmPQL(MT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=WF , family=poisson)
summary(model3)
model4<-glmmPQL(FT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=WF , family=poisson)
summary(model4)
#GLMM for MP brood care, males and females separated
model5<-glmmPQL(MT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=MP , family=poisson)
summary(model5)
model6<-glmmPQL(FT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=MP , family=poisson)
summary(model6)
#GLMM for KiP brood care, males and females separated  
model7<-glmmPQL(MT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=KP , family=poisson)
summary(model7)
model8<-glmmPQL(FT ~ ASR  + BroodAge + Julian + StartTime +AmbientTemp, ~1|FamilyID, data=KP, family=poisson)
summary(model8)


