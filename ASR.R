rm(list=ls())

#Set wd
setwd("Documents/Plovers")

library(psych)
library(ggplot2)
library(dplyr)
library(readr)
library(lme4)
library(lmerTest)

#import data 
ASR1<-read_csv("Plovers/ASRoverall.csv")
#overall ASR for each species for each week collected 
ASR2<-read_csv("Plovers/ASR.csv")
#ASR slit into each survey route per week collected 
str(ASR1)
str(ASR2)

#overall data
describe(ASR2$ASR)
#mean 0.52, median 0.5, range 0 1, sd 0.3, se 0.04 
hist(ASR2$ASR)
#normal distribution 

#describe the data for each species over the whole season 
describe(ASR2$ASR[ASR2$Species=="WF"])
#mean 0.50, median 0.50, range 0 1, sd 0.32, se 0.08
describe(ASR2$ASR[ASR2$Species=="MP"])
#mean 0.49, median 0.50, sd 0.26, se 0.06 
describe(ASR2$ASR[ASR2$Species=="KP"])
#mean 0.58, median 0.50, sd 0.34, se 0.09 

#subset all  data per species 
#white-fronted
wASR<-subset(ASR2, ASR2$Species=="WF", select=c("Week start", "Juliandate", "Species", "Route", "ASR"))
#madagascar
mASR<-subset(ASR2, ASR2$Species=="MP", select=c("Week start", "Juliandate", "Species", "Route", "ASR"))
#kittilitz
kASR<-subset(ASR2, ASR2$Species=="KP", select=c("Week start", "Juliandate", "Species", "Route", "ASR"))

#view data and test for normality 
#Wf
hist(wASR$ASR)
shapiro.test(wASR$ASR)
#non-significant so normal distribution p=0.2592
#mp
hist(mASR$ASR)
shapiro.test(mASR$ASR)
#non-significant so normal distribution, p=0.0585
#kp
hist(kASR$ASR)
shapiro.test(kASR$ASR)
##non-significant so normal distribution, p=0.09118

#linear-mixed model for asr
#ASR as response variable, julian day as fixed effect and route(area) as random effect 
#all species
asrmodel<-lmer(ASR~Juliandate+(1|Route), data=ASR2)
summary(asrmodel)
#white-fronted 
Wasrmodel<-lmer(ASR~Juliandate+(1|Route), data=wASR)
summary(Wasrmodel)
#MadagascarS
Masrmodel<-lmer(ASR~Juliandate+(1|Route), data=mASR)
summary(Masrmodel)
#KIttilitz
Kasrmodel<-lmer(ASR~Juliandate+(1|Route), data=kASR)
summary(Kasrmodel)



