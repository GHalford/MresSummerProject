rm(list=ls())

#Set wd
setwd("Documents/Plovers")

library(ggplot2)
library(dplyr)
library(readr)

#import data 
PC<-read_csv("Plovers/Observations.csv")
str(PC)
#Family=The ID number given to each couple with a nest or brood. Positive numbers are given to those found with unhatched nests and negative numbers given to those found with chicks after hatching 
#Code= The unique colour ring cobinations that help identify each bird. 
#sex is identified through the unique colour ring combinations. Unringed birds sex is identified when their pairs sex is known. 
#Observer= the person that observed the behviours
#Observation number= upto 3 observations are completed per pair, this indicates whether it was the 1st, 2nd or 3rd observation of that pair
#Site= the site the observation was taken at 
#Brood size= the number of chicks 
#Brood age= the age of the oldest chick in the brood. When hatching date is known then an exact age in days is taken. When hatching date is unknown then age is estimated by the tarsus length of the chicks when captured. 
#Date=the date the observation was completed on 
#Start time= the time of day that the 60 minute observations began 
#Abient temperature= the temperature, taken at the nearest hour to the start time of the observation 
#Brooding addentance= the percentage time the parent was either brooding or shading their chicks
#Brooding defence= the percentage time the parent was defending their chicks from percieved predators or fighting 
#Alert= the percentage time the parent spent alert whilst around their brood e.g.head up or looking for potential danger  
#Self-maintanence= percentage time parent spent on behaviours such as preening and foraging to maintain themselves 
#Other= any other general behaviours such as standing, walking or running.Includes cover, where the parent is still in the area with the chicks but is covered by vegetation 
#away= the percentage time the parent is known to not be in the same area as their brood

#Effects on brood attendance and defence for all 3 species together
hist(PC$BroodAttendance)
hist(PC$BroodDefense)
hist(PC$Away)
#all left skewed, zero inflated
mean(PC$BroodAttendance)
#5.958955
mean(PC$BroodDefense)
#1.00597
mean(PC$Away)
#16.89776
median(PC$BroodAttendance)
#0
median(PC$BroodDefense)
#0
median(PC$Away)
#0
range(PC$BroodAttendance)
#0.0 95.8
range(PC$BroodDefense)
#0.0 7.5
range(PC$AWay)
#0 100
var(PC$BroodAttendance)
#253.0582
var(PC$BroodDefense)
#2.951092
var(PC$Away)
#1102.84

#sex
mean(PC$BroodAttendance[PC$Sex=="M"])
#male mean=5.462687
mean(PC$BroodAttendance[PC$Sex=="F"])
#female mean= 6.455224
wilcox.test(PC$BroodAttendance[PC$Sex=="M"],PC$BroodAttendance[PC$Sex=="F"], paired=TRUE)
#p=0.8644, no diffence between sexes for brood attendane 
mean(PC$BroodDefense[PC$Sex=="M"])
#male mean=1.170149
mean(PC$BroodDefense[PC$Sex=="F"])
#female mean=0.841791
wilcox.test(PC$BroodDefense[PC$Sex=="M"],PC$BroodDefense[PC$Sex=="F"], paired=TRUE)
#p=0.538, no significant difference between sexes 
mean(PC$Away[PC$Sex=="M"])
#male mean=11.54478
mean(PC$Away[PC$Sex=="F"])
#female mean=22.25075
wilcox.test(PC$Away[PC$Sex=="M"],PC$Away[PC$Sex=="F"], paired=TRUE)
#p=0.191, no significant difference between sexes 

##Effects on brood attendance and defence for each species individually 
#subset data into species 
#Whitefronted Plovers
WF<-subset(PC, PC$Species=="Wfp", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))
#Madagascar Plovers
MP<-subset(PC, PC$Species=="MP", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))
#Kittilitz Plovers
KP<-subset(PC, PC$Species=="KiP", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))


#Whitefronted Plovers effects on Brood attendance and defence 
hist(WF$BroodAttendance)
hist(WF$BroodDefense)
hist(WF$Away)
#both left skewed, zero inflated
shapiro.test(WF$BroodAttendance)
#non-normal distribution 
shapiro.test(WF$BroodDefense)
#non-normal distribution
shapiro.test(WF$Away)
#non-normal
mean(WF$BroodAttendance)
#7.207353
mean(WF$BroodDefense)
#1.055882
mean(WF$Away)
#7,686765
range(WF$BroodAttendance)
#0.0 95.8
range(WF$BroodDefense)
#0.0 7.5
range(WF$Away)
#0.0 79.2
var(WF$BroodAttendance)
#376.8013
var(WF$BroodDefense)
#3.028771
var(WF$Away)
#255.3077

#difference between sexes
mean(WF$BroodAttendance[WF$Sex=="M"])
#male mean=3.655882
mean(WF$BroodAttendance[WF$Sex=="F"])
#female mean= 10.75882
wilcox.test(WF$BroodAttendance[WF$Sex=="M"], WF$BroodAttendance[WF$Sex=="F"], paired=TRUE)
#p=0.1179, no significant diffence between sexes for brood attendane, however there is a large difference between the means of males and females  
mean(WF$BroodDefense[WF$Sex=="M"])
#male mean=1.155882
mean(WF$BroodDefense[WF$Sex=="F"])
#female mean=0.9558824
wilcox.test(WF$BroodDefense[WF$Sex=="M"],WF$BroodDefense[WF$Sex=="F"], paired=TRUE)
#p=0.7817, no significant difference between sexes 
mean(WF$Away[WF$Sex=="M"])
#male mean=12.16176
mean(WF$Away[WF$Sex=="F"])
#female mean= 3.211765
wilcox.test(WF$Away[WF$Sex=="M"], WF$Away[WF$Sex=="F"], paired=TRUE)
#p=0.0.056, no significant diffence between sexes for time spent away from brood 


#Madagascar Plovers effects on Brood attendance and defence

hist(MP$BroodAttendance)
hist(MP$BroodDefense)
hist(MP$Away)
#all left skewed, zero inflated
mean(MP$BroodAttendance)
#3.007059
mean(MP$BroodDefense)
#1.511765
mean(MP$Away)
#3.944118
range(MP$BroodAttendance)
#0.0 31.7
range(MP$BroodDefense)
#0.0 6.7
range(MP$Away)
#0.0 39.2
var(MP$BroodAttendance)
#86.05726
var(MP$BroodDefense)
#4.327736
var(MP$Away)
#97.89951

#sex differences 
mean(MP$BroodAttendance[MP$Sex=="M"])
#male mean=4.070588
mean(MP$BroodAttendance[MP$Sex=="F"])
#female mean= 3.923529
wilcox.test(MP$BroodAttendance[MP$Sex=="M"], MP$BroodAttendance[MP$Sex=="F"], paired=TRUE)
#p=0.7699, no significant diffence between sexes for brood attendane,   
mean(MP$BroodDefense[MP$Sex=="M"])
#male mean=2.005882
mean(MP$BroodDefense[MP$Sex=="F"])
#female mean=1.017647
wilcox.test(MP$BroodDefense[MP$Sex=="M"], MP$BroodDefense[MP$Sex=="F"], paired=TRUE)
#p=0.178, no significant difference between sexes, however there is a large difference between the means of males and females
#sex
mean(MP$Away[MP$Sex=="M"])
#male mean=3.088235
mean(MP$Away[MP$Sex=="F"])
#female mean= 4.8
wilcox.test(MP$Away[MP$Sex=="M"], MP$Away[MP$Sex=="F"], paired=TRUE)
#p=0.2421, no significant diffence between sexes 


#Kittlilitz Plovers 
hist(KP$BroodAttendance)
hist(KP$BroodDefense)
#both left skewed, zero inflated
hist(KP$Away)
#non-normal distribution, split distribution between high and low percentages 
mean(KP$BroodAttendance)
#5.390625
mean(KP$BroodDefense)
#0.3625
mean(KP$Away)
#50.23438
range(KP$BroodAttendance)
#0.0 55.8
range(KP$BroodDefense)
#0.0 3.3
range(KP$Away)
#0 100
var(KP$BroodAttendance)
#171.7415
var(KP$BroodDefense)
#0.7946774
var(KP$Away)
#2558.209

#sex differences
mean(KP$BroodAttendance[KP$Sex=="M"])
#male mean=10.78125
mean(KP$BroodAttendance[KP$Sex=="F"])
#female mean=0
wilcox.test(KP$BroodAttendance[KP$Sex=="M"], KP$BroodAttendance[KP$Sex=="F"], paired=TRUE)
#p=0.001646,  significant diffence between sexes for brood attendane,   
mean(KP$BroodDefense[KP$Sex=="M"])
#male mean=0.3125
mean(KP$BroodDefense[KP$Sex=="F"])
#female mean=0.4125
mean(KP$BroodDefense[KP$Sex=="F"])
#female mean=0.4125
wilcox.test(KP$BroodDefense[KP$Sex=="M"], KP$BroodDefense[KP$Sex=="F"], paired=TRUE)
#p=0.5243, no significant difference between sexes
mean(KP$Away[KP$Sex=="M"])
#male mean=19.21875
mean(KP$Away[KP$Sex=="F"])
#female mean=81.25
wilcox.test(KP$Away[KP$Sex=="M"], KP$Away[KP$Sex=="F"], paired=TRUE)
#p=0.0008251,  significant diffence between sexes

