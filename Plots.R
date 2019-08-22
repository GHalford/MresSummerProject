
#Set wd
setwd("Documents/Plovers")

library(rptR)
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
#away= the percentage time the parent is 

WF<-subset(PC, PC$Species=="Wfp", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))
#Madagascar Plovers
MP<-subset(PC, PC$Species=="MP", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))
#Kittilitz Plovers
KP<-subset(PC, PC$Species=="KiP", select=c("Bird no.", "Species", "Family", "Code", "Sex", "Observation no.", "Site", "BroodSize", "BroodAge", "Date", "StartTime", "AmbientTemp", "BroodAttendance", "BroodDefense", "Alert", "Self-maintanence", "Other", "Away"))

#WF
mean(WF$BroodAttendance[WF$Sex=="M"])
#male mean=3.655882
mean(WF$BroodAttendance[WF$Sex=="F"])
#female mean= 10.75882
mean(WF$BroodDefense[WF$Sex=="M"])
#male mean=1.155882
mean(WF$BroodDefense[WF$Sex=="F"])
#male mean=0.9558824
mean(WF$Alert[WF$Sex=="M"])
#male mean=1.617647
mean(WF$Alert[WF$Sex=="F"])
#female mean= 2.797059
mean(WF$`Self-maintanence`[WF$Sex=="M"])
#male mean=16.72941
mean(WF$`Self-maintanence`[WF$Sex=="F"])
#10.01765
mean(WF$Other[WF$Sex=="M"])
#male mean=65.33235
mean(WF$Other[WF$Sex=="F"])
#female mean= 72.15882
mean(WF$Away[WF$Sex=="M"])
#male mean=12.16176
mean(WF$Away[WF$Sex=="F"])
#3.211765

#MP
mean(MP$BroodAttendance[MP$Sex=="M"])
#male mean=4.070588
mean(MP$BroodAttendance[MP$Sex=="F"])
#female mean= 3.923529
mean(MP$BroodDefense[MP$Sex=="M"])
#male mean=2.005882
mean(MP$BroodDefense[MP$Sex=="F"])
#male mean=1.017647
mean(MP$Alert[MP$Sex=="M"])
#male mean=2.694118
mean(MP$Alert[MP$Sex=="F"])
#female mean= 4.505882
mean(MP$`Self-maintanence`[MP$Sex=="M"])
#male mean=12.7
mean(MP$`Self-maintanence`[MP$Sex=="F"])
#12.84706
mean(MP$Other[MP$Sex=="M"])
#male mean=75.88824
mean(MP$Other[MP$Sex=="F"])
#female mean= 72.07059
mean(MP$Away[MP$Sex=="M"])
#male mean=3.088235
mean(MP$Away[MP$Sex=="F"])
#4.8

#KP
mean(KP$BroodAttendance[KP$Sex=="M"])
#male mean=10.78125
mean(KP$BroodAttendance[KP$Sex=="F"])
#female mean= 0
mean(KP$BroodDefense[KP$Sex=="M"])
#male mean=0.3125
mean(KP$BroodDefense[KP$Sex=="F"])
#male mean=0.4125
mean(KP$Alert[KP$Sex=="M"])
#male mean=1.975
mean(KP$Alert[KP$Sex=="F"])
#female mean= 1.71875
mean(KP$`Self-maintanence`[KP$Sex=="M"])
#male mean=2.96875
mean(KP$`Self-maintanence`[KP$Sex=="F"])
#12.84706
mean(KP$Other[KP$Sex=="M"])
#male mean=52.14375
mean(KP$Other[KP$Sex=="F"])
#female mean= 14.10625
mean(KP$Away[KP$Sex=="M"])
#male mean=3.088235
mean(KP$Away[KP$Sex=="F"])
#4.8

#subset data into species 
WF2<-subset(B, B$Species=="WfP", select=c("BroodAge", "Species", "Family", "MB", "FB", "MD", "FD"))
MP2<-subset(B, B$Species=="MP", select=c("BroodAge", "Species", "Family", "MB", "FB", "MD", "FD"))
KP2<-subset(B, B$Species=="KP", select=c("BroodAge", "Species", "Family", "MB", "FB", "MD", "FD"))


#plot individual species data onto scatter plot, males and females % time spent brooding as chicks aged 
#white-fronted
plot(WF2$FB~WF2$BroodAge, col="red",  xlab="Brood Age (days)", ylab="Brooding (% parents time)", main="White-fronted Plover")
points(WF2$MB~WF2$BroodAge, col="blue")
#madagascar
plot(MP2$FB~MP2$BroodAge, col="red", xlab="Brood Age (days)", ylab="Brooding (% parents time)", xlim=c(0,20), main="Madagascar Plover")
points(MP2$MB~MP2$BroodAge, col="blue")
#kittilitz
plot(KP2$MB~KP2$BroodAge, col="blue",xlab="Brood Age (days)", ylab="Brooding (% parents time)",xlim=c(0,20), main="Kittilitz Plover")
points(KP2$FB~KP2$BroodAge, col="red")

#plot individual species data onto scatter plot, males and females % time spent brooding as chicks aged 
#white-fronted
plot(WF2$MD~WF2$BroodAge, col="blue",  xlab="Brood Age (days)", ylab="Brood efence (% parents time)", main="White-fronted Plover")
points(WF2$FD~WF2$BroodAge, col="red")
#madagascar
plot(MP2$FD~MP2$BroodAge, col="red", xlab="Brood Age (days)", ylab="Brood defence (% parents time)",xlim=c(0,20), main="Madagascar Plover")
points(MP2$MD~MP2$BroodAge, col="blue")
#kittilitz 
plot(KP2$FD~KP2$BroodAge, col="red",xlab="Brood Age (days)", ylab="Brood defence (% parents time)", xlim=c(0,20),main="Kittilitz Plover")
points(KP2$MD~KP2$BroodAge, col="blue")

