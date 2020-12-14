rm(list=ls())
library(tidyr)
library(dplyr)
library(ez)
library(ggplot2)
#sets working directory 
setwd("/Users/tarenrohovit/Desktop/")
#import data
cylinder <- read.csv("PT_Cylinder_Raw.csv")
avatar <- read.csv("PT_Avatar_Raw.csv")



#Remove Can't Reach trials
cylinder <- filter(cylinder, !Response =="Can't Reach")
avatar <- filter(avatar, !Response == "Can't Reach")
avatar <- filter(avatar, !Response == "Can't Reac")

#convert from int to double
avatar$Response_Time <- as.double(avatar$Response_Time)
avatar$Response_Time <- avatar$Response_Time /1000
#convert  to a factor
cylinder$Ball_Rotation <- as.factor(cylinder$Ball_Rotation)
avatar$Ball_Rotation <- as.factor(avatar$Ball_Rotation)
cylinder$Chair_Rotation <- as.factor(cylinder$Chair_Rotation)
avatar$Chair_Rotation <- as.factor(avatar$Chair_Rotation)
#Convert to logical
avatar$Accuracy <- as.logical(avatar$Accuracy)
#compute means and sd
cylinderRTMean <- mean(cylinder$Response_Time)
avatarRTMean <- mean(avatar$Response_Time)
cylinderSD <- sd(cylinder$Response_Time)
avatarSD <- sd(avatar$Response_Time)

#Remove trials in which RT was 3 sd above or below the mean
cylinder <- filter(cylinder, !Response_Time > (cylinderRTMean + 3 * cylinderSD))
cylinder <- filter(cylinder, !Response_Time < (cylinderRTMean - 3 * cylinderSD))

avatar <- filter(avatar, !Response_Time > (avatarRTMean + 3 * avatarSD))
avatar <- filter(avatar, !Response_Time < (avatarRTMean - 3 * avatarSD))

#Remove empty columns and rows with missing data
cylinder$Accuracy_dc <- NULL
cylinder$X.CanReach <- NULL
avatar$Acc_dc <- NULL
avatar <- filter(avatar, !Block == "NA")

#check for missing data
cylinder[!complete.cases(cylinder),]
avatar[!complete.cases(avatar),]

#remove subjects whose accuracy on L/R judgemnts was lower than 50% in the column data
previousSID <- cylinder[1,1]
count <- 0
accSum <- 0
newcylinder <- cylinder
for (val in 1:nrow(cylinder)){
  if (cylinder[val, 1] == previousSID){
    count <- count + 1
    accSum <- accSum + cylinder[val, 10]
  }
  else{
    if (accSum / count < .5){
      newcylinder <- filter(newcylinder, !Participant == cylinder[val-1,1])
    }
    previousSID <- cylinder[val,1]
    count <- 1
    accSum <- cylinder[val, 10]
  }
}
cylinder <- newcylinder

#remove subjects whose accuracy on L/R judgemnts was lower than 50% in the avatar data
previousSID <- avatar[1,1]
count <- 0
accSum <- 0
newavatar <- avatar
for (val in 1:nrow(avatar)){
  if (avatar[val, 1] == previousSID){
    count <- count + 1
    accSum <- accSum + avatar[val, 10]
  }
  else{
    if (accSum / count < .5){
      newavatar <- filter(newavatar, !Participant == avatar[val-1,1])
    }
    previousSID <- avatar[val,1]
    count <- 1
    accSum <- avatar[val, 10]
  }
}
avatar <- newavatar


#
#TO DO: remove subjects in exp 2 where the contoller froze in more than 10% trials. Which is exp 2? How do I know if the tracker stopped?
#

#create plots
p <- ggplot(data = cylinder,aes(y = Response_Time, x = Chair_Rotation))
p + geom_boxplot() 

p2 <- ggplot(data = avatar,aes(y = Response_Time, x = Chair_Rotation))
p2 + geom_boxplot() 
