#Author: Taren Rohovit
#Last updated Feb 4th, 2021



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
cylinder <- filter(cylinder, !Response_Time > (cylinderRTMean + (3 * cylinderSD)))
cylinder <- filter(cylinder, !Response_Time < (cylinderRTMean - (3 * cylinderSD)))

avatar <- filter(avatar, !Response_Time > (avatarRTMean + (3 * avatarSD)))
avatar <- filter(avatar, !Response_Time < (avatarRTMean - (3 * avatarSD)))

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
#
#crossover analaysis for avatar data. The final result is stored in avatarCrossOverRatio
#
#
#convert from meters to cm
avatar$Ball_Distance <- avatar$Ball_Distance *100
#import data containing arm length
qualtricsdata <- read.csv("QualtricsAvatar.csv")
#convert data types
qualtricsdata$Please.have.the.participant.enter.your.arm.length. <- as.double(as.character(qualtricsdata$Please.have.the.participant.enter.your.arm.length.))
qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. <- as.double(as.character(qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.))
avatar$Ball_Rotation <- as.double(as.character(avatar$Ball_Rotation))

#percents are the different chair rotations that need to be evaluted
percents <- c(0,90,120,150,180,210,240,270)
#create the empty dataframe that we'll be adding to
#avatarCrossOverRatio <- data.frame("SID" = integer(0) , "LeftCrossOverRatio" = double(0), "RightCrossOverRatio" = double(0))
leftAvatarCrossOverRatio <- data.frame()
rightAvatarCrossOverRatio <- data.frame()
leftAvatarCrossOverRatioRow <- data.frame()
rightAvatarCrossOverRatioRow <- data.frame()
#calculate different ratios for each percent 
for (percent in percents){

  #filter the data to only look at the relevent percent
  avatarcopy <- avatar
  avatarcopy <- filter(avatarcopy, Chair_Rotation == percent)
  
  
  #set initial values for min and max reach distances
  Leftmin <- 100
  Leftmax <- 0
  Rightmin <- 100
  Rightmax <- 0
  
  
  previousSID <- avatarcopy[1,1]
  #loop through the data and computer cross over ratios
  for (val in 1:nrow(avatarcopy)){
    if (avatarcopy[val, 1] == previousSID){
      #checks for left values
      if (avatarcopy[val, 7] < 0){
        if (avatarcopy[val, 6] < Leftmin){ Leftmin <- avatarcopy[val, 6]}
        if (avatarcopy[val, 6] > Leftmax){ Leftmax <- avatarcopy[val, 6]}
      }
      #checks for right values
      if (avatarcopy[val, 7] > 0){
        if (avatarcopy[val, 6] < Rightmin){ Rightmin <- avatarcopy[val, 6]}
        if (avatarcopy[val, 6] > Rightmax){ Rightmax <- avatarcopy[val, 6]}
      }
      
    }
    else{
      if (any(previousSID %in% qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.)){
        armlengthindex <- subset(qualtricsdata, qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. == previousSID)
        leftcrossoverratio <- data.frame(SID = previousSID, 
                                     CrossOverRatio = ((Leftmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
        rightcrossoverratio <- data.frame(SID = previousSID, 
                                     CrossOverRatio = ((Rightmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
        #rename rows to labels with the coresponding percent
        colnames(leftcrossoverratio) <- paste(percent, colnames(leftcrossoverratio), sep = "_")
        colnames(rightcrossoverratio) <- paste(percent, colnames(rightcrossoverratio), sep = "_")
        #add ratio to a new dataframe in correspondance to previousSID
        leftAvatarCrossOverRatioRow <- rbind(leftAvatarCrossOverRatioRow, leftcrossoverratio)
        rightAvatarCrossOverRatioRow <- rbind(rightAvatarCrossOverRatioRow, rightcrossoverratio)
      }
      if (avatarcopy[val, 7] < 0){
        Leftmin <- avatarcopy[val,6]
        Leftmax <- avatarcopy[val,6]
      }
      if (avatarcopy[val, 7] > 0){
        Rightmin <- avatarcopy[val,6]
        Rightmax <- avatarcopy[val,6]
      }
      
      previousSID <- avatarcopy[val,1]
    }
  }
  #if the ending dataframe is empty, create one
  if (nrow(leftAvatarCrossOverRatio) == 0){
    leftAvatarCrossOverRatio <- leftAvatarCrossOverRatioRow
  }
  if (nrow(rightAvatarCrossOverRatio) == 0){
    rightAvatarCrossOverRatio <-rightAvatarCrossOverRatioRow
  }
  #otherwise add the new data to the ending dataframe
  else{
  leftAvatarCrossOverRatio <- cbind(leftAvatarCrossOverRatio, leftAvatarCrossOverRatioRow)
  rightAvatarCrossOverRatio <- cbind(rightAvatarCrossOverRatio, rightAvatarCrossOverRatioRow)
  }
  #reset the looping dataframe
  leftAvatarCrossOverRatioRow <- data.frame()
  rightAvatarCrossOverRatioRow <- data.frame()
}

#
#
#crossover analaysis for cylinder data. The final result is stored in cylinderCrossOverRatio
#
#
#convert from meters to cm
cylinder$Ball_Distance <- cylinder$Ball_Distance *100
#create the empty dataframe that we'll be adding to
#cylinderCrossOverRatio <- data.frame("SID" = integer(0) , "LeftCrossOverRatio" = double(0), "RightCrossOverRatio" = double(0))
#import data containing arm length
qualtricsdata <- read.csv("QualtricsCylinder.csv")
#convert data types
qualtricsdata$Please.have.the.participant.enter.your.arm.length. <- as.double(as.character(qualtricsdata$Please.have.the.participant.enter.your.arm.length.))
qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. <- as.double(as.character(qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.))
cylinder$Ball_Rotation <- as.double(as.character(cylinder$Ball_Rotation))

#percents are the different chair rotations that need to be evaluted
percents <- c(0,90,120,150,180,210,240,270)
leftCylinderCrossOverRatio <- data.frame()
rightCylinderCrossOverRatio <- data.frame()
leftCylinderCrossOverRatioRow <- data.frame()
rightCylinderCrossOverRatioRow <- data.frame()

for (percent in percents){
  
  #filter the data to only look at the relevent percent
  cylindercopy <- cylinder
  cylindercopy <- filter(cylindercopy, Chair_Rotation == percent)
  
  
  #set initial values for min and max reach distances
  Leftmin <- 100
  Rightmax <- 0
  Leftmin <- 100
  Rightmax <- 0
  
  previousSID <- cylindercopy[1,1]
  #loop through the data and computer cross over ratios
  for (val in 1:nrow(cylindercopy)){
    if (cylindercopy[val, 1] == previousSID){
      #checks for left values
      if (cylindercopy[val, 7] < 0){
        if (cylindercopy[val, 6] < Leftmin){ Leftmin <- cylindercopy[val, 6]}
        if (cylindercopy[val, 6] > Leftmax){ Leftmax <- cylindercopy[val, 6]}
      }
      #checks for right values
      if (cylindercopy[val, 7] > 0){
        if (cylindercopy[val, 6] < Rightmin){ Rightmin <- cylindercopy[val, 6]}
        if (cylindercopy[val, 6] > Rightmax){ Rightmax <- cylindercopy[val, 6]}
      }
    }
    else{
      if (any(previousSID %in% qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.)){
        armlengthindex <- subset(qualtricsdata, qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. == previousSID)
        leftcrossoverratio <- data.frame(SID = previousSID, 
                                     CrossOverRatio = ((Leftmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
        rightcrossoverratio <- data.frame(SID = previousSID, 
                                     CrossOverRatio = ((Rightmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
        
        colnames(leftcrossoverratio) <- paste(percent, colnames(leftcrossoverratio), sep = "_")
        colnames(rightcrossoverratio) <- paste(percent, colnames(rightcrossoverratio), sep = "_")
        #add ratio to a new dataframe in correspondance to previousSID
        leftCylinderCrossOverRatioRow <- rbind(leftCylinderCrossOverRatioRow, leftcrossoverratio)
        rightCylinderCrossOverRatioRow <- rbind(rightCylinderCrossOverRatioRow, rightcrossoverratio)
        
      }
      if (cylindercopy[val, 7] < 0){
        Leftmin <- cylindercopy[val,6]
        Leftmax <- cylindercopy[val,6]
      }
      if (cylindercopy[val, 7] > 0){
        Rightmin <- cylindercopy[val,6]
        Rightmax <- cylindercopy[val,6]
      }
      previousSID <- cylindercopy[val,1]
    }
  }
  #if the ending dataframe is empty, create one
  if (nrow(leftCylinderCrossOverRatio) == 0){
    leftCylinderCrossOverRatio <- leftCylinderCrossOverRatioRow
  }
  if (nrow(rightCylinderCrossOverRatio) == 0){
    rightCylinderCrossOverRatio <-rightCylinderCrossOverRatioRow
  }
  #otherwise add the new data to the ending dataframe
  else{
    leftCylinderCrossOverRatio <- cbind(leftCylinderCrossOverRatio, leftCylinderCrossOverRatioRow)
    rightCylinderCrossOverRatio <- cbind(rightCylinderCrossOverRatio, rightCylinderCrossOverRatioRow)
  }
  #reset the looping dataframe
  leftCylinderCrossOverRatioRow <- data.frame()
  rightCylinderCrossOverRatioRow <- data.frame()
}

#Creates a summary of the cross over ratios by collapsing accross subjects. 
leftRatioMeansAvatar <- data.frame(rotation_0 = mean(leftAvatarCrossOverRatio$`0_CrossOverRatio`), 
                         rotation_90 = mean(leftAvatarCrossOverRatio$`90_CrossOverRatio`),
                         rotation_120 = mean(leftAvatarCrossOverRatio$`120_CrossOverRatio`),
                         rotation_150 = mean(leftAvatarCrossOverRatio$`150_CrossOverRatio`),
                         rotation_180 = mean(leftAvatarCrossOverRatio$`180_CrossOverRatio`),
                         rotation_210 = mean(leftAvatarCrossOverRatio$`210_CrossOverRatio`),
                         rotation_240 = mean(leftAvatarCrossOverRatio$`240_CrossOverRatio`),
                         rotation_270 = mean(leftAvatarCrossOverRatio$`270_CrossOverRatio`),
                         LeftOrRight = "Left")
rightRatioMeansAvatar <- data.frame(rotation_0 = mean(rightAvatarCrossOverRatio$`0_CrossOverRatio`), 
                             rotation_90 = mean(rightAvatarCrossOverRatio$`90_CrossOverRatio`),
                             rotation_120 = mean(rightAvatarCrossOverRatio$`120_CrossOverRatio`),
                             rotation_150 = mean(rightAvatarCrossOverRatio$`150_CrossOverRatio`),
                             rotation_180 = mean(rightAvatarCrossOverRatio$`180_CrossOverRatio`),
                             rotation_210 = mean(rightAvatarCrossOverRatio$`210_CrossOverRatio`),
                             rotation_240 = mean(rightAvatarCrossOverRatio$`240_CrossOverRatio`),
                             rotation_270 = mean(rightAvatarCrossOverRatio$`270_CrossOverRatio`),
                             LeftOrRight = "Right")
leftRatioMeansCylinder <- data.frame(rotation_0 = mean(leftCylinderCrossOverRatio$`0_CrossOverRatio`), 
                                   rotation_90 = mean(leftCylinderCrossOverRatio$`90_CrossOverRatio`),
                                   rotation_120 = mean(leftCylinderCrossOverRatio$`120_CrossOverRatio`),
                                   rotation_150 = mean(leftCylinderCrossOverRatio$`150_CrossOverRatio`),
                                   rotation_180 = mean(leftCylinderCrossOverRatio$`180_CrossOverRatio`),
                                   rotation_210 = mean(leftCylinderCrossOverRatio$`210_CrossOverRatio`),
                                   rotation_240 = mean(leftCylinderCrossOverRatio$`240_CrossOverRatio`),
                                   rotation_270 = mean(leftCylinderCrossOverRatio$`270_CrossOverRatio`),
                                   LeftOrRight = "Left")
rightRatioMeansCylinder <- data.frame(rotation_0 = mean(rightCylinderCrossOverRatio$`0_CrossOverRatio`), 
                                    rotation_90 = mean(rightCylinderCrossOverRatio$`90_CrossOverRatio`),
                                    rotation_120 = mean(rightCylinderCrossOverRatio$`120_CrossOverRatio`),
                                    rotation_150 = mean(rightCylinderCrossOverRatio$`150_CrossOverRatio`),
                                    rotation_180 = mean(rightCylinderCrossOverRatio$`180_CrossOverRatio`),
                                    rotation_210 = mean(rightCylinderCrossOverRatio$`210_CrossOverRatio`),
                                    rotation_240 = mean(rightCylinderCrossOverRatio$`240_CrossOverRatio`),
                                    rotation_270 = mean(rightCylinderCrossOverRatio$`270_CrossOverRatio`),
                                    LeftOrRight = "Right")


#combines the left and right dataframes into a single dataframe
graphCylinder <- rbind(rightRatioMeansCylinder, leftRatioMeansCylinder)
graphAvatar <- rbind(rightRatioMeansAvatar, leftRatioMeansAvatar)
#converts to long format for the purpose of graphing
avatarLong <- gather(graphAvatar, Chair_Rotation, Value, c(rotation_0, rotation_90, rotation_120, rotation_150, rotation_180,
                                                     rotation_210, rotation_240, rotation_270),factor_key = TRUE)
cylinderLong <- gather(graphCylinder, Chair_Rotation, Value, c(rotation_0, rotation_90, rotation_120, rotation_150, rotation_180,
                                                           rotation_210, rotation_240, rotation_270),factor_key = TRUE)
#create plots
p <- ggplot(data = avatarLong, aes(y = Value, x = Chair_Rotation, color = LeftOrRight))
p + geom_point() + ggtitle("Avatar Crossover Ratios")

q <- ggplot(data = cylinderLong, aes(y = Value, x = Chair_Rotation, color = LeftOrRight))
q + geom_point() + ggtitle("Cylinder Crossover Ratios")


#This keeps the data in raw format for exporting to a csv in a readable format
leftRatioAvatar <- data.frame(SID = leftAvatarCrossOverRatio$`0_SID`,
                                   rotation_0 = leftAvatarCrossOverRatio$`0_CrossOverRatio`, 
                                   rotation_90 = leftAvatarCrossOverRatio$`90_CrossOverRatio`,
                                   rotation_120 = leftAvatarCrossOverRatio$`120_CrossOverRatio`,
                                   rotation_150 = leftAvatarCrossOverRatio$`150_CrossOverRatio`,
                                   rotation_180 = leftAvatarCrossOverRatio$`180_CrossOverRatio`,
                                   rotation_210 = leftAvatarCrossOverRatio$`210_CrossOverRatio`,
                                   rotation_240 = leftAvatarCrossOverRatio$`240_CrossOverRatio`,
                                   rotation_270 = leftAvatarCrossOverRatio$`270_CrossOverRatio`,
                                   LeftOrRight = "Left")
rightRatioAvatar <- data.frame(SID = rightAvatarCrossOverRatio$`0_SID`,
                                    rotation_0 = rightAvatarCrossOverRatio$`0_CrossOverRatio`, 
                                    rotation_90 = rightAvatarCrossOverRatio$`90_CrossOverRatio`,
                                    rotation_120 = rightAvatarCrossOverRatio$`120_CrossOverRatio`,
                                    rotation_150 = rightAvatarCrossOverRatio$`150_CrossOverRatio`,
                                    rotation_180 = rightAvatarCrossOverRatio$`180_CrossOverRatio`,
                                    rotation_210 = rightAvatarCrossOverRatio$`210_CrossOverRatio`,
                                    rotation_240 = rightAvatarCrossOverRatio$`240_CrossOverRatio`,
                                    rotation_270 = rightAvatarCrossOverRatio$`270_CrossOverRatio`,
                                    LeftOrRight = "Right")
leftRatioCylinder <- data.frame(SID = leftCylinderCrossOverRatio$`0_SID`,
                                     rotation_0 = leftCylinderCrossOverRatio$`0_CrossOverRatio`, 
                                     rotation_90 = leftCylinderCrossOverRatio$`90_CrossOverRatio`,
                                     rotation_120 = leftCylinderCrossOverRatio$`120_CrossOverRatio`,
                                     rotation_150 = leftCylinderCrossOverRatio$`150_CrossOverRatio`,
                                     rotation_180 = leftCylinderCrossOverRatio$`180_CrossOverRatio`,
                                     rotation_210 = leftCylinderCrossOverRatio$`210_CrossOverRatio`,
                                     rotation_240 = leftCylinderCrossOverRatio$`240_CrossOverRatio`,
                                     rotation_270 = leftCylinderCrossOverRatio$`270_CrossOverRatio`,
                                     LeftOrRight = "Left")
rightRatioCylinder <- data.frame(SID = rightCylinderCrossOverRatio$`0_SID`,
                                      rotation_0 = rightCylinderCrossOverRatio$`0_CrossOverRatio`, 
                                      rotation_90 = rightCylinderCrossOverRatio$`90_CrossOverRatio`,
                                      rotation_120 = rightCylinderCrossOverRatio$`120_CrossOverRatio`,
                                      rotation_150 = rightCylinderCrossOverRatio$`150_CrossOverRatio`,
                                      rotation_180 = rightCylinderCrossOverRatio$`180_CrossOverRatio`,
                                      rotation_210 = rightCylinderCrossOverRatio$`210_CrossOverRatio`,
                                      rotation_240 = rightCylinderCrossOverRatio$`240_CrossOverRatio`,
                                      rotation_270 = rightCylinderCrossOverRatio$`270_CrossOverRatio`,
                                      LeftOrRight = "Right")

#combine the left and right into a single dataframe
rawCylinder <- rbind(rightRatioCylinder, leftRatioCylinder)
rawAvatar <- rbind(rightRatioAvatar, leftRatioAvatar)

#exports the files
write.csv(rawCylinder,"CylinderCrossOverRatio.csv", row.names = FALSE)
write.csv(rawAvatar,"AvatarCrossOverRatio.csv", row.names = FALSE)




