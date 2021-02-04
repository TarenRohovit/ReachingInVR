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
#the raw data sometimes names this row "Can't reac" we need to remove this too
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
#crossover analaysis for both experiments. The final results are stored in leftAvatarCrossOverRatio,rightAvatarCrossOverRatio,
#leftCylinderCrossOverRatio, and rightCylinderCrossOverRatio
#
#
#convert from meters to cm
avatar$Ball_Distance <- avatar$Ball_Distance *100
cylinder$Ball_Distance <- cylinder$Ball_Distance *100

#percents are the different chair rotations that need to be evaluted
percents <- c(0,90,120,150,180,210,240,270)
#create the empty dataframe that we'll be adding to

#leftCrossOverRatioRow is the dataframe in which indivual rows are added to for left data points
leftCrossOverRatioRow <- data.frame()
#leftCrossOverRatioRow is the dataframe in which indivual rows are added to for right data points
rightCrossOverRatioRow <- data.frame()
#leftAvatarCrossOverRatio is one of the  final products of this loop. It is constructed by adding leftCrossOverRatioRows together
leftAvatarCrossOverRatio <- data.frame()
#rightAvatarCrossOverRatio is one of the  final products of this loop. It is constructed by adding rightCrossOverRatioRows together
rightAvatarCrossOverRatio <- data.frame()
#leftCylinderCrossOverRatio is one of the  final products of this loop. It is constructed by adding leftCrossOverRatioRows together
leftCylinderCrossOverRatio <- data.frame()
#rightCylinderCrossOverRatio is one of the  final products of this loop. It is constructed by adding rightCrossOverRatioRows together
rightCylinderCrossOverRatio <- data.frame()

#this code computes cross over ratios for both the avatar experiemnt and the cylinder experiment. This loop allows this to happen
experiments <- c("Avatar","Cylinder")
for (exp in experiments){
  
  #import data containing arm length
  if (exp == "Avatar"){ 
    qualtricsdata <- read.csv("QualtricsAvatar.csv")
  }
  if (exp == "Cylinder"){
    qualtricsdata <- read.csv("QualtricsCylinder.csv")
  }
  
  #convert data types
  qualtricsdata$Please.have.the.participant.enter.your.arm.length. <- as.double(as.character(qualtricsdata$Please.have.the.participant.enter.your.arm.length.))
  qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. <- as.double(as.character(qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.))
  
  
  #calculate different ratios for each percent 
  for (percent in percents){
    
    #filter the data to only look at the relevent percent
    if (exp == "Avatar"){ copy <- avatar}
    if (exp == "Cylinder"){copy <- cylinder}
    copy$Ball_Rotation <- as.double(as.character(copy$Ball_Rotation))
    copy <- filter(copy, Chair_Rotation == percent)
    
    #set initial values for min and max reach distances. NOTE: mins are currently not being used
    Leftmin <- 100
    Leftmax <- 0
    Rightmin <- 100
    Rightmax <- 0
    
    previousSID <- copy[1,1]
    #loop through the data and keep track of maxs and mins for both left and right
    for (val in 1:nrow(copy)){
      if (copy[val, 1] == previousSID){
        #checks for left values
        if (copy[val, 7] < 0){
          if (copy[val, 6] < Leftmin){ Leftmin <- copy[val, 6]}
          if (copy[val, 6] > Leftmax){ Leftmax <- copy[val, 6]}
        }
        #checks for right values
        if (copy[val, 7] > 0){
          if (copy[val, 6] < Rightmin){ Rightmin <- copy[val, 6]}
          if (copy[val, 6] > Rightmax){ Rightmax <- copy[val, 6]}
        }
        
      }
      #compute the actual crossover ratio and add the ratios to new dataframes that were declared earlier
      else{
        if (any(previousSID %in% qualtricsdata$Please.have.the.experimenter.enter.your.participant.number.)){
          #picks out the correct arm length to be used in each ratio calculation
          armlengthindex <- subset(qualtricsdata, qualtricsdata$Please.have.the.experimenter.enter.your.participant.number. == previousSID)
          #computes left crossover ratio
          leftcrossoverratio <- data.frame(SID = previousSID, 
                                           CrossOverRatio = ((Leftmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
          #computes right crossover ratio
          rightcrossoverratio <- data.frame(SID = previousSID, 
                                            CrossOverRatio = ((Rightmax)) / armlengthindex$Please.have.the.participant.enter.your.arm.length.)
          #rename rows to labels with the coresponding percent
          colnames(leftcrossoverratio) <- paste(percent, colnames(leftcrossoverratio), sep = "_")
          colnames(rightcrossoverratio) <- paste(percent, colnames(rightcrossoverratio), sep = "_")
          #add ratios to a new dataframe in correspondance to previousSID
          leftCrossOverRatioRow <- rbind(leftCrossOverRatioRow, leftcrossoverratio)
          rightCrossOverRatioRow <- rbind(rightCrossOverRatioRow, rightcrossoverratio)
        }
        #reset left min and max values
        if (copy[val, 7] < 0){
          Leftmin <- copy[val,6]
          Leftmax <- copy[val,6]
        }
        #reset right min and max values
        if (copy[val, 7] > 0){
          Rightmin <- copy[val,6]
          Rightmax <- copy[val,6]
        }
        #reset SID for next iteration
        previousSID <- copy[val,1]
      }
    }
    # Constructs the final output dataframe for avatar data
    if (exp == "Avatar"){ 
      #if the ending dataframe is empty, create one
      if (nrow(leftAvatarCrossOverRatio) == 0){
        leftAvatarCrossOverRatio <- leftCrossOverRatioRow
      }
      if (nrow(rightAvatarCrossOverRatio) == 0){
        rightAvatarCrossOverRatio <-rightCrossOverRatioRow
      }
      else{
      #otherwise add the new data to the ending dataframe
      leftAvatarCrossOverRatio <- cbind(leftAvatarCrossOverRatio, leftCrossOverRatioRow)
      rightAvatarCrossOverRatio <- cbind(rightAvatarCrossOverRatio, rightCrossOverRatioRow)
      }
      leftCrossOverRatioRow <- data.frame()
      rightCrossOverRatioRow <- data.frame()
    }
    #constructs the final output dataframe for cylinder data
    if (exp == "Cylinder"){ 
      #if the ending dataframe is empty, create one
      if (nrow(leftCylinderCrossOverRatio) == 0){
        leftCylinderCrossOverRatio <- leftCrossOverRatioRow
      }
      if (nrow(rightCylinderCrossOverRatio) == 0){
        rightCylinderCrossOverRatio <-rightCrossOverRatioRow
      }
      #otherwise add the new data to the ending dataframe
      else{
      rightCylinderCrossOverRatio <- cbind(rightCylinderCrossOverRatio, rightCrossOverRatioRow)
      leftCylinderCrossOverRatio <- cbind(leftCylinderCrossOverRatio, leftCrossOverRatioRow)
      }
      #reset the looping dataframe
      leftCrossOverRatioRow <- data.frame()
      rightCrossOverRatioRow <- data.frame()
    }
  }
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




