##setnew working directory - need adjustment for our own directory

setwd("/Users/martinschultze/Desktop/R-stuff/Getting-and-Cleaning-Data-Course-Project")
getwd()

##create data folder if non-existent

if (!file.exists("data")) {
        dir.create("data")
}


url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download zip file
download.file(url,destfile = "./data/wearables.zip", method = "curl")
#unzip file
unzip("./data/wearables.zip",list = TRUE)


#read features file
library(dplyr)
library(data.table)
features<-read.table("./data/UCI HAR Dataset/features.txt",header = FALSE)
#reading the files in the folder with read.table
activitys_names<-read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

##train data
features_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt",header = FALSE)
activitys_train<-read.table("./data/UCI HAR Dataset/train/Y_train.txt",header=FALSE)
subjects_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt",header=FALSE)
bodyAccX_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./data/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt",header=FALSE)

###test data
features_test<-read.table("./data/UCI HAR Dataset/test/X_test.txt",header = FALSE)
activitys_test<-read.table("./data/UCI HAR Dataset/test/Y_test.txt",header=FALSE)
subjects_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt",header=FALSE)
bodyAccX_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./data/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt",header=FALSE)

#rename colomuns in train set
names(features_train)<-features$V2
#rename colomuns in test set
names(features_test)<-features$V2

#rename colomun in activitys set in train
names(activitys_train)<-"activitys"
#rename colomun in activitys set in test
names(activitys_test)<-"activitys"

#rename colomun in subject set in train
names(subjects_train)<-"subject"
#rename colomun in subject set in test
names(subjects_test)<-"subject"

# add subject and acitivitys sets to train set
sub_act_feat_train=cbind(subjects_train,activitys_train,features_train)
# add subject and acitivitys sets to test set
sub_act_feat_test=cbind(subjects_test,activitys_test,features_test)

#question 1 : subActFeatures_both is the merges the training and the test sets
subActFeatures_both<-rbind(sub_act_feat_train,sub_act_feat_test)

#question 2 subActMeanStd is extraction from subActFeatures_both on the mean and standard deviation for each measurement
##valid columns names with no duplicates
subActFeatures_both_valid <- subActFeatures_both[ !duplicated(names(subActFeatures_both)) ]

subActMeanStd_both<-subActFeatures_both_valid%>%select(matches('mean|std'))

#question 3 : subActFeatures_both_valid with descriptive activies names
subActFeatures_both_descAct<-subActFeatures_both_valid%>% 
  arrange(activitys) %>% 
  mutate(activitys = as.character(factor(activitys, levels=1:6, 
                                  labels= activitys_names$V2)))


#question 4 :data set with descriptive variable names.
names(subActFeatures_both_descAct)<-gsub("tBodyAcc","Body acceleration signal in time domain (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyAccMag","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyAccJerk","Body acceleration jerk signal in time domain (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyAccJerkMag","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tGravityAcc","Gravity acceleration signal in time domain (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tGravityAccMag","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyGyro","Body acceleration signal in time domain (from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyGyroMag","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyGyroJerk","Body acceleration jerk signal in time domain (from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("tBodyGyroJerkMag","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyAcc","Body acceleration signal in frequence domain (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyAccMag","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyAccJerk","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyGyro","Body acceleration signal in frequence domain (from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyAccJerkMag","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("fBodyGyroMag","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("mean()", "MEAN", names(subActFeatures_both_descAct),)
names(subActFeatures_both_descAct)<-gsub("std()", "SD", names(subActFeatures_both_descAct),)

names(subActFeatures_both_descAct)

#question 5 :tidy data set with the average of each variable for each activity and each subject
tidydata<-subActFeatures_both_descAct%>%group_by(subject,activitys)%>%summarise_all(mean)
write.table(tidydata, "TidyData.txt", row.name=FALSE)


