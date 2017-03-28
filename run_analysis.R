#0. Download and unzip the original dataset
#0.1.Download the original dataset
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"UCI HAR Dataset.zip",method="curl")
#0.2 Unzip the original dataset
unzip("UCI HAR Dataset.zip")

#1. Merges the training and the test sets to create one data set.
#1.1 Merges the measurement variables
testData<-read.table("~/UCI HAR Dataset/test/X_test.txt")
trainData<-read.table("~/UCI HAR Dataset/train/X_train.txt")
features<-read.table("~/UCI HAR Dataset/features.txt")

names(features)<-c("FeatureID","FeatureName")
names(testData)<-features$FeatureName
names(trainData)<-features$FeatureName
measurementData<-rbind(testData,trainData)

#1.2 Merges the subject id
testSubjectID<-read.table("~/UCI HAR Dataset/test/subject_test.txt")
trainSubjectID<-read.table("~/UCI HAR Dataset/train/subject_train.txt")
subjectID<-rbind(testSubjectID,trainSubjectID)
names(subjectID)<-"SubjectID"

#1.3 Merges the activity id
testActivityID<-read.table("~/UCI HAR Dataset/test/y_test.txt")
trainActivityID<-read.table("~/UCI HAR Dataset/train/y_train.txt")
activityID<-rbind(testActivityID,trainActivityID)
names(activityID)<-"ActivityID"

#1.4 Combine above data to get one dataset
wholeDataset<-cbind(subjectID,activityID,measurementData)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
selectedMeasurement<-grep("mean\\(\\)|std\\(\\)",names(wholeDataset))
selectedDataset<-wholeDataset[,c(1,2,selectedMeasurement)]

#3.Uses descriptive activity names to name the activities in the data set

activityLabel<-read.table("~/UCI HAR Dataset/activity_labels.txt")
names(activityLabel)<-c("ActivityID","ActivityLabel")
library(dplyr)
selectedDataset<-left_join(selectedDataset,activityLabel,by="ActivityID")
selectedDataset<-selectedDataset[c(1,2,length(selectedDataset),3:(length(selectedDataset)-1))]

#4.Appropriately labels the data set with descriptive variable names
names(selectedDataset)<-gsub("^t","Time",names(selectedDataset)) 
names(selectedDataset)<-gsub("^f","Frequency",names(selectedDataset)) 
names(selectedDataset)<-gsub("Acc","Accelerometer",names(selectedDataset))   
names(selectedDataset)<-gsub("Gyro","Gyroscope",names(selectedDataset)) 
names(selectedDataset)<-gsub("Mag","Magnitude",names(selectedDataset))
names(selectedDataset)<-gsub("-mean\\(\\)(.)?","Mean",names(selectedDataset))
names(selectedDataset)<-gsub("-std\\(\\)(.)?","StandardDeviation",names(selectedDataset))
names(selectedDataset)<-gsub("BodyBody","Body",names(selectedDataset))
names(selectedDataset)<-gsub("([a-z])([A-Z])","\\1.\\2",names(selectedDataset)) 

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidyData<-selectedDataset %>%
    group_by(Subject.ID,Activity.ID,Activity.Label) %>%
    summarise_all(mean)
write.table(tidyData,row.name = FALSE,file = "tidy_data_set.txt")  