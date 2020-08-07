## Merges the training and the test sets to create one data set.
library(dplyr)
## Read files
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <-read.table("UCI HAR Dataset/activity_labels.txt")

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity")
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt")

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity")
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt")

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <-t(featureNames[2])
complete <- cbind(features, activity, subject)

## Extracts only the measurements on the mean and standard deviation for each measurement.
OnlyMeanAndSd <- grep("Mean|Std", names(complete), ignore.case = TRUE)
wantData <- complete[,c(OnlyMeanAndSd, 562, 563)]

## Uses descriptive activity names to name the activities in the data set
wantData$activity <- as.numeric(wantData$activity)
for (i in 1:6){
  wantData$activity[wantData$activity==i] <- as.character(activityLabels[i,2])
}
wantData$activity <- as.factor(wantData$activity)

## Appropriately labels the data set with descriptive variable names.
names(wantData)<-gsub("Acc", "Accelerometer", names(wantData))
names(wantData)<-gsub("Gyro", "Gyroscope", names(wantData))
names(wantData)<-gsub("BodyBody", "Body", names(wantData))
names(wantData)<-gsub("Mag", "Magnitude", names(wantData))
names(wantData)<-gsub("^t", "Time", names(wantData))
names(wantData)<-gsub("^f", "Frequency", names(wantData))
names(wantData)<-gsub("tBody", "TimeBody", names(wantData))
names(wantData)<-gsub("-mean()", "Mean", names(wantData), ignore.case = TRUE)
names(wantData)<-gsub("-std()", "STD", names(wantData), ignore.case = TRUE)
names(wantData)<-gsub("-freq()", "Frequency", names(wantData), ignore.case = TRUE)
names(wantData)<-gsub("angle", "Angle", names(wantData))
names(wantData)<-gsub("gravity", "Gravity", names(wantData))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
wantData$subject <- as.factor(wantData$subject)
library(data.table)
wantData <- data.table(wantData)
TidyData <- aggregate(.~subject + activity, data = wantData, FUN = mean)
TidyData <- TidyData[order(TidyData$subject, TidyData$activity),]
write.table(TidyData, file = "TidyData.txt", row.names = FALSE)
