# Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project

The run_analysis.R script cleans up the data and prepare the data for use, the steps are illustrated as below.
1.	Read files into R with read.table(), merge training datasets, test datasets and then combine all data sets together(subject, activity and features)
2.	Extracts only the measurements on the mean and standard deviation for each measurement using grep()
3.	Uses descriptive activity names to name the activities in the data set.
4.	Appropriately labels the data set with descriptive variable names
All Acc in column’s name renamed as Accelerometer
All Gyro in column’s name renamed as Gyroscope
All BodyBody in column’s name renamed as Body
All Mag in column’s name renamed as Magnitude
All start with character t in column’s name renamed as Time
All start with character f in column’s name renamed as Frequency
All tBody in column’s name renamed as TimeBody
All -mean() in column’s name renamed as Mean
All -std() in column’s name renamed as STD
All -freq() in column’s name renamed as Frequency
All angle in column’s name renamed as Angle
All gravity in column’s name renamed as Gravity
5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
