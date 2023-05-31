#load the appropriate libraries
library(dplyr)
library(data.table)

# Zip file name
destFilename <- "Dataset.zip"

# Checking if the zip file is already in the local file system. If not, download it from the URL.
if (!file.exists(destFilename)){
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL, destFilename, method="curl")
}  

# Check and unzip the downloaded file into the following directory.
if (!file.exists("./UCI HAR Dataset")) { 
  unzip(destFilename)
}

# Step 0: Extract the relevant data from the files and assign them to the approriate data frames.
featuresNames <- read.table("./UCI HAR Dataset/features.txt", header = F)
activitiesLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

featuresTest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = F)
featuresTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = F)
names(featuresTest) <- featuresNames$V2
names(featuresTrain) <- featuresNames$V2

activitiesTest <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "code")
activitiesTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "code")


# Step 1: Merges the training and the test sets to create one data set.
features <- rbind(featuresTrain, featuresTest)
activities <- rbind(activitiesTrain, activitiesTest)
subjectData <- rbind(subjectTrain, subjectTest)
MergedData <- cbind(subjectData, activities, features)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
resultsData <- MergedData %>% select(subject, code, contains("mean"), contains("std"))

# Step 3: Uses descriptive activity names to name the activities in the data set.
resultsData$code <- activitiesLabels[resultsData$code, 2]

# Step 4: Appropriately labels the data set with descriptive variable names.
names(resultsData)[1] = "Subject"
names(resultsData)[2] = "Activity"
names(resultsData)<-gsub("Acc", "Accelerometer", names(resultsData))
names(resultsData)<-gsub("Gyro", "Gyroscope", names(resultsData))
names(resultsData)<-gsub("BodyBody", "Body", names(resultsData))
names(resultsData)<-gsub("Mag", "Magnitude", names(resultsData))
names(resultsData)<-gsub("^t", "Time", names(resultsData))
names(resultsData)<-gsub("^f", "Frequency", names(resultsData))
names(resultsData)<-gsub("tBody", "TimeBody", names(resultsData))
names(resultsData)<-gsub("-mean()", "Mean", names(resultsData), ignore.case = TRUE)
names(resultsData)<-gsub("-std()", "STD", names(resultsData), ignore.case = TRUE)
names(resultsData)<-gsub("-freq()", "Frequency", names(resultsData), ignore.case = TRUE)
names(resultsData)<-gsub("angle", "Angle", names(resultsData))
names(resultsData)<-gsub("gravity", "Gravity", names(resultsData))

# Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
finalResultsData <- resultsData %>%
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))
write.table(finalResultsData, "./FinalResultsData.txt", row.name=FALSE)
