#Loading packages that are required for this R script to run.
library(data.table)
library(dplyr)

#Downloading files (assuming the working directory has been set correctly)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "Data.zip")

#Reading subject files
unzip("Data.zip")
dataSubjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
dataSubjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Reading activity files
dataActivityTrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
dataActivityTest <- read.table("./UCI HAR Dataset/test/Y_test.txt")

#Reading data files
dataTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
dataTest <- read.table("./UCI HAR Dataset/test/X_test.txt")

#Reading names and lables that will help understand what each variable means
Featurenames <- read.table("UCI HAR Dataset/features.txt")
dataActivityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

##STEP 1
#Merging training and test files
datasubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataactivity <- rbind(dataActivityTrain, dataActivityTest)
datafeatures <- rbind(dataTrain, dataTest)

#Naming the columns using Featurenames
colnames(datafeatures) <- t(Featurenames[2])
colnames(dataactivity) <- "Activity"
colnames(datasubject) <- "Subject"

#Make the dataset complete; combine features, activity and subject data
data <- cbind(datasubject, dataactivity, datafeatures)

##STEP 2
#Extracting the measurements on the mean and standard deviation (sd) for each measurement by specifying which columns to subset
meansdcolumns <- grep(".*mean.*|.*std.*", names(data), ignore.case = TRUE)
columnstouse <- c(1, 2, meansdcolumns)
meansddata <- data[,columnstouse]

##STEP 3
#Naming the activities using dataActivityLabels
meansddata$Activity <- as.character(meansddata$Activity)
View(dataActivityLabels) #there are 6 activities; the names of the activities are in the second column
for(i in 1:6){
        meansddata$Activity[meansddata$Activity == i] <- as.character(dataActivityLabels[i,2])
}

##STEP 4
#Labeling the variable names appropriately
names(meansddata)
#from the features_info.txt file accompanying the UCI HAR Dataset it can be seen what certain abbreviations mean
#-"t" stands for time
#-"f" stands for frequency
#-"Acc" stands for accelerometer
#-"Gyro" stands for gyroscope
#-"Mag" stands for magnitude
#Furthermore, the variable names could use some clean up, like removing the dashes or parentheses.

names(meansddata) <- gsub("^t", "Time", names(meansddata))
names(meansddata) <- gsub("tBody", "TimeBody", names(meansddata))
names(meansddata) <- gsub("^f", "Frequency", names(meansddata))
names(meansddata) <- gsub("Acc", "Accelerometer", names(meansddata))
names(meansddata) <- gsub("Gyro", "Gyroscope", names(meansddata))
names(meansddata) <- gsub("Mag", "Magnitude", names(meansddata))
names(meansddata) <- gsub("-mean", "Mean", names(meansddata), ignore.case = TRUE)
names(meansddata) <- gsub("-std", "SD", names(meansddata), ignore.case = TRUE)
names(meansddata) <- gsub("gravity", "Gravity", names(meansddata))
names(meansddata) <- gsub("angle", "Angle", names(meansddata))
names(meansddata) <- gsub("\\(\\)", "", names(meansddata))
names(meansddata) <- gsub("-X", "X", names(meansddata))
names(meansddata) <- gsub("-Y", "Y", names(meansddata))
names(meansddata) <- gsub("-Z", "Z", names(meansddata))

#The names are now clear and relatively easy to understand. There are no more abbreviations or unclear characters in the variable names.
names(meansddata)

##STEP 5
#Creating another dataset with the average of each variable for each activity and each subject
meansddata <- data.table(meansddata)
tidydata <- aggregate(. ~Subject + Activity, meansddata, mean)
tidydata <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(tidydata, file = "tidydata.txt", row.names = FALSE)

#Now we have a tidy dataset with the means of each variable for each activity and each subject.
