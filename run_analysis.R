# STEP 1 – Load libraries and datasets

## Load libraries
library(plyr)
library(dplyr)
library(reshape2)

## Download dataset
filename <- "dataset.zip"
foldername <- "UCI HAR Dataset"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename))
{
  download.file(fileURL, filename)
} 

if (!file.exists(foldername)) 
{ 
  unzip(filename) 
}

## Reading features and activity labels data sets
activityLabels <- read.table(paste(foldername, "/activity_labels.txt", sep=""))
features <- read.table(paste(foldername, "/features.txt", sep=""))

## Convert second column to character type
activityLabels[,2] <- as.character(activityLabels[,2])
features[,2] <- as.character(features[,2])

## Load the datasets
trainData <- read.table(paste(foldername, "/train/X_train.txt", sep=""))
trainActivities <- read.table(paste(foldername, "/train/Y_train.txt", sep=""))
trainSubjects <- read.table(paste(foldername, "/train/subject_train.txt", sep=""))

## Reading test data sets
testData <- read.table(paste(foldername, "/test/X_test.txt", sep=""))
testActivities <- read.table(paste(foldername, "/test/Y_test.txt", sep=""))
testSubjects <- read.table(paste(foldername, "/test/subject_test.txt", sep=""))


# STEP 2 - Merging
## Merge datasets 
trainData <- cbind(trainSubjects, trainActivities, trainData)
testData <- cbind(testSubjects, testActivities, testData)
allData <- rbind(trainData, testData)

## Setup column names
colnames(allData) <- c("subject", "activity", features[, 2])


# STEP 3 – Extracting the measurements of the mean and standard deviation for each measurement
## Extract data for mean and standard deviation
allData <- allData[ , grep(".*mean.*|.*std.*|activity|subject", colnames(allData))]
allData$activity <- factor(allData$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])
allData$subject <- as.factor(allData$subject)


# STEP 4 – Labeling
## Extract column names
tempName <- colnames(allData)
tempName = gsub('-mean', 'Mean', tempName)
tempName = gsub('-std', 'StandardDeviation', tempName)
## Remove brackets
tempName <- gsub('[-()]', '', tempName)

## Expand abbreviations
tempName <- gsub("^f", "frequencyDomain", tempName)
tempName <- gsub("^t", "timeDomain", tempName)
tempName <- gsub("Acc", "Accelerometer", tempName)
tempName <- gsub("Gyro", "Gyroscope", tempName)
tempName <- gsub("Mag", "Magnitude", tempName)
tempName <- gsub("Freq", "Frequency", tempName)
colnames(allData) <- tempName
rm(tempName)

# STEP 5 – Tidy data set
## Group by subject and activity and summarise using mean
Average <- allData %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

## Export as txt
write.table(Average, "TidyDataSet.txt", row.names = FALSE, quote = FALSE)