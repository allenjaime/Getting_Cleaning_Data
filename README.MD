EXPERIMENT
Carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING). 
They were wearing a smartphone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The dataset has been randomly partitioned into two sets.
70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window).
The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. 
The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. 
From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 
See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 



Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.


DATA EXTRACTION AND CLEANING
The data was extracted according to these instructions:
1) You should create one R script called run_analysis.R that does the following.
	SEE RUN_ANALYSIS.R

2) Merges the training and the test sets to create one data set.
3) Extracts only the measurements on the mean and standard deviation for each measurement. 
4) Uses descriptive activity names to name the activities in the data set
5) Appropriately labels the data set with descriptive variable names. 
6) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## RUN_ANALYSIS.R EXPLAINED ## (1) RUN_ANALYSIS ##
## Course Project P1 Getting and Cleaning Data ## CREATES DIRECTORY TO DOWNLOAD THE DATA
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Loading Packages ## LOADS NECESSARY PACKAGES FOR THE CODE TO WORK ##
library(downloader)
library(plyr)
library(reshape2)

## Download files and unzip datasets to "./data" ## DOWNLOADS THE FILES AND UNZIPS THEM ##
download(fileUrl, dest="dataset.zip", mode = "wb") 
unzip ("dataset.zip",exdir = "./data")

## Loading Labels Datasets ## LOADS THE LABELS ##
## Variables ##
feat <- read.table("./data/UCI HAR Dataset/features.txt", sep=" ", header=FALSE, col.names= c("fid", "var"))
## Act ID - Activity Names ##
acti <- read.table("./data/UCI HAR Dataset/activity_labels.txt", sep=" ", header=FALSE, col.names= c("aid", "activity"))

## Vectors for FWF reading ## UTILITY VECTOR FOR LOADING THE DATASETS ##
w561 <- rep(16, times = 561)

## Loading Train Set 70% ## LOADS TRAIN DATASETS ##
## Subject ID ##
trainsub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep=" ", header=FALSE, col.names= "subject")
## Activity ID ##
trainlab <- read.table("./data/UCI HAR Dataset/train/y_train.txt", sep=" ", header=FALSE, col.names= "act")
## Variables DATASET ##
trainset <- read.fwf("./data/UCI HAR Dataset/train/X_train.txt", widths = w561, header = FALSE)

## Loading Test Set 30% ## LOADS TEST DATASETS ##
## Subject ID ##
testsub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep=" ", header=FALSE, col.names= "subject")
## Activity ID ##
testlab <- read.table("./data/UCI HAR Dataset/test/y_test.txt", sep=" ", header=FALSE, col.names= "act")
## Variables DATASET ##
testset <- read.fwf("./data/UCI HAR Dataset/test/X_test.txt", widths = w561, header = FALSE)

## Binding Train and Test Data Set ## (2) MERGES THE DATASETS TOGETHER ##
TOTAL <- rbind(trainset, testset)
TLAB <- rbind(trainlab, testlab)
TSUB <- rbind(trainsub, testsub)
names(TOTAL) <- feat$var

## Merging to 1 Data Set: Subject and Activity ## MERGES THE SUBJECT AND ACTIVTY TO THE DATA SET ##
DATA <- cbind(TSUB, TLAB, TOTAL)

## Merge Descriptive Activity Names ## (5) MERGES DESCRIPTIVE ACTIVITY NAMES ##
TDACT <- merge(DATA, acti, by.x = "act", by.y= "aid", all.x = TRUE)
## Subject As Factor ##
TDACT$subject <- as.factor(TDACT$subject)
## Complete Data ##
TDACT <- TDACT[c(2,564,3:563)]

## Subsetting Mean() and Std() Data Set ## (3) SUBSETTING WITH GREP THE MEAN() AND STD() ##
## Matching Mean() and Std () ##
mstdi <- grep("\\bmean()\\b|\\bstd()\\b", feat$var)
## To include Subject and Activity ##
ms <- c(1,2,mstdi+2)
## Clean Data Set ##
MSData <- TDACT[,ms]
TIDY <- MSData[order(MSData$subject, MSData$activity),]
row.names(TIDY) <- NULL

## TIDY DATA SET P.4 COURSE PROJECT ##
## TIDY -- Tidy Data Set ##

## --------------------------------------------------------------------------------- ##

## New Tidy Average Data Set, by subject, by activity - Average variable (Measurement) ## (6) CREATES AVERAGES DATA SET ##
## Melting and Casting the TIDY Dataframe ##
MELTED <- melt(TIDY, id.vars=list("subject", "activity"))
CASTED <- dcast(MELTED, subject + activity ~ variable, mean)
## Assigning the Correct Names for all 66 variables (Removing Body repeated) ##
names(CASTED)[63:68] <- c("fBodyAccJerkMag-mean()", "fBodyAccJerkMag-std()", "fBodyGyroMag-mean()", "fBodyGyroMag-std()", "fBodyGyroJerkMag-mean()", "fBodyGyroJerkMag-std()")
## Complete DATA SET created ##

## Creating the data set as a txt file using write.table() -- "AVERAGES.txt"
write.table(CASTED, "./data/UCI HAR Dataset/AVERAGES.txt", sep = " ", row.names = FALSE)

## "AVERAGES.txt" Average DATA SET P.5 COURSE PROJECT ##




