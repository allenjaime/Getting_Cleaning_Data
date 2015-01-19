## Course Project P1 Getting and Cleaning Data ##
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Loading Packages ##
library(downloader)
library(plyr)
library(reshape2)

## Download files and unzip datasets to "./data" ##
download(fileUrl, dest="dataset.zip", mode = "wb") 
unzip ("dataset.zip",exdir = "./data")

## Loading Labels Datasets ##
## Variables ##
feat <- read.table("./data/UCI HAR Dataset/features.txt", sep=" ", header=FALSE, col.names= c("fid", "var"))
## Act ID - Activity Names ##
acti <- read.table("./data/UCI HAR Dataset/activity_labels.txt", sep=" ", header=FALSE, col.names= c("aid", "activity"))

## Vectors for FWF reading ##
w561 <- rep(16, times = 561)

## Loading Train Set 70% ##
## Subject ID ##
trainsub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", sep=" ", header=FALSE, col.names= "subject")
## Activity ID ##
trainlab <- read.table("./data/UCI HAR Dataset/train/y_train.txt", sep=" ", header=FALSE, col.names= "act")
## Variables DATASET ##
trainset <- read.fwf("./data/UCI HAR Dataset/train/X_train.txt", widths = w561, header = FALSE)

## Loading Test Set 30% ##
## Subject ID ##
testsub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", sep=" ", header=FALSE, col.names= "subject")
## Activity ID ##
testlab <- read.table("./data/UCI HAR Dataset/test/y_test.txt", sep=" ", header=FALSE, col.names= "act")
## Variables DATASET ##
testset <- read.fwf("./data/UCI HAR Dataset/test/X_test.txt", widths = w561, header = FALSE)

## Binding Train and Test Data Set ##
TOTAL <- rbind(trainset, testset)
TLAB <- rbind(trainlab, testlab)
TSUB <- rbind(trainsub, testsub)
names(TOTAL) <- feat$var

## Merging to 1 Data Set: Subject and Activity ##
DATA <- cbind(TSUB, TLAB, TOTAL)

## Merge Descriptive Activity Names ##
TDACT <- merge(DATA, acti, by.x = "act", by.y= "aid", all.x = TRUE)
## Subject As Factor ##
TDACT$subject <- as.factor(TDACT$subject)
## Complete Data ##
TDACT <- TDACT[c(2,564,3:563)]

## Subsetting Mean() and Std() Data Set ##
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

## New Tidy Average Data Set, by subject, by activity - Average variable (Measurement) ##
## Melting and Casting the TIDY Dataframe ##
MELTED <- melt(TIDY, id.vars=list("subject", "activity"))
CASTED <- dcast(MELTED, subject + activity ~ variable, mean)
## Assigning the Correct Names for all 66 variables (Removing Body repeated) ##
names(CASTED)[63:68] <- c("fBodyAccJerkMag-mean()", "fBodyAccJerkMag-std()", "fBodyGyroMag-mean()", "fBodyGyroMag-std()", "fBodyGyroJerkMag-mean()", "fBodyGyroJerkMag-std()")
## Complete DATA SET created ##

## Creating the data set as a txt file using write.table() -- "AVERAGES.txt"
write.table(CASTED, "./data/UCI HAR Dataset/AVERAGES.txt", sep = " ", row.names = FALSE)

## "AVERAGES.txt" Average DATA SET P.5 COURSE PROJECT ##