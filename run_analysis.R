#TITLE: run_analysis.R
#
#PURPOSE:    
#    Merges the training and the test sets to create one data set. ***dfTT
#    Extracts only the measurements on the mean and standard deviation for each measurement. ***dfTT3 
#    Uses descriptive activity names to name the activities in the data set ***dfTT1
#    Appropriately labels the data set with descriptive variable names. ***could be better
#    Creates a second, independent tidy data set with the average of each variable(561 vars) for each activity(6 types) and each subject(train=7352,test=2947).  ***dfTT5
#
# This script requires the plyr package.

# Download & unzip the source file from here
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Check that required files exist
stopifnot(file.exists("features.txt"), file.exists("activity_labels.txt"), 
          file.exists("./test/Y_test.txt"), file.exists("./train/Y_train.txt"), 
          file.exists("./test/subject_test.txt"), file.exists("./train/subject_train.txt"),
          file.exists("./test/X_test.txt"), file.exists("./train/X_train.txt") )


# read features.txt to get column names for X*.txt files
dfFeatures <- read.table("features.txt")

# read activity_labels.txt to get lookup table for activity types for Y*.txt files
dfActivity <- read.table("activity_labels.txt",col.names=c("activitytype","activityname"))



##### for each set (train=7352,test=2947), read X <measurements>, Y <activityType>, subject IDs <subjectID>



# read activity types
dfYtest <- read.table("./test/Y_test.txt",row.names=NULL,col.names=c("activitytype"))
dfYtrain <- read.table("./train/Y_train.txt",row.names=NULL,col.names=c("activitytype"))

# read subject IDs
dfSubTest <- read.table("./test/subject_test.txt",row.names=NULL,col.names=c("subjectid"))
dfSubTrain <- read.table("./train/subject_train.txt",row.names=NULL,col.names=c("subjectid"))


# read measurements
dfXtest <- read.table("./test/X_test.txt",row.names=NULL,col.names=dfFeatures$V2)
dfXtrain <- read.table("./train/X_train.txt",row.names=NULL,col.names=dfFeatures$V2)


## create an index on each file based on the row number
dfYtest$id <- seq(1,nrow(dfYtest))
dfSubTest$id <- seq(1,nrow(dfSubTest))
dfXtest$id <- seq(1,nrow(dfXtest))

dfYtrain$id <- seq(1,nrow(dfYtrain))
dfSubTrain$id <- seq(1,nrow(dfSubTrain))
dfXtrain$id <- seq(1,nrow(dfXtrain))


## merge test & train files based on that index (cbind)
dfTest1 <- merge(dfYtest, dfSubTest, by.x="id", by.y="id", all=TRUE)
dfTest2 <- merge(dfTest1, dfXtest, by.x="id", by.y="id", all=TRUE)

dfTrain1 <- merge(dfYtrain, dfSubTrain, by.x="id", by.y="id", all=TRUE)
dfTrain2 <- merge(dfTrain1, dfXtrain, by.x="id", by.y="id", all=TRUE)


## merge (rbind) test & train datasets to create ~10K records
## Step 1: Merges the training and the test sets to create one data set.
dfTT <- rbind(dfTrain2, dfTest2)
dfTT1 <- merge(dfActivity, dfTT, by.x="activitytype", by.y="activitytype", all=TRUE)



##    Extracts only the measurements on the mean and standard deviation for each measurement. 
lTTnames <- tolower(names(dfTT1))
lTTnames1a <- grep("mean", lTTnames)
lTTnames1b <- grep("std", lTTnames)

dfTT2 <- dfTT1[,1:4]   #get the ID fields
dfTT2a <- dfTT1[,lTTnames1a]   # get the mean fields
dfTT2b <- dfTT1[,lTTnames1b]   # get the std fields

## abandoned effort to rename fields
#subDot <- function(x){gsub(".","",x)}
#lTTnames2a <- sapply(lTTnames1a,subDot)
#lTTnames2b <- sapply(lTTnames1b,subDot)
#colnames(dfTT2) <- tolower(names(dfTT1))


dfTT3 <- cbind(dfTT2, dfTT2a, dfTT2b)



    
## Creates a second, independent tidy data set with the average of each variable(561 vars) for each ###activity(6 types) and each subject(train=7352,test=2947). 
#dfTT4 <- with(dfTT3, by(activityname, subjectid, colMeans(x)))
#dfTT5 <- by(dfTT3[], activityname, subjectid, colMeans)

#colMeans()
##dfTT4 <- colMeans(dfTT1[,5:565])

## thanks hadley...http://stackoverflow.com/questions/3685492/r-speeding-up-group-by-operations/3685919#3685919
##install.packages("plyr")
library("plyr")
dfTT5 <- ddply(dfTT1, c("activityname", "subjectid"), function(x) colMeans(x[,5:565]))
write.table(dfTT5,"tidy_UCIHAR.txt",sep=",",quote=FALSE,row.names=FALSE)

#dfTT1$subjectid <- as.factor(dfTT1$subjectid)
#ddply(dfTT1, .(activityname),.(subjectid), summarise, mean.count = mean(count))
#dfTT5 <- ddply(dfTT1, .(activityname, subjectid), summarize OR transform, newcolumn = myfunction(column name I want the function to act upon))
#dfTT5 <- ddply(dfTT1, .(activityname,subjectid), colMeans(dfTT1[,5:565]))

