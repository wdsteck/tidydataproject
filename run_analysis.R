#
# run_analysis.R script
#
# for more information about this script, see the CodeBook.md in the same folder as this script.
#
# This script read raw data and processes it into a data structure that facilitates analysis. It combines
# the data from several text files into a single data structure with human readable labels and factors
# that can be used for analysis.
#
# The execution of the script assumes that the uncompressed data folder is locted in the same folder as the script.
#

#
# readTestData function reads either the test or train folders and combines all the data with human readable
# labels. The read data structure is returned to the calling function.
#
# the subject column is read, the activity column is read and all the feature measurements are read.
#

readTestData <- function(testOperation) {
        # Read the first column of the test data frame. This contains the subject
        # number of each exercise done.
        testdf <- read.table(paste(dataDir, testOperation, "/subject_", testOperation, ".txt", sep = ""),
                sep = " ", header=FALSE, col.names = c("testSubject"))
        
        # add to that data frame the specific exercise performed by that subject
        testdf <- cbind(testdf, read.table(paste(dataDir, testOperation, "/y_", testOperation, ".txt", sep = ""),
                sep = " ", header=FALSE, col.names = c("testExercise")))
        
        # Now add to that the data collected for each subject for each exercise done
        testdf <- cbind(testdf, read.table(paste(dataDir, testOperation, "/X_", testOperation, ".txt", sep = ""),
                sep = "", header=FALSE, col.names = features$feature))
        
        testdf
}

# Set up the directory with the Data in it
dataDir <- "UCI HAR Dataset/"

if (!file_test("-d", file.path(getwd(), dataDir))) {
        stop(paste("The current directory", getwd(), "does not contain the data directory", dataDir))
}

dataDir <- file.path(getwd(), dataDir)
print(paste("Data directory is:", dataDir))

# first read the features table
print("Read and Process Features file")

features <- read.table(paste(dataDir, "features.txt", sep = ""),
        sep = " ", header=FALSE, stringsAsFactors = FALSE,
        col.names = c("featureNumber", "feature"))

# Tidy the feature names. Remove parens, commas, dashes and convert
# the single digit numbers to double digit numbers by adding a leading zero.
# can happen in mid word and at the end of the word.
features$feature <- gsub("(\\D)(\\d)(\\D)", "\\10\\2\\3", features$feature)
features$feature <- gsub("(\\D)(\\d)$", "\\10\\2", features$feature)
features$feature <- gsub("[(),-]", "", features$feature)

# Read the activity labels so they can be combined into the testdf.
print("Read and Process activity labels file")

activityLabels <- read.table(paste(dataDir, "activity_labels.txt", sep = ""),
        sep = " ", header=FALSE, stringsAsFactors = FALSE,
        col.names = c("activityNumber", "activityLabel"))

# Read in the data. Two folders. One called "test" and one called "train"
print("Read and Process the data in the test folder. Apply column labels from features file.")
testData <- readTestData("test")

print("Combine test folder data with the data in the train folder")
testData <- rbind(testData, readTestData("train"))

# Replace the excercise number with a human readable name
print("Convert the activity numbers to human readable activities")

testData$testSubject <- factor(testData$testSubject)
testData$testExercise <- factor(testData$testExercise,
                          levels = activityLabels$activityNumber,
                          labels = activityLabels$activityLabel)

# FInally, extract only those columns we want. They are the mean
# and standard deviation columns along with the first 2 key columns.
print("Finally, trim the number of colunms to those with mean and std data.")
testData <- testData[, sort(union(c(1,2),union(grep("mean", colnames(testData)),
                                grep ("std", colnames(testData)))))]

print("*** Data Preparation complete. Data in structure called 'testData'.")

# ensure the plyr library is loaded.

if(require("plyr")){
        print("plyr is loaded")
} else {
        print("trying to install plyr")
        install.packages("plyr")
        if(require("plyr")){
                print("plyr installed and loaded")
        } else {
                stop("could not install plyr")
        }
}

print("Creating the table of means for each subject/activity pair.")

#
# for each feature, calculate the mean of that feature for each particular subject/activity (exercise) combination.
#

testData.mean <- ddply(testData, .(testSubject, testExercise), summarize, tBodyAccmeanX=mean(tBodyAccmeanX))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccmeanY = mean(tBodyAccmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccmeanZ = mean(tBodyAccmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccstdX = mean(tBodyAccstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccstdY = mean(tBodyAccstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccstdZ = mean(tBodyAccstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccmeanX = mean(tGravityAccmeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccmeanY = mean(tGravityAccmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccmeanZ = mean(tGravityAccmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccstdX = mean(tGravityAccstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccstdY = mean(tGravityAccstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccstdZ = mean(tGravityAccstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkmeanX = mean(tBodyAccJerkmeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkmeanY = mean(tBodyAccJerkmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkmeanZ = mean(tBodyAccJerkmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkstdX = mean(tBodyAccJerkstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkstdY = mean(tBodyAccJerkstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkstdZ = mean(tBodyAccJerkstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyromeanX = mean(tBodyGyromeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyromeanY = mean(tBodyGyromeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyromeanZ = mean(tBodyGyromeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyrostdX = mean(tBodyGyrostdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyrostdY = mean(tBodyGyrostdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyrostdZ = mean(tBodyGyrostdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkmeanX = mean(tBodyGyroJerkmeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkmeanY = mean(tBodyGyroJerkmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkmeanZ = mean(tBodyGyroJerkmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkstdX = mean(tBodyGyroJerkstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkstdY = mean(tBodyGyroJerkstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkstdZ = mean(tBodyGyroJerkstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccMagmean = mean(tBodyAccMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccMagstd = mean(tBodyAccMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccMagmean = mean(tGravityAccMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tGravityAccMagstd = mean(tGravityAccMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkMagmean = mean(tBodyAccJerkMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyAccJerkMagstd = mean(tBodyAccJerkMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroMagmean = mean(tBodyGyroMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroMagstd = mean(tBodyGyroMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkMagmean = mean(tBodyGyroJerkMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, tBodyGyroJerkMagstd = mean(tBodyGyroJerkMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanX = mean(fBodyAccmeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanY = mean(fBodyAccmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanZ = mean(fBodyAccmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccstdX = mean(fBodyAccstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccstdY = mean(fBodyAccstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccstdZ = mean(fBodyAccstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanFreqX = mean(fBodyAccmeanFreqX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanFreqY = mean(fBodyAccmeanFreqY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccmeanFreqZ = mean(fBodyAccmeanFreqZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanX = mean(fBodyAccJerkmeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanY = mean(fBodyAccJerkmeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanZ = mean(fBodyAccJerkmeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkstdX = mean(fBodyAccJerkstdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkstdY = mean(fBodyAccJerkstdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkstdZ = mean(fBodyAccJerkstdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanFreqX = mean(fBodyAccJerkmeanFreqX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanFreqY = mean(fBodyAccJerkmeanFreqY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccJerkmeanFreqZ = mean(fBodyAccJerkmeanFreqZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanX = mean(fBodyGyromeanX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanY = mean(fBodyGyromeanY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanZ = mean(fBodyGyromeanZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyrostdX = mean(fBodyGyrostdX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyrostdY = mean(fBodyGyrostdY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyrostdZ = mean(fBodyGyrostdZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanFreqX = mean(fBodyGyromeanFreqX)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanFreqY = mean(fBodyGyromeanFreqY)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyGyromeanFreqZ = mean(fBodyGyromeanFreqZ)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccMagmean = mean(fBodyAccMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccMagstd = mean(fBodyAccMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyAccMagmeanFreq = mean(fBodyAccMagmeanFreq)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyAccJerkMagmean = mean(fBodyBodyAccJerkMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyAccJerkMagstd = mean(fBodyBodyAccJerkMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMagmeanFreq)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroMagmean = mean(fBodyBodyGyroMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroMagstd = mean(fBodyBodyGyroMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroMagmeanFreq = mean(fBodyBodyGyroMagmeanFreq)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroJerkMagmean = mean(fBodyBodyGyroJerkMagmean)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroJerkMagstd = mean(fBodyBodyGyroJerkMagstd)), by = c('testSubject', 'testExercise'))
testData.mean <- merge(testData.mean, ddply(testData, .(testSubject, testExercise), summarize, fBodyBodyGyroJerkMagmeanFreq = mean(fBodyBodyGyroJerkMagmeanFreq)), by = c('testSubject', 'testExercise'))

print("*** Mean Data Preparation complete. Mean Data in structure called 'testData.mean'.")
