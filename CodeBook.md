# Getting and Cleaning Data Class Codebook

This code book describes the data processing performed for the "Getting and Cleaning Data" class project. The instructions for the project and a link to the original data set can be found in the [README](./README.md) file

## Input Data
About the input data:
* All data for the project is contained in the folder called "UCI HAR Dataset". Instructions to find this file in the web can be found in the [README](./README.md) file.
* The data processing script, run_analysis.R, should be executed in the same folder that contains this data folder.
* The data is inluded in this repo for easy execution.
* The readme file in the data folder describes all the data. I will not recreate that information here. I will only discuss the processing the run_analysis.R script performs on that data.
The input data looks like this:
* _features.txt_ contains a list of all 561 data measurements that were performed during the testing. These are called features.
* _test_ and _train_ are folders that contain the data collected by the testing. 9 subjects were tested and their data is in the test folder where 21 subjects were part of training and their data is in the train folder. The analysis combines all data.
* _activity_labels.txt_ contains a list of the 6 activities that were performed during the data measurement.
* _features_info.txt_ contains information on the format of all the data files in this folder.

The _test_ and _train_ folders contain the same (similarly named) files and file structure. 3 files from each folder were used in this excercise. They are (in this case for the train folder):
* _X_train.txt_ contains all the data collected during the measurement sessions. Each line of data has 561 measurement readings for all the features for a particular subject performing a particular activity.
* _subject_train.txt_ contains a single column of numbers cooresponding to the particular subject performing an activity being measured.
* _y_train.txt_ contains a single column of numbers cooresponding to the activity being performed by a particular subject being measured.

When the 3 above files are read into columns of a data table, you get the subject, the activity and the measurements for each feature in many rows. This file format is identical for the test folder.

## Data Preparation Script
The data needs to be preparted for processing. The script _run_analysis.R_ performs this processing. It prepares the data by:
* Reading all the data files into structures.
* Ensures the feature labels are alphanumeric characters.
* Reads the data giving the columns human readable names.
* Factors the subject and activity columns to ensure analysis can be performed using those columns as keys.
* Removes unneeded feature columns - those feature columns that do not measure a mean or a standard deviation.
* The resulting data table is called _testData_.

## Data Analysis
Create a second data structure containing the mean of each feature column by 2 factors - the subject and the activity.
* Using the data prepared above (in _testData_), calculate the mean of each feature for for each subject/activity pair.
* Put this mean in a new data structure. This data table is called _testData.mean_.

