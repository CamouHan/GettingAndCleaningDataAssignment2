# Getting and Cleaning Data Assignment 2 Codebook

This is the CodeBook for the second course project of the Getting and Cleaning Data course

The source data are from the Human Activity Recognition Using Smartphones Data Set. Here is the more detailed description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here is the data which is used for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

There are total 7 steps in the ```run_analysis.R``` file (2 steps for preparing data and 5 steps for assignment requirements)

## 1:Download the Dataset

## 2:Assign values to variables 

* ```features <- features.txt```: The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
* ```activities <- activity_labels.txt``` :List of activities performed when the corresponding measurements were taken and its codes (labels)
* ```subject_test <- test/subject_test.txt``` :contains test data of 9/30 volunteer test subjects being observed
* ```x_test <- test/X_test.txt``` :contains recorded features test data
* ```y_test <- test/y_test.txt``` :contains test data of activities’code labels
* ```subject_train <- test/subject_train.txt``` : contains train data of 21/30 volunteer subjects being observed
* ```x_train <- test/X_train.txt``` :contains recorded features train data
* ```y_train <- test/y_train.txt``` :contains train data of activities’code labels

## 3: Merges the training and the test sets to create one data set

* ```total_train```  is created by merging x_train, y_train and subject_train using ```cbind()``` function
* ```total_test``` is created by merging x_test and y_test using ```cbind()``` function
* ```overall```  is created by merging ```total_test``` and ```total_train``` using rbind() function

## 4: Extracts only the measurements on the mean and standard deviation for each measurement

```overall2``` data is created by subsetting overall data, selecting only columns: subject, code and the measurements on the mean and standard deviation (std) for each measurement

## 5:Uses descriptive activity names to name the activities in the data set
    
Entire numbers in code column of the ```overall2``` data replaced with corresponding activity taken from second column of the activities variable

## 6:Appropriately labels the data set with descriptive variable names

* code column in ```overall2``` renamed into activities
* All Acc in column’s name replaced by Accelerometer
* All Gyro in column’s name replaced by Gyroscope
*  All BodyBody in column’s name replaced by Body
*  All Mag in column’s name replaced by Magnitude
* All start with character f in column’s name replaced by Frequency
* All start with character t in column’s name replaced by Time

## 7:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

* ```tidyset``` data is created by sumarizing overall2 data taking the means of each variable for each activity and each subject, after groupped by subject and activity.
* Export ```tidyset``` tidyset.txt file.
