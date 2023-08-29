## Downloading and unzipping dataset

if(!file.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="./data/smartphone.zip",method="curl") ##Buraya fazladan method kısmı ekledim hata olursa silebilirim

## Unzip smartphone.zip to data directory
unzip(zipfile = "./data/smartphone.zip",exdir="./data")

## STEP1:Merges the training and the test sets to create one data set.

## Reading files

## Reading Test files
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## Reading Training Files 
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

## Reading Feature vectors
features <- read.table("./data/UCI HAR Dataset/features.txt")

## Reading Activity Lables
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

## Assigning Column Names 
colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_id" ## İsimleri farklı koydum
colnames(subject_train) <- "subject_id"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activity_id"
colnames(subject_test) <- "subject_id"

colnames(activityLabels) <- c("activity_id","activity_name")

## Merging the data
total_train <- cbind(x_train,y_train,subject_train)
total_test <- cbind(x_test,y_test,subject_test)
overall <- rbind(total_train,total_test)

## STEP2:Extracts only the measurements on the mean and standard deviation for each measurement.

## Reading column names
colNames <- colnames(overall)

## Create a vector for id,mean and sd

mean_sd <- (grepl("activity_id",colNames)|
                grepl("subject_id",colNames)|
                grepl("mean.",colNames)|
                grepl("std.",colNames)
)

## Getting subset of overall
overall2 <- subset(overall,mean_sd == TRUE) ## Burası da farklı 

## STEP3: Uses descriptive activity names to name the activities in the data set

activitynames <- merge(overall2,activityLabels,by="activity_id",all.x=TRUE)

## STEP4:Appropriately labels the data set with descriptive variable names.

names(overall2)[2] = "activity"
names(overall2)<-gsub("Acc", "Accelerometer", names(overall2))
names(overall2)<-gsub("Gyro", "Gyroscope", names(overall2))
names(overall2)<-gsub("BodyBody", "Body", names(overall2))
names(overall2)<-gsub("Mag", "Magnitude", names(overall2))
names(overall2)<-gsub("^t", "Time", names(overall2))
names(overall2)<-gsub("^f", "Frequency", names(overall2))
names(overall2)<-gsub("tBody", "TimeBody", names(overall2))
names(overall2)<-gsub("-mean()", "Mean", names(overall2), ignore.case = TRUE)
names(overall2)<-gsub("-std()", "STD", names(overall2), ignore.case = TRUE)
names(overall2)<-gsub("-freq()", "Frequency", names(overall2), ignore.case = TRUE)
names(overall2)<-gsub("angle", "Angle", names(overall2))
names(overall2)<-gsub("gravity", "Gravity", names(overall2))

## STEP5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Creating a tidy set
tidyset <- aggregate(.~subject_id + activity_id,overall2,mean)
tidyset <- tidyset[order(tidyset$subject_id,tidyset$activity_id),]

## Writing it to a txt file 
write.table(tidyset,"tidyset.txt",row.names = FALSE)

