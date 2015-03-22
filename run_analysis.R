# Step 1. Merges the training and the test sets to create one data set.
# setwd("~/Documents/Personal/Coursera/Getting and Cleaning Data/Week 3/Run analysis")
path<-getwd()
  testpath<-eval(paste(path,"/test",sep=""))
  trainpath<-eval(paste(path,"/train",sep=""))
# loading necessary packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("reshape2")) {
  install.packages("reshape2")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}
require("data.table")
require("reshape2")
require("dplyr")

#load test and train data
  test_X<-read.table(eval(paste(testpath,"/X_test.txt",sep="")))
  train_X<-read.table(eval(paste(trainpath,"/X_train.txt",sep="")))
  subject_test_X<-read.table(eval(paste(testpath,"/subject_test.txt",sep="")))
  subject_train_X<-read.table(eval(paste(trainpath,"/subject_train.txt",sep="")))
#load activities data
  activity_test_X<-read.table(eval(paste(testpath,"/y_test.txt",sep="")))
  activity_train_X<-read.table(eval(paste(trainpath,"/y_train.txt",sep="")))

#merge subjects with the respective data
  test_dat<-cbind(subject_test_X, test_X)
  train_dat<-cbind(subject_train_X, train_X)

#merge activities with the respective data
  test_dat_act<-cbind(activity_test_X, test_dat)
  train_dat_act<-cbind(activity_train_X, train_dat)

#merge test data with train data
  dat_full<-rbind(train_dat_act,test_dat_act)

#checking that row count of the result data set is correct
#dim(test_dat_act)[1]+dim(train_dat_act)[1]==dim(dat_full)[1]
#[1] TRUE

#add features as labels to the merged data set
  features_df<-read.table("features.txt")
  features_factor<-as.character(features_df$V2)
  features<-as.character(features_factor)
  colnames(dat_full)<-c("activity","subject",features)
#check small 'corner' of data to see how it looks head(dat_full)[(1:3),(1:3)]

# Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#grepl produces logical vector with TRUE for matches
  extract_features <- grepl("mean\\(\\)|std\\(\\)", features)
  dat <-dat_full[,extract_features]

#check structure of 'dat'
#str(dat)
#'data.frame':  10299 obs. of  68 variables:
#  $ activity                        : int  5 5 5 5 5 5 5 5 5 5 ...
#  $ subject                         : int  1 1 1 1 1 1 1 1 1 1 ...

# Step 3. Uses descriptive activity names to name the activities in the data set
  activity_labels_df<-read.table("activity_labels.txt")
  activity_labels_factor<-activity_labels_df$V2
#class(activity_labels_factor)
#[1] "factor"
  activity_labels<-as.character(activity_labels_factor)

#factorize variale "activity" in the data frame "dat" using activity IDs and activity_labels
  dat$activity<- factor(dat$activity, labels = activity_labels)

#checking activity names
#head(dat$activity,30)
# [1] SITTING...

# Step 4. Appropriately labels the data set with descriptive variable names. 

#prefix t is replaced by time
#Acc is replaced by Accelerometer
#Gyro is replaced by Gyroscope
#prefix f is replaced by frequency
#Mag is replaced by Magnitude
#BodyBody is replaced by Body
#?gsub
names(dat)<-gsub("^t", "time", names(dat))
names(dat)<-gsub("Acc", "Accelerometer", names(dat))
names(dat)<-gsub("^Gyro", "Gyroscope", names(dat))
names(dat)<-gsub("^f", "frequency", names(dat))
names(dat)<-gsub("Mag", "Magnitude", names(dat))
names(dat)<-gsub("BodyBody", "Body", names(dat))


# Step 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.
# this is where dplyr package is used
dat2<-aggregate(. ~subject + activity, dat, mean)
dat2<-dat2[order(dat2$subject,dat2$activity),]
write.table(dat2, file = "tidydata.txt",row.name=FALSE)

