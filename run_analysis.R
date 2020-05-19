# GETTING AND CLEANING DATA COURSE PROJECT
# By Andrea Ballestero on 5/17/2020

# Loading data sets

library(readr)
X_test <- read.table("~/R/Coursera/ProgAssignmentMODULE3/test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("~/R/Coursera/ProgAssignmentMODULE3/test/y_test.txt", quote="\"", comment.char="")
subject_test <- read.table("~/R/Coursera/ProgAssignmentMODULE3/test/subject_test.txt", quote="\"", comment.char="")

X_train <- read.table("~/R/Coursera/ProgAssignmentMODULE3/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("~/R/Coursera/ProgAssignmentMODULE3/train/y_train.txt", quote="\"", comment.char="")
subject_train <- read.table("~/R/Coursera/ProgAssignmentMODULE3/train/subject_train.txt", quote="\"", comment.char="")

features <- read.table("~/R/Coursera/ProgAssignmentMODULE3/features.txt", quote="\"", comment.char="")
activity_labels <- read.table("~/R/Coursera/ProgAssignmentMODULE3/activity_labels.txt", quote="\"", comment.char="")

# Using descriptive activity names to name the activities in the data set

library(dplyr)

colnames(y_test) <- "activity"
act_test <- mutate(y_test, name = "Walking")
for(i in 1:2947){
  if(act_test[i,1] == 2){
    act_test[i,2] <- "Walking_upstairs"
  } else if(act_test[i,1] == 3){
    act_test[i,2] <- "Walking_downstairs"
  } else if(act_test[i,1] == 4){
    act_test[i,2] <- "Sitting"
  } else if(act_test[i,1] == 5){
    act_test[i,2] <- "Standing"
  } else if(act_test[i,1] == 6){
    act_test[i,2] <- "Laying"
  }
}
act_test2 <- data.frame(act_test$name)
colnames(act_test2) <- "activity"

colnames(y_train) <- "activity"
act_train <- mutate(y_train, name = "Walking")
for(i in 1:7352){
  if(act_train[i,1] == 2){
    act_train[i,2] <- "Walking_upstairs"
  } else if(act_train[i,1] == 3){
    act_train[i,2] <- "Walking_downstairs"
  } else if(act_train[i,1] == 4){
    act_train[i,2] <- "Sitting"
  } else if(act_train[i,1] == 5){
    act_train[i,2] <- "Standing"
  } else if(act_train[i,1] == 6){
    act_train[i,2] <- "Laying"
  }
}
act_train2 <- data.frame(act_train$name)
colnames(act_train2) <- "activity"

# Labeling the data set with descriptive variable names

features2 <- t(features[, 2])
X_test2<- X_test
colnames(X_test2) <- features2
X_train2<- X_train
colnames(X_train2) <- features2

type <- data.frame(rep("test", times = 2947))
colnames(type) <- "type"
colnames(subject_test) <- "subject"
X_test2 <- cbind(type, subject_test, act_test2, X_test2)

type <- data.frame(rep("train", times = 7352))
colnames(type) <- "type"
colnames(subject_train) <- "subject"
X_train2 <- cbind(type, subject_train, act_train2, X_train2)

# Merging X_test and X_train into a new data set called "data"

data <- rbind(X_train2, X_test2)

# Extracting only the measurements on the mean and standard deviation for each measurement.

cmean <- grep("mean", features$V2) # contain mean
cstd <- grep("std", features$V2) # contain standard deviation
select_feat <- c(features[cmean, 2], features[cstd, 2])

data_mean <- data[, cmean + 3]
data_std <- data[, cstd + 3]

type <- data$type
subject <- data$subject
activity <- data$activity
mean_std <- cbind(type, subject, activity, data_mean, data_std) # Final data set before tidy data

# Creating a second, independent tidy data set with the average of each variable for each activity and each subject

td <- group_by(mean_std, subject, activity)
tidy_data <- summarise_at(td, vars(select_feat), list(average = mean)) 

