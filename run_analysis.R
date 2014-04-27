# R that does the following:
# 1. Merges the training and the test sets to create one data set.

temp1 <- read.table("train/X_train.txt", header=FALSE)
temp2 <- read.table("test/X_test.txt", header=FALSE)
X_train <- rbind(temp1, temp2)

temp1 <- read.table("train/subject_train.txt", header=FALSE)
temp2 <- read.table("test/subject_test.txt", header=FALSE)
subject_test <- rbind(temp1, temp2)

temp1 <- read.table("train/y_train.txt", header=FALSE)
temp2 <- read.table("test/y_test.txt", header=FALSE)
y_test <- rbind(temp1, temp2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indicesOfGoodFeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X_train <- X_train[, indicesOfGoodFeatures]
names(X_train) <- features[indicesOfGoodFeatures, 2]
names(X_train) <- gsub("\\(|\\)", "", names(X_train))
names(X_train) <- tolower(names(X_train))  

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y_test[,1] = activities[y_test[,1], 2]
names(y_test) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(subject_test) <- "subject"
cleaned <- cbind(subject_test, y_test, X_train )
write.table(cleaned, "mergedCleanData.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subject_test)[,1]
numSubjects = length(unique(subject_test)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
myResult = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    myResult[row, 1] = uniqueSubjects[s]
    myResult[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    myResult[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(myResult, "dataSetWithTheAverages.txt")