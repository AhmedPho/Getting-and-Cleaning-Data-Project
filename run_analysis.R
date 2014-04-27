# R that does the following:
# 1. Merges the training and the test sets to create one data set.

temp1 <- read.table("train/X_train.txt", header=FALSE)
temp2 <- read.table("test/X_test.txt", header=FALSE)
X <- rbind(temp1, temp2)

temp1 <- read.table("train/y_train.txt", header=FALSE)
temp2 <- read.table("test/y_test.txt", header=FALSE)
Y <- rbind(temp1, temp2)

temp1 <- read.table("train/subject_train.txt", header=FALSE)
temp2 <- read.table("test/subject_test.txt", header=FALSE)
subject <- rbind(temp1, temp2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indicesOfGoodFeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indicesOfGoodFeatures]
names(X) <- features[indicesOfGoodFeatures, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))  

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(subject) <- "subject"
cleaned <- cbind(subject, Y, X )
write.table(cleaned, "mergedCleanData.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
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