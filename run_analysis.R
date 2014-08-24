## Set my working directory

currentDir <- "E:/R/R Programming"
setwd(currentDir)

newFolder <- "Getting and Cleaning Data/Project"

if(!file.exists(newFolder)){
    dir.create(newFolder)
}

newDir <- paste(currentDir, newFolder, sep = "/")
setwd(newDir)

# 1. Merge the training and the test sets to create one data set.

# Read data from files
features <- read.table('features.txt')
activityLabel <- read.table('activity_labels.txt')
subjectTrain <- read.table('subject_train.txt')
xTrain <- read.table('x_train.txt')
yTrain <- read.table('y_train.txt')
subjectTest <- read.table('subject_test.txt')
xTest <- read.table('x_test.txt')
yTest <- read.table('y_test.txt')

# Assigin column names to the data imported above
colnames(activityLabel) <- c('activityId','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"

# Create final training & test set
training <- cbind(yTrain, subjectTrain, xTrain)
test <- cbind(yTest, subjectTest, xTest)

# Merge training and test data
mergedData <- rbind(training, test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create extracted data filetered by keywords "mean" or "std" 
extract <- mergedData[grep("*mean* | *std*", colnames(mergedData)),]


# 3. Use descriptive activity names to name the activities in the data set

# Merge activityType with extract data
newData <- merge(activityType, extract, by.x ='activityId', by.y = "activityId")
colNames <- colnames(newData)

# 4. Appropriately label the data set with descriptive activity names. 

# Update variable names
for (i in 1:length(colNames)) 
{
    colNames[i] <- gsub("\\()","",colNames[i])
    colNames[i] <- gsub("-std","-StdDeviation",colNames[i])
    colNames[i] <- gsub("-mean","-Mean",colNames[i])
    colNames[i] <- gsub("^(t)","Time-",colNames[i])
    colNames[i] <- gsub("^(f)","Freq-",colNames[i])
    colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] <- gsub("[Gg]yro","Gyro-",colNames[i])
    colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Update with new descriptive names to newData
colnames(newData) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

newData2 <- newData[, names(newData) != "activityType"]

# Calculate mean values for each activity and subject IDs
tidy <- aggregate(newData2[, names(newData2) != c('activityId', 'subjectId')], by = list(activityId = newData2$activityId, subjectId = newData2$subjectId), mean)

# Add activityType to tidy data again
finalData <- merge(activityType, tidy, by.x ='activityId', by.y ='activityId')

# Create final tidy data in csv format
write.csv(finalData, "finalData.csv")