#You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each
# activity and each subject.

# unzip the file
#unzip('getdata-projectfiles-UCI HAR Dataset.zip')

# Load feature names
features <- read.table('UCI HAR Dataset/features.txt', 
  header=FALSE, col.names=c('id', 'featureName'), 
  colClasses = c('numeric', 'character'))

### 1. Merges the training and the test sets to create one data set.

loadData <- function(datatype) {
  data_file <- paste('UCI HAR Dataset/', datatype, '/X_', datatype, '.txt', 
                     sep='')
  label_file <- paste('UCI HAR Dataset/', datatype, '/Y_', datatype, '.txt', 
                      sep='')  
  subject_file <- paste('UCI HAR Dataset/', datatype, '/subject_', datatype, 
                        '.txt', sep='')
  # Load features
  result <- read.table(data_file, header=FALSE, col.names=features$featureName, 
                       colClasses = rep("numeric", nrow(features)))
  # Load labels
  result_label <- read.table(label_file, header=FALSE, col.names=c('label'), 
                             colClasses = c('numeric'))
  # Load subjects
  result_subject <- read.table(subject_file, header=FALSE, 
                               col.names=c('subject'), 
                               colClasses = c('numeric') )
  # merge labels and features for data set.
  result$label <- result_label$label
  result$subject <- result_subject$subject
  result  
}

# Load training data set
train <- loadData('train')
test <- loadData('test')

# merge train and test data
alldata <- rbind(train, test)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
requiredFeatures <- grepl("mean\\(\\)",features$featureName) | 
  grepl("std\\(\\)",features$featureName )
requiredCols <- features[requiredFeatures,]$id
requiredData <- alldata[, requiredCols]

# append label and subject since they are required in the final result
requiredData$label <- alldata$label
requiredData$subject <- alldata$subject


### 3. Uses descriptive activity names to name the activities in the data set
### 4. Appropriately labels the data set with descriptive activity names.
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt',
                              header=FALSE, col.names=c('id', 'activity_label'),
                              colClasses = c('numeric', 'character'))
# Join activity labels with the data
requiredData <- merge(requiredData, activity_labels, by.x = 'label', by.y = 'id')
# drop the numeric label column since we now have the activity_label column
requiredData <- requiredData[, !(names(requiredData) %in% c('label'))]

### 5. Creates a second, independent tidy data set with the average of each variable for each
# activity and each subject.
library(reshape2)

meltData <- melt(requiredData, id = c('subject', 'activity_label'))
result <- dcast(meltData, subject + activity_label ~ variable, mean)

# function to add a prefix for the input character
addPrefix <- function(x, prefix) {
  paste(prefix, x, sep="")
}

# set a meaningful name for the columns
headerNames <- gsub("\\.+", ".", names(result))
headerNames <- gsub("\\.$", "", headerNames)
headerNames <- sapply(headerNames, addPrefix, "mean.of.")
headerNames[1] <- 'subject'
headerNames[2] <- 'activity'

names(result) <- headerNames

# write the data to a tidy data set as txt file.
write.table(result, "tidy-data-set.txt", row.names=FALSE)

