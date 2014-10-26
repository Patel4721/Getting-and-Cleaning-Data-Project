# Run Analysis
# Author: Shailesh Patel
#
# License:
# ========
# Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
# 
# [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
# Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. 
# International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
# 
# This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed 
# to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
# 
# Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

run_analysis <- function() {
  
  # Install plyr package if it is not already  
   
  if(!is.element("plyr", installed.packages()[,1])) {
    print("installing packages...")
    install.packages("plyr")
  }
  
  # Load the plyr library
  library(plyr)  

  # Load the feature variables.
  # This contains 561 variables needed for the data set
  featureVariablesFile <- "./UCI HAR Dataset/features.txt"
  featureVariablesNames <- read.csv(featureVariablesFile, sep = " ", header = FALSE)
  
  # Clean up the variable names.  We want to:
  # - remove "-"
  # - remove "()"
  # - remove ","
  # 
  # We also want to make everything lower case
  
 ## featureVariablesNames$V2 <- gsub("-", "", featureVariablesNames$V2,)
  ##featureVariablesNames$V2 <- gsub("\\(", "", featureVariablesNames$V2,)
  ##featureVariablesNames$V2 <- gsub("\\)", "", featureVariablesNames$V2,)
  ##featureVariablesNames$V2 <- gsub("\\,", "", featureVariablesNames$V2,)
  ##featureVariablesNames$V2 <- tolower(featureVariablesNames$V2)
  
  
  # Load the test data set and assign column names.  This step requires the loading
  # of two files:
  
  #   - X_test.txt - contains the 561 variables for each observation. 
  #   - Y_test.txt - contains the test labels that link to activity names.
  #   - subject_train.txt - contains the individual's ID number.
  
  testXFile <- "./UCI HAR Dataset/test/X_test.txt"
  testX <- read.csv(testXFile, sep = "", header = FALSE, stringsAsFactors=FALSE, col.names=featureVariablesNames$V2)
  
  testYFile <- "./UCI HAR Dataset/test/Y_test.txt"
  testY <- read.csv(testYFile, sep = "", header = FALSE, stringsAsFactors=FALSE)
  names(testY) <- "activity"
  
  testXSubjectFile <- "./UCI HAR Dataset/test/subject_test.txt"
  testXSubject <- read.csv(testXSubjectFile, sep = "", header = FALSE, stringsAsFactors=FALSE)
  names(testXSubject) <- "subjectid"
  
  # Make a single data set of the test data
  testdata <- cbind(testXSubject, testY, testX)
    
  # Load the training data set and assign column names. This step requires the loading
  # of two files:  
  #     
  #   - X_train.txt - contains the 561 variables for each observation. 
  #   - Y_train.txt - contains the training labels that link to activity names.  
  #   - subject_train.txt - contains the individual's ID number.
  
  trainXFile <- "./UCI HAR Dataset/train/X_train.txt"
  trainX <- read.csv(trainXFile, sep = "", header = FALSE, stringsAsFactors=FALSE, col.names=featureVariablesNames$V2)
  
  trainYFile <- "./UCI HAR Dataset/train/Y_train.txt"
  trainY <- read.csv(trainYFile, sep = "", header = FALSE, stringsAsFactors=FALSE)
  names(trainY) <- "activity"

  trainYSubjectFile <- "./UCI HAR Dataset/train/subject_train.txt"
  trainYSubject <- read.csv(trainYSubjectFile, sep = "", header = FALSE, stringsAsFactors=FALSE)
  names(trainYSubject) <- "subjectid"
  
  # Make a single data set of the train data
  traindata <- cbind(trainYSubject, trainY, trainX)
  
  # Combine the test and training data sets
  data <- rbind(testdata, traindata)
  data <- arrange(data, subjectid)

  # Replace activity numbers with the descriptive names of the activities
  # First we want to load the activity labels and clean up names
  # We want to make names lower case and remove the underscore character
  activityLabelsFile <- "./UCI HAR Dataset/activity_labels.txt"
  activityLabels <- read.csv(activityLabelsFile, sep = " ", header = FALSE)
  activityLabels$V2 <- tolower(activityLabels$V2)
  activityLabels$V2 <- sub("_", "", activityLabels$V2,)
  
  data$activity <- factor(data$activity, levels=activityLabels$V1, labels=activityLabels$V2)
  
  # Now extract the std and mean
  # This is the first data set from the assignment
  # dataset1 will only contain the following columns:
  # - Subject ID
  # - Activity Name
  # - Columns that contain std and mean in the column name
  dataset1 <- data[, c(1, 2, grep("std", colnames(data)), grep("mean", colnames(data)))]
  
  # Save the data set file
  # Check if the data directory exists.  If not, create it
  if (!file.exists("data")) dir.create("data")
  
  file <- "./data/dataset1.csv"
  write.csv(dataset1, file, row.names=FALSE)
  
  # Create the second, independent tidy data set 
  # This data set contains the with the average of each variable for each activity and each subject. 
  dataset2 <- ddply(dataset1, .(subjectid, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
  
  # Now make the column headings more descriptive
  # Add the suffix "_mean" to column names in this dataset2
  colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")
  
  # Save the data set file
  # Check if the data directory exists.  If not, create it
  if (!file.exists("data")) dir.create("data")
  
  file <- "./data/dataset2.csv"
  write.csv(dataset2, file, row.names=FALSE)
  
}