# You should create one R script called run_analysis.R that does the following. 
# 1- Merges the training and the test sets to create one data set.
# 2- Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3- Uses descriptive activity names to name the activities in the data set
# 4- Appropriately labels the data set with descriptive activity names. 
# 5- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# also from the forum:
# - have you combined the training and test x and y into one block, given them headings, 
#     and turned the numeric activities into something easier to read. Think of it as your
#     data files are blocks of lego and you are working out how to clip them together to make a wall.
# - have you extracted some variables to do with mean and standard deviation from the full set. 
#     I am being non-specific here because in this assignment you are using you professional 
#     judgement about which variables to include and documenting your reasoning. 
#     There is no specific number of columns that is correct.

# We are selecting only the measurements whose name includes mean, std in the column name

#
# this function read one of data set file groups and merge the individual files
readOriginalData <- function(set_type) {
  # The set_type is used for the directory and to compute the file name.
  file_name <- file.path(set_type, paste0("y_", set_type, ".txt"))
  y_data <- read.table(file_name, header=FALSE, col.names=c("ActivityID"))
  
  file_name <- file.path(set_type, paste0("subject_", set_type, ".txt"))
  subject_data <- read.table(file_name, header=FALSE, col.names=c("SubjectID"))
  
  # read the column names
  data_cols <- read.table("features.txt", header=FALSE, as.is=TRUE, col.names=c("MeasureID", "MeasureName"))
  
  # read the X data file
  file_name <- file.path(set_type, paste0("X_", set_type, ".txt"))
  data <- read.table(file_name, header=FALSE, col.names=data_cols$MeasureName)
    
  # just the columns we are interested in
  col_names <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
  data <- data[,col_names]
  
  # append the activity id and subject id columns
  data$ActivityID <- y_data$ActivityID
  data$SubjectID <- subject_data$SubjectID
  
  # return the data
  data
}

# Combine training and test data sets and add the activity label as another column
getTidyData <- function() {
  library(reshape2)
  
  # merge the 2 data sets
  data <- rbind(readOriginalData("test"), readOriginalData("train"))
  
  # clean up the names
  col.names <- colnames(data)
  col.names <- gsub("\\.+mean\\.+", col.names, replacement="Mean")
  col.names <- gsub("\\.+std\\.+",  col.names, replacement="Std")
  colnames(data) <- col.names
  
  # Add the activity labels
  activity_labels <- read.table("activity_labels.txt", header=FALSE, as.is=TRUE, col.names=c("ActivityID", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  merged_data <- merge(data, activity_labels)
  
  # our id_variables:
  id_vars = c("ActivityID", "ActivityName", "SubjectID")
  # what are we measuring
  measure_vars = setdiff(colnames(merged_data), id_vars)
  
  # melt the dataset
  melted_data <- melt(merged_data, id.vars=id_vars, measure.vars=measure_vars)
  
  # reorder the table and make sure that we have a data frame output
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

print("This code has to execute from the UCI HAR Dataset directory")

print("Creating tidy dataset as TidyDataSet.txt")
write.table(getTidyData(), "TidyDataSet.txt")
print("Completed successfully.")