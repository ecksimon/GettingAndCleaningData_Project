### Peer-graded assignment: Getting and Cleaning Data course Project
getwd()
setwd("C:/Users/Simon Eckert/Documents/R/3 - Getting and cleaning data/final_assignment/getdata_projectfiles_UCI HAR Dataset")
getwd()

# 1. Merging training and test set

# Load the feature names (columns) for the data
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)
features <- as.character(features$V2)  # Extract the feature names

# Load the training and test datasets
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Assign column names to the X data (features)
colnames(X_train) <- features
colnames(X_test) <- features

# Combine the training and test data for X, y, and subjects
X_combined <- rbind(X_train, X_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)

# Combine all data into one final dataset
data_combined <- cbind(subject_combined, y_combined, X_combined)

# Rename the columns for clarity
colnames(data_combined)[1:2] <- c("subject","activity")
colnames(data_combined)[3:ncol(data_combined)] <- features

# Display the first few rows of the combined dataset
head(data_combined)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std_columns <- grep("mean|std",names(data_combined),ignore.case = TRUE)
mean_std_columns

data_combined_mean_std <- data_combined[c(1,2,mean_std_columns)]


### 3. Uses descriptive activity names to name the activities in the data set

# Load the activity labels (descriptive names)
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
activity_labels <- as.character(activity_labels$V2)  # Extract the activity names

# Replace the numeric activity labels with descriptive names
data_combined_mean_std$activity <- factor(data_combined_mean_std$activity, levels = 1:6, labels = activity_labels)

# Display the first few rows of the updated dataset
head(data_combined_mean_std)

### 4. Appropriately labels the data set with descriptive variable names

# Clean the feature names to make them more descriptive
clean_features <- colnames(data_combined_mean_std)
clean_features <- gsub("\\()", "", clean_features)  # Remove parentheses
clean_features <- gsub("-", "_", clean_features)  # Replace dashes with underscores
clean_features <- gsub("^t", "time", clean_features)  # Replace "t" with "time"
clean_features <- gsub("^f", "frequency", clean_features)  # Replace "f" with "frequency"
clean_features <- gsub("Acc", "Accelerometer", clean_features)  # Replace Acc with Accelerometer
clean_features <- gsub("Gyro", "Gyroscope", clean_features)  # Replace Gyro with Gyroscope
clean_features <- gsub("Mag", "Magnitude", clean_features)  # Replace Mag with Magnitude
clean_features <- gsub("BodyBody","Body",clean_features) # Replace mistake
clean_features <- gsub("meanFreq","mean_Frequency",clean_features) # Clarify meanFreq
clean_features <- gsub("\\(t","(time",clean_features) # Clarify t in parentheses
clean_features <- gsub("gravityMean","gravity_mean",clean_features) # Clarify grarvityMean


# Assign cleaned column names to the data
colnames(data_combined_mean_std) <- clean_features
colnames(data_combined_mean_std)

# Display the first few rows of the data to check the new column names
head(data_combined)

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject

install.packages("dplyr")
library(dplyr)

tidy_data <- data_combined_mean_std %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean))

# View the first few rows of the tidy data
head(tidy_data)










