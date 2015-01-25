# Download and unpack data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "UCI HAR Dataset.zip")
download.file(url, f, mode = "wb")
unzip(f)

# Read data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

# Step 1 - Merge the training and the test sets to create one data set.
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement. 
# Use list of all features (features.txt)
features <- read.table("UCI HAR Dataset/features.txt")
# Look up all the features with "mean" in their names
mean_features <- grep("mean", features[,2])
# Look up all the features with "std" in their names
std_features <- grep("std", features[,2])
# Extract these features into separate dataframe
x_all_mean_std <- x_all[, c(mean_features, std_features)]

# Step 3 - Use descriptive activity names to name the activities in the data set
# Use list of activities (activity_labels.txt)
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
x_all_mean_std$activity <- as.factor(y_all[,1])
# Assign descriptive names to the factor levels
levels(x_all_mean_std$activity) <- activity_labels[,2]

# Step 4 - Appropriately label the data set with descriptive variable names. 
# Select only the names of "mean" and "std" features
names_mean_std <- as.character(features[c(mean_features, std_features), 2])
# Prepare names for R naming rules
names_mean_std <- gsub("(", "", names_mean_std, fixed=TRUE)
names_mean_std <- gsub(")", "", names_mean_std, fixed=TRUE)
names_mean_std <- gsub("-", "_", names_mean_std, fixed=TRUE)
# Assign names to the dataframe variables (including the name for our "activity" variable)
names(x_all_mean_std) <- c(names_mean_std, "activity")


# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Read and merge subjects data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_all <- rbind(subject_train, subject_test)
# Add "subject" variable to the dataframe
x_all_mean_std$subject <- as.factor(subject_all[,1])
# Aggregate average for all numeric variables
result_averages <- aggregate(x_all_mean_std[, !names(x_all_mean_std) %in% c("activity", "subject")], by=list(x_all_mean_std$activity, x_all_mean_std$subject), FUN=mean)
# Adjust names in the result dataframe
names(result_averages) <- c("activity", "subject", paste(names_mean_std, "_AVG", sep=""))
# Write result into file
write.table(result_averages, file="result_averages.txt", row.names=FALSE)
