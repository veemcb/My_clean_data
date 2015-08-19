# This R script merges the test and training sets and creates a final tidy data set
# comprising the average measurment means and standard deviations for each of 6 activities
# as measured from 30 test subjects.

# These are the objectives:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.



library(dplyr)

# Read the column names of the data set into character vector
mycols <- read.table("UCI_HAR_Dataset/features.txt", stringsAsFactors=FALSE)

# Tidy the column names  by removing empty brackets and changing hyphens to underscores
mycols <- gsub("\\(\\)","", mycols[,2])
mycols <- gsub("-","_",mycols)

print("Reading in data")
# Read in test and training tables
test_data <- read.table("UCI_HAR_Dataset/test/X_test.txt")
train_data <- read.table("UCI_HAR_Dataset/train/X_train.txt")

# Give the measurements descriptive names, i.e. as read in from features.txt
# Fulfills objective #4 
colnames(test_data) <- mycols
colnames(train_data) <- mycols

# Get volunteer and activity data for the TEST SET
test_volunteers <- read.table("UCI_HAR_Dataset/test/subject_test.txt", col.names=c("Volunteer_Id"))
test_activity <- read.table("UCI_HAR_Dataset/test/y_test.txt", col.names=c("Activity_Id"))

# Get volunteer and activity data for the TRAINING SET
train_volunteers <- read.table("UCI_HAR_Dataset/train/subject_train.txt", col.names=c("Volunteer_Id"))
train_activity <- read.table("UCI_HAR_Dataset/train/y_train.txt", col.names=c("Activity_Id"))

print("Start the merging")
# Merge activity, volunteers and measurements for each set with a column bind
complete_test <- cbind(test_volunteers, test_activity, test_data)
complete_train <- cbind(train_volunteers, train_activity, train_data)
print("Merge thh actual data")

# Merge the 2 data sets with a row bind
# Objective #1 is achieved here
complete_data <- rbind(complete_test,complete_train)

print("Test and train are merged")
# Remove some variables to free up space
rm(complete_test, complete_train, test_data, train_data)

# Find duplicate column names and remove them
a <- duplicated(mycols)
# Returns array indices where a above is FALSE (i.e. the ones that are *not* duplicates)
cee <- which(!a, arr.ind = TRUE)
# Now, subset the array to throw out these duplicate columns
tidy_complete <- complete_data[,cee]

print("Duplicate columns removed")
# Column names for this tidy set
tidycols <- colnames(tidy_complete)

# Look for columns containing either [Mm]ean or [Ss]td in these steps
# Objective #2 is achieved here
a <- grep("mean", tidycols, ignore.case = TRUE)
b <- grep("std",tidycols,ignore.case = TRUE)
# Merge these two sets and remove any duplicates that could screw things up
cee <- unique(c(a,b))

# Now retain only these colummns of the data table (but this step loses volunteer and activity data)
tidy_means_stds <- tidy_complete[,cee]
print("tidy_means_stds has been made")
mn_std_vars <- colnames(tidy_means_stds)

# Allocate the activity names
# Objective #3 is achieved here
acts <- read.table("UCI_HAR_Dataset/activity_labels.txt")

# Get the volunteer and activity columns and stick them back on
id_data <- select(tidy_complete, ends_with("_Id"))
tidier <- cbind(id_data,tidy_means_stds)

# Allocate the Activity labels, clean up and group by volunteer and activity
tidier %>%
  mutate(Activity = factor(Activity_Id,acts[,1],acts[,2])) %>%
  select(-Activity_Id) %>%
  group_by(Activity, Volunteer_Id) %>%
  summarise_each(funs(mean)) -> final_tidy
# Objective #5 is achieved


# Tidy up the workspace again, deleting temporary variables
rm(tidy_complete,complete_data,tidy_means_stds,id_data)

