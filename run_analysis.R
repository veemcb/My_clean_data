# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

# Progress:
# 4 achieved - columns named from features.txt
# 1 achieved - training and test sets are merged
# 2 achieved - only measurements on the mean and standard deviation are retained
# 3 achieved - activities relabelled at line xx using factors read in from the activity_labels.txt


library(dplyr)

# This doens't work - immediate crash of RStudio
#library(data.table)
#a <- fread("UCI_HAR_Dataset/test/X_test.txt")

# Read column names into character vector
mycols <- read.table("UCI_HAR_Dataset/features.txt", stringsAsFactors=FALSE)
# Tidy up by removing empty brackets and changing hyphens to underscores
mycols <- gsub("\\(\\)","", mycols[,2])
mycols <- gsub("-","_",mycols)

print("Reading in data")
# Read in test and training tables
test_data <- read.table("UCI_HAR_Dataset/test/X_test.txt")
train_data <- read.table("UCI_HAR_Dataset/train/X_train.txt")

# Give the measurements descriptive names, i.e. as read in from features.txt
# Fulfills #4 of assigment
colnames(test_data) <- mycols
colnames(train_data) <- mycols

# Get volunteer and activity data for the TEST SET
test_volunteers <- read.table("UCI_HAR_Dataset/test/subject_test.txt", col.names=c("Volunteer_Id"))
test_activity <- read.table("UCI_HAR_Dataset/test/y_test.txt", col.names=c("Activity_Id"))

# Get volunteer and activity data for the TRAINING SET
train_volunteers <- read.table("UCI_HAR_Dataset/train/subject_train.txt", col.names=c("Volunteer_Id"))
train_activity <- read.table("UCI_HAR_Dataset/train/y_train.txt", col.names=c("Activity_Id"))

print("Start the merging")
# Merge activity, volunteers and measurements for each set
complete_test <- cbind(test_volunteers, test_activity, test_data)
complete_train <- cbind(train_volunteers, train_activity, train_data)
print("Merge teh actual data")
# Merge the 2 data sets
complete_data <- rbind(complete_test,complete_train)

print("Test and train are merged")
# Remove some variables to free up space
rm(complete_test, complete_train, test_data, train_data)

# Find duplicates with (returns array of logicals)
a <- duplicated(mycols)
# Returns array indices where a above is FALSE (i.e. the ones that are *not* duplicates)
cee <- which(!a, arr.ind = TRUE)
# Now, subset the array to throw out these duplicate columns
tidy_complete <- complete_data[,cee]#%>%

print("Duplicate columns removed")
# Column names for this tidy set
tidycols <- colnames(tidy_complete)

# Look for columns containing either [Mm]ean or [Ss]td in these steps
a <- grep("mean", tidycols, ignore.case = TRUE)
b <- grep("std",tidycols,ignore.case = TRUE)
# Merge these two sets and remove any duplicates that could screw things up
cee <- unique(c(a,b))

# Now retain only these colummns of the data table (but here I lose volunteer and activity data)
tidy_means_stds <- tidy_complete[,cee]
print("tidy_means_stds has been made")
mn_std_vars <- colnames(tidy_means_stds)

# Allocate the activity names
acts <- read.table("UCI_HAR_Dataset/activity_labels.txt")

# Get them back and stick them back on
id_data <- select(tidy_complete, ends_with("_Id"))
tidier <- cbind(id_data,tidy_means_stds)

# Allocate the Activity labels, clean up and group by volunteer and activity
tidier %>%
  mutate(Activity = factor(Activity_Id,acts[,1],acts[,2])) %>%
  select(-Activity_Id) %>%
  group_by(Activity, Volunteer_Id) %>%
  summarise_each(funs(mean)) -> final_tidy

# Get colnames for all activities

# Tidy up the workspace again
rm(tidy_complete,complete_data,tidy_means_stds,id_data)

# Now none of these columns are means or stds so can we remove them from the dataset
#select(contains("mean") || contains("std"))

#for(i in unique(f)) print(i)

# Now do the same thing for 

# Now, I would filter the unnecessary columns before rbinding with other dataset to save sapce


# Is the volunteer id already in this table? No. How do I do this?
# Read in column names and transpose
#colnames <- read.tab
# Add as col.names
# The data files are large (2947 + 7352 rows)