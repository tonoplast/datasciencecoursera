
## a placeholder for current directory (if re-running the code)
setwd(pwd)

# clear environment
rm(list=ls())

# load package(s)
library(dplyr)
library(tidyr)
library(stringr)

## a placeholder for current directory
pwd <- getwd()

# downloading zip file
this_file <- "Samsung_fitbit_data.zip"
this_Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(this_Url, this_file, method= 'curl')

# unzip file
unzip(this_file)

# set working directory
setwd("./UCI HAR Dataset")

# finding all txt files in the directory and sub directories
all_txt <- list.files(pattern=".txt", recursive=TRUE)

# remove unncessary files from the list
infile_all <- all_txt[ !grepl("(?i)inertial|(?i)readme|(?i)info", all_txt)]

# reading txt files
for (i in 1:length(infile_all)) {

  # object names using file name
  fname <- sub("\\..*", '', sub(".*/", '', infile_all[i]))
  
  # assigning objects (using above name) with txt file data
  assign(fname, read.table(infile_all[i]))
  
}


# attaching rownum and removing special characters from 'features' for colname use later
features <- features %>% 
  unite(feat_id, V2,V1, sep="_") %>%
  mutate(feat_id = str_replace_all(feat_id, "[[:punct:]]+", "_"))

# assigning colnames
colnames(activity_labels) <- c("activity_id","activity_type")

colnames(subject_test) <- "subj_id"
colnames(subject_train) <- "subj_id"
colnames(y_test) <- "activity_id"
colnames(y_train) <- "activity_id"
colnames(X_test) <- features[,1]
colnames(X_train) <- features[,1]


#############################################################################################################
# 1. Merges the training and the test sets to create one data set.

## test data
# merging
test_data <- cbind(subject_test, y_test, X_test)

# indicating what type of data (just in case)
test_data$data_type <- "test"

## train data
# merging
train_data <- cbind(subject_train, y_train, X_train)

# indicating what type of data
train_data$data_type <- "train"

## merged data
# merging
final_data <- rbind(test_data, train_data)

# putting data_type in the 3rd column (just for preference)
final_data <- final_data %>% select(1:2, data_type, everything())

# removing objects in environment except 'final_data' , 'activity_labels' and directory info
rm(list=setdiff(ls(), c("final_data","activity_labels","pwd")))


#############################################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# look for 'mean' and 'std' in column names (ignoring case) and output table
mean_std_idx <- grep('subj|activity|data_type|mean|std', ignore.case = TRUE, colnames(final_data))
mean_std_data <- final_data[mean_std_idx]


#############################################################################################################
# 3. Uses descriptive activity names to name the activities in the data set

# merging with activity_labels which has descriptive activity (activity_type)
mean_std_data <- merge(mean_std_data, activity_labels, by="activity_id")

# re-ordering columns as preference
mean_std_data <- mean_std_data %>% select(2:1, activity_type, everything())

# removing 'activity_labels' data as it's no longer needed
rm(activity_labels)


#############################################################################################################
# 4. Appropriately labels the data set with descriptive variable names.

new_name <- str_replace(names(mean_std_data), "^t", "Time")
new_name <- str_replace(new_name, "^f", "Frequency")
new_name <- str_replace(new_name, "(?i)acc", "Accelerometer")
new_name <- str_replace(new_name, "(?i)gyro", "Gyroscope")
new_name <- str_replace(new_name, "(?i)mag", "Magnitude")
new_name <- str_replace(new_name, "(?i)mean", "Mean")
new_name <- str_replace(new_name, "(?i)std", "Std")

names(mean_std_data) <- new_name


#############################################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# mean-ing by the grouped variables
summarised_data <- mean_std_data %>% 
  select(-c(activity_id, data_type)) %>%
  group_by(subj_id, activity_type) %>%
  summarise_all(funs(mean))

# output-ing data
setwd(pwd)
write.table(summarised_data, file="tidy_averaged_data.txt", row.names = FALSE)


