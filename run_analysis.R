# openning all needed ".txt" files and using proper column names
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("label", "activity"))
train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features[,2])
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
activity_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "labels")
test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features[,2])
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
activity_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "labels")


#3 preparing tables to substitute number by names of activities 
act_train <- merge(activity_train, activity_labels, by.x = "labels", by.y = "label", sort = FALSE)
act_test <- merge(activity_test, activity_labels, by.x = "labels", by.y = "label", sort = FALSE)

#2 Extracting only measures on the mean and standart deviation for each measurement 
train_extraction <- select(train, contains("mean"), contains("std"))
test_extraction <- select(test, contains("mean"), contains("std"))

#1 combining all sub tables in only one table, called "complete table" and merging the training and test sets
train_complete <- cbind(subject_train, "activity" = act_train[,2], train_extraction)
test_complete <- cbind(subject_test, "activity" = act_test[,2], test_extraction)
complete_table <- rbind(train_complete, test_complete)

#4 Modifications on lables trying to make the variable names more understandable
names(complete_table) <- gsub("()","",names(complete_table))
names(complete_table) <- gsub("^t","",names(complete_table))
names(complete_table) <- gsub("^f","FFT_",names(complete_table))
names(complete_table) <- gsub("Body","Body_",names(complete_table))
names(complete_table) <- gsub("Body_Body_","Body_",names(complete_table))
names(complete_table) <- gsub("Gravity","Gravity_",names(complete_table))

#5 Creating an independent tidy data set with the average of each variable for each activity and each subject
complete_tidy <- melt(complete_table, id = c("subject", "activity"))
by_subj <- group_by(complete_tidy, subject)
by_subj_act <- group_by(by_subj, activity, add = TRUE)
by_subj_act_var <- group_by(by_subj_act, variable, add = TRUE)
tidy_data <- summarize(by_subj_act_var, average = mean(value))

# Exporting tidy data set
write.table(tidy_data, file = "tidy_data,txt", row.name = FALSE)
