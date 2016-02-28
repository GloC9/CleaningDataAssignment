run_analysis <- function() {
    ## Read features
    features <- read.table("UCI HAR Dataset\\features.txt", sep="")
    ## Read activity labels
    activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt", sep="")
    ## Read test set
    test_set <- read.table("UCI HAR Dataset\\test\\x_test.txt", sep="")
    ## Read test labels
    test_labels <- read.table("UCI HAR Dataset\\test\\y_test.txt", sep="")
    ## Read test subjects
    test_subjects <- read.table("UCI HAR Dataset\\test\\subject_test.txt", sep="")
    ## Read train set
    train_set <- read.table("UCI HAR Dataset\\train\\x_train.txt", sep="")
    ## Read train labels
    train_labels <- read.table("UCI HAR Dataset\\train\\y_train.txt", sep="")
    ## Read train subjects
    train_subjects <- read.table("UCI HAR Dataset\\train\\subject_train.txt", sep="")

    ## Merge the test set and training set (step 1)
    data_set = rbind(test_set, train_set)
    data_labels = rbind(test_labels, train_labels)
    data_subjects = rbind(test_subjects, train_subjects)
    
    ## Get the mean() measurement column ids
    col1 <- grep("mean()", features$V2)
    ## Get the std() measurement column ids
    col2 <- grep("std()", features$V2)
    ## Get all mean() and std() column ids
    cols <- sort(c(col1, col2))
    ## Extract only the measurements on the mean and standard deviation (step 2)
    data <- data_set[,cols]
    
    ## Add activity and subject to the data set
    names(data_labels) <- c("Activity")
    names(data_subjects) <- c("Subject")
    data <- cbind(data_labels, data_subjects, data)
    ## Uses descriptive activity names in the data set (step 3)
    names(activity_labels) <- c("Activity", "Activity.Name")
    data <- merge(activity_labels, data, by.x="Activity", by.y="Activity", all=TRUE)
    data <- data[, 2:ncol(data)]

    ## Label the data set with descriptive variable names (step 4)
    v_names <- names(data)
    for (i in 3:length(v_names))
    {
        ## Get the numeric value of the variable label
        v_names[i] <- sub("V", "", v_names[i])
        ## Find the descriptive name of the variable
        v_names[i] <- as.character(features[features$V1==v_names[i], 2])
    }
    names(data) <- v_names
    
    ## Create tidy data set (step 5)
    library(dplyr)
    library(reshape2)
    ## Reshape the data set
    melted <- melt(data, id.vars=c("Subject", "Activity.Name"))
    ## Group the data set
    grouped <- group_by(melted, Subject, Activity.Name, variable)
    ## Summarize the data set
    tidy_data <- summarise(grouped, average=mean(value))
    ## Save the tidy data set to a txt file
    write.table(tidy_data, file="tidy_data.txt", row.names=FALSE)
    tidy_data
}
