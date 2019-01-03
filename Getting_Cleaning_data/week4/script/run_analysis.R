## author: Xingmin Aaron Zhang
## Getting and Cleaning Data
## Week 4 assignment
## Goal: 
## Download a dataset on mobile tracking for activity recognition and clean the dataset to produce a summary of features.


## set up working directory by overwriting the "wd" variable. Default to user home directory.
wd <- "~"
if (dir.exists("~/git/datasciencecoursera/Getting_Cleaning_data/week4")) {
    setwd("~/git/datasciencecoursera/Getting_Cleaning_data/week4")
} else if (dir.exists(wd)) {
    setwd(wd)
} else {
    stop("specified working directory not found")
}


## load libraries
if (!require(dplyr)) {
    install.packages("dplyr")
    if (!require(dplyr)) {
        stop("dplyr cannot be installed")
    }
}

## download data collected from the accelerometers from the Samsung Galaxy S smartphone
## unzip the files to the data directory
## the desired data files are in test and train folders: 
## test/X_test.txt, test/y_test.txt, train/X_train.txt, train/y_train.txt
download <- function() {
    fileurl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileurl, destfile = "accelerometer_Samsung_Galaxy_S.zip")
    ## downloaded date
    date()
}

## prepare dataset
dataPath = "accelerometer_Samsung_Galaxy_S.zip"
if (!file.exists(dataPath)) {
    download()
    unzip(dataPath)
    file.rename("UCI HAR Dataset", "data")
}



## read in data
activity_labels <- read.table("data/activity_labels.txt", sep = "", header = FALSE)
feature_names <- read.table("data/features.txt", sep = "", header = FALSE)
test.X <- read.table("data/test/X_test.txt", sep = "", header = FALSE, colClasses = "numeric")
test.y <- read.table("data/test/y_test.txt", sep = "", header = FALSE, colClasses = "numeric")
train.X <- read.table("data/train/X_train.txt", sep = "", header = FALSE, colClasses = "numeric")
train.y <- read.table("data/train/y_train.txt", sep = "", header = FALSE, colClasses = "numeric")
subject_test <- read.table("data/test/subject_test.txt", header = FALSE, colClasses = "numeric")
subject_train <- read.table("data/train/subject_train.txt", header = FALSE, colClasses = "numeric")


## check head of files to make sure they are correctly loaded
dim(test.X)
dim(test.y)
dim(train.X)
dim(train.y)
dim(subject_test)
dim(subject_train)

## select features that are means or standard deviations
selectedfeatureIndex <- grep("mean|std", feature_names$V2)
selectedfeatureName <- feature_names$V2[selectedfeatureIndex]
train.X.selected <- train.X[,selectedfeatureIndex]
test.X.selected <- test.X[,selectedfeatureIndex]
data.X <- rbind(train.X.selected, test.X.selected)
colnames(data.X) <- make.names(selectedfeatureName) # make feature names more suited for column names

## combine activities of train and test
data.y <- rbind(train.y, test.y)
colnames(data.y) <- "activityId"
## change activities Id to activities labels
colnames(activity_labels) <- c("activityId", "activity")
data.y <- data.y %>% left_join(activity_labels, by = "activityId") %>% select(activity)

## combine the subjects of train and test
data.subject <- rbind(subject_train, subject_test)
colnames(data.subject) <- "subjectId"

## combine subject, feature matrix (subset) and activity together
data <- cbind(data.subject, data.X, data.y)

## extract only the measurements on the mean and standard deviation for each measurement 
data$subjectId <- as.factor(data$subjectId)
feature_mean <- data %>% group_by(subjectId, activity) %>% summarise_all(funs(mean))
## rename the feature names by adding a prefix "mean"
feature_mean_name <- paste("mean", make.names(selectedfeatureName), sep = "_")
colnames(feature_mean) <- c("subjectId", "activity", feature_mean_name)

##write out the outcome as a CSV file
if(!dir.exists("result")) {
    dir.create("result")
}

write.table(feature_mean, "result/feature_mean.txt", sep = ",", row.names = FALSE, quote = FALSE)

## write out the column names for codebook
name <- colnames(feature_mean)
note <- c("id of the individual for the study", 
          "one of six activities performed by the subject", 
          paste("mean of", selectedfeatureName))
write.table(data.frame(name, note), "colname.tsv",sep = "\t", row.names = TRUE, quote = FALSE)

## clean up folder
file.remove(dataPath)
file.remove("colname.tsv")
unlink("data", recursive = TRUE)
