library(dplyr)

#download file
if(!file.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./data/smartphone.zip")
unzip("./data/smartphone.zip", exdir="./data")
path <- file.path("./data", "UCI HAR Dataset")

#reading data
data.activitytest <- read.table(file.path(path,"test","Y_test.txt"))
data.activitytrain <- read.table(file.path(path,"train","Y_train.txt"))

data.subjecttest <- read.table(file.path(path,"test","subject_test.txt"))
data.subjecttrain <- read.table(file.path(path,"train","subject_train.txt"))

data.featuretest <- read.table(file.path(path,"test","X_test.txt"))
data.featuretrain <- read.table(file.path(path,"train","X_train.txt"))

#reading labels
features <- read.table(file.path(path,"features.txt"))
activity <- read.table(file.path(path,"activity_labels.txt"))

#merging datasets
data.subject <- rbind(data.subjecttest, data.subjecttrain)
data.activity <- rbind(data.activitytest, data.activitytrain)
data.feature <- rbind(data.featuretest, data.featuretrain)

#naming variables
colnames(data.subject) <- "subjectID"
colnames(data.activity) <- "activityID"
colnames(data.feature)<- features[,2]

data.combine <- cbind(data.feature, data.subject, data.activity)

#extracting relevant measurements
selectedcol <- grepl("*mean\\(\\)|*std\\(\\)|subjectID|activityID", names(data.combine))
data.select <- tbl_df(data.combine[ , selectedcol])

#labelling data
colnames(activity) <- c("activityID", "activity")
data <- tbl_df(merge(data.select, activity, by="activityID"))
data <- data[, c(2,ncol(data), 3:(ncol(data)-1))]

names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data) <- gsub("Acc", "Accelerometer", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

#creating tidy data set
data.tidy <- aggregate(. ~subjectID + activity, data, mean)
data.tidy <- tbl_df(arrange(data.tidy, subjectID))

write.table(data.tidy, "./data/TidyData.txt", row.names = FALSE, quote = FALSE, col.names = TRUE)


