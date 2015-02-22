
if(!file.exists("UCI\ HAR\ Dataset/")){
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dest = "Fucidata.zip", method = "curl")
unzip("Fucidata.zip")
}
setwd("UCI\ HAR\ Dataset")

features <- read.table("features.txt", stringsAsFactors = FALSE)
activity_labels <- read.table("activity_labels.txt", stringsAsFactors = FALSE)
test <- read.table("./test/X_test.txt", stringsAsFactors = FALSE)
train <- read.table("./train/X_train.txt", stringsAsFactors =FALSE)
train_labels <- read.table("./train/y_train.txt", stringsAsFactors = FALSE)
subject_test <- read.table("./test/subject_test.txt", stringsAsFactors = FALSE)
test_labels <- read.table("./test/y_test.txt")
subject_train <- read.table("./train/subject_train.txt")
test <- read.table("./test/X_test.txt")

test_data <- cbind(subject_test, test_labels, test)
train_data <- cbind(subject_train, train_labels, train)

colnames(test_data) <- c("subject", "activity", as.character(features[,2]))
colnames(train_data) <- c("subject", "activity", as.character(features[,2]))

alldata <- rbind(train_data, test_data)

listmean <- grep("mean", colnames(alldata), ignore.case = TRUE)
listdev <- grep("st", colnames(alldata), ignore.case = TRUE)

selectlist <- sort(append(listmean, listdev))

activityname <- c(alldata[[2]])

replace_activity <- function(activityname) {
                                 activity_name = as.character(activity_labels$V2[activityname])
}


activity_name <- as.factor(sapply(activityname, replace_activity))


df <- tbl_df(alldata[c(1, 2, selectlist)])
 
df[2] = activity_name

classify_activity <- function(activityname) {
  if(activityname <=3)
  activity_type = "STATIONARY"
  else activity_type = "MOVING"
}

activity_type <- sapply(activityname, classify_activity)

df[2] = activity_type

df_tidy <- aggregate(df[3:88], df[1:2], mean)

oldnames <- colnames(df_tidy)

newnames <- make.names(oldnames)
colnames(df_tidy) <- newnames

write.table(df_tidy, file = "tidy_output.txt", row.name= FALSE)


