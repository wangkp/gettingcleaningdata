readBaseSet <- function(filePath, filteredFeatures, features) {
      cols_widths <- rep(-16, length(features))
      cols_widths[filteredFeatures] <- 16
      rawSet <- read.fwf(
            file=filePath,
            widths=cols_widths,
            col.names=features[filteredFeatures])
}

readAdditionalFile <- function(dataDirectory, filePath) {
      filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
      filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
      data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
      data
}


correctFeatureName <- function(featureName) {
      featureName <- gsub("\\(", "", featureName)
      featureName <- gsub("\\)", "", featureName)
      featureName
}

readSets <- function(dataDirectory) {
      featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
      features <- read.table(featuresFilePath)[,"V2"]
      filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))
      features <- correctFeatureName(features)
      set <- readBaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
      set <- rbind(set, readBaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))
      set$subject <- readAdditionalFile("UCI HAR Dataset", "subject")
      activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
      activities <- read.table(activitiesFilePath)[,"V2"]
      set$activity <- activities[readAdditionalFile("UCI HAR Dataset", "y")]
      set
}

createSummaryDataset <- function(dataDirectory) {
      sets <- readSets(dataDirectory)
      sets_x <- sets[,seq(1, length(names(sets)) - 2)]
      summary_by <- by(sets_x,paste(sets$subject, sets$activity, sep="_"), FUN=colMeans)
      summary <- do.call(rbind, summary_by)
      summary
}

dataDirectory <- "UCI HAR Dataset"
if (!file.exists(dataDirectory)) {
      url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
      tmp_file <- "./temp.zip"
      download.file(url,tmp_file, method="curl")
      unzip(tmp_file, exdir="./")
      unlink(tmp_file)
}

summary <- createSummaryDataset(dataDirectory)
write.table(summary, "tidydata.txt",rownames=FALSE)