#' The data for the project:
#'
#'      https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#'
#' A full description of the data set:
#'
#'      http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#'
#' You will be required to submit:
#'
#' 1) a tidy data set as described below,
#' 2) a link to a Github repository with your script for performing the analysis,
#'    and
#' 3) a code book that describes the variables, the data, and any transformations
#'    or work that you performed to clean up the data called CodeBook.md.
#' 4) You should also include a README.md in the repo with your scripts. This
#'    repo explains how all of the scripts work and how they are connected.
#'
#' You should create one R script called run_analysis.R that does the
#' following.
#'
#'  1. Merges the training and the test sets to create one data set.
#'  2. Extracts only the measurements on the mean and standard deviation for
#'     each measurement.
#'  3. Uses descriptive activity names to name the activities in the data set
#'  4. Appropriately labels the data set with descriptive variable names.
#'  5. From the data set in step 4, creates a second, independent tidy data set
#'     with the average of each variable for each activity and each subject.

library(dplyr)

dsdir = 'UCI HAR Dataset'			# Data set folder
dsets <- c('train', 'test')			# Data sets
dsname <- 'X_%s.txt'				# Data set file names
sbjctids <- 'subject_%s.txt'			# Volunteer IDs
lblids <- 'y_%s.txt'				# Activity IDs

# Activity labels:
activities = read.table(file.path(dsdir, 'activity_labels.txt'),
	col.names = c("aid", "activity"))

# Feature (column) names, with addition columns to the left
xcolnames <- c("category", "subject", "aid")
for (xcol in select(read.table(file.path(dsdir, 'features.txt')), 2)[1])
{
  xcol <- gsub('[(,-]', '_', gsub('[)]', '', paste(xcol)))
  xcolnames <- c(xcolnames, xcol)
}
xselect <- c(1, 2, 3, grep('[Mm]ean|std', xcolnames))

# Read data sets, add category and volunteer IDs:
xdata <- NA
for (dset in dsets) {
  dsfn <- file.path(dsdir, dset, sprintf(dsname, dset))
  aidfn <- file.path(dsdir, dset, sprintf(lblids, dset))
  sidfn <- file.path(dsdir, dset, sprintf(sbjctids, dset))
  message(sprintf('Loading "%s" dataset...', dsfn))
  xd <- read.table(dsfn)
  # Extract only mean and stndard deviation of each measurement
  xd <- cbind(read.table(aidfn), read.table(sidfn), xd)
  xd <- cbind(c(dset), xd)
  colnames(xd) <- xcolnames
  xd <- xd[xselect]
  if (is.na(xdata)) {
    xdata <- xd
  } else {
    xdata <- rbind(xdata, xd)
  }
  rm(xd)
}

#' 4 - Write out the tidy data set:
write.table(merge(xdata, activities, by=c("aid")), "tidy-data.csv")

#' 5. - Generate second, independent tidy data set with the average of
#'      each variable for each activity and each subject.
xdata <- group_by(xdata, aid, subject)
xcolnames <- colnames(xdata)
xd5 <- NA
for (xvar in xcolnames[4:length(xcolnames)]) {
  message(sprintf('Summarizing "%s"...', xvar))
  xd <- summarize_(xdata, interp("mean(xvar)", xvar=as.name(xvar)))
  if (is.na(xd5)) {
    xd5 <- xd
  } else {
    xd5 <- cbind(xd5, xd[,3])
  }
}
write.table(xd5, "tidy-data-set-5.csv")
