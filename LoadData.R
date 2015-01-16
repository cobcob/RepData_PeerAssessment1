### Load required libraries
library(dplyr)

#Read in data
dataset <- read.csv (file="activity/activity.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#Convert character to date
dataset$date <- as.Date(dataset$date)
#Convert data frame to data table for using dplyr package
dataset <- tbl_df (dataset)

#Mean Total Number of Steps Taken Per Day

## Histogram
#StepsPerDay <- tapply (dataset$steps, dataset$date, sum, na.rm=TRUE)

#GroupStepsByDate <- group_by(dataset, date) %>%
#        summarise(sum(steps, na.rm=TRUE))
#names(GroupStepsByDate) <- c("Date", "TotalSteps")

#hist(GroupStepsByDate$TotalSteps, xlab="Total Steps Per Day", main="Histogram of Total Steps Per Day")

#mean(GroupStepsByDate$TotalSteps)
#median(GroupStepsByDate$TotalSteps)


GroupStepsByInterval <- group_by(dataset, interval) %>%
        summarise(mean(steps, na.rm=TRUE))
names(GroupStepsByInterval) <- c("Interval", "AvgSteps")

#plot(GroupStepsByInterval$Interval, GroupStepsByInterval$AvgSteps, type="l", main="Average Number of Steps Per 5 Minute Interval", xlab="5 Minute Intervals", ylab="Average Steps")

#Create imputed dataset
ImputedDataset <- dataset
#Add imputed values by matching them from the table containing average steps per interval
ImputedDataset$ImputedSteps <- GroupStepsByInterval$AvgSteps[match(GroupStepsByInterval$Interval, ImputedDataset$interval)]
#When the steps value is NA replace this with the imputed value
ImputedDataset$steps[is.na(ImputedDataset$steps)] <- ImputedDataset$ImputedSteps[is.na(ImputedDataset$steps)]
#Remove the temporary imputed column
ImputedDataset$ImputedSteps <- NULL

ImputedDataset$day <- weekdays(ImputedDataset$date)
ImputedDataset$TimeOfWeek <- ifelse(ImputedDataset$day == "Saturday" | ImputedDataset$day == "Sunday", "weekend", "weekday")


#xyplot(GroupStepsByInterval$Interval, GroupStepsByInterval$AvgSteps, type="l", main="Average Number of Steps Per 5 Minute Interval", xlab="5 Minute Intervals", ylab="Average Steps")


updatedGroupStepsByInterval <- group_by(ImputedDataset, TimeOfWeek, interval) %>%
        summarise(mean(steps, na.rm=TRUE))
names(updatedGroupStepsByInterval) <- c("TimeOfWeek", "Interval", "AvgSteps")
updatedGroupStepsByInterval$TimeOfWeek <- as.factor(updatedGroupStepsByInterval$TimeOfWeek)

xyplot(updatedGroupStepsByInterval$Interval ~ updatedGroupStepsByInterval$AvgSteps | updatedGroupStepsByInterval$TimeOfWeek, type = "l",layout = c(1, 2), xlab = "Interval", ylab = "Average Number of steps")