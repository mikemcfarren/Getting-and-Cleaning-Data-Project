##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Mike McFarren

# run_analysis.R

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


# 1. Merges the training and the test sets to create one data set.
  
    # set working directory to folder containing zip file contents
    setwd('./UCI HAR Dataset/');

    # read each of the files
    activityLabels = read.table('./activity_labels.txt', header=FALSE); 
    features = read.table('./features.txt', header=FALSE); 

    # read the train data files
    subjectTrain = read.table('./train/subject_train.txt', header=FALSE); 
    xTrain = read.table('./train/x_train.txt', header=FALSE); 
    yTrain = read.table('./train/y_train.txt', header=FALSE); 
    
    # make the tables a bit more comprehensible by assigning names to each of the columns
    colnames(activityLabels) = c("activityid", "activitytype");
    colnames(features) = c("featureid", "featurename");
    colnames(subjectTrain) = "subjectid";
    colnames(xTrain) = features$featurename;
    colnames(yTrain) = "activityid";
    
    # merge xTrain, yTrain, and subjectTrain
    trainData = cbind(xTrain, yTrain, subjectTrain);

    
    # read the test data files
    subjectTest = read.table('./test/subject_test.txt', header=FALSE);
    xTest = read.table('./test/x_test.txt', header=FALSE);
    yTest = read.table('./test/y_test.txt', header=FALSE);
    
    # assigning names to each of the columns
    colnames(subjectTest) = "subjectid";
    colnames(xTest) = features$featurename;
    colnames(yTest) = "activityid";

    # merge (column bind) test files 
    testData = cbind(xTest, yTest, subjectTest);
    
    # append test and training data sets
    myData = rbind(trainData, testData);

    

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

    # vector of all the column headers in the original dataset
    allColumns = colnames(myData);
    
    # get a vector of columns that meet the criteria defined above
    applicableColumns = (grepl("activity..", allColumns) | grepl("subject..", allColumns) | ( grepl("-mean..", allColumns) & !grepl("-meanFreq..", allColumns) & !grepl("mean..-", allColumns) ) | ( grepl("-std..", allColumns) & !grepl("-std()..-", allColumns) ) );
    
    # get only the applicable columns that met the criteria above and store new dataset into myCleanData
    myCleanData = myData[applicableColumns==TRUE];
    
    
    
# 3. Uses descriptive activity names to name the activities in the data set

    myCleanData = merge(myCleanData, activityLabels, by='activityid', all.x=TRUE);
    
    
    
# 4. Appropriately labels the data set with descriptive variable names.

    allColumns = colnames(myCleanData);
    allColumns = gsub("\\()", "", allColumns)
    allColumns = gsub("-std$", "StdDev", allColumns)
    allColumns = gsub("-mean", "Mean", allColumns)
    allColumns = gsub("[Gg]yro", "Gyro", allColumns)
    allColumns = gsub("([Gg]ravity)", "Gravity", allColumns)
    allColumns = gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", allColumns)
    allColumns = gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", allColumns)
    allColumns = gsub("AccMag", "AccMagnitude", allColumns)
    allColumns = gsub("GyroMag", "GyroMagnitude", allColumns)
    allColumns = gsub("JerkMag", "JerkMagnitude", allColumns)
    
    allColumns = gsub("^(t)", "time", allColumns)
    allColumns = gsub("^(f)", "freq", allColumns)
    
    
    colnames(myCleanData) = allColumns;
    
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.
    
    myCleanDataWithoutLabels = myCleanData[, names(myCleanData) != 'activitytype'];
    
    # Summarize table to include just the mean of each variable for each activity and each subject
    tidyDataSet = aggregate(myCleanDataWithoutLabels[, names(myCleanDataWithoutLabels) != c('activityid', 'subjectid')], by=list(activityid=myCleanDataWithoutLabels$activityid, subjectid = myCleanDataWithoutLabels$subjectid), mean);
    

    # Merge the tidyDataSet with the descriptive activityTypes
    tidyDataSet = merge(tidyDataSet, activityLabels, by='activityid', all.x=TRUE);
    
    # Export  tidyDataSet
    write.table(tidyDataSet, './tidyDataSet.txt', row.names=FALSE, sep='\t');
    
    
