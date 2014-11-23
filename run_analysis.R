# 1. Create a dataset by merging the train and the test
#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/Morteza/Documents/test');
# Read data
xTrain<-read.table('./train/x_train.txt',header=FALSE); #import from x_train.txt
yTrain<-read.table('./train/y_train.txt',header=FALSE); #import from y_train.txt
features<-read.table('./features.txt',header=FALSE); #import from features.txt
activityType<-read.table('./activity_labels.txt',header=FALSE); #import from activity_labels.txt
subjectTrain<-read.table('./train/subject_train.txt',header=FALSE); #import from subject_train.txt
# Create column names
colnames(activityType)<-c('activityId','activityType');
colnames(subjectTrain)<-"subjectId";
colnames(xTrain)<-features[,2]; 
colnames(yTrain)<-"activityId";
#Merging yTrain, subjectTrain, and xTrain
trainingData <-cbind(yTrain,subjectTrain,xTrain);
# Read in the test data
subjectTest <-read.table('./test/subject_test.txt',header=FALSE);
xTest<-read.table('./test/x_test.txt',header=FALSE); 
yTest<-read.table('./test/y_test.txt',header=FALSE); 
# Test Data column's Name
colnames(subjectTest)<-"subjectId";
colnames(xTest)<- features[,2]; 
colnames(yTest)<-"activityId";
# Merging xTest, yTest and subjectTest data
testData <-cbind(yTest,subjectTest,xTest);#Final Test Data Created
# Final data set by merging the test and training
finalData<-rbind(trainingData,testData);
colNames<-colnames(finalData);
# 2.Mean and standard deviation 
logicalVector<-(grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
finalData<-finalData[logicalVector==TRUE];#Subset for only desired columns
# 3.Descriptive activity names 
finalData<- merge(finalData,activityType,by='activityId',all.x=TRUE);
colNames<-colnames(finalData); 
# 4.Labeling the data set with descriptive activity names 
for (i in 1:length(colNames)) 
{
colNames[i]<-gsub("\\()","",colNames[i])
colNames[i]<-gsub("-std$","StdDev",colNames[i])
colNames[i]<-gsub("-mean","Mean",colNames[i])
colNames[i]<-gsub("^(t)","time",colNames[i])
colNames[i]<-gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
colNames[i]<-gsub("[Gg]yro","Gyro",colNames[i])
colNames[i]<-gsub("AccMag","AccMagnitude",colNames[i])
colNames[i]<-gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
colNames[i]<-gsub("JerkMag","JerkMagnitude",colNames[i])
colNames[i]<-gsub("GyroMag","GyroMagnitude",colNames[i])
colNames[i]<-gsub("^(f)","freq",colNames[i])
colNames[i]<-gsub("([Gg]ravity)","Gravity",colNames[i])
};
colnames(finalData)<-colNames;

# 5. Create  tidy data set with the average of each variable for each activity and each subject. 
FDataNoActivityType<- finalData[,names(finalData) != 'activityType'];# Create a new table, FDataNoActivityType without the activityType column
# Summarizing the FDataNoActivityType
tidyData<-aggregate(FDataNoActivityType[,names(FDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=FDataNoActivityType$activityId,subjectId = FDataNoActivityType$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);# Merging to include descriptive acitvity names
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');# Export the tidyData set 
