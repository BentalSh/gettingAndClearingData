library("dplyr")

#read the files
Xtest<-read.delim2("test/X_test.txt",sep="",header=FALSE,stringsAsFactors=FALSE);
Xtrain<-read.delim2("train/X_train.txt",sep="",header=FALSE,stringsAsFactors=FALSE);
features<-read.delim2("features.txt",sep="",header=FALSE,stringsAsFactors=FALSE);
Ytest<-read.delim2("test/y_test.txt",sep="",header=FALSE,stringsAsFactors=FALSE);
Ytrain<-read.delim2("train/y_train.txt",sep="",header=FALSE,stringsAsFactors=FALSE);

Subtest<-read.delim2("test/subject_test.txt",sep="",header=FALSE,stringsAsFactors=FALSE);
Subtrain<-read.delim2("train/subject_train.txt",sep="",header=FALSE,stringsAsFactors=FALSE);

#make valid feature names
temp<-gsub("\\(\\)","",features[,2])
temp2<-gsub("\\(",".",temp)
temp3<-gsub("\\,",".",temp2)
temp4<-gsub("\\)","",temp3)
finalFeatures<-gsub("\\-",".",temp4)

#fix Xtest and Xtrain colnames
colnames(Xtest)<-finalFeatures;
colnames(Xtrain)<-finalFeatures;

#remove duplicates and non required columns
Xtest<-Xtest[,!duplicated(finalFeatures) & (grepl("mean",finalFeatures) | grepl("std",finalFeatures))]
Xtrain<-Xtrain[,!duplicated(finalFeatures) & (grepl("mean",finalFeatures) | grepl("std",finalFeatures))]

#prepare labels
testTrainLabel<-c(rep("test",nrow(Xtest)),rep("train",nrow(Xtrain)))
#change them to numbers
Xtest<-lapply(Xtest,as.numeric);
Xtrain<-lapply(Xtrain,as.numeric);

#prepare other data
colnames(Ytest)<-"labels";
colnames(Ytrain)<-"labels";

colnames(Subtest)<-"subject";
colnames(Subtrain)<-"subject";


#merge the data
mergedTest<-cbind(Xtest,Ytest,Subtest);
mergedTrain<-cbind(Xtrain,Ytrain,Subtrain);
merged<-rbind(mergedTest,mergedTrain);

data<-mutate(merged,features.set=testTrainLabel);

#replace labels with their names
labels<-read.delim2("activity_labels.txt",sep="",header=FALSE);
data$labels=labels[data$labels,2]


tidyData<- (select(data,-features.set)%>%group_by(labels,subject) %>% summarise_each(funs(mean(., na.rm = TRUE))))
