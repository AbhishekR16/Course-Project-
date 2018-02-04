##reading and combining train set

x_train<-read.table("train/x_train.txt")
y_train<-read.table("train/y_train.txt")
subject_train<-read.table("train/subject_train.txt")
names(x_train)<-fea
names(y_train)<-"activity"
names(subject_train)<-"subject"
train<-cbind(subject_train,y_train,x_train)
dim(train)

#reading and combinig test set
x_test<-read.table("test/x_test.txt")
y_test<-read.table("test/y_test.txt")
subject_test<-read.table("test/subject_test.txt")
names(x_test)<-fea
names(y_test)<-"activity"
names(subject_test)<-"subject"
test<-cbind(subject_test,y_test,x_test)
dim(test)

##MERGING of test set and train set
testtrain<-rbind(test,train)
dim(testtrain)
str(testtrain)

## selecting those columns with measurement of means and standard deviation

features<-read.table("features.txt")
fea<-as.character(features$V2)
fea<-tolower(fea)
ind1<-grep("std",fea)+2
ind2<-grep("mean",fea)+2
tt1<-testtrain[,ind1]
tt2<-testtrain[,ind2]
testtrain<-cbind(testtrain[1:2],tt1,tt2)

#Labeling activities with discriptive names

act<-read.table("activity_labels.txt")
activ<-as.character(act$V2)
temp<-as.factor(testtrain[,2])
testtrain$activity<-factor(temp,labels = activ)

#Labeling columns with discriptive names

f1<-fea[ind1-2]
f2<-fea[ind2-2]
f3<-c(f1,f2)
f4<-c("subject","activity",f3)
names(testtrain)<-f4

#average of each variable in an independent data

library(dplyr)
finaldf <- testtrain %>%
            group_by(subject,activity) %>% arrange(subject,activity) %>% 
            summarize_all(funs(mean))
#Writing data in a text file
write.table(finaldf,"finaldata.txt",row.names = FALSE)
