findVar<- function(filename) {
  ##takes a file of variable names (assuming the list starts at 0)
  ##reads in the file and searches through it for instances of std() and mean()
  ##returns a vector of the indices for the variables which correspond to a mean
  ##and std deviation, ordering them such that the mean and sd for a variable are
  ##next to each other
  
  vars<-read.table(filename)
  intNumVars<- length(vars)
  
  meanIndices<-grep("mean()",vars[[2]], fixed=TRUE)
  sdIndices<-grep("std()", vars[[2]], fixed=TRUE)
  varIndices<-1:(length(meanIndices)* 2)
  j=1
  
  for(i in 1:length(meanIndices)){
    varIndices[j]<-meanIndices[i]
    j = j + 1
    varIndices[j]<-sdIndices[i]
    j = j + 1
    
    
  }
  
  varIndices
  
}

mergeData<-function(testfolder, trainfolder, activityfolder){
  
  activitypath<-paste(activityfolder, "/activity_label.txt", sep="")
  varNamesFile<-paste(activityfolder, "/features.txt", sep="")
  
  testdatapath<- paste(testfolder, "/X_test.txt", sep="")
  testactivepath<- paste(testfolder, "/y_test.txt", sep="")
  testsubjectpath<- paste(testfolder, "/subject_test.txt", sep="")
  
  traindatapath<- paste(trainfolder, "/X_train.txt", sep="")
  trainactivepath<- paste(trainfolder, "/y_train.txt", sep="")
  trainsubjectpath<- paste(trainfolder, "/subject_train.txt", sep="")
  
  testdata.df<-read.table(testdatapath)
  testactive.df<-read.table(testactivepath)
  testsubject.df<-read.table(testsubjectpath)
  
  traindata.df<-read.table(traindatapath)
  trainactive.df<-read.table(trainactivepath)
  trainsubject.df<-read.table(trainsubjectpath)
  
  colIndices<-findVar(varNamesFile)
  
  train.df<-cbind(trainsubject.df, trainactive.df)
  test.df<-cbind(testsubject.df, testactive.df)

  for (i in colIndices){
    train.df<-cbind(train.df, traindata.df[,i+2])
    test.df<-cbind(test.df, testdata.df[,i+2])
    
  }
  
  vars<-read.table(varNamesFile)
  names(train.df)<-c("Subject", "Activity", as.character(vars[[2]][colIndices])) 
  names(test.df)<-c("Subject", "Activity", as.character(vars[[2]][colIndices]))
  
  train.df[[1]]<-as.factor(train.df[[1]])
  test.df[[1]]<-as.factor(test.df[[1]])
  train.df[[2]]<-as.factor(train.df[[2]])
  test.df[[2]]<-as.factor(test.df[[2]])
  
  alldata<-rbind(train.df, test.df)
  
  levels(alldata[[2]])[levels(alldata[[2]])==1]<- "Walking"
  levels(alldata[[2]])[levels(alldata[[2]])==2]<- "Walking_Upstairs"
  levels(alldata[[2]])[levels(alldata[[2]])==3]<- "Walking_Downstairs"
  levels(alldata[[2]])[levels(alldata[[2]])==4]<- "Sitting"
  levels(alldata[[2]])[levels(alldata[[2]])==5]<- "Standing"
  levels(alldata[[2]])[levels(alldata[[2]])==6]<- "Laying"
  
  alldata

}
  
tidySummary<-function(testfolder, trainfolder, activityfolder){
  
  alldata<-mergeData(testfolder, trainfolder, activityfolder)
  aggvector<-1:(nlevels(alldata[1]) * nlevels(alldata[2]))
  aggdata<-aggvector
  
  for (i in 3:length(alldata)){
    if (i==3){
      aggvector<-setNames(aggregate(alldata[[i]]~Subject + Activity, alldata, mean), c("Subject", "Activity", paste("Mean -",names(alldata[i]))))
      aggdata<-aggvector
    }
    else if (i%%2==0){
      aggvector<-setNames(aggregate(alldata[[i]]~Subject + Activity, alldata, sd), c("Subject", "Activity", paste("Mean -",names(alldata[i]))))
      aggdata<-cbind(aggdata, aggvector[3])
      names(aggdata[[i]])<-paste("Mean -",names(aggvector[3]))
      
    }else {
      aggvector<-setNames(aggregate(alldata[[i]]~Subject + Activity, alldata, mean), c("Subject", "Activity", paste("Mean -",names(alldata[i]))))
      aggdata<-cbind(aggdata, aggvector[3])
      names(aggdata[[i]])<-paste("Mean -",names(aggvector[3]))
    }
    
    
  }
  
  aggdata<-aggdata[order(as.numeric(as.character(aggdata[[1]]))),]
  
  aggdata

}