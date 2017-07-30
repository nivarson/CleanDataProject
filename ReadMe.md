There are three scripts included in the run_analysis.R file, as well as below.

The findVar function takes the filepath of the features.txt file,
searches it for all the mean and standard deviation variables, and 
returns the indices they appear at, for later use.

The mergeData function takes the folder path containing test data, 
training data, and activity variables as arguments. It merges that data,
making use of the findVar function to name the columns to select out of
and return only the mean and standard deviation data of interest.

The tidySummary data makes use of the mergeData function as a source
of data to then computer the means of, grouped by the first two variables.
It takes the same arguments as the mergeData function, and returns a
data frame of variable means grouped by the first two variables, generally. 
In terms of the repo this is included in, the means are grouped by Subject
then Activity, such that each subject reports the mean of data over each 
activity for every variable.

R Scripts

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
  
  for(i in 1:length(meanIndices)){ #interleaves the mean and corresponding sd for each measurement
    varIndices[j]<-meanIndices[i]
    j = j + 1
    varIndices[j]<-sdIndices[i]
    j = j + 1
    
    
  }
  
  varIndices
  
}

mergeData<-function(testfolder, trainfolder, activityfolder){
  
  #this function requires a particular folder structure implied by the
  #UCI Human Activitity Recognition data set. It takes the test data folder
  #path, training data folder path, and activity variable folder path as
  #arguments, and merges the data and creates factor variables/variable
  #names as necessary.
  
  
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
  
  train.df<-cbind(trainsubject.df, trainactive.df)#creates the train and test data frames with all subjects and activities added
  test.df<-cbind(testsubject.df, testactive.df)

  for (i in colIndices){ #adds all mean and standard deviation columns to train and test data frames
    train.df<-cbind(train.df, traindata.df[,i+2])
    test.df<-cbind(test.df, testdata.df[,i+2])
    
  }
  
  vars<-read.table(varNamesFile)#Creates new names for Subject and Activity columns
  names(train.df)<-c("Subject", "Activity", as.character(vars[[2]][colIndices])) 
  names(test.df)<-c("Subject", "Activity", as.character(vars[[2]][colIndices]))
  
  train.df[[1]]<-as.factor(train.df[[1]])#Creates factors
  test.df[[1]]<-as.factor(test.df[[1]])
  train.df[[2]]<-as.factor(train.df[[2]])
  test.df[[2]]<-as.factor(test.df[[2]])
  
  alldata<-rbind(train.df, test.df) #adds the training and test data sets together
  
  levels(alldata[[2]])[levels(alldata[[2]])==1]<- "Walking" #translates Activity numbers to named actions
  levels(alldata[[2]])[levels(alldata[[2]])==2]<- "Walking_Upstairs"
  levels(alldata[[2]])[levels(alldata[[2]])==3]<- "Walking_Downstairs"
  levels(alldata[[2]])[levels(alldata[[2]])==4]<- "Sitting"
  levels(alldata[[2]])[levels(alldata[[2]])==5]<- "Standing"
  levels(alldata[[2]])[levels(alldata[[2]])==6]<- "Laying"
  
  alldata

}
  
tidySummary<-function(testfolder, trainfolder, activityfolder){
  #this function returns a data frame that consists of the mean values
  #of merged data, renamed to indicate the means ofthe old variables. It is general enough
  #to accurately detect how many rows will be created when the merged data is aggregated
  #by the first and second columns
  
  alldata<-mergeData(testfolder, trainfolder, activityfolder) #pull the data
  aggvector<-1:(nlevels(alldata[1]) * nlevels(alldata[2]))#instantiate the data frames
  aggdata<-aggvector
  
  #loop through the data and create aggregate data frames, which are appended onto
  #the results vector aggdata, which is ultimately ordered by Subject and returned
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