The data is gyroscopic and accelerometric data from partcicipants of a 
wearable-electronics study, who wore devices which measured rotational 
and accelerometric data as they did one of six activities: 
walking, walking downstairs, walking upstairs, standing, sitting, 
and laying down.

This data was sourced, at time of writing, from here: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Original variables may be found in the "features.txt" document therein.
Context for those variables may be found in the "features_info.txt" document.

The variables included in the final set are aggregates of this data.

Mean and standard deviation data was taken from the original data set (X in the files).
This data was merged with the Activity (y in the files) and Subject (subject in the files)
data to create a relational data set with Activity and Subject as primary keys.

The aggregates of this data were then compiled, grouped by both Subject and Activity,
into the final data set that is created by the scripts in this Git project.

If you wish to run the scripts, they require a similar hierarchy imposed
by the initial zip files. Test, train, and activity (UCI HAR Dataset) folders must contain 
their original files, names included, as these are taken as arguments.

The scripts are included in the run_analysis.R file, which you will have to download and source from the project.
Should you have any issue with that, or are particularly security concious, the scripts are also available
in the ReadMe.md file in this project.
 
In order to create the data, first gather your folder paths in Rstudio for the data 
as it exists in your folder hierarchy. Suppose these are the named variables 
testpath, trainpath, and activitypath.

Executing the following code will place the data into a data frame named aggData for your viewing:

aggData<-mergedata(testpath, trainpath, activitypath)

Otherwise, the files produced by the series of functions included, applied to the data above, are included
as mergedata.csv and aggregatedata.csv in the project. Mergedata is all of the test and train data merged,
and aggregatedata includes only the mean of the mean and standard deviation measurements included in that data set,
grouped as described above.