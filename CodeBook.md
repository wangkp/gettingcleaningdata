# The CodeBook

The script merges many the different files,
into one single dataset as it has been stated in the exercise.
No other transformations than the ones done in the script has been made on the data. 
The input is described in the README.txt of the downloaded input (see "UCI HAR Dataset/README.txt" file once the input is downloaded).

The output represents the average of each variable for each activity and each subject. It is a table ; each variable is represented by a column, and each activity / subject is represented by a row.
The columns names are slightly modified from the "features.txt" input file content: all parenthesis are removed.
The row names merge the subject and the activity. For example, the row "1_LAYING" describes the means of the subject 1 for the LAYING activity.