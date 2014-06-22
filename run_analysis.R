# Definition of helping functions

# Loads Y_Train data and returns a dataframe to the main function
# This function renames the numbered activities (1 to 6) with the proper names

loadYTrain = function() {
    setwd("./train")
    
    activityList = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    
    y_TrainFile = read.table("y_train.txt", stringsAsFactors=FALSE, header=FALSE)
    colnames(y_TrainFile)[1] = "Activity"
    
    for (index in 1:6) {
        y_TrainFile$Activity[y_TrainFile$Activity == index] = activityList[index]
    }
    
    setwd("../")
    return(y_TrainFile)
}


# Loads Y_Test data and returns a dataframe to the main function
# This function renames the numbered activities (1 to 6) with the proper names

loadYTest = function() {
    setwd("./test")
    
    activityList = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    
    y_TestFile = read.table("y_test.txt", stringsAsFactors=FALSE, header=FALSE)
    colnames(y_TestFile)[1] = "Activity"
    
    for (index in 1:6) {
        y_TestFile$Activity[y_TestFile$Activity == index] = activityList[index]
    }
    
    setwd("../")
    return(y_TestFile)
}


# Loads X_Train data and returns a dataframe to the main function
# The returned dataframe has only the columns with mean() and std() strings in their names

loadXTrain = function(column_names, column_index) {
    
    setwd("./train")

    x_TrainFile = read.table("x_train.txt", stringsAsFactors=FALSE, header=FALSE)
    x_TrainFile = x_TrainFile[,column_index]

    for (index in 1:length(column_index)) {
        colnames(x_TrainFile)[index] = column_names[index]
    }
    
    setwd("../")
    return(x_TrainFile)
}


# Loads X_Test data and returns a dataframe to the main function
# The returned dataframe has only the columns with mean() and std() strings in their names

loadXTest = function(column_names, column_index) {
    
    setwd("./test")
    
    x_TestFile = read.table("x_test.txt", stringsAsFactors=FALSE, header=FALSE)
    x_TestFile = x_TestFile[,column_index]
    
    for (index in 1:length(column_index)) {
        colnames(x_TestFile)[index] = column_names[index]
    }
    
    setwd("../")
    return(x_TestFile)
}

# Main function
# Gets the names of columns from the features.txt file and creates a vector by selecting
# only the names that have std() and mean() strings in it. The vector is passed to the functions
# that load X_train and X_test data

main = function() {

    Features = read.table("features.txt", stringsAsFactors=FALSE, header=FALSE)
    
    column_index = sort(c(grep("-mean()",Features$V2, fixed=TRUE),
                          grep("-std()",Features$V2, fixed=TRUE)))
    
    column_names = Features$V2[column_index]
    
    column_names = gsub("()", "",column_names, fixed=TRUE)
    column_names = gsub("-", "_",column_names, fixed=TRUE)
    
    
    # Combines Y_Train and X_Train data into a single dataframe
    Train_DF = cbind( loadYTrain(), loadXTrain(column_names, column_index) )
    
    # Combines Y_Test and X_Test data into a single dataframe
    Test_DF = cbind( loadYTest(), loadXTest(column_names, column_index) )
    
    # Both train and test data are combined into a single dataframe
    base_DF = rbind(Train_DF, Test_DF)
    
    rm(list=c("Features", "Train_DF", "Test_DF"))
    
    
    # Assembles the query to get the data tidy data with averages of the columns with mean and std
    # values, grouped by the Activity column
    
    query = "select Activity, "
    
    for(field in column_names) {
        query =  paste(query, "avg(", field, "), ", sep="")
    }
    
    query = substr(query, 1, nchar(query)-2)
    
    query = paste(query, " from base_DF group by Activity order by Activity", sep="")
    
    # Runs the query using SQL and creates a csv file as a result
    library(sqldf)
    query_result = sqldf(query)
    write.csv(query_result, file="query_result.csv", row.names=FALSE)
}

# Execution of the program
main()
