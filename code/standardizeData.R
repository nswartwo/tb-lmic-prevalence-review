### This script contains a single function, standardData() that takes in the
### raw data and performs a number of data cleaning steps to standardize the 
### data. It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

standardData <- function(rawData){
    ### Create two dataframes that will be updated at each cleaning step. 
    ### ### 1. Contains study ID, column name, and error. 
    errorDF <- data.frame("Study ID" = NULL,
                          "Column name" = NULL, 
                          "Error message" = NULL)
    
    ### ### 2. Will contains the clean dataset.
    ### ###    Initialized with the rawData dataframe
    cleanDF <- rawData
    
    ### Clean the column names ###
    ### For standardization and efficiency, shortened column names will be 
    ### read in from the data/data_dictionary.csv file. 
    shortColumnNames <- read.csv("data/data_dictionary.csv")[,1:2]
    
    ### Check that the row count of column names equals the column count 
    ### of raw data. 
    if (nrow(shortColumnNames) != ncol(rawData)){
        stop("Length of imported column names does not match raw data.")
    } 
    
    colnames(cleanDF) <- shortColumnNames[,2]
    
    ### Clean responses ###
    
    ### Clean all responses for leading and trailing spaces
    cleanDF <- sapply(cleanDF, function(x) gsub("^\\s+.*|.*\\s+$", "", x))
    
    ### Remove all newline characters
    # cleanDF0 <- sapply(cleanDF, function(x) gsub("[\r\n]", "", x))
    
    ### Convert to a dataframe now that the column names are unique 
    ### and we are done with the sapply clean up 
    ### (returns a character matrix.)
    cleanDF <- as.data.frame(cleanDF)
    
    ### Check free responses for opportunities of standardization 
    ### ### Cough of unknown duration ### ### 

    ### ### Author name ### ###
    ### Propose: Last name, Initial(s)
    ### (regular expression could be useful here: 
    ### remove all periods, add space, add comma where missing)
    cleanDF$correspond.author <- gsub('\\.', '', cleanDF$correspond.author, perl = TRUE)
    cleanDF$correspond.author <- gsub(",([A-Z])", ", \\1", cleanDF$correspond.author, perl = TRUE)
    cleanDF$correspond.author <- gsub("(^[a-zA-Z]+) ([a-zA-Z]+$)", "\\1, \\2", cleanDF$correspond.author, perl = TRUE)
    
    ### ### Bias assessement ### ###
    ### Checking empty/invalid values - may need to be set as "Unknown" 
    # which(cleanDF[,(grep("study.quality", colnames(cleanDF))[1]):
    #          (grep("study.quality", colnames(cleanDF))[2])] != "Yes (low risk)")
    
    
    ### Identify erroneously empty fields
    print(paste0("Total number of missing fields: ", sum(is.na(cleanDF))))
    
    
    ### Create a list that contains the two dataframes:
    standardizedDataSummary <- list(errorDF, 
                                    cleanDF)
    return(standardizedDataSummary)
}
