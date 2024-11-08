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
    
    ### Use a regular expression to strip the X and numeric prefixes from the 
    ### column names. 
    colnames(cleanDF) <- gsub('[X](\\d+\\.+)+', '', colnames(cleanDF), perl = TRUE)
    
    ### Remove double periods
    colnames(cleanDF) <- gsub('(\\.\\.)', '.', colnames(cleanDF), perl = TRUE)
    
    ### Replace ".s" with simply "s"
    colnames(cleanDF) <- gsub('(\\.[s]\\.+)', 's.', colnames(cleanDF), perl = TRUE)

    ### Remove trailing periods
    colnames(cleanDF) <- gsub('\\.$', '', colnames(cleanDF)[1:10], perl = TRUE)
    
    ### Identify abnormally long column names.
    ### Propose those over 30 characters.
    for (i in 1:length(colnames)){
        if (nchar(colnames(cleanDF)) > 30){
            ### Decide if this should be interactive OR 
            ### if we should return these as another dataframe for edits. 
        }
    }
    
    ### Check free responses for opportunities of standardization 
    
    ### ### Cough of unknown duration ### ### 
    
    ### ### Author name ### ###
    ### Propose: Last name, Initial(s)
    ### (regular expression could be useful here: 
    ### remove trailing period, add space.)
    
    # cleanDF$Corresponding.author <- 
    
    ### ### Bias assessement ### ###
    ### Checking empty values - may need to be set as "Unknown" 
    cleanDF[,(grep("bias", colnames(cleanDF))[1]):
             (grep("bias", colnames(cleanDF))[2])] 
    
    ### Identify erroneously empty fields
    print(paste0("Number of missing fields: ", sum(is.na(cleanDF))))
    
    
    ### Create a list that contains the two dataframes:
    standardizedDataSummary <- list(errorDF, 
                                    cleanDF)
    return(standardizedDataSummary)
}