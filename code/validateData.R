### This script contains a single function, validateData() that takes in the
### datafame produced by the standardizedData() function and performs a 
### number of data cleaning steps to validate the data. 
### It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

validateData <- function(standardizedData){
    ### Create two dataframes that will be updated at each cleaning step. 
    ### ### 1. Contains study ID, column name, and error. 
    errorDF <- data.frame("Study title" = NULL,
                          "Study ID" = NULL,
                          "Column name" = NULL,
                          "Error message" = NULL) 
    
    ### ### 2. Will contains the clean dataset.
    ### ###    Initialized with the standardizedData dataframe
    validDF <- standardizedData
    
    ### Check that the publication year is between  1993 - 2024
    if(any(na.omit(unique(as.numeric(validDF$publication.year))) < 1993)){
        print("At least one study published outside of 1993 - 2024.")
    } else {
        print("All studies published between 1993 and 2024.")
    }
    ### Because these are valid - chose not to do error handling beyond this.
    ### Could be more thorough if updating the dataset or adapting to a new one.
    
    ### Check that there is data for at least one stratification 
    stratIndex <- c(grep("report.",colnames(validDF)),
                    ### Add in where to end the last stratification
                    which(colnames(validDF) == "data.availability.comments")-1)
    
    ### Crude first check; should examine more closely to ensure the data 
    ### extacted is not just totals as there was confusion early in the 
    ### study. 
    
    if(any(rowSums(is.na(validDF[,stratIndex[1]:stratIndex[length(stratIndex)]])) < 
       ncol(validDF[,stratIndex[1]:stratIndex[length(stratIndex)]]))){
        print("At least one stratification extracted for all papers.")
    } else {
        print("No stratifications extracted.")
    }

    ### Confirm that Covidence ID is unique 
    if(length(unique(validDF$covidence.id)) == nrow(validDF)){
        print("Covidence IDs are unique.")
    } else{
        print("Covidence IDs are not unique.")
    }
    ### Because these are unique - chose not to do error handling beyond this.
    ### Could be more thorough if updating the dataset or adapting to a new one.
    
    ### Create a list that contains the two dataframes:
    validatedDataSummary <- list(errorDF, 
                                    validDF)
    
    return(validatedDataSummary)
}