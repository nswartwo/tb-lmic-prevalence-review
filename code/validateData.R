### This script contains a single function, validateData() that takes in the
### datafame produced by the newVariables() function and performs a 
### number of data cleaning steps to validate the data. 
### It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

validateData <- function(newVariablesData,
                         errorDF){
    ### Create a dataframes that will be updated at each cleaning step. 
    ### ### Contains the clean dataset.
    ### ### Initialized with the newVariablesData dataframe
    validDF <- newVariablesData
    
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
    ### extracted is not just totals as there was confusion early in the 
    ### study. 
    
    # if(any(rowSums(is.na(validDF[,stratIndex[1]:stratIndex[length(stratIndex)]])) < 
    #    ncol(validDF[,stratIndex[1]:stratIndex[length(stratIndex)]]))){
    #     print("At least one stratification extracted for all papers.")
    # } else {
    #     print("No stratifications extracted.")
    # }
    
    # if(validDF[,paste()])
    stratCount <- 0 
    for(strat in strats){
        index <-  which(validDF[,paste0("report.", strat)] == "Yes" &
                  ### Check for stratified prevalence 100k 
                   (validDF[,paste0("prev100k.bacteriological.tb.", strat, ".total")] | 
                   validDF[,paste0("prev100k.smear.positive.tb.", strat, ".total")] |
                   validDF[,paste0("prev100k.prevalent.tb.", strat, ".total")] |
                   ### Check for adjusted stratfied prevalence 100k
                   validDF[,paste0("adj.prev100k.bacteriological.tb.", strat, ".total")] | 
                   validDF[,paste0("adj.prev100k.smear.positive.tb.", strat, ".total")] |
                   validDF[,paste0("adj.prev100k.prevalent.tb.", strat, ".total")] |
                   ### Check for tb counts 
                   validDF[,paste0("n.bacteriological.tb.", strat, ".total")] | 
                   validDF[,paste0("n.smear.positive.tb.", strat, ".total")] |
                   validDF[,paste0("n.prevalent.tb.", strat, ".total")]) != TRUE)

        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("report.", strat), 
                                                            length(index)),
                                        "Error message" = rep(paste("Selected that", strat, "stratified results reported, but none extracted."), 
                                                              length(index))))
        } else {
            stratCount <- stratCount + 1
        }
        
        index <- NA
        
    }
    
    if(stratCount == 5){
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
    validatedDataSummary <- list("clean data" = validDF, 
                                 "errors" = errorDF)
    
    return(validatedDataSummary)
}