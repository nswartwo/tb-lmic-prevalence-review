### This script contains a single function, logicalData() that takes in the
### dataset from validateData() and performs a number of data cleaning steps to ensure the 
### data are valid. It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

### to be called from within the cleanData() function. 


logicalData <- function(validData){
    ### Create two dataframes that will be updated at each cleaning step. 
    ### ### 1. Contains study ID, column name, and error. 
    errorDF <- data.frame("Study title" = NULL,
                          "Study ID" = NULL,
                          "Column name" = NULL,
                          "Error message" = NULL) 
    
    ### ### 2. Will contains the clean dataset.
    ### ###    Initialized with the standardizedData dataframe
    logicDF <- validData
    
    ###########################################################################
    ### Numerical checks ######################################################
    ###########################################################################
   
    ### Confirm publication year is greater than or equal to study year. 
    ### Requires the creation of variables study.start.year and study.end.year
    ### Initialize these new variables. 
    logicDF$study.start.year <- NA
    logicDF$study.end.year <- NA
    
    ### Split the string based on the "-" character 
    yearSplit <- strsplit(validData$study.years, "-")

    ### If length of the split is 1, set the start and end year to 
    ### the first sub-element.
    logicDF[which(sapply(yearSplit,length)==1), "study.start.year"] <-
    logicDF[which(sapply(yearSplit,length)==1), "study.end.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==1)],"[[",1))
    
    ### If length of the split is 2, set the start year to the first 
    ### sub-element and end year to second. 
    logicDF[which(sapply(yearSplit,length)==2), "study.start.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==2)],"[[",1))
    
    logicDF[which(sapply(yearSplit,length)==2), "study.end.year"] <-
        as.numeric(sapply(yearSplit[which(sapply(yearSplit,length)==2)],"[[",2))
    
    ### Check that length of that split is 1 or 2; if not add it to the errorDF. 
    ### Note, these will have NA value in the dataframe from the initialization. 
    errorDF <- data.frame("Study title" = validData$title.covidence[which(sapply(yearSplit,length) > 2)],
                          "Study ID" = validData$covidence.id[which(sapply(yearSplit,length) > 2)],
                          "Column name" = rep("study.years", length(which(sapply(yearSplit,length) > 2))),
                          "Error message" = rep("More than two years listed", length(which(sapply(yearSplit,length) > 2)))
                        ) ### close dataframe
                           
    ### Check that the publication year is greater than or equal to end year 
    which(logicDF$publication.year < logicDF$study.end.year)
    
    ### Add the errors to the errorDF 
    
    
    ### Check that target population > eligible population > participants for 
    ### each extracted stratum.
    
    ### Identify the relevant columns 
    eligibleIndex <- grep(pattern = "eligible.+total", x = colnames(validData))
    participantIndex <- grep(pattern = "participant.+total", x = colnames(validData))
    
    
    
    ### Check that presumptive TB >= symptoms and/or chest x-ray >= cases 
    ### (of various types).
   
    ### Stratified counts match total counts (with in each stratification).
    
    ### Sum of stratified counts should be within +/- 10% across all stratification
    ### (except HIV; see below).
    
    ###########################################################################
    ### Consistency checks ####################################################
    ########################################################################### 
}