### This script calls the related data cleaning functions in 
### series and returns a list containing:
### 1. a dataframe of missings
### 2. a dataframe of errors 
### 3. a dataframe of cleaned data for analysis. 

cleanData <- function(){
    library(here)
    
    ### Create an error dataframe to populate throughout the steps
    errorDF <- data.frame("Study title" = NULL,
                          "Study ID" = NULL,
                          "Column name" = NULL,
                          "Error message" = NULL) 
    
    ### Read in the rawData
    rawData <- readRDS(here("data/fullDataRaw.rds"))
    
    ### Call in the standardizeData function
    source(here("code/standardizeData.R"))
    stndDF <- standardData(rawData)
    
    ### Call in the newVariables function
    source(here("code/createNewVariables.R"))
    newDF <- newVariables(stndDF = stndDF[["clean data"]], 
                          errorDF = errorDF)
    
    ### Call in the validateData function
    source(here("code/validateData.R"))
    vldDF <- validateData(newVariablesData = newDF[["clean data"]],
                          errorDF = newDF[["errors"]])
    
    ### Call in logicalData function 
    source(here("code/logicalData.R"))
    lgcDF <- logicalData(validData = vldDF[["clean data"]], 
                         errorDF = vldDF[["errors"]])

    ### Create a summary of all the cleaning and the "cleanest dataset".
    cleanDataSummary <- list("missings" = stndDF[["missings"]], 
                             "errors" = lgcDF[["errors"]], 
                             "clean data" = lgcDF[["clean data"]])
    
    return(cleanDataSummary)
}
