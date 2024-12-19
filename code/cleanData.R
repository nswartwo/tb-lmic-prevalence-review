### This script calls the related data cleaning functions in 
### series and returns a list containing:
### 1. a dataframe of missings
### 2. a dataframe of errors 
### 3. a dataframe of cleaned data for analysis. 

cleanData <- function(){
    library(here)
    
    ### Create an error dataframe to populate throughout the steps
    errorList <- data.frame("Study title" = NULL,
                          "Study ID" = NULL,
                          "Column name" = NULL,
                          "Error message" = NULL) 
    
    ### Read in the rawData
    rawData <- readRDS(here("data/fullDataRaw.rds"))
    
    ### Call in the standardizeData function
    source(here("code/standardizeData.R"))
    stndList <- standardData(rawData)
    
    ### Call in the newVariables function
    source(here("code/createNewVariables.R"))
    newList <- newVariables(stndDF = stndList[["clean data"]], 
                          errorDF = errorDF)
    
    ### Call in the validateData function
    source(here("code/validateData.R"))
    vldList <- validateData(newVariablesData = newList[["clean data"]],
                          errorDF = newList[["errors"]])
    
    ### Call in logicalData function 
    source(here("code/logicalData.R"))
    lgcList <- logicalData(validData = vldList[["clean data"]], 
                            errorDF = vldList[["errors"]])

    ### Create a summary of all the cleaning and the "cleanest dataset".
    cleanDataSummary <- list("missings" = stndList[["missings"]], 
                             "errors" = lgcList[["errors"]], 
                             "clean data" = lgcList[["clean data"]])
    
    return(cleanDataSummary)
}
