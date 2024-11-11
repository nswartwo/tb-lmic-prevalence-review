### This script calls the related data cleaning functions in 
### series and creates a fully cleaned dataframe for analysis. 

cleanData <- function(){
    
    ### Read in the rawData
    rawData <- readRDS("data/fullDataRaw.rds")
    
    ### Call in the standardizeData function
    source("code/standardizeData.R")
    stndDF <- standardData(rawData)[[2]]
    
    ### Call in the validateData function
    source("code/validateData.R")
    vldDF <- validateData(stndDF)[[2]]
    
    ### Call in logicalData function 
    
    ### Call in function to create new variables
    
    return(vldDF)
}