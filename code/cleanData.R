### This script calls the related data cleaning functions in 
### series and creates a fully cleaned dataframe for analysis. 

cleanData <- function(){
    library(here)
    
    ### Read in the rawData
    rawData <- readRDS(here("data/fullDataRaw.rds"))
    
    ### Call in the standardizeData function
    source(here("code/standardizeData.R"))
    stndDF <- standardData(rawData)[[2]]
    
    ### Call in the validateData function
    source(here("code/validateData.R"))
    vldDF <- validateData(stndDF)[[2]]
    
    ### Call in logicalData function 
    source(here("code/logicalData.R"))
    ### Currently returns only an error matrix  
    lgcErrorDF <- logicalData(vldDF)
    
    ### Call in function to create new variables
    
    return(vldDF)
}
