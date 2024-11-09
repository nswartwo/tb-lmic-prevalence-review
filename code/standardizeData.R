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
    colnames(cleanDF) <- gsub('\\.$', '', colnames(cleanDF), perl = TRUE)
    
    ### Identify abnormally long column names.
    ### Propose those over 30 characters.
    
    ### First check if we've already created short file names and saved them. 
    ### Define a boolean that will be updated if we need to enter names
    enterNames <- FALSE
    ### If stored names exist, ask the user if they want to use these names.
    if(file.exists("data/shortColumnNames.rds")){
        useStoredNames <- as.character(readline(prompt = "Use stored short column names? (Yes/No): "))
        if (useStoredNames == "Yes") {
            newColumnNames <- readRDS("data/shortColumnNames.rds")
    ### If the user does not wish to use the stored names, update the boolean and alert the user.
        } else if (useStoredNames == "No") {
            print("Entering interactive mode to replace stored short column names.")
            enterNames <- TRUE 
        } 
    ### If no stored names exist, update the boolean and alert the user.
    } else {
        print("No stored short column names. Entering interactive mode to enter new column names.")
        enterNames <- TRUE
    }
    
    ### If either of the enter names conditions are met, enter an interactive mode where 
    ### the user will enter new names. Might want to allow for different save names. 
    if (enterNames == TRUE){
        newColumnNames <- colnames(cleanDF)
        for (i in 1:length(colnames(cleanDF))){
            if (nchar(colnames(cleanDF)[i]) > 30){
                userApproved <- "No"
                while(userApproved == "No"){
                newColumnNames[i] <- as.character(readline(prompt = paste(colnames(cleanDF)[i], 
                                     "is too long. Please enter a new column name: ")))
                userApproved <- as.character(readline(prompt = paste("You entered: ", newColumnNames[i], 
                                        ". Are you happy with this name? (Yes/No)")))
                } 
            }
        }
        ### Save the new column names
        saveRDS(newColumnNames, file = "data/shortColumnNames.rds", version = 2)
    }
    
    ### Replace the column names
    colnames(cleanDF) <- newColumnNames
    
    ### Clean all responses for leading and trailing spaces
    cleanDF <- sapply(cleanDF, function(x) gsub("^\\s+.*|.*\\s+$", "", x))
    
    ### Remove all newline characters
    # cleanDF <- sapply(cleanDF, function(x) gsub("\n", "", x))
    
    ### Check free responses for opportunities of standardization 
    
    ### ### Cough of unknown duration ### ### 

    ### ### Author name ### ###
    ### Propose: Last name, Initial(s)
    ### (regular expression could be useful here: 
    ### remove all periods, add space, add comma where missing)
    # cleanDF$Corresponding.author <- gsub('\\.', '', cleanDF$Corresponding.author, perl = TRUE)
    # cleanDF$Corresponding.author <- gsub(",([A-Z])", ", \\1", cleanDF$Corresponding.author, perl = TRUE)
    # cleanDF$Corresponding.author <- gsub("(^[a-zA-Z]+) ([a-zA-Z]+$)", "\\1, \\2", cleanDF$Corresponding.author, perl = TRUE)
    # 
    ### ### Bias assessement ### ###
    ### Checking empty/invalid values - may need to be set as "Unknown" 
    # which(cleanDF[,(grep("bias", colnames(cleanDF))[1]):
    #          (grep("bias", colnames(cleanDF))[2])] != "Yes (low risk)")
    
    ### Identify erroneously empty fields
    print(paste0("Total number of missing fields: ", sum(is.na(cleanDF))))
    
    
    ### Create a list that contains the two dataframes:
    standardizedDataSummary <- list(errorDF, 
                                    cleanDF)
    return(standardizedDataSummary)
}
