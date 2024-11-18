### This script contains a single function, standardData() that takes in the
### raw data and performs a number of data cleaning steps to standardize the 
### data. It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

standardData <- function(rawData){
    library(here)
    library(tidyverse)
    ### Create two dataframes that will be updated at each cleaning step. 
    ### ### 1. Contains study ID, column name, and error. 
    missingDF <- data.frame("Study title" = NULL,
                            "Study ID" = NULL,
                            "Column name" = NULL) 
    
    ### ### 2. Will contains the clean dataset.
    ### ###    Initialized with the rawData dataframe
    cleanDF <- rawData
    
    ### Clean the column names ###
    ### For standardization and efficiency, shortened column names will be 
    ### read in from the data/data_dictionary.csv file. 
    dict <- read.csv(here("data/data_dictionary.csv"))[,1:3]
    
    ### Check that the row count of column names equals the column count 
    ### of raw data. 
    if (nrow(dict) != ncol(rawData)){
        stop("Length of imported column names does not match raw data.")
    } 
    
    colnames(cleanDF) <- dict[,"Short.column.name"]
    
    ### Clean responses ###
    
    ### Clean all responses for leading and trailing spaces
    cleanDF <- sapply(cleanDF, function(x) gsub("^\\s+.*|.*\\s+$", "", x))
    
    ### Remove all newline characters from titles so we can match
    cleanDF[,"title.extracted"] <- gsub("[\r\n]", "", cleanDF[,"title.extracted"])

    ### Convert to a dataframe now that the column names are unique 
    ### and we are done with the sapply clean up 
    ### (returns a character matrix.)
    cleanDF <- as.data.frame(cleanDF)
    
    ### Check free responses for opportunities of standardization 
    ### ### Cough of unknown duration ### ### 
    
    
    ### Correct cough values will always be at the front of the string and 
    ### capitalized. Look for instances of "Cough" or "cough" later in the 
    ### string. 
    
    # unique(cleanDF$screening.symptoms[grep(".+'Cough'", cleanDF$screening.symptoms, perl=TRUE)])
    
    ### ### Study title ### ###
    ### Make all entries sentence case
    cleanDF$title.extracted <- gsub("\\b([[:alpha:]])([[:alpha:]]+)", 
                                    "\\U\\1\\L\\2",
                               cleanDF$title.extracted, perl=TRUE)
    
    ### ### Author name ### ###
    ### Propose: Last name, Initial(s)
    ### (regular expression could be useful here: 
    ### remove all periods, add space, add comma where missing)
    cleanDF$correspond.author <- gsub('\\.', '', cleanDF$correspond.author, perl = TRUE)
    cleanDF$correspond.author <- gsub(",([A-Z])", ", \\1", cleanDF$correspond.author, perl = TRUE)
    cleanDF$correspond.author <- gsub("(^[a-zA-Z]+) ([a-zA-Z]+$)", "\\1, \\2", cleanDF$correspond.author, perl = TRUE)
    
    ### Remove the columns that Covidence inserted for each stratification
    ### These column names start with "results."
    cleanDF <- cleanDF[,-grep("results.",colnames(cleanDF))]
    
    ### Identify erroneously empty fields
    ### These will be empty strings, not "N/A" so "is.na()" won't help
    
    ### We expect a number of empty fields in the results when a specific
    ### stratification was not included in a publication. Find these and 
    ### set those to N/A as they are "true missings".
    
    ### Identify columns that delineate the beginning of a stratification
    ### These column names start with "report."
    stratIndex <- c(grep("report.",colnames(cleanDF)),
                    ### Add in where to end the last stratification
                    which(colnames(cleanDF) == "data.availability.comments")-1)

    ### If the "report." variable for a specific stratification is "No"
    ### Then the empty values are truly missing and can be sent to "N/A"
    for(strat in 1:(length(stratIndex)-1)){
        indexRange <- stratIndex[strat]:stratIndex[strat+1]
        cleanDF[which(cleanDF[indexRange[1]]=="No"), indexRange] <- 
            sapply(cleanDF[which(cleanDF[indexRange[1]]=="No"), indexRange], 
               function(x) gsub("^$",NA, x, perl = TRUE))   
    }

    ### Additional comment fields were also optional so set those 
    ### to NA if missing
    commIndex <- c(grep("comment",colnames(cleanDF)))
    cleanDF[, commIndex] <- sapply(cleanDF[, commIndex], function(x) gsub("^$",NA, x, perl = TRUE)) 
    
    
    ### Now that we've removed the expectant missings, we can examine the
    ### remaining missings that need to be addressed. 
    nMissing <- sum(sapply(cleanDF, function(x) grepl("^$", x, perl = TRUE)))
    print(paste0("Total number of missing fields: ", 
                 nMissing))
    
    ### Find the missings 
    missList <-sapply(cleanDF, function(x) grep("^$", x, perl = TRUE))
    missList <- missList[lengths(missList) > 0]
    
    ### Convert to a dataframe
    for (col in 1:length(missList)){
        tempDF <- data.frame("Study title" = cleanDF[missList[[col]], "title.extracted"], 
                             "Study ID" = cleanDF[missList[[col]], "covidence.id"], 
                             "Column name" = names(missList)[col])
        missingDF <- rbind(missingDF, tempDF)
    }
    
    ### Reorder by title
    missingDF <- missingDF[order(missingDF$Study.title),]

    ### Save missings dataframe as a csv
    write.csv(missingDF, here("data/missingDataToCheck.csv"), row.names = FALSE)
    
    ### Convert column types to correct format
    type_mapping <- setNames(dict$Type, dict$`Short column name`)
    type_mapping <- type_mapping[names(type_mapping) %in% names(cleanDF)]
    
    ### Function to safely convert columns
    convert_column <- function(column, type) {
        if (type == "numeric") {
            suppressWarnings(parse_number(column))
        } else if (type == "character") {
            parse_character(column)
        } else if (type == "logical") {
            parse_logical(column)
        } else if (type == "integer") {
            suppressWarnings(parse_integer(column))
        } else if (type == "factor") {
            parse_factor(column)
        } else {
            column # Leave unchanged if the type isn't specified
        }
    }
    
    #### Apply the conversion based on type mapping
    cleanDF <- cleanDF %>%
        mutate(across(
            all_of(names(type_mapping)), 
            ~ convert_column(.x, type_mapping[cur_column()])
        ))
    
    ### Create a list that contains the two dataframes:
    ### Name the list to make extraction easier (e.g. out$cleanDF now accessible)
    standardizedDataSummary <- list(missDF = missingDF, 
                                    cleanDF = cleanDF)
    return(standardizedDataSummary)
}
