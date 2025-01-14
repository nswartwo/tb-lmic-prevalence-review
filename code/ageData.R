ageData <- function(stndData){
    
    ##########################################################################|
    #####                               Setup                             #####
    ##########################################################################|
    
    ### load the required libraries     
    library(here)
    library(tidyverse)
    library(janitor)
    library(dplyr)
    
    ### Select out the variables we need to sort
    ### Run the source files first to get the stndData
    
    age_data <- stndData %>% 
        dplyr:: select( covidence.id,
                        report.age.grp:adj.prev100k.ci.prevalent.tb.age.grp.total)
    
    # Define standard age group labels that need to be filled in
    standard_age_groups <- tribble(
        ~age.grp.1.range, ~age.grp.2.range, ~age.grp.3.range, ~age.grp.4.range,
        ~age.grp.5.range, ~age.grp.6.range, ~age.grp.7.range, ~age.grp.total.range, 
        "0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+", "total") %>%
        pivot_longer(everything())
    
    # Filter down to get the records where all age.grp records are mising
    missing_age_groups <- age_data %>%
        dplyr::select(covidence.id, starts_with("age.grp.") & ends_with("range")) %>%
        filter(if_all(starts_with("age.grp.") & ends_with("range"), is.na))  # Check if all are NA
    
    # Join on the standard age.grp labels
    # This creates a definition for the standard age groups in the dataset so they
    # no longer need to be assumed.
    
    age_filled <- missing_age_groups %>%
        pivot_longer(-covidence.id) %>%
        dplyr::select(-value) %>%
        left_join(standard_age_groups) %>%
        pivot_wider()
    
    #now replace the correct cells in `age_data` with these updated age groups
    #Update missing age labels in the main dataset
    age_data_updated <- age_data %>%
        left_join(age_filled, by = "covidence.id", suffix = c("", ".updated")) %>%
        mutate(
            across(
                starts_with("age.grp.") & ends_with("range"), 
                ~ coalesce(.x, get(paste0(cur_column(), ".updated")))
            )
        ) %>%
        dplyr::select(-ends_with(".updated"))
    
    #age.grp.total.range columns should always be "total"
    age_data_updated <- age_data_updated %>%
        mutate(age.grp.total.range = "total")
    
    
    
    ###########################################################################
    ### Make the 0-14 age group "age.grp.0". It will be dropped from most analyses
    ### Decided to take this approach because we have at least
    ### one study that has values for all 7 age groups starting at 15+. 
    
    ### for human speed efficiency, we will just brute force this process
    unique(age_data_updated$age.grp.1.range)
    
    indexAge0 <- which(age_data_updated$age.grp.1.range %in% c("0-14", "10-14", "0-5", "<15", "12"))
    
    tmp <- age_data_updated[indexAge0,] 

    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.1", "age.grp.0", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.2", "age.grp.1", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.3", "age.grp.2", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.4", "age.grp.3", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.5", "age.grp.4", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.6", "age.grp.5", x, perl = TRUE))
    colnames(tmp) <- sapply(colnames(tmp), function(x) gsub("age.grp.7", "age.grp.6", x, perl = TRUE))
    
    age_data_updated2<-age_data_updated
    age_data_updated3<-age_data_updated
    
    age_data_updated2 <- right_join(age_data_updated, tmp)
    # View(age_data_updated2 %>% select(contains("range")))

    ### How to handle the remaining ones 
    
    age_data_updated3[,setdiff(colnames(age_data_updated2), colnames(age_data_updated3))] <- NA
    age_data_updated3[indexAge0,] <- age_data_updated2        

    ### Sort so the age.grp.0 is at the beginning. 
    tmp2 <- age_data_updated3[,-c(1,2)]
    
    tmp3 <- tmp2[,sort(colnames(tmp2))]
    age_data_updated <- cbind(age_data_updated3[,c(1,2)], tmp3) 
    
    ### Need to reorder these to match others but later.
    
    ### Clean up some stray labels
    age_data_updated <- as.data.frame(sapply(age_data_updated, function(x) gsub(" or above", "+", x, perl = TRUE)))
    age_data_updated <- as.data.frame(sapply(age_data_updated, function(x) gsub(">65", "65+", x, perl = TRUE)))
    age_data_updated <- as.data.frame(sapply(age_data_updated, function(x) gsub(">45", "45+", x, perl = TRUE)))

    age_data_updated <- as.data.frame(sapply(age_data_updated, function(x) gsub("<15", "0-14", x, perl = TRUE)))
    
    
    ### How to handle premature totals? 
    
    ### subset to the range definitions and remove totals
    tmp4 <- (age_data_updated %>% dplyr::select(contains("range")))[,-9]
    
    #### these are the location of the issues
    whereTotal <- which(tmp4 == "Total", arr.ind = TRUE)
    
    ### Subset the dataframe 
    age_data_totals <- age_data_updated[whereTotal[1:4,1],]
    
    ### Move the total entries to the total columns
    
    ### For the first three the entries are in age.grp.5

    age_data_totals[1:3, grep(pattern = "age.grp.total", x = colnames(age_data_totals))] <- 
    age_data_totals[1:3, grep(pattern = "age.grp.5", x = colnames(age_data_totals))] 

    ### Set the age group 5 entries to NA
    age_data_totals[1:3, grep(pattern = "age.grp.5", x = colnames(age_data_totals))] <- NA
    
    ### For the last entry is in age.grp.6
    
    age_data_totals[4, grep(pattern = "age.grp.total", x = colnames(age_data_totals))] <- 
        age_data_totals[4, grep(pattern = "age.grp.6", x = colnames(age_data_totals))] 
    
    ### Set the age group 6 entries to NA
    age_data_totals[4, grep(pattern = "age.grp.6", x = colnames(age_data_totals))] <- NA

    
    ### Double check that total range is always "total" 
    
    age_data_totals$age.grp.total.range <- "total"

    age_data_updated[whereTotal[1:4,1],] <- age_data_totals
    
    ### Make sure these are numeric 
    age_data_updated[,grep("n\\.|prev100k.[^c]", colnames(age_data_updated))] <- 
        as.numeric(as.matrix(age_data_updated[,grep("n\\.|prev100k.[^c]", colnames(age_data_updated))]))

    return(age_data_updated)

}    