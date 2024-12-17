### This script contains a single function, newVariables() that takes in the
### datafame produced by the standardizedData() function and performs two tasks:
### 1. Splits CIs into two variables 
### 2. Turns symptoms into booleans. 
### It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

newVariables <- function(stndDF,
                         errorDF){
    ### load the required libraries 
    library(tidyverse)
    library(here)
    library(janitor)
    
    ##########################################################################|
    #####           Separate confidence intervals out into columns        #####
    ##########################################################################|
    ### We will use consistence naming throughout for all variables
    ### First make a dataset containing only those variables with want to 
    ### separate out, with the covidence ID.
    confvars <- stndDF %>%
                dplyr::select(covidence.id,
                contains(".ci."))
    
    ### Check that we only have one entry for each CI 
    ### We can use the "-" to identify unique intervals and pull the index that matches. 
    index <- which(rowSums(apply(confvars, 1:2, stringr::str_count, "-") > 1, na.rm = TRUE) > 0)

    ### Add to the errorDF 
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = stndDF$title.covidence[index],
                                    "Study ID" = stndDF$covidence.id[index],
                                    "Column name" = rep(".ci.", 
                                                        length(index)),
                                    "Error message" = rep("multiple extracted values in CIs", 
                                                          length(index))))
        ### Remove this for now 
        confvars <- confvars[,-index]
    }
    
    ### Now separate the confidence intervals into `.lower`, and `.higher`
    # Identify all columns containing ".CI."
    ci_cols <- names(confvars)[str_detect(names(confvars), "\\.ci\\.")]
    
    # Use purrr::reduce() to iteratively apply separate()
    confvars_cleaned <- reduce(
        ci_cols,
        ~ .x %>%
            separate(
                col = .y,
                into = c(
                    str_replace(.y, "\\.ci\\.", ".ci.lower."),
                    str_replace(.y, "\\.ci\\.", ".ci.upper.")
                ),
                sep = "-"
            ),
        .init = confvars
    )
    
    ### Convert all confidence interval columns to numeric
    confvars_cleaned <- confvars_cleaned %>%
        mutate(across(contains(".ci."), as.numeric))
    
    
    ### Now join back onto the main dataset
    cleanDF <- left_join(stndDF, confvars_cleaned)
    
    ##########################################################################|
    #####                        TB symptom variables                     #####
    ##########################################################################|
    
    ### Now we will separate out the TB symptom variables https://github.com/nswartwo/tb-lmic-prevalence-review/issues/9
    ### Select out the columns that we need. 
    
    symptomvars <- cleanDF %>%
        dplyr::select(covidence.id, screening.symptoms)
    
    ### Separate the rows out into multiple columns
    symptomvars_clean <- symptomvars %>%
        # Separate rows by symptoms
        separate_rows(screening.symptoms, sep = ";") %>%
        # Clean up whitespace and standardize text
        mutate(screening.symptoms = str_trim(screening.symptoms)) %>%
        # Create a column for "Other" symptoms and separate them from the rest
        mutate(
            other_symptom = if_else(str_starts(screening.symptoms, "Other:"), screening.symptoms, NA_character_),
            screening.symptoms = if_else(!str_starts(screening.symptoms, "Other:"), screening.symptoms, NA_character_)
        ) %>%
        # Combine "Other" symptoms into a single character column
        group_by(covidence.id) %>%
        summarise(
            other_symptom = paste(na.omit(other_symptom), collapse = "; "),
            screening.symptoms = list(na.omit(screening.symptoms)),
            .groups = "drop"
        ) %>%
        # Flatten symptom lists and unnest
        unnest_longer(screening.symptoms) %>%
        mutate(present = 1) %>%
        pivot_wider(names_from = screening.symptoms, values_from = present, values_fill = 0) %>%
        #reorder the columns for ease of use
        dplyr::select(covidence.id, `Symptom screen was not performed`, `Cough ≥ 2 weeks`, `Cough ≥ 3 weeks`, `Cough of other or unknown duration`,
               Haemoptysis, `Chest pain`, Fever, `Night sweats`, `Weight loss`, `Sputum production`, `other_symptom`) %>%
        #now sort out column names to standard format
        rename_with(str_to_lower) %>% 
        rename_with(~ gsub(" ",".", .x), contains(" ")) %>% 
        rename_with(~ gsub("_",".", .x), contains("_")) %>%
        #remove the ≥ symbols from variable names
        rename_with(~ gsub("≥", "greater.or.equal.to", .x), contains("≥")) %>%
        #remove unnecessary text from `other.symptom` column 
        mutate(other.symptom = str_remove(other.symptom, "Other: ")) %>%
        #convert boolean screening variables to yes/no factors
        mutate(across(c(symptom.screen.was.not.performed:sputum.production), ~
                          case_when(.==0 ~ "No",
                                    .==1 ~ "Yes")))
    
    ### Other symptoms column. Here we will create new boolean variables for other screening criteria.
    # symptomvars_clean %>%
    #     tabyl(other.symptom)
    
    ### Record to boolean values 
    symptomvars_clean <- symptomvars_clean %>%
        mutate(breathlessness = case_when(
            grepl("Breath", other.symptom, ignore.case=TRUE) ~ "Yes",
            TRUE ~ "No")) %>%
        mutate(previous.current.tb.treatment = case_when(
            grepl("treat|ATT", other.symptom, ignore.case=TRUE) ~ "Yes",
            TRUE ~ "No"))
    
    ### Sort out a few where other cough durations are indicated in `other.symptoms`, but 
    ### `cough.of.other.or.unknown.duration` is currently `no`
    
    symptomvars_clean <- symptomvars_clean %>%
        mutate(cough.of.other.or.unknown.duration = case_when(
            grepl("cough", other.symptom, ignore.case=TRUE) ~ "Yes",
            TRUE ~ cough.of.other.or.unknown.duration))
    
    ### Add `symptom.screening.` prefix to all of these variables
    
    symptomvars_clean <- symptomvars_clean %>%
        rename_with(
            .cols = -covidence.id,
            .fn = ~ paste0("symptom.screening.", .x))
    
    
    ### Now join back onto the clean dataset
    cleanDF <- left_join(cleanDF, symptomvars_clean)
    
    ### Create a list that contains the two dataframes:
    ### Name the list to make extraction easier (e.g. out$cleanDF now accessible)
    newVariablesSummary <- list("clean data" = cleanDF,
                                "errors" = errorDF)
    
    return(newVariablesSummary)
    
    
    
    
    
    
    
}