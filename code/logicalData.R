### This script contains a single function, logicalData() that takes in the
### dataset from validateData() and performs a number of data cleaning steps to ensure the 
### data are valid. It will return a list with two data frames: the first contains
### information about errors and the second contains the clean-er dataset. 

### to be called from within the cleanData() function. 


logicalData <- function(validData, 
                        errorDF){
    ### Create a dataframes that will be updated at each cleaning step. 
    ### ### Will contains the clean dataset.
    ### ### Initialized with the standardizedData dataframe
    
    logicDF <- validData
    
    ###########################################################################
    ### Numerical checks ######################################################
    ###########################################################################
   
    ### Confirm publication year is greater than or equal to study year. 
    ### Requires the creation of variables study.start.year and study.end.year
    ### Initialize these new variables. 
    print("Checking study years vs. publication years.")
    logicDF$study.start.year <- NA
    logicDF$study.end.year <- NA
    
    ### Split the string based on the "-" character 
    yearSplit <- strsplit(logicDF$study.years, "-")

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
    errorDF <- data.frame("Study title" = logicDF$title.covidence[which(sapply(yearSplit,length) > 2)],
                          "Study ID" = logicDF$covidence.id[which(sapply(yearSplit,length) > 2)],
                          "Column name" = rep("study.years", length(which(sapply(yearSplit,length) > 2))),
                          "Error message" = rep("More than two years listed", length(which(sapply(yearSplit,length) > 2)))
                        ) ### close dataframe
                           
    ### Check that the publication year is greater than or equal to end year 
        ### Add the errors to the errorDF 
    errorDF <- rbind(errorDF, data.frame("Study title" = logicDF$title.covidence[which(logicDF$publication.year < logicDF$study.end.year)],
                          "Study ID" = logicDF$covidence.id[which(logicDF$publication.year < logicDF$study.end.year)],
                          "Column name" = rep("study.years", length(which(logicDF$publication.year < logicDF$study.end.year))),
                          "Error message" = rep("Study year is after publication year", length(which(logicDF$publication.year < logicDF$study.end.year)))
    ) )    
    
    ### Stratification specific numeric checks. These only need to be done for 
    ### the stratifications for which the data were collected. 
    print("Checking stratification values.")
    
    ### Create a vector the strings that uniquely define each stratification
    strats <- c("sex", "rurality", "hiv", "age.grp", "sex.rurality")
    ### Identify columns that delineate the beginning of a stratification
    ### These column names start with "report."
    stratIndex <- c(grep("report.",colnames(logicDF)),
                    ### Add in where to end the last stratification
                    which(colnames(logicDF) == "data.availability.comments")-1)
    
    for(strat in strats){
        
        ### Check that target population >= eligible population
        which((logicDF[,"total.target.pop"] >= 
        logicDF[,grep(pattern = paste0("eligible.", strat, ".total"), x = colnames(logicDF))]) == FALSE)
        
        ### Check that eligible population is >= participant population
        which((logicDF[,grep(pattern = paste0("eligible.", strat, ".total"), x = colnames(logicDF))] >= 
        logicDF[,grep(pattern = paste0("participants.", strat, ".total"), x = colnames(logicDF))]) == FALSE)
        
        ### Check that sum of stratified counts match total counts 
        which((logicDF[,grep(pattern = paste0("n.", strat, ".total"), x = colnames(logicDF))] == 
               logicDF[,grep(pattern = paste0("participants.", strat, ".total"), x = colnames(logicDF))]) == FALSE)
        
        which(logicDF$n.eligible.male + logicDF$n.eligible.female != logicDF[,grep(pattern = paste0("eligible.", strat, ".total"), x = colnames(logicDF))])
        which(logicDF$n.eligible.male + logicDF$n.eligible.female != logicDF$n.eligible.sex.total)
    }
    

    ### Check that presumptive TB >= symptoms and/or chest x-ray >= cases 
    ### (of various types).
    
    ### Identify the relevant columns 
    presumpIndex <- grep(pattern = "n.presumptive.tb", x = colnames(logicDF))
    symptomIndex <- grep(pattern = "n.symptoms", x = colnames(logicDF))
    xrayIndex <- grep(pattern = "n.abnormal.xray", x = colnames(logicDF))
   
    
    print("Checking stratified counts against totals.")
    # countsStrings <- unique(unlist(strsplit(colnames(logicDF)[grepl("^n[.].+total$", colnames(logicDF))], split = "[.]")))
    countsStrings <- c("eligible", "participants", "presumptive.tb", "symptoms", 
                       "abnormal.xray", "sputum.sample", "radiologic.tb", 
                       "bacteriological.tb", "smear.positive.tb", 
                       "culture.positive.tb", "prevalent.tb")
    
    for(n in countsStrings){
        print(n)
        ### Stratified counts match total counts (with in each stratification).
        index <- which(logicDF[,paste0("n.", n, ".female")] + logicDF[,paste0("n.", n, ".male")] != logicDF[,paste0("n.", n, ".sex.total")])
        
        print("Checking sex counts.")
        ### Sex stratification
        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("n.", n, ".sex.total"), 
                                                            length(index)),
                                        "Error message" = rep("Extracted total is not equal to sum of extracted stratified data.", 
                                                              length(index))))
        }

        ### Reset the indices 
        index <- NA
        
        ### Rurality stratification
        print("Checking rurality counts.")
        index <- which(logicDF[,paste0("n.", n, ".rural")] + logicDF[,paste0("n.", n, ".urban")] != logicDF[,paste0("n.", n, ".rurality.total")])
        
        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("n.", n, ".rurality.total"), 
                                                            length(index)),
                                        "Error message" = rep("Extracted total is not equal to sum of extracted stratified data.", 
                                                              length(index))))
        }
        
        ### Reset the indices 
        index <- NA
        
        ### HIV stratification
        print("Checking HIV counts.")
        index <- which(logicDF[,paste0("n.", n, ".hiv.positive")] + logicDF[,paste0("n.", n, ".hiv.negative")] != logicDF[,paste0("n.", n, ".hiv.total")])
        
        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("n.", n, ".hiv.total"), 
                                                            length(index)),
                                        "Error message" = rep("Extracted total is not equal to sum of extracted stratified data.", 
                                                              length(index))))
        }
        
        ### Reset the indices 
        index <- NA
        
        ### Age stratification
        print("Checking age.grp counts.")
        index <- which(rowSums(cbind(logicDF[,paste0("n.", n, ".age.grp.1")], 
                       logicDF[,paste0("n.", n, ".age.grp.2")], 
                       logicDF[,paste0("n.", n, ".age.grp.3")], 
                       logicDF[,paste0("n.", n, ".age.grp.4")],
                       logicDF[,paste0("n.", n, ".age.grp.5")], 
                       logicDF[,paste0("n.", n, ".age.grp.6")], 
                       logicDF[,paste0("n.", n, ".age.grp.7")]), na.rm =TRUE) !=
                       logicDF[,paste0("n.", n, ".age.grp.total")])
        
        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("n.", n, ".age.grp.total"), 
                                                            length(index)),
                                        "Error message" = rep("Extracted total is not equal to sum of extracted stratified data.", 
                                                              length(index))))
        }
        
        ### Reset the indices 
        index <- NA
        
        ### Rurality and sex stratification
        print("Checking rurality and sex counts.")
        index <- which(logicDF[,paste0("n.", n, ".female.rural")] + 
                       logicDF[,paste0("n.", n, ".female.urban")] + 
                       logicDF[,paste0("n.", n, ".male.rural")] + 
                       logicDF[,paste0("n.", n, ".male.urban")]  !=
                       logicDF[,paste0("n.", n, ".sex.rurality.total")])
        
        if (length(index > 0)){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[index],
                                        "Study ID" = logicDF$covidence.id[index],
                                        "Column name" = rep(paste0("n.", n, ".sex.rurality.total"), 
                                                            length(index)),
                                        "Error message" = rep("Extracted total is not equal to sum of extracted stratified data.", 
                                                              length(index))))
        }
        
        ### Reset the indices 
        index <- NA
        
        ### Totals should be within +/- 10% across all stratification
        ### (except HIV; see below).
        
        
        
        ### Sum of stratified counts should be within +/- 10% across all stratification
        ### (except HIV; see below).
        
        ### Confirm that counts for the HIV stratum is less than or equal
        ### to totals in other strata (if provided)
        print("Checking that HIV totals less than others.")
        if (length(which(logicDF[,paste0("n.", n, ".hiv.total")] > logicDF[,paste0("n.", n, ".sex.total")])) > 0) {
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                      logicDF[,paste0("n.", n, ".sex.total")])],
                                        "Study ID" = logicDF$covidence.id[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                    logicDF[,paste0("n.", n, ".sex.total")])],
                                        "Column name" = rep(".hiv.total", 
                                                            length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                             logicDF[,paste0("n.", n, ".sex.total")]))),
                                        "Error message" = rep("HIV total is greater than sex total for at least one value.", 
                                                              length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                               logicDF[,paste0("n.", n, ".sex.total")])))))
        }
        
        if (length(which(logicDF[,paste0("n.", n, ".hiv.total")] > logicDF[,paste0("n.", n, ".rurality.total")])) > 0){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                          logicDF[,paste0("n.", n, ".rurality.total")])],
                                        "Study ID" = logicDF$covidence.id[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                    logicDF[,paste0("n.", n, ".rurality.total")])],
                                        "Column name" = rep(".hiv.total", 
                                                            length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                             logicDF[,paste0("n.", n, ".rurality.total")]))),
                                        "Error message" = rep("HIV total is greater than rurality total for at least one value.", 
                                                              length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                               logicDF[,paste0("n.", n, ".rurality.total")])))))
        }
    
        if (length(which(logicDF[,paste0("n.", n, ".hiv.total")] > logicDF[,paste0("n.", n, ".age.grp.total")])) > 0){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                          logicDF[,paste0("n.", n, ".age.grp.total")])],
                                        "Study ID" = logicDF$covidence.id[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                    logicDF[,paste0("n.", n, ".age.grp.total")])],
                                        "Column name" = rep(".hiv.total", 
                                                            length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                             logicDF[,paste0("n.", n, ".age.grp.total")]))),
                                        "Error message" = rep("HIV total is greater than age.grp total for at least one value.", 
                                                              length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                               logicDF[,paste0("n.", n, ".age.grp.total")])))))
        }
        
        if (length(which(logicDF[,paste0("n.", n, ".hiv.total")] > logicDF[,paste0("n.", n, ".sex.rurality.total")])) > 0){
            ### Add errors to errorDF
            errorDF <- rbind(errorDF, 
                             data.frame("Study title" = logicDF$title.covidence[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                          logicDF[,paste0("n.", n, ".sex.rurality.total")])],
                                        "Study ID" = logicDF$covidence.id[which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                                    logicDF[,paste0("n.", n, ".sex.rurality.total")])],
                                        "Column name" = rep(".hiv.total", 
                                                            length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                             logicDF[,paste0("n.", n, ".sex.rurality.total")]))),
                                        "Error message" = rep("HIV total is greater than age.grp total for at least one value.", 
                                                              length(which(logicDF[,paste0("n.", n, ".hiv.total")] > 
                                                                               logicDF[,paste0("n.", n, ".sex.rurality.total")])))))
        }
    }
    
    ### When crude population and prevalence counts are available, 
    ### calculate crude prevalence and compare with extracted crude prevalence. 
    ### Check that these are within 5% of one another. 
    prevalenceStrings <- c("bacteriological", "smear.positive", "prevalent")
    
    print("Checking extracted prevalence against calculated prevalence.")
    for(prev in prevalenceStrings){
        print(prev)
        for(strat in strats){
            print(strat)
            index <-  which(abs(logicDF[,paste0("prev100k.", prev, ".tb.", strat,".total")] - 
                                ((logicDF[,paste0("n.", prev, ".tb.", strat,".total")] / logicDF[,paste0("n.participants.", strat,".total")])*1e5) / 
                                 logicDF[,paste0("prev100k.", prev, ".tb.", strat,".total")]) > .05)
            
            if (length(index > 0)){
                ### Add errors to errorDF
                errorDF <- rbind(errorDF, 
                                 data.frame("Study title" = logicDF$title.covidence[index],
                                            "Study ID" = logicDF$covidence.id[index],
                                            "Column name" = rep(paste0("prev100k.", prev, ".tb.", strat, ".total"), 
                                                                length(index)),
                                            "Error message" = rep(paste(prev, "tb prevalence total is greater than 5% different than calculated prevalence 100k."), 
                                                                  length(index))))
            }
        }
    }
        
    ###########################################################################
    ### Consistency checks ####################################################
    ########################################################################### 
    
    ###  If the survey is marked to be "only rural" or "only urban", 
    ### confirm that the checkbox indicating stratified results is marked no 
    ### and no urban/rural stratified results are extracted.
    
    if(length(which(logicDF$study.rurality %in% c("Rural only", "Urban only") &
          logicDF$report.rurality == "Yes")) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[which(logicDF$study.rurality %in% c("Rural only", "Urban only") &
                                                                                  logicDF$report.rurality == "Yes")],
                                    "Study ID" = logicDF$covidence.id[which(logicDF$study.rurality %in% c("Rural only", "Urban only") &
                                                                            logicDF$report.rurality == "Yes")],
                                    "Column name" = rep("study.rurality", 
                                                        length(which(logicDF$study.rurality %in% c("Rural only", "Urban only") &
                                                                     logicDF$report.rurality == "Yes"))),
                                    "Error message" = rep("Study rurality does not match extracted data", 
                                                          length(which(logicDF$study.rurality %in% c("Rural only", "Urban only") &
                                                                       logicDF$report.rurality == "Yes")))))
        
    }
    
    ### Check that screening questions have consistent values (e.g. if symptom 
    ### screen is performed, we should have a response for "which of the following 
    ### screening tests were done?"
    
    ### SYMPTOM SCREENING 
    ### Set the indices 
    index <-
    ### Symptom screen not performed but symptoms listed 
    which(!grepl(pattern = "symptom", x = logicDF$screening.tests, ignore.case = TRUE) &
          !is.na(logicDF$screening.symptoms) |
    ### Symptom screen performed but symptoms not listed 
    grepl(pattern = "symptom", x = logicDF$screening.tests, ignore.case = TRUE) &
    is.na(logicDF$screening.symptoms))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("screening.tests", 
                                                        length(index)),
                                    "Error message" = rep("Screening.tests does not match extracted symptom data", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    ### CHEST XRAY 
    ### Set the indices 
    index <-
    ### X-Ray screening not performed but how abnormality was define is listed
    which(!grepl(pattern = "X-ray", x = logicDF$screening.tests, ignore.case = TRUE) &
          logicDF$positive.xray.definition != "Chest X-ray was not used" |
    ### X-Ray screening performed but how abnormality was define is not listed
    grepl(pattern = "X-ray", x = logicDF$screening.tests, ignore.case = TRUE) &
        (logicDF$positive.xray.definition == "Chest X-ray was not used" | is.na(logicDF$positive.xray.definition)))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("screening.tests", 
                                                        length(index)),
                                    "Error message" = rep("Screening.tests does not match extracted x-ray data", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    ### Check that diagnostic questions have consistent values (e.g. If "Were sputum samples
    ### tested by smear microscopy?" is no, then we should have appropriate responses for 
    ### "Which samples were tested by smear microscopy?"
    
    ### SPUTUM SMEAR
    ### Set the indices 
    index <-
    ### Subset of smear used but no details provided and vice-versa
    which(grepl(pattern = "Subset", x = logicDF$smear.samples, ignore.case = TRUE) ==
          is.na(logicDF$other.smear.samples))

    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("smear.samples", 
                                                        length(index)),
                                    "Error message" = rep("smear.samples does not match extracted data", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    ### XPERT SAMPLES
    ### Set the indices 
    index <-
    ### Subset of smear used but no details provided and vice-versa
    which(grepl(pattern = "Subset", x = logicDF$xpert.samples, ignore.case = TRUE) ==
              is.na(logicDF$other.xpert.samples))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("xpert.samples", 
                                                        length(index)),
                                    "Error message" = rep("xpert.samples does not match extracted data", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    ### CULTURE SAMPLES
    ### Set the indices 
    index <-
    ### Subset of smear used but no details provided and vice-versa
    which(grepl(pattern = "Subset", x = logicDF$culture.samples, ignore.case = TRUE) ==
              is.na(logicDF$other.culture.samples))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("culture.samples", 
                                                        length(index)),
                                    "Error message" = rep("culture.samples does not match extracted data", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    ### Given there are 8 questions, consider a threshold of 3+ indications of high 
    ### bias negates the possibility of an ultimate "low bias assessment". Similarly,
    ### 3+ indications of low bias negates the possibility of an ultimate 
    ### "high bias assessment".
    
    ### Set the quality indices 
    qualityIndex <- grep("study.quality", colnames(logicDF))
    
    index <- 
    ### Three low risk items but overall high bias
    which(rowSums(logicDF[,qualityIndex[1:8]] == "Yes (low risk)") > 3 & 
          grepl("High risk", logicDF[,qualityIndex[9]]))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("study.quality.summary", 
                                                        length(index)),
                                    "Error message" = rep("More than 3 low risk items but overall high bias", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA
    
    index <- 
    ### Three high risk items but overall low bias
    which(rowSums(logicDF[,qualityIndex[1:8]] == "No (high risk)") > 3 & 
          grepl("Low risk", logicDF[,qualityIndex[9]]))
    
    if(length(index) > 0){
        ### Add errors to errorDF
        errorDF <- rbind(errorDF, 
                         data.frame("Study title" = logicDF$title.covidence[index],
                                    "Study ID" = logicDF$covidence.id[index],
                                    "Column name" = rep("study.quality.summary", 
                                                        length(index)),
                                    "Error message" = rep("More than 3 high risk items but overall low bias", 
                                                          length(index))))
    }
    
    ### Reset the indices 
    index <- NA

    ### Create a list that contains the two dataframes:
    logicDataSummary <- list("clean data" = logicDF, 
                             "errors" = errorDF)
    
    return(logicDataSummary)

}
