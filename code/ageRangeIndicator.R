ageRangeInd <- function(cleanDF0){

library(dplyr)
library(here)

# source(here("code/cleanData.R"))
# cleanDF0 <- cleanData()[["clean data"]]

### Filter relevant columsn
popDesc <- cleanDF0 %>% dplyr::select(title.covidence,screening.criteria, participant.definition)

#### Check adults
index15 <- sapply(popDesc[,2:3], function(x) grep("15", x))
index14 <- sapply(popDesc[,2:3], function(x) grep("14 year", x))
index20 <- sapply(popDesc[,2:3], function(x) grep("20 year", x))

# View(popDesc[index15[[1]],c(1,2)])
# View(popDesc[index14[[1]],c(1,2)])

### Confirmed all of these do not contain children 
adultsOnlyTotals <- c(unique(index15[[1]], index14[[1]]), index20[[2]])

# View(popDesc[index15[[2]],c(1,3)])

### location 22 (Phillipines) cannot be ruled in as the 15 is matching clusters; row is 29
### location 36 (Kampala, Uganda) matched on less than 15 ; row is 63
childrenInTotals <- 63
### location 40 cannot be ruled in as matched on random stat; row is 81

# View(popDesc[index14[[2]],c(1,3)])

adultsOnlyTotals <- unique(c(adultsOnlyTotals, index15[[2]][c(-22,-36,-40)], index14[[2]]))


indexAdult <- sapply(popDesc, function(x) grep("adult", x, ignore.case = TRUE))
indexElderly <- sapply(popDesc, function(x) grep("elderly", x, ignore.case = TRUE))
adultsOnlyTotals <- unique(c(adultsOnlyTotals, indexAdult[[1]]))
adultsOnlyTotals <- unique(c(adultsOnlyTotals, setdiff(indexAdult[[2]], index15[[1]])))
adultsOnlyTotals <- unique(c(adultsOnlyTotals, setdiff(indexAdult[[3]], index15[[2]]), indexElderly[[1]]))

#### Create a new variable 
cleanDF <- cleanDF0
children.in.totals <- rep(NA, length = nrow(cleanDF))
cleanDF <- cbind(cleanDF0, children.in.totals)
cleanDF[adultsOnlyTotals, "children.in.totals"] <- "No"
#### Identify studies with children in totals 
indexChildren <- sapply(popDesc[,2:3], function(x) grep("children", x, ignore.case=TRUE))
index10 <- sapply(popDesc[,2:3], function(x) grep("10 year", x, ignore.case=TRUE))
index12 <- sapply(popDesc[,2:3], function(x) grep("12 year", x, ignore.case=TRUE))
indexAll <- sapply(popDesc[,2:3], function(x) grep("all ages", x, ignore.case=TRUE))

# View(popDesc[indexChildren[[1]]])
#### Confirmed these study's total has children 
childrenInTotals <- unique(c(childrenInTotals, indexChildren[[1]], index10[[1]], index10[[2]], indexAll[[2]], index12[[2]]))

under15Index <- which(cleanDF$age.grp.0.range != "0-14")
#### Confirmed these studies' totals has children 

childrenInTotals <- unique(c(childrenInTotals, under15Index))
cleanDF[childrenInTotals, "children.in.totals"] <- "Yes"

#### Now let's see if we can reduce the needs for checks based on those that reported age 

cleanDF <- cleanDF %>% mutate(children.in.totals = ifelse(is.na(children.in.totals) &
                                report.age.grp == "Yes" &
                                age.grp.0.range == "0-14" &
                                n.participants.age.grp.0 > 0, "Yes", children.in.totals)) %>%  
                  mutate(children.in.totals = ifelse(is.na(children.in.totals) &
                                                         report.age.grp == "Yes" &
                                                         age.grp.0.range == "0-14" &
                                                         is.na(n.participants.age.grp.0), "No", children.in.totals))


cleanDF <- cleanDF  %>% 
      mutate(children.in.totals = ifelse(is.na(children.in.totals) &
                                         report.age.grp == "Yes" & 
                                         is.na(n.participants.age.grp.0) &
                                         is.na(n.participants.age.grp.1) &
                                         (grepl("10", age.grp.1.range) | grepl("10", age.grp.2.range)), 
                                         "Yes", children.in.totals)) 
cleanDF <- cleanDF  %>% 
    mutate(children.in.totals = ifelse(is.na(children.in.totals) &
                                           report.age.grp == "Yes" & 
                                           is.na(n.participants.age.grp.0) &
                                           is.na(n.participants.age.grp.1) &
                                           (grepl("10", age.grp.1.range) & grepl("10", age.grp.2.range) ==FALSE), 
                                       "No", children.in.totals))


###############################################################################

cleanDF<- cleanDF  %>% mutate(children.in.totals = ifelse(is.na(children.in.totals) & 
                                   report.age.grp == "Yes" &
                                   (grepl("15", age.grp.2.range) | grepl("14", age.grp.2.range) | 
                                   grepl("55", age.grp.6.range)),
                                   "No", children.in.totals)) 

###############################################################################
##### Save to CSV for checking 
###############################################################################
checkStudies <- cleanDF[which(is.na(cleanDF$children.in.totals)), 
                        c("study.id", "covidence.id", "title.covidence", "screening.criteria", 
                          "participant.definition", "report.age.grp", "age.grp.0.range", 
                          "n.participants.age.grp.0", "age.grp.1.range", "n.participants.age.grp.1")]
if (nrow(checkStudies) > 0){
ids <- read_csv(here("data/concensusIDs.csv")) %>%
    rename("Concensus.Assignee" = `Consensus Asignee`, 
           "covidence.id" = Study.ID) 

checkStudies0 <- left_join(checkStudies,  ids[,-1], by="covidence.id")
write.csv(x = checkStudies0, file = here("data/popAgeRangeCheck.csv"), row.names = FALSE)
} else {
    ### Convert the variable to a factor 
    cleanDF$children.in.totals <- as.factor(cleanDF$children.in.totals)
    print("All studies age ranges identified.")
    print("New variable `children.in.totals` created.")
}
    return(cleanDF)
}