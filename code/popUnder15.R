library(dplyr)
library(here)
cleanDF0 <- cleanData()[["clean data"]]

popDesc <- cleanDF %>% dplyr::select(screening.criteria, participant.definition)

#### Check adults
index15 <- sapply(popDesc[,1:2], function(x) grep("15", x))

View(table(popDesc[index15[[1]],1]))
### Confirmed all of these do not contain children 
adultsOnlyTotals <- index15[[1]]

popDesc[index15[[2]],2]

### 19 and 33 cannot be ruled in as the 15 is matching clusters
### 30 matched on less than 15 
### 34 cannot be ruled in as matched on random stat
### 44 matched a number? check this! 

adultsOnlyTotals <- unique(c(adultsOnlyTotals, index15[[2]][c(-19,-33,-30,-34,-44)]))


indexAdult <- sapply(popDesc[,1:2], function(x) grep("adult", x))
# popDesc[setdiff(indexAdult[[1]], index15[[1]]),1]


#### Now use the age ranges to specify
# indexNotStnd <- which(cleanDF$age.grp.1.range != "15-24")
# cleanDF[indexNotStnd, "age.grp.1.range"]

#### Identify studies with children in totals 
indexChildren <- sapply(popDesc[,1:2], function(x) grep("children", x))
# View(table(popDesc[indexChildren[[1]],1]))
# View(popDesc[indexChildren[[1]], ])
#### Confirmed this study's total has children 
childrenInTotals <- 78

under15Index <- which(cleanDF$age.grp.0.range != "0-14")

#### Confirmed these studies' totals has children 
childrenInTotals <- c(childrenInTotals, under15Index)


#### Export the names of the ones that are unclear to CSV 

checkIndex <- (1:nrow(cleanDF))[-c(adultsOnlyTotals,childrenInTotals)]

checkStudies <- cleanDF[checkIndex, c("study.id", "covidence.id", "title.covidence", "screening.criteria", "participant.definition")]

write.csv(x = checkStudies, file = here("data/popAgeRangeCheck.csv"), row.names = FALSE)
