#### When running the logical checks on totals, we observed a larger than
#### expected number of errors for the following two checks: 
### 1. Totals should be similar (within 5%) across all strata
### 2. Sum of stratified counts equal total counts (within each stratum)
### Here we visualize these to see if our code is not working as expected.



##########################################################################|
#####                               Setup                             #####
##########################################################################|

library(here)
library(dplyr)
source("code/cleanData.R")
dataSummary <- cleanData()

##########################################################################|
#####                        Review distribution                      #####
##########################################################################|

dim(dataSummary[["errors"]])
summary(as.factor(dataSummary[["errors"]][,"Error.message"]))


##########################################################################|
#####                   Total not equal sum of strata                 #####
##########################################################################|

filtErrors <- dataSummary[["errors"]] %>% 
              filter(Error.message == "Extracted total is not equal to sum of extracted stratified data.")

#### How many affected studies 
length(unique(filtErrors$Study.ID)) 
#### Percent
length(unique(filtErrors$Study.ID)) / 115 * 100

#### Super high! Perhaps check is too sensitive? Remove all sex matches. 
#### Because we instructed this to represent the totals so it should not match. 
filtErrorsNoSex <- filtErrors[-grep("sex", filtErrors$Column.name),]

#### How many affected studies 
length(unique(filtErrorsNoSex$Study.ID)) 
#### Percent
length(unique(filtErrorsNoSex$Study.ID)) / 115 * 100

#### Still a huge percent. Can we understand the breakdown more? 
#### No sex.rurality errors. 
filtErrorsSex <- filtErrors[grep("sex", filtErrors$Column.name),]
filtErrorsAge <- filtErrors[grep("age.grp", filtErrors$Column.name),]
filtErrorsRurality <- filtErrors[grep("rurality.", filtErrors$Column.name),]

#### How many affected studies 
length(unique(filtErrorsSex$Study.ID))
length(unique(filtErrorsAge$Study.ID))
length(unique(filtErrorsRurality$Study.ID))

#### Check whether they are greater or lesser
for (col in unique(filtErrorsAge$Column.name)){
    print(col)
    test1 <- dataSummary[["clean data"]]  %>% 
        filter(covidence.id %in% filtErrorsAge[which(filtErrorsAge$Column.name == col),"Study.ID"]) %>%
        select(contains(gsub(".total","",col))) 
    print(test1)
    print(paste("Number of studies with calculated total not equal to extracted total:",
                nrow(test1)))
    print(paste("Number of studies with calculated total less than extracted total:",
          sum(rowSums(test1[,-length(test1)], na.rm = TRUE) < test1[,length(test1)])))
}

for (col in unique(filtErrorsSex$Column.name)){
    print(col)
    test1 <- dataSummary[["clean data"]]  %>% 
        filter(covidence.id %in% filtErrorsSex[which(filtErrorsSex$Column.name == col),"Study.ID"]) %>%
        select(paste0((gsub(".sex.total","", col)), c(".male", ".female", ".sex.total")))
    print(paste("Number of studies with calculated total not equal to extracted total:",
                nrow(test1)))
    print(paste("Number of studies with calculated total less than extracted total:",
                sum(rowSums(test1[,-length(test1)], na.rm = TRUE) < test1[,length(test1)])))
}

##########################################################################|
#####                   Totals               #####
##########################################################################|
filtErrors <- dataSummary[["errors"]][grep("Difference in extracted totals are greater than 5%", dataSummary[["errors"]]$Error.message),]

test2<- dataSummary[["clean data"]]  %>% 
        filter(covidence.id == "25143") %>% 
    dplyr::select(contains("n.culture.positive.tb"))

#### How many affected studies 
length(unique(filtErrors$Study.ID)) 
#### Percent
length(unique(filtErrors$Study.ID)) / 115 * 100
