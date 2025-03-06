##### ABOUT THIS SCRIPT #######################################################
### This script reads in the clean dataset from data folder and then 
### replicates the methods in Horton et. al to determine change.  

bactPostIndicator <- function(){
##############################################################################|
##### LOAD IN NECESSARY PACKAGES ##############################################
library(dplyr)
library(here)
library(magrittr)
##############################################################################|
##### LOAD CLEAN DATA SET AND FILTER TO SURVEYS REPORTING SEX #################
cleanSexDF0 <- readRDS(here("data/fullDataClean.rds")) %>%
    filter(report.sex == "Yes")    
##############################################################################|
##### ADD WHO REGION AND STUDY ID TO DATA  ####################################
regionWHO <- read.csv("data/who-regions.csv")[,c(1,4)] %>% 
    rename(study.country = Entity,
           WHO.region = World.regions.according.to.WHO)

figID <- read.csv(here("data/titlesForFigures.csv"))
colnames(figID)[1] <- "covidence.id"

cleanSexDF0  %<>% left_join(figID[,c("covidence.id", "figure.id")], 
                            by = "covidence.id") %>% 
    mutate(figure.id.yr = paste(figure.id, study.years))

### Rename some countries to match the WHO CSV.
### For labeling these will remain as extracted. 

cleanSexDF0[which(cleanSexDF0$study.country == "Viet Nam"), "study.country"] <- "Vietnam"
cleanSexDF0[which(cleanSexDF0$study.country == "The Gambia"), "study.country"] <- "Gambia"
cleanSexDF0[which(cleanSexDF0$study.country == "United Republic of Tanzania"), "study.country"] <- "Tanzania"
cleanSexDF0[which(cleanSexDF0$study.country == "Lao PDR"), "study.country"] <- "Laos"
cleanSexDF0[which(cleanSexDF0$study.country == "Democratic People's Republic of Korea"), "study.country"] <- "North Korea"

### Join the WHO data 
cleanSexDF0 %<>% left_join(regionWHO)


##############################################################################|
##### CREATE BACTERIOLOGICAL ESTIMATES 

### ADJUSTED PREVALENCE 
newBactSexDF <- cleanSexDF0 %>% mutate("sex.analysis.indicator" = case_when(
                        ### Bacteriological positive adjusted prevalence with CI reported by survey
                        !is.na(adj.prev100k.ci.bacteriological.tb.male) ~ "adj.prev100k.ci.bacteriological.tb", 
                        ### Smear positive adjusted prevalence with CI reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & !is.na(adj.prev100k.ci.smear.positive.tb.male) ~ "adj.prev100k.ci.smear.positive.tb", 
                        ### Bacteriological crude adjusted prevalence with CI reported by survey
                        !is.na(prev100k.ci.bacteriological.tb.male) ~ "prev100k.ci.bacteriological.tb", 
                        ### Smear positive crude prevalence reported with CI by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & !is.na(prev100k.ci.smear.positive.tb.male) ~ "prev100k.ci.smear.positive.tb",
                        ### Bacteriological positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        !is.na(n.bacteriological.tb.male) & !is.na(n.participants.male) ~ "n.bacteriological.tb",
                        ### Culture positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & !is.na(n.culture.positive.tb.male) & !is.na(n.participants.male) ~ "n.culture.positive.tb",
                        ### Smear positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & is.na(n.culture.positive.tb.male) & 
                        !is.na(n.smear.positive.tb.male) & !is.na(n.participants.male) ~ "n.smear.positive.tb", 
                        ### Bacteriological positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & is.na(n.smear.positive.tb.male) &
                        !is.na(adj.prev100k.bacteriological.tb.male) & !is.na(n.participants.male) ~ "adj.prev100k.bacteriological.tb",
                        ### Smear positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & is.na(n.smear.positive.tb.male) &
                        is.na(adj.prev100k.bacteriological.tb.male) & !is.na(adj.prev100k.smear.positive.tb.male) &
                        !is.na(n.participants.male) ~ "adj.prev100k.smear.positive.tb",
                        ### Bacteriological positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & is.na(n.smear.positive.tb.male) &
                        is.na(adj.prev100k.bacteriological.tb.male) & is.na(adj.prev100k.smear.positive.tb.male) &
                        !is.na(prev100k.bacteriological.tb.male) & !is.na(n.participants.male) ~ "prev100k.bacteriological.tb",
                        ### Smear positive TB counts and participants reported by survey
                        is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        is.na(n.bacteriological.tb.male) & is.na(n.smear.positive.tb.male) &
                        is.na(adj.prev100k.bacteriological.tb.male) & is.na(adj.prev100k.smear.positive.tb.male) &
                        is.na(prev100k.bacteriological.tb.male) & !is.na(prev100k.smear.positive.tb.male) &
                        !is.na(n.participants.male) ~ "prev100k.smear.positive.tb",
                        .default = "none"))
                        # "sex.analysis.indicator.2" = case_when(
                        # ### Bacteriological positive adjusted prevalence with CI reported by survey
                        # !is.na(adj.prev100k.ci.bacteriological.tb.male) ~ "adj.prev100k.bacteriological.tb", 
                        # ### Smear positive adjusted prevalence with CI reported by survey
                        # is.na(adj.prev100k.ci.bacteriological.tb.male) & !is.na(adj.prev100k.ci.smear.positive.tb.male) ~ "adj.prev100k.smear.positive.tb", 
                        # ### Bacteriological crude adjusted prevalence with CI reported by survey
                        # !is.na(prev100k.ci.bacteriological.tb.male) ~ "prev100k.bacteriological.tb", 
                        # ### Smear positive crude prevalence reported with CI by survey
                        # is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        # is.na(prev100k.ci.bacteriological.tb.male) & !is.na(prev100k.ci.smear.positive.tb.male) ~ "prev100k.smear.positive.tb",
                        # ### Bacteriological positive TB counts and participants reported by survey
                        # is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        # is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        # !is.na(n.bacteriological.tb.male) & !is.na(n.participants.male) ~ "n.bacteriological.tb",
                        # ### Culture positive TB counts and participants reported by survey
                        # is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        # is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        # is.na(n.bacteriological.tb.male) & !is.na(n.culture.positive.tb.male) &
                        # n.culture.positive.tb.male > n.smear.positive.tb.male & !is.na(n.participants.male) ~ "n.culture.positive.tb",
                        # ### Smear positive TB counts and participants reported by survey
                        # is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.smear.positive.tb.male) &
                        # is.na(prev100k.ci.bacteriological.tb.male) & is.na(prev100k.ci.smear.positive.tb.male) &
                        # is.na(n.bacteriological.tb.male) & (is.na(n.culture.positive.tb.male) | n.culture.positive.tb.male < n.smear.positive.tb.male) & 
                        # !is.na(n.smear.positive.tb.male) & !is.na(n.participants.male) ~ "n.smear.positive.tb", 
                        # .default = "none"))

table(newBactSexDF$sex.analysis.indicator)

##############################################################################|
##### CHECKS TO CONFIRM THE FINAL DATA ########################################

### Check where smear positive counts are higher than culture counts

highSmr <- newBactSexDF %>% filter(n.smear.positive.tb.male > n.culture.positive.tb.male) %>%
                            dplyr::select(title.covidence, n.smear.positive.tb.male, 
                                          n.culture.positive.tb.male,
                                          adj.prev100k.smear.positive.tb.male,
                                          prev100k.smear.positive.tb.male,
                                          sex.analysis.indicator,
                                          smear.positive.tb.definition, 
                                          culture.positive.tb.definition)

write.csv(x = highSmr, file = here("data/checks/cultureSmallerThanSmear.csv"))                       
                       
### Check where study used Xpert and compare to bacteriological positive
### definitions.

xpert <- cleanSexDF0 %>% filter(xpert.used != "Neither", 
                       xpert.used != "Unknown") %>% select(title.covidence, 
                                                           xpert.used, 
                                                           bacteriological.tb.definition)

write.csv(x = xpert, file = here("data/checks/usedXpert.csv"))                       

### Output the studies which we have no data for bacteriological positive
### analysis. 
excluded <- newBactSexDF %>% filter(sex.analysis.indicator == "none") %>%
    dplyr::select(title.covidence, contains("female"),
                  sex.analysis.indicator,
                  smear.positive.tb.definition, culture.positive.tb.definition)              

write.csv(x = excluded, file = here("data/checks/excludedSexSurveys.csv"))                

return(newBactSexDF)

}