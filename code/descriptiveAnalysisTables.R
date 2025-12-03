### This script calls the related data cleaning wrapper function
### and then uses that clean data to begin a descriptive analysis
### of the collected data. Best used on non-stratified data, but 
### can be modified to plot stratified data. 

### Load packages 
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gt)
library(maps)
library(ggpubr)
##############################################################################|
##### Create the clean dataset ################################################
##############################################################################|
source(here("code/cleanData.R"))
cleanDF0 <- cleanData()[["clean data"]]

##############################################################################|
##### Read in the naming schema for figures ###################################
##############################################################################|

figID <- read.csv(here("data/titlesForFigures.csv"))
colnames(figID)[1] <- "covidence.id"
cleanDF0 <- cleanDF0 %>% left_join(figID[,c("covidence.id", "figure.id")], by = "covidence.id") %>% 
    mutate(figure.id.yr = paste(figure.id, study.start.year))

##############################################################################|
##### Read in the World Bank Region data ######################################
##############################################################################|
regionWHO <- read.csv("data/who-regions.csv")[,c(1,4)] %>% 
    rename(study.country = Entity)

### Rename some countries to match the WHO CSV.
### For labeling these will remain as extracted. 
cleanDF <- cleanDF0 
cleanDF[which(cleanDF$study.country == "Viet Nam"), "study.country"] <- "Vietnam"
cleanDF[which(cleanDF$study.country == "The Gambia"), "study.country"] <- "Gambia"
cleanDF[which(cleanDF$study.country == "United Republic of Tanzania"), "study.country"] <- "Tanzania"
cleanDF[which(cleanDF$study.country == "Lao PDR"), "study.country"] <- "Laos"
cleanDF[which(cleanDF$study.country == "Democratic People's Republic of Korea"), "study.country"] <- "North Korea"

### Join the WHO data 
cleanDF <- cleanDF %>% left_join(regionWHO)

##############################################################################|
################## DESCRIPTIVE TABLES ########################################
##############################################################################|

##### Totals by sex #####
sexTbl0 <- cleanDF %>% 
    dplyr::select(starts_with("n.") & ends_with(c("male", "female", "sex.total"))) %>% 
    colSums(na.rm=TRUE)

sexTbl <- matrix(sexTbl0,3,length(sexTbl0)/3, byrow = FALSE)[-3,]

rownames(sexTbl) <- c("Male (N)", "Female (N)")


sexTbl <- sexTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(sexTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `Male (N)` + `Female (N)`, 
           "Female (%)" = round(`Female (N)`/`Total (N)`*100,1), 
           "Male (%)" = round(`Male (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, `Female (N)`, `Female (%)`, `Male (N)`,`Male (%)`, `Total (N)`) %>%
    gt() %>% 
    tab_header(
        title = paste("Sex distribution totals across", sum(cleanDF$report.sex=="Yes"),  "TB prevalence surveys")) %>%
    fmt_number(drop_trailing_zeros = TRUE); sexTbl

gtsave(data = sexTbl, filename = "output/descriptive/sexTable.pdf") 


##### Totals by rurality #####
ruralityTbl0 <- cleanDF %>% 
    dplyr::select(starts_with("n.") & ends_with(c("urban", "rural", "rurality.total"))) %>% 
    colSums(na.rm=TRUE)

### Note the ends with is not specific enough here but the dimensionality
### should match that of sexTbl above so we will leverage that at the moment
ruralityTbl <- matrix(ruralityTbl0[1:length(sexTbl0)],3,length(sexTbl0)/3, byrow = FALSE)[-3,]

rownames(ruralityTbl) <- c("Urban (N)", "Rural (N)")


ruralityTbl <- ruralityTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(ruralityTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `Urban (N)` + `Rural (N)`, 
           "Rural (%)" = round(`Rural (N)`/`Total (N)`*100,1), 
           "Urban (%)" = round(`Urban (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, `Rural (N)`, `Rural (%)`, `Urban (N)`,`Urban (%)`, `Total (N)`) %>%
    gt() %>% 
    tab_header(
        title = paste("Rurality distribution totals across", sum(cleanDF$report.rurality=="Yes"), "TB prevalence surveys")) %>%
    fmt_number(drop_trailing_zeros = TRUE); 

gtsave(data = ruralityTbl, filename = "output/descriptive/ruralityTable.pdf") 


##### Totals by HIV #####
hivTbl0 <- cleanDF %>% 
    dplyr::select(starts_with("n.") & ends_with(c("hiv.positive", "hiv.negative", "hiv.total"))) %>% 
    colSums(na.rm=TRUE)

hivTbl <- matrix(hivTbl0,3,length(hivTbl0)/3, byrow = FALSE)[-3,]

rownames(hivTbl) <- c("HIV positive (N)", "HIV negative (N)")


hivTbl <- hivTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(hivTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `HIV positive (N)` + `HIV negative (N)`, 
           "HIV negative (%)" = round(`HIV negative (N)`/`Total (N)`*100,1), 
           "HIV positive (%)" = round(`HIV positive (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, `HIV negative (N)`, `HIV negative (%)`, `HIV positive (N)`,`HIV positive (%)`, `Total (N)`) %>%
    na.omit() %>%
    gt() %>% 
    tab_header(
        title = paste("HIV status distribution totals across", sum(cleanDF$report.hiv=="Yes"),  "TB prevalence surveys")) %>%
    fmt_number(drop_trailing_zeros = TRUE); hivTbl

gtsave(data = hivTbl, filename = "output/descriptive/hivTable.pdf") 

##### Totals by AGE #####
ageTbl0 <- cleanDF %>% 
    filter(age.grp.1.range == "15-24",
           age.grp.5.range == "55-64") %>% 
    dplyr::select(starts_with("n.") & contains("age.grp") & !contains("age.grp.0")) %>% 
    colSums(na.rm=TRUE)

ageTbl <- matrix(ageTbl0,8,length(ageTbl0)/8, byrow = FALSE)[-8,]

ageTbl[6,] <- ageTbl[6,] + ageTbl[7,]; ageTbl <- ageTbl[-7,]

rownames(ageTbl) <- c("15-24 years (N)", "25-34 years (N)",
                      "35-44 years (N)", "45-54 years (N)",
                      "55-64 years (N)", "65+ years (N)")


ageTbl <- ageTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(ageTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `15-24 years (N)` + `25-34 years (N)` + `35-44 years (N)` + 
               `45-54 years (N)` + `55-64 years (N)` + `65+ years (N)`, 
           "15-24 years (%)" = round(`15-24 years (N)`/`Total (N)`*100,1), 
           "25-34 years (%)" = round(`25-34 years (N)`/`Total (N)`*100,1),
           "35-44 years (%)" = round(`35-44 years (N)`/`Total (N)`*100,1), 
           "45-54 years (%)" = round(`45-54 years (N)`/`Total (N)`*100,1),
           "55-64 years (%)" = round(`55-64 years (N)`/`Total (N)`*100,1), 
           "65+ years (%)" = round(`65+ years (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, contains("years"), `Total (N)`) %>%
    na.omit() %>%
    gt() %>% 
    tab_header(
        title = paste("Age distribution totals across 64 TB prevalence surveys with standard age groups")) %>%
    fmt_number(drop_trailing_zeros = TRUE); ageTbl

gtsave(data = ageTbl, filename = "output/descriptive/ageTable.pdf") 

##### Study specific CSV export #####
### We will use these to create formatted tables in word.

### Sex studies 
source(here("code/bacterialPositiveIndicator.R"))

sexSurveys <- bactPostIndicator("sex") %>% filter(sex.analysis.indicator !="none")
studySexDetails <- data.frame("Survey ID" = sexSurveys$figure.id, 
                              "Year(s) of study" = sexSurveys$study.years,
                              "WHO region" = gsub("\\(WHO\\)", "region", sexSurveys$WHO.region),
                              "Reported measure of bacteriological TB" = case_when(sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" ~ "Adjusted bacteriological TB prevalence with confidence interval",
                                                                                   sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" ~ "Adjusted smear positive TB prevalence with confidence interval",
                                                                                   sexSurveys$sex.analysis.indicator == "prev100k.ci.bacteriological.tb" ~ "Crude bacteriological TB prevalence with confidence interval",
                                                                                   sexSurveys$sex.analysis.indicator == "prev100k.ci.smear.positive.tb" ~ "Crude smear positive TB prevalence with confidence interval",
                                                                                   sexSurveys$sex.analysis.indicator == "n.bacteriological.tb"  ~ "Count of persons with bacteriologically positive TB",
                                                                                   sexSurveys$sex.analysis.indicator == "n.smear.positive.tb"  ~ "Count of persons with smear positive TB",
                                                                                   sexSurveys$sex.analysis.indicator == "n.culture.positive.tb"  ~ "Count of persons with culture positive TB",
                                                                                   sexSurveys$sex.analysis.indicator == "prev100k.smear.positive.tb"  ~ "Crude smear positive TB prevalence"),
                              "Reported female bacteriological TB prevalence" = case_when(sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" ~ sexSurveys$adj.prev100k.bacteriological.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" ~ sexSurveys$adj.prev100k.smear.positive.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.ci.bacteriological.tb" ~ sexSurveys$prev100k.bacteriological.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.ci.smear.positive.tb" ~ sexSurveys$prev100k.smear.positive.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "n.bacteriological.tb"  ~ sexSurveys$n.bacteriological.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "n.smear.positive.tb"  ~ sexSurveys$n.smear.positive.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "n.culture.positive.tb"  ~ sexSurveys$n.culture.positive.tb.female,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.smear.positive.tb"  ~ sexSurveys$prev100k.smear.positive.tb.female),
                              "Reported male bacteriological TB prevalence" = case_when(sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" ~ sexSurveys$adj.prev100k.bacteriological.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" ~ sexSurveys$adj.prev100k.smear.positive.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.ci.bacteriological.tb" ~ sexSurveys$prev100k.bacteriological.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.ci.smear.positive.tb" ~ sexSurveys$prev100k.smear.positive.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "n.bacteriological.tb"  ~ sexSurveys$n.bacteriological.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "n.smear.positive.tb"  ~ sexSurveys$n.smear.positive.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "n.culture.positive.tb"  ~ sexSurveys$n.culture.positive.tb.male,
                                                                                        sexSurveys$sex.analysis.indicator == "prev100k.smear.positive.tb"  ~ sexSurveys$prev100k.smear.positive.tb.male),
                              "Total participants (N)" = sexSurveys$n.participants.sex.total,
                              "Male participants (%) " = (sexSurveys$n.participants.male/sexSurveys$n.participants.sex.total)*100,
                              "Probability of bias" = ifelse(grepl("Low", sexSurveys$study.quality.summary), "Low", 
                                                    ifelse(grepl("High", sexSurveys$study.quality.summary), "High", "Moderate")),
                                                            check.names = FALSE)

write.csv(studySexDetails, file = here("output/descriptive/sexStudyTable.csv"))              

### Rurality studies 
ruralitySurveys <- cleanDF %>% filter(report.rurality == "Yes")
studyRuralityDetails <- data.frame("Survey ID" = ruralitySurveys$figure.id, 
                                   "WHO region" = ruralitySurveys$World.regions.according.to.WHO,
                                   "Year(s) of study" = ruralitySurveys$study.years,
                                   "Urban participants (N)" = ruralitySurveys$n.participants.urban,
                                   "Urban TB positive (N)" = ruralitySurveys$n.bacteriological.tb.urban,
                                   "Rural participants (N)" = ruralitySurveys$n.participants.rural,
                                   "Rural TB positive (N)" = ruralitySurveys$n.bacteriological.tb.rural,
                                   "Total participants (N)" = ruralitySurveys$n.participants.urban + ruralitySurveys$n.participants.rural,
                                   "Total TB positive (N)" = ruralitySurveys$n.bacteriological.tb.urban + ruralitySurveys$n.bacteriological.tb.rural,
                                   check.names = FALSE)

write.csv(studyRuralityDetails, file = here("output/descriptive/ruralityStudyTable.csv"))  

### HIV studies 
# hivSurveys <- cleanDF %>% filter(report.hiv == "Yes")
hivSurveys <- bactPostIndicator("hiv") %>% filter(hiv.analysis.indicator !="none")
studyHIVDetails <- data.frame("Survey ID" = hivSurveys$figure.id, 
                              "WHO region" = hivSurveys$World.regions.according.to.WHO, #"Africa region", 
                              "Year(s) of study" = hivSurveys$study.years,
                              "HIV+ participants (N)" = hivSurveys$n.participants.hiv.positive,
                              "HIV+ TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.positive, 
                              "HIV- participants (N)" = hivSurveys$n.participants.hiv.negative,
                              "HIV- TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.negative, 
                              "Total participants (N)" = hivSurveys$n.participants.hiv.positive + hivSurveys$n.participants.hiv.negative,
                              "Total TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.positive + hivSurveys$n.bacteriological.tb.hiv.negative, 
                              check.names = FALSE)

write.csv(studyHIVDetails, file = here("output/descriptive/hivStudyTable.csv"))  




