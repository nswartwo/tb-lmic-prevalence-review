library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gt)
library(ggpubr)
####################
##### Needs to be functionalized so it can be used by all strata ######
source(here("code/bacterialPositiveIndicator.R"))
cleanDF0 <- bactPostIndicator("sex", FALSE)
table(cleanDF0$sex.analysis.indicator)


### Sex studies 
sexSurveys <- cleanDF0 %>% filter(sex.analysis.indicator != "none")

femaleEstimates <- maleEstimates <- vector()

for (i in 1:nrow(sexSurveys)){
    if(sexSurveys$sex.analysis.indicator[i] %in% c("adj.prev100k.ci.bacteriological.tb", 
                                                   "adj.prev100k.ci.smear.positive.tb", 
                                                   "prev100k.ci.bacteriological.tb", 
                                                   "prev100k.ci.smear.positive.tb")){
        maleEstimates[i] <- paste0(sexSurveys[i, paste0(gsub(".ci", "", sexSurveys$sex.analysis.indicator), ".male")[i]],
                                  " (",
                                  sexSurveys[i, paste0(sexSurveys$sex.analysis.indicator, ".male")[i]], ")")
        femaleEstimates[i] <- paste0(sexSurveys[i, paste0(gsub(".ci", "", sexSurveys$sex.analysis.indicator), ".female")[i]],
                                   " (",
                                   sexSurveys[i, paste0(sexSurveys$sex.analysis.indicator, ".female")[i]], ")")
    } else {
        maleEstimates[i] <- sexSurveys[i,paste0(sexSurveys$sex.analysis.indicator, ".male")[i]]
        femaleEstimates[i] <- sexSurveys[i,paste0(sexSurveys$sex.analysis.indicator, ".female")[i]]
    }
}

indicatorLabels <- c("Adjusted bacteriological positive TB prevalence", 
                     "Count of culture positive TB", 
                     "Count of bacteriological positive TB",
                     "Count of smear positive TB", 
                     "Crude bacteriological positive TB prevalence",
                     "Crude smear positive TB prevalence", 
                     "Adjusted smear positive TB prevalence", 
                     "Crude smear positive TB prevalence (no CI)")

names(indicatorLabels) <- unique(sexSurveys$sex.analysis.indicator)

studySexDetails <- data.frame("Survey ID" = sexSurveys$figure.id, 
                              # "title" = sexSurveys$title.extracted,
                              "Year(s) of study" = sexSurveys$study.years,
                              "WHO region" = gsub("\\(WHO\\)", "region", sexSurveys$WHO.region),

                              "Reported measure of bacteriological TB" = indicatorLabels[sexSurveys$sex.analysis.indicator],
                              "Female TB prevalence (as reported)" = femaleEstimates, 
                              "Male TB prevalence (as reported)" = maleEstimates,  
                              "Female TB prevalence (as modelled)" = expit(bactModelDF$LogOdds_Female)*1e5, 
                              "Male TB prevalence (as modelled)" = expit(bactModelDF$LogOdds_Male)*1e5, 
                              "Total participants (N)" = sexSurveys$n.participants.sex.total,
                              "Male participants (%)" = round((sexSurveys$n.participants.male/sexSurveys$n.participants.sex.total)*100,2),
                              "Probability of bias" = ifelse(grepl("Low", sexSurveys$study.quality.summary), "Low", 
                                                             ifelse(grepl("High", sexSurveys$study.quality.summary), "High", "Moderate")),
                              check.names = FALSE)

write.csv(studySexDetails, file = here("output/descriptive/sexStudyTableV2.csv"))    

##### Totals by sex #####
sexTbl0 <- sexSurveys %>% 
    dplyr::select(starts_with("n.") & ends_with(c("male", "female", "sex.total"))) %>% 
    colSums(na.rm=TRUE)

sexTbl <- matrix(sexTbl0,3,length(sexTbl0)/3, byrow = FALSE)[-3,]

rownames(sexTbl) <- c("Male (N)", "Female (N)")


sexTblGT <- sexTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(sexTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `Male (N)` + `Female (N)`, 
           "Female (%)" = round(`Female (N)`/`Total (N)`*100,1), 
           "Male (%)" = round(`Male (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, `Female (N)`, `Female (%)`, `Male (N)`,`Male (%)`, `Total (N)`) %>%
    gt() %>% 
    tab_header(
        title = paste("Sex distribution totals across", sum(sexSurveys$sex.analysis.indicator!="None"),  "TB prevalence surveys")) %>%
    fmt_number(drop_trailing_zeros = TRUE); sexTblGT

sexTblCSV <- sexTbl %>% t() %>% as.data.frame() %>% 
    mutate("Description" = unique(sapply(names(sexTbl0), function(x) sub("[^.]+\\.([^.]+)\\..*", "\\1", x))),
           "Total (N)" = `Male (N)` + `Female (N)`, 
           "Female (%)" = round(`Female (N)`/`Total (N)`*100,1), 
           "Male (%)" = round(`Male (N)`/`Total (N)`*100,1)) %>% 
    dplyr::select(Description, `Female (N)`, `Female (%)`, `Male (N)`,`Male (%)`, `Total (N)`) 

gtsave(data = sexTblGT, filename = "output/descriptive/sexTable.pdf") 
write_csv(x = sexTblCSV, file= "output/descriptive/sexTableCumulative.csv") 


#### Number of countries in each WHO region
table((regionWHO %>% filter(study.country %in% unique(sexSurveys$study.country)))$WHO.region)
