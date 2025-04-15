##### LOAD IN NECESSARY PACKAGES ##############################################
library(dplyr)
library(here)
library(ggplot2)
library(reshape2)

##### LOAD CLEAN DATA SET AND FILTER TO SURVEYS REPORTING SEX #################
source(here("code/bacterialPositiveIndicator.R"))
cleanSexDF0 <- bactPostIndicator("sex") %>% filter(sex.analysis.indicator !="none")


cleanSexDF <- cleanSexDF0 %>% 
    filter(sex.analysis.indicator %in% c("adj.prev100k.ci.bacteriological.tb",
                                         "adj.prev100k.ci.smear.positive.tb")) %>% 
    mutate("id" = row_number(),
           adj.prev100k.bacteriological.tb.male = adj.prev100k.bacteriological.tb.male/1e5,
           adj.prev100k.ci.upper.bacteriological.tb.male = adj.prev100k.ci.upper.bacteriological.tb.male/1e5,
           adj.prev100k.ci.lower.bacteriological.tb.male = adj.prev100k.ci.lower.bacteriological.tb.male/1e5,
           
           adj.prev100k.bacteriological.tb.female = adj.prev100k.bacteriological.tb.female/1e5,
           adj.prev100k.ci.upper.bacteriological.tb.female = adj.prev100k.ci.upper.bacteriological.tb.female/1e5,
           adj.prev100k.ci.lower.bacteriological.tb.female = adj.prev100k.ci.lower.bacteriological.tb.female/1e5,
           
           adj.prev100k.smear.positive.tb.male = adj.prev100k.smear.positive.tb.male/1e5,
           adj.prev100k.ci.upper.smear.positive.tb.male = adj.prev100k.ci.upper.smear.positive.tb.male/1e5,
           adj.prev100k.ci.lower.smear.positive.tb.male = adj.prev100k.ci.lower.smear.positive.tb.male/1e5,
           
           adj.prev100k.smear.positive.tb.female = adj.prev100k.smear.positive.tb.female/1e5,
           adj.prev100k.ci.upper.smear.positive.tb.female = adj.prev100k.ci.upper.smear.positive.tb.female/1e5,
           adj.prev100k.ci.lower.smear.positive.tb.female = adj.prev100k.ci.lower.smear.positive.tb.female/1e5,
           
           prev100k.bacteriological.tb.male = prev100k.bacteriological.tb.male/1e5,
           prev100k.ci.upper.bacteriological.tb.male = prev100k.ci.upper.bacteriological.tb.male/1e5,
           prev100k.ci.lower.bacteriological.tb.male = prev100k.ci.lower.bacteriological.tb.male/1e5,
           
           prev100k.bacteriological.tb.female = prev100k.bacteriological.tb.female/1e5,
           prev100k.ci.upper.bacteriological.tb.female = prev100k.ci.upper.bacteriological.tb.female/1e5,
           prev100k.ci.lower.bacteriological.tb.female = prev100k.ci.lower.bacteriological.tb.female/1e5,
           
           prev100k.smear.positive.tb.male = prev100k.smear.positive.tb.male/1e5,
           prev100k.ci.upper.smear.positive.tb.male = prev100k.ci.upper.smear.positive.tb.male/1e5,
           prev100k.ci.lower.smear.positive.tb.male = prev100k.ci.lower.smear.positive.tb.male/1e5,
           
           prev100k.smear.positive.tb.female = prev100k.smear.positive.tb.female/1e5,
           prev100k.ci.upper.smear.positive.tb.female = prev100k.ci.upper.smear.positive.tb.female/1e5,
           prev100k.ci.lower.smear.positive.tb.female = prev100k.ci.lower.smear.positive.tb.female/1e5) %>%
    select(id,
           covidence.id, 
           figure.id.yr, 
           study.geography, 
           title.extracted,
           study.country,
           WHO.region,
           study.start.year, 
           study.end.year, 
           sex.analysis.indicator,
           ends_with("male"), ends_with("sex")) %>%
    
    mutate("tempPrevMale" =  case_when(sex.analysis.indicator == 
                                           "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.male, 
                                       sex.analysis.indicator == 
                                           "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.male,
                                       sex.analysis.indicator == 
                                           "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.male, 
                                       sex.analysis.indicator == 
                                           "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.male),
           "tempPrevFemale" = case_when(sex.analysis.indicator == 
                                            "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.female, 
                                        sex.analysis.indicator == 
                                            "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.female,
                                        sex.analysis.indicator == 
                                            "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.female, 
                                        sex.analysis.indicator == 
                                            "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.female)) %>%
    pivot_longer(cols = c(tempPrevMale,
                          tempPrevFemale), 
                 names_to = "Sex", 
                 values_to = "Adjusted Prevalence", 
                 names_prefix = "tempPrev") %>% 
    mutate("Standard Error" = case_when(sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" & 
                                            Sex == "Male" ~ (adj.prev100k.ci.upper.bacteriological.tb.male-
                                                                 adj.prev100k.ci.lower.bacteriological.tb.male)/3.92,
                                        sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" & 
                                            Sex == "Female" ~ (adj.prev100k.ci.upper.bacteriological.tb.female-
                                                                   adj.prev100k.ci.lower.bacteriological.tb.female)/3.92,
                                        sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" & 
                                            Sex == "Male" ~ (adj.prev100k.ci.upper.smear.positive.tb.male-
                                                                 adj.prev100k.ci.lower.smear.positive.tb.male)/3.92,
                                        sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" & 
                                            Sex == "Female" ~ (adj.prev100k.ci.upper.smear.positive.tb.female-
                                                                   adj.prev100k.ci.lower.smear.positive.tb.female)/3.92,
                                        sex.analysis.indicator == "prev100k.ci.bacteriological.tb" & 
                                            Sex == "Male" ~ (prev100k.ci.upper.bacteriological.tb.male-
                                                                 prev100k.ci.lower.bacteriological.tb.male)/3.92,
                                        sex.analysis.indicator == "prev100k.ci.bacteriological.tb" & 
                                            Sex == "Female" ~ (prev100k.ci.upper.bacteriological.tb.female-
                                                                   prev100k.ci.lower.bacteriological.tb.female)/3.92,
                                        sex.analysis.indicator == "prev100k.ci.smear.positive.tb" & 
                                            Sex == "Male" ~ (prev100k.ci.upper.smear.positive.tb.male-
                                                                 prev100k.ci.lower.smear.positive.tb.male)/3.92,
                                        sex.analysis.indicator == "prev100k.ci.smear.positive.tb" & 
                                            Sex == "Female" ~ (prev100k.ci.upper.smear.positive.tb.female-
                                                                   prev100k.ci.lower.smear.positive.tb.female)/3.92), 
           "Phi" = (`Adjusted Prevalence`/`Standard Error`^2) - (`Adjusted Prevalence`/`Standard Error`)^2 - 1, 
           "LogOdds" = log(`Adjusted Prevalence`/(1-`Adjusted Prevalence`)), 
           "LogOddsStandardErrorAdj" = case_when(sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
                                                     Sex == "Male" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.male/
                                                                              (1-adj.prev100k.ci.upper.bacteriological.tb.male)) -
                                                                          log(adj.prev100k.ci.lower.bacteriological.tb.male/
                                                                                  (1-adj.prev100k.ci.lower.bacteriological.tb.male))) /3.92,
                                                 sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
                                                     Sex == "Female" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.female/
                                                                                (1-adj.prev100k.ci.upper.bacteriological.tb.female)) -
                                                                            log(adj.prev100k.ci.lower.bacteriological.tb.female /
                                                                                    (1-adj.prev100k.ci.lower.bacteriological.tb.female))) /3.92,
                                                 sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" &
                                                     Sex == "Male" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.male/
                                                                              (1-adj.prev100k.ci.upper.smear.positive.tb.male)) -
                                                                          log(adj.prev100k.ci.lower.smear.positive.tb.male /
                                                                                  (1-adj.prev100k.ci.lower.smear.positive.tb.male)))/3.92,
                                                 sex.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" &
                                                     Sex == "Female" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.female/
                                                                                (1-adj.prev100k.ci.upper.smear.positive.tb.female)) -
                                                                            log(adj.prev100k.ci.lower.smear.positive.tb.female /
                                                                                    (1-adj.prev100k.ci.lower.smear.positive.tb.female)))/3.92),
           "LogOddsStandardErrorCrude" = case_when(sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
                                                       Sex == "Male" ~ (log(prev100k.ci.upper.bacteriological.tb.male/
                                                                                (1-prev100k.ci.upper.bacteriological.tb.male)) -
                                                                            log(prev100k.ci.lower.bacteriological.tb.male/
                                                                                    (1-prev100k.ci.lower.bacteriological.tb.male))) /3.92,
                                                   sex.analysis.indicator == "prev100k.ci.bacteriological.tb" &
                                                       Sex == "Female" ~ (log(prev100k.ci.upper.bacteriological.tb.female/
                                                                                  (1-prev100k.ci.upper.bacteriological.tb.female)) -
                                                                              log(prev100k.ci.lower.bacteriological.tb.female /
                                                                                      (1-prev100k.ci.lower.bacteriological.tb.female))) /3.92,
                                                   sex.analysis.indicator == "prev100k.ci.smear.positive.tb" &
                                                       Sex == "Male" ~ (log(prev100k.ci.upper.smear.positive.tb.male/
                                                                                (1-prev100k.ci.upper.smear.positive.tb.male)) -
                                                                            log(prev100k.ci.lower.smear.positive.tb.male /
                                                                                    (1-prev100k.ci.lower.smear.positive.tb.male)))/3.92,
                                                   sex.analysis.indicator == "prev100k.ci.smear.positive.tb" &
                                                       Sex == "Female" ~ (log(prev100k.ci.upper.smear.positive.tb.female/
                                                                                  (1-prev100k.ci.upper.smear.positive.tb.female)) -
                                                                              log(prev100k.ci.lower.smear.positive.tb.female /
                                                                                      (1-prev100k.ci.lower.smear.positive.tb.female)))/3.92))


bactBothCIs <- cleanSexDF %>% filter(
    !is.na(adj.prev100k.ci.bacteriological.tb.male) & 
        !is.na(prev100k.ci.bacteriological.tb.male) == TRUE) %>% 
    mutate("StandardErrorAdj" = case_when(Sex == "Male" ~ (adj.prev100k.ci.upper.bacteriological.tb.male-
                                                               adj.prev100k.ci.lower.bacteriological.tb.male)/3.92,
                                          Sex == "Female" ~ (adj.prev100k.ci.upper.bacteriological.tb.female-
                                                                 adj.prev100k.ci.lower.bacteriological.tb.female)/3.92),
           "StandardErrorCrude" = case_when(Sex == "Male" ~ (prev100k.ci.upper.bacteriological.tb.male-
                                                                 prev100k.ci.lower.bacteriological.tb.male)/3.92,
                                            Sex == "Female" ~ (prev100k.ci.upper.bacteriological.tb.female-
                                                                   prev100k.ci.lower.bacteriological.tb.female)/3.92),
           "LogOddsAdj" = case_when(Sex == "Male" ~ log(adj.prev100k.bacteriological.tb.male/(1-adj.prev100k.bacteriological.tb.male)),
                                    Sex == "Female" ~ log(adj.prev100k.bacteriological.tb.female/(1-adj.prev100k.bacteriological.tb.female))),
           "LogOddsCrude" = case_when(Sex == "Male" ~ log(prev100k.bacteriological.tb.male/(1-adj.prev100k.bacteriological.tb.male)),
                                      Sex == "Female" ~ log(prev100k.bacteriological.tb.female/(1-adj.prev100k.bacteriological.tb.female))),
           "LogOddsStandardErrorAdj" = case_when( Sex == "Male" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.male/
                                                                           (1-adj.prev100k.ci.upper.bacteriological.tb.male)) -
                                                                       log(adj.prev100k.ci.lower.bacteriological.tb.male/
                                                                               (1-adj.prev100k.ci.lower.bacteriological.tb.male))) /3.92,
                                                  Sex == "Female" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.female/
                                                                             (1-adj.prev100k.ci.upper.bacteriological.tb.female)) -
                                                                         log(adj.prev100k.ci.lower.bacteriological.tb.female /
                                                                                 (1-adj.prev100k.ci.lower.bacteriological.tb.female))) /3.92),
           "LogOddsStandardErrorCrude" = case_when( Sex == "Male" ~ (log(prev100k.ci.upper.bacteriological.tb.male/
                                                                             (1-prev100k.ci.upper.bacteriological.tb.male)) -
                                                                         log(prev100k.ci.lower.bacteriological.tb.male/
                                                                                 (1-prev100k.ci.lower.bacteriological.tb.male))) /3.92,
                                                    Sex == "Female" ~ (log(prev100k.ci.upper.bacteriological.tb.female/
                                                                               (1-prev100k.ci.upper.bacteriological.tb.female)) -
                                                                           log(prev100k.ci.lower.bacteriological.tb.female /
                                                                                   (1-prev100k.ci.lower.bacteriological.tb.female))) /3.92)) %>%
    select(figure.id.yr, 
           Sex,
           LogOdds,
           LogOddsStandardErrorAdj,
           LogOddsStandardErrorCrude) 

smrBothCIs <- cleanSexDF %>% filter(
    !is.na(adj.prev100k.ci.smear.positive.tb.male) & 
        !is.na(prev100k.ci.smear.positive.tb.male) == TRUE) %>% 
    mutate("StandardErrorAdj" = case_when(Sex == "Male" ~ (adj.prev100k.ci.upper.smear.positive.tb.male-
                                                               adj.prev100k.ci.lower.smear.positive.tb.male)/3.92,
                                          Sex == "Female" ~ (adj.prev100k.ci.upper.smear.positive.tb.female-
                                                                 adj.prev100k.ci.lower.smear.positive.tb.female)/3.92),
           "StandardErrorCrude" = case_when(Sex == "Male" ~ (prev100k.ci.upper.smear.positive.tb.male-
                                                                 prev100k.ci.lower.smear.positive.tb.male)/3.92,
                                            Sex == "Female" ~ (prev100k.ci.upper.smear.positive.tb.female-
                                                                   prev100k.ci.lower.smear.positive.tb.female)/3.92),
           "LogOddsAdj" = case_when(Sex == "Male" ~ log(adj.prev100k.smear.positive.tb.male/(1-adj.prev100k.smear.positive.tb.male)),
                                    Sex == "Female" ~ log(adj.prev100k.smear.positive.tb.female/(1-adj.prev100k.smear.positive.tb.female))),
           "LogOddsCrude" = case_when(Sex == "Male" ~ log(prev100k.smear.positive.tb.male/(1-adj.prev100k.smear.positive.tb.male)),
                                      Sex == "Female" ~ log(prev100k.smear.positive.tb.female/(1-adj.prev100k.smear.positive.tb.female))),
           "LogOddsStandardErrorAdj" = case_when( Sex == "Male" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.male/
                                                                           (1-adj.prev100k.ci.upper.smear.positive.tb.male)) -
                                                                       log(adj.prev100k.ci.lower.smear.positive.tb.male/
                                                                               (1-adj.prev100k.ci.lower.smear.positive.tb.male))) /3.92,
                                                  Sex == "Female" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.female/
                                                                             (1-adj.prev100k.ci.upper.smear.positive.tb.female)) -
                                                                         log(adj.prev100k.ci.lower.smear.positive.tb.female /
                                                                                 (1-adj.prev100k.ci.lower.smear.positive.tb.female))) /3.92),
           "LogOddsStandardErrorCrude" = case_when( Sex == "Male" ~ (log(prev100k.ci.upper.smear.positive.tb.male/
                                                                             (1-prev100k.ci.upper.smear.positive.tb.male)) -
                                                                         log(prev100k.ci.lower.smear.positive.tb.male/
                                                                                 (1-prev100k.ci.lower.smear.positive.tb.male))) /3.92,
                                                    Sex == "Female" ~ (log(prev100k.ci.upper.smear.positive.tb.female/
                                                                               (1-prev100k.ci.upper.smear.positive.tb.female)) -
                                                                           log(prev100k.ci.lower.smear.positive.tb.female /
                                                                                   (1-prev100k.ci.lower.smear.positive.tb.female))) /3.92)) %>%
    filter(is.na(adj.prev100k.ci.bacteriological.tb.male) & is.na(adj.prev100k.ci.bacteriological.tb.male)) %>% 
    select(figure.id.yr, 
           Sex,
           LogOdds,
           LogOddsStandardErrorAdj,
           LogOddsStandardErrorCrude) 

totalBothCIs0 <- full_join(bactBothCIs, smrBothCIs) %>% 
    mutate(calc.design.effect = LogOddsStandardErrorAdj/LogOddsStandardErrorCrude,
           sampleVar = LogOddsStandardErrorCrude^2,
           
           clusterVar = LogOddsStandardErrorAdj^2 - sampleVar, 
           percClusterSE = sqrt(abs(clusterVar))/LogOddsStandardErrorAdj)

totalBothCIs0$figure.id.yr <- factor(totalBothCIs0$figure.id.yr, 
                                     levels = c(sort(unique(totalBothCIs0$figure.id.yr), decreasing = FALSE)))
###############################################################################

meanFemale <- (as.numeric(totalBothCIs0  %>% filter (Sex =="Female") %>% summarize(mean(clusterVar))))
meanMale <- (as.numeric(totalBothCIs0  %>% filter (Sex =="Male") %>% summarize(mean(clusterVar))))

ggplot(data = totalBothCIs0) +
    geom_point(aes(x=figure.id.yr, y=clusterVar, color = Sex), 
               size = 5) +
    theme_minimal(base_size = 18) + coord_flip() + 
    xlab("Survey ID") + 
    geom_hline(yintercept=meanFemale,
               linetype="dashed", size=1, color = "#F8766D")+
    geom_hline(yintercept=meanMale,
               linetype="dashed", size=1, color="#00BFC4")

###############################################################################
### Single estimate across both sexes 
meanTotal <- (as.numeric(totalBothCIs0 %>% summarize(mean(clusterVar))))
