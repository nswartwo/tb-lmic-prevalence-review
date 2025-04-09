##### LOAD IN NECESSARY PACKAGES ##############################################
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(here)
library(magrittr)
library(DT)
library(metafor)
library(meta)
library(brms)
library(posterior)
library(tidybayes)

library(ggplot2)

expit <- function (x) {exp(x)/(1 + exp(x))}
##############################################################################|
##### LOAD CLEAN DATA SET AND FILTER TO SURVEYS REPORTING SEX #################
source(here("code/bacterialPositiveIndicator.R"))
cleanSexDF0 <- bactPostIndicator("sex") %>% filter(sex.analysis.indicator !="none")

##############################################################################|
##### CREATE A TIDY DATASET OF RELEVANT VARIABLES #############################
cleanSexDF <- cleanSexDF0 %>% 
    # filter(sex.analysis.indicator %in% c("adj.prev100k.ci.bacteriological.tb",
    #                                      "adj.prev100k.ci.smear.positive.tb",
    #                                      "prev100k.ci.bacteriological.tb",
    #                                      "prev100k.ci.smear.positive.tb")) %>% 
    filter(sex.analysis.indicator != "none") %>% 
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
           ends_with("male"), ends_with("sex"),
           sex.analysis.indicator) %>%
    
    mutate("tempPrevMale" =  case_when(sex.analysis.indicator == 
                                           "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.male, 
                                       sex.analysis.indicator == 
                                           "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.male,
                                       sex.analysis.indicator == 
                                           "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.male, 
                                       sex.analysis.indicator == 
                                           "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.male, 
                                       sex.analysis.indicator == 
                                           "n.bacteriological.tb" ~ n.bacteriological.tb.male/n.participants.male, 
                                       sex.analysis.indicator == 
                                           "n.smear.positive.tb" ~ n.smear.positive.tb.male/n.participants.male,
                                       sex.analysis.indicator == 
                                           "n.culture.positive.tb" ~ n.culture.positive.tb.male/n.participants.male,
                                       sex.analysis.indicator == 
                                           "prev100k.smear.positive.tb" ~ prev100k.smear.positive.tb.male),
           "tempPrevFemale" = case_when(sex.analysis.indicator == 
                                            "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.female, 
                                        sex.analysis.indicator == 
                                            "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.female,
                                        sex.analysis.indicator == 
                                            "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.female, 
                                        sex.analysis.indicator == 
                                            "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.female, 
                                        sex.analysis.indicator == 
                                            "n.bacteriological.tb" ~ n.bacteriological.tb.female/n.participants.female, 
                                        sex.analysis.indicator == 
                                            "n.smear.positive.tb" ~ n.smear.positive.tb.female/n.participants.female,
                                        sex.analysis.indicator == 
                                            "n.culture.positive.tb" ~ n.culture.positive.tb.female/n.participants.female,
                                        sex.analysis.indicator == 
                                            "prev100k.smear.positive.tb" ~ prev100k.smear.positive.tb.female)) %>%
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
                                                                   prev100k.ci.lower.smear.positive.tb.female)/3.92,
                                        sex.analysis.indicator %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                      "n.culture.positive.tb", "prev100k.smear.positive.tb") & Sex == "Male" ~ 
                                            sqrt((`Adjusted Prevalence`*(1-`Adjusted Prevalence`))/n.participants.male),
                                        sex.analysis.indicator  %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                       "n.culture.positive.tb", "prev100k.smear.positive.tb") & Sex == "Female" ~ 
                                            sqrt((`Adjusted Prevalence`*(1-`Adjusted Prevalence`))/n.participants.female)),
           
           "Phi" = (`Adjusted Prevalence`/`Standard Error`^2) - (`Adjusted Prevalence`/`Standard Error`)^2 - 1, 
           "LogOdds" = log(`Adjusted Prevalence`/(1-`Adjusted Prevalence`)), 
           "LogOddsStandardError" = case_when(sex.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
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
                                                                                 (1-adj.prev100k.ci.lower.smear.positive.tb.female)))/3.92,
                                              sex.analysis.indicator == "prev100k.ci.bacteriological.tb" &
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
                                                                                 (1-prev100k.ci.lower.smear.positive.tb.female)))/3.92, 
                                              
                                              sex.analysis.indicator %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                            "n.culture.positive.tb", "prev100k.smear.positive.tb") & Sex == "Male" ~ 
                                                  log(1/(n.participants.male*`Adjusted Prevalence`*(1-`Adjusted Prevalence`)))^2,
                                              sex.analysis.indicator  %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                             "n.culture.positive.tb", "prev100k.smear.positive.tb") & Sex == "Female" ~ 
                                                  log(1/(n.participants.female*`Adjusted Prevalence`*(1-`Adjusted Prevalence`)))^2
           ))

#### Main effect model 
mainEffectMod <- brms::brm(formula = `LogOdds`|se(`LogOddsStandardError`, 
                                           sigma=TRUE) ~ 1 + Sex + (1 + Sex | study.country) + (1|id),
                    data = cleanSexDF,
                    prior = prior(normal(0,10), class=Intercept) +
                        prior(normal(0, 10), class = b) +
                        prior(exponential(1), class=sd),
                    family = "gaussian",
                    control = list(adapt_delta = 0.90),
                    cores = 4,
                    chains = 4,
                    iter = 4000)


nd <- cleanSexDF %>%
    select(WHO.region, study.country, Sex) %>%
    distinct() %>%
    mutate(LogOddsStandardError=1)

mainEffectModEst <- mainEffectMod %>%
    add_epred_draws(newdata = nd, re_formula = ~(1 + Sex)) %>%
    ungroup() %>%
    mutate(value = expit(.epred)*1e5) %>%
    select(study.country, WHO.region, Sex, value, .draw) %>%
    pivot_wider(
        names_from = Sex,
        values_from = value
    ) %>%
    unnest_longer(col = c(Male, Female)) %>%
    mutate(mfRatio = Male/Female) %>%
    pivot_longer(cols = c(Female, Male, mfRatio)) %>% 
    rename(Sex = name)


mainEffectModEst %>% group_by(Sex) %>% mean_qi(value)


### Region Model 
regionEffectMod <- brms::brm(formula = `LogOdds`|se(`LogOddsStandardError`, 
                                                  sigma=TRUE) ~ 1 + Sex + (1 + Sex | WHO.region/study.country) + (1|id),
                           data = cleanSexDF,
                           prior = prior(normal(0,10), class=Intercept) +
                               prior(normal(0, 10), class = b) +
                               prior(exponential(1), class=sd),
                           family = "gaussian",
                           control = list(adapt_delta = 0.90),
                           cores = 4,
                           chains = 4,
                           iter = 4000)



regionEffectModEst <- regionEffectMod %>%
    add_epred_draws(newdata = nd, re_formula = ~(1 + Sex | WHO.region)) %>%
    ungroup() %>%
    mutate(value = expit(.epred)*1e5) %>%
    select(study.country, WHO.region, Sex, value, .draw) %>%
    pivot_wider(
        names_from = Sex,
        values_from = value
    ) %>%
    unnest_longer(col = c(Male, Female)) %>%
    mutate(mfRatio = Male/Female) %>%
    pivot_longer(cols = c(Female, Male, mfRatio)) %>% 
    rename(Sex = name)


regionEffectModEst %>% group_by(Sex, WHO.region) %>% mean_qi(value)


### Get the empirical values 

region_empirical <- cleanSexDF %>%
    select(id, WHO.region, study.country, Sex, LogOdds) %>%
    mutate(value = expit(LogOdds)*1e5) %>%
    pivot_wider(names_from=Sex,
                values_from = value,
                id_cols = c(id, study.country, WHO.region)) %>%
    mutate(mfRatio = Male/Female) %>%
    pivot_longer(cols = c(Male, Female, mfRatio))

regionEffectModEst %>%
    ggplot() +
    geom_vline(data = region_empirical %>% filter(name=="mfRatio"),
               aes(xintercept = 1), colour="grey50", linetype=2) +
    stat_pointinterval(aes(x = value, y = WHO.region), .width = 0.95) +  
    geom_point(data = region_empirical, aes(y = WHO.region, x = value),
               colour = "mediumseagreen", shape = 1) +
    facet_wrap(~name, scale="free_x") +
    scale_y_discrete(limits=rev)


