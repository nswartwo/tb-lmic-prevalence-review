##### ABOUT THIS SCRIPT #######################################################
### This script reads in the clean dataset from data folder and then 
### runs fits a Bayesian generalized multivariate multilevel model on the 
### studies that report sex stratified prevalence results. This model aims to
### estimate sex stratified prevalence adjusted for geographic factors. 
##############################################################################|
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
##### MALE TO FEMALE PARTICIPATION RATIOS (WITH CALCULATED CIS) ###############
##############################################################################|
### Calculate the ratios (same as in descriptiveAnalysisFigures.R)

cleanSexDF0 %<>% 
    mutate(ratio.participants.mf = n.participants.male / n.participants.female) 

##############################################################################|
##### CREATE A TIDY DATASET OF RELEVANT VARIABLES #############################
cleanSexDF <- cleanSexDF0 %>% 
    filter(sex.analysis.indicator %in% c("adj.prev100k.ci.bacteriological.tb",
                                         "adj.prev100k.ci.smear.positive.tb")) %>% 
    mutate(adj.prev100k.bacteriological.tb.male = adj.prev100k.bacteriological.tb.male/1e5,
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
           adj.prev100k.ci.lower.smear.positive.tb.female = adj.prev100k.ci.lower.smear.positive.tb.female/1e5) %>%
    select(covidence.id, 
           figure.id.yr, 
           study.geography, 
           title.extracted,
           study.country,
           WHO.region,
           study.start.year, 
           sex.analysis.indicator,
           ends_with("male"), ends_with("sex")) %>%
    
    mutate("tempPrevMale" =  case_when(sex.analysis.indicator == 
                                           "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.male, 
                                       sex.analysis.indicator == 
                                           "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.male),
           "tempPrevFemale" = case_when(sex.analysis.indicator == 
                                            "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.female, 
                                        sex.analysis.indicator == 
                                            "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.female)) %>%
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
                                                                   adj.prev100k.ci.lower.smear.positive.tb.female)/3.92), 
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
                                                                                 (1-adj.prev100k.ci.lower.smear.positive.tb.female)))/3.92), 
           "id" = row_number())

##############################################################################|
##### BAYESIAN MODEL #########################################################
##############################################################################|

##############################################################################|
##### Model 1: Only survey level random effects
##############################################################################|

model1 <- brms::brm(formula = `LogOdds`|se(`LogOddsStandardError`, 
                                           sigma=TRUE) ~ 1 + (1|id) + Sex,
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model1)
plot(model1)
pairs(model1)
pp_check(model1, ndraws = 100)

model1_est0 <- model1 %>% 
    as_draws_df() %>%
    select(b_Intercept, b_SexMale) %>%
    transmute(Female = expit(b_Intercept)*1e5, 
              Male = expit(b_Intercept + b_SexMale)*1e5) %>%
    mutate(mfRatio = Male/Female, 
           model = "Only random effects")

model1_est <- melt(model1_est0)


ggplot(data = model1_est) +
    stat_halfeye(aes(x=value, fill=model), normalize="panels", alpha=0.5) +
    facet_grid(model~fct_relevel(variable,
                                 "Male", "Female", "mfRatio"), 
               scales = "free_x") +
    labs(x="", y="") +
    theme_minimal() + theme(legend.position = "bottom")

##############################################################################|
##### Model 2: Add in country level fixed effects
##############################################################################|

model2 <- brms::brm(formula = LogOdds | se(LogOddsStandardError,
                                           sigma=TRUE) ~ 1 + Sex + study.country + (1|id),
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model2)
plot(model2)

pp_check(model2, ndraws =100)

model2_est <- model2 %>%
    add_epred_draws(newdata = crossing(study.country = unique(cleanSexDF$study.country),
                                       Sex = c("Male", "Female"),
                                       LogOddsStandardError = 1),
                    re_formula = NA) %>%
    transmute(value = expit(.epred)*1e5) %>%
    mutate(model = "Country level fixed effects") %>% ungroup()

mfRatio <- model2_est[1:(dim(model2_est)[1]/2),]
mfRatio$value <- model2_est[((dim(model2_est)[1]/2+1):dim(model2_est)[1]),"value"] /
    model2_est[1:(dim(model2_est)[1]/2),"value"]
mfRatio$Sex <- "mfRatio"

model2_est <- rbind(model2_est, mfRatio)

ggplot(data = model2_est) +
    stat_pointinterval(aes(x=value, colour=study.country),
                       position = position_dodge(width=0.5)) +
    # scale_colour_discrete(limits=rev) +
    facet_grid(model~fct_relevel(Sex,
                                 "Male", "Female")) +
    labs(x="", y="") +
    theme_minimal()

ggplot(data = model2_est %>% filter(Sex == "Male")) +
    geom_point(aes(x=value, color = study.country))

##############################################################################|
##### Model 3: Add in WHO region level fixed effects
##############################################################################|

model3 <- brms::brm(formula = LogOdds | se(LogOddsStandardError, 
                                           sigma=TRUE) ~ 1 + Sex + WHO.region + (1|id),
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model3)
plot(model3)

pp_check(model3, ndraws =100)

model3_est <- model3 %>% 
    add_epred_draws(newdata = crossing(WHO.region = unique(cleanSexDF$WHO.region), 
                                       Sex = c("Male", "Female"),
                                       LogOddsStandardError = 1),
                    re_formula = NA) %>%
    transmute(value = expit(.epred)*1e5) %>%
    mutate(model = "Country level fixed effects") %>% ungroup()

mfRatio <- model3_est[1:(dim(model3_est)[1]/2),]
mfRatio$value <- model3_est[((dim(model3_est)[1]/2+1):dim(model3_est)[1]),"value"] /
    model3_est[1:(dim(model3_est)[1]/2),"value"]
mfRatio$Sex <- "mfRatio"

# model3_est <- rbind(model3_est, mfRatio)

ggplot(data = model3_est) +
    stat_pointinterval(aes(x=value, colour=WHO.region), 
                       position = position_dodge(width=0.5)) +
    scale_colour_discrete(limits=rev) +
    facet_grid(model~fct_relevel(Sex,
                                 "Male", "Female")) +
    labs(x="", y="") +
    theme_ggdist() 

##############################################################################|
##### Model 4: Add in study level effects (multi-level model)
##############################################################################|

model4 <- brms::brm(formula = LogOdds | se(LogOddsStandardError, 
                                           sigma=TRUE) ~ 1 + Sex + (1|id/title.extracted),
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model4)
plot(model4)
pairs(model4)

pp_check(model4, ndraws =100)

model4_est <- model4 %>% 
    as_draws_df() %>%
    transmute(value = expit(.epred)*1e5) %>%
    mutate(model = "Country level fixed effects") %>% ungroup()

mfRatio <- model4_est[1:(dim(model4_est)[1]/2),]
mfRatio$value <- model4_est[((dim(model4_est)[1]/2+1):dim(model4_est)[1]),"value"] /
    model4_est[1:(dim(model4_est)[1]/2),"value"]
mfRatio$Sex <- "mfRatio"

### causes a weird bug to fix 
# model4_est <- rbind(model4_est, mfRatio)

ggplot(data = model4_est) +
    stat_pointinterval(aes(x=value, colour=WHO.region), 
                       position = position_dodge(width=0.5)) +
    scale_colour_discrete(limits=rev) +
    facet_grid(model~fct_relevel(Sex,
                                 "Male", "Female")) +
    labs(x="", y="") +
    theme_ggdist() 
