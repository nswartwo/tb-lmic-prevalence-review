##### ABOUT THIS SCRIPT #######################################################
### This script reads in the clean dataset from data folder and then 
### runs fits a Bayesian generalized multivariate multilevel model on the 
### studies that report sex stratified prevalence results. This model aims to
### estimate sex stratified prevalence adjusted for geographic factors. 
##############################################################################|
##### LOAD IN NECESSARY PACKAGES ##############################################
library(dplyr)
library(here)
library(magrittr)
library(DT)
library(metafor)
library(meta)
library(brms)
library(posterior)
library(tidybayes)
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

### Calculate the confidence intervals 
    # maleParticipants <- cleanSexDF0[, c("n.participants.male", "n.participants.sex.total")]
    # stats::binom.test(x=cleanSexDF0$n.participants.male[1], n = cleanSexDF0$n.participants.sex.total[1])

##############################################################################|
##### SEX STRATIFIED BACTERIOLOGICALLY POSITIVE TB PREVALENCE   ###############
###############################################################################


### Calculation of the bacteriological TB prevalence among males 
### Random effects weighted model 
### Use only those surveys which report counts of participants and 
### bacteriological TB. 
prevSexBact <- prevSexFilt %>% 
    filter(is.na(n.bacteriological.tb.male)== FALSE &
               is.na(n.participants.male)== FALSE)



propBactMale <-metaprop(n.bacteriological.tb.male, n.participants.male, figure.id.yr, 
                         data=prevSexBact2, random=TRUE, 
                         pscale = 1e5)

summary(propBactMale)

prevSexBact2 <- prevSexFilt %>% 
                mutate(n.bacteriological.tb.male = ifelse(is.na(n.bacteriological.tb.male) &
                                                         !is.na(prev100k.bacteriological.tb.male),
                                                         round(prev100k.bacteriological.tb.male*n.participants.male/1e5),
                                                         n.bacteriological.tb.male)) %>%
                filter(is.na(n.bacteriological.tb.male)== FALSE &
                       is.na(n.participants.male)== FALSE)

propBactMale2 <-metaprop(n.bacteriological.tb.male, n.participants.male, figure.id.yr, 
                         data=prevSexBact2, random=TRUE, 
                         pscale = 1e5)

summary(propBactMale2)

### Proper calculation of the bacteriological TB prevalence among females 
### Random effects weighted model 
propBactFemale <-metaprop(n.bacteriological.tb.female, n.participants.female, figure.id.yr, 
                        data=prevSexBact, random=TRUE,
                        pscale = 1e5)

summary(propBactFemale)

##############################################################################|
##### MALE TO FEMALE RATIO OF BACTERIOLOGICAL PREVALENCE  #####################
##############################################################################|

prevSexFilt %>% filter(is.na(n.participants.male)== TRUE &  
                       is.na(n.bacteriological.tb.male)== TRUE &
                        is.na(prev100k.bacteriological.tb.male)== TRUE &
                           is.na(adj.prev100k.bacteriological.tb.male)== FALSE)

bactPrevMF <- rma(ai=n.bacteriological.tb.male, 
                  bi=n.participants.male - n.bacteriological.tb.male, 
                  ci=n.bacteriological.tb.female, 
                  di=n.participants.female - n.bacteriological.tb.female, 
                  data=prevSexBact, measure="RR", 
                  slab=paste(figure.id.yr), method="REML")

forest(bactPrevMF,
       atransf=exp, cex=.75,  xlab="M:F ratio", mlab="Overall summary", 
       psize=1, col="black", border="black", )

##############################################################################|
##### SEX STRATIFIED SMEAR POSITIVE TB PREVALENCE   ###########################
##############################################################################|

### Proper calculation of the bacteriological TB prevalence among males 
### Random effects weighted model 
prevSexSmear <- prevSexFilt %>% filter(is.na(n.smear.positive.tb.male)== FALSE &
                                          is.na(n.participants.male)== FALSE )

propSmearMale <-metaprop(n.smear.positive.tb.male, n.participants.male, figure.id.yr, 
                        data=prevSexSmear, random=TRUE, pscale = 1e5)

summary(propSmearMale)

### Proper calculation of the bacteriological TB prevalence among females 
### Random effects weighted model 
propSmearFemale <-metaprop(n.smear.positive.tb.female, n.participants.female, figure.id.yr, 
                          data=prevSexSmear, random=TRUE, pscale = 1e5)

summary(propSmearFemale)

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
                     study.country,
                     WHO.region,
                     study.years, 
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
                                                                                      (1-adj.prev100k.ci.lower.smear.positive.tb.female)))/3.92))

##############################################################################|
##### BAYESIAN MODEL #########################################################
##############################################################################|

##### Model 1: Only survey level random effects
model1 <- brms::brm(formula = `LogOdds`|se(`LogOddsStandardError`, sigma=TRUE) ~ 1 + (1|figure.id.yr) + Sex,
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model1)
plot(model1)

pp_check(model1, ndraws =100)


##### Model 2: Add in country level fixed effects
model2 <- brms::brm(formula = `LogOdds`|se(`LogOddsStandardError`, sigma=TRUE) ~ 1 + (1|figure.id.yr) + Sex + study.country,
                    data = cleanSexDF,
                    family = "gaussian",
                    control = list(adapt_delta = 0.99),
                    cores = 4,
                    chains = 4,
                    iter = 4000)

summary(model2)
plot(model2)

pp_check(model2, ndraws =100)


