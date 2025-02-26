##### ABOUT THIS SCRIPT #######################################################
### This script reads in the clean dataset from data folder and then 
### replicates the methods in Horton et. al to determine change.  
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

### Open a PDF 
pdf(file = here("output/HortonAnalysis.pdf"), width = 8.5, height = 11.5)

### Calculation of the bacteriological TB prevalence among males 
### Random effects weighted model 
### Option 1: Use only those surveys which report counts of 
### participants and bacteriological TB. 
prevSexBact <- cleanSexDF0 %>% 
    filter(is.na(n.bacteriological.tb.male)== FALSE &
               is.na(n.participants.male)== FALSE)


### Males 
propBactMale <-metaprop(n.bacteriological.tb.male, n.participants.male, figure.id.yr, 
                        data=prevSexBact, random=TRUE, 
                        pscale = 1e5)

summary(propBactMale)

### Females 
propBactFemale <-metaprop(n.bacteriological.tb.female, n.participants.female, figure.id.yr, 
                          data=prevSexBact, random=TRUE,
                          pscale = 1e5)

print(summary(propBactFemale))

### Option 2: Use those surveys which report counts of participants
### and bacteriological TB AND those that report prev100k. 

prevSexBact2 <- cleanSexDF0 %>% 
    mutate(n.bacteriological.tb.male = ifelse(is.na(n.bacteriological.tb.male) &
                                              !is.na(prev100k.bacteriological.tb.male),
                                              round(prev100k.bacteriological.tb.male*n.participants.male/1e5),
                                              n.bacteriological.tb.male),
           n.bacteriological.tb.female = ifelse(is.na(n.bacteriological.tb.female) &
                                              !is.na(prev100k.bacteriological.tb.female),
                                              round(prev100k.bacteriological.tb.female*n.participants.female/1e5),
                                              n.bacteriological.tb.female)) %>%
    filter(is.na(n.bacteriological.tb.male)== FALSE &
           is.na(n.participants.male)== FALSE)

### Males
propBactMale2 <-metaprop(n.bacteriological.tb.male, n.participants.male, figure.id.yr, 
                         data=prevSexBact2, random=TRUE, 
                         pscale = 1e5)

summary(propBactMale2)

### Females
propBactFemale2 <-metaprop(n.bacteriological.tb.female, n.participants.female, figure.id.yr, 
                         data=prevSexBact2, random=TRUE, 
                         pscale = 1e5)

summary(propBactFemale2)

##############################################################################|
##### MALE TO FEMALE RATIO OF BACTERIOLOGICAL PREVALENCE  #####################
##############################################################################|

bactPrevMF <- rma(ai=n.bacteriological.tb.male, 
                  bi=n.participants.male - n.bacteriological.tb.male, 
                  ci=n.bacteriological.tb.female, 
                  di=n.participants.female - n.bacteriological.tb.female, 
                  data=prevSexBact, measure="RR", 
                  slab=paste(figure.id.yr), method="REML")

forest(bactPrevMF,
       atransf=exp, cex=.75,  xlab="M:F ratio (Bacteriologically-Positive TB)", mlab="Overall summary", 
       psize=1, col="black", border="black" )

##############################################################################|
##### SEX STRATIFIED SMEAR POSITIVE TB PREVALENCE   ###########################
##############################################################################|

### Calculation of smear positive TB prevalence among males 
### Random effects weighted model 
prevSexSmear <- cleanSexDF0 %>% filter(is.na(n.smear.positive.tb.male)== FALSE &
                                           is.na(n.participants.male)== FALSE )

propSmearMale <-metaprop(n.smear.positive.tb.male, n.participants.male, figure.id.yr, 
                         data=prevSexSmear, random=TRUE, pscale = 1e5)

summary(propSmearMale)

### Calculation of smear positive TB prevalence among females 
### Random effects weighted model 
propSmearFemale <-metaprop(n.smear.positive.tb.female, n.participants.female, figure.id.yr, 
                           data=prevSexSmear, random=TRUE, pscale = 1e5)

summary(propSmearFemale)

##############################################################################|
##### MALE TO FEMALE RATIO OF SMEAR POSITIVE PREVALENCE  ######################
##############################################################################|

### calculate order 
prevSexSmear %<>% mutate(smear.order = n.smear.positive.tb.male/n.participants.male/
                             n.smear.positive.tb.female/n.participants.female)
smearPrevMF <- rma(ai=n.smear.positive.tb.male, 
                  bi=n.participants.male - n.smear.positive.tb.male, 
                  ci=n.smear.positive.tb.female, 
                  di=n.participants.female - n.smear.positive.tb.female, 
                  data=prevSexSmear, measure="RR", 
                  slab=paste(figure.id.yr), method="REML")

par(cex=1, font=2)
metafor::forest(smearPrevMF, 
                order = prevSexSmear$WHO.region,
       atransf=exp, cex=.85,  xlab="M:F ratio (Smear-Positive TB)", 
       mlab="Overall summary", 
       psize=1, col="black", border="black",
       rows=c(3:20, 23, 26:49, 52:61))
par(cex=0.85, font=2)
text(-8.65, 50.5, "South-East Asian Region")
text(-8.85, 24.5, "Region of the Americas")
text(-9.65, 21.5, "African Region")


##############################################################################|
##### COUNTRIES WITH MORE THAN ONE PREVALENCE SURVEY  #########################
##############################################################################|

countries <- as.vector(unlist(prevSexBact %>% count(study.country) %>% 
                              filter(n > 1) %>%
                              select(study.country)))

for (country in countries){
    tmpBactPrevMF <- rma(ai=n.bacteriological.tb.male, 
                       bi=n.participants.male - n.bacteriological.tb.male, 
                       ci=n.bacteriological.tb.female, 
                       di=n.participants.female - n.bacteriological.tb.female, 
                       data=prevSexBact, measure="RR", 
                       subset = (study.country == country),
                       slab=paste(figure.id.yr), method="REML")
    par(cex=1, font=2)
    print(forest(tmpBactPrevMF,
           atransf=exp, cex=.75,  xlab="M:F ratio (Bacteriologically-Positive TB)", 
           mlab="Overall summary", 
           psize=1, col="black", border="black" ))
}

##############################################################################|
##### UNIVARIATE META REGRESSION OF MF PREVALENCE RATIOS ######################
##############################################################################|
print("Univariate meta regression")
### BY WHO REGION 
# prevSexBact$WHO.region <- as.factor(prevSexBact$WHO.region)
#     
# metab1 <- rma(ai=n.bacteriological.tb.male, 
#               bi=n.participants.male - n.bacteriological.tb.male, 
#               ci=n.bacteriological.tb.female, 
#               di=n.participants.female - n.bacteriological.tb.female, 
#               mods = regionWHO,
#               data=prevSexBact, measure="RR", 
#               slab=paste(figure.id.yr), method="REML")
# metab1$b <- exp(metab1$b)
# metab1$ci.lb <- exp(metab1$ci.lb)
# metab1$ci.ub <- exp(metab1$ci.ub)
# summary(metab1)

### BY NATIONAL VS. SUBNATIONAL SURVEYS 
prevSexBact %<>% mutate(national=ifelse(study.geography == "Nationally representative",
                        1, 0))

metab2 <- rma(ai=n.bacteriological.tb.male, 
              bi=n.participants.male - n.bacteriological.tb.male, 
              ci=n.bacteriological.tb.female, 
              di=n.participants.female - n.bacteriological.tb.female, 
              mods = ~national,
              data=prevSexBact, measure="RR", 
              slab=paste(figure.id.yr), method="REML")

metab2$b <- exp(metab2$b)
metab2$ci.lb <- exp(metab2$ci.lb)
metab2$ci.ub <- exp(metab2$ci.ub)
summary(metab2)

### BY SURVEY START YEAR
metab3 <- rma(ai=n.bacteriological.tb.male, 
              bi=n.participants.male - n.bacteriological.tb.male, 
              ci=n.bacteriological.tb.female, 
              di=n.participants.female - n.bacteriological.tb.female, 
              mods = ~study.start.year,
              data=prevSexBact, measure="RR", 
              slab=paste(figure.id.yr), method="REML")

metab3$b <- exp(metab3$b)
metab3$ci.lb <- exp(metab3$ci.lb)
metab3$ci.ub <- exp(metab3$ci.ub)
summary(metab3)

### BY LOW VS MODERATE/HIGH RISK OF BIAS 

prevSexBact %<>% mutate(low.risk=ifelse(study.quality.summary ==
"Low risk of bias - Further research is very unlikely to change our confidence in the estimate",
                        1, 0))

metab7 <- rma(ai=n.bacteriological.tb.male, 
              bi=n.participants.male - n.bacteriological.tb.male, 
              ci=n.bacteriological.tb.female, 
              di=n.participants.female - n.bacteriological.tb.female, 
              mods = ~low.risk,
              data=prevSexBact, measure="RR", 
              slab=paste(figure.id.yr), method="REML")

metab7$b <- exp(metab7$b)
metab7$ci.lb <- exp(metab7$ci.lb)
metab7$ci.ub <- exp(metab7$ci.ub)
summary(metab7)


### surveys with initial screening procedures requiring self-report of
### signs/symptoms vs.broader initial screening procedures.

prevSexBact %<>% mutate(symp.only = ifelse(screening.tests == "Symptom screen only",
                                        1, 0))

metab8 <- rma(ai=n.bacteriological.tb.male, 
              bi=n.participants.male - n.bacteriological.tb.male, 
              ci=n.bacteriological.tb.female, 
              di=n.participants.female - n.bacteriological.tb.female, 
              mods = ~symp.only,
              data=prevSexBact, measure="RR", 
              slab=paste(figure.id.yr), method="REML")

exp(metab8$b)
exp(metab8$ci.lb)
exp(metab8$ci.ub)
summary(metab8)

### Surveys with diagnosis by smear microscopy vs.other diagnostic measures.

# prevSexBact %<>% mutate(smear.only = ifelse(screening.tests == "Symptom screen only",
#                                            1, 0))
# 
# metab8 <- rma(ai=n.bacteriological.tb.male, 
#               bi=n.participants.male - n.bacteriological.tb.male, 
#               ci=n.bacteriological.tb.female, 
#               di=n.participants.female - n.bacteriological.tb.female, 
#               mods = ~symp.only,
#               data=prevSexBact, measure="RR", 
#               slab=paste(figure.id.yr), method="REML")
# 
# exp(metab8$b)
# exp(metab8$ci.lb)
# exp(metab8$ci.ub)
# summary(metab8)

##############################################################################|
### Close the PDF 
dev.off()
##############################################################################|
##############################################################################|
