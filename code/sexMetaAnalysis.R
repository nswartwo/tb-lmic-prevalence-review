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
    mutate(figure.id.yr = paste(figure.id, study.start.year))

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
##### CREATE A TIDY DATASET OF RELEVANT VARIABLES #############################

#### Not sure if this step is necessary for BRMS package? 

##############################################################################|
##### SETUP INITIAL BAYESIAN MODEL (JUST TINKERING FOR NOW) ###################
##### Use count data first with country fixed effect 
##############################################################################|
model1 <- brms::brm(n.bacteriological.tb.sex.total ~ study.country,
                    data = cleanSexDF0, 
                    family = poisson(), chains = 3,
                    iter = 3000, warmup = 1000)

summary(model1)
plot(model1)

##### Next step run the model with rates to understand how to incorporate beta 
##### offset terms. 

