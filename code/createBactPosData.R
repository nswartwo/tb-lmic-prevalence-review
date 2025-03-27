### Creates and saves the bacteriologically positive datasets

source(here("code/bacterialPositiveIndicator.R"))

cleanDF0 <- bactPostIndicator("rurality", FALSE) %>% filter(rurality.analysis.indicator !="none")
dim(cleanDF0)
saveRDS(cleanDF0, file = here("data/bactPosRuralityData.rds"), version = 2)

cleanDF1 <- bactPostIndicator("age.grp", FALSE) %>% filter(age.grp.analysis.indicator !="none")
dim(cleanDF1)
saveRDS(cleanDF1, file = here("data/bactPosAgeGrpData.rds"), version = 2)


cleanDF2 <- bactPostIndicator("hiv", FALSE) %>% filter(hiv.analysis.indicator !="none")
dim(cleanDF2)
saveRDS(cleanDF2, file = here("data/bactPosHivData.rds"), version = 2)

cleanDF3 <- bactPostIndicator("sex", FALSE) %>% filter(sex.analysis.indicator !="none")
dim(cleanDF3)
saveRDS(cleanDF3, file = here("data/bactPosSexData.rds"), version = 2)
