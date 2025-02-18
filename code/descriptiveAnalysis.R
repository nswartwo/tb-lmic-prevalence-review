### This script calls the related data cleaning wrapper function
### and then uses that clean data to begin a descriptive analysis
### of the collected data. 

### Load packages 
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gt)
library(maps)
library(ggpubr)

### Setup palette 
myPal <- c( "#44AA99", "#882255", "#332288", "#117733", "#6699CC", "#CC6677",
            "#AA4499", "#999933", "#A41034","#88CCEE", "#DDCC77", "#888888")

##############################################################################|
##### Create the clean dataset ################################################
##############################################################################|
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
##### DESCRIPTIVE PLOTS #######################################################
##############################################################################|
### Open PDF 
pdf("output/descriptiveAnalysis.pdf", width = 11.5, height = 8.5)

##### PLOTS BY GEOGRAPHY ######################################################

### COUNTRY 
### Count the county instances
nCountry <- cleanDF %>% 
            count(study.country) %>% 
            left_join(regionWHO) 

### Setup a world map 
world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

### Make a heat map of prevalence studies by country 
ggplot(nCountry) +
    geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "lightgrey", color = "black", size = 0.25) +
    geom_map(map = world_map, aes(map_id = study.country, fill = n)) +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02",
                        name = "Number of prevalence surveys") +
    expand_limits(x = world_map$long, y = world_map$lat) + 
    theme_void() + theme(legend.position = "inside",
                            legend.position.inside = c(.2,.2)) + 
    ggtitle ("Number of prevalence surveys by country")

### Bar plot of countries by total survey count
ggplot(nCountry) + geom_col(aes(x=study.country, y=n), color = "black") + 
                   theme_minimal() + coord_flip() + 
                   theme(legend.position = "inside",
                         legend.position.inside = c(.85,.15), 
                         legend.background = element_rect(color = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=study.country, y=n, label = n), hjust = 1.5, color = "white") + 
    labs(y = "Survey country", "Number of prevalence surveys") + 
    ggtitle("Number of prevalence surveys by country")

### Also make a bar plot colored by WHO Region
ggplot(nCountry) + 
    geom_col(aes(x=study.country, y=n, fill = World.regions.according.to.WHO), color = "black") + 
    theme_minimal() +
    coord_flip() + theme(legend.position = "inside",
                         legend.position.inside = c(.85,.15), 
                         legend.background = element_rect(color = "white"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=study.country, y=n, label = n), hjust = 1.5, color = "white") + 
    scale_fill_manual(values=myPal, name = "WHO Region") + 
    labs(x = "Survey country", y="Number of prevalence surveys") + 
    ggtitle("Number of prevalence surveys by country")

##### WHO REGION
nRegion <- cleanDF %>% 
           count(World.regions.according.to.WHO)

ggplot(nRegion) + 
    geom_col(aes(x=World.regions.according.to.WHO, y=n), color = "black") + 
    theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    geom_text(aes(x=World.regions.according.to.WHO, y=n, label = n), hjust = 2, color ="white") + 
    labs(x = "Survey region", y="Number of prevalence surveys") + 
    ggtitle("Number of prevalence surveys by World Bank region")

##### PLOTS BY PREVALENCE STRATIFICATIONS #####################################
reportStrats0 <- cleanDF %>% select("covidence.id", "study.country", 
                                   colnames(cleanDF)[grep("report", colnames(cleanDF))])

reportStrats <- reshape2::melt(reportStrats0, id.vars = c("covidence.id", "study.country"), 
                               value.name = "report", variable.name = "stratification") 

### Surveys by which prevalence stratifications were reported
ggplot(reportStrats %>% filter(report == "Yes"), aes(x = stratification)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Prevalence stratification") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, colour = "white") +
    ggtitle("Number of prevalence surveys by stratification")

### Scope of prevalence review  
ggplot(cleanDF, aes(x = study.geography)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Scope of prevalence review") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.2, colour = "white") +
    ggtitle("Number of prevalence surveys by representativeness")


###### TO DO: WHAT TYPE OF PREVALENCE DATA ###### 
prevData0 <- cleanDF %>% 
             select("covidence.id", colnames(cleanDF)[grep("^prev100k", colnames(cleanDF))])
adjPrevData <- cleanDF %>% 
             select(colnames(cleanDF)[grep("adj.prev100k", colnames(cleanDF))])


##### PLOTS BY TIME ###########################################################
### Surveys by publication year
ggplot(cleanDF, aes(x = publication.year)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Publication year") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("Number of prevalence surveys by publication year")

### Surveys by start year
ggplot(cleanDF, aes(x = study.start.year)) + 
    geom_bar(color = "black") + 
    # geom_bar(aes(fill = World.regions.according.to.WHO), color = "black") + 
    theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Survey start year") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("Number of prevalence surveys by start year")



##### PLOTS BY SCREENING ALGORITHM ############################################
### Total studies by screening algorithm 
cleanDF %>% 
mutate(simple.screen.tests = ifelse(grepl("Other:", screening.tests)==TRUE, "Other", as.character(screening.tests))) %>% 
ggplot(aes(x = simple.screen.tests)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Screening algorithm") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("Number of prevalence surveys by screening algorithm")

### Surveys by symptom screening
symptoms0 <- cleanDF %>% select("covidence.id", "study.country", 
                         colnames(cleanDF)[grep("symptom.screening.", colnames(cleanDF))])
colnames(symptoms0) <- gsub("symptom.screening.", "", colnames(symptoms0))
symptoms <- reshape2::melt(symptoms0, id.vars = c("covidence.id", "study.country"), 
                               value.name = "use", variable.name = "symptom") 

ggplot(symptoms %>% filter(use == "Yes"), aes(x = symptom)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "Symptom included") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.5, colour = "white") +
    ggtitle("Number of prevalence surveys by symptoms screened")

### Survey definition of abnormal xray definition
cleanDF %>% 
mutate(positive.xray.definition = ifelse(grepl("Other", positive.xray.definition), "Other", as.character(positive.xray.definition))) %>%
ggplot(aes(x = positive.xray.definition)) + 
geom_bar(color = "black") + theme_minimal() + 
geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.5, colour = "white") +
coord_flip() + theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank()) +
labs(y = "Number of prevalence surveys", x = "Abnormal chest X-ray definition") +
ggtitle("Number of prevalence surveys by definition of abnormal chest X-ray")

##### PLOTS BY PREVALENCE DEFINITION ##########################################
### Definition includes people currently on tb treament 
ggplot(cleanDF, aes(x = include.current.tb.tx)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.5, colour = "white") +
    ggtitle("TB prevalence definition includes people on TB treatment")

##### INCLUDE CONTACT TRACING #####    
cleanDF %>% mutate(contact.investigation = ifelse(grepl("Other:", contact.investigation)==TRUE, "Unknown", as.character(contact.investigation))) %>% 
ggplot(aes(x = contact.investigation)) + 
    geom_bar(color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.1, colour = "white") +
    ggtitle("TB prevalence definition includes people idenitifed through contact tracing")

##### DIAGNOSTIC ALGORITHM ####################################################
cleanDF %>% 
dplyr::select("smear.used", "xpert.used", "culture.used") %>% 
mutate("xpert.used" = ifelse(xpert.used != "Neither", "Yes", "Neither")) %>% 
pivot_longer(everything()) %>% 
filter(value == "Yes") %>% 
ggplot(aes(name)) + geom_bar(stat = "count", color = "black") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom",
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank()) +
    labs(y = "Number of prevalence surveys", x = "") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.5, colour = "white") +
    ggtitle("TB diagnostic method")

### Combination of diagnostic methods
cleanDF %>% 
    dplyr::select("covidence.id", "smear.used", "xpert.used", "culture.used") %>% 
    mutate("xpert.used" = ifelse(xpert.used != "Neither", "Yes", "Neither"), 
           "All methods used" = ifelse(xpert.used == "Yes" & smear.used == "Yes" & culture.used == "Yes", "Yes", "No"), 
           "Smear and Xpert Used" = ifelse(xpert.used == "Yes" & smear.used == "Yes" & culture.used == "No", "Yes", "No"), 
           "Culture and Xpert Used" = ifelse(xpert.used == "Yes" & smear.used == "No" & culture.used == "Yes", "Yes", "No"), 
           "Smear and Culture Used" = ifelse(xpert.used == "No" & smear.used == "Yes" & culture.used == "Yes", "Yes", "No"), 
           "Only smear used"  = ifelse(xpert.used != "Yes" & smear.used == "Yes" & culture.used != "Yes", "Yes", "No"), 
           "Only Xpert used"  = ifelse(xpert.used == "Yes" & smear.used != "Yes" & culture.used != "Yes", "Yes", "No"), 
           "Only culture used"  = ifelse(xpert.used != "Yes" & smear.used != "Yes" & culture.used == "Yes", "Yes", "No")) %>% 
    select (! ends_with(".used")) %>%
    select (! covidence.id) %>%
    pivot_longer(everything()) %>% 
    filter(value == "Yes") %>% 
    ggplot(aes(name)) + geom_bar(color = "black") + 
    theme_minimal() + coord_flip() +
    theme(legend.position="bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
         labs(y = "Number of prevalence surveys", x = "") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = 1.5, colour = "white") +
    ggtitle("Combination of TB diagnostic methods")
    
##### BY TIME AND REGION ####################################################
timeRegion <- cleanDF %>% 
              select(study.start.year, World.regions.according.to.WHO, study.country) %>% 
              group_by(study.start.year) %>% 
              count(World.regions.according.to.WHO)

ggplot(timeRegion, aes(x=study.start.year)) + 
    geom_point(aes(y=World.regions.according.to.WHO, color=World.regions.according.to.WHO, size = n)) + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_blank()) +
    ggtitle("Survey start years by World Bank region") + 
    labs(y="", x="") + 
    guides(color = guide_legend(nrow = 2))

timeRegion <- cleanDF %>% 
    group_by(publication.year) %>% 
    count(World.regions.according.to.WHO) 

ggplot(timeRegion, aes(x=publication.year)) + 
    geom_point(aes(y=World.regions.according.to.WHO, color=World.regions.according.to.WHO, size = n)) + 
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_blank()) +
    labs(y="", x="") + 
    ggtitle("Study publication years by World Bank region") + 
    guides(color = guide_legend(nrow = 2))

##### PARTICIPANT RATIOS ####################################################
##### Male to female ratio of participants ##### 
cleanDF %>% 
    mutate(ratio.participants.mf = n.participants.male / n.participants.female) %>% 
    filter(is.na(ratio.participants.mf) == FALSE) %>%
    arrange(ratio.participants.mf) %>%
    mutate(figure.id.yr=factor(figure.id.yr, levels=figure.id.yr)) %>%
    ggplot(aes(y = figure.id.yr, x = ratio.participants.mf, color = ratio.participants.mf < 1)) +
    geom_point() + 
    theme_minimal(base_size = 8) + 
    scale_color_manual(labels = c("More male participants", "More female participants"), 
                       values = c("#44AA99","#882255"), name = "") +
    ggtitle("Male to female ratio of participants") + 
    labs(x = "Male to female ratio", y = "") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank()) + 
    geom_text(aes(label = round(ratio.participants.mf,2)), hjust = - .5, colour = "black", size = 3)


##### Urban to rural ratio of participants ##### 
cleanDF %>% 
    mutate(ratio.participants.ur = n.participants.urban / n.participants.rural) %>% 
    filter(is.na(ratio.participants.ur) == FALSE) %>%
    arrange(ratio.participants.ur) %>%
    mutate(figure.id.yr=factor(figure.id.yr, levels=figure.id.yr)) %>%
    ggplot(aes(y = figure.id.yr, x = ratio.participants.ur, color = ratio.participants.ur < 1)) +
    geom_point(size = 2) + 
    theme_minimal() + 
    scale_color_manual(labels = c("More urban participants", "More rural participants"), 
                       values = c("#882255", "#44AA99"), name = "") +
    ggtitle("Crude urban to rural ratio of participants") + 
    labs(x = "Urban to rural ratio", y = "") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank()) + 
    geom_text(aes(label = round(ratio.participants.ur,2)), hjust = - .2, colour = "black")

dev.off()

##############################################################################|
##### RISK OF BIAS SUMMARY ####################################################
##############################################################################|
### Standardize the study quality/risk of bias data 
riskData <- cleanDF %>% select(contains("study.quality")) %>% 
    select(! "study.quality.comments") %>% 
    mutate("study.quality.representative" = ifelse(grepl("Unknown", study.quality.representative), 
                                                   "Unknown",  as.character(study.quality.representative))) %>% 
    mutate(bias.risk = case_when(grepl("High", study.quality.summary) ~ "High risk of bias", 
                                 grepl("Low", study.quality.summary) ~ "Low risk of bias",
                                 grepl("Moderate", study.quality.summary) | grepl("Medium", study.quality.summary) ~ "Moderate risk of bias",
                                 grepl("N/A", study.quality.summary) ~ "Unknown"))

levels(riskData$study.quality.random.selection)[2] <- "Unknown"
levels(riskData$study.quality.nonresponse)[3] <- "Unknown"
levels(riskData$study.quality.direct.data.collect)[2] <- "Unknown"
levels(riskData$study.quality.case.definition)[2] <- "Unknown"
levels(riskData$study.quality.valid.instrument)[3] <- "Unknown"
levels(riskData$study.quality.same.data.collect)[2] <- "Unknown"
levels(riskData$study.quality.numerator.denominator)[3] <- "Unknown"

### Open PDF 

pdf("output/riskOfBias.pdf", width = 8.5, height = 11.5)

risk1 <- riskData %>% 
        mutate(study.quality.representative = fct_relevel(study.quality.representative, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
        ggplot() + 
        geom_bar(aes(study.quality.representative, fill = bias.risk), position = "stack") + 
        theme_minimal() + theme(legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank()) + coord_flip() +
        labs(y="", x="") + 
        scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
        # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
        ggtitle("Was the study’s sample population a true or close representation of the target population?")

risk2 <- riskData %>% 
    mutate(study.quality.random.selection = fct_relevel(study.quality.random.selection, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
    ggplot() + 
    geom_bar(aes(study.quality.random.selection, fill = bias.risk), position = "stack") + 
    theme_minimal() + theme(legend.position = "bottom",
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank()) + coord_flip() +
    labs(y="", x="") + 
    scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
    # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
    ggtitle("Was some form of random selection used to select the sample or was a census undertaken?")

risk3 <- riskData %>% 
    mutate(study.quality.nonresponse = fct_relevel(study.quality.nonresponse, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
    ggplot() + 
    geom_bar(aes(study.quality.nonresponse, fill = bias.risk), position = "stack") + 
    theme_minimal() + theme(legend.position = "bottom",
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank()) + coord_flip() +
    labs(y="", x="") + 
    scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
    # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
    ggtitle("Was the likelihood of non-response bias minimal?")

risk4 <- riskData %>% 
    mutate(study.quality.direct.data.collect = fct_relevel(study.quality.direct.data.collect, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
    ggplot() + 
    geom_bar(aes(study.quality.direct.data.collect, fill = bias.risk), position = "stack") + 
    theme_minimal() + theme(legend.position = "bottom",
                            panel.grid.minor = element_blank(),
                            panel.grid.major = element_blank()) + coord_flip() +
    labs(y="", x="") + 
    scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
    # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
    ggtitle("Were data collected directly from subjects (as opposed to a proxy)?")

risk5 <- riskData %>% 
        mutate(study.quality.case.definition = fct_relevel(study.quality.case.definition, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
        ggplot() + 
        geom_bar(aes(study.quality.case.definition, fill = bias.risk), position = "stack") + 
        theme_minimal() + theme(legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank()) + coord_flip() +
        labs(y="", x="") + 
        scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
        # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
        ggtitle("Was an acceptable case definition used in the study?")

risk6 <- riskData %>% 
        mutate(study.quality.valid.instrument = fct_relevel(study.quality.valid.instrument, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
        ggplot() + 
        geom_bar(aes(study.quality.valid.instrument, fill = bias.risk), position = "stack") + 
        theme_minimal() + theme(legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank()) + coord_flip() +
        labs(y="", x="") + 
        scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
        # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
        ggtitle("Was the study instrument that measured the parameter of interest shown to have reliability and validity?")

risk7 <- riskData %>% 
        mutate(study.quality.same.data.collect = fct_relevel(study.quality.same.data.collect, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
        ggplot() + 
        geom_bar(aes(study.quality.same.data.collect, fill = bias.risk), position = "stack") + 
        theme_minimal() + theme(legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank()) + coord_flip() +
        labs(y="", x="") + 
        scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
        # geom_text(aes(y = 1, x=study.quality.representative, label = bias.risk), size = 3, position = position_stack(vjust = 0.5)) +
        ggtitle("Was the same mode of data collection used for all subjects?")

risk8 <- riskData %>% 
        mutate(study.quality.same.data.collect = fct_relevel(study.quality.same.data.collect, c("Unknown", "No (high risk)", "Yes (low risk)"))) %>%
        ggplot(aes(x = study.quality.same.data.collect)) + 
        geom_bar(aes(fill = bias.risk), position = "stack") + 
        theme_minimal() + theme(legend.position = "bottom",
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank()) + coord_flip() +
        labs(y="", x="") + 
        scale_fill_manual(values = myPal[c(9,4,11,12)], name = "Overall bias assessment") +
        # geom_text(aes(label = after_stat(count)), position = "stack", stat = "count", hjust = 1.2, colour = "white") +
        ggtitle("Were the numerator and denominator for the parameter of interest appropriate?")

ggarrange(risk1, risk2, risk3, risk4,
          risk5, risk6, risk7, risk8,
          ncol=1, nrow=8, common.legend = TRUE, legend="bottom")

dev.off()

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

gtsave(data = sexTbl, filename = "output/sexTable.pdf") 

    
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

gtsave(data = ruralityTbl, filename = "output/ruralityTable.pdf") 


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

gtsave(data = hivTbl, filename = "output/hivTable.pdf") 

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

gtsave(data = ageTbl, filename = "output/ageTable.pdf") 

##### Study specific CSV export #####
### We will use these to create formatted tables in word.

### Sex studies 
sexSurveys <- cleanDF %>% filter(report.sex == "Yes")
studySexDetails <- data.frame("Survey ID" = sexSurveys$figure.id, 
                              "WHO region" = sexSurveys$World.regions.according.to.WHO,
                              "Year(s) of study" = sexSurveys$study.years,
                              "Female participants (N)" = sexSurveys$n.participants.female,
                              "Female TB positive (N)" = sexSurveys$n.bacteriological.tb.female, 
                              "Male participants (N)" = sexSurveys$n.participants.male,
                              "Male TB positive (N)" = sexSurveys$n.bacteriological.tb.male, 
                              "Total participants (N)" = sexSurveys$n.participants.female + sexSurveys$n.participants.male,
                              "Total TB positive (N)" = sexSurveys$n.bacteriological.tb.female + sexSurveys$n.bacteriological.tb.male, 
                              check.names = FALSE)

write.csv(studySexDetails, file = here("output/sexStudyTable.csv"))              

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

write.csv(studyRuralityDetails, file = here("output/ruralityStudyTable.csv"))  
                           
### HIV studies 
hivSurveys <- cleanDF %>% filter(report.hiv == "Yes")
studyHIVDetails <- data.frame("Survey ID" = hivSurveys$figure.id, 
                              "WHO region" = hivSurveys$World.regions.according.to.WHO,
                              "Year(s) of study" = hivSurveys$study.years,
                              "HIV+ participants (N)" = hivSurveys$n.participants.hiv.positive,
                              "HIV+ TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.positive, 
                              "HIV- participants (N)" = hivSurveys$n.participants.hiv.negative,
                              "HIV- TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.negative, 
                              "Total participants (N)" = hivSurveys$n.participants.hiv.positive + hivSurveys$n.participants.hiv.negative,
                              "Total TB positive (N)" = hivSurveys$n.bacteriological.tb.hiv.positive + hivSurveys$n.bacteriological.tb.hiv.negative, 
                              check.names = FALSE)

write.csv(studyHIVDetails, file = here("output/hivStudyTable.csv"))  




