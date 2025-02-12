### This script calls the related data cleaning wrapper function
### and then uses that clean data to begin a descriptive analysis
### of the collected data. 

### Load packages 
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(maps)

### Create the clean dataset 
cleanDF0 <- cleanData()[["clean data"]]

### Read in the World Bank Region data 
regionWB <- read.csv("data/world-regions-according-to-the-world-bank.csv")[,c(1,4)] %>% rename(study.country = Entity)

##### GEOGRAPHY #####
##### Rename some countries 

cleanDF <- cleanDF0 
cleanDF[which(cleanDF$study.country == "Viet Nam"), "study.country"] <- "Vietnam"
cleanDF[which(cleanDF$study.country == "The Gambia"), "study.country"] <- "Gambia"
cleanDF[which(cleanDF$study.country == "United Republic of Tanzania"), "study.country"] <- "Tanzania"
cleanDF[which(cleanDF$study.country == "Lao PDR"), "study.country"] <- "Laos"
cleanDF[which(cleanDF$study.country == "Democratic People's Republic of Korea"), "study.country"] <- "North Korea"


#### Open PDF 
pdf("output/descriptiveAnalysis.pdf", width = 11.5, height = 8.5)

###### COUNTRY 
##### Count the county instances
nCountry <- cleanDF %>% count(study.country)
# nCountry <- nCountry[-which(nCountry$study.country == "Viet Nam"), ]
# nCountry[which(nCountry$study.country == "Vietnam"), "n"] <- nCountry[which(nCountry$study.country == "Vietnam"), "n"] + 4

##### Setup a map 
world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")

##### Make a heat map 
ggplot(nCountry) +
    geom_map(
        dat = world_map, map = world_map, aes(map_id = region),
        fill = "lightgrey", color = "black", size = 0.25
    ) +
    geom_map(map = world_map, aes(map_id = study.country, fill = n), size = 0.25) +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Number of prevalence surveys") +
    expand_limits(x = world_map$long, y = world_map$lat) + 
    theme_minimal() + theme(legend.position="bottom")

##### Also make a bar plot 
ggplot(nCountry) + geom_col(aes(x=study.country, y=n, fill = n)) + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Number of prevalence surveys")  + 
    geom_text(aes(x=study.country, y=n, label = n), hjust = -0.2) + 
    ggtitle("Number of prevalence studies by country")

##### WORLD BANK REGION
nRegion <- left_join(x = cleanDF, y = regionWB, by="study.country") %>% count(World.regions.according.to.WB)

ggplot(nRegion) + geom_col(aes(x=World.regions.according.to.WB, y=n, fill = n)) + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Number of prevalence surveys")  + 
    geom_text(aes(x=World.regions.according.to.WB, y=n, label = n), hjust = -0.2) + 
    ggtitle("Number of prevalence studies by World Bank region")

##### STRATIFICATIONS #####
reportStrats0 <- cleanDF %>% select("covidence.id", "study.country", 
                                   colnames(cleanDF)[grep("report", colnames(cleanDF))])

reportStrats <- reshape2::melt(reportStrats0, id.vars = c("covidence.id", "study.country"), 
                               value.name = "report", variable.name = "stratification") 

ggplot(reportStrats %>% filter(report == "Yes"), aes(x = stratification)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("Number of prevalence studies by stratification")


###### SCOPE OF PREVALENCE REVIEW  #####
ggplot(cleanDF, aes(x = study.geography)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - 1, colour = "black") +
    ggtitle("Number of prevalence studies by representativeness")


###### TO DO: WHAT TYPE OF PREVALENCE DATA ###### 
prevData0 <- cleanDF %>% select("covidence.id", colnames(cleanDF)[grep("^prev100k", colnames(cleanDF))])
adjPrevData <- cleanDF %>% select(colnames(cleanDF)[grep("adj.prev100k", colnames(cleanDF))])


###### TEMPORAL VARIABLES ######
ggplot(cleanDF, aes(x = publication.year)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - 1, colour = "black") +
    ggtitle("Number of prevalence studies by publication year")

ggplot(cleanDF, aes(x = study.start.year)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - 1, colour = "black") +
    ggtitle("Number of prevalence studies by study start year") 

ggplot(cleanDF, aes(x = study.end.year)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - 1, colour = "black") +
    ggtitle("Number of prevalence studies by study end year") 

##### SCREENING ALGORITHM #####
ggplot(cleanDF, aes(x = screening.tests)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("Number of prevalence studies by screening algorithm")

symptoms0 <- cleanDF %>% select("covidence.id", "study.country", 
                                    colnames(cleanDF)[grep("symptom", colnames(cleanDF))])

symptoms <- reshape2::melt(symptoms0, id.vars = c("covidence.id", "study.country"), 
                               value.name = "use", variable.name = "symptom") 

ggplot(symptoms %>% filter(use == "Yes"), aes(x = symptom)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("Number of prevalence studies by symptoms screened")

cleanDF %>% 
mutate(positive.xray.definition = ifelse(grepl("Other", positive.xray.definition), "Other", as.character(positive.xray.definition))) %>%
ggplot(aes(x = positive.xray.definition)) + 
geom_bar() + theme_minimal() + 
geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
coord_flip() +
    
ggtitle("Number of prevalence studies by screening algorithm")

##### INCLUDE PPL ON TB TX #####    
ggplot(cleanDF, aes(x = include.current.tb.tx)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("TB prevalence definition includes people on TB treatment")

##### INCLUDE CONTACT TRACING #####    
ggplot(cleanDF, aes(x = contact.investigation)) + geom_bar() + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("TB prevalence definition includes people idenitifed through contact tracing")

##### DIAGNOSTIC ALGORITHM #####  
cleanDF %>% 
dplyr::select("smear.used", "xpert.used", "culture.used") %>% 
mutate("xpert.used" = ifelse(xpert.used != "Neither", "Yes", "Neither")) %>% 
pivot_longer(everything()) %>% 
filter(value == "Yes") %>% 
ggplot(aes(name)) + geom_bar(stat = "count") + theme_minimal() +
    coord_flip() + theme(legend.position="bottom") +
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    ggtitle("TB diagnostic method")

cleanDF %>% 
    dplyr::select("covidence.id", "smear.used", "xpert.used", "culture.used") %>% 
    mutate("xpert.used" = ifelse(xpert.used != "Neither", "Yes", "Neither")) %>% 
    reshape2::melt(id.vars = "covidence.id", variable.name = "test.used") %>%  
    filter(value == "Yes") %>%
    ggplot() + geom_point(aes(x = as.factor(covidence.id), y = test.used, color = test.used), shape = 15, size = 2) + 
    theme_void() +
    theme(legend.position="bottom", axis.text.x=element_blank(), 
          axis.ticks.x=element_blank()) + coord_fixed(ratio = .8) + 
    ggtitle("Combination of TB diagnostic methods")

cleanDF %>% 
    dplyr::select("covidence.id", "smear.used", "xpert.used", "culture.used") %>% 
    mutate("xpert.used" = ifelse(xpert.used != "Neither", "Yes", "Neither"), 
           "All methods used" = ifelse(xpert.used == "Yes" & smear.used == "Yes" & culture.used == "Yes", "Yes", "No"), 
           "Smear and Xpert Used" = ifelse(xpert.used == "Yes" & smear.used == "Yes" & culture.used == "No", "Yes", "No"), 
           "Culture and Xpert Used" = ifelse(xpert.used == "Yes" & smear.used == "No" & culture.used == "Yes", "Yes", "No"), 
           "Smear and Culture Used" = ifelse(xpert.used == "No" & smear.used == "Yes" & culture.used == "Yes", "Yes", "No"), 
           "Only smear used"  = ifelse(xpert.used == "No" & smear.used == "Yes" & culture.used == "No", "Yes", "No"), 
           "Only Xpert used"  = ifelse(xpert.used == "Yes" & smear.used == "No" & culture.used == "No", "Yes", "No"), 
           "Only culture used"  = ifelse(xpert.used == "No" & smear.used == "No" & culture.used == "Yes", "Yes", "No")) %>% 
    select (! ends_with(".used")) %>%
    select (! covidence.id) %>%
    pivot_longer(everything()) %>% 
    filter(value == "Yes") %>% 
    ggplot(aes(name)) + geom_bar() + 
    geom_text(aes(label = after_stat(count)), stat = "count", hjust = - .5, colour = "black") +
    theme_minimal() + coord_flip()
    theme(legend.position="bottom") + 
    ggtitle("Combination of TB diagnostic methods")
    
        

##### BY TIME AND REGION #####
timeRegion <- left_join(x = cleanDF, y = regionWB, by="study.country") %>% 
              select(study.start.year, World.regions.according.to.WB, study.country) %>% 
              group_by(study.start.year) %>% 
              count(World.regions.according.to.WB)

ggplot(timeRegion, aes(x=study.start.year)) + 
    geom_point(aes(y=World.regions.according.to.WB, color=World.regions.according.to.WB, size = n)) + 
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
    ggtitle("Study start years by World Bank region") + 
    guides(color = guide_legend(nrow = 2))

timeRegion <- left_join(x = cleanDF, y = regionWB, by="study.country") %>% 
    select(study.end.year, World.regions.according.to.WB, study.country) %>% 
    group_by(study.end.year) %>% 
    count(World.regions.according.to.WB)

ggplot(timeRegion, aes(x=study.end.year)) + 
    geom_point(aes(y=World.regions.according.to.WB, color=World.regions.according.to.WB, size = n)) + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
    ggtitle("Study end years by World Bank region") + 
    guides(color = guide_legend(nrow = 2))

timeRegion <- left_join(x = cleanDF, y = regionWB, by="study.country") %>% 
    select(publication.year, World.regions.according.to.WB, study.country) %>% 
    group_by(publication.year) %>% 
    count(World.regions.according.to.WB) 

ggplot(timeRegion, aes(x=publication.year)) + 
    geom_point(aes(y=World.regions.according.to.WB, color=World.regions.according.to.WB, size = n)) + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") + 
    ggtitle("Study publication years by World Bank region") + 
    guides(color = guide_legend(nrow = 2))


dev.off()

