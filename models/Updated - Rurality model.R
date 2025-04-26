##### LOAD IN NECESSARY PACKAGES ##############################################
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(here)
library(magrittr)
library(DT)
library(scales)
library(metafor)
library(meta)
library(brms)
library(posterior)
library(tidybayes)

library(ggplot2)

expit <- function (x) {exp(x)/(1 + exp(x))}
##############################################################################|
##### LOAD CLEAN DATA SET AND FILTER TO SURVEYS REPORTING rurality #################
bactPosRuralityData <- readRDS(here("data/bactPosRuralityData.rds"))
SDS <- bactPosRuralityData %>%
    mutate("id" = row_number(),
           adj.prev100k.bacteriological.tb.urban = adj.prev100k.bacteriological.tb.urban/1e5,
           adj.prev100k.ci.upper.bacteriological.tb.urban = adj.prev100k.ci.upper.bacteriological.tb.urban/1e5,
           adj.prev100k.ci.lower.bacteriological.tb.urban = adj.prev100k.ci.lower.bacteriological.tb.urban/1e5,
           
           adj.prev100k.bacteriological.tb.rural = adj.prev100k.bacteriological.tb.rural/1e5,
           adj.prev100k.ci.upper.bacteriological.tb.rural = adj.prev100k.ci.upper.bacteriological.tb.rural/1e5,
           adj.prev100k.ci.lower.bacteriological.tb.rural = adj.prev100k.ci.lower.bacteriological.tb.rural/1e5,
           
           adj.prev100k.smear.positive.tb.urban = adj.prev100k.smear.positive.tb.urban/1e5,
           adj.prev100k.ci.upper.smear.positive.tb.urban = adj.prev100k.ci.upper.smear.positive.tb.urban/1e5,
           adj.prev100k.ci.lower.smear.positive.tb.urban = adj.prev100k.ci.lower.smear.positive.tb.urban/1e5,
           
           adj.prev100k.smear.positive.tb.rural = adj.prev100k.smear.positive.tb.rural/1e5,
           adj.prev100k.ci.upper.smear.positive.tb.rural = adj.prev100k.ci.upper.smear.positive.tb.rural/1e5,
           adj.prev100k.ci.lower.smear.positive.tb.rural = adj.prev100k.ci.lower.smear.positive.tb.rural/1e5,
           
           prev100k.bacteriological.tb.urban = prev100k.bacteriological.tb.urban/1e5,
           prev100k.ci.upper.bacteriological.tb.urban = prev100k.ci.upper.bacteriological.tb.urban/1e5,
           prev100k.ci.lower.bacteriological.tb.urban = prev100k.ci.lower.bacteriological.tb.urban/1e5,
           
           prev100k.bacteriological.tb.rural = prev100k.bacteriological.tb.rural/1e5,
           prev100k.ci.upper.bacteriological.tb.rural = prev100k.ci.upper.bacteriological.tb.rural/1e5,
           prev100k.ci.lower.bacteriological.tb.rural = prev100k.ci.lower.bacteriological.tb.rural/1e5,
           
           prev100k.smear.positive.tb.urban = prev100k.smear.positive.tb.urban/1e5,
           prev100k.ci.upper.smear.positive.tb.urban = prev100k.ci.upper.smear.positive.tb.urban/1e5,
           prev100k.ci.lower.smear.positive.tb.urban = prev100k.ci.lower.smear.positive.tb.urban/1e5,
           
           prev100k.smear.positive.tb.rural = prev100k.smear.positive.tb.rural/1e5,
           prev100k.ci.upper.smear.positive.tb.rural = prev100k.ci.upper.smear.positive.tb.rural/1e5,
           prev100k.ci.lower.smear.positive.tb.rural = prev100k.ci.lower.smear.positive.tb.rural/1e5) %>%
    select(id,
           covidence.id, 
           figure.id.yr, 
           study.geography, 
           title.extracted,
           study.country,
           WHO.region,
           study.start.year, 
           study.end.year, 
           ends_with("urban"), ends_with("rural"), ends_with("rurality"),
           rurality.analysis.indicator) %>%
    
    mutate("tempPrevurban" =  case_when(rurality.analysis.indicator == 
                                           "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.urban, 
                                       rurality.analysis.indicator == 
                                           "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.urban,
                                       rurality.analysis.indicator == 
                                           "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.urban, 
                                       rurality.analysis.indicator == 
                                           "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.urban, 
                                       rurality.analysis.indicator == 
                                           "n.bacteriological.tb" ~ n.bacteriological.tb.urban/n.participants.urban, 
                                       rurality.analysis.indicator == 
                                           "n.smear.positive.tb" ~ n.smear.positive.tb.urban/n.participants.urban,
                                       rurality.analysis.indicator == 
                                           "n.culture.positive.tb" ~ n.culture.positive.tb.urban/n.participants.urban,
                                       rurality.analysis.indicator == 
                                           "prev100k.smear.positive.tb" ~ prev100k.smear.positive.tb.urban),
           "tempPrevrural" = case_when(rurality.analysis.indicator == 
                                            "adj.prev100k.ci.bacteriological.tb" ~ adj.prev100k.bacteriological.tb.rural, 
                                        rurality.analysis.indicator == 
                                            "adj.prev100k.ci.smear.positive.tb" ~ adj.prev100k.smear.positive.tb.rural,
                                        rurality.analysis.indicator == 
                                            "prev100k.ci.bacteriological.tb" ~ prev100k.bacteriological.tb.rural, 
                                        rurality.analysis.indicator == 
                                            "prev100k.ci.smear.positive.tb" ~ prev100k.smear.positive.tb.rural, 
                                        rurality.analysis.indicator == 
                                            "n.bacteriological.tb" ~ n.bacteriological.tb.rural/n.participants.rural, 
                                        rurality.analysis.indicator == 
                                            "n.smear.positive.tb" ~ n.smear.positive.tb.rural/n.participants.rural,
                                        rurality.analysis.indicator == 
                                            "n.culture.positive.tb" ~ n.culture.positive.tb.rural/n.participants.rural,
                                        rurality.analysis.indicator == 
                                            "prev100k.smear.positive.tb" ~ prev100k.smear.positive.tb.rural)) %>%
    pivot_longer(cols = c(tempPrevurban,
                          tempPrevrural), 
                 names_to = "rurality", 
                 values_to = "Adjusted Prevalence", 
                 names_prefix = "tempPrev") %>% 
    mutate("Standard Error" = case_when(rurality.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" & 
                                            rurality == "urban" ~ (adj.prev100k.ci.upper.bacteriological.tb.urban-
                                                                 adj.prev100k.ci.lower.bacteriological.tb.urban)/3.92,
                                        rurality.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" & 
                                            rurality == "rural" ~ (adj.prev100k.ci.upper.bacteriological.tb.rural-
                                                                   adj.prev100k.ci.lower.bacteriological.tb.rural)/3.92,
                                        rurality.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" & 
                                            rurality == "urban" ~ (adj.prev100k.ci.upper.smear.positive.tb.urban-
                                                                 adj.prev100k.ci.lower.smear.positive.tb.urban)/3.92,
                                        rurality.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" & 
                                            rurality == "rural" ~ (adj.prev100k.ci.upper.smear.positive.tb.rural-
                                                                   adj.prev100k.ci.lower.smear.positive.tb.rural)/3.92,
                                        rurality.analysis.indicator == "prev100k.ci.bacteriological.tb" & 
                                            rurality == "urban" ~ (prev100k.ci.upper.bacteriological.tb.urban-
                                                                 prev100k.ci.lower.bacteriological.tb.urban)/3.92,
                                        rurality.analysis.indicator == "prev100k.ci.bacteriological.tb" & 
                                            rurality == "rural" ~ (prev100k.ci.upper.bacteriological.tb.rural-
                                                                   prev100k.ci.lower.bacteriological.tb.rural)/3.92,
                                        rurality.analysis.indicator == "prev100k.ci.smear.positive.tb" & 
                                            rurality == "urban" ~ (prev100k.ci.upper.smear.positive.tb.urban-
                                                                 prev100k.ci.lower.smear.positive.tb.urban)/3.92,
                                        rurality.analysis.indicator == "prev100k.ci.smear.positive.tb" & 
                                            rurality == "rural" ~ (prev100k.ci.upper.smear.positive.tb.rural-
                                                                   prev100k.ci.lower.smear.positive.tb.rural)/3.92,
                                        rurality.analysis.indicator %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                      "n.culture.positive.tb", "prev100k.smear.positive.tb") & rurality == "urban" ~ 
                                            sqrt((`Adjusted Prevalence`*(1-`Adjusted Prevalence`))/n.participants.urban),
                                        rurality.analysis.indicator  %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                       "n.culture.positive.tb", "prev100k.smear.positive.tb") & rurality == "rural" ~ 
                                            sqrt((`Adjusted Prevalence`*(1-`Adjusted Prevalence`))/n.participants.rural)),
           
           "Phi" = (`Adjusted Prevalence`/`Standard Error`^2) - (`Adjusted Prevalence`/`Standard Error`)^2 - 1, 
           "LogOdds" = log(`Adjusted Prevalence`/(1-`Adjusted Prevalence`)), 
           "LogOddsStandardError" = case_when(rurality.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
                                                  rurality == "urban" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.urban/
                                                                           (1-adj.prev100k.ci.upper.bacteriological.tb.urban)) -
                                                                       log(adj.prev100k.ci.lower.bacteriological.tb.urban/
                                                                               (1-adj.prev100k.ci.lower.bacteriological.tb.urban))) /3.92,
                                              rurality.analysis.indicator == "adj.prev100k.ci.bacteriological.tb" &
                                                  rurality == "rural" ~ (log(adj.prev100k.ci.upper.bacteriological.tb.rural/
                                                                             (1-adj.prev100k.ci.upper.bacteriological.tb.rural)) -
                                                                         log(adj.prev100k.ci.lower.bacteriological.tb.rural /
                                                                                 (1-adj.prev100k.ci.lower.bacteriological.tb.rural))) /3.92,
                                              rurality.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" &
                                                  rurality == "urban" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.urban/
                                                                           (1-adj.prev100k.ci.upper.smear.positive.tb.urban)) -
                                                                       log(adj.prev100k.ci.lower.smear.positive.tb.urban /
                                                                               (1-adj.prev100k.ci.lower.smear.positive.tb.urban)))/3.92,
                                              rurality.analysis.indicator == "adj.prev100k.ci.smear.positive.tb" &
                                                  rurality == "rural" ~ (log(adj.prev100k.ci.upper.smear.positive.tb.rural/
                                                                             (1-adj.prev100k.ci.upper.smear.positive.tb.rural)) -
                                                                         log(adj.prev100k.ci.lower.smear.positive.tb.rural /
                                                                                 (1-adj.prev100k.ci.lower.smear.positive.tb.rural)))/3.92,
                                              rurality.analysis.indicator == "prev100k.ci.bacteriological.tb" &
                                                  rurality == "urban" ~ (log(prev100k.ci.upper.bacteriological.tb.urban/
                                                                           (1-prev100k.ci.upper.bacteriological.tb.urban)) -
                                                                       log(prev100k.ci.lower.bacteriological.tb.urban/
                                                                               (1-prev100k.ci.lower.bacteriological.tb.urban))) /3.92,
                                              rurality.analysis.indicator == "prev100k.ci.bacteriological.tb" &
                                                  rurality == "rural" ~ (log(prev100k.ci.upper.bacteriological.tb.rural/
                                                                             (1-prev100k.ci.upper.bacteriological.tb.rural)) -
                                                                         log(prev100k.ci.lower.bacteriological.tb.rural /
                                                                                 (1-prev100k.ci.lower.bacteriological.tb.rural))) /3.92,
                                              rurality.analysis.indicator == "prev100k.ci.smear.positive.tb" &
                                                  rurality == "urban" ~ (log(prev100k.ci.upper.smear.positive.tb.urban/
                                                                           (1-prev100k.ci.upper.smear.positive.tb.urban)) -
                                                                       log(prev100k.ci.lower.smear.positive.tb.urban /
                                                                               (1-prev100k.ci.lower.smear.positive.tb.urban)))/3.92,
                                              rurality.analysis.indicator == "prev100k.ci.smear.positive.tb" &
                                                  rurality == "rural" ~ (log(prev100k.ci.upper.smear.positive.tb.rural/
                                                                             (1-prev100k.ci.upper.smear.positive.tb.rural)) -
                                                                         log(prev100k.ci.lower.smear.positive.tb.rural /
                                                                                 (1-prev100k.ci.lower.smear.positive.tb.rural)))/3.92, 
                                              
                                              rurality.analysis.indicator %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                            "n.culture.positive.tb", "prev100k.smear.positive.tb") & rurality == "urban" ~ 
                                                  log(1/(n.participants.urban*`Adjusted Prevalence`*(1-`Adjusted Prevalence`)))^2,
                                              rurality.analysis.indicator  %in% c("n.bacteriological.tb", "n.smear.positive.tb", 
                                                                             "n.culture.positive.tb", "prev100k.smear.positive.tb") & rurality == "rural" ~ 
                                                  log(1/(n.participants.rural*`Adjusted Prevalence`*(1-`Adjusted Prevalence`)))^2
           ))



##############################################################################|
##### BAYESIAN MODEL #########################################################
##############################################################################|

##############################################################################|
##### Model 1: Only survey level random effects
##############################################################################|

model1 <- brms::brm(
    formula = `LogOdds` | se(`LogOddsStandardError`, sigma = TRUE) ~ 1 + (1 | id) + rurality,
    data = SDS,
    prior = prior(normal(0, 10), class = Intercept) +
        prior(normal(0, 10), class = b) +
        prior(exponential(1), class = sd),
    family = "gaussian",
    control = list(adapt_delta = 0.99),
    cores = 4,
    chains = 4,
    iter = 4000
)

####### Model 1 Diagnostics

summary(model1)

plot(model1)

pairs(model1)

pp_check(model1, ndraws = 100)
####### model 1 results
model1_ur_est0 <- model1 %>% 
    as_draws_df() %>%
    select(b_Intercept, b_ruralityurban) %>%
    transmute(
        Rural = expit(b_Intercept) * 1e5,
        Urban = expit(b_Intercept + b_ruralityurban) * 1e5
    ) %>%
    mutate(urRatio = Urban / Rural,
           model = "Only random effects")

model1_est <- melt(model1_ur_est0)
model1_est_summary <- model1_est %>% reframe(meanPrev = mean(value), .by = c(variable)); model1_est_summary

ggplot(data = model1_est) +
    stat_halfeye(aes(x = value, fill = model), normalize = "panels", alpha = 0.5, size = 5) +
    facet_grid(model ~ fct_relevel(variable,
                                   "Urban", "Rural", "urRatio"), 
               scales = "free_x") +
    labs(x = "", y = "") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")

####### Urb:rural ratio 0.98
############

########  Model 2: survey level random effects + WHO region level fixed effects
model2_ur <- brms::brm(
    formula = `LogOdds` | se(`LogOddsStandardError`, sigma = TRUE) ~ 1 + rurality + WHO.region + (1 | id),
    data = SDS,
    prior = prior(normal(0, 10), class = Intercept) +
        prior(normal(0, 10), class = b) +
        prior(exponential(1), class = sd),
    family = "gaussian",
    control = list(adapt_delta = 0.90),
    cores = 4,
    chains = 4,
    iter = 4000
)


######## Model 2 results
model2_ur_est0 <- model2_ur %>%
    add_epred_draws(
        newdata = crossing(WHO.region = unique(SDS$WHO.region), 
                           rurality = c("urban", "rural"),
                           LogOddsStandardError = 1),
        re_formula = NA
    ) %>%
    ungroup() %>%
    mutate(value = expit(.epred) * 1e5) %>%
    select(WHO.region, rurality, .draw, value) %>%
    pivot_wider(names_from = rurality, values_from = value) %>%
    mutate(urRatio = urban / rural) %>%
    select(-.draw) %>%
    pivot_longer(cols = c(urban, rural, urRatio)) %>%
    mutate(model = "Country level fixed effects") %>%
    rename(rurality = name)

model2_ur_est_summary <- model2_ur_est0 %>%
    reframe(meanPrev = mean(value), .by = c(rurality, WHO.region))

model2_ur_est_summary

######plot model 2
ggplot(data = model2_ur_est0) +
    stat_pointinterval(
        aes(x = value, colour = WHO.region),
        position = position_dodge(width = 0.5),
        size = 5
    ) +
    facet_grid(model ~ fct_relevel(rurality,
                                   "urban", "rural", "urRatio"),
               scales = "free_x") +
    labs(x = "", y = "") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")



##### Model region!
regionEffectMod_ur <- brms::brm(
    formula = LogOdds | se(LogOddsStandardError, sigma = TRUE) ~ 1 + rurality + (1 + rurality | WHO.region/study.country) + (1 | id),
    data = SDS,
    prior = prior(normal(0, 10), class = Intercept) +
        prior(normal(0, 10), class = b) +
        prior(exponential(1), class = sd),
    family = "gaussian",
    control = list(adapt_delta = 0.90),
    cores = 4,
    chains = 4,
    iter = 4000
)

# Create new data for predictions
nd_ur <- SDS %>%
    select(WHO.region, study.country) %>%
    distinct() %>%
    crossing(rurality = c("urban", "rural")) %>%
    mutate(LogOddsStandardError = 1)

# Draw posterior predictions
regionEffectModEst_ur <- regionEffectMod_ur %>%
    add_epred_draws(newdata = nd_ur, re_formula = ~(1 + rurality | WHO.region)) %>%
    ungroup() %>%
    mutate(value = expit(.epred) * 1e5) %>%
    select(study.country, WHO.region, rurality, value, .draw) %>%
    pivot_wider(names_from = rurality, values_from = value) %>%
    unnest_longer(col = c(urban, rural)) %>%
    mutate(urban_rural_Ratio = urban / rural) %>%
    pivot_longer(cols = c(urban, rural, urban_rural_Ratio)) %>%
    dplyr::rename(rurality = name) %>%
    mutate(type = "Region mean and 95% credible interval",
           WHO.region = gsub("\\(WHO\\)", "region", WHO.region))

# Summarise posterior draws
regionEffectModEst_ur %>%
    group_by(rurality, WHO.region) %>%
    mean_qi(value)

# Get empirical (raw) values
region_empirical_ur <- SDS %>%
    select(id, WHO.region, study.country, rurality, LogOdds) %>%
    mutate(value = expit(LogOdds) * 1e5) %>%
    pivot_wider(names_from = rurality, values_from = value, id_cols = c(id, study.country, WHO.region)) %>%
    mutate(urban_rural_Ratio = urban / rural) %>%
    pivot_longer(cols = c(urban, rural, urban_rural_Ratio)) %>%
    mutate(type = "Survey-specific ratios",
           type2 = "Survey-specific TB prevalence",
           WHO.region = gsub("\\(WHO\\)", "region", WHO.region))

# Quick plot: empirical data points
ggplot(region_empirical_ur %>% filter(name != "urban_rural_Ratio")) +
    geom_point(aes(y = WHO.region, x = value, color = name), size = 2) +
    theme_minimal()



##### Set your palette
myPal <- c("black", "blue")

# 1. Plot Urban vs Rural prevalence
regionEffectModEst_ur %>%
    filter(rurality != "urban_rural_Ratio") %>%
    ggplot() +
    geom_point(data = region_empirical_ur %>%
                   filter(name != "urban_rural_Ratio"), 
               aes(y = name, x = value, color = type2),
               alpha = 0.5, size = 2) +
    stat_pointinterval(aes(x = value, y = rurality, color = type), .width = 0.95) +
    facet_wrap(~WHO.region, scales = "free_x") +
    scale_y_discrete(limits = rev) +
    expand_limits(x = 0) +
    theme_minimal(base_size = 10) +
    scale_color_manual(values = myPal) +
    theme(
        legend.title = element_blank(), 
        legend.position = "bottom",  # <------ this is the main fix!
        legend.box.margin = margin(t = 10),  # add some spacing above the legend
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    ylab("Urban/Rural") +
    xlab("Bacteriologically confirmed TB prevalence (per 100,000 persons)")

# 2. Plot Urban-to-Rural ratios

regionEffectModEst_ur %>%
    filter(rurality == "urban_rural_Ratio") %>%
    ggplot() +
    geom_point(data = region_empirical_ur %>%
                   filter(name == "urban_rural_Ratio"), 
               aes(y = WHO.region, x = value, color = type),
               alpha = 0.6, size = 3) +
    stat_pointinterval(aes(x = value, y = WHO.region, color = type), .width = 0.95) +
    scale_y_discrete(limits = rev,
                     labels = label_wrap(10)) +
    expand_limits(y = 0) +
    theme_bw(base_size = 10) +
    scale_x_continuous(breaks = seq(0, 7, 1)) +
    scale_color_manual(values = myPal) +
    theme(legend.title = element_blank(), 
          legend.position = c(.73, .07),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 22)) +
    ylab("WHO region") +
    xlab("Urban-to-rural ratio of bacteriologically confirmed TB prevalence") +
    ggtitle("Urban-to-Rural Ratios of Bacteriologically Confirmed TB Prevalence by WHO Region")

