
prevModList <- list("intcpt" =  bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + (1 + Sex | p | study.country)), 
                    "no-gdi" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                   smoking + Sex:smoking +
                                   HIV.AIDS + Sex:HIV.AIDS + 
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)) ,
                    "no-alc" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   GDI + Sex:GDI + 
                                   smoking + Sex:smoking +
                                   HIV.AIDS + Sex:HIV.AIDS + 
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-year" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                    bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-smoke" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                     year_z + Sex:year_z +
                                     GDI + Sex:GDI + 
                                     alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                     HIV.AIDS + Sex:HIV.AIDS + 
                                     type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                     bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-hiv" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   GDI + Sex:GDI + 
                                   alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                   smoking + Sex:smoking +
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-diab" = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                    year_z + Sex:year_z +
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-bmi"  = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                    year_z + Sex:year_z +
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                    (1 + Sex | p | study.country)),
                    "all"  = bf(logTbPrev  | se(logTbPrevSE,  sigma = TRUE) ~ 1 + Sex + 
                                    year_z + Sex:year_z +
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                    bmi + Sex:bmi + (1 + Sex | p | study.country)))
                    
notifModList <- list("intcpt" =  bf(logNotifRate | se(logNotifRateSE, sigma = TRUE) ~ 1 + Sex + (1 + Sex | p | study.country)), 
                     "no-gdi" =  bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                   smoking + Sex:smoking +
                                   HIV.AIDS + Sex:HIV.AIDS + 
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)) ,
                    "no-alc" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   GDI + Sex:GDI + 
                                   smoking + Sex:smoking +
                                   HIV.AIDS + Sex:HIV.AIDS + 
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-year" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                    bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-smoke" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                     year_z + Sex:year_z +
                                     GDI + Sex:GDI + 
                                     alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                     HIV.AIDS + Sex:HIV.AIDS + 
                                     type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                     bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-hiv" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   GDI + Sex:GDI + 
                                   alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                   smoking + Sex:smoking +
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-diab" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                    year_z + Sex:year_z +
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    bmi + Sex:bmi + (1 + Sex | p | study.country)),
                    "no-bmi"  = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                    year_z + Sex:year_z +
                                    GDI + Sex:GDI + 
                                    alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                    smoking + Sex:smoking +
                                    HIV.AIDS + Sex:HIV.AIDS + 
                                    type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                    (1 + Sex | p | study.country)), 
                    "all" = bf(logNotifRate | se(logNotifRateSE,   sigma = TRUE) ~ 1 + Sex + 
                                   year_z + Sex:year_z +
                                   GDI + Sex:GDI + 
                                   alcohol.use.disorder + Sex:alcohol.use.disorder + 
                                   smoking + Sex:smoking +
                                   HIV.AIDS + Sex:HIV.AIDS + 
                                   type2.diabetes.mellitus + Sex:type2.diabetes.mellitus +
                                   bmi + Sex:bmi + (1 + Sex | p | study.country)))

looList <- list()
modResList <- list()

for (mod in names(notifModList)){
    
    print(mod)
    
    modResList[[mod]] <- brm(
        formula = prevModList[[mod]] + notifModList[[mod]] + set_rescor(FALSE),
        data = notifPrevRatio2 %>% na.omit(),
        family = gaussian(), 
        # sample_prior = "no-only",
        prior = c(
            prior(student_t(7, 0, 1.5), class = Intercept, resp = "logTbPrev"),
            prior(student_t(7, 0, 1.5), class = Intercept, resp = "logNotifRate"),
            prior(normal(0,1), class = b, resp = "logTbPrev"),
            prior(normal(0,1), class = b, resp = "logNotifRate"),
            prior(exponential(2), class = sd, resp = "logTbPrev"),
            prior(exponential(2), class = sd, resp = "logNotifRate")
        ),
        control = list(adapt_delta = 0.99),
        chains = 4,
        cores = 4,
        iter = 4000, 
        warmup = 1000
    )
    
    looList[[mod]] <- loo(modResList[[mod]], 
                          model_names = as.character(mod))
}

looList2 <- list()
for (mod in names(notifModList)){
    looList2[[mod]] <- brms::loo(modResList[[mod]], 
                                 model_names = as.character(mod))
}

loo_compare(looList2[["intcpt"]], 
            looList2[["no-gdi"]],
            looList2[["no-alc"]], 
            looList2[["no-year"]],
            looList2[["no-smoke"]], 
            looList2[["no-hiv"]], 
            looList2[["no-diab"]],
            looList2[["no-bmi"]], 
            looList2[["all"]])
