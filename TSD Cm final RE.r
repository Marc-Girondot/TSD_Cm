library(embryogrowth)
library(loo)
library(brms)
# library(openxlsx)

Cm_ROSIE <- subset(ROSIE, subset = Species == "Chelonia_mydas")
nrow(Cm_ROSIE)
# unique(Cm_ROSIE$Incubation_Setup)
# unique(Cm$Sexing_Method)
# unique(Cm$Chemical_Treatment)
Cm <- subset(Cm_ROSIE, subset = (Data_Elsewhere == 0) & (Incubation_Setup == "Constant") & (Sexing_Method == "Histology"))
Cm <- Cm[(Cm$Chemical_Treatment != "DDE") | (is.na(Cm$Chemical_Treatment)), ]
Cm[which(grepl("Miller", Cm$Source)), "Intersex"] <- 0
Cm$Males <- Cm$Males + Cm$Intersex
Cm$Total_Sexed <- Cm$Males + Cm$Females
Cm <- subset(Cm, subset = (Total_Sexed != 0))


Cm$ID <- as.character(as.numeric(as.factor(Cm[, "Source"])))
Cm$Clutch <- paste0(Cm$Clutch_ID, "_", Cm$ID)

Cm_all <- Cm[, c("Males", "Females", "Intersex", "Total_Sexed", "RMU.2023", "Clutch", "ID", "Mean_Temp", "Source")]


tsd_flexit_all <- tsd(males = Cm_all$Males, females = Cm_all$Females, temperatures = Cm_all$Mean_Temp, 
                  equation = "flexit**", parameters.initial = c(P=29, SL=2, SH=2))

priors_all <- tsd_MHmcmc_p(result = tsd_flexit_all, default = "dunif")
flexit_CM_all <- tsd_MHmcmc(result = tsd_flexit_all, parametersMCMC = priors_all, 
                        n.adapt = 1000, thin = 10, n.iter = 20000)

outFl_all <- as.parameters(flexit_CM_all, index = 1:2000)
outFl_all <- cbind(outFl_all, TRTL=outFl_all[, "P"]-outFl_all[, "SL"])
outFl_all <- cbind(outFl_all, TRTH=outFl_all[, "P"]+outFl_all[, "SH"])
outFl_all <- cbind(outFl_all, TRT=outFl_all[, "SL"]+outFl_all[, "SH"])

apply(outFl_all, MARGIN=2, quantile, probs = c(0.025, 0.5, 0.975))


Cm <- subset(Cm, subset = (Cm$Fluctuation < 0.5) | (is.na(Cm$Fluctuation)))
Cm <- subset(Cm, subset = (Cm$Mean_Temp_SD < 0.2)| (is.na(Cm$Mean_Temp_SD)))

# Cm$Mean_Temp
Cm <- Cm[, c("Males", "Females", "Intersex", "Total_Sexed", "RMU.2023", "Clutch", "ID", "Mean_Temp", "Source")]

library(openxlsx)
write.xlsx(Cm, file="SM2.xlsx")

sum(Cm$Total_Sexed)

length(unique(Cm$Mean_Temp))
length(unique(Cm$Source))
unique(Cm$Source)
unique(Cm$RMU.2023)
rm(Cm_ROSIE)

Cm.SM2 <- data.frame(Source=character(0), Total.Sexed=numeric(0), 
                     Total.Temperatures=numeric(0), 
                     Total.Clutches=numeric(0), 
                     RMU.2023=character(0))
for (i in unique(Cm$Source)) {
  Cm.int <- subset(Cm, subset = Source==i)
  Cm.SM2 <- rbind(Cm.SM2, data.frame(Source=i, Total.Sexed=sum(Cm.int$Total_Sexed), 
                  Total.Temperatures=length(unique(Cm.int$Mean_Temp)), 
                  Total.Clutches=length(unique(Cm.int$Clutch)), 
                  RMU.2023=Cm.int$RMU.2023[1]))
}

write.xlsx(Cm.SM2, file="Cm_SM2.xlsx")

#################################

Cm_NA <- subset(Cm, subset = (RMU.2023 == "North Atlantic"))
tsd_flexit_NA <- tsd(males = Cm_NA$Males, females = Cm_NA$Females, temperatures = Cm_NA$Mean_Temp, 
                     equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors_NA <- tsd_MHmcmc_p(result = tsd_flexit_NA, default = "dunif")
flexit_CM_NA <- tsd_MHmcmc(result = tsd_flexit_NA, parametersMCMC = priors_NA, 
                           n.iter = 10000,
                           n.adapt = 1000,
                           thin = 10)

png(filename = 'FlexitCM_noPoints_noSD_Bayesian_NA.png', width=7*72, height=5*72, pointsize=12, res=72)
plot(tsd_flexit_NA, resultmcmc = flexit_CM_NA, show.observations=FALSE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))
dev.off()

Cm_EISA <- subset(Cm, subset = (RMU.2023 == "East Indian and Southeast Asia"))
tsd_flexit_EISA <- tsd(males = Cm_EISA$Males, females = Cm_EISA$Females, temperatures = Cm_EISA$Mean_Temp, 
                       equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors_EISA <- tsd_MHmcmc_p(result = tsd_flexit_EISA, default = "dunif")
flexit_CM_EISA <- tsd_MHmcmc(result = tsd_flexit_EISA, parametersMCMC = priors_EISA, 
                             n.iter = 10000,
                             n.adapt = 1000,
                             thin = 10)

plot(tsd_flexit_EISA, resultmcmc = flexit_CM_EISA, show.observations=TRUE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))

png(filename = 'FlexitCM_noPoints_noSD_Bayesian_EISA.png', width=7*72, height=5*72, pointsize=12, res=72)
plot(tsd_flexit_EISA, resultmcmc = flexit_CM_EISA, show.observations=FALSE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))
dev.off()

Cm_NI <- subset(Cm, subset = (RMU.2023 == "Northwest Indian"))
tsd_flexit_NI <- tsd(males = Cm_NI$Males, females = Cm_NI$Females, temperatures = Cm_NI$Mean_Temp, 
                     equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors_NI <- tsd_MHmcmc_p(result = tsd_flexit_NI, default = "dunif")
flexit_CM_NI <- tsd_MHmcmc(result = tsd_flexit_NI, parametersMCMC = priors_NI, 
                           n.iter = 10000,
                           n.adapt = 1000,
                           thin = 10)

plot(tsd_flexit_NI, resultmcmc = flexit_CM_NI, show.observations=TRUE, show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))

png(filename = 'FlexitCM_noPoints_noSD_Bayesian_NI.png', width=7*72, height=5*72, pointsize=12, res=72)
plot(tsd_flexit_NI, resultmcmc = flexit_CM_NI, show.observations=FALSE, show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))
dev.off()

Cm_SP <- subset(Cm, subset = (RMU.2023 == "Southwest Pacific"))
tsd_flexit_SP <- tsd(males = Cm_SP$Males, females = Cm_SP$Females, temperatures = Cm_SP$Mean_Temp, 
                     equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors_SP <- tsd_MHmcmc_p(result = tsd_flexit_SP, default = "dunif")
flexit_CM_SP <- tsd_MHmcmc(result = tsd_flexit_SP, parametersMCMC = priors_SP, 
                           n.iter = 10000,
                           n.adapt = 1000,
                           thin = 10)
plot(tsd_flexit_SP, resultmcmc = flexit_CM_SP, show.observations=TRUE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))


png(filename = 'FlexitCM_noPoints_noSD_Bayesian_SP.png', width=7*72, height=5*72, pointsize=12, res=72)
plot(tsd_flexit_SP, resultmcmc = flexit_CM_SP, show.observations=FALSE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))
dev.off()

Cm_SA <- subset(Cm, subset = (RMU.2023 == "South Atlantic"))
tsd_flexit_SA <- tsd(males = Cm_SA$Males, females = Cm_SA$Females, temperatures = Cm_SA$Mean_Temp, 
                     equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors_SA <- tsd_MHmcmc_p(result = tsd_flexit_SA, default = "dunif")
flexit_CM_SA <- tsd_MHmcmc(result = tsd_flexit_SA, parametersMCMC = priors_SA, 
                           n.iter = 10000,
                           n.adapt = 1000,
                           thin = 10)

plot(tsd_flexit_SA, resultmcmc = flexit_CM_SA, show.observations=TRUE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))

png(filename = 'FlexitCM_noPoints_noSD_Bayesian_SA.png', width=7*72, height=5*72, pointsize=12, res=72)
plot(tsd_flexit_SA, resultmcmc = flexit_CM_SA, show.observations=FALSE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35))
dev.off()

tsd_flexit <- tsd(males = Cm$Males, females = Cm$Females, temperatures = Cm$Mean_Temp, 
                     equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors <- tsd_MHmcmc_p(result = tsd_flexit, default = "dunif")
flexit_CM <- tsd_MHmcmc(result = tsd_flexit, parametersMCMC = priors, 
                           n.iter = 10000,
                           n.adapt = 1000,
                           thin = 10)

flexit_CM_loo <- loo::loo(flexit_CM$WAIC)
flexit_CM_SA_loo <- loo::loo(flexit_CM_SA$WAIC)
flexit_CM_SP_loo <- loo::loo(flexit_CM_SP$WAIC)
flexit_CM_NA_loo <- loo::loo(flexit_CM_NA$WAIC)
flexit_CM_EISA_loo <- loo::loo(flexit_CM_EISA$WAIC)
flexit_CM_NI_loo <- loo::loo(flexit_CM_NI$WAIC)

WAIC_CM <- cbind(as.data.frame(flexit_CM_SA$WAIC), as.data.frame(flexit_CM_SP$WAIC), 
                 as.data.frame(flexit_CM_NA$WAIC), as.data.frame(flexit_CM_EISA$WAIC), 
                 as.data.frame(flexit_CM_NI$WAIC))
WAIC_CM <- as.matrix(WAIC_CM)
flexit_CM_WAIC_CM_loo <- loo::loo(WAIC_CM)

llok <- loo_compare(list(flexit_CM_FE_RMU=flexit_CM_WAIC_CM_loo, 
                         flexit_CM_NoRMU=flexit_CM_loo))
print(llok, simplify=FALSE)

options(mc.cores=1)

###################################
flexit_formula <- bf(
  Males | trials(Total_Sexed) ~ 
    (0.999998/(
      1+exp(
        ((-2.94443897916644*(P-Mean_Temp))
         /
           (
             (SL/ (1+ exp(100*(Mean_Temp - P))))+
               (SH/ (1+ exp(100*(P - Mean_Temp))))
           )
        )
      )
    ) ) + (1-0.999999)
  ,
  SL + SH + P ~ 1 ,
  nl = TRUE, 
  family = binomial(link="identity")
)

control <- list(
  # adapt_engaged = TRUE,
  adapt_delta = 0.99, #increased from default of 0.8
  # stepsize = 0.05, # 0.05 default
  max_treedepth = 20
)

prior_ini <- set_prior(prior = "normal(29, 0.5)", class = "b", coef="", 
                       nlpar = "P", lb = 28, ub = 30, check = TRUE) + 
  set_prior(prior = "student_t(20, 0, 2)", class = "b", coef="", 
            nlpar = "SL", lb = 0, ub = 4, check = TRUE) + 
  set_prior(prior = "student_t(20, 0, 2)", class = "b", coef="", 
            nlpar = "SH", lb = 0, ub = 4, check = TRUE)

flexit_noRMU_noID_noClutch <- brm(
  formula = flexit_formula,
  data = Cm,
  save_pars = save_pars(all = TRUE), 
  chains = getOption("mc.cores"),
  cores = getOption("mc.cores"),
  sample_prior = TRUE, 
  warmup = 10000,
  iter = 50000,
  thin = 10, 
  prior = prior_ini, 
  control = control, 
  seed = 123, 
  init=rep(list(list(b_P_Intercept=29.2, b_SL_Intercept=2, b_SH_Intercept=2.3)),
           getOption("mc.cores"))
)
summary(flexit_noRMU_noID_noClutch)

save(flexit_noRMU_noID_noClutch, file=file.path("dataOut", "tsd", "flexit_noRMU_noID_noClutch.Rdata"))

plot(mcmc_plot(flexit_noRMU_noID_noClutch, type = "areas", prob = 0.95, variable = "b_P_Intercept"))
plot(mcmc_plot(flexit_noRMU_noID_noClutch, type = "areas", prob = 0.95, variable = c("b_SL_Intercept", "b_SH_Intercept")))

conditions <- NULL
plot(conditional_effects(flexit_noRMU_noID_noClutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = TRUE, ndraws = 100))
plot(conditional_effects(flexit_noRMU_noID_noClutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = FALSE, ndraws = 100))



###################################

flexit_formula <- bf(
  Males | trials(Total_Sexed) ~ 
    (0.999998/(
      1+exp(
        ((-2.94443897916644*(P-Mean_Temp))
         /
           (
             (SL/ (1+ exp(100*(Mean_Temp - P))))+
               (SH/ (1+ exp(100*(P - Mean_Temp))))
           )
        )
      )
    ) ) + (1-0.999999)
  ,
  SL + SH + P ~ 1 + (1 | RMU.2023),
  nl = TRUE, 
  family = binomial(link="identity")
)

# prior_ini <- default_prior(flexit_formula, 
#                            data = Cm)

prior_ini <- structure(list(prior = c("normal(29, 0.5)", ""         , "normal(0.5, 0.1)"    , ""        , ""         , "normal(2, 0.5)"       , ""         , "normal(0.5, 0.1)"    , ""        , ""         , "normal(2, 0.5)"       , ""         , "normal(0.5, 0.1)"    , ""        , ""         ), 
                            class = c("b"              , "b"        , "sd"                  , "sd"      , "sd"       , "b"                    , "b"        , "sd"                  , "sd"      , "sd"       , "b"                    , "b"        , "sd"                  , "sd"      , "sd"       ), 
                            coef =  c(""               , "Intercept", ""                    , ""        , "Intercept", ""                     , "Intercept", ""                    , ""        , "Intercept", ""                     , "Intercept", ""                    , ""        , "Intercept"), 
                            group = c(""               , ""         , ""                    , "RMU.2023"  , "RMU.2023"   , ""                     , ""         , ""                    , "RMU.2023"  , "RMU.2023"   , ""                     , ""         , ""                    , "RMU.2023"  , "RMU.2023"   ), 
                            resp =  c(""               ,  ""        , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         ), 
                            dpar =  c(""               ,  ""        , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         ), 
                            nlpar = c("P"              ,  "P"       , "P"                   , "P"       , "P"        , "SH"                   , "SH"       , "SH"                  , "SH"      , "SH"       , "SL"                   , "SL"       , "SL"                  , "SL"      , "SL"       ), 
                            lb =    c("28"             , ""         , "0.001"               , ""        , ""         , "0"                    , ""         , "0.001"               , ""        , ""         , "0"                    , ""         , "0.001"               , ""        , ""         ), 
                            ub =    c("30"             , ""         , "3"                   , ""        , ""         , "5"                    , ""         , "3"                   , ""        , ""         , "5"                    , ""         , "3"                   , ""        , ""         ), 
                            source = c("default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default")),           
                       row.names = c(NA, -15L), class = c("brmsprior", "data.frame"))

control <- list(
  # adapt_engaged = TRUE,
  adapt_delta = 0.99, #increased from default of 0.8
  # stepsize = 0.05, # 0.05 default
  max_treedepth = 20
)

flexit_RMU_noID_noClutch <- brm(
  formula = flexit_formula,
  data = Cm,
  save_pars = save_pars(all = TRUE), 
  chains = getOption("mc.cores"),
  cores = getOption("mc.cores"),
  sample_prior = TRUE, 
  warmup = 10000,
  iter = 50000,
  thin = 10, 
  prior = prior_ini, 
  control = control, 
  seed = 123, 
  init=rep(list(list(b_P_Intercept=29.2, b_SL_Intercept=2, b_SH_Intercept=2.3, 
                     sd_RMU.2023__SL_Intercept=0.5, sd_RMU.2023__P_Intercept=0.5, sd_RMU.2023__SH_Intercept=0.5)),
           getOption("mc.cores"))
)
summary(flexit_RMU_noID_noClutch)

save(flexit_RMU_noID_noClutch, file=file.path("dataOut", "tsd", "flexit_RMU_noID_noClutch.Rdata"))

load(file=file.path("dataOut", "tsd", "flexit_RMU_noID_noClutch.Rdata"))

plot(density(prior_draws(flexit_RMU_noID_noClutch, variable = "b_P_Intercept")[, 1]))
plot(density(prior_draws(flexit_RMU_noID_noClutch, variable = "b_SL_Intercept")[, 1]))
plot(density(prior_draws(flexit_RMU_noID_noClutch, variable = "b_SH_Intercept")[, 1]))
plot(density(prior_draws(flexit_RMU_noID_noClutch, variable = "sd_RMU.2023")[, 1]))
plot(density(prior_draws(flexit_RMU_noID_noClutch, variable = "sd_RMU.2023__1")[, 1]))

hist(prior_draws(flexit_RMU_noID_noClutch)[, "b_P"])

plot(mcmc_plot(flexit_RMU_noID_noClutch, type = "areas", prob = 0.95, variable = "b_P_Intercept"))
plot(mcmc_plot(flexit_RMU_noID_noClutch, type = "areas", prob = 0.95, variable = c("b_SL_Intercept", "b_SH_Intercept")))

conditions <- make_conditions(flexit_RMU_noID_noClutch, vars="RMU.2023")
plot(conditional_effects(flexit_RMU_noID_noClutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = TRUE, ndraws = 100))
plot(conditional_effects(flexit_RMU_noID_noClutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = FALSE, ndraws = 100))

kx <- as_draws(flexit_RMU_noID_noClutch)
# J'ai bien les 8*(20 000-5 000)/10=12 000 itérations
P_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_P_Intercept)))
SH_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_SH_Intercept)))
SL_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_SL_Intercept)))
quantile(x=SH_posterior+SL_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=P_posterior, probs = c(0.025, 0.5, 0.975))


######################

flexit_formula <- bf(
  Males | trials(Total_Sexed) ~ 
    (0.999998/(
      1+exp(
        ((-2.94443897916644*(P-Mean_Temp))
         /
           (
             (SL/ (1+ exp(100*(Mean_Temp - P))))+
               (SH/ (1+ exp(100*(P - Mean_Temp))))
           )
        )
      )
    ) ) + (1-0.999999)
  ,
  SL + SH + P ~ 1 + (1 | Clutch),
  nl = TRUE, 
  family = binomial(link="identity")
)

# prior_ini <- default_prior(flexit_formula, 
#                            data = Cm)

prior_ini <- structure(list(prior = c("normal(29, 0.5)", ""         , "normal(0.5, 0.1)"    , ""        , ""         , "normal(2, 0.5)"       , ""         , "normal(0.5, 0.1)"    , ""        , ""         , "normal(2, 0.5)"       , ""         , "normal(0.5, 0.1)"    , ""        , ""         ), 
                            class = c("b"              , "b"        , "sd"                  , "sd"      , "sd"       , "b"                    , "b"        , "sd"                  , "sd"      , "sd"       , "b"                    , "b"        , "sd"                  , "sd"      , "sd"       ), 
                            coef =  c(""               , "Intercept", ""                    , ""        , "Intercept", ""                     , "Intercept", ""                    , ""        , "Intercept", ""                     , "Intercept", ""                    , ""        , "Intercept"), 
                            group = c(""               , ""         , ""                    , "Clutch"  , "Clutch"   , ""                     , ""         , ""                    , "Clutch"  , "Clutch"   , ""                     , ""         , ""                    , "Clutch"  , "Clutch"   ), 
                            resp =  c(""               ,  ""        , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         ), 
                            dpar =  c(""               ,  ""        , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         , ""                     , ""         , ""                    , ""        , ""         ), 
                            nlpar = c("P"              ,  "P"       , "P"                   , "P"       , "P"        , "SH"                   , "SH"       , "SH"                  , "SH"      , "SH"       , "SL"                   , "SL"       , "SL"                  , "SL"      , "SL"       ), 
                            lb =    c("28"             , ""         , "0.001"               , ""        , ""         , "0"                    , ""         , "0.001"               , ""        , ""         , "0"                    , ""         , "0.001"               , ""        , ""         ), 
                            ub =    c("30"             , ""         , "3"                   , ""        , ""         , "5"                    , ""         , "3"                   , ""        , ""         , "5"                    , ""         , "3"                   , ""        , ""         ), 
                            source = c("default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default", "default")),           
                       row.names = c(NA, -15L), class = c("brmsprior", "data.frame"))

control <- list(
  # adapt_engaged = TRUE,
  adapt_delta = 0.99, #increased from default of 0.8
  # stepsize = 0.05, # 0.05 default
  max_treedepth = 20
)

flexit_noRMU_noID_Clutch <- brm(
  formula = flexit_formula,
  data = Cm,
  save_pars = save_pars(all = TRUE), 
  chains = getOption("mc.cores"),
  cores = getOption("mc.cores"),
  sample_prior = TRUE, 
  thin = 10, 
  warmup = 10000,
  iter = 50000,
  prior = prior_ini, 
  control = control, 
  seed = 123, 
  init=rep(list(list(b_P_Intercept=29.2, b_SL_Intercept=2, b_SH_Intercept=2.3, 
                     sd_Clutch__SL_Intercept=0.5, sd_Clutch__P_Intercept=0.5, sd_Clutch__SH_Intercept=0.5)),
           getOption("mc.cores"))
)
summary(flexit_noRMU_noID_Clutch)

save(flexit_noRMU_noID_Clutch, file=file.path("dataOut", "tsd", "flexit_noRMU_noID_Clutch.Rdata"))

load(file=file.path("dataOut", "tsd", "flexit_noRMU_noID_Clutch.Rdata"))

plot(density(prior_draws(flexit_noRMU_noID_Clutch, variable = "b_P_Intercept")[, 1]))
plot(density(prior_draws(flexit_noRMU_noID_Clutch, variable = "b_SL_Intercept")[, 1]))
plot(density(prior_draws(flexit_noRMU_noID_Clutch, variable = "b_SH_Intercept")[, 1]))
plot(density(prior_draws(flexit_noRMU_noID_Clutch, variable = "sd_Clutch")[, 1]))
plot(density(prior_draws(flexit_noRMU_noID_Clutch, variable = "sd_Clutch__1")[, 1]))

hist(prior_draws(flexit_noRMU_noID_Clutch)[, "b_P"])
hist(prior_draws(flexit_noRMU_noID_Clutch)[, "b_SL"])
hist(prior_draws(flexit_noRMU_noID_Clutch)[, "b_SH"])
mcmc_plot(flexit_noRMU_noID_Clutch)
plot(mcmc_plot(flexit_noRMU_noID_Clutch, type = "areas", prob = 0.95, variable = "b_P_Intercept"))
plot(mcmc_plot(flexit_noRMU_noID_Clutch, type = "areas", prob = 0.95, variable = c("b_SL_Intercept", "b_SH_Intercept")))
plot(mcmc_plot(flexit_noRMU_noID_Clutch, type = "areas", prob = 0.95, variable = c("sd_Clutch__P_Intercept", "sd_Clutch__SL_Intercept", "sd_Clutch__SH_Intercept")))

P <- mcmc_plot(flexit_noRMU_noID_Clutch, type = "areas", prob = 0.95, variable = "b_P_Intercept")

conditions <- make_conditions(flexit_noRMU_noID_Clutch, vars="Clutch")
svg(filename = "Figure 1 SM.svg", width = 8, height = 8, pointsize = 10)
plot(conditional_effects(flexit_noRMU_noID_Clutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = TRUE, ndraws = 100))
dev.off()

ce <- conditional_effects(flexit_noRMU_noID_Clutch, categorical = FALSE, effects='Mean_Temp', 
                          condition=conditions, robust = TRUE, 
                          re_formula=NULL, spaghetti = FALSE, ndraws = 100)
ce_data <- ce[[1]]

plot(conditional_effects(flexit_noRMU_noID_Clutch, categorical = FALSE, effects='Mean_Temp', 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = FALSE, ndraws = 100))



conditions <- NULL
ke_RE <- conditional_effects(flexit_noRMU_noID_Clutch, categorical = FALSE, 
                         effects='Mean_Temp', 
                         int_conditions=list(Mean_Temp=seq(from=25, to=35, by=0.1)), 
                         condition=conditions, robust = TRUE, 
                         re_formula=NULL, spaghetti = FALSE, ndraws = 100)
plot(ke_RE)
ke_RE[[1]]
ke <- conditional_effects(flexit_noRMU_noID_Clutch, categorical = FALSE, 
                         effects='Mean_Temp', 
                         int_conditions=list(Mean_Temp=seq(from=25, to=35, by=0.1)), 
                         condition=conditions, robust = TRUE, 
                         re_formula=NA, spaghetti = FALSE, ndraws = 100)
plot(ke)

kx <- as_draws(flexit_noRMU_noID_Clutch, variable = c("b_P_Intercept", "b_SH_Intercept", "b_SL_Intercept", 
                                                      "sd_Clutch__SL_Intercept", "sd_Clutch__P_Intercept", "sd_Clutch__SH_Intercept"))

# J'ai bien les 8*(20 000-5 000)/10=12 000 itérations
P_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_P_Intercept)))
SH_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_SH_Intercept)))
SL_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$b_SL_Intercept)))
sd_Clutch__SH_Intercept_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$sd_Clutch__SH_Intercept)))
sd_Clutch__SL_Intercept_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$sd_Clutch__SL_Intercept)))
sd_Clutch__P_Intercept_posterior <- unname(unlist(lapply(kx, FUN=function(x) x$sd_Clutch__P_Intercept)))

quantile(x=SH_posterior+SL_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=SL_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=SH_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=P_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=sd_Clutch__SL_Intercept_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=sd_Clutch__SH_Intercept_posterior, probs = c(0.025, 0.5, 0.975))
quantile(x=sd_Clutch__P_Intercept_posterior, probs = c(0.025, 0.5, 0.975))



hist(P_posterior)
plot(density(P_posterior, n = 512))

P_posterior <- P_posterior[seq(from=1, to=length(P_posterior), len=10000)]
SH_posterior <- SH_posterior[seq(from=1, to=length(P_posterior), len=10000)]
SL_posterior <- SL_posterior[seq(from=1, to=length(P_posterior), len=10000)]

tsd_flexit <- tsd(males = Cm$Males, females = Cm$Females, temperatures = Cm$Mean_Temp, 
                  equation = "flexit**", parameters.initial = c(P=29, SL=1, SH=1))
priors <- tsd_MHmcmc_p(result = tsd_flexit, default = "dunif")
flexit_MCMC <- tsd_MHmcmc(result = tsd_flexit, parametersMCMC = priors, 
                          n.adapt = 1000, 
                          n.iter = 10000, 
                          thin = 1)

flexit_MCMC$resultMCMC$`1`[, "P"] <- P_posterior
flexit_MCMC$resultMCMC$`1`[, "SL"] <- SL_posterior
flexit_MCMC$resultMCMC$`1`[, "SH"] <- SH_posterior

tsd_flexit$par <- as.parameters(flexit_MCMC, index = "median")

plot(x=tsd_flexit, resultmcmc = flexit_MCMC, 
     show.observations=FALSE, 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35), 
     replicate.CI = 10000)

polygon(x=c(ke_RE[[1]]$Mean_Temp, rev(ke_RE[[1]]$Mean_Temp)), 
        y=c(ke_RE[[1]]$lower__, rev(ke_RE[[1]]$upper__)), 
        col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1), border = NA)

polygon(x=c(ke[[1]]$Mean_Temp, rev(ke[[1]]$Mean_Temp)), 
        y=c(ke[[1]]$lower__, rev(ke[[1]]$upper__)), 
        col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1), border = NA)


###############################


flexit_formula <- bf(
  Males | trials(Total_Sexed) ~ 
    (0.999998/(
      1+exp(
        ((-2.94443897916644*(P-Mean_Temp))
         /
           (
             (SL/ (1+ exp(100*(Mean_Temp - P))))+
               (SH/ (1+ exp(100*(P - Mean_Temp))))
           )
        )
      )
    ) ) + (1-0.999999)
  ,
  SL + SH + P ~ 1 + (1 | RMU.2023 / Clutch),
  nl = TRUE, 
  family = binomial(link="identity")
)

# prior_ini <- default_prior(flexit_formula, 
#                             data = Cm)

prior_ini <- structure(list(
  prior = c("normal(29, 0.5)", "", "student_t(3, 0, 2.5)", "", "", "", "", "student_t(20, 0, 2)", "", "student_t(3, 0, 2.5)", "", "", "", "", "student_t(20, 0, 2)", "", "student_t(3, 0, 2.5)", "", "", "", ""), 
  class = c("b", "b", "sd", "sd", "sd", "sd", "sd", "b", "b", "sd", "sd", "sd", "sd", "sd", "b", "b", "sd", "sd", "sd", "sd", "sd"), 
  coef = c("", "Intercept", "", "", "Intercept", "", "Intercept", "", "Intercept", "", "", "Intercept", "", "Intercept", "", "Intercept", "", "", "Intercept", "", "Intercept"), 
  group = c("", "", "", "RMU.2023", "RMU.2023", "RMU.2023:Clutch", "RMU.2023:Clutch", "", "", "", "RMU.2023", "RMU.2023", "RMU.2023:Clutch", "RMU.2023:Clutch", "", "", "", "RMU.2023", "RMU.2023", "RMU.2023:Clutch", "RMU.2023:Clutch"), 
  resp = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), dpar = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), 
  nlpar = c("P", "P", "P", "P", "P", "P", "P", "SH", "SH", "SH", "SH", "SH", "SH", "SH", "SL", "SL", "SL", "SL", "SL", "SL", "SL"), 
  lb = c("28", "", "0", "", "", "", "", "0", "", "0", "", "", "", "", "0", "", "0", "", "", "", ""), 
  ub = c("30", "", "", "", "", "", "", "7", "", "", "", "", "", "", "7", "", "", "", "", "", ""), 
  source = c("default", 
             "default", "default", "default", "default", "default", "default", 
             "default", "default", "default", "default", "default", "default", 
             "default", "default", "default", "default", "default", "default", 
             "default", "default")), 
  row.names = c(NA, -21L), class = c("brmsprior", "data.frame"))


control <- list(
  # adapt_engaged = TRUE,
  adapt_delta = 0.99, #increased from default of 0.8
  # stepsize = 0.05, # 0.05 default
  max_treedepth = 20
)

flexit_RMU_noID_Clutch <- brm(
  formula = flexit_formula,
  data = Cm,
  save_pars = save_pars(all = TRUE), 
  chains = getOption("mc.cores"),
  cores = getOption("mc.cores"),
  sample_prior = TRUE, 
  thin = 10, 
  warmup = 10000,
  iter = 50000,
  prior = prior_ini, 
  control = control
)

save(flexit_RMU_noID_Clutch, file=file.path("dataOut", "tsd", "flexit_RMU_noID_Clutch.Rdata"))

summary(flexit_RMU_noID_Clutch)

###############################

load(file=file.path("dataOut", "tsd", "flexit_RMU_noID_Clutch.Rdata"))
load(file=file.path("dataOut", "tsd", "flexit_noRMU_noID_noClutch.Rdata"))
load(file=file.path("dataOut", "tsd", "flexit_RMU_noID_noClutch.Rdata"))
load(file=file.path("dataOut", "tsd", "flexit_noRMU_noID_Clutch.Rdata"))


library(loo)
flexit_noRMU_noID_noClutch_loo <- loo(flexit_noRMU_noID_noClutch)
flexit_noRMU_noID_Clutch_loo <- loo(flexit_noRMU_noID_Clutch)
flexit_RMU_noID_noClutch_loo <- loo(flexit_RMU_noID_noClutch)
flexit_RMU_noID_Clutch_loo <- loo(flexit_RMU_noID_Clutch, nsamples=4000)


llok <- loo_compare(list(flexit_noRMU_noID_noClutch=flexit_noRMU_noID_noClutch_loo, 
                       flexit_RMU_noID_noClutch=flexit_RMU_noID_noClutch_loo, 
                       flexit_noRMU_noID_Clutch=flexit_noRMU_noID_Clutch_loo, 
                       flexit_RMU_noID_Clutch=flexit_RMU_noID_Clutch_loo))
print(llok, simplify=FALSE)

llokweight <- loo_model_weights(list(flexit_noRMU_noID_noClutch=flexit_noRMU_noID_noClutch_loo, 
                                     flexit_RMU_noID_noClutch=flexit_RMU_noID_noClutch_loo, 
                                     flexit_noRMU_noID_Clutch=flexit_noRMU_noID_Clutch_loo, 
                                     flexit_RMU_noID_Clutch=flexit_RMU_noID_Clutch_loo))
llokweight
  
tsd_flexit <- tsd(males = Cm$Males, females = Cm$Females, temperatures = Cm$Mean_Temp, 
                  equation = "flexit**", parameters.initial = c(P=29, SL=2, SH=2))

priors <- tsd_MHmcmc_p(result = tsd_flexit, default = "dunif")
flexit_CM <- tsd_MHmcmc(result = tsd_flexit, parametersMCMC = priors, 
                        n.adapt = 1000, thin = 10, n.iter = 20000)

save(tsd_flexit, file=file.path("dataOut", "tsd_flexit.Rdata"))
save(flexit_CM, file=file.path("dataOut", "flexit_CM.Rdata"))

outFl <- as.parameters(flexit_CM, index = 1:2000)
outFl <- cbind(outFl, TRTL=outFl[, "P"]-outFl[, "SL"])
outFl <- cbind(outFl, TRTH=outFl[, "P"]+outFl[, "SH"])
outFl <- cbind(outFl, TRT=outFl[, "SL"]+outFl[, "SH"])

apply(outFl, MARGIN=2, quantile, probs = c(0.025, 0.5, 0.975))


library(HelpersMG)
# svg(filename = 'Figure 1.svg', width=7, height=7, pointsize=12)
png(filename = 'Figure 1.png', width=7*300, height=7*300, pointsize=12, res=300)
layout(1:2)
par(mar=c(3, 4, 1, 2))
plot_errbar(x=1:4, y=llok[c("flexit_noRMU_noID_noClutch", "flexit_RMU_noID_noClutch", 
                            "flexit_noRMU_noID_Clutch", 
                            "flexit_RMU_noID_Clutch"), "looic"], 
            errbar.y = 1.96*llok[c("flexit_noRMU_noID_noClutch", "flexit_RMU_noID_noClutch", "flexit_noRMU_noID_Clutch", 
                                                                                        "flexit_RMU_noID_Clutch"), "se_looic"], xaxt="n", las=1, 
            ylab="LOOIC", bty="n", xlab="")
axis(1, at=1:4, labels = c("None", "RMU", "Clutch", "Clutch in RMU"))
text(x = 1, y=llok["flexit_noRMU_noID_noClutch", "looic"], labels=specify_decimal(llokweight["flexit_noRMU_noID_noClutch"], 3), pos=4)
text(x = 2, y=llok["flexit_RMU_noID_noClutch", "looic"], labels=specify_decimal(llokweight["flexit_RMU_noID_noClutch"], 3), pos=4)
text(x = 3, y=llok["flexit_noRMU_noID_Clutch", "looic"], labels=specify_decimal(llokweight["flexit_noRMU_noID_Clutch"], 3), pos=4)
text(x = 4, y=llok["flexit_RMU_noID_Clutch", "looic"], labels=specify_decimal(llokweight["flexit_RMU_noID_Clutch"], 3), pos=2)

text(x=ScalePreviousPlot(x=0.95, y=0.1)$x, y=ScalePreviousPlot(x=0.95, y=0.1)$y,
     labels = "A", cex=2)


# par(mar=c(4, 4, 1, 2))
par(xpd=TRUE)
plot(tsd_flexit, resultmcmc = flexit_CM, show.observations=TRUE, 
     replicate.CI = 2000, pch=unlist(sapply(Cm$RMU.2023, FUN=function(x) switch(EXPR = x, 
                                                                         "Southwest Pacific"=3, 
                                                                         "South Atlantic"=0, 
                                                                         "Northwest Indian"=16, 
                                                                         "North Atlantic"=15, 
                                                                         "East Indian and Southeast Asia"=1))), 
     show.observations.sd = FALSE, use.ggplot = FALSE, xlim=c(25, 35), mar=c(4, 4, 2.5, 1))

legend("topright", pch=c(15, 0, 16, 1, 3), legend = c("NA", "SA", "NWI", "EISA", "SP"))

text(x=ScalePreviousPlot(x=0.95, y=0.1)$x, y=ScalePreviousPlot(x=0.95, y=0.1)$y, 
     labels = "B", cex=2)


dev.off()

as.parameters(flexit_CM, index = "quantile")
k <- as.parameters(flexit_CM, index = "all")[, c("SL", "SH")]
quantile(k[, "SL"] + k[, "SH"], probs = c(0.025, 0.5, 0.975))
