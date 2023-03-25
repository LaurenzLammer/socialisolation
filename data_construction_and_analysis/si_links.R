# set working directory
setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results_si_links")

# load required packages
library("lme4") # 1.1-31
library("lmerTest") # 3.1-3
# ensure that lmerTest doesn't mask lmer, which would cause us multiple problems
lmer <- lme4::lmer
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/diagnostic_fcns.r")
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/glmm_stability.r")
library("BayesFactor") #0.9.12.4.4
library(car) #3.1-1
library(doMC) #1.3.8
library(tidyverse)

# load full dataset
data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_full_data.csv")

# harmonize follow-up with baseline data with differing column names
data$SOZIO_F0055 <- ifelse(data$fu == 1 & is.na(data$SOZIO_F0055), data$SOZIO_ERWERB, data$SOZIO_F0055)
data$SOZIO_F0056 <- ifelse(data$fu == 1 & is.na(data$SOZIO_F0056), data$SOZIO_NERWERB_GR, data$SOZIO_F0056)
data$SOZIO_F0079 <- ifelse(data$fu == 1 & is.na(data$SOZIO_F0079), data$SOZIO_HAUSH_GROE, data$SOZIO_F0079)

# create variables for living alone, number of persons sharing the dwelling, employment and marital status
# only count participants that are married and living with their spouses
data$married <- ifelse(data$marit_stat == 1, 1, 0)
# properly code answers as NA
data$married <- ifelse(data$marit_stat == 95, NA, data$married)
# only count participants as non-working if they are not gainfully employed 
# and this is not due to studies / military / alternative service
data$non_working <- ifelse(data$SOZIO_F0055 == 0 & !(data$SOZIO_F0056 == 1 | data$SOZIO_F0056 == 6), 1, 0)
# properly code answers as NA
data$non_working <- ifelse(data$SOZIO_F0055 == 98 | (data$SOZIO_F0055 == 0 & (data$SOZIO_F0056 == 95 |
                            data$SOZIO_F0056 == 97 | data$SOZIO_F0056 == 98)), NA, data$non_working)
# create a variable coding people living alone as 1 and those living with others as 0
data$live_alone <- ifelse(data$SOZIO_F0079 == 1, 1, 0)
# create a variable counting the umber of persons sharing the dwelling with the participant
data$n_dwell <- data$SOZIO_F0079


lme <- function(poi){
  # calculate the full LME
  # REML=F necessary for lmer_test employed later on
  if(poi == "age"){
    res <- lmer(LSNS_sum ~ age_base + age_change + (1|SIC), 
                data=data, REML=F, na.action = na.omit)
  } else {
    res <- lmer(formula = paste0("LSNS_sum ~ ", poi, " + (1|SIC)"), 
                data=data, REML=F, na.action = na.omit)
  }
  # get and save the model's coefficients, confidence intervals and sample size
  sres <- summary(res)
  coeff <- as.data.frame(sres$coefficients)
  CI <- as.data.frame(confint.merMod(res))
  coeff <- merge(coeff, CI, all = T, by="row.names")
  coeff$n_total <- nobs(res)
  coeff$n_individual <- ngrps(res)
  write.csv(coeff, file = paste0("coefficients/LSNS_on_", poi, ".csv"))
  # create a histogram and a qq-plot of residuals & plot residuals against fitted values
  # save plots
  # for further information please see: 
  # https://github.com/keyfm/eva/blob/master/trpm8/src/diagnostic_fcns.r
  png(filename = paste0("diag_plots/LSNS_on_", poi, ".png"))
  diagnostics.plot(res)
  dev.off()
  # determine stability of LME by excluding levels of random effects, one at a time
  # for further information please see: 
  # https://github.com/keyfm/eva/blob/master/trpm8/src/glmm_stability.r
  stab_results <- glmm.model.stab(res)
  write.csv(stab_results$summary, file = paste0("model_stab/LSNS_on_", poi, ".csv")) 
  # change the data type of the lmer results and obtain reliable p-value via full-null model comparison
  pval_res <- as_lmerModLmerTest(res)
  if(poi == "age"){
    p <- as.data.frame(drop1(pval_res, ddf = "Satterthwaite"))
  } else {
    p <- as.data.frame(drop1(pval_res, scope = poi, ddf = "Satterthwaite"))
  }
  # store relevant data for the model in a handy list for later use
  list <- list(res, stab_results, p)
  names(list) <- c("res", "stab_results", "p")
  return(list)
}

# LSNS link to marital status
marit_status <- lme("married")
# LSNS link to not working
non_working <- lme("non_working")
# LSNS link to living alone
live_alone <- lme("live_alone")
# LSNS link to number of presons living in the participant's dwelling
n_dwell <- lme("n_dwell")
# LSNS link to SES
SES <- lme("SES")
# LSNS link to age
age <- lme("age")
# LSNS link to gender
sex <- lme("sex")
# LSNS link to migration background
migrat <- lme("migrat")
# LSNS link to chronic stress
TICS <- lme("TICS_sum")

comprehensive <- list(marit_status, non_working, live_alone, n_dwell, SES, sex, migrat, TICS, age)
# check for warnings in stability tests
stab_warnings <- as.data.frame(lapply(comprehensive, function(x) print(unique(x$stab_results$detailed$warnings))))
write.csv(stab_warnings, file = "model_stab/warnings_check.csv")
# write p-value tables, age is dealt with separately because it has 2 rows 
pvals <- data.frame(matrix(unlist(lapply(comprehensive[-9], function(x) print(x$p))), nrow = length(comprehensive) - 1, 
                           byrow = TRUE))
colnames(pvals) <- c("Sum Sq", "Mean Sq", "NumDF", "DenDF",  "F value", "Pr(>F)")
rownames(pvals) <- c("marit_status", "non_working", "live_alone", "n_dwell", "SES", "gender", "migration background", "TICS")
pvals["age_base",] <- age$p[1,]
pvals["age_change",] <- age$p[2,]
write.csv(pvals, file = "pvals/overview.csv")

write.csv(data, file = "data_si_links.csv")

save.image(file = "workspace/workspace.RData")
