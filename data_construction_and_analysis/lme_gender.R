library("lme4") # 1.1-26
library("lmerTest") # 3.1-3
# ensure that lmerTest doesn't mask lmer, which would cause us multiple problems
lmer <- lme4::lmer
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/diagnostic_fcns.r")
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/glmm_stability.r")
library("BayesFactor") #0.9.1.4.2
library(car) #3.0-10
library(BiocManager) # 1.30.12
library(qvalue) # 2.18.0
library(doMC) #1.3.7

### import longitudinal dataset, 
data <- read.csv("/data/pt_life/ResearchProjects/LLammer/Data/compiled_scaled_data.csv")
# change this variable to "male" if appropriate
variable <- "female"
### set path variable & modify dataset
if(variable == "female"){
  path = "/data/pt_life/ResearchProjects/LLammer/Results_female/"
  data <- subset(data, data$sex == 0)
  } else{
  path = "/data/pt_life/ResearchProjects/LLammer/Results_male/"
  data <- subset(data, data$sex == 1)
  }

#make subject a factor to please BayeysFactor
data$subject <- as.factor(data$subject)

# we will start with hypothesis 1.5: Participants that are socially more isolated at baseline 
# will experience aggravated age-related changes in hippocampal volume over the follow-up period.
# hypothesis 1.5, model 1

# first we have to subset our data to exclude hippocampal volume outliers
datax <- subset(data, data$outlier_HCV != 1)

# first we have to subset our data to exclude hippocampal volume outliers
# calculate the full LME
# REML=F necessary for lmer_test employed later on
res <- lmer(HCV~LSNS_base+LSNS_change+age_base+age_change+LSNS_base:age_change+(1|subject), 
            data=datax, REML=F, na.action = na.omit)
# get and save the model's coefficients, confidence intervals and sample size
sres <- summary(res)
coeff <- as.data.frame(sres$coefficients)
CI <- as.data.frame(confint.merMod(res))
coeff <- merge(coeff, CI, all = T, by="row.names")
coeff$n_total <- nobs(res)
coeff$n_individual <- ngrps(res)
write.csv(coeff, file = paste0(path, "/coefficients/HCV_on_LSNS_base:age_change_model1.csv"))
# linear (non-mixed) model to calculate VIFs 
test <- lm(HCV~LSNS_base+LSNS_change+age_base+age_change, data=datax)
# calculate and save VIFs
vifres <- as.data.frame(vif(test))
write.csv(vifres, file = paste0(path, "/VIFs/HCV_on_LSNS_base:age_change_model1.csv"))
# calculating the VIFs for all models with identical predictors would be sufficient in the absence of NAs in our dvs
# unfortunately, we cannot expect that
# hence, we will calculate the VIFs for every single model

# to accelerate computation time we will not calculate bayesian statistics for this sensitivity analysis

# create a histogram and a qq-plot of residuals & plot residuals against fitted values
# save plots
# for further information please see: 
# https://github.com/keyfm/eva/blob/master/trpm8/src/diagnostic_fcns.r
png(filename = paste0(path, "/diag_plots/HCV_on_LSNS_base:age_change_model1.png"))
diagnostics.plot(res)
dev.off()

# determine stability of LME by excluding levels of random effects, one at a time
# for further information please see: 
# https://github.com/keyfm/eva/blob/master/trpm8/src/glmm_stability.r
stab_results <- glmm.model.stab(res)
write.csv(stab_results$summary, file = paste0(path, "/model_stab/HCV_on_LSNS_base:age_change_model1.csv")) 

# change the data type of the lmer results and obtain reliable p-value via full-null model comparison
pval_res <- as_lmerModLmerTest(res)
p <- as.data.frame(drop1(pval_res, scope = "LSNS_base:age_change", ddf = "Satterthwaite"))
# prepare columns to be filled later(if FDR-correction is applied - in this case it is going to be applied)
p$qval <- 0
p$significance <- "not applicable"
p$sided_p <- ifelse(coeff[coeff$Row.names == "LSNS_base:age_change",]$Estimate < 0, p$`Pr(>F)`/2, 1-p$`Pr(>F)`/2)

# store relevant data for the model in a handy list for later use
list151 <- list(res, stab_results, vifres, p)
names(list151) <- c("res", "stab_results", "vifres", "p")

# The models to test our hypotheses have a very similar structure.
# Henceforth, we can reduce the necessary lines of code by employing the function "fastlme". 
fastlme <- function(poi = "LSNS_base", dv, model = 1){
  # saves code as most models have a very similar structure
  # tests model assumptions and stability
  # performs full-null model comparison
  # see above model for step-by-step explanation
  # poi = predictor of interest = LSNS_base / LSNS_change / LSNS_base:age_change / LSNS_base:LSNS_change.
  # dv = dependent variable of the model = Hippocampal volume or a cognitive function
  # model = model 1 (limited control variables, is default) or model 2 (all control variables)
  # further subsetting in case of model == 2
  if(dv == "HCV"){
    datax = subset(data, data$outlier_HCV != 1)
  }
  else if(dv == "memo"){
    datax = subset(data, data$outlier_memo != 1)
  }
  else if(dv == "exfunct"){
    datax = subset(data, data$outlier_exfunct != 1)
  }
  else{
    datax = subset(data, data$outlier_procspeed != 1)
  }
  if(model == 1){
    if(poi == "LSNS_base"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change"), data=datax)
      vifres <- as.data.frame(vif(test))
      }
    else if(poi == "LSNS_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change"), data=datax)
      vifres <- as.data.frame(vif(test))
      }
    else if(poi == "LSNS_base:age_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+LSNS_base:age_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change"), data=datax)
      vifres <- as.data.frame(vif(test))
    }
    else{
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+LSNS_base:LSNS_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change"), data=datax)
      vifres <- as.data.frame(vif(test))
    }
  } 
  
  else{
    datay <- subset(datax, !is.na(datax$CES.D))
    if(poi == "LSNS_base"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D+(1|subject)"),
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
      }
    else if(poi == "LSNS_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D+(1|subject)"), 
             data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
      }
    else if(poi == "LSNS_base:age_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D+LSNS_base:age_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
    }
    else{
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D+LSNS_base:LSNS_change+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
    }
  }
  write.csv(vifres,
            file = paste0(path, "/VIFs/", dv, "_on_", poi, "_model", model, ".csv"))
  sres <- summary(res)
  coeff <- as.data.frame(sres$coefficients)
  CI <- as.data.frame(confint.merMod(res))
  coeff <- merge(coeff, CI, all = T, by="row.names")
  coeff$n_total <- nobs(res)
  coeff$n_individual <- ngrps(res)
  write.csv(coeff, file = paste0(path, "/coefficients/", dv, "_on_", poi, "_model", model, ".csv"))
  png(filename = paste0(path, "/diag_plots/", dv, "_on_", poi, "_model", model, ".png"))
  diagnostics.plot(res)
  dev.off()
  stab_results <- glmm.model.stab(res)
  write.csv(stab_results$summary, file = paste0(path, "/model_stab/", dv, "_on_", poi, "_model", model, ".csv"))
  pval_res <- as_lmerModLmerTest(res)
  p <- as.data.frame(drop1(pval_res, scope = poi, ddf = "Satterthwaite"))
  p$qval <- 0
  p$significance <- "not applicable"
  p$sided_p <- ifelse(coeff[coeff$Row.names == poi,]$Estimate < 0, p$`Pr(>F)`/2, 1-p$`Pr(>F)`/2)
  list <- list(res, stab_results, vifres, p)
  names(list) <- c("res", "stab_results", "vifres", "p")
  return(list)
}

#hypothesis 1.1: Social isolation is negatively associated with hippocampal volume across individuals.
list111 <- fastlme(dv = "HCV")
list112 <- fastlme(dv = "HCV", model = 2)

#hypothesis 1.3: Social isolation is negatively associated with hippocampal volume within individuals.
list131 <- fastlme(poi = "LSNS_change" ,dv = "HCV")
list132 <- fastlme(poi = "LSNS_change", dv = "HCV", model = 2)

# hypothesis 2.1: Social isolation is negatively associated with cognitive functions across individuals.
# a: executive function
list211a <- fastlme(dv = "exfunct")
list212a <- fastlme(dv = "exfunct", model = 2)
# b: memory performance
list211b <- fastlme(dv = "memo")
list212b <- fastlme(dv = "memo", model = 2)
# c: processing speed
list211c <- fastlme(dv = "procspeed")
list212c <- fastlme(dv = "procspeed", model = 2)

# hypothesis 2.2: Social isolation is negatively associated with cognitive functions within individuals.
# a: executive function
list221a <- fastlme(poi = "LSNS_change" , dv = "exfunct")
list222a <- fastlme(poi = "LSNS_change" , dv = "exfunct", model = 2)
# b: memory performance
list221b <- fastlme(poi = "LSNS_change" , dv = "memo")
list222b <- fastlme(poi = "LSNS_change" , dv = "memo", model = 2)
# c: processing speed
list221c <- fastlme(poi = "LSNS_change" , dv = "procspeed")
list222c <- fastlme(poi = "LSNS_change" , dv = "procspeed", model = 2)

# hypothesis 1.5: Participants that are socially more isolated at baseline 
# will experience aggravated age-related changes in hippocampal volume over the follow-up period.
# 151 was the first model calculated
list152 <- fastlme(poi = "LSNS_base:age_change", dv = "HCV", model = 2)

# hypothesis 2.3: Participants that are socially more isolated at baseline 
# will experience aggravated age-related changes in cognitive function over the follow-up period. 
# a: executive function
list231a <- fastlme(poi = "LSNS_base:age_change" , dv = "exfunct")
list232a <- fastlme(poi = "LSNS_base:age_change" , dv = "exfunct", model = 2)
# b: memory performance
list231b <- fastlme(poi = "LSNS_base:age_change" , dv = "memo")
list232b <- fastlme(poi = "LSNS_base:age_change" , dv = "memo", model = 2)
# c: processing speed
list231c <- fastlme(poi = "LSNS_base:age_change" , dv = "procspeed")
list232c <- fastlme(poi = "LSNS_base:age_change" , dv = "procspeed", model = 2)

# hypothesis 5.1: In people who are socially more isolated at baseline, an increase in social isolation 
# from baseline to follow-up will have a stronger negative association with HC volume 
# than in people who are less socially isolated at baseline.
list511 <- fastlme(poi = "LSNS_base:LSNS_change", dv = "HCV")
list512 <- fastlme(poi = "LSNS_base:LSNS_change", dv = "HCV", model = 2)

# calculate q-values and test FDR-corrected significance at alpha = 0.05
qval1 <- qvalue(c(list111$p$sided_p, list131$p$sided_p, list151$p$sided_p, list211a$p$sided_p, list211b$p$sided_p, 
                  list211c$p$sided_p, list221a$p$sided_p, list221b$p$sided_p, list221c$p$sided_p, list231a$p$sided_p,
                  list231b$p$sided_p, list231c$p$sided_p), fdr.level = 0.05, pi0 = 1)

qval2 <- qvalue(c(list112$p$sided_p, list132$p$sided_p, list152$p$sided_p, list212a$p$sided_p, list212b$p$sided_p, 
                  list212c$p$sided_p, list222a$p$sided_p, list222b$p$sided_p, list222c$p$sided_p, list232a$p$sided_p,
                  list232b$p$sided_p, list232c$p$sided_p), fdr.level = 0.05, pi0 = 1)

# add information from FDR-correction to lists
family1 <- list(list111, list131, list151, list211a, list211b, list211c, list221a, list221b, list221c, list231a, 
                list231b, list231c)
family2 <- list(list112, list132, list152, list212a, list212b, list212c, list222a, list222b, list222c, list232a, 
                list232b, list232c)
names(family1) <- c("111", "131", "151", "211a", "211b", "211c", "221a", "221b", "221c", "231a", "231b", "231c")
names(family2) <- c("112", "132", "152", "212a", "212b", "212c", "222a", "222b", "222c", "232a", "232b", "232c")

counter <- 0
for(n in names(family1)){
  counter <- counter +1
  family1[[n]]$p$qval <- qval1$qvalues[counter]
  family1[[n]]$p$significance <- qval1$significant[counter]
}

counter <- 0
for(n in names(family2)){
  counter <- counter +1
  family2[[n]]$p$qval <- qval2$qvalues[counter]
  family2[[n]]$p$significance <- qval2$significant[counter]
}

# create a comprehensive list of lists to use lapply

comprehensive <- c(family1, family2, list(list511), list(list512))
hypotheses <- c("111", "131", "151", "211a", "211b", "211c", "221a", "221b", "221c", "231a", "231b", 
                "231c", "112", "132", "152", "212a", "212b", "212c", "222a", "222b", "222c", "232a", 
                "232b", "232c", "511", "512")
names(comprehensive) <- hypotheses

# test if our VIF threshold was exceeded in any model and save results
vif_tresh_check <- as.data.frame(lapply(comprehensive, function(x) ifelse(max(x$vifres) > 10, print("VIF threshold exceeded"), print("ok"))))
write.csv(vif_tresh_check, file = paste0(path, "/VIFs/threshold_check.csv"))

# check for warnings in stability tests
stab_warnings <- as.data.frame(lapply(comprehensive, function(x) print(unique(x$stab_results$detailed$warnings))))
write.csv(stab_warnings, file = paste0(path, "/model_stab/warnings_check.csv"))

# write p-value tables
pvals <- data.frame(matrix(unlist(lapply(comprehensive, function(x) print(x$p))), nrow = length(comprehensive), 
                           byrow = TRUE))
colnames(pvals) <- c("Sum Sq", "Mean Sq", "NumDF", "DenDF",  "F value", "Pr(>F)", "qval", "significance", "sided p-value")
rownames(pvals) <- hypotheses
write.csv(pvals, file = paste0(path, "/pvals/overview.csv"))


save.image(file = paste0(path, "/Workspace/workspace2.RData"))
