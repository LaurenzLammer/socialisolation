library("lme4") # 1.1-31
library("lmerTest") # 3.1-3
# ensure that lmerTest doesn't mask lmer, which would cause us multiple problems
lmer <- lme4::lmer
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/diagnostic_fcns.r")
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/glmm_stability.r")
library("BayesFactor") #0.9.12.4.4
library(car) #3.1-1
library(BiocManager) # 1.30.19
library(qvalue) # 2.30.0
library(doMC) #1.3.8

data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
path = "/data/pt_life/ResearchProjects/LLammer/si_update/Results_cut_off_interact"
data$LSNS_cut <- ifelse(data$LSNS_sum > 18, 1, 0)

#make subject a factor to please BayeysFactor
data$subject <- as.factor(data$subject)

# we will start with hypothesis 1.5: Participants that are socially more isolated at baseline 
# will experience aggravated age-related changes in hippocampal volume over the follow-up period.
# hypothesis 1.5, model 1

# The models to test our hypotheses have a very similar structure.
# Henceforth, we can reduce the necessary lines of code by employing the function "fastlme". 
fastlme <- function(poi = "LSNS_base", dv, model = 1){
  # saves code as most models have a very similar structure
  # tests model assumptions and stability
  # performs full-null model comparison
  # calculates BF
  # see above model for step-by-step explanation
  # poi = predictor of interest = LSNS_base / LSNS_change / LSNS_base:age_change / LSNS_base:LSNS_change.
  # dv = dependent variable of the model = Hippocampal volume or a cognitive function
  # model = model 1 (limited control variables, is default) or model 2 (all control variables)
  # further subsetting in case of model == 2, because GeneralTestBF can't handle NAs
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
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_base:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+LSNS_cut+age_base+age_change+sex"), data=datax)
      vifres <- as.data.frame(vif(test))
      bf <- generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_base:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+subject")), 
                          data=datax, whichRandom = "subject",  multicore = T,
                          neverExclude = c("LSNS_change", "age_base", "age_change", "sex", "subject", "^LSNS_base$", "^LSNS_cut$"))
      }
    else if(poi == "LSNS_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change+LSNS_change:LSNS_cut+LSNS_cut+age_base+age_change+sex+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+LSNS_cut+age_base+age_change+sex"), data=datax)
      vifres <- as.data.frame(vif(test))
      bf <- generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+LSNS_change:LSNS_cut+LSNS_cut+age_base+age_change+sex+subject")), 
                          data=datax, whichRandom = "subject",  multicore = T,
                          neverExclude = c("LSNS_base", "age_base", "age_change", "sex", "subject", "^LSNS_change$", "^LSNS_cut$"))
      }
  } 
  
  else{
    datay <- subset(datax, !is.na(datax$CES.D))
    if(poi == "LSNS_base"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_base:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+(1|subject)"),
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+LSNS_cut+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
      bf <- generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_base:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+subject")), 
            data=datay, whichRandom = "subject", multicore = T, neverExclude = 
            c("LSNS_change", "age_base", "age_change", "sex", "education", "BMI", "hypertension", "diabetes", "CES.D", "subject", "^LSNS_base$", "^LSNS_cut$"))
      }
    else if(poi == "LSNS_change"){
      res <- lmer(formula = paste0(dv, "~LSNS_base+LSNS_change:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+(1|subject)"), 
             data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~LSNS_base+LSNS_change+LSNS_cut+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D"), 
                 data=datax, na.action = na.omit)
      vifres <- as.data.frame(vif(test))
      bf <- generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change:LSNS_cut+LSNS_cut+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+subject")), 
            data=datay, whichRandom = "subject", multicore = T, neverExclude = 
            c("LSNS_base", "age_base", "age_change", "sex", "education", "BMI", "hypertension", "diabetes", "CES.D", "subject", "^LSNS_change$", "^LSNS_cut$"))
    }
  }
  bf_nolog <- extractBF(bf, logbf = F) 
  bf_nolog["full_vs_null", 1] <- bf_nolog[1,1] / bf_nolog[2,1]
  chains <- posterior(bf, 1, iterations = 10000)
  if(poi == "LSNS_base"){
    siding_factor <- mean(chains[,"LSNS_base.&.LSNS_cut"]<0)
  } else if(poi == "LSNS_change"){
    siding_factor <- mean(chains[,"LSNS_change.&.LSNS_cut"]<0)
  } 
  bf_nolog["full_vs_null_sided", 1] <- bf_nolog["full_vs_null", 1] * 2 * siding_factor
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
  if(poi == "LSNS_base"){
    p <- as.data.frame(drop1(pval_res, scope = "LSNS_base:LSNS_cut", ddf = "Satterthwaite"))
    p$sided_p <- ifelse(coeff[coeff$Row.names == "LSNS_base:LSNS_cut",]$Estimate < 0, p$`Pr(>F)`/2, 1-p$`Pr(>F)`/2)
  } else if(poi == "LSNS_change"){
    p <- as.data.frame(drop1(pval_res, scope = "LSNS_change:LSNS_cut", ddf = "Satterthwaite"))
    p$sided_p <- ifelse(coeff[coeff$Row.names == "LSNS_change:LSNS_cut",]$Estimate < 0, p$`Pr(>F)`/2, 1-p$`Pr(>F)`/2)
  } 
  p$qval <- 0
  p$significance <- "not applicable"
  list <- list(res, stab_results, vifres, bf, bf_nolog, p)
  names(list) <- c("res", "stab_results", "vifres", "bf", "bf_nolog", "p")
  save.image(file = paste0(path, "/Workspace/workspace.RData"))
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


# calculate q-values and test FDR-corrected significance at alpha = 0.05
qval1 <- qvalue(c(list111$p$sided_p, list131$p$sided_p, list211a$p$sided_p, list211b$p$sided_p, 
                  list211c$p$sided_p, list221a$p$sided_p, list221b$p$sided_p, list221c$p$sided_p), fdr.level = 0.05, pi0 = 1)

qval2 <- qvalue(c(list112$p$sided_p, list132$p$sided_p, list212a$p$sided_p, list212b$p$sided_p, 
                  list212c$p$sided_p, list222a$p$sided_p, list222b$p$sided_p, list222c$p$sided_p), fdr.level = 0.05, pi0 = 1)

# add information from FDR-correction to lists
family1 <- list(list111, list131, list211a, list211b, list211c, list221a, list221b, list221c)
family2 <- list(list112, list132, list212a, list212b, list212c, list222a, list222b, list222c)
names(family1) <- c("111", "131", "211a", "211b", "211c", "221a", "221b", "221c")
names(family2) <- c("112", "132", "212a", "212b", "212c", "222a", "222b", "222c")

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

comprehensive <- c(family1, family2)
hypotheses <- c("111", "131", "211a", "211b", "211c", "221a", "221b", "221c", "112", "132", "212a", "212b", "212c", "222a", "222b", "222c")
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

# write a dataframe containing all Bayes-Factor information

BF <- data.frame(matrix(nrow = length(comprehensive)*4, ncol = 4))
BF_names <- c()
counter <- 1
for(n in names(comprehensive)){
  BF[((counter):(counter+3)),] <- comprehensive[[n]]$bf_nolog
  BF_names <- c(BF_names, rownames(comprehensive[[n]]$bf_nolog))
  counter <- counter + 4
}

colnames(BF) <- c("bf", "error", "time", "code")
BF$model <- BF_names
for(n in (1:length(BF$model))){
  BF$hypothesis[n] <- hypotheses[ceiling(n/4)]
}

write.csv(BF, file = paste0(path, "/bayes_factor/bayes_factors.csv"))

save.image(file = paste0(path, "/Workspace/workspace.RData"))
