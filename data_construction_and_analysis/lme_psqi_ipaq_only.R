library("lme4") # 1.1-31
library("lmerTest") # 3.1-3
# ensure that lmerTest doesn't mask lmer, which would cause us multiple problems
lmer <- lme4::lmer
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/diagnostic_fcns.r")
source("/data/gh_gr_agingandobesity_share/literature/methods/statistics/linear_models_course_rogermundry_2018/functions/glmm_stability.r")
library(car) #3.1-1
library(BiocManager) # 1.30.19
library(qvalue) # 2.30.0

data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
path = "/data/pt_life/ResearchProjects/LLammer/si_update/Results_psqi_ipaq_only"
# The models to test our hypotheses have a very similar structure.
# Henceforth, we can reduce the necessary lines of code by employing the function "fastlme". 
fastlme <- function(poi, dv){
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
    if(poi == "IPAQ_total_met"){
      res <- lmer(formula = paste0(dv, "~IPAQ_total_met+age_base+age_change+sex+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~IPAQ_total_met+age_base+age_change+sex"), data=datax)
      vifres <- as.data.frame(vif(test))
      }
    else{
      res <- lmer(formula = paste0(dv, "~psqi_glob+age_base+age_change+sex+(1|subject)"), 
                  data=datax, REML=F, na.action = na.omit)
      test <- lm(formula = paste0(dv, "~psqi_glob+age_base+age_change+sex"), data=datax)
      vifres <- as.data.frame(vif(test))
      }
  write.csv(vifres,
            file = paste0(path, "/VIFs/", dv, "_on_", poi, ".csv"))
  sres <- summary(res)
  coeff <- as.data.frame(sres$coefficients)
  CI <- as.data.frame(confint.merMod(res))
  coeff <- merge(coeff, CI, all = T, by="row.names")
  coeff$n_total <- nobs(res)
  coeff$n_individual <- ngrps(res)
  write.csv(coeff, file = paste0(path, "/coefficients/", dv, "_on_", poi, ".csv"))
  png(filename = paste0(path, "/diag_plots/", dv, "_on_", poi, ".png"))
  diagnostics.plot(res)
  dev.off()
  stab_results <- glmm.model.stab(res)
  write.csv(stab_results$summary, file = paste0(path, "/model_stab/", dv, "_on_", poi, ".csv"))
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
list11 <- fastlme(dv = "HCV", poi = "psqi_glob")
list12 <- fastlme(dv = "HCV", poi = "IPAQ_total_met")
list21 <- fastlme(dv = "exfunct", poi = "psqi_glob")
list22 <- fastlme(dv = "exfunct", poi = "IPAQ_total_met")
list31 <- fastlme(dv = "memo", poi = "psqi_glob")
list32 <- fastlme(dv = "memo", poi = "IPAQ_total_met")
list41 <- fastlme(dv = "procspeed", poi = "psqi_glob")
list42 <- fastlme(dv = "procspeed", poi = "IPAQ_total_met")

# create a comprehensive list of lists to use lapply

comprehensive <- list(list11, list12, list21, list22, list31, list32, list41, list42)
hypotheses <- c("11", "12", "21", "22", "31", "32", "41", "42")
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

save.image(file = paste0(path, "/Workspace/workspace.RData"))
