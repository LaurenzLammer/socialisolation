library("lavaan")
library("tidyverse")
library("car")

### load and prepare dataset 
df <- read.csv("/data/pt_life/ResearchProjects/LLammer/Data/compiled_scaled_data.csv")
df <- reshape(df, idvar = "subject", timevar = "fu", direction = "wide")
df <- df %>%
  rename(
    TICS_bl = TICS_sum.0, 
    TICS_fu = TICS_sum.1,
    HCV_bl = HCV.0,
    HCV_fu = HCV.1, 
    exfunct_bl = exfunct.0, 
    exfunct_fu = exfunct.1,
    memo_bl = memo.0,
    memo_fu = memo.1,
    procspeed_bl = procspeed.0, 
    procspeed_fu = procspeed.1, 
    LSNS_base = LSNS_base.1,
    sex = sex.0, 
    age_base = age_base.0,
    age_change = age_change.1, 
    hypertension = hypertension.0,
    diabetes = diabetes.0,
    BMI = BMI.0,
    education = education.0, 
    CES.D = CES.D.0
  ) %>%
  mutate(LSNS_fu = LSNS_base.0 + LSNS_change.1)

fastsem <- function(med = "HCV", dv, model = 1){
  # a function to estimate indirect effects of social isolation via the mediator on the dependent variable
  # returns information on correlation of predictors of individual regressions, fit indices and estimations of effects
  # legal values for med are "HCV", and "TICS"
  # legal values for dv are "HCV", "exfunct", "memo" and "procspeed"
  if(model == 1){
    if(med == "HCV"){
      test1 <- lm(formula = paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + HCV_fu +  ", dv, "_bl + 
                                   age_base + age_change + sex"), data=df)
      test2 <- lm(formula = "HCV_fu ~ LSNS_base + LSNS_fu + HCV_bl + age_base + age_change + sex", data=df)
      formulae <- paste(paste0(dv, "_fu ~ LSNS_base + LSNS_fu + b*HCV_bl + HCV_fu +  ", dv, "_bl + age_base + age_change + sex"), 
                         "HCV_fu ~ a*LSNS_base + LSNS_fu + HCV_bl + age_base + age_change + sex", 
                         "indirect := a*b", sep = "\n")
    }
    else{
      test1 <- lm(formula = paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + TICS_bl + TICS_fu +  
                                   age_base + age_change + sex"), data=df)
      test2 <- lm(formula = "TICS_fu ~ LSNS_base + LSNS_fu + TICS_bl + age_base + age_change + sex", data=df)
      formulae <- paste(paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + b*TICS_bl + TICS_fu + age_base + age_change + sex"), 
                         "TICS_fu ~ a*LSNS_base + LSNS_fu + TICS_bl + age_base + age_change + sex", 
                         "indirect := a*b", sep = "\n")
    }
  }
  else{
    if(med == "HCV"){
      test1 <- lm(formula = paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + HCV_fu +  ", dv, "_bl + age_base + age_change + 
                                   sex + hypertension + diabetes + BMI + education + CES.D"), data=df)
      test2 <- lm(formula = "HCV_fu ~ LSNS_base + LSNS_fu + HCV_bl + age_base + age_change + sex + hypertension + diabetes + 
                  BMI + education + CES.D", data=df)
      formulae <- paste(paste0(dv, "_fu ~ LSNS_base + LSNS_fu + b*HCV_bl + HCV_fu +  ", dv, "_bl + age_base + age_change + sex + hypertension + diabetes + BMI + education + CES.D"), 
                         "HCV_fu ~ a*LSNS_base + LSNS_fu + HCV_bl + age_base + age_change + sex + hypertension + diabetes + BMI + education + CES.D", 
                         "indirect := a*b", sep = "\n")
    }
    else{
      test1 <- lm(formula = paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + TICS_bl + TICS_fu +  
                                   age_base + age_change + sex + hypertension + diabetes + BMI + education + CES.D"), data=df)
      test2 <- lm(formula = "TICS_fu ~ LSNS_base + LSNS_fu + TICS_bl + age_base + age_change + sex + CES.D", data=df)
      formulae <- paste(paste0(dv, "_fu ~ LSNS_base + LSNS_fu + HCV_bl + b*TICS_bl + TICS_fu + age_base + age_change + sex + hypertension + diabetes + BMI + education + CES.D"), 
                         "TICS_fu ~ a*LSNS_base + LSNS_fu + TICS_bl + age_base + age_change + sex + CES.D", 
                         "indirect := a*b", sep = "\n")
    }
  }
  vifres1 <- as.data.frame(vif(test1))
  vifres2 <- as.data.frame(vif(test2))
  if(max(vifres1) >= 10 | max(vifres2) >= 10){
    write_lines(paste0("VIF-threshold exceeded in mediation with ", dv, " as dependent variable 
                           and ", med, " as mediator in model", model), append = T, file = 
                  "/data/pt_life/ResearchProjects/LLammer/Results/mediation/VIF/threshold_exceeded.txt")
  }
  stest1 <- summary(test1, corr = T)
  stest2 <- summary(test2, corr = T)
  fit <- sem(formulae, df)
  sumfit <- summary(fit, fit.measures=T, rsq=T)
  sumfit$PE$sided_pvalue <- NA
  sumfit$PE[nrow(sumfit$PE)-2, "sided_pvalue"] <- ifelse(sumfit$PE[nrow(sumfit$PE)-2, "est"] < 0, sumfit$PE[nrow(sumfit$PE)-2, "pvalue"]/2, 
                                                        1 - sumfit$PE[nrow(sumfit$PE)-2, "pvalue"]/2)
  write.csv(sumfit$PE, paste0("/data/pt_life/ResearchProjects/LLammer/Results/mediation/estimates/", dv, "_on_", med, "model", model, ".csv"))
  write.csv(sumfit$FIT, paste0("/data/pt_life/ResearchProjects/LLammer/Results/mediation/fit_indices/", dv, "_on_", med, "model", model, ".csv"))
  res_list <- list(vifres1, vifres2, stest1, stest2, fit, sumfit)
  names(res_list) <- c("vifres1", "vifres2", "stest1", "stest2", "fit", "sumfit")
  return(res_list)
}

fit311 <- fastsem(med = "TICS", dv = "HCV")
fit312 <- fastsem(med = "TICS", dv = "HCV", model = 2)

fit411a <- fastsem(dv = "exfunct")
fit412a <- fastsem(dv = "exfunct", model = 2)
fit411b <- fastsem(dv = "memo")
fit412b <- fastsem(dv = "memo", model = 2)
fit411c <- fastsem(dv = "procspeed")
fit412c <- fastsem(dv = "procspeed", model = 2)

fits <- list(fit311, fit312, fit411a, fit412a, fit411b, fit412b, fit411c, fit412c)
indirect_overview <- as.data.frame(lapply(fits, function(x) tail(x$sumfit$PE$pvalue, n =3)[1]))
indirect_overview[2,] <- as.data.frame(lapply(fits, function(x) tail(x$sumfit$PE$sided_pvalue, n =3)[1]))
indirect_overview[3,] <- as.data.frame(lapply(fits, function(x) tail(x$sumfit$PE$est, n =3)[1]))
colnames(indirect_overview) <- c("311", "312", "411a", "412a", "411b", "412b", "411c", "412c")
rownames(indirect_overview) <- c("p_value", "sided_p_value", "estimate")
write.csv(indirect_overview, "/data/pt_life/ResearchProjects/LLammer/Results/mediation/pvals/pvals.csv")

save.image("/data/pt_life/ResearchProjects/LLammer/Results/mediation/Workspace/workspace.RData")
