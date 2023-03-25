library("lavaan")
library("tidyverse")
library("car")

### load and prepare dataset 
df <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
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

df$HCV_bl <- scale(df$HCV_bl)
df$HCV_fu <- scale(df$HCV_fu)
fastfimlsem <- function(med = "HCV", dv, model = 1){
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
  } else{
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
               "/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/VIF/threshold_exceeded_fiml.txt")
  }
  stest1 <- summary(test1, corr = T)
  stest2 <- summary(test2, corr = T)
  fit <- sem(formulae, df, estimator='ml', missing = "fiml", fixed.x = F)
  sumfit <- summary(fit, fit.measures=T, rsq=T)
  sumfit$pe$sided_pvalue <- NA
  sumfit$pe[nrow(sumfit$pe)-2, "sided_pvalue"] <- ifelse(sumfit$pe[nrow(sumfit$pe)-2, "est"] < 0, sumfit$pe[nrow(sumfit$pe)-2, "pvalue"]/2, 
                                                         1 - sumfit$pe[nrow(sumfit$pe)-2, "pvalue"]/2)
  write.csv(sumfit$pe, paste0("/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/estimates/", dv, "_on_", med, "model", model, "_fiml.csv"))
  write.csv(sumfit$fit, paste0("/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/fit_indices/", dv, "_on_", med, "model", model, "_fiml.csv"))
  res_list <- list(vifres1, vifres2, stest1, stest2, fit, sumfit)
  names(res_list) <- c("vifres1", "vifres2", "stest1", "stest2", "fit", "sumfit")
  return(res_list)
}

fimlfit311 <- fastfimlsem(med = "TICS", dv = "HCV")
fimlfit312 <- fastfimlsem(med = "TICS", dv = "HCV", model = 2)

fimlfit411a <- fastfimlsem(dv = "exfunct")
fimlfit412a <- fastfimlsem(dv = "exfunct", model = 2)
fimlfit411b <- fastfimlsem(dv = "memo")
fimlfit412b <- fastfimlsem(dv = "memo", model = 2)
fimlfit411c <- fastfimlsem(dv = "procspeed")
fimlfit412c <- fastfimlsem(dv = "procspeed", model = 2)

fimlfits <- list(fimlfit311, fimlfit312, fimlfit411a, fimlfit412a, fimlfit411b, fimlfit412b, fimlfit411c, fimlfit412c)
fimlindirect_overview <- as.data.frame(lapply(fimlfits, function(x) tail(x$sumfit$pe$pvalue, n =3)[1]))
fimlindirect_overview[2,] <- as.data.frame(lapply(fimlfits, function(x) tail(x$sumfit$pe$sided_pvalue, n =3)[1]))
fimlindirect_overview[3,] <- as.data.frame(lapply(fimlfits, function(x) tail(x$sumfit$pe$est, n =3)[1]))
colnames(fimlindirect_overview) <- c("311", "312", "411a", "412a", "411b", "412b", "411c", "412c")
rownames(fimlindirect_overview) <- c("p_value", "sided_p_value", "estimate")

write.csv(fimlindirect_overview, "/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/pvals/pvals_fiml.csv")

save.image("/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/Workspace/workspace_fiml.RData")
