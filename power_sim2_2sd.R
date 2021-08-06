library(MASS)
library(tidyverse)
library(BayesFactor)
library(lme4)

dir_path <- "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/run"
number <- 1
while (dir.exists(paste0(dir_path,number))) {
  number <- number + 1 
}
dir.create(paste0(dir_path,number))
setwd(paste0(dir_path,number))
set.seed(number)

simncalc <- function(dv, model, proportion){
  # simulates data with similar properties as original data
  # calculates BFs for models with LSNS_base & LSNS_change as poi
  # proportion defines effect size in terms of 1 point on the LSNS relative to 1 year in baseline age
  load("/data/pt_life/ResearchProjects/LLammer/Results/Workspace/workspace2.RData")
  
  covmatrix <- cov(data[,c("age_base", "LSNS_base", "sex", "hypertension", "diabetes", "BMI", "education", "CES.D")], use = "complete.obs") # determine (co)variance of relevant variables
  datasim <- as.data.frame(mvrnorm(n=length(unique(data$subject)), mu=c(mean(data$age_base), mean(data$LSNS_base), 
                                                                        mean(data$sex), mean(data$hypertension), mean(data$diabetes), mean(data$BMI),
                                                                        mean(data$education), mean(data$CES.D, na.rm = T)), Sigma=covmatrix, empirical=TRUE)) # simulate data
  datasim$sex <- ifelse(datasim$sex > 0.5, 1, 0) # make variables categorical
  datasim$hypertension <- ifelse(datasim$hypertension > 0.5, 1, 0) # make variables categorical
  datasim$diabetes <- ifelse(datasim$diabetes > 0.5, 1, 0) # make variables categorical
  datasim$education <- ifelse(datasim$education > 0.5, 1, 0) # make variables categorical
  
  data_both <- data %>%
    group_by(subject) %>%
    filter(n() == 2)
  data_fu <- subset(data_both, data_both$fu == 1)
  res_lc <- lm(LSNS_change ~ LSNS_base + age_base + sex + hypertension + diabetes + BMI + education + CES.D, data = data_fu)
  res_ac <- lm(age_change ~ LSNS_base + age_base + sex + hypertension + diabetes + BMI + education + CES.D, data = data_fu)
  
  sub <- tibble(
    id = 1:length(unique(data$subject)),
    sub_intercept_hcv1  = rnorm(length(unique(data$subject)), 0, sd(ranef(list111$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_hcv2  = rnorm(length(unique(data$subject)), 0, sd(ranef(list112$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_ef1  = rnorm(length(unique(data$subject)), 0, sd(ranef(list211a$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_ef2  = rnorm(length(unique(data$subject)), 0, sd(ranef(list212a$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_memo1  = rnorm(length(unique(data$subject)), 0, sd(ranef(list211b$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_memo2  = rnorm(length(unique(data$subject)), 0, sd(ranef(list211b$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_ps1  = rnorm(length(unique(data$subject)), 0, sd(ranef(list211c$res)$subject$`(Intercept)`)), # random intercept
    sub_intercept_ps2  = rnorm(length(unique(data$subject)), 0, sd(ranef(list211c$res)$subject$`(Intercept)`)), # random intercept
    age_base = datasim[,1],
    LSNS_base = datasim[,2],
    sex = datasim[,3],
    hypertension = datasim[,4],
    diabetes = datasim[,5],
    BMI = datasim[,6],
    education = datasim[,7],
    CESD = datasim[,8],
  )
 
  sub <- sub %>%
    plyr::mutate(
      err_lc = rnorm(nrow(.), 0, sd(residuals(res_lc))),
      err_ac = rnorm(nrow(.), 0, sd(residuals(res_ac))),
      LSNS_change = res_lc$coefficients[1] + res_lc$coefficients[2] * LSNS_base + res_lc$coefficients[3] * age_base + 
        res_lc$coefficients[4] * sex + res_lc$coefficients[5] * hypertension + res_lc$coefficients[6] * diabetes + 
        res_lc$coefficients[7] * BMI + res_lc$coefficients[8] * education + res_lc$coefficients[9] * CESD + err_lc,
      age_change = res_ac$coefficients[1] + res_ac$coefficients[2] * LSNS_base + res_ac$coefficients[3] * age_base + 
        res_ac$coefficients[4] * sex + res_ac$coefficients[5] * hypertension + res_ac$coefficients[6] * diabetes + 
        res_ac$coefficients[7] * BMI + res_ac$coefficients[8] * education + res_ac$coefficients[4] * CESD + err_ac
    )
  
  tp_n  <- 2
  tp <- tibble(
    tp = c("bl","fu")
  )
  
  df <- crossing(
    id = sub$id, # get subject IDs from the sub data table
    tp = tp$tp, # get stimulus IDs from the stim data table
  ) %>%
    left_join(sub, by = "id") %>% # includes the intercept and conditin for each subject
    left_join(tp, by = "tp")
  
  df <- df %>%
    group_by(id) %>%
    mutate(
      age_change = case_when(
        tp == "bl" ~ 0,
        TRUE ~ age_change
      ),
      LSNS_change = case_when(
        tp == "bl" ~ 0,
        TRUE ~ LSNS_change
      )
    )
  
  
  # reduce dataframe to resemble actual number of baseline and follow-up obs
  if(dv == "HCV"){
    datax <- subset(data, data$outlier_HCV != 1)
  } else if(dv == "exfunct"){
    datax <- subset(data, data$outlier_exfunct != 1)
  } else if(dv == "memo"){
    datax <- subset(data, data$outlier_memo != 1)
  } else{
    datax <- subset(data, data$outlier_procspeed != 1)
  }
  if(model == 2){
    datax <- subset(datax, !is.na(datax$CES.D))
  }
  datax_both <- datax %>%
    group_by(subject) %>%
    filter(n() == 2)
  
  nfu <- length(which(datax_both$fu == 1))
  nbl <- nrow(datax) - nfu
  df <- df[-sample(seq(from = 1, to = nrow(df)-1, by = 2), nrow(df)/2 - nbl),]
  df <- df %>%
    group_by(id) %>%
    filter(n() == 2)
  df_bl <- df[df$tp == "bl",]
  df_fu <- df[df$tp == "fu",]
  df_fu <- df_fu[sample(seq(from = 1, to = nrow(df_fu), by = 1), nfu),]
  df <- rbind(df_bl, df_fu)
  
  # calculate dependent variable with random effects
  
  if(dv == "HCV"){
    if(model == 1){
      df <- df %>%
        plyr::mutate(
          error = rnorm(nrow(.), 0, 2*sd(residuals(list111$res))),
          dv = fixef(list111$res)[1] + sub_intercept_hcv1 + error + fixef(list111$res)[4] * proportion * LSNS_base + 
            fixef(list111$res)[4] * proportion * LSNS_change + fixef(list111$res)[4] *
            age_base + fixef(list111$res)[5] * age_change + fixef(list111$res)[6] * sex
        )
    } else {
      plyr::mutate(
        error = rnorm(nrow(.), 0, 2*sd(residuals(list112$res))),
        dv = fixef(list112$res)[1] + sub_intercept_hcv2 + error + fixef(list112$res)[4] * proportion * LSNS_base + 
          fixef(list112$res)[4] * proportion * LSNS_change + fixef(list112$res)[4] *
          age_base + fixef(list112$res)[5] * age_change + fixef(list112$res)[6] * sex + fixef(list112$res)[7] * education + 
          fixef(list112$res)[8] * BMI + fixef(list112$res)[9] * hypertension + fixef(list112$res)[10] * diabetes + 
          fixef(list112$res)[11] * CESD
      )
    }
  } else if(dv == "exfunct"){
    if(model == 1){
      df <- df %>%
        plyr::mutate(
          error = rnorm(nrow(.), 0, 2*sd(residuals(list211a$res))),
          dv = fixef(list211a$res)[1] + sub_intercept_ef1 + error + fixef(list211a$res)[4] * proportion * LSNS_base + 
            fixef(list211a$res)[4] * proportion * LSNS_change + fixef(list211a$res)[4] *
            age_base + fixef(list211a$res)[5] * age_change + fixef(list211a$res)[6] * sex
        )
    } else {
      plyr::mutate(
        error = rnorm(nrow(.), 0, 2*sd(residuals(list212a$res))),
        dv = fixef(list212a$res)[1] + sub_intercept_ef2 + error + fixef(list212a$res)[4] * proportion * LSNS_base + 
          fixef(list212a$res)[4] * proportion * LSNS_change + fixef(list212a$res)[4] *
          age_base + fixef(list212a$res)[5] * age_change + fixef(list212a$res)[6] * sex + fixef(list212a$res)[7] * education + 
          fixef(list212a$res)[8] * BMI + fixef(list212a$res)[9] * hypertension + fixef(list212a$res)[10] * diabetes + 
          fixef(list212a$res)[11] * CESD
      )
    }
  } else if(dv == "memo"){
    if(model == 1){
      df <- df %>%
        plyr::mutate(
          error = rnorm(nrow(.), 0, 2*sd(residuals(list211b$res))),
          dv = fixef(list211b$res)[1] + sub_intercept_memo1 + error + fixef(list211b$res)[4] * proportion * LSNS_base + 
            fixef(list211b$res)[4] * proportion * LSNS_change + fixef(list211b$res)[4] *
            age_base + fixef(list211b$res)[5] * age_change + fixef(list211b$res)[6] * sex
        )
    } else {
      plyr::mutate(
        error = rnorm(nrow(.), 0, 2*sd(residuals(list212b$res))),
        dv = fixef(list212b$res)[1] + sub_intercept_memo2 + error + fixef(list212b$res)[4] * proportion * LSNS_base + 
          fixef(list212b$res)[4] * proportion* LSNS_change + fixef(list212b$res)[4] *
          age_base + fixef(list212b$res)[5] * age_change + fixef(list212b$res)[6] * sex + fixef(list212b$res)[7] * education + 
          fixef(list212b$res)[8] * BMI + fixef(list212b$res)[9] * hypertension + fixef(list212b$res)[10] * diabetes + 
          fixef(list212b$res)[11] * CESD
      )
    }
  } else if(dv == "procspeed"){
    if(model == 1){
      df <- df %>%
        plyr::mutate(
          error = rnorm(nrow(.), 0, 2*sd(residuals(list211c$res))),
          dv = fixef(list211c$res)[1] + sub_intercept_ps1 + error + fixef(list211c$res)[4] * proportion * LSNS_base + 
            fixef(list211c$res)[4] * proportion * LSNS_change + fixef(list211c$res)[4] *
            age_base + fixef(list211c$res)[5] * age_change + fixef(list211c$res)[6] * sex
        )
    } else {
      plyr::mutate(
        error = rnorm(nrow(.), 0, 2*sd(residuals(list212c$res))),
        dv = fixef(list212c$res)[1] + sub_intercept_ps2 + error + fixef(list212c$res)[4] * proportion * LSNS_base + 
          fixef(list212c$res)[4] * proportion * LSNS_change + fixef(list212c$res)[4] *
          age_base + fixef(list212c$res)[5] * age_change + fixef(list212c$res)[6] * sex + fixef(list212c$res)[7] * education + 
          fixef(list212c$res)[8] * BMI + fixef(list212c$res)[9] * hypertension + fixef(list212c$res)[10] * diabetes + 
          fixef(list212c$res)[11] * CESD
      )
    }
  } 
  df$id <- as.factor(df$id)
  if(model == 1){
    bf <- generalTestBF(dv~LSNS_base+LSNS_change+age_base+age_change+sex+id, 
                                   data=df, whichRandom = "id", multicore = T,
                                   neverExclude = c("age_base", "age_change", "sex", "id"))
  } else{
    bf <- generalTestBF(dv~LSNS_base+LSNS_change+age_base+age_change+sex+hypertension+diabetes+education+
                                     BMI+CESD+id, 
                                   data=df, whichRandom = "id", multicore = T,
                                   neverExclude = c("age_base", "age_change", "sex", "id", "hypertension", "diabetes", 
                                                    "education", "BMI", "CESD"))
  }
  
  
  return(bf)
}


bf1 <- simncalc(dv = "HCV", model = 1, proportion = 0.1)
bf2 <- simncalc(dv = "HCV", model = 1, proportion = 0.2)
bf3 <- simncalc(dv = "HCV", model = 1, proportion = 0.5)
bf4 <- simncalc(dv = "HCV", model = 2, proportion = 0.1)
bf5 <- simncalc(dv = "HCV", model = 2, proportion = 0.2)
bf6 <- simncalc(dv = "HCV", model = 2, proportion = 0.5)
bf7 <- simncalc(dv = "exfunct", model = 1, proportion = 0.1)
bf8 <- simncalc(dv = "exfunct", model = 1, proportion = 0.2)
bf9 <- simncalc(dv = "exfunct", model = 1, proportion = 0.5)
bf10 <- simncalc(dv = "exfunct", model = 2, proportion = 0.1)
bf11 <- simncalc(dv = "exfunct", model = 2, proportion = 0.2)
bf12 <- simncalc(dv = "exfunct", model = 2, proportion = 0.5)
bf13 <- simncalc(dv = "memo", model = 1, proportion = 0.1)
bf14 <- simncalc(dv = "memo", model = 1, proportion = 0.2)
bf15 <- simncalc(dv = "memo", model = 1, proportion = 0.5)
bf16 <- simncalc(dv = "memo", model = 2, proportion = 0.1)
bf17 <- simncalc(dv = "memo", model = 2, proportion = 0.2)
bf18 <- simncalc(dv = "memo", model = 2, proportion = 0.5)
bf19 <- simncalc(dv = "procspeed", model = 1, proportion = 0.1)
bf20 <- simncalc(dv = "procspeed", model = 1, proportion = 0.2)
bf21 <- simncalc(dv = "procspeed", model = 1, proportion = 0.5)
bf22 <- simncalc(dv = "procspeed", model = 2, proportion = 0.1)
bf23 <- simncalc(dv = "procspeed", model = 2, proportion = 0.2)
bf24 <- simncalc(dv = "procspeed", model = 2, proportion = 0.5)

save.image("workspace.RData")

