# this script is used to simulate the calculation of BFs in a realistic setting to examine the occurrence of BFs > 3
# due to chance

library("BayesFactor")
library(doMC)

# load actual data
data <- read.csv("/data/pt_life/LLammer/Data/compiled_scaled_data.csv")
data$subject <- as.factor(data$subject)

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
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+subject")), 
                                     data=datax, whichRandom = "subject",  multicore = T,
                                     neverExclude = c("LSNS_change", "age_base", "age_change", "sex", "subject"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else if(poi == "LSNS_change"){
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+subject")), 
                                     data=datax, whichRandom = "subject",  multicore = T,
                                     neverExclude = c("LSNS_base", "age_base", "age_change", "sex", "subject"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else if(poi == "LSNS_base:age_change"){
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+LSNS_base:age_change+subject")), 
                                     data=datax, whichRandom = "subject",  multicore = T,
                                     neverExclude = c("LSNS_change", "age_base", "sex", "subject", "^LSNS_base$", "^age_change$"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else{
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+LSNS_base:LSNS_change+subject")), 
                                     data=datax, whichRandom = "subject",  multicore = T,
                                     neverExclude = c("age_base", "age_change", "sex", "subject", "^LSNS_base$", "^LSNS_change$"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
  } 
  
  else{
    datay <- subset(datax, !is.na(datax$CES.D))
    if(poi == "LSNS_base"){
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+subject")), 
                                     data=datay, whichRandom = "subject", multicore = T, neverExclude = 
                                       c("LSNS_change", "age_base", "age_change", "sex", "education", "BMI", "hypertension", "diabetes", "CES.D", "subject"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else if(poi == "LSNS_change"){
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+subject")), 
                                     data=datay, whichRandom = "subject", multicore = T, neverExclude = 
                                       c("LSNS_base", "age_base", "age_change", "sex", "education", "BMI", "hypertension", "diabetes", "CES.D", "subject"))),
                      logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else if(poi == "LSNS_base:age_change"){
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+LSNS_base:age_change+subject")), 
                                     data=datay, whichRandom = "subject", multicore = T, 
                                     neverExclude = c("LSNS_change", "age_base", "sex", "education", "BMI", "hypertension", 
                                                      "diabetes", "CES.D", "subject", "^LSNS_base$", "^age_change$"))), logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
    else{
      bf <- extractBF((generalTestBF(formula = as.formula(paste0(dv, "~LSNS_base+LSNS_change+age_base+age_change+sex+education+BMI+hypertension+diabetes+CES.D+LSNS_base:LSNS_change+subject")), 
                                     data=datay, whichRandom = "subject", multicore = T, 
                                     neverExclude = c("age_base", "age_change", "sex", "education", "BMI", "hypertension", 
                                                      "diabetes", "CES.D", "subject", "^LSNS_base$", "^LSNS_change$"))), logbf = F)
      bf["full_vs_null", 1] <- bf[1,1] / bf[2,1]
    }
  }
  return(bf)
}

# create new directory, because script will be run in parallel on multiple servers to accelerate computation 
directory = "/data/pt_life/LLammer/Analysis/Simulation2/run1/"
counter = 1
while (dir.exists(directory)) {
  counter = counter + 1
  directory = paste0(substr(directory, 1, 46), counter, "/")
}
dir.create(directory, recursive = T)

# replace actual LSNS data with simulated data
data$LSNS_base <- rnorm(length(data$subject), mean = 15, sd = 3)
data$LSNS_change <- rnorm(length(data$subject), mean = 0, sd = 2.5)

# calculate models of 4 dependent variables regressed on LSNS_base & LSNS_change and their interactioncontrolling for
# different variables 
bf1a <- fastlme(dv = "HCV")
bf1b <- fastlme(dv = "HCV", model = 2)
bf2a <- fastlme(poi = "LSNS_change" ,dv = "HCV")
bf2b <- fastlme(poi = "LSNS_change", dv = "HCV", model = 2)
bf3a <- fastlme(poi = "LSNS_base:age_change", dv = "HCV")
bf3b <- fastlme(poi = "LSNS_base:age_change", dv = "HCV", model = 2)
bf4a <- fastlme(dv = "exfunct")
bf4b <- fastlme(dv = "exfunct", model = 2)
bf5a <- fastlme(poi = "LSNS_change" ,dv = "exfunct")
bf5b <- fastlme(poi = "LSNS_change", dv = "exfunct", model = 2)
bf6a <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct")
bf6b <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct", model = 2)
bf7a <- fastlme(dv = "memo")
bf7b <- fastlme(dv = "memo", model = 2)
bf8a <- fastlme(poi = "LSNS_change" ,dv = "memo")
bf8b <- fastlme(poi = "LSNS_change", dv = "memo", model = 2)
bf9a <- fastlme(poi = "LSNS_base:age_change", dv = "memo")
bf9b <- fastlme(poi = "LSNS_base:age_change", dv = "memo", model = 2)
bf10a <- fastlme(dv = "procspeed")
bf10b <- fastlme(dv = "procspeed", model = 2)
bf11a <- fastlme(poi = "LSNS_change" ,dv = "procspeed")
bf11b <- fastlme(poi = "LSNS_change", dv = "procspeed", model = 2)
bf12a <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed")
bf12b <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed", model = 2)

save.image(paste0(directory, "workspace.RData"))

# create new directory, because script will be run in parallel on multiple servers to accelerate computation 
directory = "/data/pt_life/LLammer/Analysis/Simulation2/run1/"
counter = 1
while (dir.exists(directory)) {
  counter = counter + 1
  directory = paste0(substr(directory, 1, 46), counter, "/")
}
dir.create(directory, recursive = T)

# replace actual LSNS data with simulated data
data$LSNS_base <- rnorm(length(data$subject), mean = 15, sd = 3)
data$LSNS_change <- rnorm(length(data$subject), mean = 0, sd = 2.5)

# calculate models of 4 dependent variables regressed on LSNS_base & LSNS_change and their interactioncontrolling for
# different variables 
bf1a <- fastlme(dv = "HCV")
bf1b <- fastlme(dv = "HCV", model = 2)
bf2a <- fastlme(poi = "LSNS_change" ,dv = "HCV")
bf2b <- fastlme(poi = "LSNS_change", dv = "HCV", model = 2)
bf3a <- fastlme(poi = "LSNS_base:age_change", dv = "HCV")
bf3b <- fastlme(poi = "LSNS_base:age_change", dv = "HCV", model = 2)
bf4a <- fastlme(dv = "exfunct")
bf4b <- fastlme(dv = "exfunct", model = 2)
bf5a <- fastlme(poi = "LSNS_change" ,dv = "exfunct")
bf5b <- fastlme(poi = "LSNS_change", dv = "exfunct", model = 2)
bf6a <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct")
bf6b <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct", model = 2)
bf7a <- fastlme(dv = "memo")
bf7b <- fastlme(dv = "memo", model = 2)
bf8a <- fastlme(poi = "LSNS_change" ,dv = "memo")
bf8b <- fastlme(poi = "LSNS_change", dv = "memo", model = 2)
bf9a <- fastlme(poi = "LSNS_base:age_change", dv = "memo")
bf9b <- fastlme(poi = "LSNS_base:age_change", dv = "memo", model = 2)
bf10a <- fastlme(dv = "procspeed")
bf10b <- fastlme(dv = "procspeed", model = 2)
bf11a <- fastlme(poi = "LSNS_change" ,dv = "procspeed")
bf11b <- fastlme(poi = "LSNS_change", dv = "procspeed", model = 2)
bf12a <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed")
bf12b <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed", model = 2)

save.image(paste0(directory, "workspace2.RData"))

# create new directory, because script will be run in parallel on multiple servers to accelerate computation 
directory = "/data/pt_life/LLammer/Analysis/Simulation2/run1/"
counter = 1
while (dir.exists(directory)) {
  counter = counter + 1
  directory = paste0(substr(directory, 1, 46), counter, "/")
}
dir.create(directory, recursive = T)

# replace actual LSNS data with simulated data
data$LSNS_base <- rnorm(length(data$subject), mean = 15, sd = 3)
data$LSNS_change <- rnorm(length(data$subject), mean = 0, sd = 2.5)

# calculate models of 4 dependent variables regressed on LSNS_base & LSNS_change and their interactioncontrolling for
# different variables 
bf1a <- fastlme(dv = "HCV")
bf1b <- fastlme(dv = "HCV", model = 2)
bf2a <- fastlme(poi = "LSNS_change" ,dv = "HCV")
bf2b <- fastlme(poi = "LSNS_change", dv = "HCV", model = 2)
bf3a <- fastlme(poi = "LSNS_base:age_change", dv = "HCV")
bf3b <- fastlme(poi = "LSNS_base:age_change", dv = "HCV", model = 2)
bf4a <- fastlme(dv = "exfunct")
bf4b <- fastlme(dv = "exfunct", model = 2)
bf5a <- fastlme(poi = "LSNS_change" ,dv = "exfunct")
bf5b <- fastlme(poi = "LSNS_change", dv = "exfunct", model = 2)
bf6a <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct")
bf6b <- fastlme(poi = "LSNS_base:age_change", dv = "exfunct", model = 2)
bf7a <- fastlme(dv = "memo")
bf7b <- fastlme(dv = "memo", model = 2)
bf8a <- fastlme(poi = "LSNS_change" ,dv = "memo")
bf8b <- fastlme(poi = "LSNS_change", dv = "memo", model = 2)
bf9a <- fastlme(poi = "LSNS_base:age_change", dv = "memo")
bf9b <- fastlme(poi = "LSNS_base:age_change", dv = "memo", model = 2)
bf10a <- fastlme(dv = "procspeed")
bf10b <- fastlme(dv = "procspeed", model = 2)
bf11a <- fastlme(poi = "LSNS_change" ,dv = "procspeed")
bf11b <- fastlme(poi = "LSNS_change", dv = "procspeed", model = 2)
bf12a <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed")
bf12b <- fastlme(poi = "LSNS_base:age_change", dv = "procspeed", model = 2)

save.image(paste0(directory, "workspace3.RData"))
