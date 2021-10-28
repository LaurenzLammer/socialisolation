# prepare dataframe
setwd("/data/pt_life/ResearchProjects/LLammer/Results/coefficients/")
coefficient_overview <- data.frame(matrix(ncol = 13, nrow = 17*4))
colnames(coefficient_overview) <- c("dv", "model", "predictor", "estimate", "standard error",
                                    "t-value", "2.5%", "97.5%", "p-value", "FDR-corrected", "BF", 
                                    "n_total", "n_individual")
coefficient_overview$dv <- c(rep("HCV",17), rep("exfunct",17), rep("memo",17), rep("procspeed",17))
coefficient_overview$model <- c(rep(1,6), rep(2,11))
coefficient_overview$predictor <- c( "LSNS_base*age_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "LSNS_base*age_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "BMI", "CESD", "diabetes", "education", "hypertension")
# load results of lmes and copy the results into the dataframe
file_list <- list.files(pattern = "LSNS_base:age_change")
files <- lapply(file_list, read.csv)
names(files) <- lapply(file_list, function(x) substr(x, 1, nchar(x)-4))

coefficient_overview[1:6,c(4:8,12,13)] <- files$`HCV_on_LSNS_base:age_change_model1`[c(7,6,8,4,5,9),3:9]
coefficient_overview[7:17,c(4:8,12,13)] <- files$`HCV_on_LSNS_base:age_change_model2`[c(12,11,13,4,5,14,6:10),3:9]
coefficient_overview[18:23,c(4:8,12,13)] <- files$`exfunct_on_LSNS_base:age_change_model1`[c(7,6,8,4,5,9),3:9]
coefficient_overview[24:34,c(4:8,12,13)] <- files$`exfunct_on_LSNS_base:age_change_model2`[c(12,11,13,4,5,14,6:10),3:9]
coefficient_overview[35:40,c(4:8,12,13)] <- files$`memo_on_LSNS_base:age_change_model1`[c(7,6,8,4,5,9),3:9]
coefficient_overview[41:51,c(4:8,12,13)] <- files$`memo_on_LSNS_base:age_change_model2`[c(12,11,13,4,5,14,6:10),3:9]
coefficient_overview[52:57,c(4:8,12,13)] <- files$`procspeed_on_LSNS_base:age_change_model1`[c(7,6,8,4,5,9),3:9]
coefficient_overview[58:68,c(4:8,12,13)] <- files$`procspeed_on_LSNS_base:age_change_model2`[c(12,11,13,4,5,14,6:10),3:9]

rows <- c(1,7,18,24,35,41,52,58)
pvals <- read.csv("/data/pt_life/ResearchProjects/LLammer/Results/pvals/overview.csv")
coefficient_overview[rows, 9:10] <- 
  pvals[c(3,15,10,22,11,23,12,24),c(10,8)]
bfs <- read.csv("/data/pt_life/ResearchProjects/LLammer/Results/bayes_factor/bayes_factors.csv")
coefficient_overview[rows, 11] <- 
  bfs[c(12,60,40,88,44,92,48,96),2]
write.csv(coefficient_overview, row.names = F,
          "/data/pt_life/ResearchProjects/LLammer/Results/coefficients/overview_full_lsns_base_age_change.csv")

coefficient_overview <- data.frame(matrix(ncol = 13, nrow = 17))
colnames(coefficient_overview) <- c("dv", "model", "predictor", "estimate", "standard error",
                                    "t-value", "2.5%", "97.5%", "p-value", "FDR-corrected", "BF", 
                                    "n_total", "n_individual")
coefficient_overview$dv <- "HCV"
coefficient_overview$model <- c(rep(1,6), rep(2,11))
coefficient_overview$predictor <- c( "LSNS_base*LSNS_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "LSNS_base*LSNS_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "BMI", "CESD", "diabetes", "education", "hypertension")

file_list <- list.files(pattern = "LSNS_base:LSNS_change")
files <- lapply(file_list, read.csv)
names(files) <- lapply(file_list, function(x) substr(x, 1, nchar(x)-4))

coefficient_overview[1:6,c(4:8,12,13)] <- files$`HCV_on_LSNS_base:LSNS_change_model1`[c(7,6,8,4,5,9),3:9]
coefficient_overview[7:17,c(4:8,12,13)] <- files$`HCV_on_LSNS_base:LSNS_change_model2`[c(12,11,13,4,5,14,6:10),3:9]

rows <- c(1,7)
coefficient_overview[rows, 9:10] <- pvals[c(25,26),c(10,8)]
coefficient_overview[rows, 11] <- bfs[c(100,104),2]
write.csv(coefficient_overview, row.names = F,
          "/data/pt_life/ResearchProjects/LLammer/Results/coefficients/overview_full_lsns_base_lsns_change.csv")
