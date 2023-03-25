# create dataframe and name columns
coefficient_overview <- data.frame(matrix(ncol = 13, nrow = 60))
colnames(coefficient_overview) <- c("dv", "model", "predictor", "estimate", "standard error",
                                    "t-value", "2.5%", "97.5%", "p-value", "FDR-corrected", "BF", 
                                    "n_total", "n_individual")
coefficient_overview$dv <- c(rep("HCV",15), rep("exfunct",15), rep("memo",15), rep("procspeed",15))
coefficient_overview$model <- c(rep(1,5), rep(2,10))
coefficient_overview$predictor <- c("LSNS_base", "LSNS_change", "age_base", "age_change", "sex", "LSNS_base", 
                                    "LSNS_change", "age_base", "age_change", "sex", "BMI", "CESD", "diabetes", 
                                    "education", "hypertension")
# load results of lmes and copy them into the dataframe
setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results/coefficients/")
file_list <- list.files(pattern = ".csv")
files <- lapply(file_list, read.csv)
names(files) <- lapply(file_list, function(x) substr(x, 1, nchar(x)-4))

coefficient_overview[c(3,4,1,2,5),c(4:8,12,13)] <- files$HCV_on_LSNS_base_model1[4:8,3:9]
coefficient_overview[c(8,9,11:15,6,7,10),c(4:8,12,13)] <- files$HCV_on_LSNS_base_model2[c(4:13),3:9]
coefficient_overview[c(18,19,16,17,20),c(4:8,12,13)] <- files$exfunct_on_LSNS_base_model1[4:8,3:9]
coefficient_overview[c(23,24,26:30,21,22,25),c(4:8,12,13)] <- files$exfunct_on_LSNS_base_model2[c(4:13),3:9]
coefficient_overview[c(33,34,31,32,35),c(4:8,12,13)] <- files$memo_on_LSNS_base_model1[4:8,3:9]
coefficient_overview[c(38,39,41:45,36,37,40),c(4:8,12,13)] <- files$memo_on_LSNS_base_model2[c(4:13),3:9]
coefficient_overview[c(48,49,46,47,50),c(4:8,12,13)] <- files$procspeed_on_LSNS_base_model1[4:8,3:9]
coefficient_overview[c(53,54,56:60,51,52,55),c(4:8,12,13)] <- files$procspeed_on_LSNS_base_model2[c(4:13),3:9]

n <- 0
m <- 1
rows <- c(1)
while(n < 15){
  n <- n + 1
  if((n %% 2) != 0){
    m <- m + 1
  }
  else if((n %% 2) == 0 & (n %% 4) != 0){
    m <- m + 4
  }
  else{
    m <- m + 9
  }
  rows <- c(rows, m)
}
pvals <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Results/pvals/overview.csv")
coefficient_overview[rows, 9:10] <- 
  pvals[c(1,2,13,14,4,7,16,19,5,8,17,20,6,9,18,21),c(10,8)]

bfs <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Results/bayes_factor/bayes_factors.csv")
coefficient_overview[rows, 11] <- 
  bfs[c(4,8,52,56,16,28,64,76,20,32,68,80,24,36,72,84),2]
write.csv(coefficient_overview, row.names = F,
          "/data/pt_life/ResearchProjects/LLammer/si_update/Results/coefficients/overview_full.csv")

# create overviews for models with interaction terms

coefficient_overview <- data.frame(matrix(ncol = 13, nrow = 17*4))
colnames(coefficient_overview) <- c("dv", "model", "predictor", "estimate", "standard error",
                                    "t-value", "2.5%", "97.5%", "p-value", "FDR-corrected", "BF", 
                                    "n_total", "n_individual")
coefficient_overview$dv <- c(rep("HCV",17), rep("exfunct",17), rep("memo",17), rep("procspeed",17))
coefficient_overview$model <- c(rep(1,6), rep(2,11))
coefficient_overview$predictor <- c( "LSNS_base*age_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "LSNS_base*age_change", "LSNS_base", "LSNS_change", "age_base", "age_change", "sex", 
                                     "BMI", "CESD", "diabetes", "education", "hypertension")

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
pvals <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Results/pvals/overview.csv")
coefficient_overview[rows, 9:10] <- 
  pvals[c(3,15,10,22,11,23,12,24),c(10,8)]
bfs <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Results/bayes_factor/bayes_factors.csv")
coefficient_overview[rows, 11] <- 
  bfs[c(12,60,40,88,44,92,48,96),2]
write.csv(coefficient_overview, row.names = F,
          "/data/pt_life/ResearchProjects/LLammer/si_update/Results/coefficients/overview_full_lsns_base_age_change.csv")

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
          "/data/pt_life/ResearchProjects/LLammer/si_update/Results/coefficients/overview_full_lsns_base_lsns_change.csv")
