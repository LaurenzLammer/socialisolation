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
setwd("/data/pt_life/ResearchProjects/LLammer/Results/coefficients/")
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
pvals <- read.csv("/data/pt_life/ResearchProjects/LLammer/Results/pvals/overview.csv")
coefficient_overview[rows, 9:10] <- 
  pvals[c(1,2,13,14,4,7,16,19,5,8,17,20,6,9,18,21),c(10,8)]

bfs <- read.csv("/data/pt_life/ResearchProjects/LLammer/Results/bayes_factor/bayes_factors.csv")
coefficient_overview[rows, 11] <- 
  bfs[c(4,8,52,56,16,28,64,76,20,32,68,80,24,36,72,84),2]
write.csv(coefficient_overview, row.names = F,
          "/data/pt_life/ResearchProjects/LLammer/Results/coefficients/overview_full.csv")
