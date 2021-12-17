## script to evaluate the results of the power simulation

setwd("/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation")

## load all workspaces
files <- list.files(pattern = "workspace.RData", recursive = T)

## prepare dataframe
overview <- data.frame(matrix(ncol = 11, nrow = 24*length(files)))
colnames(overview) <- c("simulation", "dv", "model", "effect", "BFA0b", "BFA0c", "BFA0b_sided", "BFA0c_sided", "BF_full", "BF_null_b", "BF_null_c")
overview$dv <- c(rep("HCV", 6), rep("exfunct", 6), rep("memo", 6), rep("procspeed", 6))
overview$model <- c(rep(1, 3), rep(2, 3))
overview$effect <- c(0.1,0.2,0.5)
## transfer information from workspaces to dataframe
n <- 0
for (i in 1:length(files)) {
  load(files[i])
  for(sim in list(bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8, bf9, bf10, bf11, bf12, bf13, bf14, bf15, bf16, bf17,
                  bf18, bf19, bf20, bf21, bf22, bf23, bf24)){
    n <- n + 1
    overview$simulation[n] <- i
    overview$BF_full[n] <- sim$bf@bayesFactor[3,1]
    overview$BF_null_b[n] <- sim$bf@bayesFactor[2,1]
    overview$BF_null_c[n] <- sim$bf@bayesFactor[1,1]
    overview$BFA0b[n] <- exp(overview$BF_full[n]-overview$BF_null_b[n])
    overview$BFA0c[n] <- exp(overview$BF_full[n]-overview$BF_null_c[n])
    overview$BFA0b_sided[n] <- overview$BFA0b[n]  * 2 * sim$siding_factor_base
    overview$BFA0c_sided[n] <- overview$BFA0c[n]  * 2 * sim$siding_factor_change
  }
}
write.csv(overview, "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/results.csv")

# create dataframe for summary tables of the percentages of false positives, undecided BFs and true positives for baseline social isolation (BFA0b) & change in social siolation (BFAOc) for different effect sizes and dvs at a threshold of 3 and 10.75 
sum_up <- data.frame(matrix(ncol = 7, nrow =10))
colnames(sum_up) <- c("BFA0b > 3 in %", "3 >= BFA0b >= 1/3 in %", "BFA0b < 1/3 in %", 
                      "BFA0c > 3 in %", "3 >= BFA0c >= 1/3 in %", "BFA0c < 1/3 in %", "n")
rownames(sum_up) <- c("overall", "model 1", "model 2", "HCV", "exfunct", "memo", "procspeed", "effect = 0.1",
                      "effect = 0.2", "effect = 0.5")
sum_up["overall",] <- c(length(which(overview$BFA0b > 3))/nrow(overview)*100, 
                        length(which(3 >= overview$BFA0b & overview$BFA0b >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0b < 1/3))/nrow(overview)*100,
                        length(which(overview$BFA0c > 3))/nrow(overview)*100, 
                        length(which(3 >= overview$BFA0c & overview$BFA0c >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0c < 1/3))/nrow(overview)*100, nrow(overview))

sub_m1 <- subset(overview, overview$model == 1)
sum_up["model 1",] <- c(length(which(sub_m1$BFA0b > 3))/nrow(sub_m1)*100, 
                        length(which(3 >= sub_m1$BFA0b & sub_m1$BFA0b >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0b < 1/3))/nrow(sub_m1)*100,
                        length(which(sub_m1$BFA0c > 3))/nrow(sub_m1)*100, 
                        length(which(3 >= sub_m1$BFA0c & sub_m1$BFA0c >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0c < 1/3))/nrow(sub_m1)*100, nrow(sub_m1))

sub_m2 <- subset(overview, overview$model == 2)
sum_up["model 2",] <- c(length(which(sub_m2$BFA0b > 3))/nrow(sub_m2)*100, 
                        length(which(3 >= sub_m2$BFA0b & sub_m2$BFA0b >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0b < 1/3))/nrow(sub_m2)*100,
                        length(which(sub_m2$BFA0c > 3))/nrow(sub_m2)*100, 
                        length(which(3 >= sub_m2$BFA0c & sub_m2$BFA0c >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0c < 1/3))/nrow(sub_m2)*100, nrow(sub_m2))

sub_hcv <- subset(overview, overview$dv == "HCV")
sum_up["HCV",] <- c(length(which(sub_hcv$BFA0b > 3))/nrow(sub_hcv)*100, 
                    length(which(3 >= sub_hcv$BFA0b & sub_hcv$BFA0b >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0b < 1/3))/nrow(sub_hcv)*100,
                    length(which(sub_hcv$BFA0c > 3))/nrow(sub_hcv)*100, 
                    length(which(3 >= sub_hcv$BFA0c & sub_hcv$BFA0c >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0c < 1/3))/nrow(sub_hcv)*100, nrow(sub_hcv))

sub_ef <- subset(overview, overview$dv == "exfunct")
sum_up["exfunct",] <- c(length(which(sub_ef$BFA0b > 3))/nrow(sub_ef)*100, 
                        length(which(3 >= sub_ef$BFA0b & sub_ef$BFA0b >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0b < 1/3))/nrow(sub_ef)*100,
                        length(which(sub_ef$BFA0c > 3))/nrow(sub_ef)*100, 
                        length(which(3 >= sub_ef$BFA0c & sub_ef$BFA0c >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0c < 1/3))/nrow(sub_ef)*100, nrow(sub_ef))

sub_memo <- subset(overview, overview$dv == "memo")
sum_up["memo",] <- c(length(which(sub_memo$BFA0b > 3))/nrow(sub_memo)*100, 
                     length(which(3 >= sub_memo$BFA0b & sub_memo$BFA0b >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0b < 1/3))/nrow(sub_memo)*100,
                     length(which(sub_memo$BFA0c > 3))/nrow(sub_memo)*100, 
                     length(which(3 >= sub_memo$BFA0c & sub_memo$BFA0c >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0c < 1/3))/nrow(sub_memo)*100, nrow(sub_memo))

sub_ps <- subset(overview, overview$dv == "procspeed")
sum_up["procspeed",] <- c(length(which(sub_ps$BFA0b > 3))/nrow(sub_ps)*100, 
                          length(which(3 >= sub_ps$BFA0b & sub_ps$BFA0b >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0b < 1/3))/nrow(sub_ps)*100,
                          length(which(sub_ps$BFA0c > 3))/nrow(sub_ps)*100, 
                          length(which(3 >= sub_ps$BFA0c & sub_ps$BFA0c >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0c < 1/3))/nrow(sub_ps)*100, nrow(sub_ps))

sub_01 <- subset(overview, overview$effect == 0.1) 
sum_up["effect = 0.1",] <- c(length(which(sub_01$BFA0b > 3))/nrow(sub_01)*100, 
                             length(which(3 >= sub_01$BFA0b & sub_01$BFA0b >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0b < 1/3))/nrow(sub_01)*100,
                             length(which(sub_01$BFA0c > 3))/nrow(sub_01)*100, 
                             length(which(3 >= sub_01$BFA0c & sub_01$BFA0c >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0c < 1/3))/nrow(sub_01)*100, nrow(sub_01))

sub_02 <- subset(overview, overview$effect == 0.2)
sum_up["effect = 0.2",] <- c(length(which(sub_02$BFA0b > 3))/nrow(sub_02)*100, 
                             length(which(3 >= sub_02$BFA0b & sub_02$BFA0b >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0b < 1/3))/nrow(sub_02)*100,
                             length(which(sub_02$BFA0c > 3))/nrow(sub_02)*100, 
                             length(which(3 >= sub_02$BFA0c & sub_02$BFA0c >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0c < 1/3))/nrow(sub_02)*100, nrow(sub_02))

sub_05 <- subset(overview, overview$effect == 0.5)
sum_up["effect = 0.5",] <- c(length(which(sub_05$BFA0b > 3))/nrow(sub_05)*100, 
                             length(which(3 >= sub_05$BFA0b & sub_05$BFA0b >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0b < 1/3))/nrow(sub_05)*100,
                             length(which(sub_05$BFA0c > 3))/nrow(sub_05)*100, 
                             length(which(3 >= sub_05$BFA0c & sub_05$BFA0c >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0c < 1/3))/nrow(sub_05)*100, nrow(sub_05))

write.csv(sum_up, "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/summary.csv")

sum_up_sided <- data.frame(matrix(ncol = 7, nrow =10))
colnames(sum_up_sided) <- c("BFA0b_sided > 3 in %", "3 >= BFA0b_sided >= 1/3 in %", "BFA0b_sided < 1/3 in %", 
                            "BFA0c_sided > 3 in %", "3 >= BFA0c_sided >= 1/3 in %", "BFA0c_sided < 1/3 in %", "n")
rownames(sum_up_sided) <- c("overall", "model 1", "model 2", "HCV", "exfunct", "memo", "procspeed", "effect = 0.1",
                            "effect = 0.2", "effect = 0.5")
sum_up_sided["overall",] <- c(length(which(overview$BFA0b_sided > 3))/nrow(overview)*100, 
                              length(which(3 >= overview$BFA0b_sided & overview$BFA0b_sided >= 1/3))/nrow(overview)*100, 
                              length(which(overview$BFA0b_sided < 1/3))/nrow(overview)*100,
                              length(which(overview$BFA0c_sided > 3))/nrow(overview)*100, 
                              length(which(3 >= overview$BFA0c_sided & overview$BFA0c_sided >= 1/3))/nrow(overview)*100, 
                              length(which(overview$BFA0c_sided < 1/3))/nrow(overview)*100, nrow(overview))

sum_up_sided["model 1",] <- c(length(which(sub_m1$BFA0b_sided > 3))/nrow(sub_m1)*100, 
                              length(which(3 >= sub_m1$BFA0b_sided & sub_m1$BFA0b_sided >= 1/3))/nrow(sub_m1)*100, 
                              length(which(sub_m1$BFA0b_sided < 1/3))/nrow(sub_m1)*100,
                              length(which(sub_m1$BFA0c_sided > 3))/nrow(sub_m1)*100, 
                              length(which(3 >= sub_m1$BFA0c_sided & sub_m1$BFA0c_sided >= 1/3))/nrow(sub_m1)*100, 
                              length(which(sub_m1$BFA0c_sided < 1/3))/nrow(sub_m1)*100, nrow(sub_m1))

sum_up_sided["model 2",] <- c(length(which(sub_m2$BFA0b_sided > 3))/nrow(sub_m2)*100, 
                              length(which(3 >= sub_m2$BFA0b_sided & sub_m2$BFA0b_sided >= 1/3))/nrow(sub_m2)*100, 
                              length(which(sub_m2$BFA0b_sided < 1/3))/nrow(sub_m2)*100,
                              length(which(sub_m2$BFA0c_sided > 3))/nrow(sub_m2)*100, 
                              length(which(3 >= sub_m2$BFA0c_sided & sub_m2$BFA0c_sided >= 1/3))/nrow(sub_m2)*100, 
                              length(which(sub_m2$BFA0c_sided < 1/3))/nrow(sub_m2)*100, nrow(sub_m2))

sum_up_sided["HCV",] <- c(length(which(sub_hcv$BFA0b_sided > 3))/nrow(sub_hcv)*100, 
                          length(which(3 >= sub_hcv$BFA0b_sided & sub_hcv$BFA0b_sided >= 1/3))/nrow(sub_hcv)*100, 
                          length(which(sub_hcv$BFA0b_sided < 1/3))/nrow(sub_hcv)*100,
                          length(which(sub_hcv$BFA0c_sided > 3))/nrow(sub_hcv)*100, 
                          length(which(3 >= sub_hcv$BFA0c_sided & sub_hcv$BFA0c_sided >= 1/3))/nrow(sub_hcv)*100, 
                          length(which(sub_hcv$BFA0c_sided < 1/3))/nrow(sub_hcv)*100, nrow(sub_hcv))

sum_up_sided["exfunct",] <- c(length(which(sub_ef$BFA0b_sided > 3))/nrow(sub_ef)*100, 
                              length(which(3 >= sub_ef$BFA0b_sided & sub_ef$BFA0b_sided >= 1/3))/nrow(sub_ef)*100, 
                              length(which(sub_ef$BFA0b_sided < 1/3))/nrow(sub_ef)*100,
                              length(which(sub_ef$BFA0c_sided > 3))/nrow(sub_ef)*100, 
                              length(which(3 >= sub_ef$BFA0c_sided & sub_ef$BFA0c_sided >= 1/3))/nrow(sub_ef)*100, 
                              length(which(sub_ef$BFA0c_sided < 1/3))/nrow(sub_ef)*100, nrow(sub_ef))

sum_up_sided["memo",] <- c(length(which(sub_memo$BFA0b_sided > 3))/nrow(sub_memo)*100, 
                           length(which(3 >= sub_memo$BFA0b_sided & sub_memo$BFA0b_sided >= 1/3))/nrow(sub_memo)*100, 
                           length(which(sub_memo$BFA0b_sided < 1/3))/nrow(sub_memo)*100,
                           length(which(sub_memo$BFA0c_sided > 3))/nrow(sub_memo)*100, 
                           length(which(3 >= sub_memo$BFA0c_sided & sub_memo$BFA0c_sided >= 1/3))/nrow(sub_memo)*100, 
                           length(which(sub_memo$BFA0c_sided < 1/3))/nrow(sub_memo)*100, nrow(sub_memo))

sum_up_sided["procspeed",] <- c(length(which(sub_ps$BFA0b_sided > 3))/nrow(sub_ps)*100, 
                                length(which(3 >= sub_ps$BFA0b_sided & sub_ps$BFA0b_sided >= 1/3))/nrow(sub_ps)*100, 
                                length(which(sub_ps$BFA0b_sided < 1/3))/nrow(sub_ps)*100,
                                length(which(sub_ps$BFA0c_sided > 3))/nrow(sub_ps)*100, 
                                length(which(3 >= sub_ps$BFA0c_sided & sub_ps$BFA0c_sided >= 1/3))/nrow(sub_ps)*100, 
                                length(which(sub_ps$BFA0c_sided < 1/3))/nrow(sub_ps)*100, nrow(sub_ps))

sum_up_sided["effect = 0.1",] <- c(length(which(sub_01$BFA0b_sided > 3))/nrow(sub_01)*100, 
                                   length(which(3 >= sub_01$BFA0b_sided & sub_01$BFA0b_sided >= 1/3))/nrow(sub_01)*100, 
                                   length(which(sub_01$BFA0b_sided < 1/3))/nrow(sub_01)*100,
                                   length(which(sub_01$BFA0c_sided > 3))/nrow(sub_01)*100, 
                                   length(which(3 >= sub_01$BFA0c_sided & sub_01$BFA0c_sided >= 1/3))/nrow(sub_01)*100, 
                                   length(which(sub_01$BFA0c_sided < 1/3))/nrow(sub_01)*100, nrow(sub_01))

sum_up_sided["effect = 0.2",] <- c(length(which(sub_02$BFA0b_sided > 3))/nrow(sub_02)*100, 
                                   length(which(3 >= sub_02$BFA0b_sided & sub_02$BFA0b_sided >= 1/3))/nrow(sub_02)*100, 
                                   length(which(sub_02$BFA0b_sided < 1/3))/nrow(sub_02)*100,
                                   length(which(sub_02$BFA0c_sided > 3))/nrow(sub_02)*100, 
                                   length(which(3 >= sub_02$BFA0c_sided & sub_02$BFA0c_sided >= 1/3))/nrow(sub_02)*100, 
                                   length(which(sub_02$BFA0c_sided < 1/3))/nrow(sub_02)*100, nrow(sub_02))

sum_up_sided["effect = 0.5",] <- c(length(which(sub_05$BFA0b_sided > 3))/nrow(sub_05)*100, 
                                   length(which(3 >= sub_05$BFA0b_sided & sub_05$BFA0b_sided >= 1/3))/nrow(sub_05)*100, 
                                   length(which(sub_05$BFA0b_sided < 1/3))/nrow(sub_05)*100,
                                   length(which(sub_05$BFA0c_sided > 3))/nrow(sub_05)*100, 
                                   length(which(3 >= sub_05$BFA0c_sided & sub_05$BFA0c_sided >= 1/3))/nrow(sub_05)*100, 
                                   length(which(sub_05$BFA0c_sided < 1/3))/nrow(sub_05)*100, nrow(sub_05))

write.csv(sum_up_sided, "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/summary_sided.csv")

sum_up <- data.frame(matrix(ncol = 7, nrow =10))
colnames(sum_up) <- c("BFA0b > 10.75 in %", "10.75 >= BFA0b >= 1/3 in %", "BFA0b < 1/3 in %", 
                      "BFA0c > 10.75 in %", "10.75 >= BFA0c >= 1/3 in %", "BFA0c < 1/3 in %", "n")
rownames(sum_up) <- c("overall", "model 1", "model 2", "HCV", "exfunct", "memo", "procspeed", "effect = 0.1",
                      "effect = 0.2", "effect = 0.5")
sum_up["overall",] <- c(length(which(overview$BFA0b > 10.75))/nrow(overview)*100, 
                        length(which(10.75 >= overview$BFA0b & overview$BFA0b >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0b < 1/3))/nrow(overview)*100,
                        length(which(overview$BFA0c > 10.75))/nrow(overview)*100, 
                        length(which(10.75 >= overview$BFA0c & overview$BFA0c >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0c < 1/3))/nrow(overview)*100, nrow(overview))

sub_m1 <- subset(overview, overview$model == 1)
sum_up["model 1",] <- c(length(which(sub_m1$BFA0b > 10.75))/nrow(sub_m1)*100, 
                        length(which(10.75 >= sub_m1$BFA0b & sub_m1$BFA0b >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0b < 1/3))/nrow(sub_m1)*100,
                        length(which(sub_m1$BFA0c > 10.75))/nrow(sub_m1)*100, 
                        length(which(10.75 >= sub_m1$BFA0c & sub_m1$BFA0c >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0c < 1/3))/nrow(sub_m1)*100, nrow(sub_m1))

sub_m2 <- subset(overview, overview$model == 2)
sum_up["model 2",] <- c(length(which(sub_m2$BFA0b > 10.75))/nrow(sub_m2)*100, 
                        length(which(10.75 >= sub_m2$BFA0b & sub_m2$BFA0b >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0b < 1/3))/nrow(sub_m2)*100,
                        length(which(sub_m2$BFA0c > 10.75))/nrow(sub_m2)*100, 
                        length(which(10.75 >= sub_m2$BFA0c & sub_m2$BFA0c >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0c < 1/3))/nrow(sub_m2)*100, nrow(sub_m2))

sub_hcv <- subset(overview, overview$dv == "HCV")
sum_up["HCV",] <- c(length(which(sub_hcv$BFA0b > 10.75))/nrow(sub_hcv)*100, 
                    length(which(10.75 >= sub_hcv$BFA0b & sub_hcv$BFA0b >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0b < 1/3))/nrow(sub_hcv)*100,
                    length(which(sub_hcv$BFA0c > 10.75))/nrow(sub_hcv)*100, 
                    length(which(10.75 >= sub_hcv$BFA0c & sub_hcv$BFA0c >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0c < 1/3))/nrow(sub_hcv)*100, nrow(sub_hcv))

sub_ef <- subset(overview, overview$dv == "exfunct")
sum_up["exfunct",] <- c(length(which(sub_ef$BFA0b > 10.75))/nrow(sub_ef)*100, 
                        length(which(10.75 >= sub_ef$BFA0b & sub_ef$BFA0b >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0b < 1/3))/nrow(sub_ef)*100,
                        length(which(sub_ef$BFA0c > 10.75))/nrow(sub_ef)*100, 
                        length(which(10.75 >= sub_ef$BFA0c & sub_ef$BFA0c >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0c < 1/3))/nrow(sub_ef)*100, nrow(sub_ef))

sub_memo <- subset(overview, overview$dv == "memo")
sum_up["memo",] <- c(length(which(sub_memo$BFA0b > 10.75))/nrow(sub_memo)*100, 
                     length(which(10.75 >= sub_memo$BFA0b & sub_memo$BFA0b >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0b < 1/3))/nrow(sub_memo)*100,
                     length(which(sub_memo$BFA0c > 10.75))/nrow(sub_memo)*100, 
                     length(which(10.75 >= sub_memo$BFA0c & sub_memo$BFA0c >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0c < 1/3))/nrow(sub_memo)*100, nrow(sub_memo))

sub_ps <- subset(overview, overview$dv == "procspeed")
sum_up["procspeed",] <- c(length(which(sub_ps$BFA0b > 10.75))/nrow(sub_ps)*100, 
                          length(which(10.75 >= sub_ps$BFA0b & sub_ps$BFA0b >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0b < 1/3))/nrow(sub_ps)*100,
                          length(which(sub_ps$BFA0c > 10.75))/nrow(sub_ps)*100, 
                          length(which(10.75 >= sub_ps$BFA0c & sub_ps$BFA0c >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0c < 1/3))/nrow(sub_ps)*100, nrow(sub_ps))

sub_01 <- subset(overview, overview$effect == 0.1) 
sum_up["effect = 0.1",] <- c(length(which(sub_01$BFA0b > 10.75))/nrow(sub_01)*100, 
                             length(which(10.75 >= sub_01$BFA0b & sub_01$BFA0b >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0b < 1/3))/nrow(sub_01)*100,
                             length(which(sub_01$BFA0c > 10.75))/nrow(sub_01)*100, 
                             length(which(10.75 >= sub_01$BFA0c & sub_01$BFA0c >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0c < 1/3))/nrow(sub_01)*100, nrow(sub_01))

sub_02 <- subset(overview, overview$effect == 0.2)
sum_up["effect = 0.2",] <- c(length(which(sub_02$BFA0b > 10.75))/nrow(sub_02)*100, 
                             length(which(10.75 >= sub_02$BFA0b & sub_02$BFA0b >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0b < 1/3))/nrow(sub_02)*100,
                             length(which(sub_02$BFA0c > 10.75))/nrow(sub_02)*100, 
                             length(which(10.75 >= sub_02$BFA0c & sub_02$BFA0c >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0c < 1/3))/nrow(sub_02)*100, nrow(sub_02))

sub_05 <- subset(overview, overview$effect == 0.5)
sum_up["effect = 0.5",] <- c(length(which(sub_05$BFA0b > 10.75))/nrow(sub_05)*100, 
                             length(which(10.75 >= sub_05$BFA0b & sub_05$BFA0b >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0b < 1/3))/nrow(sub_05)*100,
                             length(which(sub_05$BFA0c > 10.75))/nrow(sub_05)*100, 
                             length(which(10.75 >= sub_05$BFA0c & sub_05$BFA0c >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0c < 1/3))/nrow(sub_05)*100, nrow(sub_05))

write.csv(sum_up, "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/summary2.csv")

sum_up_sided <- data.frame(matrix(ncol = 7, nrow =10))
colnames(sum_up_sided) <- c("BFA0b_sided > 10.75 in %", "10.75 >= BFA0b_sided >= 1/3 in %", "BFA0b_sided < 1/3 in %", 
                            "BFA0c_sided > 10.75 in %", "10.75 >= BFA0c_sided >= 1/3 in %", "BFA0c_sided < 1/3 in %", "n")
rownames(sum_up_sided) <- c("overall", "model 1", "model 2", "HCV", "exfunct", "memo", "procspeed", "effect = 0.1",
                            "effect = 0.2", "effect = 0.5")
sum_up_sided["overall",] <- c(length(which(overview$BFA0b_sided > 10.75))/nrow(overview)*100, 
                        length(which(10.75 >= overview$BFA0b_sided & overview$BFA0b_sided >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0b_sided < 1/3))/nrow(overview)*100,
                        length(which(overview$BFA0c_sided > 10.75))/nrow(overview)*100, 
                        length(which(10.75 >= overview$BFA0c_sided & overview$BFA0c_sided >= 1/3))/nrow(overview)*100, 
                        length(which(overview$BFA0c_sided < 1/3))/nrow(overview)*100, nrow(overview))

sum_up_sided["model 1",] <- c(length(which(sub_m1$BFA0b_sided > 10.75))/nrow(sub_m1)*100, 
                        length(which(10.75 >= sub_m1$BFA0b_sided & sub_m1$BFA0b_sided >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0b_sided < 1/3))/nrow(sub_m1)*100,
                        length(which(sub_m1$BFA0c_sided > 10.75))/nrow(sub_m1)*100, 
                        length(which(10.75 >= sub_m1$BFA0c_sided & sub_m1$BFA0c_sided >= 1/3))/nrow(sub_m1)*100, 
                        length(which(sub_m1$BFA0c_sided < 1/3))/nrow(sub_m1)*100, nrow(sub_m1))

sum_up_sided["model 2",] <- c(length(which(sub_m2$BFA0b_sided > 10.75))/nrow(sub_m2)*100, 
                        length(which(10.75 >= sub_m2$BFA0b_sided & sub_m2$BFA0b_sided >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0b_sided < 1/3))/nrow(sub_m2)*100,
                        length(which(sub_m2$BFA0c_sided > 10.75))/nrow(sub_m2)*100, 
                        length(which(10.75 >= sub_m2$BFA0c_sided & sub_m2$BFA0c_sided >= 1/3))/nrow(sub_m2)*100, 
                        length(which(sub_m2$BFA0c_sided < 1/3))/nrow(sub_m2)*100, nrow(sub_m2))

sum_up_sided["HCV",] <- c(length(which(sub_hcv$BFA0b_sided > 10.75))/nrow(sub_hcv)*100, 
                    length(which(10.75 >= sub_hcv$BFA0b_sided & sub_hcv$BFA0b_sided >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0b_sided < 1/3))/nrow(sub_hcv)*100,
                    length(which(sub_hcv$BFA0c_sided > 10.75))/nrow(sub_hcv)*100, 
                    length(which(10.75 >= sub_hcv$BFA0c_sided & sub_hcv$BFA0c_sided >= 1/3))/nrow(sub_hcv)*100, 
                    length(which(sub_hcv$BFA0c_sided < 1/3))/nrow(sub_hcv)*100, nrow(sub_hcv))

sum_up_sided["exfunct",] <- c(length(which(sub_ef$BFA0b_sided > 10.75))/nrow(sub_ef)*100, 
                        length(which(10.75 >= sub_ef$BFA0b_sided & sub_ef$BFA0b_sided >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0b_sided < 1/3))/nrow(sub_ef)*100,
                        length(which(sub_ef$BFA0c_sided > 10.75))/nrow(sub_ef)*100, 
                        length(which(10.75 >= sub_ef$BFA0c_sided & sub_ef$BFA0c_sided >= 1/3))/nrow(sub_ef)*100, 
                        length(which(sub_ef$BFA0c_sided < 1/3))/nrow(sub_ef)*100, nrow(sub_ef))

sum_up_sided["memo",] <- c(length(which(sub_memo$BFA0b_sided > 10.75))/nrow(sub_memo)*100, 
                     length(which(10.75 >= sub_memo$BFA0b_sided & sub_memo$BFA0b_sided >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0b_sided < 1/3))/nrow(sub_memo)*100,
                     length(which(sub_memo$BFA0c_sided > 10.75))/nrow(sub_memo)*100, 
                     length(which(10.75 >= sub_memo$BFA0c_sided & sub_memo$BFA0c_sided >= 1/3))/nrow(sub_memo)*100, 
                     length(which(sub_memo$BFA0c_sided < 1/3))/nrow(sub_memo)*100, nrow(sub_memo))

sum_up_sided["procspeed",] <- c(length(which(sub_ps$BFA0b_sided > 10.75))/nrow(sub_ps)*100, 
                          length(which(10.75 >= sub_ps$BFA0b_sided & sub_ps$BFA0b_sided >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0b_sided < 1/3))/nrow(sub_ps)*100,
                          length(which(sub_ps$BFA0c_sided > 10.75))/nrow(sub_ps)*100, 
                          length(which(10.75 >= sub_ps$BFA0c_sided & sub_ps$BFA0c_sided >= 1/3))/nrow(sub_ps)*100, 
                          length(which(sub_ps$BFA0c_sided < 1/3))/nrow(sub_ps)*100, nrow(sub_ps))

sum_up_sided["effect = 0.1",] <- c(length(which(sub_01$BFA0b_sided > 10.75))/nrow(sub_01)*100, 
                             length(which(10.75 >= sub_01$BFA0b_sided & sub_01$BFA0b_sided >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0b_sided < 1/3))/nrow(sub_01)*100,
                             length(which(sub_01$BFA0c_sided > 10.75))/nrow(sub_01)*100, 
                             length(which(10.75 >= sub_01$BFA0c_sided & sub_01$BFA0c_sided >= 1/3))/nrow(sub_01)*100, 
                             length(which(sub_01$BFA0c_sided < 1/3))/nrow(sub_01)*100, nrow(sub_01))

sum_up_sided["effect = 0.2",] <- c(length(which(sub_02$BFA0b_sided > 10.75))/nrow(sub_02)*100, 
                             length(which(10.75 >= sub_02$BFA0b_sided & sub_02$BFA0b_sided >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0b_sided < 1/3))/nrow(sub_02)*100,
                             length(which(sub_02$BFA0c_sided > 10.75))/nrow(sub_02)*100, 
                             length(which(10.75 >= sub_02$BFA0c_sided & sub_02$BFA0c_sided >= 1/3))/nrow(sub_02)*100, 
                             length(which(sub_02$BFA0c_sided < 1/3))/nrow(sub_02)*100, nrow(sub_02))

sum_up_sided["effect = 0.5",] <- c(length(which(sub_05$BFA0b_sided > 10.75))/nrow(sub_05)*100, 
                             length(which(10.75 >= sub_05$BFA0b_sided & sub_05$BFA0b_sided >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0b_sided < 1/3))/nrow(sub_05)*100,
                             length(which(sub_05$BFA0c_sided > 10.75))/nrow(sub_05)*100, 
                             length(which(10.75 >= sub_05$BFA0c_sided & sub_05$BFA0c_sided >= 1/3))/nrow(sub_05)*100, 
                             length(which(sub_05$BFA0c_sided < 1/3))/nrow(sub_05)*100, nrow(sub_05))

write.csv(sum_up_sided, "/data/pt_life/ResearchProjects/LLammer/Analysis/Power_Simulation/summary2_sided.csv")
