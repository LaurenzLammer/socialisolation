library(BiocManager)
library(qvalue)
library(lavaan)

setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results_blcs")
load(file = "blcs_fiml.RData")
overview <- data.frame(matrix(nrow = 8, ncol = 10))
colnames(overview) <- c("dv", "predictor", "estimate", "se", "z-value", "p.value", "sided_p.value", "q_value", "significant", "n")
n <- 1
for(sum in list(sum_hcv$pe, sum_exfunct$pe, sum_memo$pe, sum_procspeed$pe)){
  overview[n,c(1:6,10)] <- sum[17,c(1,3,5:8)]
  overview[n+1,c(1:6,10)] <- sum[19,c(1,3,5:8)]
  n <- n + 2
}
n <- 1
for(fit in list(fitBLCS, fitBLCS2, fitBLCS3, fitBLCS4)){
  overview[n,"n"] <- lavInspect(fit, what = "nobs")
  overview[n+1,"n"] <- lavInspect(fit, what = "nobs")
  n <- n +2
}
overview$sided_p.value <- ifelse(overview$estimate < 0, overview$p.value/2, 1 - overview$p.value/2)
lsns_ps <- overview[c(1,3,5,7), "sided_p.value"]
reverse_ps <- overview[c(2,4,6,8), "sided_p.value"]
lsns_q <- qvalue(p = lsns_ps, fdr.level = 0.05, pi0 = 1)
reverse_q <- qvalue(p = reverse_ps, fdr.level = 0.05, pi0 = 1)
overview[c(1,3,5,7), "q_value"] <- lsns_q$qvalues
overview[c(1,3,5,7), "significant"] <- lsns_q$significant
overview[c(2,4,6,8), "q_value"] <- reverse_q$qvalues
overview[c(2,4,6,8), "significant"] <- reverse_q$significant
write.csv(overview, "overview_fiml.csv", row.names = F)
