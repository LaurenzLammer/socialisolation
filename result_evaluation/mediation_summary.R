setwd("/data/pt_life/ResearchProjects/LLammer/Results/mediation/")
coefficient_overview <- data.frame(matrix(ncol = 8, nrow = 8))
colnames(coefficient_overview) <- c("dv", "mediator", "model", "estimate", "standard error",
                                    "z-value",  "p_value", "n")
coefficient_overview$dv <- c(rep("HCV",2), rep("exfunct",2), rep("memo",2), rep("procspeed",2))
coefficient_overview$model <- c(1,2)
coefficient_overview$mediator <- c(rep("TICS",2), rep("HCV",6))
load("/data/pt_life/ResearchProjects/LLammer/Results/mediation/Workspace/workspace.RData")
coefficient_overview[1,4:7] <- fit311$sumfit$PE[45,6:9]
coefficient_overview[2,4:7] <- fit312$sumfit$PE[101,6:9]
coefficient_overview[3,4:7] <- fit411a$sumfit$PE[45,6:9]
coefficient_overview[4,4:7] <- fit412a$sumfit$PE[105,6:9]
coefficient_overview[5,4:7] <- fit411b$sumfit$PE[45,6:9]
coefficient_overview[6,4:7] <- fit412b$sumfit$PE[105,6:9]
coefficient_overview[7,4:7] <- fit411c$sumfit$PE[45,6:9]
coefficient_overview[8,4:7] <- fit412c$sumfit$PE[105,6:9]
n <- 0
for (fit in list(fit311, fit312, fit411a, fit412a, fit411b, fit412b, fit411c, fit412c)) {
  n <- n + 1
  coefficient_overview$n[n] <- nobs(fit$fit)
}
coefficient_overview$p_value <- ifelse(coefficient_overview$estimate < 0, coefficient_overview$p_value/2, 
                                       1-(coefficient_overview$p_value/2))
write.csv(coefficient_overview, "summary.csv", row.names = F)
