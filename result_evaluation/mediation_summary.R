setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/")
library(lavaan)

# create oveview of preregistered analyses
coefficient_overview <- data.frame(matrix(ncol = 8, nrow = 8))
colnames(coefficient_overview) <- c("dv", "mediator", "model", "estimate", "standard error",
                                    "z-value",  "p_value", "n")
coefficient_overview$dv <- c(rep("HCV",2), rep("exfunct",2), rep("memo",2), rep("procspeed",2))
coefficient_overview$model <- c(1,2)
coefficient_overview$mediator <- c(rep("TICS",2), rep("HCV",6))
load("/data/pt_life/ResearchProjects/LLammer/si_update/Results_mediation/Workspace/workspace_fiml.RData")
coefficient_overview[1,4:7] <- fimlfit311$sumfit$pe[54,6:9]
coefficient_overview[2,4:7] <- fimlfit312$sumfit$pe[115,6:9]
coefficient_overview[3,4:7] <- fimlfit411a$sumfit$pe[54,6:9]
coefficient_overview[4,4:7] <- fimlfit412a$sumfit$pe[119,6:9]
coefficient_overview[5,4:7] <- fimlfit411b$sumfit$pe[54,6:9]
coefficient_overview[6,4:7] <- fimlfit412b$sumfit$pe[119,6:9]
coefficient_overview[7,4:7] <- fimlfit411c$sumfit$pe[54,6:9]
coefficient_overview[8,4:7] <- fimlfit412c$sumfit$pe[119,6:9]
n <- 0
for (fit in list(fimlfit311, fimlfit312, fimlfit411a, fimlfit412a, fimlfit411b, fimlfit412b, fimlfit411c, fimlfit412c)) {
  n <- n + 1
  coefficient_overview$n[n] <- nobs(fit$fit)
}
coefficient_overview$p_value <- ifelse(coefficient_overview$estimate < 0, coefficient_overview$p_value/2, 
                                       1-(coefficient_overview$p_value/2))

write.csv(coefficient_overview, "summary.csv", row.names = F)

# create overview of psqi analyses
coefficient_overview <- data.frame(matrix(ncol = 8, nrow = 8))
colnames(coefficient_overview) <- c("dv", "mediator", "model", "estimate", "standard error",
                                    "z-value",  "p_value", "n")
coefficient_overview$dv <- c(rep("HCV",2), rep("exfunct",2), rep("memo",2), rep("procspeed",2))
coefficient_overview$model <- c(1,2)
coefficient_overview$mediator <- "psqi"
coefficient_overview[1,4:7] <- fimlfitpsqihcv1$sumfit$pe[54,6:9]
coefficient_overview[2,4:7] <- fimlfitpsqihcv2$sumfit$pe[115,6:9]
coefficient_overview[3,4:7] <- fimlfitpsqief1$sumfit$pe[54,6:9]
coefficient_overview[4,4:7] <- fimlfitpsqief2$sumfit$pe[115,6:9]
coefficient_overview[5,4:7] <- fimlfitpsqimemo1$sumfit$pe[54,6:9]
coefficient_overview[6,4:7] <- fimlfitpsqimemo2$sumfit$pe[115,6:9]
coefficient_overview[7,4:7] <- fimlfitpsqips1$sumfit$pe[54,6:9]
coefficient_overview[8,4:7] <- fimlfitpsqips2$sumfit$pe[115,6:9]
n <- 0
for (fit in list(fimlfitpsqihcv1, fimlfitpsqihcv2, fimlfitpsqief1, fimlfitpsqief2, fimlfitpsqimemo1, fimlfitpsqimemo2, fimlfitpsqips1, fimlfitpsqips2)) {
  n <- n + 1
  coefficient_overview$n[n] <- nobs(fit$fit)
}
coefficient_overview$p_value <- ifelse(coefficient_overview$estimate < 0, coefficient_overview$p_value/2, 
                                       1-(coefficient_overview$p_value/2))

write.csv(coefficient_overview, "summary_psqi.csv", row.names = F)
