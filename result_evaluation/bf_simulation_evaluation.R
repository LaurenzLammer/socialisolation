library(ggplot2)
library(tidyverse)

setwd("/data/pt_life/ResearchProjects/LLammer/Analysis/Simulation/") 
summary <- data.frame(matrix(ncol = 5, nrow = length(dirs)*24))
colnames(summary) <- c("bf", "bf_sided", "poi", "model", "batch")
summary$model <- c(1,2)
n <- 0
for (dir in dirs) {
  if (file.exists(paste0(dir, "/workspace2.RData"))) {
    load(paste0(dir, "/workspace2.RData"))
    for (sim in list(bf1a, bf1b, bf2a, bf2b, bf3a, bf3b, bf4a, bf4b, bf5a, bf5b, bf6a, bf6b, bf7a, bf7b, 
                     bf8a, bf8b, bf9a, bf9b, bf10a, bf10b, bf11a, bf11b, bf12a, bf12b)) {
      n <- n + 1
      summary$batch[n] <- 2
      bf_nolog <- extractBF(sim$bf, logbf = F)
      bf_nolog["full_v_null",1] <- bf_nolog[1,1] / bf_nolog[2,1]
      if (sum(sapply(list(bf1a, bf1b, bf4a, bf4b, bf7a, bf7b, bf10a, bf10b), FUN = identical, sim)) > 0){
        bf_nolog["full_v_null_sided",1] <- bf_nolog["full_v_null",1] * 2 * mean(sim$chains[,"LSNS_base"]<0)
        summary$poi[n] <- "LSNS_base"
      } else if (sum(sapply(list(bf2a, bf2b, bf5a, bf5b, bf8a, bf8b, bf11a, bf11b), FUN = identical, sim)) > 0) {
        bf_nolog["full_v_null_sided",1] <- bf_nolog["full_v_null",1] * 2 * mean(sim$chains[,"LSNS_change"]<0)
        summary$poi[n] <- "LSNS_change"
      } else {
        bf_nolog["full_v_null_sided",1] <- bf_nolog["full_v_null",1] * 2 * mean(sim$chains[,"LSNS_base.&.age_change"]<0)
        summary$poi[n] <- "LSNS_base*age_change"
      }
      summary$bf[n] <- bf_nolog["full_v_null",1]
      summary$bf_sided[n] <- bf_nolog["full_v_null_sided",1]
    }
  } else {
    load(paste0(dir, "/workspace.RData"))
    for (sim in list(bf1a, bf1b, bf2a, bf2b, bf3a, bf3b, bf4a, bf4b, bf5a, bf5b, bf6a, bf6b, bf7a, bf7b, 
                     bf8a, bf8b, bf9a, bf9b, bf10a, bf10b, bf11a, bf11b, bf12a, bf12b)) {
      n <- n + 1
      summary$batch[n] <- 1
      if (sum(sapply(list(bf1a, bf1b, bf4a, bf4b, bf7a, bf7b, bf10a, bf10b), FUN = identical, sim)) > 0){
        summary$poi[n] <- "LSNS_base"
      } else if (sum(sapply(list(bf2a, bf2b, bf5a, bf5b, bf8a, bf8b, bf11a, bf11b), FUN = identical, sim)) > 0) {
        summary$poi[n] <- "LSNS_change"
      } else {
        summary$poi[n] <- "LSNS_base*age_change"
      }
      summary$bf[n] <- sim[3,1]
      summary$bf_sided[n] <- sim[4,1]
    }
  }
} 
write.csv(summary, "summary.csv", row.names = F)
ggplot(summary, aes(x = bf)) + geom_histogram() + scale_x_log10()
length(which(summary$bf>3))
ggplot(summary, aes(x = bf_sided)) + geom_histogram() + scale_x_log10() +
  geom_vline(xintercept = 3, linetype="dashed", color = "red", size=0.5) +
  geom_vline(xintercept = 1/3, linetype="dashed", color = "red", size=0.5) +
  xlab("sided BFA0")
ggsave("bf_sided_hist.tiff", dpi = 600)
over_thresh <- subset(summary, summary$bf_sided>3)
over_thresh <- over_thresh %>% arrange(desc(bf_sided))
for (n in 1:nrow(over_thresh)){
  over_thresh$FWER[n] <- 1-((1-(n/nrow(summary)))^12)
}
write.csv(over_thresh, "over_thresh.csv", row.names = F)
