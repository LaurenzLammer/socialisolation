setwd("/data/pt_life/ResearchProjects/LLammer/Analysis/Simulation/")
df <- data.frame(matrix(nrow = 100, ncol = 3))
colnames(df) <- c("n", "FWER_freq", "FWER_bayes")
df$n <- 1:100
df$FWER_freq <- 1-0.95^df$n
df$FWER_bayes <- 1-0.97^df$n
tiff("t_test_fwer.tiff", width = 5.2, height = 3.25, units = "in", res = 600)
plot(df$n, df$FWER_freq, type = "l", col = "red", yaxs = "i", xaxs = "i", xlab = 
       "number of tests", 
     ylab = "FWER", cex.lab = 1.25, sub = "with a sample size of 15 and an effect size of 0")

lines(df$n, df$FWER_bayes, col = "blue")
for(n in c(0.2,0.4,0.6,0.8)){
  abline(h = n, lty = "dashed", col = "grey")
}       
for(n in c(10,20,30,40,50,60,70,80,90)){
  abline(v = n, lty = "dashed", col = "grey")
}       
legend(x = 60, y = 0.5, legend = c("Bayesian", "Frequentist"), col = c("blue", "red"),
       lty = c(1,1), bg = "white", cex = 1)
dev.off()
