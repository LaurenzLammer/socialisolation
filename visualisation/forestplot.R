# load data and split it into subsets for each dependent variable and model
df <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Results/coefficients/overview_full.csv")
coeff_HCV <- subset(df, df$dv == "HCV" & df$model == 1)
coeff_exfunct <- subset(df, df$dv == "exfunct" & df$model == 1)
coeff_memo <- subset(df, df$dv == "memo" & df$model == 1)
coeff_procspeed <- subset(df, df$dv == "procspeed" & df$model == 1)
coeff_HCV2 <- subset(df, df$dv == "HCV" & df$model == 2)
coeff_exfunct2 <- subset(df, df$dv == "exfunct" & df$model == 2)
coeff_memo2 <- subset(df, df$dv == "memo" & df$model == 2)
coeff_procspeed2 <- subset(df, df$dv == "procspeed" & df$model == 2)
#some ugly post-hoc standardisation
data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
sds <- vector(length = 5, mode = "numeric")
datax <- subset(data, data$outlier_HCV != 1)
sd_hcv <- sd(datax$HCV)
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5] <- 1
for(n in 1:5){
  coeff_HCV[n,c("estimate", "X2.5.", "X97.5.")] <- ((coeff_HCV[n,c("estimate", "X2.5.", "X97.5.")])*sds[n])/sd_hcv
}
sds <- vector(length = 10, mode = "numeric")
datax <- subset(datax, !is.na(datax$CES.D))
sd_hcv <- sd(datax$HCV)
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5:10] <- 1
for(n in 1:10){
  coeff_HCV2[n,c("estimate", "X2.5.", "X97.5.")] <- ((coeff_HCV2[n,c("estimate", "X2.5.", "X97.5.")])*sds[n])/sd_hcv
}
sds <- vector(length = 5, mode = "numeric")
datax <- subset(data, data$outlier_exfunct != 1)
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5] <- 1
for(n in 1:5){
  coeff_exfunct[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_exfunct[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
sds <- vector(length = 10, mode = "numeric")
datax <- subset(datax, !is.na(datax$CES.D))
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5:10] <- 1
for(n in 1:10){
  coeff_exfunct2[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_exfunct2[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
sds <- vector(length = 5, mode = "numeric")
datax <- subset(data, data$outlier_memo != 1)
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5] <- 1
for(n in 1:5){
  coeff_memo[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_memo[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
sds <- vector(length = 10, mode = "numeric")
datax <- subset(datax, !is.na(datax$CES.D))
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5:10] <- 1
for(n in 1:10){
  coeff_memo2[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_memo2[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
sds <- vector(length = 5, mode = "numeric")
datax <- subset(data, data$outlier_procspeed != 1)
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5] <- 1
for(n in 1:5){
  coeff_procspeed[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_procspeed[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
sds <- vector(length = 10, mode = "numeric")
datax <- subset(datax, !is.na(datax$CES.D))
sds[1] <- sd(datax$LSNS_base)
sds[2] <- sd(datax$LSNS_change)
sds[3] <- sd(datax$age_base)
sds[4] <- sd(datax$age_change)
sds[5:10] <- 1
for(n in 1:10){
  coeff_procspeed2[n,c("estimate", "X2.5.", "X97.5.")] <- coeff_procspeed2[n,c("estimate", "X2.5.", "X97.5.")]*sds[n]
}
coeff_HCV <- coeff_HCV[c(1,3,2,4,5),]
coeff_exfunct <- coeff_exfunct[c(1,3,2,4,5),]
coeff_memo <- coeff_memo[c(1,3,2,4,5),]
coeff_procspeed <- coeff_procspeed[c(1,3,2,4,5),]
tiff(file = "/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/forestplot.tiff", width = 8.5, height = 4,
     units = "in", res = 600)
par(mar=c(4.5,11,2,1)+.1)
# create empty plot
plot(0, xlim = c(-0.5, 0.15), ylim = c(0.75, 5.25), xlab = "", main = "", 
     yaxt = "n", ylab ="", cex.main = 1.8, cex.axis = 1.4)
axis(2, at = c(1,2,3,4,5), labels = c("\u2642 vs. \u2640", "age_change","LSNS_change", "age_base", "LSNS_base"), 
     las = 1, cex.axis =1.5)
title(xlab = "Corrected Standardised Effect Sizes",line = 3, cex.lab = 1.25)
title(main = "Forest plot of predictors' effect sizes in model 1", cex = 1.25)
# add grid
for (n in c(1.5, 2.5, 3.5, 4.5)) {
  abline(h = n, lwd = 0.2)
}
for (n in c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1)) {
  abline(v = n, lty = "dashed", col = rgb(0.502, 0.502, 0.502, alpha = 0.5), lwd = 0.15)
}
abline(v = 0, lwd = 0.4)
# add effect sizes of the different predictors
for (n in 1:5) {
  points(x = coeff_HCV$estimate[n], y = 6-n + 0.2, pch = 21, col = "#a6cee3", bg = "#a6cee3", cex = 1.8)
  points(x = coeff_exfunct$estimate[n], y = 6-n + 0.0666, pch = 22, col = "#1f78b4", bg = "#1f78b4", cex = 1.8)
  points(x = coeff_memo$estimate[n], y = 6-n - 0.0666, pch = 23, col = "#b2df8a", bg = "#b2df8a", cex = 1.8)
  points(x = coeff_procspeed$estimate[n], y = 6-n - 0.2, pch = 24, col = "#33a02c", bg = "#33a02c", cex = 1.8)
}
# add confidence intervals of the effect sizes
for (n in 1:5) {
  segments(x0 = coeff_HCV$X2.5.[n], y0 = 6-n + 0.2, x1 = coeff_HCV$X97.5.[n], y1 = 6-n+ 0.2, col = "#a6cee3", lwd = 3.2)
  segments(x0 = coeff_exfunct$X2.5.[n], y0 = 6-n + 0.0666, x1 = coeff_exfunct$X97.5.[n], y1 = 6-n+ 0.0666, col = "#1f78b4", lwd = 3.2)
  segments(x0 = coeff_memo$X2.5.[n], y0 = 6-n - 0.0666, x1 = coeff_memo$X97.5.[n], y1 = 6-n - 0.0666, col = "#b2df8a", lwd = 3.2)
  segments(x0 = coeff_procspeed$X2.5.[n], y0 = 6-n - 0.2, x1 = coeff_procspeed$X97.5.[n], y1 = 6-n - 0.2, col = "#33a02c", lwd = 3.2)
}
legend(x = -0.5, y = 3.5, c("Hippocampal Volume", "Executive Function", "Memory", "Processing Speed"), 
       col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"), y.intersp = 0.8, x.intersp = 0.6, bg = "white", box.lwd = 1.5,
       pch = c(21,22,23,24), pt.bg = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"), lwd=3, pt.cex = 1, cex = 0.6)
dev.off()

coeff_HCV2 <- coeff_HCV2[c(1,3,2,4,5:10),]
coeff_exfunct2 <- coeff_exfunct2[c(1,3,2,4,5:10),]
coeff_memo2 <- coeff_memo2[c(1,3,2,4,5:10),]
coeff_procspeed2 <- coeff_procspeed2[c(1,3,2,4,5:10),]
tiff(file = "/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/forestplot2.tiff", width = 8.5, height = 4,
     units = "in", res = 300)
par(mar=c(4.5,11,2,1)+.1)
plot(0, xlim = c(-0.5, 0.15), ylim = c(0.75, 10.25), xlab = "", main = "", 
     yaxt = "n", ylab ="", cex.main = 1.8, cex.axis = 1.4)
axis(2, at = c(1:10), labels = c( "Hypertension", "Education", "Diabetes", "CESD", "BMI", "\u2642 vs. \u2640", "age_change","LSNS_change", "age_base", "LSNS_base"), 
     las = 1, cex.axis =1.5)
title(xlab = "Corrected Standardised Effect Sizes",line = 3, cex.lab = 1.25)
title(main = "Forest plot of predictors' effect sizes in model 2", cex = 1.25)
for (n in 0.5+1:9) {
  abline(h = n, lwd = 0.2)
}
for (n in c(-0.5, -0.4, -0.3, -0.2, -0.1, 0.1)) {
  abline(v = n, lty = "dashed", col = rgb(0.502, 0.502, 0.502, alpha = 0.25), lwd = 0.15)
}
abline(v = 0, lwd = 0.4)
for (n in 1:10) {
  points(x = coeff_HCV2$estimate[n], y = 11-n + 0.2, pch = 21, col = "#a6cee3", bg = "#a6cee3", cex = 1.2)
  points(x = coeff_exfunct2$estimate[n], y = 11-n + 0.0666, pch = 22, col = "#1f78b4", bg = "#1f78b4", cex = 1.2)
  points(x = coeff_memo2$estimate[n], y = 11-n - 0.0666, pch = 23, col = "#b2df8a", bg = "#b2df8a", cex = 1.2)
  points(x = coeff_procspeed2$estimate[n], y = 11-n - 0.2, pch = 24, col = "#33a02c", bg = "#33a02c", cex = 1.2)
}
for (n in 1:10) {
  segments(x0 = coeff_HCV2$X2.5.[n], y0 = 11-n + 0.2, x1 = coeff_HCV2$X97.5.[n], y1 = 11-n+ 0.2, col = "#a6cee3", lwd = 2)
  segments(x0 = coeff_exfunct2$X2.5.[n], y0 = 11-n + 0.0666, x1 = coeff_exfunct2$X97.5.[n], y1 = 11-n+ 0.0666, col = "#1f78b4", lwd = 2)
  segments(x0 = coeff_memo2$X2.5.[n], y0 = 11-n - 0.0666, x1 = coeff_memo2$X97.5.[n], y1 = 11-n - 0.0666, col = "#b2df8a", lwd = 2)
  segments(x0 = coeff_procspeed2$X2.5.[n], y0 = 11-n - 0.2, x1 = coeff_procspeed2$X97.5.[n], y1 = 11-n - 0.2, col = "#33a02c", lwd = 2)
}
legend(x = -0.5, y = 5.5, c("Hippocampal Volume", "Executive Function", "Memory", "Processing Speed"), 
       col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"), y.intersp = 0.8, x.intersp = 0.6, bg = "white", box.lwd = 1.5,
       pch = c(21,22,23,24), pt.bg = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"), lwd=3, pt.cex = 1, cex = 0.6)
dev.off()

