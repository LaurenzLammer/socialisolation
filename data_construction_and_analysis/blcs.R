library(lavaan)
library(tidyverse)
library(semPlot)

data <- read.csv("/data/pt_life/ResearchProjects/LLammer/Data/compiled_scaled_data.csv")
setwd("/data/pt_life/ResearchProjects/LLammer/Results_blcs")
df <- reshape(data, idvar = "subject", timevar = "fu", direction = "wide")

BLCS<-'

LSNS_sum.1 ~ 1*LSNS_sum.0     # This parameter regresses fu LSNS perfectly on bl LSNS
dLSNS1 =~ 1*LSNS_sum.1        # This defines the latent change score factor as measured perfectly by scores on fu LSNS
dLSNS1 ~ 1                    # This estimates the intercept of the change score 
LSNS_sum.0 ~  1               # This estimates the intercept of bl LSNS 
LSNS_sum.1 ~ 0*1              # This constrains the intercept of fu LSNS to 0

HCV.1 ~ 1*HCV.0     # This parameter regresses fu HCV perfectly on bl HCV
dHCV1 =~ 1*HCV.1     # This defines the latent change score factor as measured perfectly by scores on fu HCV
HCV.1 ~ 0*1          # This line constrains the intercept of fu HCV to 0
HCV.1 ~~ 0*HCV.1     # This fixes the variance of the fu HCV to 0  

dLSNS1 ~~  dLSNS1             # This estimates the variance of the change scores
LSNS_sum.0 ~~ LSNS_sum.0      # This estimates the variance of the bl LSNS
LSNS_sum.1 ~~ 0*LSNS_sum.1    # This fixes the variance of the fu LSNS to 0  

dHCV1 ~ 1             # This estimates the intercept of the change score 
HCV.0 ~ 1             # This estimates the intercept of bl HCV 
dHCV1 ~~ dHCV1        # This estimates the variance of the change scores 
HCV.0 ~~ HCV.0        # This estimates the variance of bl HCV 

dHCV1~LSNS_sum.0+HCV.0    # This estimates the LSNS to HCV coupling parameter and the HCV to HCV self-feedback
dLSNS1~HCV.0+LSNS_sum.0   # This estimates the HCV to LSNS coupling parameter and the LSNS to LSNS self-feedback

LSNS_sum.0 ~~  HCV.0     # This estimates the bl LSNS bl HCV covariance
dLSNS1~~dHCV1          # This estimates the dLSNS and dHCV covariance
'

fitBLCS <- lavaan(BLCS, data=df, estimator='mlr',fixed.x=FALSE)
sum_hcv <- summary(fitBLCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

BLCS2<-'

LSNS_sum.1 ~ 1*LSNS_sum.0     # This parameter regresses fu LSNS perfectly on bl LSNS
dLSNS1 =~ 1*LSNS_sum.1        # This defines the latent change score factor as measured perfectly by scores on fu LSNS
dLSNS1 ~ 1                    # This estimates the intercept of the change score 
LSNS_sum.0 ~  1               # This estimates the intercept of bl LSNS 
LSNS_sum.1 ~ 0*1              # This constrains the intercept of fu LSNS to 0

exfunct.1 ~ 1*exfunct.0     # This parameter regresses fu exfunct perfectly on bl exfunct
dexfunct1 =~ 1*exfunct.1     # This defines the latent change score factor as measured perfectly by scores on fu exfunct
exfunct.1 ~ 0*1          # This line constrains the intercept of fu exfunct to 0
exfunct.1 ~~ 0*exfunct.1     # This fixes the variance of the fu exfunct to 0  

dLSNS1 ~~  dLSNS1             # This estimates the variance of the change scores
LSNS_sum.0 ~~ LSNS_sum.0      # This estimates the variance of the bl LSNS
LSNS_sum.1 ~~ 0*LSNS_sum.1    # This fixes the variance of the fu LSNS to 0  

dexfunct1 ~ 1             # This estimates the intercept of the change score 
exfunct.0 ~ 1             # This estimates the intercept of bl exfunct 
dexfunct1 ~~ dexfunct1        # This estimates the variance of the change scores 
exfunct.0 ~~ exfunct.0        # This estimates the variance of bl exfunct 

dexfunct1~LSNS_sum.0+exfunct.0    # This estimates the LSNS to HCV coupling parameter and the HCV to HCV self-feedback
dLSNS1~exfunct.0+LSNS_sum.0   # This estimates the HCV to LSNS coupling parameter and the LSNS to LSNS self-feedback

LSNS_sum.0 ~~  exfunct.0     # This estimates the bl LSNS bl HCV covariance
dLSNS1~~dexfunct1          # This estimates the dLSNS and dHCV covariance
'

fitBLCS2 <- lavaan(BLCS2, data=df, estimator='mlr',fixed.x=FALSE)
sum_exfunct <- summary(fitBLCS2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

BLCS3<-'

LSNS_sum.1 ~ 1*LSNS_sum.0     # This parameter regresses fu LSNS perfectly on bl LSNS
dLSNS1 =~ 1*LSNS_sum.1        # This defines the latent change score factor as measured perfectly by scores on fu LSNS
dLSNS1 ~ 1                    # This estimates the intercept of the change score 
LSNS_sum.0 ~  1               # This estimates the intercept of bl LSNS 
LSNS_sum.1 ~ 0*1              # This constrains the intercept of fu LSNS to 0

memo.1 ~ 1*memo.0     # This parameter regresses fu memo perfectly on bl memo
dmemo1 =~ 1*memo.1     # This defines the latent change score factor as measured perfectly by scores on fu memo
memo.1 ~ 0*1          # This line constrains the intercept of fu memo to 0
memo.1 ~~ 0*memo.1     # This fixes the variance of the fu memo to 0  

dLSNS1 ~~  dLSNS1             # This estimates the variance of the change scores
LSNS_sum.0 ~~ LSNS_sum.0      # This estimates the variance of the bl LSNS
LSNS_sum.1 ~~ 0*LSNS_sum.1    # This fixes the variance of the fu LSNS to 0  

dmemo1 ~ 1             # This estimates the intercept of the change score 
memo.0 ~ 1             # This estimates the intercept of bl memo 
dmemo1 ~~ dmemo1        # This estimates the variance of the change scores 
memo.0 ~~ memo.0        # This estimates the variance of bl memo 

dmemo1~LSNS_sum.0+memo.0    # This estimates the LSNS to memo coupling parameter and the memo to memo self-feedback
dLSNS1~memo.0+LSNS_sum.0   # This estimates the memo to LSNS coupling parameter and the LSNS to LSNS self-feedback

LSNS_sum.0 ~~  memo.0     # This estimates the bl LSNS bl memo covariance
dLSNS1~~dmemo1          # This estimates the dLSNS and dmemo covariance
'

fitBLCS3 <- lavaan(BLCS3, data=df, estimator='mlr',fixed.x=FALSE)
sum_memo <- summary(fitBLCS3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

BLCS4<-'

LSNS_sum.1 ~ 1*LSNS_sum.0     # This parameter regresses fu LSNS perfectly on bl LSNS
dLSNS1 =~ 1*LSNS_sum.1        # This defines the latent change score factor as measured perfectly by scores on fu LSNS
dLSNS1 ~ 1                    # This estimates the intercept of the change score 
LSNS_sum.0 ~  1               # This estimates the intercept of bl LSNS 
LSNS_sum.1 ~ 0*1              # This constrains the intercept of fu LSNS to 0

procspeed.1 ~ 1*procspeed.0     # This parameter regresses fu procspeed perfectly on bl procspeed
dprocspeed1 =~ 1*procspeed.1     # This defines the latent change score factor as measured perfectly by scores on fu procspeed
procspeed.1 ~ 0*1          # This line constrains the intercept of fu procspeed to 0
procspeed.1 ~~ 0*procspeed.1     # This fixes the variance of the fu procspeed to 0  

dLSNS1 ~~  dLSNS1             # This estimates the variance of the change scores
LSNS_sum.0 ~~ LSNS_sum.0      # This estimates the variance of the bl LSNS
LSNS_sum.1 ~~ 0*LSNS_sum.1    # This fixes the variance of the fu LSNS to 0  

dprocspeed1 ~ 1             # This estimates the intercept of the change score 
procspeed.0 ~ 1             # This estimates the intercept of bl procspeed 
dprocspeed1 ~~ dprocspeed1        # This estimates the variance of the change scores 
procspeed.0 ~~ procspeed.0        # This estimates the variance of bl procspeed 

dprocspeed1~LSNS_sum.0+procspeed.0    # This estimates the LSNS to procspeed coupling parameter and the procspeed to procspeed self-feedback
dLSNS1~procspeed.0+LSNS_sum.0   # This estimates the procspeed to LSNS coupling parameter and the LSNS to LSNS self-feedback

LSNS_sum.0 ~~  procspeed.0     # This estimates the bl LSNS bl procspeed covariance
dLSNS1~~dprocspeed1          # This estimates the dLSNS and dprocspeed covariance
'

fitBLCS4 <- lavaan(BLCS4, data=df, estimator='mlr',fixed.x=FALSE)
sum_procspeed <- summary(fitBLCS4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

tiff("semplot.tff", width = 480*1.62, height = 480, res = 300)
semPaths(fitBLCS, layout = "circle2", intercepts = F, residuals = F ,sizeMan = 12, sizeLat = 12, nCharNodes = 0, color = "white", 
         edge.color = c("black", "black","black","black","blue","black","blue","black","black","black"), 
         nodeLabels = c("HCV_FU", "LSNS_FU", "HCV_BL", "LSNS_BL", 
                        expression(paste(Delta, "HCV")), expression(paste(Delta, "LSNS"))),
         sizeMan2 = 12, sizeLat2 = 12, as.expression = "nodes")
dev.off()
save.image("blcs.RData")