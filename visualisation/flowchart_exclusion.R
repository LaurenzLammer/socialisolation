library(tidyverse)
library(lme4)
library(DiagrammeR)

setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results/descriptive_stats/")
df <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_full_data.csv")
df <- subset(df, !is.na(df$LSNS_sum)) 
length1 <- nrow(df)
length1_fu <- length(which(df$fu == 1))
df <- subset(df, df$excludelesion == 0 & df$excludetumor == 0) 
length2 <- nrow(df)
length2_fu <- length(which(df$fu == 1))
df <- subset(df, df$stroke == 0 | is.na(df$stroke))
length3 <- nrow(df)
length3_fu <- length(which(df$fu == 1))
df <- subset(df, df$cancer == 0)
length4 <- nrow(df)
length4_fu <- length(which(df$fu == 1))
df <- subset(df, (is.na(df$epilepsy) | df$epilepsy != 1) & (is.na(df$MS) | df$MS != 1) & 
               (is.na(df$parkinson) | df$parkinson != 1)) 
length5 <- nrow(df)
length5_fu <- length(which(df$fu == 1))
df <- subset(df, (is.na(df$dementia) | df$dementia != 1) & (df$MMST_MMST >= 24 | (is.na(df$MMST_MMST) & (df$outlier_CERAD == 0)))) 
length6 <- nrow(df)
length6_fu <- length(which(df$fu == 1))
df <- subset(df, df$centr_act_med == 0) 
length7 <- nrow(df)
length7_fu <- length(which(df$fu == 1))

df$CESD_sum <- log(df$CESD_sum+1)


# turn CESD_sum outliers by 4SD into NAs
df$CESD_sum <- ifelse(diff(c(df$CESD_sum, mean(df$CESD_sum, na.rm = T))) >= 
                        4*sd(df$CESD_sum, na.rm = T), NA, df$CESD_sum)


# mark outliers (by 3SD, wave-specific) for LSNS and cognitive functions
df$outlier_LSNS <- 0
df$outlier_exfunct <- 0
df$outlier_memo <- 0
df$outlier_procspeed <- 0
df[df$fu == 0,] <- df[df$fu == 0,] %>%
  mutate(outlier_LSNS = case_when(
    diff(c(LSNS_sum, mean(LSNS_sum, na.rm = T))) >= 3*sd(LSNS_sum, na.rm = T) ~ 1,
    is.na(LSNS_sum) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(outlier_exfunct = case_when(
    diff(c(exfunct, mean(exfunct, na.rm = T))) >= 3*sd(exfunct, na.rm = T) ~ 1,
    is.na(exfunct) ~ NA_real_,
    TRUE ~ 0)) %>%  
  mutate(outlier_memo = case_when(
    diff(c(memo, mean(memo, na.rm = T))) >= 3*sd(memo, na.rm = T) ~ 1,
    is.na(memo) ~ NA_real_,
    TRUE ~ 0)) %>% 
  mutate(outlier_procspeed = case_when(
    diff(c(procspeed, mean(procspeed, na.rm = T))) >= 3*sd(procspeed, na.rm = T) ~ 1,
    is.na(procspeed) ~ NA_real_,
    TRUE ~ 0)) 

df[df$fu == 1, ] <- df[df$fu == 1,] %>%
  mutate(outlier_LSNS = case_when(
    diff(c(LSNS_sum, mean(LSNS_sum, na.rm = T))) >= 3*sd(LSNS_sum, na.rm = T) ~ 1,
    is.na(LSNS_sum) ~ NA_real_,
    TRUE ~ 0)) %>%
  mutate(outlier_exfunct = case_when(
    diff(c(exfunct, mean(exfunct, na.rm = T))) >= 3*sd(exfunct, na.rm = T) ~ 1,
    is.na(exfunct) ~ NA_real_,
    TRUE ~ 0)) %>%  
  mutate(outlier_memo = case_when(
    diff(c(memo, mean(memo, na.rm = T))) >= 3*sd(memo, na.rm = T) ~ 1,
    is.na(memo) ~ NA_real_,
    TRUE ~ 0)) %>% 
  mutate(outlier_procspeed = case_when(
    diff(c(procspeed, mean(procspeed, na.rm = T))) >= 3*sd(procspeed, na.rm = T) ~ 1,
    is.na(procspeed) ~ NA_real_,
    TRUE ~ 0))

# exclude LSNS outliers
df <- subset(df, df$outlier_LSNS != 1) 
length8 <- nrow(df)
length8_fu <- length(which(df$fu == 1))

mean_TIV <- mean(df$EstimatedTotalIntraCranialVol, na.rm = T)
res <- lmer(mean_HCV ~ EstimatedTotalIntraCranialVol + (1|SIC), data = df, REML = F, na.action = na.omit)
sres <- summary(res)
beta <- sres$coefficients[2]
df$HCV_adj <- df$mean_HCV - beta * (df$EstimatedTotalIntraCranialVol - mean_TIV)

# mark HCV outliers
df$outlier_HCV <- 0
df[df$fu == 0,] <- df[df$fu == 0,] %>%
  mutate(outlier_HCV = case_when(
    diff(c(HCV_adj, mean(HCV_adj, na.rm = T))) >= 3*sd(HCV_adj, na.rm = T) ~ 1,
    is.na(HCV_adj) ~ NA_real_,
    TRUE ~ 0))
df[df$fu == 1,] <- df[df$fu == 1,] %>%
  mutate(outlier_HCV = case_when(
    diff(c(HCV_adj, mean(HCV_adj, na.rm = T))) >= 3*sd(HCV_adj, na.rm = T) ~ 1,
    is.na(HCV_adj) ~ NA_real_,
    TRUE ~ 0))

grViz("digraph flowchart {
node [shape = rectangle, width = 8, fillcolor = Antiquewhite, style = filled, fontsize = 30]
a [label = '@@1', width = 8, height = 1.25]
b [label = '@@2']
h [label = '@@3']
i [label = '@@4']
j [label = '@@5']
k [label = '@@6']
l [label = '@@7']
m [label = '@@8']
n [label = '@@9']
o [label = '@@10']
p [label = '@@11']
q [label = '@@12']
r [label = '@@13']
s [label = '@@14']
t [label = '@@15']

node [shape = point, width = 0, height = 0]
''

{rank=same; b;''}
a -> '' [dir = none];
'' -> h;
h -> i -> j -> k;
h -> l -> m -> n;
h -> o -> p -> q; 
h -> r -> s -> t;
'' -> b [dir = back, label = '   stepwise exclusion   ', fontsize = 30]
}

[1]: paste(paste0('MRI subsample, >49a, with LSNS (n = ', length1, ')'), paste0('BL = ', length1 - length1_fu, '  |  ', 'FU = ', length1_fu), sep = '\\n')
[2]: paste(paste0('lesions and tumors (n = ', length1 - length2, ')'), paste0('BL = ', (length1 - length1_fu) - (length2 - length2_fu), '  |  ', 'FU = ', length1_fu - length2_fu), paste0('history of stroke (n = ', length2 -length3, ')'), paste0('BL = ', (length2 - length2_fu) - (length3 - length3_fu), '  | ', 'FU = ', length2_fu - length3_fu), paste0('recent cancer treatment (n = ', length3 - length4, ')'), paste0('BL = ', (length3 - length3_fu) - (length4 - length4_fu), '  | ', 'FU = ', length3_fu - length4_fu), paste0('neurological diseases (n = ', length4 -length5, ')'), paste0('BL = ', (length4 - length4_fu) - (length5 - length5_fu), '  | ', 'FU = ', length4_fu - length5_fu), paste0('cognitive impairement (n = ', length5 - length6, ')'), paste0('BL = ', (length5 - length5_fu) - (length6 - length6_fu), '  | ', 'FU = ', length5_fu - length6_fu), paste0('centrally active medication (n = ', length6 - length7, ')'), paste0('BL = ', (length6 - length6_fu) - (length7 - length7_fu), '  | ', 'FU = ', length6_fu - length7_fu), paste0('LSNS outlier (n = ', length7 - length8, ')'), paste0('BL = ', (length7 - length7_fu) - (length8 - length8_fu), '  | ', 'FU = ', length7_fu - length8_fu), sep = '\\n')
[3]: paste(paste0('eligble participants (n = ', length8, ')'), paste0('BL = ', length8 - length8_fu, '  |  ', 'FU = ', length8_fu), sep = '\\n')
[4]: paste(paste0('usable MRI (n = ', length(which(df$FS_usable == 1)) , ')'), paste0('BL = ', length(which(df$FS_usable == 1 & df$ fu == 0)), '   |  ', 'FU = ', length(which(df$FS_usable == 1 & df$fu == 1))), sep = '\\n')
[5]: paste(paste0('no HCV outlier (n = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1)), ')'), paste0('BL = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1 & df$fu == 0)), '  |  ', 'FU = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1 & df$fu == 1))), sep = '\\n')
[6]: paste(paste0('all control variables for model 2 (n = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1 & !is.na(df$CESD_sum))), ')'), paste0('BL = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1 & !is.na(df$CESD_sum) & df$fu == 0)), '  |  ', 'FU = ', length(which(df$FS_usable == 1 & df$outlier_HCV != 1 & !is.na(df$CESD_sum) & df$fu == 1))), sep = '\\n')
[7]: paste(paste0('executive functions available (n = ', length(which(!is.na(df$exfunct))), ')'), paste0('BL = ', length(which(!is.na(df$exfunct) & df$ fu == 0)), '  |  ', 'FU = ', length(which(!is.na(df$exfunct) & df$fu == 1))), sep = '\\n')
[8]: paste(paste0('no executive functions outlier (n = ', length(which(df$outlier_exfunct != 1)), ')'), paste0('BL = ', length(which(df$outlier_exfunct != 1 & df$fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_exfunct != 1 & df$fu == 1))), sep = '\\n')
[9]: paste(paste0('all control variables for model 2 (n = ', length(which(df$outlier_exfunct != 1 & !is.na(df$CESD_sum))), ')'), paste0('BL = ', length(which(df$outlier_exfunct != 1 & !is.na(df$CESD_sum) & df$ fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_exfunct != 1 & !is.na(df$CESD_sum) & df$fu == 1))), sep = '\\n')
[10]: paste(paste0('memory available (n = ', length(which(!is.na(df$memo))), ')'), paste0('BL = ', length(which(!is.na(df$memo) & df$fu == 0)), '  |  ', 'FU = ', length(which(!is.na(df$memo) & df$fu == 1))), sep = '\\n')
[11]: paste(paste0('no memory outlier (n = ', length(which(df$outlier_memo != 1)), ')'), paste0('BL = ', length(which(df$outlier_memo != 1 & df$ fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_memo != 1 & df$ fu == 1))), sep = '\\n')
[12]: paste(paste0('all control variables for model 2 (n = ', length(which(df$outlier_memo != 1 & !is.na(df$CESD_sum))), ')'), paste0('BL = ', length(which(df$outlier_memo != 1 & !is.na(df$CESD_sum) & df$fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_memo != 1 & !is.na(df$CESD_sum) & df$fu == 1))), sep = '\\n')
[13]: paste(paste0('processing speed available (n = ', length(which(!is.na(df$procspeed))), ')'), paste0('BL = ', length(which(!is.na(df$procspeed) & df$fu == 0)), '  |  ', 'FU = ', length(which(!is.na(df$procspeed) & df$fu == 1))), sep = '\\n')
[14]: paste(paste0('no processing speed outlier (n = ', length(which(df$outlier_procspeed != 1)), ')'), paste0('BL = ', length(which(df$outlier_procspeed != 1 & df$ fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_procspeed != 1 & df$ fu == 1))), sep = '\\n')
[15]: paste(paste0('all control variables for model 2 (n = ', length(which(df$outlier_procspeed != 1 & !is.na(df$CESD_sum))), ')'), paste0('BL = ', length(which(df$outlier_procspeed != 1 & !is.na(df$CESD_sum) & df$fu == 0)), '  |  ', 'FU = ', length(which(df$outlier_procspeed != 1 & !is.na(df$CESD_sum) & df$fu == 1))), sep = '\\n')
")

