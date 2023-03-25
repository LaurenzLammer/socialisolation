library(tidyverse)
library(cowplot)
data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_full_data.csv")
data <- data %>%
  group_by(SIC) %>%
  mutate(
    trend_lsns = ifelse(fu == 0 & lead(LSNS_sum) >  LSNS_sum, "increase", "decrease"),
    trend_lsns = ifelse(fu == 0 & lead(LSNS_sum) ==  LSNS_sum, "unchanged", trend_lsns),
    trend_lsns = ifelse(fu == 1, lag(trend_lsns), trend_lsns),
    trend_ef = ifelse(fu == 0 & lead(exfunct) >  exfunct, "increase", "decrease"),
    trend_ef = ifelse(fu == 0 & lead(exfunct) ==  exfunct, "unchanged", trend_ef),
    trend_ef = ifelse(fu == 1, lag(trend_ef), trend_ef),
    trend_memo = ifelse(fu == 0 & lead(memo) >  memo, "increase", "decrease"),
    trend_memo = ifelse(fu == 0 & lead(memo) ==  memo, "unchanged", trend_memo),
    trend_memo = ifelse(fu == 1, lag(trend_memo), trend_memo),
    trend_ps = ifelse(fu == 0 & lead(procspeed) >  procspeed, "increase", "decrease"),
    trend_ps = ifelse(fu == 0 & lead(procspeed) ==  procspeed, "unchanged", trend_ps),
    trend_ps = ifelse(fu == 1, lag(trend_ps), trend_ps)
  )
df <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
df <- df %>%
  group_by(subject) %>%
  mutate(
    trend_hcv = ifelse(fu == 0 & lead(HCV) >  HCV, "increase", "decrease"),
    trend_hcv = ifelse(fu == 0 & lead(HCV) ==  HCV, "unchanged", trend_hcv),
    trend_hcv = ifelse(fu == 1, lag(trend_hcv), trend_hcv)
  )
data$trend_lsns <- factor(data$trend_lsns)
df$trend_hcv <- factor(df$trend_hcv)
data$trend_ef <- factor(data$trend_ef)
data$trend_memo <- factor(data$trend_memo)
data$trend_ps <- factor(data$trend_ps)

p_lsns <- ggplot(data = data, aes(x = age, y = LSNS_sum, group = SIC, colour = trend_lsns)) + labs(y="LSNS")
p_lsns <- p_lsns + geom_point() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p_lsns <- p_lsns + geom_line() + scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3"))
p_lsns 
ggsave("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghetti_plot_lsns_change.tiff", dpi = 600, 
       width = 20, height = 14, units = "cm")
p_hcv <- ggplot(data = df, aes(x = age, y = HCV, group = subject, colour = trend_hcv)) + labs(y="Hippocampal Volume")
p_hcv <- p_hcv + geom_point() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p_hcv <- p_hcv + geom_line() + scale_colour_manual(values=c("#d95f02", "#1b9e77", "#7570b3"))
p_hcv 
ggsave("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghetti_plot_hcv_change.tiff", dpi = 600, 
       width = 20, height = 14, units = "cm")
p_ef <- ggplot(data = data, aes(x = age, y = exfunct, group = SIC, colour = trend_ef)) + labs(y="Executive Functions")
p_ef <- p_ef + geom_point() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p_ef <- p_ef + geom_line() + scale_colour_manual(values=c("#d95f02", "#1b9e77", "#7570b3"))
p_ef 
ggsave("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghetti_plot_ef_change.tiff", dpi = 600, 
       width = 20, height = 14, units = "cm")
p_memo <- ggplot(data = data[data$memo >= -5,], aes(x = age, y = memo, group = SIC, colour = trend_memo)) + labs(y="Memory")
p_memo <- p_memo + geom_point() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p_memo <- p_memo + geom_line() + scale_colour_manual(values=c("#d95f02", "#1b9e77", "#7570b3"))
p_memo 
ggsave("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghetti_plot_memo_change.tiff", dpi = 600, 
       width = 20, height = 14, units = "cm")
p_ps <- ggplot(data = data[data$procspeed >= -5,], aes(x = age, y = procspeed, group = SIC, colour = trend_ps)) + labs(y="Processing Speed")
p_ps <- p_ps + geom_point()
p_ps <- p_ps + geom_line() + scale_colour_manual(values=c("#d95f02", "#1b9e77", "#7570b3"))
p_ps
ggsave("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghetti_plot_ps_change.tiff", dpi = 600, 
       width = 20, height = 14, units = "cm")

plots <- plot_grid(p_lsns, p_hcv, p_ef, p_memo, p_ps, ncol = 1, nrow = 5) 
ggsave(filename = "/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/spaghettiplots.tiff", device = "tiff", dpi = 600, width = 8, height = 16)

