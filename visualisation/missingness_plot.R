library(cowplot)
library(naniar)
library(ggplot2)
setwd("/data/pt_life/ResearchProjects/LLammer/si_update/Results/visualisation/")
data <- read.csv("/data/pt_life/ResearchProjects/LLammer/si_update/Data/compiled_scaled_data.csv")
data$gender <- data$sex
data$CESD <- data$CES.D
data$LSNS <- data$LSNS_sum
data$TICS <- data$TICS_sum
data <- data[,c("TICS", "gender", "procspeed", "memo", "LSNS", "hypertension", "HCV", "exfunct", "education", "diabetes", "CESD", "BMI",
                "age")]

datafct <- data
datafct$LSNS <- as.factor(round(datafct$LSNS, digits = 0))
heatmap <- gg_miss_fct(datafct, LSNS) + theme(plot.margin = unit(c(0,1,1,1), "cm")) +
  scale_x_discrete(breaks=c(0,5,10,15,20,25,30))
hist <- ggplot(data, aes(x=LSNS, fill = count)) + geom_histogram(fill = "#280033") +
  theme_classic() + theme(plot.margin = unit(c(1,1,0,1), "cm"), axis.title.x=element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  coord_cartesian(xlim = c(0, 30)) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
tiff("missingplot.tiff", width = 5, height = 3.75, res = 600, units = "in")
plot_grid(hist, heatmap, rel_heights = c(1.6, 4), labels = c('A', 'B'),
          ncol = 1, nrow = 2, align = "v", axis = "lr")
dev.off()
