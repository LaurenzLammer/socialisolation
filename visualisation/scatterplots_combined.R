load("/data/pt_life/ResearchProjects/LLammer/Results/Workspace/workspace2.RData")
library(effects)
library(ggplot2)
library(cowplot)
library(ggplotify)

# a function that will create three complex scatterplots (baseline social isolation, change in social isolation, baseline age) for the requested dependent variable and model
comp_plot <- function(dv, model = 1){
  if(dv == "HCV"){
    datax <- subset(data, data$outlier_HCV != 1)
    y_axis_label <- "Hippocampal volume in mm^3"
    x_axis_label <- rep("", 3)
    x_lab_height <- 0.2
    y_value <- 4750 # position for asterisks (significance before FDR)
    y_value2 <- 4650 # position for asterisks (significance after FDR)
    if(model == 1){
      hyp <- list111
      # get the p-/q-values for baseline social isolation and change in social isolation
      pb <- qval1$pvalues[1]
      qb <- qval1$qvalues[1]
      pc <- qval1$pvalues[2] 
      qc <- qval1$qvalues[2]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list111$bf_nolog["full_vs_null_sided",1]/(list111$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list111$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list131$bf_nolog["full_vs_null_sided",1]/(list131$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list131$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
    else{
      hyp <- list112
      pb <- qval2$pvalues[1]
      qb <- qval2$qvalues[1]
      pc <- qval2$pvalues[2] 
      qc <- qval2$qvalues[2]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list112$bf_nolog["full_vs_null_sided",1]/(list112$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list112$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list132$bf_nolog["full_vs_null_sided",1]/(list132$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list132$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
  }
  else if(dv == "exfunct"){
    datax <- subset(data, data$outlier_exfunct != 1)
    y_axis_label <- "executive functions, z-scored"
    x_axis_label <- rep("", 3)
    x_lab_height <- 0.2
    y_value <- 3.1
    y_value2 <- 2.7
    if(model == 1){
      hyp <- list211a
      pb <- qval1$pvalues[4]
      qb <- qval1$qvalues[4]
      pc <- qval1$pvalues[7] 
      qc <- qval1$qvalues[7]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list211a$bf_nolog["full_vs_null_sided",1]/(list211a$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list211a$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list221a$bf_nolog["full_vs_null_sided",1]/(list221a$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list221a$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
    else{
      hyp <- list212a
      pb <- qval2$pvalues[4]
      qb <- qval2$qvalues[4]
      pc <- qval2$pvalues[7]
      qc <- qval2$qvalues[7]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list212a$bf_nolog["full_vs_null_sided",1]/(list212a$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list212a$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list222a$bf_nolog["full_vs_null_sided",1]/(list222a$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list222a$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
  }
  else if(dv == "memo"){
    datax <- subset(data, data$outlier_memo != 1)
    y_axis_label <- "memory, z-scored"
    x_axis_label <- rep("", 3)
    x_lab_height <- 0.2
    y_value <- 1.6
    y_value2 <- 1.4
    if(model == 1){
      hyp <- list211b
      pb <- qval1$pvalues[5]
      qb <- qval1$qvalues[5]
      pc <- qval1$pvalues[8]
      qc <- qval1$qvalues[8]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list211b$bf_nolog["full_vs_null_sided",1]/(list211b$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list211b$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list221b$bf_nolog["full_vs_null_sided",1]/(list221b$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list221b$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
    else{
      hyp <- list212b
      pb <- qval2$pvalues[5]
      qb <- qval2$qvalues[5]
      pc <- qval2$pvalues[8]
      qc <- qval2$qvalues[8]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list212b$bf_nolog["full_vs_null_sided",1]/(list212b$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list212b$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list222b$bf_nolog["full_vs_null_sided",1]/(list222b$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list222b$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
  }
  else{
    datax <- subset(data, data$outlier_procspeed != 1)
    y_axis_label <- "processing speed, z-scored"
    x_axis_label <- c("baseline LSNS", "change in LSNS", "baseline age in years, centered")
    x_lab_height <- 0.5
    y_value <- 1.8
    y_value2 <- 1.4
    if(model == 1){
      hyp <- list211c
      pb <- qval1$pvalues[6]
      qb <- qval1$qvalues[6]
      pc <- qval1$pvalues[9] 
      qc <- qval1$qvalues[9]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list211c$bf_nolog["full_vs_null_sided",1]/(list211c$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list211c$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list221c$bf_nolog["full_vs_null_sided",1]/(list221c$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list221c$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
    else{
      hyp <- list212c
      pb <- qval2$pvalues[6]
      qb <- qval2$qvalues[6]
      pc <- qval2$pvalues[9]
      qc <- qval2$qvalues[9]
      df1 <- data.frame(model = c("full", "null"), 
                        bf = c((list212c$bf_nolog["full_vs_null_sided",1]/(list212c$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list212c$bf_nolog["full_vs_null_sided",1] +1))*100))
      df2 <- data.frame(model = c("full", "null"), 
                        bf = c((list222c$bf_nolog["full_vs_null_sided",1]/(list222c$bf_nolog["full_vs_null_sided",1] +1))*100, 
                               (1/(list222c$bf_nolog["full_vs_null_sided",1] +1))*100))
    }
  }
  if(model == 2){
    datax <- subset(datax, !is.na(datax$CES.D))
  }
  LSNS_base_effect <- effect(term = "LSNS_base", mod = hyp$res) 
  lbe <- as.data.frame(LSNS_base_effect)
  LSNS_change_effect <- effect(term = "LSNS_change", mod = hyp$res) 
  lce <- as.data.frame(LSNS_change_effect)
  age_base_effect <- effect(term = "age_base", mod = hyp$res) 
  abe <- as.data.frame(age_base_effect)
  significances <- c(pb, qb, pc, qc)
  label_b1 <- ""
  label_b2 <- ""
  label_c1 <- ""
  label_c2 <- ""
  asterisiks <- c(label_b1, label_b2, label_c1, label_c2)
  for(n in 1:4){
    if(significances[n] < 0.0001){
      asterisiks[n] <- "****"
    } else if(significances[n] < 0.001){
      asterisiks[n] <- "***"
    } else if(significances[n] < 0.01){
      asterisiks[n] <- "**"
    } else if(significances[n] < 0.05){
      asterisiks[n] <- "*"
    } 
  }
  p1 <- ggplot() +
    geom_point(data=datax, aes_string("LSNS_base", dv), col = "gray80") +
    geom_line(data = lbe, aes(LSNS_base, fit), col = "royalblue3") +
    geom_ribbon(data = lbe, aes(x=LSNS_base, ymin=lower, ymax=upper), alpha= 0.3, fill="royalblue3") +
    theme_bw() +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
    xlim(-2, 32) +
    labs(x= x_axis_label[1]) +
    geom_text(data = data.frame(x = -2, y = y_value), aes(x = x, y = y), label = asterisiks[1], cex = 10, hjust = 0) +
    geom_text(data = data.frame(x = -2, y = y_value2), aes(x = x, y = y), label = asterisiks[2], cex = 10, hjust = 0)
  p2 <- ggplot() +
    geom_point(data=datax, aes_string("LSNS_change", dv), col = "gray80") +
    geom_line(data = lce, aes(LSNS_change, fit), col = "royalblue3") +
    geom_ribbon(data = lce, aes(x=LSNS_change, ymin=lower, ymax=upper), alpha= 0.3, fill="royalblue3") +
    theme_bw() +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
    xlim(-17,17) +
    labs(x= x_axis_label[2]) +
    geom_text(data = data.frame(x = -17, y = y_value), aes(x = x, y = y), label = asterisiks[3], cex = 10, hjust = 0) +
    geom_text(data = data.frame(x = -17, y = y_value2), aes(x = x, y = y), label = asterisiks[4], cex = 10, hjust = 0)
  p3 <- ggplot() +
    geom_point(data=datax, aes_string("age_base", dv), col = "gray80") +
    geom_line(data = abe, aes(age_base, fit), col = "grey42") +
    geom_ribbon(data = abe, aes(x=age_base, ymin=lower, ymax=upper), alpha= 0.3, fill="grey42") +
    theme_bw() +
    xlim(-22,12) +
    labs(y= y_axis_label, x= x_axis_label[3])
  # create pie charts to visualise the Bayes factors of the full and null model
  pie1 <- ggplot(df1, aes(x="", y=bf, fill=model)) +
    theme_void() + theme(legend.position="none") +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values= c("#009E73", "#000000"))
  pie2 <- ggplot(df2, aes(x="", y=bf, fill=model)) +
    theme_void() + theme(legend.position="none") +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values= c("#009E73", "#000000"))
  p1 <- ggdraw(p1) + 
    draw_plot(pie1, 0.75, 0.8, .2, .2) 
  p2 <- ggdraw(p2) + 
    draw_plot(pie2, 0.75, 0.8, .2, .2) 
  all <- plot_grid(p3, p1, p2,  ncol = 3, rel_widths = c(1.18,1,1))
  return(all)
  }

# create plots for all dependent variables
q1 <- comp_plot(dv = "HCV") + theme(plot.margin=margin(b=-0.7,unit="cm"))
q2 <- comp_plot(dv = "exfunct") + theme(plot.margin=margin(b=-0.7,unit="cm"))
q3 <- comp_plot(dv = "memo") + theme(plot.margin=margin(b=-0.7,unit="cm"))
q4 <- comp_plot(dv = "procspeed")
# arrange the plots to get one composite plot
plots <- plot_grid(q1, q2, q3, q4, ncol = 1, nrow = 4) 
ggsave(filename = "/data/pt_life/ResearchProjects/LLammer/Results/visualisation/model1_scatterplots.tff", device = "tiff", dpi = 600, width = 8, height = 12.8)

