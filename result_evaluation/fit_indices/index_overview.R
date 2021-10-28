library(lavaan)
load("/data/pt_life/ResearchProjects/LLammer/Results/mediation/Workspace/workspace.RData")
overview <- data.frame(matrix(nrow = 10, ncol = 16))
colnames(overview) <- c("311","311","312","312","411a","411a","411b","411b","411c","411c","412a","412a","412b","412b","412c","412c")
rownames(overview) <- c("chisq", "df", "pvalue", "chisq/df", "rmsea", "rmsea_lower", "rmsea_upper", "srmr", "nnfi", "cfi")
fit_check <- function(fit){
  xx <- as.data.frame(fitMeasures(fit$fit))
  values <- c(xx["chisq",], xx["df",], xx["pvalue",], xx["chisq",]/xx["df",], xx["rmsea",], xx["rmsea.ci.lower",], xx["rmsea.ci.upper",], xx["srmr",], xx["nnfi",], xx["cfi",])
  chisq <- ""
  df <- ""
  if(xx["pvalue",] > 0.05){
    pvalue <- "good fit"
  }
  else if(0.5 >= xx["pvalue",] & xx["pvalue",] >= 0.01){
    pvalue <- "acceptable fit"
  }
  else{
    pvalue <- "unacceptable fit"
  }
  if(xx["chisq",]/xx["df",] <= 2){
    chisq_df <- "good fit"
  }
  else if(3 >= xx["chisq",]/xx["df",] & xx["chisq",]/xx["df",] >= 2){
    chisq_df <- "acceptable fit"
  }
  else{
    chisq_df <- "unacceptable fit"
  }
  if(xx["rmsea",] <= 0.05){
    rmsea <- "good fit"
  }
  else if(0.08 >= xx["rmsea",] & xx["rmsea",]  > 0.05){
    rmsea <- "acceptable fit"
  } 
  else{
    rmsea <- "unacceptable fit"
  }
  rmsea_l <- ""
  rmsea_u <- ""
  if(xx["srmr",] <= 0.05){
    srmr <- "good fit"
  }
  else if(0.1 >= xx["srmr",] & xx["srmr",]  > 0.05){
    srmr <- "acceptable fit"
  } 
  else{
    srmr <- "unacceptable fit"
  }
  if(0.97 <= xx["nnfi",] &  xx["nnfi",] <= 1){
    nnfi <- "good fit"
  }
  else if(0.95 <= xx["nnfi",] &  xx["nnfi",] < 0.97){
    nnfi <- "acceptable fit"
  }
  else{
    nnfi <- "unacceptable fit"
  }
  if(0.97 <= xx["cfi",] &  xx["cfi",] <= 1){
    cfi <- "good fit"
  }
  else if(0.95 <= xx["cfi",] & xx["cfi",] < 0.97){
    cfi <- "acceptable fit"
  }
  else{
    cfi <- "unacceptable fit"
  }
  evaluations <- c(chisq, df, pvalue, chisq_df, rmsea, rmsea_l, rmsea_u, srmr, nnfi, cfi)
  res <- list(values, evaluations)
  return(res)
}
res <- fit_check(fit311)
overview[,1] <- res[[1]]
overview[,2] <- res[[2]]
res <- fit_check(fit312)
overview[,3] <- res[[1]]
overview[,4] <- res[[2]]
res <- fit_check(fit411a)
overview[,5] <- res[[1]]
overview[,6] <- res[[2]]
res <- fit_check(fit411b)
overview[,7] <- res[[1]]
overview[,8] <- res[[2]]
res <- fit_check(fit411c)
overview[,9] <- res[[1]]
overview[,10] <- res[[2]]
res <- fit_check(fit412a)
overview[,11] <- res[[1]]
overview[,12] <- res[[2]]
res <- fit_check(fit412b)
overview[,13] <- res[[1]]
overview[,14] <- res[[2]]
res <- fit_check(fit412c)
overview[,15] <- res[[1]]
overview[,16] <- res[[2]]

write.csv(overview, "/data/pt_life/ResearchProjects/LLammer/Results/mediation/fit_indices/overview.csv")
