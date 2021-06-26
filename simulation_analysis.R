# load the results of 7 simulations with 16 hypotheses each and bind them into one dataframe
dict = "/data/pt_life/LLammer/Analysis/Simulation/run"
space = "/workspace.RData"
counter = 1
while (counter < 8) {
  load(paste0(dict, counter,  space))
  allbf <- list(bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8, bf9, bf10, bf11, bf12, bf13, bf14, bf15, bf16)
  new_df <- as.data.frame(lapply(allbf, function(x) print(x[3,1])))
  new_df <- melt(new_df)
  if(counter != 1){
    df <- rbind(df, new_df)
  }
  else{
    df <- new_df
  }
  counter = counter + 1
}

# of 112 hypotheses, 1 was a false positive, 8 failed to find moderate evidence against H0 and 103 correctly  
# identified no effect
FP = length(which(df$value>3)) # n = 1
TN = length(which(df$value<1/3)) # n = 103
A = length(which(df$value>1/3 & df$value<3)) # n = 8
maximum = max(df$value) # 3.729851
minimum = min(df$value) # 0.04513596
hist(log(df$value))
ggplot(df, aes(x = value)) + geom_histogram(fill = "blue", bins = 75) + scale_x_log10(expand = c(0, 0)) + 
  geom_vline(xintercept = 1/3, linetype="dashed", color = "red", size=0.75) + 
  geom_vline(xintercept = 3, linetype="dashed", color = "red", size=0.75) + 
  scale_y_continuous(expand = c(0, 0)) + 
  xlab("BFA0")
FWER = FP / length(df$value) # 0.8928571%
topn(df$value, 6, decreasing = T, index = F) # 3.7298512 2.9840345 2.8315297 0.6661122 0.5100214 0.3798403
