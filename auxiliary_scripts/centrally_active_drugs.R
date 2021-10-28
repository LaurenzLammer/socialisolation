df <- read.csv("/data/pt_life/ResearchProjects/LLammer/Data/compiled_full_data.csv")
# M03B - centrally active muscle relexants
# N06D - antidementive drugs
# N05A - antipsychotic drugs
# N05B - anxiolytic drugs
# N06B - psychostimulative drugs, ADHD drugs & nootropic drugs
# N05C - sedative and hypnotic drugs
# N06A - antidepressants
# N02A - opioids
# N03 - antiepileptic drugs
# N07A - parasympathomimetics
# N02C - migraine remedies
# N07C - vertigo medications
# N04 - parkinson's disease medication
# R05DA - opium alkaloids and derivatives

drugs <- data.frame(matrix(ncol = 4, nrow = 14))
colnames(drugs) <- c("n of particpants taking", "n of participants taking no other CA med", "n of particpants taking plant based", "n of participants taking no other CA med")
rownames(drugs) <- c("centrally active muscle relexants", "antidementive drugs", "antipsychotic drugs",
                     "anxiolytic drugs", "psychostimulative drugs, ADHD drugs & nootropic drugs", "sedative and hypnotic drugs",
                     "antidepressants", "opioids", "antiepileptic drugs", "parasympathomimetics", "migraine remedies", 
                     "vertigo medications", "parkinson's disease medication", "opium alkaloids and derivatives")

not_to_match <- c("^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^M03B", ", M03B")
drugs["centrally active muscle relexants",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["centrally active muscle relexants",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                               !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N05A", ", N05A", "^N05B", ", N05B", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N06D", ", N06D")
drugs["antidementive drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["antidementive drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                               !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05B", ", N05B", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N05A", ", N05A")
drugs["antipsychotic drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["antipsychotic drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N05B", ", N05B")
drugs["anxiolytic drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["anxiolytic drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N06A", ", N06A", "^N06B", 
                  ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N05C", ", N05C")
drugs["sedative and hypnotic drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["sedative and hypnotic drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                             !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N06A", ", N06A")
drugs["antidepressants",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["antidepressants",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                              !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N06B", ", N06B")
drugs["psychostimulative drugs, ADHD drugs & nootropic drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["psychostimulative drugs, ADHD drugs & nootropic drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                                                   !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N02A", ", N02A")
drugs["opioids",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["opioids",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                                                   !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N03", ", N03")
drugs["antiepileptic drugs",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["antiepileptic drugs",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                     !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N07A", ", N07A")
drugs["parasympathomimetics",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["parasympathomimetics",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N02C", ", N02C", "^N07A", 
                  ", N07A", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N07C", ", N07C")
drugs["vertigo medications",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["vertigo medications",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                  !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", 
                  ", N07A", "^N07C", ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N02C", ", N02C")
drugs["migraine remedies",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["migraine remedies",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", 
                  ", N07A", "^N07C", ", N07C", "^N02C", ", N02C", "^R05DA", ", R05DA")
to_match <- c("^N04", ", N04")
drugs["parkinson's disease medication",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["parkinson's disease medication",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                               !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", 
                  ", N07A", "^N07C", ", N07C", "^N02C", ", N02C", "^N04", ", N04")
to_match <- c("^R05DA", ", R05DA")
drugs["opium alkaloids and derivatives",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["opium alkaloids and derivatives",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                            !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

# no questionable muscle relaxants

not_to_match <- c("^M03B", ", M03B", "^N05A", ", N05A", "^N05B", ", N05B", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N06DP", ", N06DP")
drugs[2,3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs[2,4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

# no questionable antipsychotics

# lavendar oil users (anxiolytic) should not be excluded

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N06B", ", N06B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N05BX05", ", N05BX05")
drugs["anxiolytic drugs",3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["anxiolytic drugs",4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                              !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

# psychostimulatives

# hypnotics and sedatives
# one coudl question Baldrianm, it allegedly contains a component with a sedative potential, 
# but I think that this shouldn't affect test performance

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N06A", ", N06A", "^N06B", 
                  ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N05CM09", ", N05CM09")
drugs["sedative and hypnotic drugs",3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["sedative and hypnotic drugs",4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                         !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))


not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
                  ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N06AH", ", N06AH")
drugs["antidepressants",3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["antidepressants",4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                             !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", 
                  ", N07A", "^N07C", ", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N02CP", ", N02CP", "^N02CH", ", N02CH")
drugs["migraine remedies",3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["migraine remedies",4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                               !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N02C", ", N02C", "^N07A", 
                  ", N07A", "^N04", ", N04", "^R05DA", ", R05DA")
to_match <- c("^N07CH", ", N07CH")
drugs["vertigo medications",3] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["vertigo medications",4] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                 !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

not_to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N05C", 
                  ", N05C", "^N06A", ", N06A", "^N06B", ", N06B", "^N02A", ", N02A", "^N03", ", N03", "^N07A", 
                  ", N07A", "^N07C", ", N07C", "^N02C", ", N02C", "^R05DA", ", R05DA")
to_match <- c("^N04", ", N04")
drugs["parkinson's disease medication",1] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))
drugs["parkinson's disease medication",2] <- length(which(grepl(paste0(to_match, collapse = "|"), df$ADULT_MEDA_H_ATC) & 
                                                            !grepl(paste0(not_to_match, collapse = "|"), df$ADULT_MEDA_H_ATC)))

write.csv(drugs, "/data/pt_life/ResearchProjects/LLammer/drugs.csv")
