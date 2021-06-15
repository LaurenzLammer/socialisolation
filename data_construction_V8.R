library(kit)
library(readxl)
library(tidyverse)
library(lubridate)
library(eeptools)
library(data.table)
library(naniar)


trust <- function(var, tops = 5, type = "cont"){
  # a brief overview intended to give an idea of the trustworthiness of the data
  # tops can be used to determine the number of max/min values to print
  # type cont gives information on continuous and type cat on categorical variables
  print(paste0("n of observations: ", length(which(!is.na(var)))))
  print(head(unique(var), n = 20))
  hist(var)
  if(type == "cont"){
  print(paste0("top ", tops, " max: "))
  print(topn(var, tops, decreasing = T, index = F))
  print(paste0("rown of max: ", which.max(var)))
  print(paste0("top ", tops, " min: "))
  print(topn(var, tops, decreasing = F, index = F))
  print(paste0("rown of min: ", which.min(var)))
  print(paste0("median: ", median(var, na.rm = T)))
  print(paste0("mean: ", mean(var, na.rm = T)))
  print(paste0("sd: ", sd(var, na.rm = T)))
  }
  if(type == "cat"){
    print("n of measures == 1")
    print(length(which(var == 1)))
    print("n of measures == 0")
    print(length(which(var == 0)))
    print("percentage of measures == 1")
    print(paste0(length(which(var == 1))/length(which(!is.na(var))), " %"))
  }
}

# load baseline data
bl_data <- read_excel("/data/pt_life/LLammer/Data/data/Baseline/PV0573_datajoin_2021-01-14.xlsx")
# order data and remove duplicates
bl_data <- arrange(.data = bl_data, "SIC", factor(SGROUP, levels = c("A1_HAUPT01", "A1_PILOT_2", "A1_PILOT")))
bl_data <- distinct(.data = bl_data, SIC, .keep_all = T)

# remove hashtags around drug names
bl_data$ADULT_MEDA_H_ATC <- str_replace_all(bl_data$ADULT_MEDA_H_ATC, "#", "") 
# in the ATC nomenclature all antihypertensive drugs begin with "C02", all diuretics begin with "C03", 
# all beta-blockers with "C07", all Ca-chanel blockers with "C08" and all RAAS affecting drugs with "C09"  
# all antidiabetic drugs begin with "A10" in the ATC nomenclature
bl_data <- bl_data %>%
  mutate(BPmed = case_when(
    grepl("^C02", ADULT_MEDA_H_ATC) | grepl(", C02", ADULT_MEDA_H_ATC) ~ 1,
    grepl("^C03", ADULT_MEDA_H_ATC) | grepl(", C03", ADULT_MEDA_H_ATC) ~ 1,
    grepl("^C07", ADULT_MEDA_H_ATC) | grepl(", C07", ADULT_MEDA_H_ATC) ~ 1,
    grepl("^C08", ADULT_MEDA_H_ATC) | grepl(", C08", ADULT_MEDA_H_ATC) ~ 1,
    grepl("^C09", ADULT_MEDA_H_ATC) | grepl(", C09", ADULT_MEDA_H_ATC) ~ 1,
    is.na(ADULT_MEDA_H_ATC) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(diabmed = case_when(
    grepl("^A10", ADULT_MEDA_H_ATC) | grepl(", A10", ADULT_MEDA_H_ATC) ~ 1,
    is.na(ADULT_MEDA_H_ATC) ~ NA_real_,
    TRUE ~ 0
  ))
# properly code NAs and calculate mean systolic blood pressure
bl_data <- replace_with_na_at(bl_data, c("BLUTDRUCKMESS_F0024", "BLUTDRUCKMESS_F0019", "BLUTDRUCKMESS_F0014"), ~.x %in% c(998, 996, 18))
bl_data$BPsyst <- (bl_data$BLUTDRUCKMESS_F0024 + bl_data$BLUTDRUCKMESS_F0019 + bl_data$BLUTDRUCKMESS_F0014)/3
# categorise participants as hypertensive or non-hypertensive and diabetic or non-diabetic
bl_data <- bl_data %>%
  mutate(hypertension = case_when(
    BPmed == 1 ~ 1,
    BPsyst > 160 ~ 1,
    ADULT_BP_KNOWN_HYPERTENSION == 1 ~ 1,
    is.na(BPmed) & is.na(BPsyst) & is.na(ADULT_BP_KNOWN_HYPERTENSION) ~ NA_real_,
    TRUE ~ 0
  )) %>%
  mutate(diabetes = case_when(
    diabmed == 1 ~ 1, 
    HBA1C_E_NUM_VALUE >= 6 ~ 1,
    DERIVAT_DIABETES_DS_HISTORY == 1 ~ 1,
    is.na(diabmed) & is.na(HBA1C_E_NUM_VALUE) & DERIVAT_DIABETES_DS_HISTORY == -1 ~ NA_real_,
    TRUE ~ 0
  ))
# rename medical anamnesis and cognitive function labels to make them understandable
bl_data <- bl_data %>%
  rename(
    epilepsy = MEDANAM_F0167, 
    parkinson = MEDANAM_F0171,
    MS = MEDANAM_F0179,
    cancera = MEDANAM_F0242,
    cancerb = MEDANAM_F0255
  )
# summarize information on cancer
bl_data <- bl_data %>%
  mutate(cancer = case_when(
    cancera == 1 ~ 1,
    cancerb == 1 ~1, 
    TRUE ~ 0
  ))
# load data from radiological assessments
radio <- read.csv("/data/pt_life/LLammer/Data/bl_radio_assessment.csv")
# rename variables to allow merging and merge
radio <- radio %>%
  rename(
    SIC = pv_pseudonym
  )
bl_data <- merge(bl_data, radio, all.x = T, by = "SIC")
# 2 or more stroke lesions will be our proxy for a history of stroke at baseline
# 999999 was the code for participants without a brain MRI
# identify participants that have to be excluded due to tumors and non-congenital lesions
bl_data <- replace_with_na_at(bl_data, "mri_lesion_num", ~.x == 999999)
bl_data <- bl_data %>%
  mutate(stroke = case_when(
    mri_lesion_num >= 2 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(excludetumor = case_when(
    is.na(MRT_BefundTumours) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(excludelesion = case_when(
    is.na(MRT_BefundLesions) ~ 0,
    MRT_BefundLesions == "kongenital" ~ 0,
    TRUE ~ 1
  ))
# load data on participants' sex and birth date
sex_age_data <- read_excel("/data/pt_life/LLammer/Data/data/Baseline/PV0573_R00001.xlsx")
# rename variables to allow merging and merge
sex_age_data <- sex_age_data %>%
  rename(
    SIC = TEILNEHMER_SIC
  )
bl_data <- merge(bl_data, sex_age_data, all = T)
# define sex: female is 0, male is 1
bl_data$sex <- ifelse(bl_data$TEILNEHMER_GESCHLECHT == 2, 0, bl_data$TEILNEHMER_GESCHLECHT)
# birthmonth and year are available -- turn them into an usable form
bl_data$birth <- as.Date(parse_date_time(bl_data$TEILNEHMER_GEB_JJJJMM, "ym"))
# categorise participants with less than a tertiary degree (score below 3.6) as 1 in a variable called education
bl_data$education <- ifelse(bl_data$SES2_SESBLDG < 3.6, 1, 0)

# calculate CESD sum score, impute up to 4 items if necessary
CESD_cols <- sprintf("CES_D_%s",seq(1:20))
bl_data$CESD_sum <- rowSums(bl_data[,CESD_cols])
bl_data$CESD_na_count <- apply(is.na(bl_data[, CESD_cols]), 1, sum)
bl_data$CESD_sum <- ifelse((is.na(bl_data$CESD_sum) & bl_data$CESD_na_count < 5), 
                      rowSums(bl_data[,CESD_cols], na.rm = T)*20/(20-bl_data$CESD_na_count), bl_data$CESD_sum)

# load all follow-up data and merge them into one dataframe
# es fehlen: Medikamentenanamnese
anam <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/pv0573_anam_20210511.xlsx")
card_anam <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/pv0573_kardanam_20210511.xlsx")
LSNS <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T00010_NODUP.xlsx")
TMT <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T00041_NODUP.xlsx")
CERADa <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T00042_NODUP.xlsx")
CERADb <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T00044_NODUP.xlsx")
radio_fu <- read.csv("/data/pt_life/LLammer/Data/latest_radio_assessment.csv")
MRI_info_fu <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T01158_NODUP.xlsx")
TICS <- read_excel("/data/pt_life/LLammer/Data/data/Followup/pv573/PV0573_T00384_NODUP.xlsx")
# they need invariant subject column names for merging
colnames(anam)[1] <- "SIC"
colnames(LSNS)[1] <- "SIC"
colnames(TMT)[1] <- "SIC"
colnames(CERADa)[1] <- "SIC"
colnames(CERADb)[1] <- "SIC"
colnames(radio_fu)[18] <- "SIC"
colnames(MRI_info_fu)[1] <- "SIC"
colnames(TICS)[1] <- "SIC"
# replace deviating codes in CERAD with NA
fu <- Reduce(function(x,y) merge(x = x, y = y, all = T), 
             list(anam, LSNS, TMT, CERADa, CERADb, radio_fu, MRI_info_fu, TICS))

# we have to remove duplicates again
fu <- fu %>% group_by(SIC)
fu$edat_med_anam <- as.Date(parse_date_time(fu$edat_med_anam, "Ymd"))
fu <- arrange(.data = fu, desc(edat_med_anam), .by_group = T)
fu <- distinct(.data = fu, SIC, .keep_all = T)

# rename medical anamnesis labels to make them understandable
# some things in the medical anamnesis have changed between baseline and follow-up
# no specific epilepsy question
# a question asking for a dementia diagnosis
# different questions regarding cancer
# hence he have to make some changes to the processing of the data
# furthermore there now is a cardiological anamnesis with information on history of stroke
# we also rename the colname of the date of the follow-up MRI
fu <- fu %>%
  rename(
    other_diseases = mediz_an.f31a, 
    stroke = kard_an.f7, 
    parkinson = mediz_an.f27,
    MS = mediz_an.f26,
    dementia = mediz_an.f30,
    cancera = mediz_an.f2,
    cancera_cured = mediz_an.f2f,
    cancera_no_therapy = mediz_an.f2c_keinebeh,
    cancerb = mediz_an.f3,
    cancerb_cured = mediz_an.f3f,
    cancerb_no_therapy = mediz_an.f3c_keinebeh,
    cancerc = mediz_an.f4,
    cancerc_cured = mediz_an.f4f,
    cancerc_no_therapy = mediz_an.f4c_keinebeh,
    MRT_DATUM = MRT_ZW_U_DATUM
  )

# mark participants with epilepsy, cancer and tumors or non-congenital lesions
# no need to use lesion number as proxy for stroke in fu, as anamnesis contains information
fu <- fu %>%
  mutate(epilepsy = case_when(
    other_diseases == "EPILEPSIE" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(cancer = case_when(
    !is.na(cancera) & cancera_cured != 1 & cancera_no_therapy != 1 ~ 1,
    !is.na(cancerb) & cancerb_cured != 1 & cancerb_no_therapy != 1 ~ 1,
    !is.na(cancerc) & cancerc_cured != 1 & cancerc_no_therapy != 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(excludetumor = case_when(
    med_Befund..Tumours == "" | is.na(med_Befund..Tumours) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(excludelesion = case_when(
    is.na(med_Befund..Lesions) ~ 0,
    med_Befund..Lesions == "kongenital" ~ 0,
    med_Befund..Lesions == "" ~ 0,
    TRUE ~ 1
  ))

# we will have to use some baseline data for the follow-up
fu_data <- data.frame(matrix(nrow = nrow(bl_data), ncol = 1))
colnames(fu_data) = "SIC"
fu_data$SIC <- bl_data$SIC
fu_data$sex <- bl_data$sex
fu_data$birth <- bl_data$birth
fu_data$hypertension <- bl_data$hypertension
fu_data$diabetes <- bl_data$diabetes
fu_data$BMI_BMI <- bl_data$BMI_BMI
fu_data$CESD_sum <- bl_data$CESD_sum
fu_data$education <- bl_data$education


# merge fu_data with fu
fu_data <- merge(fu, fu_data, all = T, by = "SIC")

# distinguish baseline and follow-up data and merge them
bl_data$fu <- 0
fu_data$fu <- 1
df <- bind_rows(bl_data, fu_data)
# centrally active drugs begin with the following ATC-codes: 
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

# make sure no hashtags are around ACT-codes
df$ADULT_MEDA_H_ATC <- str_replace_all(df$ADULT_MEDA_H_ATC, "#", "") 
# find all participants taking centrally active medication to exclude them from the analyses
to_match <- c("^M03B", ", M03B", "^N06D", ", N06D", "^N05A", ", N05A", "^N05B", ", N05B", "^N06B", ", N06B", "^N05C", 
", N05C", "^N06A", ", N06A", "^N02A", ", N02A", "^N03", ", N03", "^N07A", ", N07A", "^N02C", ", N02C", "^N07C", 
", N07C", "^N04", ", N04", "^R05DA", ", R05DA")
df <- df %>%
  mutate(centr_act_med = case_when(
    grepl(paste0(to_match, collapse = "|"), ADULT_MEDA_H_ATC) ~ 1,
    TRUE ~ 0
  ))

# turn MRI date and LSNS date into a usable form to calculate age at MRI, use LSNS date if MRI date isn't available
df$MRT_DATUM <- as.Date(df$MRT_DATUM)
df$LSNS_DATUM <- as.Date(df$LSNS_DATUM)
df$age <- round(as.numeric(as.period(interval(start = df$birth, end = df$MRT_DATUM), unit = "years"), "years"), 1)
df <- df %>%
  mutate(age = case_when(
    is.na(age) ~ round(as.numeric(as.period(interval(start = birth, end = LSNS_DATUM), unit = "years"), "years"), 1),
    TRUE ~ age
  ))

# determine groups of columns belonging to the different cognitive tests 
a <- c("CERAD_WL1_", "CERAD_WL2_", "CERAD_WL3_")
b <- c("BUTTER", "ARM", "STRAND", "BRIEF", "KONIGI", "HUTTE", "STANGE", "KARTE", "GRAS", "MOTOR")
c <- c("KIRCHE", "KAFFEE", "BUTTER", "DOLLAR", "ARM", "STRAND", "FUNF", "BRIEF", "HOTEL", "BERG", "KONIGI", "HUTTE", 
       "PANTOF", "STANGE", "DORF", "BAND", "KARTE", "HEER", "GRAS", "MOTOR")
learning_cols <- paste0(rep(a, each = length(b)), b)
recall_cols <- paste0("CERAD_WL4_", b)
recognition_cols <- paste0("CERAD_WLW_", c)
phon_flu_cols <- paste0("CERAD_S_", c(15, 30, 45, 60), "S_CORR")
sem_flu_cols <- paste0("CERAD_VF_", c(15, 30, 45, 60), "S_CORR")
# replace different codes with NA
df <- replace_with_na_at(df, .vars = c("TMT_TIMEA", "TMT_TIMEB", sem_flu_cols), condition = ~.x %in% c(997, 998, 999))
df <- replace_with_na_at(df, .vars = c(recognition_cols, phon_flu_cols), condition =  ~.x %in% c(97, 98, 99))
# calculate scores for different cognitive tests
df$learning <- rowSums(df[,learning_cols]) 
df$recall <- rowSums(df[, recall_cols])
df$recognition <- rowSums(df[, recognition_cols])
df$sem_flu <- rowSums(df[, sem_flu_cols])
df$phon_flu <- rowSums(df[,phon_flu_cols])
# some learning and recall scores have to be turned into NAs, because the test are not really evaluable, i.e. marked 
# "does not apply" (97), "unratable" (98) or "refused to respond" (99)
d <- c("97", "98", "99")
learning_na_cols <- paste0(rep(a, each = length(d)), d)
recall_na_cols <- paste0("CERAD_WL4_", d)
df$learning_na <- rowSums(df[,learning_na_cols], na.rm = T)
df$recall_na <- rowSums(df[,recall_na_cols], na.rm = T)
df$learning <- ifelse(df$learning_na > 0, NA, df$learning)
df$recall <- ifelse(df$recall_na > 0, NA, df$recall)

# calculate TMT composite score and negate it to facilitate later calculations
df$TMT <- -((df$TMT_TIMEB - df$TMT_TIMEA)/df$TMT_TIMEA)
# calculate sum score for questionnaires
LSNS_cols <- sprintf("LSNS_%s",seq(1:6))
TICS_cols <- c("S010425_F0009", sprintf("S010425_F00%s",seq(from = 10, to = 65)))
df$LSNS_sum <- rowSums(df[,LSNS_cols])
df$TICS_sum <- rowSums(df[,TICS_cols])

#########continue debugging here
# count missing items for LSNS, CESD & TICS
df$LSNS_na_count <- apply(is.na(df[, LSNS_cols]), 1, sum)
df$TICS_na_count <- apply(is.na(df[, TICS_cols]), 1, sum)

# impute up to 1 / 6 items in LSNS / TICS
df$LSNS_sum <- ifelse((is.na(df$LSNS_sum) & df$LSNS_na_count < 2), rowSums(df[,LSNS_cols], na.rm = T)*6/5, df$LSNS_sum)
df$TICS_sum <- ifelse((is.na(df$TICS_sum) & df$TICS_na_count < 7), 
                      rowSums(df[,TICS_cols], na.rm = T)*57/(57-df$TICS_na_count), df$TICS_sum)
# negate LSNS-scores, so that higher scores mean more social isolation
df$LSNS_sum <- abs(30-df$LSNS_sum)

# turn outliers (by 4SD / 3SD) of BMI / TICS into NAs
df$BMI_BMI <- ifelse(diff(c(df$BMI_BMI, mean(df$BMI_BMI, na.rm = T))) >= 4*sd(df$BMI_BMI, na.rm = T), NA, df$BMI_BMI)
df$TICS_sum <- ifelse(diff(c(df$TICS_sum, mean(df$TICS_sum, na.rm = T))) >= 3*sd(df$TICS_sum, na.rm = T), NA, df$TICS_sum)

# imputation
df$BMI_BMI <-ifelse(is.na(df$BMI_BMI), mean(df$BMI_BMI, na.rm = T), df$BMI_BMI)
df$hypertension <- ifelse(is.na(df$hypertension), 
                rbinom(length(which(is.na(df$hypertension))), 1, mean(df$hypertension, na.rm = T)), df$hypertension)
df$diabetes <- ifelse(is.na(df$diabetes), 
                      rbinom(length(which(is.na(df$diabetes))), 1, mean(df$diabetes, na.rm = T)), df$diabetes)
df$education <- ifelse(is.na(df$education), 
                       rbinom(length(which(is.na(df$education))), 1, mean(df$education, na.rm = T)), df$education)

# standardise and center variables
df <- df %>%
  mutate_at(.vars = c("learning", "recall", "recognition", "sem_flu", "phon_flu", "TMT", 
              "TMT_TIMEA"), ~(scale(.) %>% as.vector)) %>%
  mutate(age = age-mean(age, na.rm = T))

# calculate composite scores for cognitive functions
df$exfunct <- (df$phon_flu + df$sem_flu + df$TMT)/3
df$memo <- (df$learning + df$recall + df$recognition)/3
df$procspeed <- -df$TMT_TIMEA
df$cerad <- df$learning + df$recall + df$recognition + df$phon_flu + df$sem_flu + df$TMT
# count missing items for memo & exfunct
df$exfunct_na_count <- apply(is.na(df[, c("phon_flu", "sem_flu", "TMT")]), 1, sum)
df$memo_na_count <- apply(is.na(df[, c("learning", "recall", "recognition")]), 1, sum)

# calculate composite scores of exfunct & memo based on 2 tests if only two are available
df$exfunct <- ifelse((is.na(df$exfunct) & df$exfunct_na_count < 2), 
                     rowSums(df[,c("phon_flu", "sem_flu", "TMT")], na.rm = T)/2, df$exfunct)
df$memo <- ifelse((is.na(df$memo) & df$memo_na_count < 2), 
                  rowSums(df[,c("learning", "recall", "recognition")], na.rm = T)/2, df$memo)
# scale oputcome variables to improve interpretability
df <- df %>%
  mutate_at(.vars = c("exfunct", "memo", "procspeed"), ~(scale(.) %>% as.vector))

# calculate between and within measures of age and LSNS
df <- df %>%
  group_by(SIC) %>%
  mutate(
    age_base = ifelse(fu == 0, age, lag(age)),
    LSNS_base = ifelse(fu == 0, LSNS_sum, lag(LSNS_sum))
  )
# for some subjects there are follow-up LSNS-scores but no baseline scores
# the follow-up will be treated like a baseline in these cases
df <- df %>%
  group_by(SIC) %>%
  mutate(
    age_base = ifelse(fu == 1 & is.na(LSNS_base), age, age_base),
    LSNS_base = ifelse(fu == 1 & is.na(LSNS_base), LSNS_sum, LSNS_base)
  )

df <- df %>%
  group_by(SIC) %>%
  mutate(
    age_change = ifelse(fu == 0, 0, age - age_base),
    LSNS_change = ifelse(fu == 0, 0, LSNS_sum - LSNS_base)
  )
# mark outliers (negatively by 2SD, wave-specific) for overall CERAD performance
df$outlier_CERAD<- ifelse(((df$fu == 0 & mean(df$cerad[df$fu == 0], na.rm = T)-df$cerad >= 
                              2*sd(df$cerad[df$fu == 0], na.rm = T) & !is.na(df$cerad))|
                             (df$fu == 1 & mean(df$cerad[df$fu == 1], na.rm = T)-df$cerad >= 
                                2*sd(df$cerad[df$fu == 1], na.rm = T) & !is.na(df$cerad))),
                          1, 0)

# load FreeSurfer quality assessmet
qc <- read.csv("/data/pt_life/LLammer/Data/fs_qc.csv")
df <- merge(df, qc)
df <- df %>%
  mutate(FS_usable = case_when(
    fu == 0 & BL_usable == 1 ~ "1",
    fu == 0 & BL_usable == 0 ~ "0",
    fu == 0 & (BL_usable == "qc pending" | is.na(BL_usable)) ~ "qc pending",
    fu == 1 & FU_usable == 1 ~ "1",
    fu == 1 & FU_usable == 0 ~ "0",
    fu == 1 & (FU_usable == "qc pending" | is.na(FU_usable)) ~ "qc pending",
    fu == 1 & FU_usable == "FU doesn't exist (yet)" ~ "FU doesn't exist (yet)",
    TRUE ~ "qc pending"
  ))

#load aseg data
aseg <- read.table("/data/pt_life/LLammer/Data/aseg_stats.txt", header = T)
aseg$fu <- ifelse(grepl("_fu", aseg$Measure.volume), 1, 0)
aseg$mrt_pseudonym <- substr(aseg$Measure.volume, 1, 10)
aseg$mean_HCV <- (aseg$Right.Hippocampus + aseg$Left.Hippocampus)/2
pseudo <- read_excel("/data/pt_life/LLammer/Data/PV573_PV-MRT-Pseudonymliste.xlsx")
colnames(pseudo)[1] <-"SIC"
aseg <- merge(aseg, pseudo, all = T)
df <- merge(df, aseg, all.x = T)

#write full dataset
write.csv(df, "/data/pt_life/LLammer/Data/compiled_full_data.csv", row.names = F)

# reduce dataset (exclusion criteria)
df <- subset(df, !is.na(df$LSNS_sum)) #3984 -> 3105
df <- subset(df, df$excludelesion == 0 & df$excludetumor == 0) #3105 -> 2881
df <- subset(df, df$stroke == 0 | is.na(df$stroke))#2881 -> 2847
df <- subset(df, df$cancer == 0)#2847 -> 2648
df <- subset(df, (is.na(df$epilepsy) | df$epilepsy != 1) & (is.na(df$MS) | df$MS != 1) & 
               (is.na(df$parkinson) | df$parkinson != 1)) # 2648 -> 2614
df <- subset(df, df$centr_act_med == 0) # 2614-> 2388
df <- subset(df, (is.na(df$dementia) | df$dementia != 1) & (df$MMST_MMST >= 24 | (is.na(df$MMST_MMST) & 
      (df$outlier_CERAD == 0)))) # 2388 -> 2345


# visually test if variables are normally distributed
ggplot(gather(df[, c("LSNS_sum", "LSNS_base", "LSNS_change", "BMI_BMI", "CESD_sum", "TICS_sum", "memo", 
                     "procspeed", "exfunct", "age_base", "age_change")]), aes(value)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~key, scales = 'free') 

# transform predictor variables to obtain a normal distribution
# add +1 to avoid log(0)

df$CESD_sum <- log(df$CESD_sum+1)
# test if transformations were successful

hist(df$CESD_sum)

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
df <- subset(df, df$outlier_LSNS != 1) #2345 -> 2307

# calculate adjusted HCV
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

#prepare subject identifiers for BayesFactor function
pseudo$identifier <- 1:length(pseudo$SIC)
df <- merge(df, pseudo, all.x = T, by = "SIC")
df$identifier <- as.factor(df$identifier)
# rename variables so that they fit the analysis scripts
df <- df %>%
  rename(
    BMI = BMI_BMI,
    subject = identifier,
    CES.D = CESD_sum,
    HCV = HCV_adj
  )


#write unscaled table
write.csv(df, "/data/pt_life/LLammer/Data/compiled_unscaled_data.csv", row.names = F)

#scale variables
df <- df %>%
  mutate_at(.vars = c("BMI", "TICS_sum", "exfunct", "memo", "procspeed", "CES.D"), ~(scale(.) %>% as.vector))

#prepare qdec table
df <- merge(df, pseudo, all.x = T)
subdir <- "/data/pt_life_freesurfer/freesurfer_all/"
qdec <- subset(df, ((df$fu == 0 & dir.exists(paste0(subdir, df$mrt_pseudonym))) | 
                 (df$fu == 1 & dir.exists(paste0(subdir, df$mrt_pseudonym, "_fu")))) & df$FS_usable == 1)
qdec$fsid <- ifelse(qdec$fu == 0, qdec$mrt_pseudonym, paste0(qdec$mrt_pseudonym, "_fu"))
qdec$"fsid-base" <- paste0(qdec$mrt_pseudonym, "_temp")
write.table(qdec[,c("fsid", "fsid-base")], "/data/pt_life/LLammer/Analysis/Preprocessing/long_qdec.dat", 
          row.names = F, quote = F, sep = "\t")

# reduce dataframe to relevant variables

df <- df[,c("subject", "fu", "age", "age_base", "age_change", "sex", "LSNS_sum", "LSNS_base", "LSNS_change", "HCV",
            "memo", "exfunct", "procspeed", "outlier_HCV", "outlier_exfunct", "outlier_memo", "outlier_procspeed", 
            "BMI", "hypertension", "diabetes", "education", "CES.D", "TICS_sum")]
# prepare subject names so that BayesFactor can deal with them

# write prepared dataset
write.csv(df, "/data/pt_life/LLammer/Data/compiled_scaled_data.csv", row.names = F)

