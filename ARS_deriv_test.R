

source(file.path("/well/doherty/users/rii148/risk_scores/array_job/master_branch/",'0_conf.R'), echo = T)
source(file.path("/well/doherty/users/rii148/risk_scores/array_job/master_branch/",'ARS_deriv.R'), echo = T)

load_val_set<- function(split,folds,data_soruce,stacked = FALSE){
  censor_date = NULL
  cens_file = '_ukb'
  suffix = '.incident'
  baseline_measures <- c("eid",'Age' ,'Sex', 'BodyMassIndex' ,'Diabetes', 'SystolBloodPressur' , 'TownsendIndex' , 'CholesRatio' , 
                         'CVDFamilyHistory', 'Ethnicity' , 'Smoking','AssessCenter','SleepActivity') 
  
  dis_list <- c('Essential..primary..hypertension', 'Migraine', 'Rheumatoid.arthritis', 'Systemic.lupus.erythematosus', 'Severe.mental.illnes',
                'Chronic.kidney.disease', 'Atrial.fibrillation.and.flutter', 'Corticosteroid', 'diabetes.type1', 'diabetes.type2','macce',
                'cad','mace','qrisk','gp_macce','cvd')
  
  #ecg_df <- read.csv("/well/doherty/users/rii148/ecg_scores/macce_res/minb_set/ecg/full/full_agg.csv")
  ukbc <- read.csv("/well/doherty/users/rii148/risk_scores/macce_all.csv")
  train<- read.csv("/well/doherty/users/rii148/risk_scores/ARS/dataset_deriv_Sep2021.csv")
  deriv <- read.csv("/well/doherty/users/rii148/risk_scores/ARS/dataset_test_Sep2021.csv")
  
  #split<- 0 #select which split we wish to evaluate 
  folds <-5
  
  
  full_set <- rbind(train,deriv)
  
  #ars_val_dat <- read.csv("/well/doherty/users/rii148/risk_scores/ARS/dataset_test_Sep2021.csv")
  step_data <- read.csv('/well/doherty/users/gji710/steps_cancer/data/rap_data/stepcount2.1.5_20230615.csv')
  ars_val_dat <- data.frame()
  if(stacked){

    for(i in 1:10){
      cv_path <- paste0('/well/doherty/users/rii148/risk_scores/ARS/nested_cv/split',i,'/')
      val_fold <- read.csv(paste0(cv_path,"predicted_test.csv"))
      ars_val_dat <- rbind(ars_val_dat)
    }
  }
  else{
    cv_path <- paste0('/well/doherty/users/rii148/risk_scores/ARS/nested_cv/split',split,'/')
    ars_val_dat <- read.csv(paste0(cv_path,"predicted_test.csv"))
  }
  
  ars_val_dat <- merge(ars_val_dat, full_set[, baseline_measures],by = "eid",all.x = TRUE)
  ars_val_dat <- merge(ars_val_dat, ukbc[, c("eid", 'DatAttendAssessCent','UkBiobankAssessCent','date_of_death','mortailty','HdlCholesterol','Cholesterol',dis_list)],by = "eid",all.x = TRUE)
  
  # Assuming df is your data frame
  # Fill missing "chol" values with "hdl" times "ratio"
  chol_na = is.na(ars_val_dat$Cholesterol)
  hdl_na  = is.na(ars_val_dat$HdlCholesterol)
  ratio_na= is.na(ars_val_dat$CholesRatio)
  
  ars_val_dat[chol_na,'Cholesterol'] <- ars_val_dat[chol_na,'HdlCholesterol']   * ars_val_dat[chol_na,'CholesRatio'] 
  ars_val_dat[hdl_na,'HdlCholesterol'] <- ars_val_dat[hdl_na,'Cholesterol']   / ars_val_dat[hdl_na,'CholesRatio'] 
  ars_val_dat[ratio_na,'CholesRatio'] <- ars_val_dat[ratio_na,'Cholesterol']   / ars_val_dat[ratio_na,'HdlCholesterol'] 
  
  
  
  dat_cov <- read.csv("/well/doherty/projects/scratch/swissre/shared_data/covariates/covariate_data_2023_02_16.csv") 
  # Select covariates data
  dat_cov <- select(dat_cov,
                    eid,
                    date_end_accel_raw,
                    age_accel_entry_yrs,
                    sex_raw,
                    smoking_cat,
                    bmi_raw,
                    systolic_bp_mean)
  #CadencePeak1Adjusted.steps.min.,
  #CadencePeak30Adjusted.steps.min.,                             
  #overall_median_se_perc,
  #overall_median_tst_min,
  #StepsDayMed)
  
  ars_val_dat <- merge(ars_val_dat, dat_cov[, c("eid", 'date_end_accel_raw')],by = "eid")
  ars_val_dat <- merge(ars_val_dat, step_data[, c("eid", 'StepsDayMedAdjusted')],by = "eid")
  
  #Loop through diseases and instances
  # List of diseases
  
  for (dis in dis_list) {
    cat(dis, "\n")
    ars_val_dat[,paste0(dis, '.prevalent')] = get_prev(ars_val_dat,dis,entry_col = 'date_end_accel_raw' )
    print("hgot prev")
    ars_val_dat[,paste0(dis, '.incident')] = get_inc(ars_val_dat,dis ,entry_col= 'date_end_accel_raw')
  }
  
  ars_val_dat['censor_date'] <- as.Date(ars_val_dat$date_end_accel_raw) %m+% years(6)
  ars_val_dat = censoring(ars_val_dat,'macce_obs_time',suffix = cens_file)
  
  ars_val_dat$Age
  #df$TimeDifference <- ifelse(is.na(df$Date1), NA, as.numeric(difftime(as.Date(df$Date1), as.Date(df$Date2), units = "days")))
  macce_cond = ars_val_dat$macce.incident == 1
  ars_val_dat[macce_cond,'macce_obs_time'] = as.numeric(difftime(as.Date(ars_val_dat[macce_cond,'macce']), as.Date(ars_val_dat[macce_cond,'date_end_accel_raw']), units = "days")/365)
  death_censor = (ars_val_dat$mortailty == 1) &  (ars_val_dat$macce.incident ==0)
  ars_val_dat[death_censor,'macce_obs_time'] =  as.numeric(difftime(as.Date(ars_val_dat[death_censor,'date_of_death']),as.Date(ars_val_dat[death_censor,'date_end_accel_raw']),units = "days")/365)
  
  
  
  
  #drop prev
  dim(ars_val_dat)
  ars_val_dat = ars_val_dat[ars_val_dat$cad.prevalent == 0,]
  dim(ars_val_dat)
  ars_val_dat = ars_val_dat[ars_val_dat$mace.prevalent == 0,]
  dim(ars_val_dat)
  ars_val_dat = ars_val_dat[ars_val_dat$qrisk.prevalent == 0,]
  dim(ars_val_dat)
  ars_val_dat = ars_val_dat[ars_val_dat$macce.prevalent == 0,]
  dim(ars_val_dat)
  ars_val_dat = ars_val_dat[ars_val_dat$cvd.prevalent == 0,]
  dim(ars_val_dat)
  
  ars_val_dat = ars_val_dat[ars_val_dat$gp_macce.prevalent == 0,]
  dim(ars_val_dat)
  
  ars_val_dat <- assign_lab(ars_val_dat, 9.0, event = 'macce', lab = 'macce_9y')
  sum(ars_val_dat$macce_9y)
  median(ars_val_dat$macce_obs_time)
  
  ###Step 2 modelo derivation ###
  # Now we will consider applying shrinkage and variable selection during the 
  # model estimation by using the lasso. This requires the use of the glmnet package
  
  # load libraries
  library(glmnet)
  library(mfp)
  library(reticulate)
  
  
  ars_val_dat <- merge(ars_val_dat, ukbc[, c("eid", 'sbp_aut_0_0','sbp_aut_0_1')], by = "eid")
  ars_val_dat <- merge(ars_val_dat, ukbc[, c("eid", 'sbp_man_0_0','sbp_man_0_1')], by = "eid")
  
  #is.na(swiss_re$'sbp_aut_0_0')
  #swiss_re <- swiss_re[!is.na(swiss_re$'sbp_aut_0_0') & !is.na(swiss_re$'sbp_aut_0_0'),]
  dim(ars_val_dat)
  
  #apply(df, 1, get_country_for_city,dict)
  ars_val_dat[,'sbp5'] <- apply(ars_val_dat,1,calculate_variance,'sbp_aut_0_0','sbp_aut_0_1')
  sum(is.na(ars_val_dat$sbp5))
  
  
  
  ars_val_dat$Smoking_binary <- as.integer(ars_val_dat$Smoking == 'Current')
  
  #mark missing data with NA
  columns_to_check <- c("eid","Age", "Sex", "BodyMassIndex", "Diabetes", "diabetes.type1.prevalent",
                        "diabetes.type2.prevalent", "Essential..primary..hypertension.prevalent", "SystolBloodPressur",
                        "TownsendIndex", "CholesRatio", "Corticosteroid.prevalent", "Migraine.prevalent",
                        "Rheumatoid.arthritis.prevalent", "Systemic.lupus.erythematosus.prevalent",
                        "Severe.mental.illnes.prevalent", "Atrial.fibrillation.and.flutter.prevalent", "sbp5",
                        "CVDFamilyHistory", "Ethnicity", "Smoking", "Smoking_binary", "Cholesterol", "HdlCholesterol",
                        "Chronic.kidney.disease.prevalent", "macce_obs_time", "macce.incident", "date_end_accel_raw",
                        "mortailty")
  
  # Replace empty strings "" with NA in specific columns
  ars_val_dat <- ars_val_dat %>%
    mutate_at(vars(columns_to_check), ~ ifelse(. == "", NA, .))
  
  #if we want gp data
  if(data_soruce == 'gp'){
    #loads hdl, hdl_ratio, ,cholestrol ,SBP and SD(SBP) within 5 years  
    gp_df <- load_gp_records(event_df = ars_val_dat)
    
    ars_val_dat <- replace_ukb_cols(ars_val_dat ,gp_df )
    dim(ars_val_dat)
    print(colnames(ars_val_dat))
    
  }
  
  #Factroise multi-class covars
  print('pre-factor')
  print(ars_val_dat$Smoking )
  ars_val_dat$Smoking <- factor(ars_val_dat$Smoking)
  print('post factor')
  print(ars_val_dat$Smoking )
  
  ars_val_dat$Ethnicity <- factor(ars_val_dat$Ethnicity)
  ars_val_dat$Sex <- ifelse(ars_val_dat$Sex == "Female", 0, 1)
  ars_val_dat$CVDFamilyHistory  <- ifelse(ars_val_dat$CVDFamilyHistory  == "False", 0, 1)
  ars_val_dat$Ethnicity <- ifelse(ars_val_dat$Ethnicity == "White", 0, 1)
  ars_val_dat$Diabetes <- ifelse(ars_val_dat$Diabetes  == "True", 1, 0)
  ars_val_dat <- ars_val_dat[ars_val_dat$StepsDayMedAdjusted >0 ,]
  dim(ars_val_dat)
  
  dim(ars_val_dat)
  ars_val_dat <- ars_val_dat %>% dplyr::rename(ActivityRiskScore = predicted)
  return(ars_val_dat)
}


