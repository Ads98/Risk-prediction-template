# load libraries
library(pmsampsize)
library(survival)
library(boot)
library(rms)
library(flexsurv)
library(mfp)
library(glmnet)
library(rms)
#library(survcomp)
library(lubridate)
library(dplyr)
library(purrr)
library(glmnet)
library(mfp)
library(reticulate)



source(file.path("risk_scores/primary_care/",'read_gp.R'), echo = T)
source(file.path("risk_scores/ARS/",'approximate_R2.R'), echo = T)
#source(file.path("birm_rp/Packages to Install/ R Install Files/",'stdca.R'), echo = T)


###Step1 load data and sample size estimation###


# Load necessary library
library(lubridate)

# Define the function
get_inc_10y <- function(df, dis, entry_col = 'DatAttendAssessCent', adjust = FALSE) {
  # Initialize the incident column to 0
  df[, paste0(dis, '_10y')] <- 0
  
  # Calculate the condition for setting the incident column to 1
  if(adjust){
    cond <- (df[,dis] > df[,entry_col]) & (as.numeric(difftime(df[,dis], df[,entry_col], units = "days")) / 365.25 <= 10)
  }
  else{
    cond <- (df[,dis] > df[,entry_col]) & (as.numeric(difftime(df[,dis], df[,entry_col], units = "days")) / 365.25 <= 10)
  }
  
  # Set the incident column to 1 where the condition is true
  df[cond, paste0(dis, '_10y')] <- 1
  
  # Return the updated incident column
  return(df[, paste0(dis, '_10y')])
}

load_ref_dict <- function(dict_loc, ...) {
  #data <- read.csv(dict_loc, header = FALSE, skip = 1, ...)
  data <- read.csv(dict_loc,header = FALSE, skip = 1,colClasses="character",na.strings="?")
  print("hello")
  print(data)
  data['FixedDate'] <- as.Date(data$V2,format="%Y/%m/%d")
  print(data)
  
  named_vector <- as.vector(data$V2)
  
  names(named_vector) <- data$V1
  print(named_vector)
  return(named_vector)
}




allign_fu <- function(df, label, obs_col, censor_date,suffix = '.incident') {
 
  
  # Adjust labeling to match censoring date
  incident_cond <- (df[,paste0(label, suffix)] == 1) & (df[,label] > df[,censor_date])
  df[incident_cond, label] <- NA
  df[incident_cond, paste0(label, suffix)] <- 0
  
  print(sum(incident_cond))
  # Adjust mortality labeling for new censoring date
  death_cond <- (df[,'mortailty'] == 1) & (df[,'date_of_death'] > df[,censor_date])
  df[death_cond, 'date_of_death'] <- NA
  df[death_cond,'mortailty'] <- 0 
  
  # Adjust latest administrative censoring date
  print(df[!(is.na(df[,label])),label])
  print(censor_date)
  date_cond <- df[!(is.na(df[,label])),label] > df[!(is.na(df[,label])),censor_date]
  #date_cond <-   df[!(is.na(df[,label])),label] > censor_date 
  # Replace NA values with FALSE in your matrix or vector
  date_cond[is.na(date_cond)] <- FALSE
  
  print(date_cond)
  print("hello")
  df[date_cond, label] <- ''
  print('set date ')
  lab_cond = df[,paste0(label,suffix)] == 0
  df[lab_cond, obs_col] <- as.numeric(difftime(as.Date(df[lab_cond,censor_date]) ,as.Date(df[lab_cond,'date_end_accel_raw'])),units = 'days')/365
  
  return(df)
}

sensitivity_censor <- function(df, label, obs_col, censor_date) {
  censor_date <- as.Date(censor_date)  # Convert censor_date to datetime
  
  # Adjust labeling to match censoring date
  incident_cond <- (df[,paste0(label, '.incident')] == 1) & (df[,label] > censor_date)
  df[incident_cond, label] <- NA
  df[incident_cond, paste0(label, '.incident')] <- 0
  
  # Adjust mortality labeling for new censoring date
  death_cond <- (df[,'mortailty'] == 1) & (df[,'date_of_death'] > censor_date)
  df[death_cond, 'date_of_death'] <- NA
  df[death_cond,'mortailty'] <- 0 
  
  # Adjust latest administrative censoring date
  print(df[!(is.na(df[,label])),label])
  print(censor_date)
  date_cond <- df[!(is.na(df[,label])),label] > censor_date
  #date_cond <-   df[!(is.na(df[,label])),label] > censor_date 
  # Replace NA values with FALSE in your matrix or vector
  date_cond[is.na(date_cond)] <- FALSE
  
  print(date_cond)
  print("hello")
  df[date_cond, label] <- ''
  print('set date ')
  print(censor_date)
  print(colnames(df))
  print(label)
  df[df[,paste0(label, '.incident')] == 0, obs_col] <- as.numeric(difftime(as.Date(censor_date) ,as.Date(df[,'date_end_accel_raw'])),units = 'days')/365
  
  return(df)
}
get_country_for_city <- function(row,dict) {
  city <- row['AssessCenter']
  #print(dict)
  #print(names(dict))
  if (city %in% names(dict)) {
    country <- dict[[city]]
    return(country)
  } else {
    return("Not Found")
  }
}

get_date <- function(row,end_dates){
  print("start")
  print(df)
  censor <- row['date_end_accel_raw']
  print(end_dates)
  print(row['AssessCenter'])
  print(as.Date(end_dates[row['AssessCenter']]))
  print(as.Date(censor))
  obs_date <-as.numeric(difftime(as.Date(end_dates[row['AssessCenter']]),as.Date(censor)))
  print('obs date')
  print(obs_date)
  print('censor:')
  print(censor)
  print("row")
  print(row['AssessCenter'])
  print('end date:')
  print(end_dates[row['AssessCenter']])
  print(obs_date)
  return(obs_date/365)
}
censoring <- function(df, obs_col, suffix = "") {
  date_parser <- function(x) as_datetime(x, format = "%Y/%m/%d", tz = "UTC", na.rm = TRUE)
  
  dict <- load_ref_dict('risk_scores/ARS/AssessCentre.csv')

  df$country <- apply(df, 1, get_country_for_city,dict)
  end_dates <- load_ref_dict(paste0('risk_scores/ARS/CensorDates', '_ukb', '.csv'))
  print('ending')
  print(end_dates)
  df$macce_obs_time <- apply(df, 1, get_date,end_dates)
  
  return(df)
}



assign_lab <- function(df, time, event = 'qrisk', lab = 'qrisk_10y') {
  df[,lab] <- 0
  cond <- (df[[paste0(event, '.incident')]] == 1) & (df[[paste0(event, '_obs_time')]] <= time)
  df[cond, lab] <- 1
  return(df)
}


# Define a function to calculate incident cases
get_inc <- function(df,dis, entry_col = 'DatAttendAssessCent',adjust = FALSE) {
  df[,paste0(dis, '.incident')] <- 0
  if(adjust){
    cond <- df[,dis] > df[,entry_col]
  }
  else{
    cond <- df[,dis] > df[,entry_col]  
  }
  df[cond, paste0(dis, '.incident')] <- 1
  return(df[,paste0(dis, '.incident')])
}



# Define a function to calculate prevalent cases
get_prev <- function(df,dis, entry_col = 'DatAttendAssessCent') {
  print('hello')
  df[,paste0(dis, '.prevalent')] <- 0
  print('its me')
  cond <- df[,dis] <= df[,entry_col]  & df[,dis] != ''
  print('your looking for')
  df[cond, paste0(dis, '.prevalent')] <- 1
  print('i remember all the times')
  return(df[,paste0(dis, '.prevalent')])
}

# Function to calculate variance between a and b for each row
calculate_variance <- function(row,v1,v2) {
  print(v1)
  print(v2)
  if(!is.na(row[v1]) & !is.na(row[v2])){
  #return((as.numeric(row[v1]) - as.numeric(row[v2])^2))
    return(sd(c(row[v1],row[v2])))
  }
  else{
    return(NA)
  }
}

load_data<- function(split,folds,data_soruce,stack_splits = FALSE){
  censor_date = NULL
  cens_file = '_ukb'
  suffix = '.incident'
  
  baseline_measures <- c("eid",'Age' ,'Sex', 'BodyMassIndex' ,'Diabetes', 'SystolBloodPressur' , 'TownsendIndex' , 'CholesRatio' , 
                'CVDFamilyHistory', 'Ethnicity' , 'Smoking','AssessCenter','AccOverallAvg','SleepActivity','ModerVigorActivity','LightActivity','SedentaryActivity') 
  
  dis_list <- c('Essential..primary..hypertension', 'Migraine', 'Rheumatoid.arthritis', 'Systemic.lupus.erythematosus', 'Severe.mental.illnes',
                'Chronic.kidney.disease', 'Atrial.fibrillation.and.flutter', 'Corticosteroid', 'diabetes.type1', 'diabetes.type2','macce',
                'cad','mace','qrisk','gp_macce','cvd')
  
  #Read UK-Biobank datsets
  ukbc <- read.csv("risk_scores/macce_all.csv")#compelte tabular data
  cvd_prs <- read.csv("risk_scores/cvd_prs.csv") #CVD ploygenic risks cores
  train<- read.csv("risk_scores/ARS/dataset_deriv_Sep2021.csv") #Acceroemeter cohort part 1 
  deriv <- read.csv("risk_scores/ARS/dataset_test_Sep2021.csv") #Acceroemeter cohort part 2
  full_set <- rbind(train,deriv) # Full accelerometer cohort
  
  #if(data_soruce == 'gp'){
  #no_prev , macce_cv 
  cv_path <- paste0('risk_scores/ARS/nested_cv/macce_cv/split') #Read specfied dervation set from Netested-CV (Split pre-specfied in conf file)
  if(stack_splits){
    res_file <- 'predicted_test.csv'
    folds <-10
    score_col <- 'predicted'
  } else{
    res_file <- '/val_pred_'
    score_col <- 'predicted'
  }
  swiss_re <- data.frame()
  oof <- list() #Read out of fold ARS estimates
  for(i in 1:folds){
    if(stack_splits){
      oof[[i]] <- read.csv(paste0(cv_path,i-1,'/',res_file))
    }else{
      oof[[i]] <- read.csv(paste0(cv_path,split,res_file,i-1,'.csv'))
    }
    
    swiss_re <- rbind(swiss_re, oof[[i]] )
  }
    swiss_re <- merge(swiss_re, full_set[, baseline_measures],by = "eid",all.x = TRUE) #combine to form dervation set
#  }else{
#    swiss_re <- read.csv("risk_scores/ARS/dataset_deriv_Sep2021.csv")
#  }
  dim(swiss_re)
  swiss_re <- merge(swiss_re, ukbc[, c("eid", 'DatAttendAssessCent','UkBiobankAssessCent','date_of_death','mortailty','HdlCholesterol','Cholesterol',dis_list)],by = "eid",all.x = TRUE) # Merge with dates of interest
  dim(swiss_re)
  
  
  
  # Fill missing "chol" values with "hdl" times "ratio"
  chol_na = is.na(swiss_re$Cholesterol)
  hdl_na  = is.na(swiss_re$HdlCholesterol)
  ratio_na= is.na(swiss_re$CholesRatio)
  
  swiss_re[chol_na,'Cholesterol'] <- swiss_re[chol_na,'HdlCholesterol']   * swiss_re[chol_na,'CholesRatio'] 
  swiss_re[hdl_na,'HdlCholesterol'] <- swiss_re[hdl_na,'Cholesterol']   / swiss_re[hdl_na,'CholesRatio'] 
  swiss_re[ratio_na,'CholesRatio'] <- swiss_re[ratio_na,'Cholesterol']   / swiss_re[ratio_na,'HdlCholesterol'] 
  
  
  #Read in manualy extracted accerometer features such as steps and sleep activivty
  dat_cov <- read.csv("/well/doherty/projects/scratch/swissre/shared_data/covariates/covariate_data_2023_02_16.csv") 
  step_data <- read.csv('/well/doherty/users/gji710/steps_cancer/data/rap_data/stepcount2.1.5_20230615.csv')
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
  
  #Add steps and polygenic risk scores to our acceleromter datasdet
  dim(swiss_re)
  swiss_re <- merge(swiss_re, dat_cov[, c("eid", 'date_end_accel_raw')],by = "eid")
  dim(swiss_re)
  swiss_re <- merge(swiss_re, step_data[, c("eid", 'StepsDayMedAdjusted')],by = "eid")
  dim(swiss_re)
  swiss_re <- merge(swiss_re, cvd_prs[, c("eid", 'p26223')],by = "eid",all.x = TRUE)
  dim(swiss_re)
  names(swiss_re)[names(swiss_re) == "p26223"] <- "CVD_PGRS"
  
  #Loop through diseases  instances
  
  for (dis in dis_list) {
    cat(dis, "\n")
    swiss_re[,paste0(dis, '.prevalent')] <- get_prev(swiss_re,dis,entry_col = 'date_end_accel_raw' )
    print("hgot prev")
    swiss_re[,paste0(dis, '.incident')] <- get_inc(swiss_re,dis ,entry_col= 'date_end_accel_raw')
  }
  
  dim(swiss_re)
  #Allign censoring dates
  #swiss_re['censor_date'] <- as.Date(swiss_re$date_end_accel_raw) %m+% years(6) #May no longer be needed when using GP data speak to Charlie,Aiden and Derick
  swiss_re = censoring(swiss_re,'macce_obs_time',suffix = cens_file) #Censors invdiauls based on MACCE event and mortailty
  
  print('Alligning dates base don selected cesnoring')
  if(!(is_null(censor_date))){
    swiss_re = sensitivity_censor(swiss_re,'macce','macce_obs_time',censor_date)
  }
  print('Adjusting for mortailty')
  #df$TimeDifference <- ifelse(is.na(df$Date1), NA, as.numeric(difftime(as.Date(df$Date1), as.Date(df$Date2), units = "days")))
  #Accounts for loss of follow-up due to mortailty
  macce_cond = swiss_re$macce.incident == 1
  swiss_re[macce_cond,'macce_obs_time'] = as.numeric(difftime(as.Date(swiss_re[macce_cond,'macce']), as.Date(swiss_re[macce_cond,'date_end_accel_raw']), units = "days")/365)
  death_censor = (swiss_re$mortailty == 1) &  (swiss_re$macce.incident ==0)
  swiss_re[death_censor,'macce_obs_time'] =  as.numeric(difftime(as.Date(swiss_re[death_censor,'date_of_death']),as.Date(swiss_re[death_censor,'date_end_accel_raw']),units = "days")/365)
  
  
  
  
  #drop prev CDV cases
  dim(swiss_re)
  swiss_re = swiss_re[swiss_re$cad.prevalent == 0,]
  dim(swiss_re)
  swiss_re = swiss_re[swiss_re$mace.prevalent == 0,]
  dim(swiss_re)
  swiss_re = swiss_re[swiss_re$qrisk.prevalent == 0,]
  dim(swiss_re)
  swiss_re = swiss_re[swiss_re$macce.prevalent == 0,]
  dim(swiss_re)
  swiss_re = swiss_re[swiss_re$cvd.prevalent == 0,]
  dim(swiss_re)
  
  swiss_re = swiss_re[swiss_re$gp_macce.prevalent == 0,]
  dim(swiss_re)
  
  swiss_re <- assign_lab(swiss_re, 9.0, event = 'macce', lab = 'macce_9y')
  
  
  
  swiss_re <- merge(swiss_re, ukbc[, c("eid", 'sbp_aut_0_0','sbp_aut_0_1')], by = "eid")
  swiss_re <- merge(swiss_re, ukbc[, c("eid", 'sbp_man_0_0','sbp_man_0_1')], by = "eid")
  
  dim(swiss_re)
  
  swiss_re[,'sbp5'] <- apply(swiss_re,1,calculate_variance,'sbp_aut_0_0','sbp_aut_0_1')
  sum(is.na(swiss_re$sbp5))
  dim(swiss_re)
  
  # Replace empty strings "" with NA in specific columns
  swiss_re$Smoking_binary <- as.integer(swiss_re$Smoking == 'Current')
  #mark missing data with NA
  columns_to_check <- c("eid","Age", "Sex", "BodyMassIndex", "Diabetes", "diabetes.type1.prevalent",
                        "diabetes.type2.prevalent", "Essential..primary..hypertension.prevalent", "SystolBloodPressur",
                        "TownsendIndex", "CholesRatio", "Corticosteroid.prevalent", "Migraine.prevalent",
                        "Rheumatoid.arthritis.prevalent", "Systemic.lupus.erythematosus.prevalent",
                        "Severe.mental.illnes.prevalent", "Atrial.fibrillation.and.flutter.prevalent", "sbp5",
                        "CVDFamilyHistory", "Ethnicity", "Smoking", "Smoking_binary", "Cholesterol", "HdlCholesterol",
                        "Chronic.kidney.disease.prevalent", "macce_obs_time", "macce.incident", "date_end_accel_raw",
                        "mortailty")
  
  
  swiss_re <- swiss_re %>%
    mutate_at(vars(columns_to_check), ~ ifelse(. == "", NA, .))
  dim(swiss_re)
  
  #Factroise multi-class covars
  print('pre-factor')
  print(swiss_re$Smoking )
  swiss_re$Smoking <- factor(swiss_re$Smoking)
  print('post factor')
  print(swiss_re$Smoking )
  
  print('factorise')
  swiss_re$Ethnicity <- factor(swiss_re$Ethnicity)
  swiss_re$Sex <- ifelse(swiss_re$Sex == "Female", 0, 1)
  swiss_re$CVDFamilyHistory  <- ifelse(swiss_re$CVDFamilyHistory  == "False", 0, 1)
  swiss_re$Ethnicity <- ifelse(swiss_re$Ethnicity == "White", 0, 1)
  swiss_re$Diabetes <- ifelse(swiss_re$Diabetes  == "True", 1, 0)
  swiss_re <- swiss_re[swiss_re$StepsDayMedAdjusted >0 ,]
  
  #convert activty % to hours
  print('24 hour PA')
  print(sum(swiss_re['SleepActivity']))
  print(sum(swiss_re['ModerVigorActivity']))
  print(sum(swiss_re['SedentaryActivity']))
  print(sum(swiss_re['LightActivity']))
  
  epsilon <- 1e-6 
  swiss_re['SleepActivity'] <- (swiss_re$SleepActivity/100) *24
  swiss_re['ModerVigorActivity']<- ((swiss_re$ModerVigorActivity/100) *24) + epsilon 
  swiss_re['SedentaryActivity'] <- (swiss_re$SedentaryActivity/100) *24
  swiss_re['LightActivity'] <- (swiss_re$LightActivity/100) *24
  print('got 24hr PA')
  
  dim(swiss_re)


  
  #loads hdl, hdl_ratio, ,cholestrol ,SBP and SD(SBP) within 5 years  
  if(data_soruce == 'gp'){
    gp_df <- load_gp_records(event_df = swiss_re)
    
    #if we want gp data
    #gp_df <- merge(swiss_re, gp_df, by = "eid", all.x = TRUE)
    swiss_re <- replace_ukb_cols(swiss_re ,gp_df )
    dim(swiss_re)
    print(colnames(swiss_re))
   # print(swiss_re$ActivityRiskScore)
    
  }
  print('ARS name')
  if(stack_splits){
    print('rename stacked ARS')
    swiss_re <- swiss_re %>% dplyr::rename(ActivityRiskScore = predicted)
    
  }else{
    print('rename split ARS')
    swiss_re <- swiss_re %>% dplyr::rename(ActivityRiskScore = Predictions)
  }
  print('post-gp')
  print(colnames(swiss_re))
  print(swiss_re$Smoking )
  
  attach(swiss_re)
  return(swiss_re)
}

