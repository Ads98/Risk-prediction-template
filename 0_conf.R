#GLOBAL configuartion varibles
#dat soruce
split<- 0 #select which split we wish to evaluate 
folds <-5
eval_time <- 6
soruce <- 'ukb'




args <- commandArgs(TRUE)
model <- 'qrisk'
store <- '/ukb_models/cut_off/cv/stacked/qrisk_ars_fp'

fit_poly <- TRUE
model_type <- 'ars'
data_source <- 'ukb'
fp_terms <- c('StepsDayMedAdjusted','ModerVigorActivity','LightActivity','SedentaryActivity','AccOverallAvg') #SleepActivity c('ActivtyRiskScore')#

age_interact <- TRUE
