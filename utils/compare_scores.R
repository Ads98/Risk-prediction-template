###Test for signficnat diafferances between models for each metrics
#save(list = ls(all.names = TRUE), file = paste0('risk_scores/ch6_nb_10.RData'))
#save(list = ls(all.names = TRUE), file = paste0('risk_scores/ch6_reclass_ahs_pa.RData'))
source("forest_table_manual.R") #change source path
source(file.path("risk_scores/array_job/9_year_censoring/asx_strat_cent",'ARS_utils.R'), echo = T)
source(file.path("risk_scores/array_job/9_year_censoring",'res_utils.R'), echo = T)

source(file.path("risk_scores/array_job/9_year_censoring",'get_metrics.R'), echo = T)


# Load in UK Biobank data
#load(file = "data/ukb_data_case.Rdata")
met <- 'nri_cont'
th <- 0.1
var_th <- ''
if(met == 'c_idx'){
  base <- 'c_index'
  func <- get_apaprent_c
}else if (met == 'ici') {
  base <- 'ICI'
  func <- get_apaprent_ici
}else if (met == 'nb') {
  base <- 'Net benefit'
  func <- get_apaprent_nb
} else if (met == 'nri') {
  base <- 'NRI'
  func <- get_apaprent_nri
}else if (met == 'nri_cont') {
  base <- 'NRI'
  func <- get_apaprent_nri_cont
}else{
  base <- met
}

data_source <- 'ukb'
model <-'qrisk'
asx_suffix <-'fp'
asx_type <- 'ars'
asx_terms <- c("ActivityRiskScore")
asx_terms_pa <- c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg")
outcome <- 'macce'
#results paths
store_asx <- paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome,"/asx_",asx_type,'_',asx_suffix)
store_asx_pa <- paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome,"/asx_",'pa','_','lin')

store_asx_old <- paste0("/ukb_models/cut_off/cv/stacked/asx/asx_",asx_type,'_',asx_suffix)
store_ars <- paste0('/',data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome,"/",model,"_ars_fp")
store_ars_old <- paste0('/',data_source,"_models/cut_off/cv/stacked/all_nri/",outcome,"/",model,"_ars_fp")
store_pa <- paste0('/',data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome,"/",model,"_pa_lin")

#Load asx results
#asx_ terms <- c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg")

lienar_predictions_asx <- get_adjusted_lp(env_path,store_asx,'asx', asx_type,'ukb',acc_terms =  asx_terms,condensed_model = FALSE )
lienar_predictions_asx_pa <- get_adjusted_lp(env_path,store_asx_pa,'asx', 'pa','ukb',acc_terms =  asx_terms_pa,condensed_model = FALSE )

full_asx <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/')) #" array.
full_asx_pa <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/')) #" array.
full_asx_old <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_old,'/')) #" array.  
rows_with_na <-c()
rows_with_na <- null_rows(full_asx)

#load pa only ahs
store_pa_only <- "/ukb_models/cut_off/cv/stacked/pa_only/center/acc_ars_fp"
lienar_predictions_pa_only <- get_adjusted_lp(env_path,store_pa_only,'acc', 'ars','ukb',acc_terms =  c("ActivityRiskScore"),condensed_model = FALSE )


#pa only multivar 
store_pa_only_mv <- "/ukb_models/cut_off/cv/stacked/pa_only/center/acc_pa_lin"
lienar_predictions_pa_only_mv <- get_adjusted_lp(env_path,store_pa_only_mv,'acc', 'pa','ukb',acc_terms =  c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),condensed_model = FALSE )





#laod qrisk3 results
lienar_predictions_pa <- get_adjusted_lp(env_path,store_pa,model, 'pa',data_source,acc_terms =  c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),condensed_model = TRUE )

#DL ARS results
lienar_predictions_ars <- get_adjusted_lp(env_path,store_ars,model, 'ars',data_source,acc_terms = c('ActivityRiskScore'),condensed_model = TRUE )




#legacy ars for comaprison
#DL ARS results
lienar_predictions_ars_old <- get_adjusted_lp(env_path,store_ars_old,model, 'ars',data_source,acc_terms = c('ActivityRiskScore'),condensed_model = TRUE )



df_asx <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = pate0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome),
  model = "asx",
  split = "",
  model_type = asx_type,
  store_suffix = asx_suffix,
  data_source = "ukb",
  metric = "c_idx",
  acc_terms = asx_terms,
  condensed_model = FALSE)

df_asx_pa <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome),
  model = "asx",
  split = "",
  model_type = 'pa',
  store_suffix = 'lin',
  data_source = "ukb",
  metric = "c_idx",
  acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = FALSE)

df_pa_only <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/pa_only/center/",outcome),
  model = "acc",
  split = "",
  model_type = "pa",
  store_suffix = "lin",
  data_source = "ukb",
  metric = "c_idx",
  acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = FALSE)

df_ars_only <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/pa_only/center/",outcome),
  model = "acc",
  split = "",
  model_type = "ars",
  store_suffix = "fp",
  data_source = "ukb",
  metric = "c_idx",
  acc_terms = c('ActivityRiskScore'),#c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = FALSE)


df_pa <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/",data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome),
  model =model,
  split = "",
  model_type = "pa",
  store_suffix = "lin",
  data_source = data_source,
  metric = "c_idx",
  acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = TRUE)

df_ars <- get_predictions(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store =  paste0("/",data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome),
  model = model,
  split = "",
  model_type = "ars",
  store_suffix = "fp",
  data_source = data_source,
  metric = "c_idx",
  acc_terms = c("ActivityRiskScore"),
  condensed_model = TRUE)

##function to calculated generic 

observed_delta <- function(func, args_list1, args_list2, optims1, optims2){
  print('app1')
  app_1 <- do.call(func, args_list1)
  print('app2')
  app_2 <- do.call(func, args_list2)
  
  print(paste0('app1 = ',app_1))
  print(paste0('app2 = ',app_2))
  
  adj_1 <- app_1 - mean(optims1)
  adj_2 <- app_2 - mean(optims2)
  
  adj_ci_1 <- round(pool_ci(app_1- optims1),3)
  adj_ci_2 <- round(pool_ci(app_2- optims2),3)
  
  print(paste0('adj_1 = ',round(adj_1,3),"( ",  adj_ci_1[1]," - ",  adj_ci_1[2],")"))
  print(paste0('adj_1 = ',round(adj_2,3),"( ",  adj_ci_2[1]," - ",  adj_ci_2[2],")"))
  #print(paste0('adj_2 = ',adj_2))
  
  return(adj_2 - adj_1)
  #nri_cont_adj_f <- round(nri_cont_app - nri_cont_shrink[[3]],3)
}

#get optimism adjusted C-idx and 95% CI



result_pa_only <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/pa_only/center/",outcome),
  model = "acc",
  split = "",
  model_type = "pa",
  store_suffix = "lin",
  data_source ='ukb',
  metric = met,
  acc_terms = c('StepsDayMedAdjusted','ModerVigorActivity','LightActivity','SedentaryActivity','AccOverallAvg'),
  condensed_model = FALSE
)


result_ars_lin_only <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/pa_only/center/",outcome),
  model = "acc",
  split = "",
  model_type = "ars",
  store_suffix = "lin",
  data_source ='ukb',
  metric = met,
  acc_terms = c('ActivityRiskScore'),#c('StepsDayMedAdjusted','ModerVigorActivity','LightActivity','SedentaryActivity','AccOverallAvg'),
  condensed_model = FALSE
)


result_ars_only <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/pa_only/center/",outcome),
  model = "acc",
  split = "",
  model_type = "ars",
  store_suffix = "fp",
  data_source ='ukb',
  metric = met,
  acc_terms = c('ActivityRiskScore'),#c('StepsDayMedAdjusted','ModerVigorActivity','LightActivity','SedentaryActivity','AccOverallAvg'),
  condensed_model = FALSE
)


#get optimism adjusted C-idx and 95% CI
result_asx_old <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome),
  model = "asx",
  split = "",
  model_type = asx_type,
  store_suffix = asx_suffix,
  data_source = 'ukb',
  metric = met,
  acc_terms = asx_terms,
  condensed_model = FALSE
)

result_asx_pa <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome),
  model = "asx",
  split = "",
  model_type = 'pa',
  store_suffix = 'lin',
  data_source = 'ukb',
  metric = met,
  acc_terms = asx_terms_pa,
  condensed_model = FALSE
)


result_asx <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/ukb_models/cut_off/cv/stacked/all_nri/9y/asx/center/",outcome),
  model = "asx",
  split = "",
  model_type = asx_type,
  store_suffix = asx_suffix,
  data_source = 'ukb',
  metric = met,
  acc_terms = asx_terms,
  condensed_model = FALSE
)

for(i in 1:nrow(result_asx)){
  print(result_asx[i,'metric'])
  print(paste0(result_asx[i,'mean_value'],'(',result_asx[i,'ci_low'],' - ',result_asx[i,'ci_high'],')'))
  cat('\n')
}



sex_cond <- imputed_datasets[[1]]$Sex == 1





#get optimism adjusted C-idx and 95% CI
result_pa <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = paste0("/",data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome),
  model = model,
  split = "",
  model_type = "pa",
  store_suffix = "lin",
  data_source = data_source,
  metric = met,
  acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = TRUE
)

for(i in 1:nrow(result_pa)){
  print(result_pa[i,'metric'])
  print(paste0(result_pa[i,'mean_value'],'(',result_pa[i,'ci_low'],' - ',result_pa[i,'ci_high'],')'))
  cat('\n')
}








#get optimism adjusted C-idx and 95% CI
result_ars <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store =  paste0("/",data_source,"_models/cut_off/cv/stacked/all_nri/9y/center/",outcome),
  model = model,
  split = "",
  model_type = "ars",
  store_suffix = "fp",
  data_source =data_source,
  metric = met,
  acc_terms = c("ActivityRiskScore"),
  condensed_model = TRUE
)
for(i in 1:nrow(result_ars)){
  print(result_ars[i,'metric'])
  print(paste0(result_ars[i,'mean_value'],'(',result_ars[i,'ci_low'],' - ',result_ars[i,'ci_high'],')'))
  cat('\n')
}


#get optimism adjusted delta aginst age
full_pa_only <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa_only,'/')) #" array.  


full_pa_only_mv <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa_only_mv,'/')) #" array.  

full_pa <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/')) #" array.  
cond_pa <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/',store_pa,'/')) # array.


full_ars <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/')) #" array.  
cond_ars <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/',store_ars,'/')) # array. 

full_ars_old <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars_old,'/')) #" array.  
cond_ars_old <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/',store_ars_old,'/')) # array. 

#calculate optims for each model
th <- 0.1
var_th <- '_over_75'
optims_pa_only <- optim_dist(full_pa_only,rows_with_na,met = paste0(met,var_th))
optims_pa_only_mv <- optim_dist(full_pa_only_mv,rows_with_na,met = paste0(met,var_th))

optims_asx <- optim_dist(full_asx,rows_with_na,met = paste0(met,var_th))
optims_asx_pa <- optim_dist(full_asx_pa,rows_with_na,met = paste0(met,var_th))
#optims_asx_old <- optim_dist(full_asx_old,rows_with_na,met = paste0(met,var_th))
#get optimism adjusted delta aginst age

rows_with_na <-c()
rows_with_na <- null_rows(full_pa)
optims_pa <- optim_dist(full_pa,rows_with_na,met = paste0(met,var_th))
rows_with_na <-c()
rows_with_na <- null_rows(cond_pa)
optims_pa_cond <- optim_dist(cond_pa,rows_with_na,met = paste0(met,var_th))
rows_with_na <-c()
rows_with_na <- null_rows(full_ars)
optims_ars <- optim_dist(full_ars,rows_with_na,met = paste0(met,var_th))
rows_with_na <-c()
rows_with_na <- null_rows(cond_ars)
optims_ars_cond <- optim_dist(cond_ars,rows_with_na,met =  paste0(met,var_th))

#set codntion based on age and sex subroups
if (grepl("_under_55", var_th)) {
  print('under55')
  age_cond <- imputed_datasets[[1]]$age < 55
} else if (grepl("_55_65", var_th)) {
  print('55-65')
  age_cond <- imputed_datasets[[1]]$age >= 55 & imputed_datasets[[1]]$age < 65
} else if (grepl("_65_75", var_th)) {
  print('65-75')
  age_cond <- imputed_datasets[[1]]$age >= 65 & imputed_datasets[[1]]$age < 75
} else if (grepl("_over_75", var_th)) {
  print('75')
  age_cond <- imputed_datasets[[1]]$age >= 75
} else {
  age_cond <- rep(TRUE,nrow(imputed_datasets[[1]])) # Default case if no match is found
}


#####################ASX#################
#male
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_asx[[1]][[1]],sex_cond & age_cond)
  args_list2  <- list(lienar_predictions_asx[[1]][[2]],sex_cond & age_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond & age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_asx_pa, sex_cond = sex_cond & age_cond,score_col = 'ars')
}else if (met =='nb') {
  print('NB')
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond & age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_asx_pa, sex_cond = sex_cond & age_cond,rs = 'ars',th = th)
} else if (met =='nri'|met == 'nri_cont') {
  print('NRI')
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond & age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
  args_list2 <- args_list1
}

asx_delta_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 =args_list2,
  optims1 = optims_asx_pa[[1]],
  optims2 = optims_asx_pa[[2]]
)

#obs_1 <- func(lienar_predictions_asx[[1]][[1]],sex_cond) - mean(optims_asx[[1]])
#obs_2 <- func(lienar_predictions_asx[[1]][[2]],sex_cond) - mean(optims_asx[[2]])
#obs_diff = obs_2 -obs_1

obs_diff_asx_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[1]],
  optims2 = optims_asx_pa[[2]])

ci_asx_m <- round(pool_ci(asx_delta_m),digits)
print(paste0(round(mean(asx_delta_m),digits),"(95% CI ",ci_asx_m[1]," - ",ci_asx_m[2],")"))
get_p_val(asx_delta_m,apaprent_diff =obs_diff_asx_m )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_asx_pa[[1]]
  ci_asx_m <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),digits),"(95% CI ",ci_asx_m[1]," - ",ci_asx_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_asx[[1]][[3]],!sex_cond & age_cond)
  args_list2  <- list(lienar_predictions_asx[[1]][[4]],!sex_cond & age_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_asx_pa, sex_cond = !sex_cond & age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_asx_pa, sex_cond = !sex_cond & age_cond,score_col = 'ars')
}else if ((met =='nb')) {
  args_list1 <-   list(df = df_asx_pa, sex_cond = !sex_cond & age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_asx_pa, sex_cond = !sex_cond & age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  print('nri')
  args_list1 <-   list(df = df_asx_pa, sex_cond  = !sex_cond & age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

asx_delta_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[3]],
  optims2 = optims_asx_pa[[4]]
)
obs_diff_asx_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[3]],
  optims2 = optims_asx_pa[[4]])

ci_asx_f <- round(pool_ci(asx_delta_f),digits)
print(paste0(round(mean(asx_delta_f),digits),"(95% CI ",ci_asx_f[1]," - ",ci_asx_f[2],")"))
get_p_val(asx_delta_f,apaprent_diff =obs_diff_asx_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_asx_pa[[3]]
  ci_asx_f <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),digits),"(95% CI ",ci_asx_f[1]," - ",ci_asx_f[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
  
  
}



######################################


#####################QRISK3#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[1]],sex_cond & age_cond)
  args_list2  <- list(lienar_predictions_pa[[1]][[2]],sex_cond & age_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond & age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond & age_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond & age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond & age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont' ) {
  print('nri')
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond& age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}


obs_diff_pa_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_pa[[2]])

q_vs_pa_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_pa[[2]]
)

ci_pa_m <- round(pool_ci(q_vs_pa_m),digits)
print(paste0(round(mean(q_vs_pa_m),digits),"(95% CI ",ci_pa_m[1]," - ",ci_pa_m[2],")"))
get_p_val(q_vs_pa_m,apaprent_diff =obs_diff_pa_m )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_pa[[1]]
  ci_pa_m <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),digits),"(95% CI ",ci_pa_m[1]," - ",ci_pa_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

#female


if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_pa[[1]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond& age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond& age_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond& age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond& age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond& age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

obs_diff_pa_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_pa[[4]])


q_vs_pa_f <- adjusted_delta(
  func = func,
  args_list1 =args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_pa[[4]]
)

ci_pa_f <- round(pool_ci(q_vs_pa_f),digits)
print(paste0(round(mean(q_vs_pa_f),digits),"(95% CI ",ci_pa_f[1]," - ",ci_pa_f[2],")"))
get_p_val(q_vs_pa_f,apaprent_diff =obs_diff_pa_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_pa[[3]]
  ci_pa_f <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_pa_f[1]," - ",ci_pa_f[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

######################################



#####################QRISK3_cond#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[3]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_pa[[3]][[2]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs1 = 'qrisk3_con',rs2 = 'ars_con',th = th)
}

obs_diff_con_pa_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[1]],
  optims2 = optims_pa_cond[[2]])


conv_vs_pa_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 =args_list2,
  optims1 = optims_pa_cond[[1]],
  optims2 = optims_pa_cond[[2]]
)



ci_con_pa_m <- round(pool_ci(conv_vs_pa_m),digits)
print(paste0(round(mean(conv_vs_pa_m),digits),"(95% CI ",ci_con_pa_m[1]," - ",ci_con_pa_m[2],")"))
get_p_val(conv_vs_pa_m,apaprent_diff =obs_diff_con_pa_m )


if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_pa_cond[[1]]
  print('res')
  ci_con_pa_m <- round(pool_ci(nri_dist_adj),3)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_con_pa_m[1]," - ",ci_con_pa_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

#female
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[3]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_pa[[3]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  print('nri')
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs1 = 'qrisk3_con',rs2 = 'ars_con',th = th)
}

obs_diff_con_pa_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[3]],
  optims2 = optims_pa_cond[[4]])

conv_vs_pa_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[3]],
  optims2 = optims_pa_cond[[4]]
)

ci_con_pa_f <- round(pool_ci(conv_vs_pa_f),digits)
print(paste0(round(mean(conv_vs_pa_f),digits),"(95% CI ",ci_con_pa_f[1]," - ",ci_con_pa_f[2],")"))
get_p_val(conv_vs_pa_f,apaprent_diff =obs_diff_con_pa_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_pa_cond[[3]]
  ci_con_pa_f <- round(pool_ci(nri_dist_adj),3)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_con_pa_f[1]," - ",ci_con_pa_f[2],")"))
  
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

######################################



#####################QRISK3_cond vs qrisk#################
#male
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[3]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_pa[[1]][[1]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,score_col = 'qrisk3')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,rs = 'qrisk3',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs1 = 'qrisk3_con',rs2 = 'qrisk3')
}



obs_diff_con_q_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[1]],
  optims2 = optims_pa[[1]])


conv_vs_q_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[1]],
  optims2 = optims_pa[[1]]
)

ci_con_q_m <- round(pool_ci(conv_vs_q_m),digits)
print(paste0(round(mean(conv_vs_q_m),3),"(95% CI ",ci_con_q_m[1]," - ",ci_con_q_m[2],")"))
get_p_val(conv_vs_q_m,apaprent_diff =obs_diff_con_q_m )


#female
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[3]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_pa[[1]][[3]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,score_col = 'qrisk3')
}else if (met =='nb'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,rs = 'qrisk3',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs1 = 'qrisk3_con',rs2 = 'qrisk3')
}

obs_diff_con_q_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[3]],
  optims2 = optims_pa[[3]])

conv_vs_q_f <- adjusted_delta(
  func = func,
  args_list1 =args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_cond[[3]],
  optims2 = optims_pa[[3]]
)


ci_con_q_f <- round(pool_ci(conv_vs_q_f),digits)
print(paste0(round(mean(conv_vs_q_f),3),"(95% CI ",ci_con_q_f[1]," - ",ci_con_q_f[2],")"))
get_p_val(conv_vs_q_f,apaprent_diff =obs_diff_con_q_f )


######################################




#####################QRISK3 vs qrisk_cond + PA#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_pa[[3]][[2]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars_con')
}

obs_diff_pa_con_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_pa_cond[[1]])

cond_vs_pa_cond_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_pa_cond[[2]]
)


ci_pa_con_m <- round(pool_ci(cond_vs_pa_cond_m),digits)
print(paste0(round(mean(cond_vs_pa_cond_m),digits),"(95% CI ",ci_pa_con_m[1]," - ",ci_pa_con_m[2],")"))
get_p_val(cond_vs_pa_cond_m,apaprent_diff =obs_diff_pa_con_m )


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_pa[[3]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_pa, sex_cond = !sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars_con')
}

obs_diff_pa_con_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_pa_cond[[4]])


conv_vs_pa_cond_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_pa_cond[[4]]
)


ci_pa_con_f <- round(pool_ci(conv_vs_pa_cond_f),digits)
print(paste0(round(mean(conv_vs_pa_cond_f),digits),"(95% CI ",ci_pa_con_f[1]," - ",ci_pa_con_f[2],")"))
get_p_val(conv_vs_pa_cond_f,apaprent_diff =obs_diff_pa_con_f )


####AHS comparisons###########
#####################QRISK3 vs qr3 + AHS#################
th <- 0.075

#var_th <- '_5_65_75'
#var_th <- '_5_under_55'
var_th <- '_7.5'
optims_pa <- optim_dist(full_pa,rows_with_na,met = paste0(met,var_th))
optims_ars <- optim_dist(full_ars,rows_with_na,met = paste0(met,var_th))
rows_with_na <-c()
rows_with_na <- null_rows(cond_ars)
optims_ars_cond <- optim_dist(cond_ars,rows_with_na,met =  paste0(met,var_th))

if (grepl("_under_55", var_th)) {
  print('under55')
  age_cond <- imputed_datasets[[1]]$age < 55
} else if (grepl("_55_65", var_th)) {
  print('55-65')
  age_cond <- imputed_datasets[[1]]$age >= 55 & imputed_datasets[[1]]$age < 65
} else if (grepl("_65_75", var_th)) {
  print('65-75')
  age_cond <- imputed_datasets[[1]]$age >= 65 & imputed_datasets[[1]]$age < 75
} else if (grepl("_over_75", var_th)) {
  print('75')
  age_cond <- imputed_datasets[[1]]$age >= 75
} else {
  print('all')
  age_cond <- rep(TRUE,nrow(imputed_datasets[[1]])) # Default case if no match is found
}

#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[1]],sex_cond & age_cond)
  args_list2  <- list(lienar_predictions_ars[[1]][[2]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond& age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond& age_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond & age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond & age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont' ) {
  print('nri')
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond& age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}


obs_diff_ars_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_ars[[2]])

q_vs_ars_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_ars[[2]]
)

ci_ars_m <- round(pool_ci(q_vs_ars_m),digits)
print(paste0(round(mean(q_vs_ars_m),digits),"(95% CI ",ci_ars_m[1]," - ",ci_ars_m[2],")"))
get_p_val(q_vs_ars_m,apaprent_diff =obs_diff_ars_m )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars[[1]]
  ci_ars_m <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),digits),"(95% CI ",ci_ars_m[1]," - ",ci_ars_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

#female
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_ars[[1]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond& age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond& age_cond,score_col = 'ars')
}else if (met =='nb') {
  print('nb')
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond& age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond& age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond& age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

obs_diff_ars_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_ars[[4]])


q_vs_ars_f <- adjusted_delta(
  func = func,
  args_list1 =args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_ars[[4]]
)

ci_ars_f <- round(pool_ci(q_vs_ars_f),digits)
print(paste0(round(mean(q_vs_ars_f),digits),"(95% CI ",ci_ars_f[1]," - ",ci_ars_f[2],")"))
get_p_val(q_vs_ars_f,apaprent_diff =obs_diff_ars_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars[[3]]
  ci_ars_f <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_ars_f[1]," - ",ci_ars_f[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}



#####################asx + AHs vs qr3 #################
th <- 0.05
#Adam
get_age_c <- function(lp_full,df_cond,lp_cond){
  avg_lp <- Reduce(`+`, lp_full) / length(lp_full)
  avg_lp <- avg_lp[lp_cond]
  # Calculate adjusted C-index
  c_app <- round(get_idx(df[df_cond, ], avg_lp),3)
  return(c_app)
}

func <- get_age_c
#var_th <- '_5_65_75'
#var_th <- '_5_under_55'
met <- 'c'
var_th <- '_over_75'

#calualte c-index for each group
df_m <- df_ars[sex_cond,]
df_f <- df_ars[!sex_cond,]




optims_asx <- optim_dist(full_asx,rows_with_na,met = paste0(met,var_th))
optims_ars <- optim_dist(full_ars,rows_with_na,met = paste0(met,var_th))
rows_with_na <-c()
if (grepl("_under_55", var_th)) {
  print('under55')
  age_cond <- imputed_datasets[[1]]$age < 55
  age_cond_m <- df_m$age < 55
  age_cond_f <- df_f$age <55
} else if (grepl("_55_65", var_th)) {
  print('55-65')
  age_cond <- imputed_datasets[[1]]$age >= 55 & imputed_datasets[[1]]$age < 65
  age_cond_m <- df_m$age >= 55 & df_m$age < 65
  age_cond_f <- df_f$age >= 55 & df_f$age < 65
} else if (grepl("_65_75", var_th)) {
  print('65-75')
  age_cond <- imputed_datasets[[1]]$age >= 65 & imputed_datasets[[1]]$age < 75
  age_cond_m <- df_m$age >= 65 & df_m$age < 75
  age_cond_f <- df_f$age >= 65 & df_f$age < 75
  
} else if (grepl("_over_75", var_th)) {
  print('75')
  age_cond <- imputed_datasets[[1]]$age > 75
  age_cond_m <- df_m$Age >75
  age_cond_f <- df_f$Age >75
  
} else {
  age_cond <- rep(TRUE,nrow(imputed_datasets[[1]])) # Default case if no match is found
}

#male
met <-'c_idx'
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[1]],sex_cond & age_cond,age_cond_m)
  args_list2  <- list(lienar_predictions_asx[[1]][[2]],sex_cond& age_cond,age_cond_m)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond& age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_asx, sex_cond = sex_cond& age_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond & age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_asx, sex_cond = sex_cond & age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont' ) {
  print('nri')
  df_ars$asx <- df_asx$ars
  args_list1 <-   list(df = df_ars, sex_cond = (sex_cond& age_cond),rs1 = 'qrisk3',rs2 = 'ars',th = th)
}


obs_diff_ars_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_asx[[2]])

q_vs_ars_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_asx[[2]]
)

ci_ars_m <- round(pool_ci(q_vs_ars_m),digits)
print(paste0(round(mean(q_vs_ars_m),digits),"(95% CI ",ci_ars_m[1]," - ",ci_ars_m[2],")"))
get_p_val(q_vs_ars_m,apaprent_diff =obs_diff_ars_m )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars[[2]]
  ci_ars_m <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),digits),"(95% CI ",ci_ars_m[1]," - ",ci_ars_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

#female
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[3]],!sex_cond & age_cond,age_cond_f)
  args_list2  <- list(lienar_predictions_asx[[1]][[4]],!sex_cond & age_cond,age_cond_f)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_asx, sex_cond = !sex_cond& age_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond& age_cond,score_col = 'ars')
}else if (met =='nb') {
  print('nb')
  args_list1 <-   list(df = df_asx, sex_cond = !sex_cond& age_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond& age_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  df_ars[['asx']] <- df_asx$ars
  print('nri')
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond& age_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

obs_diff_ars_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_asx[[4]])


q_vs_ars_f <- adjusted_delta(
  func = func,
  args_list1 =args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_asx[[4]]
)

ci_ars_f <- round(pool_ci(q_vs_ars_f),digits)
print(paste0(round(mean(q_vs_ars_f),digits),"(95% CI ",ci_ars_f[1]," - ",ci_ars_f[2],")"))
get_p_val(q_vs_ars_f,apaprent_diff =obs_diff_ars_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars[[3]]
  ci_ars_f <- round(pool_ci(nri_dist_adj),digits)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_ars_f[1]," - ",ci_ars_f[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}



######################################

#####################QRISK3_cond AHS#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[3]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_ars[[3]][[2]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  print('nb')
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,rs1 = 'qrisk3_con',rs2 = 'ars_con',th = th)
}

obs_diff_con_ars_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars_cond[[1]],
  optims2 = optims_ars_cond[[2]])


conv_vs_ars_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 =args_list2,
  optims1 = optims_ars_cond[[1]],
  optims2 = optims_ars_cond[[2]]
)



ci_con_ars_m <- round(pool_ci(conv_vs_ars_m),digits)
print(paste0(round(mean(conv_vs_ars_m),digits),"(95% CI ",ci_con_ars_m[1]," - ",ci_con_ars_m[2],")"))
get_p_val(conv_vs_ars_m,apaprent_diff =obs_diff_con_ars_m )


if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars_cond[[1]]
  print('res')
  ci_con_ars_m <- round(pool_ci(nri_dist_adj),3)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_con_ars_m[1]," - ",ci_con_ars_m[2],")"))
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

#female
if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[3]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_ars[[3]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,score_col = 'qrisk3_con')
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,rs = 'qrisk3_con',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  print('nri')
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,rs1 = 'qrisk3_con',rs2 = 'ars_con',th = th)
}

obs_diff_con_ars_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars_cond[[3]],
  optims2 = optims_ars_cond[[4]])

conv_vs_ars_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars_cond[[3]],
  optims2 = optims_ars_cond[[4]]
)

ci_con_ars_f <- round(pool_ci(conv_vs_ars_f),digits)
print(paste0(round(mean(conv_vs_ars_f),digits),"(95% CI ",ci_con_ars_f[1]," - ",ci_con_ars_f[2],")"))
get_p_val(conv_vs_ars_f,apaprent_diff =obs_diff_con_ars_f )

if(met == 'nri'| met == 'nri_cont'){
  nri_app <- do.call(func,args_list1)
  nri_dist_adj <- nri_app - optims_ars_cond[[3]]
  ci_con_ars_f <- round(pool_ci(nri_dist_adj),3)
  print(paste0(round(mean(nri_dist_adj),3),"(95% CI ",ci_con_ars_f[1]," - ",ci_con_ars_f[2],")"))
  
  get_p_val(nri_dist_adj,apaprent_diff =nri_app )
}

######################################

#####################QRISK3 vs qrisk_cond + AHS#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_ars[[3]][[2]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_ars, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars_con')
}

obs_diff_ars_con_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_ars_cond[[1]])

cond_vs_ars_cond_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[1]],
  optims2 = optims_ars_cond[[2]]
)


ci_ars_con_m <- round(pool_ci(cond_vs_ars_cond_m),digits)
print(paste0(round(mean(cond_vs_ars_cond_m),digits),"(95% CI ",ci_ars_con_m[1]," - ",ci_ars_con_m[2],")"))
get_p_val(cond_vs_ars_cond_m,apaprent_diff =obs_diff_ars_con_m )


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_ars[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_ars[[3]][[4]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,score_col = 'qrisk3')
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,score_col = 'ars_con')
}else if (met =='nb') {
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,rs = 'qrisk3',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,rs = 'ars_con',th = th)
} else if (met =='nri_cat') {
  args_list1 <-   list(df = df_ars, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars_con')
}

obs_diff_ars_con_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_ars_cond[[4]])


conv_vs_ars_cond_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_ars[[3]],
  optims2 = optims_ars_cond[[4]]
)


ci_ars_con_f <- round(pool_ci(conv_vs_ars_cond_f),digits)
print(paste0(round(mean(conv_vs_ars_cond_f),digits),"(95% CI ",ci_ars_con_f[1]," - ",ci_ars_con_f[2],")"))
get_p_val(conv_vs_ars_cond_f,apaprent_diff =obs_diff_ars_con_f )


#####################AHS VS multivar + PA#################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa_only_mv[[1]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_pa_only[[1]][[1]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa_only, sex_cond = sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_ars_only, sex_cond = sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa_only, sex_cond = sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_ars_only, sex_cond = sex_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa_only, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
  args_list2 <-   list(df = df_ars_only, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
}

obs_diff_ahs_pa_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_only_mv[[1]],
  optims2 = optims_pa_only[[1]])

ahs_vs_pa_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_only_mv[[1]],
  optims2 = optims_pa_only[[2]]
)


ci_ahs_pa_m <- round(pool_ci(ahs_vs_pa_m),digits)
print(paste0(round(mean(ahs_vs_pa_m),digits),"(95% CI ",ci_ahs_pa_m[1]," - ",ci_ahs_pa_m[2],")"))
get_p_val(ahs_vs_pa_m,apaprent_diff =obs_diff_ahs_pa_m )


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa_only_mv[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_pa_only[[1]][[3]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa_only, sex_cond = !sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_ars_only, sex_cond = !sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa_only, sex_cond = !sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_ars_only, sex_cond = !sex_cond,rs = 'ars',th = th)
} else  if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa_only, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
  args_list2 <-   list(df = df_ars_only, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
}

obs_diff_ahs_pa_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_only_mv[[3]],
  optims2 = optims_pa_only[[4]])


ahs_vs_pa_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa_only_mv[[3]],
  optims2 = optims_pa_only[[4]]
)


ci_ahs_pa_f <- round(pool_ci(ahs_vs_pa_f),digits)
print(paste0(round(mean(ahs_vs_pa_f),digits),"(95% CI ",ci_ahs_pa_f[1]," - ",ci_ahs_pa_f[2],")"))
get_p_val(ahs_vs_pa_f,apaprent_diff =obs_diff_ahs_pa_f )



#######################Age + AHS vs age +PA#################################################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_asx_pa[[1]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_asx[[1]][[1]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_asx, sex_cond = sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_asx, sex_cond = sex_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_asx_pa, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
  args_list2 <-   list(df = df_asx, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
}

obs_diff_ahs_pa_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[1]],
  optims2 = optims_asx[[1]])

ahs_vs_pa_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[1]],
  optims2 = optims_asx[[2]]
)


ci_ahs_pa_m <- round(pool_ci(ahs_vs_pa_m),digits)
print(paste0(round(mean(ahs_vs_pa_m),digits),"(95% CI ",ci_ahs_pa_m[1]," - ",ci_ahs_pa_m[2],")"))
get_p_val(ahs_vs_pa_m,apaprent_diff =obs_diff_ahs_pa_m )


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_asx_pa[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_asx[[1]][[3]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_asx_pa, sex_cond = !sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_asx, sex_cond = !sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_asx_pa, sex_cond = !sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_asx, sex_cond = !sex_cond,rs = 'ars',th = th)
} else if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_asx_pa, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
  args_list2 <-   list(df = df_asx, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars')
}

obs_diff_ahs_pa_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[3]],
  optims2 = optims_asx[[4]])


ahs_vs_pa_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx_pa[[3]],
  optims2 = optims_asx[[4]]
)


ci_ahs_pa_f <- round(pool_ci(ahs_vs_pa_f),digits)
print(paste0(round(mean(ahs_vs_pa_f),digits),"(95% CI ",ci_ahs_pa_f[1]," - ",ci_ahs_pa_f[2],")"))
get_p_val(ahs_vs_pa_f,apaprent_diff =obs_diff_ahs_pa_f )



#######################QRISK + PA vs + QRISK + AHS#################################################
#male

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[1]],sex_cond)
  args_list2  <- list(lienar_predictions_ars[[1]][[1]],sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = sex_cond,rs = 'ars',th = th)
} else  if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
  args_list2 <-   list(df = df_ars, sex_cond = sex_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

obs_diff_ahs_pa_m <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_ars[[1]])

ahs_vs_pa_m <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[1]],
  optims2 = optims_ars[[2]]
)


ci_ahs_pa_m <- round(pool_ci(ahs_vs_pa_m),digits)
print(paste0(round(mean(ahs_vs_pa_m),digits),"(95% CI ",ci_ahs_pa_m[1]," - ",ci_ahs_pa_m[2],")"))
get_p_val(ahs_vs_pa_m,apaprent_diff =obs_diff_ahs_pa_m )


#female

if(met == 'c_idx'){
  args_list1 <- list(lienar_predictions_pa[[1]][[3]],!sex_cond)
  args_list2  <- list(lienar_predictions_ars[[1]][[3]],!sex_cond)
}else if (met == 'ici'){
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,score_col = 'ars')
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,score_col = 'ars')
}else if (met =='nb') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs = 'ars',th = th)
  args_list2 <-  list(df = df_ars, sex_cond = !sex_cond,rs = 'ars',th = th)
} else  if (met =='nri' | met == 'nri_cont') {
  args_list1 <-   list(df = df_pa, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th )
  args_list2 <-   list(df = df_ars, sex_cond = !sex_cond,rs1 = 'qrisk3',rs2 = 'ars',th = th)
}

obs_diff_ahs_pa_f <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_ars[[4]])


ahs_vs_pa_f <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_pa[[3]],
  optims2 = optims_ars[[4]]
)


ci_ahs_pa_f <- round(pool_ci(ahs_vs_pa_f),digits)
print(paste0(round(mean(ahs_vs_pa_f),digits),"(95% CI ",ci_ahs_pa_f[1]," - ",ci_ahs_pa_f[2],")"))
get_p_val(ahs_vs_pa_f,apaprent_diff =obs_diff_ahs_pa_f )

##################Display results####################
if(met == 'ici'){
  digits <- 4
}else{
  digits <- 3
}
print('Age vs Age + PA')
cat('\nmale')
print(paste0(round(mean(asx_delta_m),digits),"(95% CI ",ci_asx_m[1]," - ",ci_asx_m[2],")"))
get_p_val(asx_delta_m,apaprent_diff =obs_diff_asx_m )
cat('\nfemale')
print(paste0(round(mean(asx_delta_f),digits),"(95% CI ",ci_asx_f[1]," - ",ci_asx_f[2],")"))
get_p_val(asx_delta_f,apaprent_diff =obs_diff_asx_f )

print('QRISK3 cond vs QRISK3 COND + PA')
cat('\nmale')
print(paste0(round(mean(conv_vs_pa_m),digits),"(95% CI ",ci_con_pa_m[1]," - ",ci_con_pa_m[2],")"))
get_p_val(conv_vs_pa_m,apaprent_diff =obs_diff_con_pa_m )
cat('\nfemale')
print(paste0(round(mean(conv_vs_pa_f),digits),"(95% CI ",ci_con_pa_f[1]," - ",ci_con_pa_f[2],")"))
get_p_val(conv_vs_pa_f,apaprent_diff =obs_diff_con_pa_f )

print("QRISK3 VS condensed QRISK3 +PA ")
cat('\nmale')
print(paste0(round(mean(cond_vs_pa_cond_m),digits),"(95% CI ",ci_pa_con_m[1]," - ",ci_pa_con_m[2],")"))
get_p_val(cond_vs_pa_cond_m,apaprent_diff =obs_diff_pa_con_m )

cat('\nfemale')
print(paste0(round(mean(conv_vs_pa_cond_f),digits),"(95% CI ",ci_pa_con_f[1]," - ",ci_pa_con_f[2],")"))
get_p_val(conv_vs_pa_cond_f,apaprent_diff =obs_diff_pa_con_f)


print("QRISK3 VS QRISK3 + PA")
cat('\nmale')
print(paste0(round(mean(q_vs_pa_m),digits),"(95% CI ",ci_pa_m[1]," - ",ci_pa_m[2],")"))
get_p_val(q_vs_pa_m,apaprent_diff =obs_diff_pa_m )
cat('\nFRmale')
print(paste0(round(mean(q_vs_pa_f),digits),"(95% CI ",ci_pa_f[1]," - ",ci_pa_f[2],")"))
get_p_val(q_vs_pa_f,apaprent_diff =obs_diff_pa_f )




print("QRISK3 VS condensed QRISK3  ")
cat('\nmale')
print(paste0(round(mean(conv_vs_q_m),digits),"(95% CI ",ci_con_q_m[1]," - ",ci_con_q_m[2],")"))
get_p_val(conv_vs_q_m,apaprent_diff =obs_diff_con_q_m )

cat('\nfemale')
print(paste0(round(mean(conv_vs_q_f),digits),"(95% CI ",ci_con_q_f[1]," - ",ci_con_q_f[2],")"))
get_p_val(conv_vs_q_f,apaprent_diff =obs_diff_con_q_f )


print('')

cat('\nfemale')
print(paste0(round(mean(conv_vs_pa_cond_f),digits),"(95% CI ",ci_pa_con_f[1]," - ",ci_pa_con_f[2],")"))
get_p_val(conv_vs_pa_cond_f,apaprent_diff =obs_diff_pa_con_f )

######################################Calibration curves##################################
#get apapreant and adjusted predictors + risk scores

#get optimism adjusted C-idx and 95% CI
result_asx <- get_adjusted_results(
  env_path = "risk_scores/stacked/array_job",
  data_path = "risk_scores/ARS/bs_imp/con/6y/stacked/",
  store = "/ukb_models/cut_off/cv/stacked/asx",
  model = "asx",
  split = "",
  model_type = "pa",
  store_suffix = "lin",
  data_source = "ukb",
  metric = "c_idx",
  acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
  condensed_model = FALSE
)



###cal curves
source(file.path("risk_scores",'cal_plot_imp.R'), echo = T)

#######################ASX###############################
xmax = 20
cal_curve(df_asx[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "Age Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 7000,title = "Age + AHS Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx,'/figures/center/male'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "Age + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)


xmax = 15
cal_curve(df_asx[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "Age Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 7000,title = "Age + AHS Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx,'/figures/center/female'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "Age + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)



#######################ASX PA ###############################
xmax = 20
cal_curve(df_asx_pa[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Age Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx_pa[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 2000,title = "Age + PA Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx_pa,'/figures/center/male'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx_pa[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Age + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)


xmax = 15
cal_curve(df_asx_pa[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Age Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx_pa[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 2000,title = "Age + PA Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx_pa,'/figures/center/female'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_asx_pa[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Age + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_asx_pa,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)




#######################PA only###############################
store_acc <- "/ukb_models/cut_off/cv/stacked/pa_only/center/acc_pa_lin"
xmax = 20
cal_curve(df_pa_only[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Multivariate  PA Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_acc,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_acc,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

xmax = 15
#Multivariate
cal_curve(df_pa_only[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 2000,title = "Multivariate  PA Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_acc,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_acc,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)



#######################Qrisk + PA###############################
xmax = 20
cal_curve(df_pa[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_pa[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 + PA Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx,'/figures/center/male'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_pa[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)


xmax = 20
cal_curve(df_pa[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_pa[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 + PA Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_asx,'/figures/center/female'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_pa[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

#cond
xmax = 20
cal_curve(df_pa[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_con_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 (Excl HDL & SBP) Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)
cal_curve(df_pa[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_con_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 (Excl HDL & SBP) + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

xmax = 15
cal_curve(df_pa[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_con_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 (Excl HDL & SBP) Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_pa[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_con_adj',xmin =0,xmax = xmax,hist_max = 6000,title = "QRISK3 (Excl HDL & SBP) + PA Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/figures/female/'),offset = 2.5,event_col = event_col,time_col = time_col)



#######################Qrisk + ARS###############################
xmax = 20
cal_curve(df_ars[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_ars[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 + AHS Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_ars,'/figures/center/male'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_ars[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)


xmax = 15
cal_curve(df_ars[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_ars[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 + AHS Calibration (non adj)",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/male',store_ars,'/figures/center/female'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_ars[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

#cond
xmax = 20
cal_curve(df_ars[sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_con_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 (Excl HDL & SBP) Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)
cal_curve(df_ars[sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_con_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 (Excl HDL & SBP) + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/male/'),offset = 2.5,event_col = event_col,time_col = time_col)

xmax = 15
cal_curve(df_ars[!sex_cond,],male_q,rows_with_na,lab = event_col, pred_event_prob = 'qrisk3_con_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 (Excl HDL & SBP) Calibration",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)

cal_curve(df_ars[!sex_cond,],male_ars,rows_with_na,lab = event_col, pred_event_prob = 'ars_con_adj',xmin =0,xmax = xmax,hist_max = 7000,title = "QRISK3 (Excl HDL & SBP) + AHS Calibration",store_path =  paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),met_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_ars,'/figures/center/female/'),offset = 2.5,event_col = event_col,time_col = time_col)









#################generic metric comaprison################################################
func <- get_apaprent_ici
optims_asx <- optim_dist(full_asx,rows_with_na,met = 'ici')
obs_1 <- get_apaprent_ici(df = df_asx, sex_cond = sex_cond,score_col = 'qrisk3_adj') - mean(optims_asx[[1]])
obs_2 <- get_apaprent_ici(df = df_asx, sex_cond = sex_cond,score_col = 'ars_adj')  - mean(optims_asx[[2]])
obs_diff = obs_2 -obs_1

args_list1 <-   list(df = df_asx, sex_cond = sex_cond,score_col = 'qrisk3_adj')
args_list2 <-  list(df = df_asx, sex_cond = sex_cond,score_col = 'ars_adj')
#get optimism adjusted delta aginst age

sex_cond <- imputed_datasets[[1]]$Sex == 1

asx_delta_m_ici <- adjusted_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx[[1]],
  optims2 = optims_asx[[2]]
)

obs_diff2 <- observed_delta(
  func = func,
  args_list1 = args_list1,
  args_list2 = args_list2,
  optims1 = optims_asx[[1]],
  optims2 = optims_asx[[2]])

ci <- round(pool_ci(asx_delta_m_ici),4)
print(paste0(round(mean(asx_delta_m_ici),4),"(95% CI ",ci[1]," - ",ci[2],")"))
get_p_val(asx_delta_m_ici,apaprent_diff =obs_diff )


df_nb <- data.frame(
  Age = df_asx$qrisk3_adj/100,
  'QRISK3 (Excl HDL & SBP)' = df_pa$qrisk3_con_adj/100,
  'QRISK3' = df_pa$qrisk3_adj/100,
  'Age + PA' = df_asx$ars_adj/100,
  'QRISK3 (Excl HDL & SBP) + PA' = df_pa$ars_con_adj/100,
  'QRISK3 + PA' = df_pa$ars_adj/100,
  "macce.incident" = df_asx$macce.incident,
  "macce_obs_time" = df_asx$macce_obs_time,
  check.names = FALSE
)

color_map <- c(
  "Age" = "lightblue" , 
  "Age + PA" = "blue",
  #"QRISK3 (Excl HDL & SBP)" = "green", 
  #"QRISK3 (Excl HDL & SBP) + PA" = "lightgreen", 
  "QRISK3" = "orange", 
  "QRISK3 + PA" = "red"
)



preds_all <- c("Age",
               "Age + PA",
               # "QRISK3 (Excl HDL & SBP)", 
               "QRISK3", 
               # "QRISK3 (Excl HDL & SBP) + PA",
               "QRISK3 + PA")



preds_age <- c("Age", 
               "Age + PA")


preds_con <- c("QRISK3 (Excl HDL & SBP)", 
               "QRISK3 (Excl HDL & SBP) + PA")
preds_q <- c("QRISK3", 
             "QRISK3 + PA")

#store_path <- "risk_scores/array_job/master_branch/plot_scripts/outputs/ukb_plots/nb_curve/con/"
store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store_pa,'/figures/center/female/')
nb_cond <- !sex_cond

png(paste0(store_path,"nb_curve_age_vs_q.png" ),type="cairo")  
pgrs_nb <- stdca(data=df_nb[nb_cond,], outcome="macce.incident", ttoutcome="macce_obs_time", timepoint=6.0, 
                 predictors=preds_all, xstop=0.2,ymin = -0.02, loess.span=0.2, smooth=TRUE,colour_map = color_map)

dev.off()

#store_path <- "risk_scores/array_job/master_branch/plot_scripts/outputs/ukb_plots/nb_curve/con/"
png(paste0(store_path,"nb_curve_age.png" ),type="cairo")  
pgrs_nb <- stdca(data=df_nb[nb_cond,], outcome="macce.incident", ttoutcome="macce_obs_time", timepoint=6.0, 
                 predictors=preds_age, xstop=0.2,ymin = -0.02, loess.span=0.2, smooth=TRUE)

dev.off()


#store_path <- "risk_scores/array_job/master_branch/plot_scripts/outputs/ukb_plots/nb_curve/con/"
png(paste0(store_path,"nb_curve_cond.png" ),type="cairo")  
pgrs_nb <- stdca(data=df_nb[nb_cond,], outcome="macce.incident", ttoutcome="macce_obs_time", timepoint=6.0, 
                 predictors=preds_con, xstop=0.2,ymin = -0.02, loess.span=0.2, smooth=TRUE)

dev.off()


#store_path <- "risk_scores/array_job/master_branch/plot_scripts/outputs/ukb_plots/nb_curve/con/"
png(paste0(store_path,"nb_curve_q.png" ),type="cairo")  
pgrs_nb <- stdca(data=df_nb[nb_cond,], outcome="macce.incident", ttoutcome="macce_obs_time", timepoint=6.0, 
                 predictors=preds_q, xstop=0.2,ymin = -0.02, loess.span=0.2, smooth=TRUE)

dev.off()

#comapre acorss ages 
age_cond <- 'male 55-65'
condition_fn <- select_cond(age_cond)

condition <- condition_fn(df_pa)

nri_1 <- calculate_nri(df_asx[condition,],'qrisk3','ars',th = 0.1)[1] - mean(optims_pa[[1]])
nri_2 <- calculate_nri(df_asx[condition,],'qrisk3_adj','ars_adj',th = 0.1)[1]












###########tmp testing

cent_age <- c()
cent_sbp <- c()
cent_ars <- c()
delta_ars <- c()
delta_age <- c()
delta_sbp <- c()

cent_ars_fp <- c()
delta_ars_fp <- c()

cent_ars_fp2 <- c()
delta_ars_fp2 <- c()


for(index in 1:1000){
  bootstrap_indices <- unlist(bs_samples[index,2:length(bs_samples)])
  df_bs <- df[bootstrap_indices, ]
  delta_age <- c(delta_age,abs(mean(df$Age) - mean(df_bs$Age)))
  delta_ars <- c(delta_ars,abs(mean(df$ActivityRiskScore) - mean(df_bs$ActivityRiskScore)))
  delta_ars_fp <- c(delta_ars_fp,abs(mean(df$ActivityRiskScore_1) - mean(df_bs$ActivityRiskScore_1)))
  cent_ars <- c(cent_ars,mean(df$ActivityRiskScore - mean(df_bs$ActivityRiskScore)))
  cent_age <- c(cent_age,mean(df$Age - mean(df_bs$Age)))
  cent_ars_fp <- c(cent_ars_fp,mean(df$ActivityRiskScore_1 - mean(df_bs$ActivityRiskScore_1)))
  
  delta_ars_fp2 <- c(delta_ars_fp2,abs(mean(df$ActivityRiskScore_2) - mean(df_bs$ActivityRiskScore_2)))
  cent_ars_fp2 <- c(cent_ars_fp2,mean(df$ActivityRiskScore_2 - mean(df_bs$ActivityRiskScore_2)))
}



for(index in 1:200){
  if(index%% 10 == 0)
  {
    print(index)
  }
  bootstrap_indices <- unlist(bs_samples[index,2:length(bs_samples)])
  df_bs <- swiss_re[bootstrap_indices, ]
  delta_age <- c(delta_age,abs(mean(swiss_re$Age) - mean(df_bs$Age)))
  delta_ars <- c(delta_ars,abs(mean(swiss_re$ActivityRiskScore) - mean(df_bs$ActivityRiskScore)))
  delta_sbp <- c(delta_sbp,abs(mean(swiss_re$SystolBloodPressur) - mean(df_bs$SystolBloodPressur)))
  cent_ars <- c(cent_ars,mean(swiss_re$ActivityRiskScore - mean(df_bs$ActivityRiskScore)))
  cent_age <- c(cent_age,mean(swiss_re$Age - mean(df_bs$Age)))
  cent_sbp <- c(cent_sbp,mean(swiss_re[!is.na(swiss_re$SystolBloodPressur),]$SystolBloodPressur - mean(df_bs$SystolBloodPressur,na.rm = TRUE)))
}



ars_delta_m <- adjusted_delta(
  func = func,
  args_list1 = list(lienar_predictions_asx[[1]][[1]],sex_cond),
  args_list2 = list(lienar_predictions_ars[[1]][[2]],sex_cond),
  optims1 = optims_asx[[1]],
  optims2 = optims_ars[[2]]
)

ars_delta_f <- adjusted_delta(
  func = func,
  args_list1 = list(lienar_predictions_asx[[1]][[3]],!sex_cond),
  args_list2 = list(lienar_predictions_ars[[1]][[4]],!sex_cond),
  optims1 = optims_asx[[3]],
  optims2 = optims_ars[[4]]
)



ars_delta_cond_m <- adjusted_delta(
  func = func,
  args_list1 = list(lienar_predictions_asx[[1]][[1]],sex_cond),
  args_list2 = list(lienar_predictions_ars[[3]][[2]],sex_cond),
  optims1 = optims_asx[[1]],
  optims2 = optims_ars_cond[[2]]
)

ars_delta_cond_f <- adjusted_delta(
  func = func,
  args_list1 = list(lienar_predictions_ars[[1]][[3]],!sex_cond),
  args_list2 = list(lienar_predictions_ars[[3]][[4]],!sex_cond),
  optims1 = optims_asx[[3]],
  optims2 = optims_ars_cond[[4]]
)







#example use
#get the cindex ofr qrisk3 pa and then pa asx
model_path <- paste0(store, split, '/', model, '_', model_type, '_', store_suffix)
lienar_predictions <- get_adjusted_lp(env_path,model_path,model, model_type,data_source,fp_terms = fp_terms,condensed_model = condensed_model )

optims <- optim_dist(full_pa,rows_with_na,met = met)



sex_cond <- imputed_datasets[[1]]$Sex == 1
asx_delta <- adjusted_delta(
  func = func,
  args_list1 = list(lienar_predictions[[1]][[1]],sex_cond & age_cond),
  args_list2 = list(lienar_predictions[[1]][[2]],sex_cond & age_cond),
  optims1 = optims[[1]],
  optims2 = optims[[2]]
)

if(met =='ici'){
  digits <-4
}else{
  digits <-3
}
