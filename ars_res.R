###
#Process ARS results
###
source(file.path("risk_scores/array_job/master_branch/",'0_conf.R'), echo = T)
source(file.path("risk_scores/array_job/master_branch/",'ARS_deriv.R'), echo = T)
source(file.path("risk_scores/array_job/master_branch/",'val_utils.R'), echo = T)
source(file.path("risk_scores/array_job/master_branch/",'predict_utils.R'), echo = T)

args <- commandArgs(TRUE)
model <- as.character(args[1])
store <- as.character(args[2])
split <- as.character(args[3])
fit_poly <- as.character(args[4])
model_type <- as.character(args[5])
data_source <- as.character(args[6])
fp_terms <- strsplit(args[7], ",")[[1]]

model <- tolower(model)  # Convert model name to lowercase
model_type <- tolower(model_type)  # Convert model name to lowercase

if(model_type != 'ars' &model_type != 'hc'){
  print('Errormodle type must be ars or hc')
}


fit_poly <- tolower(fit_poly)  # Convert model name to lowercase
if(fit_poly != 't' &fit_poly != 'f'){
  stop('Error invalid input fit polynomail must be treue or false')
}
if(fit_poly == 't'){
  fit_poly <- TRUE
  store_suffix <- paste0(model_type,'_fp')
}else{
  fit_poly <- FALSE
  store_suffix <- paste0(model_type,'_lin')
}
if(length(fp_terms) == 0){
  if(model_type == 'ars'){
    fp_terms <- c('ActivityRiskScore')
  }else if(model_type == 'hc'){
    fp_terms <- c('StepsDayMedAdjusted','SleepActivity')
  }
}

data_source <- tolower(data_source) 
if(data_source != 'ukb' &data_source != 'gp'){
  stop('Error invalid input data source must be either gp or ukb')
}


store <- paste0(store,split,'/',model,'_',store_suffix)


#read bootsratp results for optimism calalulationm
full_res <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/')) #" array.  risk_scores/ARS/bs_imp/con/6y/full/imp.  'risk_scores/ARS/bs_imp/con/6y/full/array/'
cond_res <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/',store,'/')) # array. "risk_scores/ARS/bs_imp/con/6y/condensed/imp .  'risk_scores/ARS/bs_imp/con/6y/condensed/array/'

#get full modle rowa
rows_with_na <-c()
rows_with_na <- null_rows(full_res)
shrinkage <- get_optimism(full_res,rows_with_na)

#get condensed model rows
rows_with_na_con <- null_rows(cond_res)
shrinkage_con <- get_optimism(cond_res,rows_with_na_con)

full_models <- load_modles(path  = paste0('risk_scores/stacked/array_job/',store,'/full_models/')) #risk_scores/ARS/org_imp/con/ risk_scores/array_job/full_models/
cond_models <- load_modles(path  = paste0('risk_scores/stacked/array_job/',store,'/condensed_models/'),suffix = '_cond') # risk_scores/array_job/condensed_models/
#load orignal data
imputed_datasets <- list()
for (i in 1:5) {
  imputed_datasets[[i]] <- read.csv(paste0("risk_scores/stacked/array_job/",store,"/full_models/","org_imp_",i,".csv"))#risk_scores/array_job/full_models/","org_imp_",i,".csv". con/
}

sex_cond <- imputed_datasets[[1]]$Sex == 1
#get baslien survival for full models
lp_adj_full <- adjust_lp(imputed_datasets,full_models,shrinkage,model = model)
lp_adj_con <- adjust_lp(imputed_datasets,cond_models,shrinkage_con,model =model)

data_path <- "risk_scores/stacked/array_job"
load(file=paste0(data_path,store,'/',data_source,'_',model,'_',model_type,'.RData'))
risk_models <- list(1,2,3,4,5)
baseline_surv <-  get_basline_survival(imputed_datasets,risk_models,sex_cond,lp = lp_adj_full,time = 6)
baseline_surv_con <- get_basline_survival(imputed_datasets,risk_models,sex_cond,lp= lp_adj_con,time = 6)



rs_adj_full <- adjust_rs(imputed_datasets,full_models,lp_adj_full,base_surv = baseline_surv)
rs_adj_con <- adjust_rs(imputed_datasets,cond_models,lp_adj_con,base_surv = baseline_surv_con)

df <-data.frame(imputed_datasets[[1]])
sex_cond<- df$Sex ==1

df['qrisk3'] <- 0
df['ars'] <- 0
df['qrisk3_con'] <- 0
df['ars_con'] <- 0

df[sex_cond,'qrisk3'] <- rs_adj_full[[1]]
df[sex_cond,'ars'] <- rs_adj_full[[2]]

df[!sex_cond,'qrisk3'] <- rs_adj_full[[3]]
df[!sex_cond,'ars'] <- rs_adj_full[[4]]

df[sex_cond,'qrisk3_con'] <- rs_adj_con[[1]]
df[sex_cond,'ars_con'] <- rs_adj_con[[2]]

df[!sex_cond,'qrisk3_con'] <- rs_adj_con[[3]]
df[!sex_cond,'ars_con'] <- rs_adj_con[[4]]




