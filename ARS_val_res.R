source(file.path("risk_scores/",'ARS_eval_utils_try_catch.R'), echo = T)

source(file.path("risk_scores/",'reclass.R'), echo = T)

# store <- paste0('gp_models/cv/split0/',model,'/score_hc_lin/')
# 
split <- 3
load(file=paste0('risk_scores/ARS/ukb_ars_fp_split',split,'.RData'))
age_interact <- TRUE
selected_function <- switch(model,
                            "qrisk" = get_q_val,
                            "score2" = get_score_features,
                            "aha" = get_aha_feature,
                            default = {
                              stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
                            }
)
if (model == 'qrisk') {
  imp_vars <- q_vars
  formula <- QRISK3
  formula_cond <- QRISK3_cond
} else if (model == 'score2') {
  imp_vars <- score_vars
  formula <- SCORE2
  formula_cond <- SCORE2_cond
} else if (model == 'aha') {
  imp_vars <- aha_vars
  formula <- AHA
  formula_cond <- AHA_cond
} else {
  stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
}
features <- c("eid","ActivityRiskScore",'Age' ,'Sex', 'BodyMassIndex' ,'Diabetes', 'diabetes.type1.prevalent' , 'diabetes.type2.prevalent',
              "StepsDayMedAdjusted","SleepActivity","SelfWalkPace",
              'Essential..primary..hypertension.prevalent',  'SystolBloodPressur' , 'TownsendIndex' ,'Cholesterol','HdlCholesterol', 'CholesRatio' , 'Corticosteroid.prevalent' , 'Migraine.prevalent' ,
              'Rheumatoid.arthritis.prevalent' , 'Systemic.lupus.erythematosus.prevalent' , 'Severe.mental.illnes.prevalent' ,
              'Atrial.fibrillation.and.flutter.prevalent' ,'sbp5' , 'Ethnicity' , 'Smoking','Smoking_binary','CVDFamilyHistory','Chronic.kidney.disease.prevalent',
              'macce_obs_time','macce.incident','macce','date_end_accel_raw','mortailty','date_of_death') #,
set.seed(0)  
#ars_val_dat <- data.frame(ars_val_dat)
#ars_val_dat <- ars_val_dat[,features]
ars_val_dat <- load_val_set(split = split,folds = 5,data_soruce = 'ukb')
ars_val_dat['censor_date'] <- as.Date(ars_val_dat$date_end_accel_raw) %m+% years(6)
ars_val_dat = allign_fu(ars_val_dat,'macce','macce_obs_time','censor_date')


#ars_val_dat = allign_fu(ars_val_dat,'macce','macce_obs_time','censor_date')


macce_cond = ars_val_dat$macce.incident == 1

death_censor = (ars_val_dat$mortailty == 1) &  (ars_val_dat$macce.incident ==0)
ars_val_dat[death_censor,'macce_obs_time'] =  as.numeric(difftime(as.Date(ars_val_dat[death_censor,'date_of_death']),as.Date(ars_val_dat[death_censor,'date_end_accel_raw']),units = "days")/365)

get_base_surv(df,lp,lab = 'macce.incident',time = 9,){
  res  <- tryCatch({
    base <- coxph(Surv(df$macce_obs_time,df[,lab])~offset(lp))
    base_surv <- summary(survfit(base),time=time)$surv
    return(base_surv)
  },
  
  error = function(e) {
    # Handle the error
    cat("An error occurred: ", conditionMessage(e), "\n")
    return(NA)  # Return a default value or do something else
  },
  
  finally = {
    # Cleanup code (optional)
  }
  )
  return(res)
}

get_test_scores <-function(df,df_base,lp,lp_base,lab = 'macce.incident',time = 9,th = 0.1,baseline_surv = NULL){
  res  <- tryCatch({
    
    base <- coxph(Surv(df_base$macce_obs_time,df_base[,lab])~offset(lp_base))
    base_surv <- summary(survfit(base),time=time)$surv
    risk_score <-  100 * (1 - base_surv^exp(lp))
    return(risk_score)
  },
  
  error = function(e) {
    # Handle the error
    cat("An error occurred: ", conditionMessage(e), "\n")
    return(NA)  # Return a default value or do something else
  },
  
  finally = {
    # Cleanup code (optional)
  }
  )
  return(res)
}
test_rs <- function(test_datasets,train_datasets,mods,test_lp,train_lp,base_surv = NULL){
  #get risk scores
  q_score_m <- list()
  q_score_f <- list()
  
  ars_score_m <- list()
  ars_score_f <- list()
  
  
  for (i in 1:5) {
    train_cond = train_datasets[[i]]$Sex ==1
    test_cond = test_datasets[[i]]$Sex ==1
    print('cond res')
    print(test_datasets[[i]][test_cond,'Sex'])
    q_score_m[[i]] <- get_scores(test_datasets[[i]][test_cond,],test_lp[[1]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[1]])
    #print(paste0('len = ',length(q_score_m[[i]] )))
    ars_score_m[[i]] <- get_scores(test_datasets[[i]][test_cond,],test_lp[[2]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[3]])
    
    q_score_f[[i]] <- get_scores(test_datasets[[i]][!test_cond,],test_lp[[3]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[2]])
    ars_score_f[[i]] <- get_scores(test_datasets[[i]][!test_cond,],test_lp[[4]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[4]])
    
    #q_score_m[[i]] <- get_test_scores(test_datasets[[i]][test_cond,],train_datasets[[i]][train_cond,],test_lp[[1]][[i]],train_lp[[1]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[1]])
    #ars_score_m[[i]] <- get_test_scores(test_datasets[[i]][test_cond,],train_datasets[[i]][train_cond,],test_lp[[2]][[i]],train_lp[[2]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[3]])
    
    #q_score_f[[i]] <- get_test_scores(test_datasets[[i]][!test_cond,],train_datasets[[i]][!train_cond,],test_lp[[3]][[i]],train_lp[[3]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[2]])
    #ars_score_f[[i]] <- get_test_scores(test_datasets[[i]][!test_cond,],train_datasets[[i]][!train_cond,],test_lp[[4]][[i]],train_lp[[4]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[4]])
    
  }
  #pool riks cores together using rubisn rule:https://pubmed.ncbi.nlm.nih.gov/25630926/
  male_q_pool <- Reduce(`+`, q_score_m) / 5
  print(mean(male_q_pool))
  male_ars_pool <- Reduce(`+`, ars_score_m) / 5
  
  female_q_pool <- Reduce(`+`, q_score_f) / 5
  female_ars_pool <- Reduce(`+`, ars_score_f) / 5
  return(list(male_q_pool,male_ars_pool,female_q_pool,female_ars_pool))
  
}


full_res <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/')) #" array.  risk_scores/ARS/bs_imp/con/6y/full/imp.  'risk_scores/ARS/bs_imp/con/6y/full/array/'
cond_res <-load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/',store,'/')) # array. "risk_scores/ARS/bs_imp/con/6y/condensed/imp .  'risk_scores/ARS/bs_imp/con/6y/condensed/array/'

#load orignal data
test_datasets <- list()
ars_val_dat <- ars_val_dat[ars_val_dat$StepsDayMedAdjusted>0,]
# Impute missing values in df_test using the imp_model
# Copy the settings from the original model
#model_settings <- imp_model$method

# Create a new model for the new dataset using the copied settings
#new_imp_model <- mice(ars_val_dat[, imp_vars], m = 5)

# Impute missing values in df_test using the new model


#mean_ars <-   lapply(imputed_datasets, function(df){sum(df[1:train_end,]$ActivityRiskScore)})
#imp_male <- lapply(imputed_datasets, function(df){df[df$Sex==1,]})
#imp_female <-lapply(imputed_datasets, function(df){df[df$Sex==0,]})

#mean_ars_m <- remove_rows(imputed_datasets,rows_with_na,met = 'ActivityRiskScore') #sapply(res, function(df) df[-unique(rows_with_na),met])
#res1_m <- sapply(res1, function(df) df[-unique(rows_with_na),met])
#mean_ars <-rowMeans(mean_ars)


#train_end < - nrow(swiss_re)
#full_len <-  train_end + nrow(ars_val_dat)
imp_vars <- c(fp_terms,imp_vars)
imp_model <- readRDS(paste0("risk_scores/stacked/array_job/",store,"/imp_model.rds"))
imp_test  <- readRDS( paste0("risk_scores/stacked/array_job/",store,"/imp_model.rds"))
imp_test <- mice.mids(imp_model, newdata = ars_val_dat[,imp_vars])

##############################

if(!fit_poly){
  print("rename")
  #colnames(imputed_datasets[[i]])[colnames(imputed_datasets[[i]]) == "ActivityRiskScore"] <- "ARS_1"
  imp_cond <- bs_datasets[[i]]$Sex == 1
  
  for(j in 1:length(fp_terms)){
    term <- fp_terms[j]
    new_term <- paste0(term,'_1')
    bs_datasets[[i]][imp_cond,new_term] <- bs_datasets[[i]][imp_cond,term] - mean(bs_datasets[[i]][imp_cond,term])
    bs_datasets[[i]][!imp_cond,new_term] <- bs_datasets[[i]][!imp_cond,term] - mean(bs_datasets[[i]][!imp_cond,term])
  }
}

#stacked_bs <- rbind(stacked_bs, bs_datasets[[i]] )  # Stack it onto the combined data frame
#store data for later optimism adjustmeants 
#}

if(!fit_poly){
  for(j in 1:length(fp_terms)){
    term <- fp_terms[j]
    fp_terms[j] <- paste0(term,'_1')
  }
}
##############################
test_datasets <- list()
for (i in 1:5) {
  #test_datasets[[i]] <- complete(new_imp_model,i)
  #test_datasets[[i]] <- na.omit(ars_val_dat)
  test_datasets[[i]] <- complete(imp_test, i)     #complete(imp_model, i)[(1+train_end): full_len,] #imp_data <- rbind(df,ars_val_dat)
  train_data <- complete(imp_model,i)
  test_datasets[[i]]['age'] <- test_datasets[[i]]$Age
  print("start data")
  print(i)
  
  
  if(model == 'qrisk'){
    test_datasets[[i]] <- selected_function(test_datasets[[i]],data.frame(train_data)) 
    test_datasets[[i]]  <- cbind(test_datasets[[i]] , model.matrix(~ Smoking - 1, data = test_datasets[[i]]))
    test_datasets[[i]]  <- remove_duplicate_columns(test_datasets[[i]] )
  }
  else{
    print('score')
    test_datasets[[i]] <- selected_function(test_datasets[[i]],complete(imp_test, i),imp_vars,imp_model = imp_model)
  }
  
  if(!fit_poly){
    print("rename")
    
    
    #imp_cond <- bs_datasets[[i]]$Sex == 1
    imp_cond <- test_datasets[[i]]$Sex == 1
    base_cond <- train_data$Sex  == 1
    
    for(j in 1:length(fp_terms)){
      term <- fp_terms[j]
      new_term <- paste0(term,'_1')
      test_datasets[[i]][imp_cond,new_term] <- test_datasets[[i]][imp_cond,term] - mean(train_data[base_cond,term])
      test_datasets[[i]][!imp_cond,new_term] <- test_datasets[[i]][!imp_cond,term] - mean(train_data[!base_cond,term])
    }
    
    
    
    # test_datasets[[i]][imp_cond,'ARS_1'] <- test_datasets[[i]][imp_cond,'ActivityRiskScore'] - mean(imputed_datasets[[1]][base_cond,'ActivityRiskScore']) #mean(test_datasets[[i]]$ActivityRiskScore)
    #test_datasets[[i]][!imp_cond,'ARS_1'] <- test_datasets[[i]][!imp_cond,'ActivityRiskScore'] -  mean(imputed_datasets[[1]][!base_cond,'ActivityRiskScore']) #mean(test_datasets[[i]]$ActivityRiskScore)
    
  }
  
}







#imp.ignore <- mice(imp_data[,imp_vars], ignore = c(rep(FALSE, 69899), rep(TRUE, 88438 - 69899)),  m = 5 )

imp_test  <- readRDS( paste0("risk_scores/stacked/array_job/",store,"/imp_model.rds"))

# Now, test_datasets contains 5 imputed datasets for df_test using the existing model's settings

#load orignal data
imputed_datasets <- list()
for (i in 1:5) {
  imputed_datasets[[i]] <- read.csv(paste0("risk_scores/stacked/array_job/",model,"/full_models/","org_imp_",i,".csv"))#risk_scores/array_job/full_models/","org_imp_",i,".csv". con/
}


if (fit_poly){
  #calulate new for,mula based on the fractional polynomial and age interactions
  mfp_model <- add_mfp(test_datasets,age_interact,fp_term_male,fp_term_female,formula,fp_terms = fp_terms)
  test_datasets <- mfp_model[[1]]
  
  mfp_model_con <- add_mfp(test_datasets,age_interact,fp_term_male_con,fp_term_female_con,formula_cond,fp_terms = fp_terms,suffix= '_con')
  test_datasets <- mfp_model_con[[1]]
  
} 

#calulate sghrinakge of predictors
rows_with_na <-c()
rows_with_na <- null_rows(full_res)
shrinkage <- get_optimism(full_res,rows_with_na)

#calulate sghrinakge of predictors
rows_with_na_con <- null_rows(cond_res)
shrinkage_con <- get_optimism(cond_res,rows_with_na_con)

full_models <- load_modles(path  = paste0('risk_scores/stacked/array_job/',store,'/full_models/')) #risk_scores/ARS/org_imp/con/ risk_scores/array_job/full_models/
cond_models <- load_modles(path  = paste0('risk_scores/stacked/array_job/',store,'/condensed_models/'),suffix = '_cond') # risk_scores/array_job/condensed_models/


###################################################
#get baslien survival for full models
print('male pooled S0')
base_male_q <- lapply(q_imp_mod_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod)})
base_male_q <-mean(unlist(base_male_q))
base_male_ars <- lapply(ars_imp_mod_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod)})
base_male_ars <-mean(unlist(base_male_ars))

print('female pooled S0')
base_female_q <- lapply(q_imp_mod_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod)})
base_female_q <-mean(unlist(base_female_q))
base_female_ars <- lapply(ars_imp_mod_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod)})
base_female_ars <-mean(unlist(base_female_ars))


#get baslien survival for condensed models
print('male pooled S0')
base_male_q_con <- lapply(q_imp_condensed_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod)})
base_male_q_con <-mean(unlist(base_male_q_con))
base_male_ars_con <- lapply(ars_imp_condensed_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod)})
base_male_ars_con <-mean(unlist(base_male_ars_con))

base_female_q_con <- lapply(q_imp_condensed_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod)})
base_female_q_con <- mean(unlist(base_female_q_con))
base_female_ars_con <- lapply(ars_imp_condensed_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod)})
base_female_ars_con <- mean(unlist(base_female_ars_con))

baseline_surv <- list(base_male_q,base_female_q,base_male_ars,base_female_ars)
baseline_surv_con <- list(base_male_q_con,base_female_q_con,base_male_ars_con,base_female_ars_con)



################################################################################################

#get orignal lp
base_adj_full <- adjust_lp(imputed_datasets,full_models,c(1,1,1,1),model = model)
base_rs_full <- adjust_rs(imputed_datasets,full_models,base_adj_full,base_surv = baseline_surv)

base_adj_con <- adjust_lp(imputed_datasets,cond_models,c(1,1,1,1),model = model)
base_rs_con <- adjust_rs(imputed_datasets,cond_models,base_adj_con,base_surv = baseline_surv_con)

lp_adj_full_test <- adjust_lp(test_datasets,full_models,c(1,1,1,1),model = model)#shrinkage
rs_adj_full_test <- test_rs(test_datasets,imputed_datasets,full_models,lp_adj_full_test,base_adj_full,base_surv = baseline_surv)

lp_adj_con_test <- adjust_lp(test_datasets,cond_models,c(1,1,1,1),model = model)#shrinkage_con
rs_adj_con_test <- test_rs(test_datasets,imputed_datasets,cond_models,lp_adj_con_test,base_adj_con,base_surv = baseline_surv_con)

df_val <-data.frame(test_datasets[[1]])
val_cond<- df_val$Sex ==1

df_val['qrisk3'] <- 0
df_val['ars'] <- 0
df_val['qrisk3_con'] <- 0
df_val['ars_con'] <- 0

df_val[val_cond,'qrisk3'] <- rs_adj_full_test[[1]]
df_val[val_cond,'ars'] <- rs_adj_full_test[[2]]

df_val[!val_cond,'qrisk3'] <- rs_adj_full_test[[3]]
df_val[!val_cond,'ars'] <- rs_adj_full_test[[4]]

df_val[val_cond,'qrisk3_con'] <- rs_adj_con_test[[1]]
df_val[val_cond,'ars_con'] <- rs_adj_con_test[[2]]

df_val[!val_cond,'qrisk3_con'] <- rs_adj_con_test[[3]]
df_val[!val_cond,'ars_con'] <- rs_adj_con_test[[4]]

preds <-c('qrisk3_prob','ars_prob')
df_val['qrisk3_prob'] <-df_val$qrisk3/100
df_val['ars_prob'] <-df_val$ars/100
save.image(file=paste0('risk_scores/ARS/cv_res/qrisk_split',split,'_ars_fp.RData'))

# Get the names of all objects in the global environment
all_objects <- ls()

# Identify names of functions
function_names <- all_objects[sapply(all_objects, function(x) is.function(get(x)))]

# Remove all objects that are not functions
rm(list = setdiff(all_objects, function_names))





png("/gpfs3risk_scores/ARS/nb_curve/ARS/score/test/ovr_nb.png", type="cairo")  
pgrs_nb <- stdca(data=df_val[ ,], outcome="macce.incident", ttoutcome="macce_obs_time", timepoint=6.0, 
                 predictors=preds, xstop=0.3, loess.span=0.2, smooth=TRUE)

dev.off()


#meaure peromance of adjusted risk scores
th = 0.1
calc_nb(df_val[val_cond,],df_val[val_cond,'qrisk3_con'],lab = 'macce.incident',th = th) - calc_nb(df_val[val_cond,],df_val[val_cond,'qrisk3'],lab = 'macce.incident',th = th)

calc_nb(df_val[val_cond,],df_val[val_cond,'qrisk3'],lab = 'macce.incident',th = th)

calc_nb(df_val[val_cond,],df_val[val_cond,'ars'],lab = 'macce.incident',th = th)

calc_nb(df_val[!val_cond,],df_val[!val_cond,'qrisk3'],lab = 'macce.incident',th = th)

calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars'],lab = 'macce.incident',th = th)




calc_nb(df_val[val_cond,],df_val[val_cond,'qrisk3_con'],lab = 'macce.incident',th = th)

calc_nb(df_val[val_cond,],df_val[val_cond,'ars_con'],lab = 'macce.incident',th = th)


calc_nb(df_val[!val_cond,],df_val[!val_cond,'qrisk3_con'],lab = 'macce.incident',th = th)

calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars_con'],lab = 'macce.incident',th = th)

calc_nb(df_val,df_val[,'qrisk3'],lab = 'macce.incident',th = th)
calc_nb(df_val,df_val[,'ars'],lab = 'macce.incident',th = th)

calc_nb(df_val,df_val[,'ars'],lab = 'macce.incident',th = th) - calc_nb(df_val,df_val[,'qrisk3'],lab = 'macce.incident',th = th)
calc_nb(df_val,df_val[,'ars_con'],lab = 'macce.incident',th = th) - calc_nb(df_val,df_val[,'qrisk3_con'],lab = 'macce.incident',th = th)
calc_nb(df_val,df_val[,'ars_con'],lab = 'macce.incident',th = th) - calc_nb(df_val,df_val[,'qrisk3'],lab = 'macce.incident',th = th)

get_nri(df_val[val_cond,],'qrisk3','ars','macce.incident',cutoff = c(0,th,1))
get_nri(df_val[!val_cond,],'qrisk3','ars','macce.incident',cutoff = c(0,th,1))
get_nri(df_val,'qrisk3','ars','macce.incident',cutoff = c(0,th,1))

get_nri(df_val[val_cond,],'qrisk3_con','ars_con','macce.incident',cutoff = c(0,th,1))
get_nri(df_val[!val_cond,],'qrisk3_con','ars_con','macce.incident',cutoff = c(0,th,1))

get_nri(df_val[val_cond,],'qrisk3','ars_con','macce.incident',cutoff = c(0,th,1))
get_nri(df_val[!val_cond,],'qrisk3','ars_con','macce.incident',cutoff = c(0,th,1))
get_nri(df_val,'qrisk3','ars_con','macce.incident',cutoff = c(0,th,1))
get_nri(df_val,'qrisk3','qrisk3_con','macce.incident',cutoff = c(0,th,1))

calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars'],lab = 'macce.incident',th = th) - calc_nb(df_val[!val_cond,],df_val[!val_cond,'qrisk3'],lab = 'macce.incident',th = th)
calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars_con'],lab = 'macce.incident',th = th) - calc_nb(df_val[!val_cond,],df_val[!val_cond,'qrisk3_con'],lab = 'macce.incident',th = th)

png('/gpfs3risk_scores/validation/ecg_reclass_age5.png',type = 'cairo',width=1400, height=400)
count_reclass_score(df_val,base = 'q',pred = 'rs_full',lab = 'macce.incident',th = 10,res_path = "risk_scores/validation/qrisk/",name = "reclass_scores.png")
count_reclass(df_val,base = 'qrisk3',pred = 'ars',lab = 'macce.incident',th = 10,res_path = "risk_scores/validation/qrisk/",name = "reclass_q_ars.png")
dev.off()
get_cal(df_val[val_cond,],df_val[val_cond,'qrisk3'],lab = 'macce.incident')
get_cal(df_val[val_cond,],df_val[val_cond,'ars'],lab = 'macce.incident')

get_cal(df_val[!val_cond,],df_val[!val_cond,'qrisk3'],lab = 'macce.incident')
get_cal(df_val[!val_cond,],df_val[!val_cond,'ars'],lab = 'macce.incident')

get_cal(df_val[val_cond,],df_val[val_cond,'qrisk3_con'],lab = 'macce.incident')
get_cal(df_val[val_cond,],df_val[val_cond,'ars_con'],lab = 'macce.incident')

get_cal(df_val[!val_cond,],df_val[!val_cond,'qrisk3_con'],lab = 'macce.incident')
get_cal(df_val[!val_cond,],df_val[!val_cond,'ars_con'],lab = 'macce.incident')


source(file.path("risk_scores",'cal_plot_imp.R'), echo = T)
#config


male_q = full_res[[1]][[1]]
male_ars = full_res[[1]][[2]]
female_q = full_res[[1]][[3]]
female_ars = full_res[[1]][[4]]

male_q_con = cond_res[[1]][[1]]
male_ars_con = cond_res[[1]][[2]]
female_q_con = cond_res[[1]][[3]]
female_ars_con = cond_res[[1]][[4]]


cal_curve(df_val[val_cond,],male_q,rows_with_na,lab = 'macce.incident', pred_event_prob = 'qrisk3',xmin =0,xmax = 20,hist_max = 5000,title = "Male QRISK3 calibration hc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/cv/split0/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df_val[val_cond,],male_ars,rows_with_na,lab = 'macce.incident', pred_event_prob = 'ars',xmin =0,xmax = 20,hist_max = 5000,title = "Male ARS calibration gc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/cv/split0/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)

cal_curve(df_val[!val_cond,],female_q,rows_with_na,lab = 'macce.incident', pred_event_prob = 'qrisk3',xmin =0,xmax = 20,hist_max = 5000,title = "Female QRISK3 calibration hc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/cv/split0/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df_val[!val_cond,],female_ars,rows_with_na,lab = 'macce.incident', pred_event_prob = 'ars',xmin =0,xmax = 20,hist_max = 5000,title = "Female ARS calibration hc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/cv/split0/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)



cal_curve(df_val[val_cond,,],male_q_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'qrisk3_con',xmin =0,xmax = 20,hist_max = 5000,title = "Male QRISK3 Condensed calibration hc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df_val[val_cond,,],male_ars_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'ars_con',xmin =0,xmax = 20,hist_max = 5000,title = "Male ARS Condensed calibratio hcn",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)

cal_curve(df_val[!val_cond,,],female_q_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'qrisk3_con',xmin =0,xmax = 20,hist_max = 5000,title = "Female QRISK3 Condensed calibration hc",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df_val[!val_cond,,],female_ars_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'ars_con',xmin =0,xmax = 20,hist_max = 5000,title = "Female ARS Condensed calibration hc ",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/val/",model,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)




cal_curve(df,female_ars_bs,lab = 'macce.incident', pred_event_prob = c('qrisk3','ars_con','ars'),xmin =0,xmax = 20,hist_max = 8000,title = "Risk score calibration",store_path = paste0("/gpfs3risk_scores/ARS/cal_curves/imp_6y/bhf/"),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)


get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_full_test[[1]]) / 5)
get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_full_test[[2]]) / 5)

get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_full_test[[3]]) / 5)
get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_full_test[[4]]) / 5)

get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_con_test[[1]]) / 5)
get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_con_test[[2]]) / 5)

get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_con_test[[3]]) / 5)
get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_con_test[[4]]) / 5)



get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_full_test[[2]]) / 5) - get_idx(df_val[val_cond,], Reduce(`+`, lp_adj_full_test[[1]]) / 5)
get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_full_test[[4]]) / 5) - get_idx(df_val[!val_cond,], Reduce(`+`, lp_adj_full_test[[3]]) / 5)


get_idx(df[sex_cond,], Reduce(`+`, lp_adj_full[[2]]) / 5) - get_idx(df[sex_cond,], Reduce(`+`, lp_adj_full[[1]]) / 5)
get_idx(df[!sex_cond,], Reduce(`+`, lp_adj_full[[4]]) / 5) - get_idx(df[!sex_cond,], Reduce(`+`, lp_adj_full[[3]]) / 5)




bs_metrics <- function(df_val,resample_indices,base = 'qrisk3', comp = 'ars',lp = null,base_lp = 1, comp_lp = 2 ,num_iterations = 1000){
  cidx_results <- vector("list", num_iterations)
  ici_results <- vector("list", num_iterations)
  r2_results <- vector("list", num_iterations)
  nb_results <- vector("list", num_iterations)
  
  cidx_comp <- vector("list", num_iterations)
  ici_comp <- vector("list", num_iterations)
  r2_comp <- vector("list", num_iterations)
  nb_comp<- vector("list", num_iterations)
  
  for (i in 1:num_iterations) {
    print(paste0('iter = ',i))
    # Perform resampling by using the precomputed indices
    resampled_data <- df_val[,][resample_indices[[i]],]
    # Perform resampling on lp_adj_full using the same indices
    resampled_lp_adj <- (Reduce(`+`, lp[[base_lp]]) / 5)[resample_indices[[i]]] #lapply(lp_adj_full[[1]], function(lp) lp[resample_indices[[i]], ])
    resampled_comp_lp <- (Reduce(`+`, lp[[comp_lp]]) / 5)[resample_indices[[i]]] #lapply(lp_adj_full[[1]], function(lp) lp[resample_indices[[i]], ])
    
    # Call your function with the resampled data and resampled lp_adj_full
    cidx <- get_idx(resampled_data, resampled_lp_adj)[[1]]
    #ici <- get_cal(resampled_data,resampled_data[,base],lab = 'macce.incident')
    nb <- calc_nb(resampled_data,resampled_data[,base],lab = 'macce.incident')
    
    # Store the result in the list
    cidx_results[[i]] <- cidx
    #ici_results[[i]] <- ici
    nb_results[[i]] <- nb
    
    cidx <- get_idx(resampled_data, resampled_comp_lp)[[1]]
    #ici <- get_cal(resampled_data,resampled_data[,comp],lab = 'macce.incident')
    nb <- calc_nb(resampled_data,resampled_data[,comp],lab = 'macce.incident')
    
    # Store the result in the list
    cidx_comp[[i]] <- cidx
    #ici_comp[[i]] <- ici
    nb_comp[[i]] <- nb
  }
  
  cidx_results <- unlist(cidx_results)
  #ici_results <- unlist(ici_results)
  nb_results <- unlist(nb_results)
  base_res <- list(cidx_results,nb_results)
  
  cidx_comp <- unlist(cidx_comp)
  ici_comp <- unlist(ici_comp)
  nb_comp <- unlist(nb_comp)
  comp_res <- list(cidx_comp,nb_comp)
  return(list(base_res,comp_res))
}
# Define the number of bootstrap iterations
num_iterations <- 1000


base <- 'qrisk3'
comp <- 'ars'
base_lp <- 1
comp_lp <- 2

# Precompute the resampling indices to use for all iterations
resample_indices <- lapply(1:num_iterations, function(i) sample(nrow(df_val[,]), replace = TRUE))

bs_res_core <- bs_metrics(df_val,resample_indices,lp = lp_adj_full_test,base = 'qrisk3', comp = 'ars',base_lp = 1, comp_lp = 2 )
bs_res_sub <- bs_metrics(df_val,resample_indices,lp_adj_con,base = 'qrisk3', comp = 'ars_con',base_lp = 1, comp_lp = 2 )

bs_res_core2 <- bs_metrics(df_val,resample_indices,lp = lp_adj_full_test,base = 'qrisk3', comp = 'ars',base_lp = 1, comp_lp = 2 )
bs_res_sub2 <- bs_metrics(df_val,resample_indices,lp_adj_con,base = 'qrisk3', comp = 'ars_con',base_lp = 1, comp_lp = 2 )

#sex comaprison
male_indices <- lapply(1:num_iterations, function(i) sample(nrow(df_val[sex_cond,]), replace = TRUE))
female_indices <- lapply(1:num_iterations, function(i) sample(nrow(df_val[!sex_cond,]), replace = TRUE))

bs_res_core <- bs_metrics(df_val,male_indices,lp = lp_adj_full_test,base = 'qrisk3', comp = 'ars',base_lp = 1, comp_lp = 2 )
bs_res_sub <- bs_metrics(df_val,female_indices,lp_adj_con,base = 'qrisk3', comp = 'ars',base_lp = 1, comp_lp = 2 )



source(file.path("plot_scripts/",'get_ci.R'), echo = T)
source(file.path("plot_scripts/",'unpaired_t.R'), echo = T)
source(file.path("ecg_scores/",'boot_strap_res.R'), echo = T)
source(file.path("plot_scripts/",'unpaired_t.R'), echo = T)

ci_calc(bs_res_core2[[1]][[2]])
ci_calc(bs_res_core2[[2]][[2]])

pool_ci(bs_res_core2[[1]][[2]])
pool_ci(bs_res_core2[[2]][[2]])

onetPermutation(x = (bs_res_core2[[2]][[2]] - bs_res_core2[[1]][[2]]) , nsim = 1000, plotit = FALSE)

print(t.test((bs_res_core2[[2]][[2]] - bs_res_core2[[1]][[2]]), mu = 0))
mean_stat <- function(data, idx)
{
  df <- data[idx]
  
  # Find the spearman correlation between 
  # the 3rd and 4th columns of dataset
  c(mean(df))
}

calc_nb(resampled_data,resampled_data[,comp],lab = 'macce.incident')
bootstrap <- boot(bs_res_core2[[2]][[2]], calc_nb, R = 1000)

postive_n <- c()
neative_n <- c()


c_val <- function(data, indices,base =3 ,lp =lp_adj_full_test )
{
  get_idx(data[indices,],(Reduce(`+`, lp[[base]])[indices] / 5))[[1]]
}

c_diff <- function(data, indices,base =3 ,comp = 4,lp =lp_adj_full_test ) {
  q_cidx <- get_idx(data[indices,],(Reduce(`+`, lp[[base]])[indices] / 5))[[1]]
  ars_cidx <- get_idx(data[indices,],(Reduce(`+`, lp[[comp]]) / 5)[indices])[[1]]
  #print(q_cidx)
  #print(ars_cidx)
  ars_cidx - q_cidx
}


ici_diff <- function(data, indices) {
  get_cal(df_val[indices,],df_val[indices,'ars'],lab = 'macce.incident') - get_cal(df_val[indices,],df_val[indices,'qrisk3'],lab = 'macce.incident')
}

nb_diff <- function(data, indices,base = 'qrisk3',comp = 'ars',th = 0.1) {
  diff = calc_nb(data[indices,],rs =data[indices,comp] , lab = 'macce.incident', th = th) - calc_nb(data[indices,],rs =data[indices,base] , lab = 'macce.incident', th = th)
  diff
}

bootstrap_m_full <- boot(data = df_val[val_cond,], statistic = c_diff, R = 1000,base = 1, comp = 2,lp = lp_adj_full_test)
bootstrap_f_full <- boot(data = df_val[!val_cond,], statistic = c_diff, R = 1000,base = 3, comp = 4,lp = lp_adj_full_test)

bootstrap_m_con <- boot(data = df_val[val_cond,], statistic = c_diff, R = 1000,base = 1, comp = 2,lp = lp_adj_con_test)
bootstrap_f_con <- boot(data = df_val[!val_cond,], statistic = c_diff, R = 1000,base = 3, comp = 4,lp = lp_adj_con_test)

print(paste0('obs = ',bootstrap_f$t0))

pool_ci(bootstrap_m_full$t)
get_p_val(bootstrap_m_full$t0,bootstrap_m_full$t)

pool_ci(bootstrap_f_full$t)
get_p_val(bootstrap_f_full$t0,bootstrap_f_full$t)

c_df <- data.frame(df_val[!sex_cond,])
c_df['q_cidx'] <-  get_idx(c_df,Reduce(`+`, lp_adj_full_test[[3]]) / 5)[[1]]
c_df['ars_cidx'] <-  get_idx(c_df,Reduce(`+`, lp_adj_full_test[[4]]) / 5)[[1]]

get_c <- function(data, indices)
{
  get_idx(data[indices,],(Reduce(`+`, lp_adj_full_test[[4]])[indices] / 5))[[1]]  
}
get_nb <- function(data, indices)
{
  calc_nb(data[indices,],rs =data[indices,'ars'] , lab = 'macce.incident', th = 0.1)
}
bootstrap_full <- boot(data = df_val[!val_cond,], statistic = get_c, R = 1000)


bootstrap_full
mean(bootstrap_full$t)
pool_ci(bootstrap_full$t)
boot.ci(boot.out = bootstrap_full, 
        type = c("norm", "basic",
                 "perc"))

boot_full.under.H0 <- bootstrap_full$t - mean(bootstrap_full$t)

mean(abs(boot_full.under.H0) > abs(bootstrap_full$t0))

mean(bootstrap_full$t <0)
# Bootstrap function to calculate the statistic of interest
calculate_difference <- function(data, indices) {
  sample1 <- data[indices, 'A']
  sample2 <- data[indices, 'B']
  return(mean(sample1) - mean(sample2))
}

bs_diff <-bs_res_core2[[2]][[2]] - bs_res_core2[[1]][[2]]
observed_difference <-  calc_nb(df_val[,],df_val[,'ars'],lab = 'macce.incident') -calc_nb(df_val[,],df_val[,'qrisk3'],lab = 'macce.incident')
mean(abs(bs_diff) >= abs(observed_difference)) #abs(observed_difference)
mean(abs(observed_difference) < abs(bs_diff - mean(bs_diff)))
mean(bs_diff <= 0)
print(t.test(bs_diff, mu = 0))


diff2 = function(d1,i){
  d = d1; 
  d$group <- d$group[i];  # randomly re-assign groups
  Mean= tapply(X=d$time, INDEX=d$group, mean)
  Diff = Mean[1]-Mean[2]
  Diff
}


b4 = boot(data = nb_df, statistic = calculate_difference, R = 1000)
mean(abs(b4$t) > abs(b4$t0))

2 * min(mean(bs_diff >= observed_difference),
        mean(bs_diff <= observed_difference))



#c-index sig
c_diff <- bs_res_core2[[2]][[1]] - bs_res_core2[[1]][[1]]
c_obs <- get_idx(df_val[sex_cond,], Reduce(`+`, lp_adj_full_test[[2]]) / 5) -get_idx(df_val[sex_cond,], Reduce(`+`, lp_adj_full_test[[1]]) / 5)
c.under.H0 <- c_diff - mean(c_diff)
mean(abs(c.under.H0) > abs(c_obs))

png('/gpfs3ecg_scores/nb_diff.png',type = 'cairo',width=1400, height=400)
hist(bootstrap$t)
dev.off()

with(bootstrap, pnorm(abs((2*t0 - mean(t) - 1) / sqrt(var(t)[1,1])), lower.tail=F)*2)

dev_df <- imputed_datasets[[1]]
dev_cond <- dev_df$Sex == 1

dev_df['age'] <-dev_df$Age
dev_df['qrisk3'] <- 0
dev_df['ars'] <- 0
dev_df['qrisk3_con'] <- 0
dev_df['ars_con'] <- 0

dev_df[dev_cond,'qrisk3'] <- base_rs_full [[1]]
dev_df[dev_cond,'ars'] <- base_rs_full [[2]]

dev_df[!dev_cond,'qrisk3'] <- base_rs_full [[3]]
dev_df[!dev_cond,'ars'] <- base_rs_full [[4]]

dev_df[dev_cond,'qrisk3_con'] <- base_rs_con[[1]]
dev_df[dev_cond,'ars_con'] <- base_rs_con[[2]]

dev_df[!dev_cond,'qrisk3_con'] <- base_rs_con [[3]]
dev_df[!dev_cond,'ars_con'] <- base_rs_con[[4]]



pool_bs(full_res[[1]][[3]],'c_idx',rows_with_na)
pool_bs(full_res[[1]][[4]],'c_idx',rows_with_na)

dev_cond = dev_df$Sex == 0
dev_obs <- calc_nb(dev_df[dev_cond,],dev_df[dev_cond,'ars_con'],lab = 'macce.incident',th = th) - calc_nb(dev_df[dev_cond,],dev_df[dev_cond,'qrisk3'],lab = 'macce.incident',th = th)
#dev_obs <- get_idx(dev_df[dev_cond,], Reduce(`+`, base_adj_con[[2]]) / 5) -get_idx(dev_df[dev_cond,], Reduce(`+`, base_adj_full[[1]]) / 5)
dev_obs
q_dev_cidx <- rowMeans(remove_rows(full_res[[1]][[3]],rows_with_na,met ='nb_10'))
ars_dev_cidx <- rowMeans(remove_rows(full_res[[1]][[4]],rows_with_na,met ='nb_10'))
dev_diff <- ars_dev_cidx - q_dev_cidx
mean(dev_diff)
pool_ci(dev_diff)

dev.under.H0 <- dev_diff - mean(dev_diff)
mean(abs(dev.under.H0) > abs(dev_obs))

show_stat <- function(stat){
  print(mean(stat))
  pool_ci(stat)
}
show_stat(stat <- rowMeans(remove_rows(full_res[[1]][[3]],rows_with_na,met ='nb_10')))
show_stat(stat <- rowMeans(remove_rows(cond_res[[1]][[4]],rows_with_na,met ='nb_10')))
show_stat(stat <- rowMeans(remove_rows(full_res[[1]][[4]],rows_with_na,met ='nb_10')))
save.image(file='risk_scores/ARS/cv_res/ars_val_eval.RData')










#step 5 process reuslts
get_p_val <- function(obs,diff){
  p = mean(abs(obs) < abs(diff - mean(diff)))
  print(paste0('centered p = ',p))
  print(paste0('p over 0 ',mean(diff <0)))
}
c_diff <- function(data,lp_base,lp_comp ) {
  q_cidx <- get_idx(data[,],data[,lp_base])[[1]]
  comp_cidx <- get_idx(data[,],data[,lp_comp])[[1]]
  #print(q_cidx)
  #print(ars_cidx)
  return(comp_cidx - q_cidx)
  
}


ici_diff <- function(data,rs,rs_base ='ukbb_q') {
  print('hello')
  return(get_cal(data[,],data[,rs],lab = 'macce_10y') - get_cal(data[,],data[,rs_base],lab = 'macce_10y'))
}

nb_diff <- function(data, rs,rs_base = 'ukbb_q') {
  #print(rs)
  #print(calc_nb(data[,],rs =data[,rs] , lab = 'macce.incident', th = 0.1))
  #print(mean(data[,rs]))
  #print('qrisk')
  #print( calc_nb(data[,],rs =data[,'ukbb_q'] , lab = 'macce.incident', th = 0.1))
  #print(mean(data[,'ukbb_q']))
  print(rs_base)
  diff = calc_nb(data[,],rs =data[,rs] , lab = 'macce.incident', th = 0.1) - calc_nb(data[,],rs =data[,rs_base] , lab = 'macce.incident', th = 0.1)
  return(diff)
}


r2_diff <- function(data,lp_base,lp_comp) {
  print(get_r2(data[,lp_base] ))
  diff = get_r2(data[,lp_comp]) - get_r2(data[,lp_base])
  return(diff)
}


nri_subset <- function(df,cond,rs,res_path,offset){
  bs_df <- read.csv(paste0(res_path,'bs_res_para.csv'))
  start <- (offset *1000) + 1
  fin  <- (offset +1) * 1000
  
  delta_nb <- bs_df[start:fin,'nb_10'] - bs_df[1:1000,'nb_10']
  
  observed_difference <-nb_diff(df[cond,],rs)
  
  get_nri(df[cond,] ,'ukbb_q',rs,'macce_10y')
  print(mean(delta_nb))
  print(pool_ci(delta_nb))
  print('')
  print("p above zero")
  print(mean(delta_nb > 0))
  print("ho: disturbutions differ")
  print(paste0("obsrevd diff = ",observed_difference))
  
  print(mean(abs(observed_difference) < abs(delta_nb - mean(delta_nb))))
  
  print("two sided test")
  print(2 * min(mean(delta_nb >= observed_difference),
                mean(delta_nb <= observed_difference))
  )
  
  # Perform the test
  print("wilcox.test")
  test_result <- wilcox.test(delta_nb, observed_difference, paired = FALSE, alternative = "greater")
  
  # Print the p-value
  print(test_result$p.value)
  
  
}

res <- read.csv()
show_diff <- function(df,met,offset,org_df,rs,offset_base = 0,rs_base = 'ukbb_q',lp_base = NULL,lp_comp = NULL,digits = 3,test = 'greater'){
  start <- (offset *1000 )+1
  fin <- (offset +1) * 1000
  
  base_start <- (offset_base *1000 )+1
  base_fin <- (offset_base +1) * 1000
  
  diff <- df[start:fin,met]- df[base_start:base_fin,met]
  diff_h0 <- diff - mean(diff)
  png('/gpfs3delta_nb.png',type = 'cairo',width=1400, height=400)
  hist(diff,breaks = 20)
  dev.off()
  png('/gpfs3delta_h0.png',type = 'cairo',width=1400, height=400)
  hist(diff_h0,breaks = 20)
  dev.off()
  print(paste0('H1: sd: ',sd(diff),'var: ',var(diff)))
  print(paste0('H0: sd: ',sd(diff_h0),' var: ',var(diff_h0)))
  avg <- round(mean(diff),digits =digits)
  ci <- round(pool_ci(diff),digits = digits)
  print(paste0('mean = ',mean(diff)))
  print(pool_ci(diff))
  
  
  #get obs diff
  print('get diff')
  if(met == 'nb_10'){
    observed_difference <- nb_diff(org_df,rs,rs_base = rs_base)
  }else if(met == 'ici'){
    observed_difference <- ici_diff(org_df,rs,rs_base = rs_base)
  }else if(met == 'r2'){
    observed_difference <- r2_diff(org_df,lp_base,lp_comp)
  }else if(met == 'cidx'){
    observed_difference <- c_diff(org_df,lp_base,lp_comp)
  }
  
  
  print("ho: disturbutions differ")
  print(paste0("obsrevd diff = ",observed_difference))
  #print(mean(abs(observed_difference) < abs(diff - mean(diff))))
  p = mean(abs(observed_difference) < abs(diff - mean(diff)))
  
  print("p above zero")
  print(mean(diff < 0))
  print("two sided test")
  print(2 * min(mean(diff >= observed_difference),
                mean(diff <= observed_difference))
  )
  print("two sided test 0's")
  print(2 * min(mean(diff >= 0),
                mean(diff <= 0))
  )
  # Perform the test
  print("wilcox.test")
  test_result <-  wilcox.test(df[start:fin,met], df[base_start:base_fin,met], paired = TRUE,alternative = "two.sided") #wilcox.test(diff, observed_difference, paired = TRUE, alternative = "greater")
  
  # Print the p-value
  print(test_result$p.value)
  
  diff_h0 <- diff - mean(diff)
  cvs <- quantile(observed_difference - diff   , c(0.025, 0.975))
  
  
  print(paste0('percentile p-val: ',mean(diff > cvs[1] & diff < cvs[2])))
  print(paste0('percentile p-val 2 : ',2 * min(mean(diff_h0 < observed_difference), mean(diff_h0 > observed_difference))))
  se <- sqrt(var(diff))
  z <- observed_difference/se
  print(paste0('z-score p: ',(1-pnorm(abs(-z)))*2))
  
  norm_ci <- 2*observed_difference - mean(diff) + qnorm(c(0.025, 0.975)) %o% sqrt(var(diff))
  
  print(paste0('norm p-val: ',(pnorm(abs((2*observed_difference - mean(diff) - 1) / sqrt(var(diff))), lower.tail=F)*2)))
  
  
  print(paste0(avg,'(',ci[1],' - ',ci[2],') p = ',p))
  
}

window = 'full'
offset = 5
score = paste0('ukbb_',window)
lp = paste0('',window,'_lin')

scores = c('rest','ex','rec','all','full')
for(s in 1:length(scores)){
  
  window = paste0('',scores[s])
  offset = s
  score = paste0('ukbb_',window)
  lp = paste0('',window,'_lin')
  
  cat(paste0('\n score: ',scores[s],'\n'))
  #show_diff(lin_res,'cidx',offset,Subjects_lin,score,'q_lin',lp)
  show_diff(covid_res,'nb_10',offset,Subjects_covid,score,offset_base = 0,rs_base = 'ukbb_q',lp_base = 'q_lin',lp_comp = lp) 
}


show_diff(ds_res,'cidx',offset,Subjects_full,score,'q_lin',lp)
show_diff(ds_res,'ici',offset,Subjects_full,score,'q_lin',lp,digits = 4)
show_diff(ds_res,'r2',offset,Subjects_full,score,'q_lin',lp)

