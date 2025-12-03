
library(survival)
library(rms)
library(mice)
library(dplyr)
library(ggplot2)
library(survival)
library(rms)
library(pROC)
library(PredictABEL)
library(plyr)
library(Hmisc)
library(rmda)
library(metafor)
library(survival)
library(rms)
library(prodlim)
library(lubridate)
library(mfp)



create_bs <-function(df,path,iter = 100){
  print('path = ')
  print(path)
  print('new path')
  print(paste0(path,"/bs_samples.csv"))
  samples <- data.frame(matrix(NA, ncol = nrow(df), nrow = iter))
  #samples <- c()
  idx = seq(1:nrow(df))
  #samples <-list(samples,list(idx))
  #samples[,] <- idx
  
  for(i in 1:iter){
    #samples <- list(samples,list(sample(idx, replace = TRUE)))
    samples[i,] <- sample(idx, replace = TRUE)
  }
  print('writting csv')
  
  write.csv(samples,paste0(path,"/bs_samples.csv"))
  return(samples)
}

main_model <- function(df,model,store,fit_poly,fp_terms,data_soruce,model_type,age_interact = TRUE,new_bs = FALSE){
  source(file.path("risk_scores/array_job/9_year_censoring/centered",'ARS_utils.R'), echo = T)
  print('data_soruce')
  selected_function <- switch(model,
                              "qrisk" = get_q_feature,
                              "score2" = get_score_features,
                              "aha" = get_aha_feature,
                              default = {
                                stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
                              }
  )
  
  
  if(model == 'qrisk') {
    imp_vars <- q_vars
    formula <- QRISK3
    formula_m <- QRISK3
    formula_f <- QRISK3
    formula_cond <- QRISK3_cond
  } else if(model == 'score2') {
    imp_vars <- score_vars
    formula <- SCORE2
    formula_m <- SCORE2
    formula_f <- SCORE2
    formula_cond <- SCORE2_cond
  } else if(model == 'aha') {
    imp_vars <- aha_vars
    formula <- AHA
    formula_m <- AHA_m
    formula_f <- AHA_f
    formula_cond <- AHA_cond
  } else if(model == 'fr'){
    imp_vars <- framingham_vars
    formula <- framingham
    formula_m <- framingham_m
    formula_f <- framingham_f
    formula_cond <- framingham_cond
  }else {
    stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
  }
  
  
  
  
  

  
  # Step 1: Load your data and define your survival object
  # Assuming 'macce_obs_time' is the time-to-event variable and 'macce_incident' is the event indicator
  features <- c("eid","ActivityRiskScore",'Age' ,'Sex', 'BodyMassIndex' ,'Diabetes', 'diabetes.type1.prevalent' , 'diabetes.type2.prevalent',
                "StepsDayMedAdjusted","SleepActivity","ModerVigorActivity",'LightActivity','SedentaryActivity','AccOverallAvg',
                'Essential..primary..hypertension.prevalent',  'SystolBloodPressur' , 'TownsendIndex' ,'Cholesterol','HdlCholesterol', 'CholesRatio' , 'Corticosteroid.prevalent' , 'Migraine.prevalent' ,
                'Rheumatoid.arthritis.prevalent' , 'Systemic.lupus.erythematosus.prevalent' , 'Severe.mental.illnes.prevalent' ,
                'Atrial.fibrillation.and.flutter.prevalent' ,'sbp5' , 'Ethnicity' , 'Smoking','Smoking_binary','CVDFamilyHistory','Chronic.kidney.disease.prevalent',
                'macce_obs_time','macce.incident','macce','date_end_accel_raw','mortailty','date_of_death')
  
  
  
  set.seed(0)  
  print('set seed')
  
  
  train_end <- nrow(df)

  
  #test_end <- nrow(df) - train_end
  print('select features')
  print(dim(df))
  print(colnames(df))
  
  df <- df[,features]
  print('got features')
  df['age'] <- df$Age #to presrve age post mean centering
  #df['ActivityRiskScore'] <- abs(df$ActivityRiskScore)
  
  #Obtain tiem-to-event labels for time point of intrest i. 5,6 or 10 years
  df['censor_date'] <- as.Date(df$date_end_accel_raw) %m+% years(eval_time)
  df[['macce_eval']] = df$macce
  df[['macce_eval_time']] = df$macce_obs_time
  df[['macce_eval_lab']] = df$macce.incident

  df = allign_fu(df,'macce_eval','macce_eval_time','censor_date',suffix = '_lab')
  print('mortailty')
  
  macce_cond = df$macce.incident == 1
  
  death_censor = (df$mortailty == 1) &  (df$macce.incident ==0)
  df[death_censor,'macce_obs_time'] =  as.numeric(difftime(as.Date(df[death_censor,'date_of_death']),as.Date(df[death_censor,'date_end_accel_raw']),units = "days")/365)
  
  
  

  imp_vars <- c(fp_terms,imp_vars,'eid','macce_eval_time','macce_eval_lab')
  print('imp vars')
  print(imp_vars)
  pred_matrix <- make.predictorMatrix(df[,imp_vars])
  # Exclude eid and eval labels form being use din the impuation model
  pred_matrix[, c("eid", "macce_eval_time","macce_eval_lab")] <- 0  # Do not use eid/col1 to predict others
  pred_matrix["eid", ] <- 0             # Do not impute eid
  pred_matrix["macce_eval_time", ] <- 0            # Do not impute macce_eval_lab
  pred_matrix["macce_eval_lab", ] <- 0            # Do not impute macce_eval_lab
  
  imp_model <-  mice(df[,imp_vars], method = "pmm", predictorMatrix = pred_matrix,seed = 0,m = 5)#ignore = c(rep(FALSE, train_end),rep(TRUE, test_end)
  
  #we can now remove thes test data set after retriving out imputatiion model
  #df <- df[1:train_end,]
  #stacked_data <- complete(imp_model, n = 5,action = 'long',include = FALSE)
  stacked_data <- data.frame()
  
  imputed_datasets <- list()
  
  # Store each imputed dataset in the list
  for (i in 1:5) {
    imputed_datasets[[i]] <- complete(imp_model, i)#[1:train_end,]
    imputed_datasets[[i]]['age'] <- imputed_datasets[[i]]$Age #preserve age for later testing
    print("start data")
    print(dim(imputed_datasets[[i]]))
    print(i)
    #imputed_datasets[[i]] <- get_q_feature(imputed_datasets[[i]],data.frame(imputed_datasets[[i]]))
    
    
    if(model == 'qrisk'){
      imputed_datasets[[i]] <- selected_function(imputed_datasets[[i]],data.frame(imputed_datasets[[i]])) 
      print('got imp')
      print('bind')
      imputed_datasets[[i]]  <- cbind(imputed_datasets[[i]] , model.matrix(~ Smoking - 1, data = imputed_datasets[[i]]))
      print('smoking')
      imputed_datasets[[i]]  <- remove_duplicate_columns(imputed_datasets[[i]] )
      print('no dups')
    }
    else{
      print('score')
      imputed_datasets[[i]] <- selected_function(imputed_datasets[[i]],data.frame(imputed_datasets[[i]]),imp_vars)
    }
    if(!fit_poly){
      print("rename")
      #colnames(imputed_datasets[[i]])[colnames(imputed_datasets[[i]]) == "ActivityRiskScore"] <- "ARS_1"
      imp_cond <- imputed_datasets[[i]]$Sex == 1
      #imputed_datasets[[i]][imp_cond,'ARS_1'] <- imputed_datasets[[i]][imp_cond,'ActivityRiskScore'] - mean(imputed_datasets[[i]][imp_cond,'ActivityRiskScore'])
      #imputed_datasets[[i]][!imp_cond,'ARS_1'] <- imputed_datasets[[i]][!imp_cond,'ActivityRiskScore'] - mean(imputed_datasets[[i]][!imp_cond,'ActivityRiskScore'])
      for(j in 1:length(fp_terms)){
        term <- fp_terms[j]
        new_term <- paste0(term,'_1')
        imputed_datasets[[i]][imp_cond,new_term] <- imputed_datasets[[i]][imp_cond,term] #- mean(imputed_datasets[[i]][imp_cond,term])
        imputed_datasets[[i]][!imp_cond,new_term] <- imputed_datasets[[i]][!imp_cond,term] #- mean(imputed_datasets[[i]][!imp_cond,term])
      }
    }
    stacked_data <- rbind(stacked_data, imputed_datasets[[i]] )  # Stack it onto the combined data frame
    #store data for later optimism adjustmeants 
  }
  
  if(!fit_poly){
    for(j in 1:length(fp_terms)){
      term <- fp_terms[j]
      fp_terms[j] <- paste0(term,'_1')
    }
  }
  
  # Convert back to a mids object

  #get fractional polynomal term in stacked data set 
  #lapply(bs_datasets, function(df) {df = df[df$ActivityRiskScore >0.001]}
  stack_cond = stacked_data$Sex == 1
  print(data_soruce)
  if (fit_poly){
    fp_male <- fit_mfp(stacked_data[stack_cond,],age_interact,formula = formula,model = model,fp_terms = fp_terms) # add fp_terms formula = formula
    fp_female <- fit_mfp(stacked_data[!stack_cond,],age_interact,formula = formula,model = model,fp_terms = fp_terms)#formula = formula
    
    fp_formula_male <- fp_male[[1]]
    fp_formula_female <- fp_female[[1]]
    
    fp_term_male <- fp_male[[2]]
    fp_term_female <- fp_female[[2]]
    
    #calulate new for,mula based on the fractional polynomial and age interactions
    mfp_model <- add_mfp(imputed_datasets,age_interact,fp_term_male,fp_term_female,formula,fp_terms = fp_terms,center = FALSE)
    imputed_datasets <- mfp_model[[1]]
    fp_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model[[2]])
    fp_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model[[3]])
  } else{
    base <-"Surv(macce_obs_time,macce.incident)~"
    for(t in fp_terms){
      base <- paste0(base,' ', t , ' + ')
    }
    fp_formula_male <- paste0(base,formula)
    fp_formula_female <- paste0(base,formula)
    

    
    if(model == 'qrisk' & age_interact)
    {
      
      for(t in fp_terms){
        fp_formula_male <- paste0(fp_formula_male,' + age_1 * ', t)
        fp_formula_male <- paste0(fp_formula_male,' + age_2 * ', t)
        
        fp_formula_female <- paste0(fp_formula_female,' + age_1 * ', t)
        fp_formula_female <- paste0(fp_formula_female,' + age_2 * ', t)
      }
      
    }
    else if (age_interact){
      
      for(t in fp_terms){
        fp_formula_male <- paste0(fp_formula_male,' + Age * ', t)
        
        fp_formula_female <- paste0(fp_formula_female,' + Age * ', t)
      }
      
    }
  }
  
  
  #saveRDS(ars_mod_m, file = paste0(path,"ars_mod_m", suffix,".rds"))
  
  #fit full models  
  male_mods <- lapply(imputed_datasets, function(imputed_df) {
    #fit_model(imputed_df[imputed_df$Sex == 1,])
    fit_stacked(imputed_df[imputed_df$Sex == 1,],formula,fp_formula_male,model = model) #org_formula = formula
    
  })
  
  female_mods <- lapply(imputed_datasets, function(imputed_df) {
    #fit_model(imputed_df[imputed_df$Sex == 0,])
    fit_stacked(imputed_df[imputed_df$Sex == 0,],formula,fp_formula_female,model = model) #org_formula = formula
  })
  
  q_imp_mod_m <- list()
  ars_imp_mod_m <- list()
  
  q_imp_mod_f <- list()
  ars_imp_mod_f <- list()
  
  for (i in 1:5) {
    q_imp_mod_m[[i]] <- male_mods[[i]][[1]]
    ars_imp_mod_m[[i]] <- male_mods[[i]][[2]]
    
    q_imp_mod_f[[i]] <- female_mods[[i]][[1]]
    ars_imp_mod_f[[i]] <- female_mods[[i]][[2]]
  }
  
  
  q_pool_mod_m <- summary(pool(q_imp_mod_m))
  ars_pool_mod_m  <- summary(pool(ars_imp_mod_m))
  
  q_pool_mod_f <- summary(pool(q_imp_mod_f))
  ars_pool_mod_f  <- summary(pool(ars_imp_mod_f))
  
  #fit condensed models
  if(fit_poly){
    fp_con_male <- fit_mfp(stacked_data[stack_cond,],age_interact,formula = formula_cond,model = model,fp_terms = fp_terms) #add fp_terms when testing
    fp_con_female <- fit_mfp(stacked_data[!stack_cond,],age_interact,formula = formula_cond,model = model,fp_terms = fp_terms)
    
    
    fp_formula_male_con <- fp_con_male[[1]]
    fp_formula_female_con <- fp_con_female[[1]]
    
    fp_term_male_con <- fp_con_male[[2]]
    fp_term_female_con <- fp_con_female[[2]]
    
    #calulate new for,mula based on the fractional polynomial and age interactions
    mfp_model_con <- add_mfp(imputed_datasets,age_interact,fp_term_male_con,fp_term_female_con,formula_cond,fp_terms = fp_terms,suffix= '_con',center = FALSE)
    
    imputed_datasets <- mfp_model_con[[1]]
    fp_con_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model_con[[2]])
    fp_con_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model_con[[3]])
    
  }else{
    
    base <-"Surv(macce_obs_time,macce.incident)~"
    for(t in fp_terms){
      base <- paste0(base,' ', t , ' + ')
    }
    fp_con_formula_male <- paste0(base,formula_cond)
    fp_con_formula_female <- paste0(base,formula_cond)
    
    #fp_con_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula_cond)
    #fp_con_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula_cond)
    
    if(model == 'qrisk' & age_interact)
    {
      for(t in fp_terms){
        fp_con_formula_male <- paste0(fp_con_formula_male,' + age_1 * ', t)
        fp_con_formula_male <- paste0(fp_con_formula_male,' + age_2 * ', t)
        
        fp_con_formula_female <- paste0(fp_con_formula_female,' + age_1 * ', t)
        fp_con_formula_female <- paste0(fp_con_formula_female,' + age_2 * ', t)
      }
      
     
    }
    else if(age_interact){
      
      for(t in fp_terms){
        fp_con_formula_male <- paste0(fp_con_formula_male,' + Age * ', t)
        fp_con_formula_female <- paste0(fp_con_formula_female,' + Age * ', t)
        
      }
    
    }
  }
  
  
  
  male_condensed <- lapply(imputed_datasets, function(imputed_df) {
    #fit_condensed(imputed_df[imputed_df$Sex == 1,])
    fit_stacked(imputed_df[imputed_df$Sex == 1,],formula_cond,fp_con_formula_male,model = model) #org_formula = formula_cond
  })
  
  female_condensed <- lapply(imputed_datasets, function(imputed_df) {
    #fit_condensed(imputed_df[imputed_df$Sex == 0,])
    fit_stacked(imputed_df[imputed_df$Sex == 0,],formula_cond,fp_con_formula_female,model = model) #org_formula = formula_cond
  })
  
  q_imp_condensed_m <- list()
  ars_imp_condensed_m <- list()
  
  q_imp_condensed_f <- list()
  ars_imp_condensed_f <- list()
  
  for (i in 1:5) {
    q_imp_condensed_m[[i]] <- male_condensed[[i]][[1]]
    ars_imp_condensed_m[[i]] <- male_condensed[[i]][[2]]
    
    q_imp_condensed_f[[i]] <- female_condensed[[i]][[1]]
    ars_imp_condensed_f[[i]] <- female_condensed[[i]][[2]]
  }
  
  
  q_pool_condensed_m <- summary(pool(q_imp_condensed_m))
  ars_pool_condensed_m  <- summary(pool(ars_imp_condensed_m))
  
  q_pool_condensed_f <- summary(pool(q_imp_condensed_f))
  ars_pool_condensed_f  <- summary(pool(ars_imp_condensed_f))
  
  #store impuation model
  print(data_soruce)
  data_path <- "risk_scores/stacked/array_job"
  #print(paste0(data_path,store))
  if (!file.exists(paste0(data_path,store))) {
    print('create')
    dir.create(paste0(data_path,store), recursive = TRUE)  # 'recursive = TRUE' creates parent directories if needed
  }
  #save imp model and ddata
  saveRDS(imp_model, file = paste0(data_path,store,"/imp_model.rds"))
  write.csv(df,paste0(data_path,store,"/org_data.csv"))
  
  #store models and data for ptimsiism adjustmenat following bootstrap
  print(paste0('storing data in: risk_scores/stacked/array_job',store ,'/full_models/'))
  store_imputaion(imputed_datasets,q_pool_mod_m,ars_pool_mod_m,q_pool_mod_f,ars_pool_mod_f,m = 5,path = paste0('risk_scores/stacked/array_job',store ,'/full_models/'))
  
  store_imputaion(imputed_datasets,data = FALSE, suffix = '_cond',q_pool_condensed_m,ars_pool_condensed_m,q_pool_condensed_f,ars_pool_condensed_f,m = 5,path = paste0('risk_scores/stacked/array_job',store,'/condensed_models/'))
  print(data_soruce)
  #save.image(file=paste0(data_path,store,'/',data_soruce,'_',model,'_',model_type,'.RData'))
  save(list = ls(all.names = TRUE), file = paste0(data_path,store,'/',data_soruce,'_',model,'_',model_type,'.RData'))
  #save.image(file='risk_scores/ARS/gp_score_ars.RData')
  
  if(new_bs & (!file.exists(paste0("/gpfs3risk_scores/array_job",store,'/bs_samples.csv')))){
    
    if (!file.exists(paste0("/gpfs3risk_scores/array_job",store))) {
      print('create BS')
      dir.create(paste0("/gpfs3risk_scores/array_job",store), recursive = TRUE)  # 'recursive = TRUE' creates parent directories if needed
    }
    print(paste0("/gpfs3risk_scores/array_job",store))
    print('creating bs iterations')
    create_bs(df,paste0("/gpfs3risk_scores/array_job",store),iter = 1000)
  }
}






