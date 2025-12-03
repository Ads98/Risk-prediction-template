
select_cond <- function(cond_type){
  cond <- switch(cond_type,
                 "male" = function(x) x[, 'Sex'] == 1,
                 "female" = function(x) x[, 'Sex'] == 0,
                 
                 # Age-specific conditions
                 "under_55" = function(x) x[, 'Age'] < 55,
                 "55_65" = function(x) x[, 'Age'] >= 55 & x[, 'Age'] < 65,
                 "65_75" = function(x) x[, 'Age'] >= 65 & x[, 'Age'] < 75,
                 "over_75" = function(x) x[, 'Age'] >= 75,
                 
                 # Combined sex and age conditions
                 "male 55" = function(x) x[, 'Sex'] == 1 & x[, 'Age'] < 55,
                 "male 55-65" = function(x) x[, 'Sex'] == 1 & x[, 'Age'] >= 55 & x[, 'Age'] < 65,
                 "male 65-75" = function(x) x[, 'Sex'] == 1 & x[, 'Age'] >= 65 & x[, 'Age'] < 75,
                 "male 75" = function(x) x[, 'Sex'] == 1 & x[, 'Age'] >= 75,
                 
                 "female 55" = function(x) x[, 'Sex'] == 0 & x[, 'Age'] < 55,
                 "female 55-65" = function(x) x[, 'Sex'] == 0 & x[, 'Age'] >= 55 & x[, 'Age'] < 65,
                 "female 65-75" = function(x) x[, 'Sex'] == 0 & x[, 'Age'] >= 65 & x[, 'Age'] < 75,
                 "female 75" = function(x) x[, 'Sex'] == 0 & x[, 'Age'] >= 75,
                 
                 # Default case
                 function(x) rep(TRUE, nrow(x)) # No condition specified
                 
                 # default = {
                 #   cond <- function(x)rep(TRUE,nrow(x)) #NULL
                 #   print("No condition specfied")
  )
  return(cond)
}

#claulate shrinkage & optimism

calculate_and_assign_metrics <- function(full_res,suffix = '') {
  # Identify rows with missing data
  rows_with_na <- null_rows(full_res)
  
  # Calculate overall shrinkage
  print('assign met')
  assign(paste0("shrinkage",suffix), get_optimism(full_res, rows_with_na), envir = .GlobalEnv)
  print('Define the metrics to evaluate')
  # Define the metrics to evaluate
  metrics <- c("c_idx", "ici", "r2", "nri_10", "nri_7.5", "nri_5", "nri_cont", "nb_10", "nb_7.5", "nb_5")
  
  # Loop through each metric and calculate shrinkage and optimism
  for (met in metrics) {
    shrink_var <- paste0(met, "_shrink",suffix)
    optim_var <- paste0(met, "_optims",suffix)
    
    # Calculate shrinkage and optimism
    assign(shrink_var, performance_shrinkage(full_res, rows_with_na, met = met), envir = .GlobalEnv)
    assign(optim_var, optim_dist(full_res, rows_with_na, met = met), envir = .GlobalEnv)
  }
}

get_base_surv_mp <- function(df,time = 6,mod = null,lab = 'macce.incident',lp = NULL,M = 5){
  print('mod = ')
  print(mod)
  if(is.null(lp)){
    base_surv <- summary(survfit(mod),time=time)$surv
  }else{
    shrunk_lp <- reduce_mp(lp,M = M)
    base <- coxph(Surv(df$macce_obs_time,df[,lab])~offset(shrunk_lp))
    base_surv <- summary(survfit(base),time=time)$surv
  }
  return(base_surv)
}


#claulate the basline surival for qrisk 3 and ars models
compute_baseline_survival <- function(lp, q_models, ars_models, df, time = 6) {
  # Compute baseline survival for males
  sex_cond <- df$Sex == 1
  print('lp start len')
  print(length(lp))
  print(length(lp[[1]]))
  base_male_q <- lapply(q_models$m, function(mod) {
    get_base_surv_mp(df[sex_cond, ], time = time, mod = mod, lp = lp[[1]])
  })
  base_male_q <- mean(unlist(base_male_q))
  
  base_male_ars <- lapply(ars_models$m, function(mod) {
    get_base_surv_mp(df[sex_cond, ], time = time, mod = mod, lp = lp[[2]])
  })
  base_male_ars <- mean(unlist(base_male_ars))
  
  # Compute baseline survival for females
  base_female_q <- lapply(q_models$f, function(mod) {
    get_base_surv_mp(df[!sex_cond, ], time = time, mod = mod, lp = lp[[3]])
  })
  base_female_q <- mean(unlist(base_female_q))
  
  base_female_ars <- lapply(ars_models$f, function(mod) {
    get_base_surv_mp(df[!sex_cond, ], time = time, mod = mod, lp = lp[[4]])
  })
  base_female_ars <- mean(unlist(base_female_ars))
  
  # Return results in a list
  return(list(
    base_male_q = base_male_q,
    base_female_q = base_female_q,
    base_male_ars = base_male_ars,
    base_female_ars = base_female_ars
  ))
}


# Function to fit a Cox model and extract the hazard ratio and variance for a given covariate
extract_hr <- function(imputed_data, covariate,hc_covs) {
  # Fit the Cox model
  formula <- as.formula(paste0("Surv(macce_obs_time, macce.incident) ~ ", covariate))
  print(formula)
  
  # Check if the covariate is binary
  is_binary <- length(unique(covariate[!is.na(covariate)])) == 2
  print('covariate')
  print(covariate)
  print('hc covs')
  print(hc_covs)
  # Standardize the covariate if it is not binary
  if (!is_binary & !(covariate %in% hc_covs)) {
    print('sd')
    imputed_data[[covariate]] <- (imputed_data[[covariate]] - mean(imputed_data[[covariate]], na.rm = TRUE)) / sd(imputed_data[[covariate]], na.rm = TRUE)
    print(paste0(covariate, ' standardized'))
  }
  
  
  model <- coxph(formula, data = imputed_data)
  print(model)
  
  # Extract hazard ratio (exp(coef)) and its variance
  hr <- exp(coef(model)[1])
  var <- (summary(model)$coefficients[1, "se(coef)"])^2  # Variance of the coefficient
  
  print(c(HR = hr, VAR = var))
  return(c(HR = hr, VAR = var))
}


#get apapreant and ajdusted lp's
get_adjusted_lp <- function(
    env_path,
    store,
    model,
    model_type,
    data_source,
    acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
    condensed_model = FALSE ){
  
  model_suffix <- c('_m','_f')
  
  load(paste0(env_path, store, '/', data_source, '_', model, '_', model_type, '.RData'))
  
  fp_terms <- acc_terms
  full_models <- load_modles(path = paste0('risk_scores/stacked/array_job/', store, '/full_models/'),suffix = model_suffix)
  if (condensed_model) {
    cond_models <- load_modles(path = paste0('risk_scores/stacked/array_job/', store, '/condensed_models/'), suffix = paste0(model_suffix,'_cond'))
  }
  
  print(paste0('mods path: ','risk_scores/ARS/bs_imp/con/6y/stacked/full/', store, '/'))
  full_res <- load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/', store, '/'))
  if (condensed_model) {
    cond_res <- load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/', store, '/'))
  }
  
  #load orignal data
  imputed_datasets <- list()
  for (i in 1:5) {
    imputed_datasets[[i]] <- read.csv(paste0("risk_scores/stacked/array_job/",store,"/full_models/","org_imp_",i,".csv"))#risk_scores/array_job/full_models/","org_imp_",i,".csv". con/
  }
  
  
  # Claulate shrinkage and optims
  print('get metrics')
  calculate_and_assign_metrics(full_res)
  
  print('cond res')
  if(condensed_model){
    calculate_and_assign_metrics(cond_res,suffix = '_cond')
  }
  print('got metrcis')
  print(paste0('model = ',model))
  print(paste0('store: ',store))

  print('getting lp')
  lp_full <- adjust_lp(imputed_datasets,full_models,c(1,1,1,1),model = model,fp_terms = fp_terms )#shrinkage
  lp_adj_full <- adjust_lp(imputed_datasets,full_models,shrinkage,model = model,fp_terms = fp_terms)#shrinkage
  
  print('got lps')
  if(condensed_model){
    lp_con <- adjust_lp(imputed_datasets,cond_models,c(1,1,1,1),model =model,fp_terms = fp_terms)#shrinkage_con
    lp_adj_con <- adjust_lp(imputed_datasets,cond_models,shrinkage_cond,model =model,fp_terms = fp_terms)#shrinkage_con
    return(list(lp_full,lp_adj_full,lp_con,lp_adj_con))
  }
  return(list(lp_full,lp_adj_full))
  
}


#get apapreant and adjusted predictors + risk scores
get_predictions <- function(
    env_path,
    data_path,
    store,
    model,
    split,
    model_type,
    store_suffix,
    data_source,
    metric,
    acc_terms = c("StepsDayMedAdjusted", "ModerVigorActivity", "LightActivity", "SedentaryActivity", "AccOverallAvg"),
    condensed_model = FALSE){
  
  store <- paste0(store, split, '/', model, '_', model_type, '_', store_suffix)
  
  model_suffix <- c('_m','_f')
  
  load(paste0(env_path, store, '/', data_source, '_', model, '_', model_type, '.RData'))
  fp_terms <- acc_terms
  
  full_res <- load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/', store, '/'))
  if (condensed_model) {
    cond_res <- load_results(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/condensed/', store, '/'))
  }

  
  #load orignal data
  imputed_datasets <- list()
  for (i in 1:5) {
    imputed_datasets[[i]] <- read.csv(paste0("risk_scores/stacked/array_job/",store,"/full_models/","org_imp_",i,".csv"))#risk_scores/array_job/full_models/","org_imp_",i,".csv". con/
  }
  
  # Claulate shrinkage and optims
  print('full res')
  calculate_and_assign_metrics(full_res)
  
  if(condensed_model){
    calculate_and_assign_metrics(cond_res,suffix = '_cond')
  }
  
  #lienar predictor for orignal and adjusted models
  print('get ls')
  lienar_predictions <- get_adjusted_lp(env_path,store,model, model_type,data_source,acc_terms = fp_terms,condensed_model = condensed_model )
  
  print('got lps')
  lp_full <- lienar_predictions[[1]]
  lp_adj_full <- lienar_predictions[[2]]
  #lp_full <- adjust_lp(imputed_datasets,full_models,c(1,1,1,1),model = model,fp_terms = fp_terms )#shrinkage
  #lp_adj_full <- adjust_lp(imputed_datasets,full_models,shrinkage,model = model,fp_terms = fp_terms)#shrinkage
  
  if(condensed_model){
    lp_con <- lienar_predictions[[3]]
    lp_adj_con <- lienar_predictions[[4]]
    #lp_con <- adjust_lp(imputed_datasets,cond_models,c(1,1,1,1),model =model,fp_terms = fp_terms)#shrinkage_con
    #lp_adj_con <- adjust_lp(imputed_datasets,cond_models,shrinkage_con,model =model,fp_terms = fp_terms)#shrinkage_con
  }
  
  print('male pooled S0')
  #df<- imputed_datasets[[1]]
  sex_cond <- df$Sex == 1
  
  # Define model lists
  q_models <- list(m = q_imp_mod_m, f = q_imp_mod_f)
  ars_models <- list(m = ars_imp_mod_m, f = ars_imp_mod_f)
  
  if(condensed_model){
    q_models_cond <- list(m = q_imp_condensed_m, f = q_imp_condensed_f)
    ars_models_cond <- list(m = ars_imp_condensed_m, f = ars_imp_condensed_f)
  }
  
  # Compute baseline survival

  print('baselin surv non adj')
  baseline_surv_full <- compute_baseline_survival(NULL, q_models, ars_models, df, time = 6)
  print('baselin surv adj')
  baseline_surv_adj_full <- compute_baseline_survival(lp_adj_full, q_models, ars_models, df, time = 6)
  print('Got baseline')
  
  print('getting rs')
  rs_full <- adjust_rs(imputed_datasets,full_models,lp_full,base_surv = baseline_surv_full)
  rs_adj_full <- adjust_rs(imputed_datasets,full_models,lp_adj_full,base_surv = baseline_surv_adj_full,cent = TRUE)

  
  if(condensed_model){
    baseline_surv_con <- compute_baseline_survival(NULL, q_models_cond, ars_models_cond, df, time = 6)
    baseline_surv_adj_con <- compute_baseline_survival(lp_adj_con, q_models_cond, ars_models_cond, df, time = 6)
   
    rs_adj_con <- adjust_rs(imputed_datasets,cond_models,lp_adj_con,base_surv = baseline_surv_adj_con,cent = TRUE)
    rs_con <- adjust_rs(imputed_datasets,cond_models,lp_con,base_surv = baseline_surv_con)
    
  }
 
  
  df <-data.frame(imputed_datasets[[1]])
  sex_cond<- df$Sex ==1
  
  #orignal lps
  df['qrisk3'] <- 0
  df['ars'] <- 0
  df['qrisk3_con'] <- 0
  df['ars_con'] <- 0
  
  df[sex_cond,'qrisk3'] <- rs_full[[1]]
  df[sex_cond,'ars'] <- rs_full[[2]]
  
  df[!sex_cond,'qrisk3'] <- rs_full[[3]]
  df[!sex_cond,'ars'] <- rs_full[[4]]
  
  
  #####adjusted lps 
  df['qrisk3_adj'] <- 0
  df['ars_adj'] <- 0
  df['qrisk3_con_adj'] <- 0
  df['ars_con_adj'] <- 0
  
  df[sex_cond,'qrisk3_adj'] <- rs_adj_full[[1]]
  df[sex_cond,'ars_adj'] <- rs_adj_full[[2]]
  
  df[!sex_cond,'qrisk3_adj'] <- rs_adj_full[[3]]
  df[!sex_cond,'ars_adj'] <- rs_adj_full[[4]]
  
  if(condensed_model){
    df[sex_cond,'qrisk3_con'] <- rs_con[[1]]
    df[sex_cond,'ars_con'] <- rs_con[[2]]
    
    df[!sex_cond,'qrisk3_con'] <- rs_con[[3]]
    df[!sex_cond,'ars_con'] <- rs_con[[4]]
    
    df[sex_cond,'qrisk3_con_adj'] <- rs_adj_con[[1]]
    df[sex_cond,'ars_con_adj'] <- rs_adj_con[[2]]
    
    df[!sex_cond,'qrisk3_con_adj'] <- rs_adj_con[[3]]
    df[!sex_cond,'ars_con_adj'] <- rs_adj_con[[4]]
  }
  return(df)
}

get_imp_scores <- function(imputed_dataset, lp_adj_full, baseline_surv, score_type, lab, time, th, dataset_index) {
  # Set sex condition
  sex_cond <- imputed_dataset$Sex == 1
  
  # Determine index based on score type
  if (grepl("qrisk3", score_type)) {
    male_lp_index <- 1
    female_lp_index <- 3
    male_baseline_index <- 1
    female_baseline_index <- 2
  } else if (grepl("ars", score_type)) {
    male_lp_index <- 2
    female_lp_index <- 4
    male_baseline_index <- 3
    female_baseline_index <- 4
  } else {
    stop("Invalid score type. Must contain 'qrisk3' or 'ars'.")
  }
  
  print(paste0('score_type  ',score_type))
  print(paste0('dataset_index  ',dataset_index))
  print(paste0('male_lp_index  ',male_lp_index))
  print(paste0('sex_cond  ',mean(sex_cond)))
  print(paste0('score_type  ',score_type))
  print(paste0('baseline_surv  ',baseline_surv[[male_baseline_index]]))
  print(paste0(' lp_adj_full[[male_lp_index]][[dataset_index]]  ',mean( lp_adj_full[[male_lp_index]][[dataset_index]])))
  
  imputed_dataset[[score_type]] <- 0
  # Male scores
  imputed_dataset[sex_cond, ][[score_type]] <- get_scores(
    df = imputed_dataset[sex_cond, ],
    lp = lp_adj_full[[male_lp_index]][[dataset_index]],
    lab = lab,
    time = time,
    th = th,
    baseline_surv = baseline_surv[[male_baseline_index]]
  )
  print('male rs')
  #print(imputed_dataset[[score_type]])
  # Female scores
  imputed_dataset[!sex_cond, ][[score_type]] <- get_scores(
    df = imputed_dataset[!sex_cond, ],
    lp = lp_adj_full[[female_lp_index]][[dataset_index]],
    lab = lab,
    time = time,
    th = th,
    baseline_surv = baseline_surv[[female_baseline_index]]
  )
  print('return vals')
  #print(imputed_dataset[[score_type]])
  return(imputed_dataset)
}
# Define the function
get_rs_hz <- function(imputed_datasets, scores, lp_adj_full, baseline_surv, lab = 'macce.incident', time = 6, th = 0.5,hc_covs = c('')) {
  # Iterate through imputed datasets and calculate scores for each score type
  print('geeting scores')
  for (i in seq_along(imputed_datasets)) {
    for (score_type in scores) {
      imputed_datasets[[i]] <- get_imp_scores(
        imputed_dataset = imputed_datasets[[i]],
        lp_adj_full = lp_adj_full,
        baseline_surv = baseline_surv,
        score_type = score_type,
        lab = lab,
        time = time,
        th = th,
        dataset_index = i  # Pass dataset index explicitly
      )
    }
    print('rs vals')
    #print(imputed_datasets[[i]][[score_type]])
  }
  print('got scores')
  
  # List to store combined results
  combined_results <- list()
  
  # Calculate Rubin's rules for each score
  for (score_type in scores) {
    for (sex in c("male", "female")) {
      # Set sex condition
      sex_cond <- ifelse(sex == "male", TRUE, FALSE)
      
      hr_list <- c()
      var_list <- c()
      
      # Extract HR and VAR for each imputed dataset
      for (imp_data in imputed_datasets) {
        hr_var <- extract_hr(imp_data[imp_data$Sex == sex_cond, ], score_type,hc_covs)
        hr_list <- c(hr_list, hr_var[paste0("HR.", score_type)])
        var_list <- c(var_list, hr_var["VAR"])
      }
      
      # Rubin's rules calculations
      mean_hr <- mean(hr_list)
      within_var <- mean(var_list)
      between_var <- var(hr_list)
      total_var <- within_var + (1 + 1 / length(hr_list)) * between_var
      ci_lower <- exp(log(mean_hr) - 1.96 * sqrt(total_var))
      ci_upper <- exp(log(mean_hr) + 1.96 * sqrt(total_var))
      
      # Store results
      combined_results[[paste0(score_type, "_", sex)]] <- c(HR = mean_hr, CI_lower = ci_lower, CI_upper = ci_upper)
    }
  }
  
  # Print results
  for (x in names(combined_results)) {
    hz <- round(combined_results[[x]], 3)
    print(paste0(x, ": ", hz[1], " (", hz[2], " - ", hz[3], ")"))
  }
  
  return(combined_results)
}


drop_old_col <- function(df){
  # Drop the specified columns
  return(df[, !colnames(df) %in% c("c_under_65", "c_over_65")])
}

load_results <- function(path = 'risk_scores/ARS/bs_imp/con/6y/full/')  {
  empty_df <- data.frame(
    cbl = numeric(0),
    cidx = numeric(0),
    ici = numeric(0),
    r2 = numeric(0),
    nb_10 = numeric(0),
    nb_7.5 = numeric(0),
    nb_5 = numeric(0)
  )
  
  # Create a list containing 5 copies of the empty data frame
  
  male_q <- replicate(5, empty_df, simplify = FALSE)
  female_q <- replicate(5, empty_df, simplify = FALSE)
  
  male_ars <- replicate(5, empty_df, simplify = FALSE)
  female_ars <- replicate(5, empty_df, simplify = FALSE)
  
  male_q_bs <- replicate(5, empty_df, simplify = FALSE)
  female_q_bs <- replicate(5, empty_df, simplify = FALSE)
  
  male_ars_bs <- replicate(5, empty_df, simplify = FALSE)
  female_ars_bs <-replicate(5, empty_df, simplify = FALSE)
  print('path')
  print(paste0(path,"org/male_ars/imp_",i ,"/male_ars.csv"))
  for (i in 1:5) {
    male_q[[i]] <-  read.csv(paste0(path,"org/male_q/imp_",i,"/male_q.csv"),sep =' ') #org/male_q/
    male_q[[i]] <- drop_old_col(male_q[[i]])
    
    male_ars[[i]] <-read.csv(paste0(path,"org/male_ars/imp_",i ,"/male_ars.csv"),sep =' ') #org/male_ars/
    male_ars[[i]] <- drop_old_col(male_ars[[i]])
    
    female_q[[i]] <- read.csv(paste0(path,"org/female_q/imp_",i,"/female_q.csv"),sep =' ')#org/female_q/
    female_q[[i]] <- drop_old_col(female_q[[i]])
    
    female_ars[[i]] <- read.csv(paste0(path,"org/female_ars/imp_",i,"/female_ars.csv"),sep =' ') # org/female_ars/
    female_ars[[i]] <- drop_old_col(female_ars[[i]])
    
    male_q_bs[[i]] <- read.csv(paste0(path,"bs/male_q/imp_",i,"/male_q.csv"),sep =' ') #bs/male_q/
    male_q_bs[[i]] <- drop_old_col(male_q_bs[[i]])
    
    male_ars_bs[[i]] <-  read.csv(paste0(path,"bs/male_ars/imp_",i,"/male_ars.csv"),sep =' ')#bs/male_ars/
    male_ars_bs[[i]] <- drop_old_col(male_ars_bs[[i]])
    
    female_q_bs[[i]] <- read.csv(paste0(path,"bs/female_q/imp_",i,"/female_q.csv"),sep =' ')#bs/female_q/
    female_q_bs[[i]] <- drop_old_col(female_q_bs[[i]])
    
    female_ars_bs[[i]] <- read.csv(paste0(path,"bs/female_ars/imp_",i,"/female_ars.csv"),sep =' ')#bs/female_ars/
    female_ars_bs[[i]] <- drop_old_col(female_ars_bs[[i]])
  }
  
  print(male_ars[[5]][1:5,'c_idx'])
  org_models <- list(male_q,male_ars,female_q,female_ars)
  bs_models <- list(male_q_bs,male_ars_bs,female_q_bs,female_ars_bs)
  
  return(list(org_models,bs_models))
}


get_apaprent_c <- function(df,lp_full,sex_cond,lab='macce_eval_lab',time='macce_eval_time'){
  avg_lp <- Reduce(`+`, lp_full) / length(lp_full)
  
  # Calculate adjusted C-index
  c_app <- round(get_idx(df[sex_cond, ], avg_lp,lab = lab,time = time),3)
  return(c_app)
}

get_apaprent_ici <- function(df,sex_cond,score_col,lab = 'macce.incident'){
  ici_app <- get_cal(df[sex_cond,], df[sex_cond, score_col], lab = lab)
  return(ici_app)
}

get_apaprent_r2 <- function(lp_full,sex_cond){
  avg_lp <- Reduce(`+`, lp_full) / length(lp_full)
  
  # Calculate adjusted C-index
  c_app <- round(get_idx(df[sex_cond, ], avg_lp),3)
  return(c_app)
}


get_apaprent_nb <- function(df,sex_cond,rs,lab = 'macce.incident',th = 0.1){
  nb_app <-calc_nb(df[sex_cond,],df[sex_cond,rs],lab = lab,th = th) 
  
  return(nb_app)
}

get_apaprent_nri <- function(df,sex_cond,rs1,rs2,th = 0.1){
  nri_app <- calculate_nri(df[sex_cond,],rs1,rs2,event_col,th = th)[1]
  
  # Calculate adjusted C-index
  return(nri_app)
}

get_apaprent_nri_cont <- function(df,sex_cond,rs1,rs2,th = 0.1){
  nri_app <- calculate_nri(df[sex_cond,],rs1,rs2,event_col,th = th)[2]
  
  # Calculate adjusted C-index
  return(nri_app)
}

calculate_adjusted_c_index <- function(df, lp_full, shrinkage, optims, sex_cond) {
  # Calculate the average linear predictor across imputed datasets
  avg_lp <- Reduce(`+`, lp_full) / length(lp_full)
  
  # Calculate adjusted C-index
  c_adj <- round(get_idx(df[sex_cond, ], avg_lp) - shrinkage, 3)
  
  # Calculate confidence intervals
  c_adj_ci <- round(pool_ci(get_idx(df[sex_cond, ], avg_lp) - optims), 3)
  
  # Return the adjusted C-index and confidence intervals
  result <- paste0(c_adj, " (", c_adj_ci[1], " - ", c_adj_ci[2], ")")
  #return(result)
  return(list(c_adj,c_adj_ci[1],c_adj_ci[2]))
}

calculate_ici <- function(df, score_col, shrinkage, optims, lab, sex_cond) {
  # Calculate apparent ICI
  print(score_col)
  print(lab)
  print(sum(sex_cond))
  ici_app <- get_cal(df[sex_cond,], df[sex_cond, score_col], lab = lab)
  
  # Adjust ICI
  
  ici_adj <- round(ici_app - shrinkage, 4)
  ici_dist <- ici_app - optims
  # Calculate confidence intervals
  ici_adj_ci <- round(pool_ci(ici_dist), 4)
  
  print(paste0('ici app : ',ici_app))
  print(paste0('shrinakge: ',shrinkage))
  print(paste0('optim: ',mean(optims)))
  print(paste0('max optim: ',max(optims)))
  print(paste0('min ici ',min(ici_app)))
  print(paste0('min ici adj  ',min(ici_dist)))
  min_dx <- which(ici_dist == min(ici_dist))
  print(min_dx)
  print(optims[min_dx[1]])
  
  # Return the adjusted ICI and confidence intervals
  result <- paste0(ici_adj, " (", ici_adj_ci[1], " - ", ici_adj_ci[2], ")")
  #return(result)
  return(list(ici_adj,ici_adj_ci[1],ici_adj_ci[2]))
}

calculate_adjusted_r2 <- function(lp_full, shrinkage, optims, sex_cond, df) {
  # Calculate the average linear predictor across imputed datasets
  avg_lp <- Reduce(`+`, lp_full) / length(lp_full)
  
  # Calculate apparent R²
  r2_app <- get_r2(avg_lp)
  
  # Adjusted R²
  r2_adj <- round(r2_app - shrinkage, 4)
  
  # Confidence intervals for adjusted R²
  r2_adj_ci <- round(pool_ci(r2_app - optims), 4)
  
  # Return adjusted R² with confidence intervals
  result <- paste0(r2_adj, " (", r2_adj_ci[1], " - ", r2_adj_ci[2], ")")
  return(list(r2_adj,r2_adj_ci[1],r2_adj_ci[2]))
}


adj_nri <- function(df,rs1,rs2,cond,shrink,nri_optims,lab = 'macce.incident',th = 0.1){
  print('th')
  print(th)
  nri_app <- calculate_nri(df[cond,],rs1,rs2,event_col,th = th)[1]
  nri_adj <- round(nri_app - shrink,3)
  nri_adj_ci <- round(pool_ci(nri_app - nri_optims),3)
  print(paste0(nri_adj," (",nri_adj_ci[1]," - ",nri_adj_ci[2],")"))
  return(list(nri_adj,nri_adj_ci[1],nri_adj_ci[2]))
}
#boot strap 95% CI using Rubiuns rule

get_nris <- function(df,rs1,rs2,cond,shrink,nri_optims,shrink_idx,th = c(0.1,0.075,0.005)){
  nri_10 <- adj_nri(df,rs1,rs2,cond,shrink[[1]][[shrink_idx]],nri_optims[[1]][[shrink_idx]],lab = 'macce.incident',th = 0.1)
  nri_7_5 <- adj_nri(df,rs1,rs2,cond,shrink[[2]][[shrink_idx]],nri_optims[[2]][[shrink_idx]],lab = 'macce.incident',th = 0.075)
  nri_5 <- adj_nri(df,rs1,rs2,cond,shrink[[3]][[shrink_idx]],nri_optims[[3]][[shrink_idx]],lab = 'macce.incident',th = 0.05)
  
  return(list(
    list(mean = nri_10[[1]], ci_low = nri_10[[2]], ci_high = nri_10[[3]]),
    list(mean = nri_7_5[[1]], ci_low = nri_7_5[[2]], ci_high = nri_7_5[[3]]),
    list(mean = nri_5[[1]], ci_low = nri_5[[2]], ci_high = nri_5[[3]])
  ))
  #return(c(nb_5,nb_7_5,nb_10))
  
}
adj_nb <- function(df,rs,cond,shrink,nb_optims,lab = 'macce.incident',th = 0.1){
  
  nb_app <-calc_nb(df[cond,],df[cond,rs],lab = lab,th = th) 
  nb_adj <- round(nb_app - shrink,3)
  nb_adj_ci <- round(pool_ci(nb_app - nb_optims),3)
  print(paste0(nb_adj," (",nb_adj_ci[1]," - ",nb_adj_ci[2],")"))
  return(list(nb_adj,nb_adj_ci[1],nb_adj_ci[2]))
}

get_nbs <- function(df, rs, cond, shrink, nb_optims, shrink_idx) {
  nb_10 <- adj_nb(df, rs, cond, shrink[[1]][[shrink_idx]], nb_optims[[1]][[shrink_idx]], lab = 'macce.incident', th = 0.1)
  nb_7_5 <- adj_nb(df, rs, cond, shrink[[2]][[shrink_idx]], nb_optims[[2]][[shrink_idx]], lab = 'macce.incident', th = 0.075)
  nb_5 <- adj_nb(df, rs, cond, shrink[[3]][[shrink_idx]], nb_optims[[3]][[shrink_idx]], lab = 'macce.incident', th = 0.05)
  
  # Return a list where each entry is a named list with mean, CI low, and CI high
  return(list(
    list(mean = nb_10[[1]], ci_low = nb_10[[2]], ci_high = nb_10[[3]]),
    list(mean = nb_7_5[[1]], ci_low = nb_7_5[[2]], ci_high = nb_7_5[[3]]),
    list(mean = nb_5[[1]], ci_low = nb_5[[2]], ci_high = nb_5[[3]])
  ))
}
# get_nbs <- function(df,rs,cond,shrink,nb_optims,shrink_idx){
#   nb_10 <- adj_nb(df,rs,cond,shrink[[1]][[shrink_idx]],nb_optims[[1]][[shrink_idx]],lab = 'macce.incident',th = 0.1)
#   nb_7_5 <- adj_nb(df,rs,cond,shrink[[2]][[shrink_idx]],nb_optims[[2]][[shrink_idx]],lab = 'macce.incident',th = 0.075)
#   nb_5 <- adj_nb(df,rs,cond,shrink[[3]][[shrink_idx]],nb_optims[[3]][[shrink_idx]],lab = 'macce.incident',th = 0.05)
#   
#   
#   return(c(nb_5,nb_7_5,nb_10))
#   
# }












# Example usage:

# # Adjust linear predictors
# lp_full <- adjust_lp(imputed_datasets, full_models, c(1, 1, 1, 1), model = model, fp_terms = fp_terms)
# lp_adj_full <- adjust_lp(imputed_datasets, full_models, shrinkage, model = model, fp_terms = fp_terms)
# lp_con <- adjust_lp(imputed_datasets, cond_models, c(1, 1, 1, 1), model = model, fp_terms = fp_terms)
# lp_adj_con <- adjust_lp(imputed_datasets, cond_models, shrinkage_con, model = model, fp_terms = fp_terms)
# 
# # Define model lists
# q_models <- list(m = q_imp_mod_m, f = q_imp_mod_f)
# ars_models <- list(m = ars_imp_mod_m, f = ars_imp_mod_f)
# 
# # Compute baseline survival
# baseline_surv_full <- compute_baseline_survival(lp_full, q_models, ars_models, df, time = 6)
# baseline_surv_adj_full <- compute_baseline_survival(lp_adj_full, q_models, ars_models, df, time = 6)
# baseline_surv_con <- compute_baseline_survival(lp_con, q_models, ars_models, df, time = 6)
# baseline_surv_adj_con <- compute_baseline_survival(lp_adj_con, q_models, ars_models, df, time = 6)

# Print results
# print("Baseline survival for lp_full:")
# print(baseline_surv_full)
# 
# print("Baseline survival for lp_adj_full:")
# print(baseline_surv_adj_full)
# 
# print("Baseline survival for lp_con:")
# print(baseline_surv_con)
# 
# print("Baseline survival for lp_adj_con:")
# print(baseline_surv_adj_con)



# base_male_q <- lapply(q_imp_mod_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod,lp = lp_adj_full[[1]])})#lp = lp_adj_full[[1]]
# base_male_q <-mean(unlist(base_male_q))
# base_male_ars <- lapply(ars_imp_mod_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod,lp = lp_adj_full[[2]])})#lp = lp_adj_full[[2]]
# base_male_ars <-mean(unlist(base_male_ars))
# 
# print('female pooled S0')
# base_female_q <- lapply(q_imp_mod_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod, lp = lp_adj_full[[3]])})#lp = lp_adj_con[[3]]
# base_female_q <-mean(unlist(base_female_q))
# base_female_ars <- lapply(ars_imp_mod_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod, lp = lp_adj_full[[4]])})#lp = lp_adj_full[[4]]
# base_female_ars <-mean(unlist(base_female_ars))
# 
# #get baslien survival for condensed models
# print('male pooled S0')
# #base_male_q_con <- lapply(seq(5),function(i) {get_base_surv_mp(imputed_datasets[[i]][sex_cond,],time = 6,mod =q_imp_condensed_m[[i]],lp = lp_adj_con[[1]])})
# base_male_q_con <- lapply(q_imp_condensed_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod,lp = lp_adj_con[[1]])})#lp = lp_adj_con[[1]]
# base_male_q_con <-mean(unlist(base_male_q_con))
# base_male_ars_con <- lapply(ars_imp_condensed_m,function(mod) {get_base_surv_mp(df[sex_cond,],time = 6,mod =mod,lp = lp_adj_con[[2]])})#lp = lp_adj_con[[2]]
# base_male_ars_con <-mean(unlist(base_male_ars_con))
# 
# base_female_q_con <- lapply(q_imp_condensed_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod,lp = lp_adj_con[[3]])})#lp = lp_adj_con[[3]]
# base_female_q_con <- mean(unlist(base_female_q_con))
# base_female_ars_con <- lapply(ars_imp_condensed_f,function(mod) {get_base_surv_mp(df[!sex_cond,],time = 6,mod =mod,lp = lp_adj_con[[4]])})#lp = lp_adj_con[[4]]
# base_female_ars_con <- mean(unlist(base_female_ars_con))
# 
# baseline_surv <- list(base_male_q,base_female_q,base_male_ars,base_female_ars)
#baseline_surv_con <- list(base_male_q_con,base_female_q_con,base_male_ars_con,base_female_ars_con)