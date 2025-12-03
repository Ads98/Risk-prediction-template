source(file.path("risk_scores/",'ARS_eval_utils_try_catch.R'), echo = T)
source(file.path("risk_scores/",'reclass.R'), echo = T)
library(tidyr)
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
  for (i in 1:5) {
    male_q[[i]] <-  read.csv(paste0(path,"org/male_q/imp_",i,"/male_q.csv"),sep =' ') #org/male_q/
    
    male_ars[[i]] <-read.csv(paste0(path,"org/male_ars/imp_",i ,"/male_ars.csv"),sep =' ') #org/male_ars/
    
    female_q[[i]] <- read.csv(paste0(path,"org/female_q/imp_",i,"/female_q.csv"),sep =' ')#org/female_q/
    
    female_ars[[i]] <- read.csv(paste0(path,"org/female_ars/imp_",i,"/female_ars.csv"),sep =' ') # org/female_ars/
    
    
    male_q_bs[[i]] <- read.csv(paste0(path,"bs/male_q/imp_",i,"/male_q.csv"),sep =' ') #bs/male_q/
    
    male_ars_bs[[i]] <-  read.csv(paste0(path,"bs/male_ars/imp_",i,"/male_ars.csv"),sep =' ')#bs/male_ars/
    
    female_q_bs[[i]] <- read.csv(paste0(path,"bs/female_q/imp_",i,"/female_q.csv"),sep =' ')#bs/female_q/
    
    female_ars_bs[[i]] <- read.csv(paste0(path,"bs/female_ars/imp_",i,"/female_ars.csv"),sep =' ')#bs/female_ars/
    
  }
  
  org_models <- list(male_q,male_ars,female_q,female_ars)
  bs_models <- list(male_q_bs,male_ars_bs,female_q_bs,female_ars_bs)
  
  return(list(org_models,bs_models))
}

null_rows <-function(mods){
  #get invalid rows
  # Find the row numbers with NA values in any column
  org_mods <- mods[[1]]
  bs_mods <- mods[[2]]
  rows_with_na <- c()
  for(m in 1:length(org_mods)){
    for (i in 1:5) {
      rows_with_na <- c(rows_with_na,which(apply( org_mods[[m]][[i]], 1, function(row) any(is.na(row)))))
      
      rows_with_na <- c(rows_with_na,which(apply( bs_mods[[m]][[i]], 1, function(row) any(is.na(row)))))
      
      #remove uncalibrated scores
      rows_with_na <- c(rows_with_na,which(apply( org_mods[[m]][[i]], 1, function(row) any((row['ici'] >10)))))
      
    }
    print(paste0('m = ',m))
    print(unique(rows_with_na))
  }
  return(unique(rows_with_na))
}

get_pred <- function(df,mod,ars = FALSE,model = 'qrisk'){
  
  #test_form <- get_formula(rownames(summary(mod)$coef),summary(mod)$estimate)
  test_form <- get_formula(mod$term,mod$estimate)
  print(paste0("test form : ",test_form))
  
  matrix_data <- model.matrix(as.formula(test_form), data = df)
  print("dim matrix data")
  print(dim(matrix_data))
  matrix_data <- matrix_data[, -1]
  print(dim(matrix_data))
  # Initialize an empty matrix for mean-centered data
  matrix_cent <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
  matrix_weight <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
  print("rename")
  for(i in 1:ncol(matrix_data)){
    print(i)
    
    colnames(matrix_data)[i] <- as.character(mod$term[i])
  }
  
  print("assign cols")
  colnames(matrix_cent) <- colnames(matrix_data)
  colnames(matrix_weight) <- colnames(matrix_data)
  
  # Mean center each column
  ars_term = ''
  age_1_ars =''
  age_2_ars = ''
  
  fp_term <- regmatches(test_form, gregexpr("~\\s*(\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\)\\s*)", test_form))[[1]]   
  fp_term <- sub("^\\s*~\\s*", "", fp_term) # remove'~ charecter
  
  
  print(fp_term)
  #if not 2 degree polynomal test for 1 degree polynomial
  if (identical(fp_term, character(0))) {
    print("here")
    fp_term <-regmatches(test_form, gregexpr("I\\([^ActivityRiskScore]+ActivityRiskScore \\+ [^+]+", test_form))[[1]]
    
  }
  else{
    print("split")
    fp_term<- strsplit(fp_term, " \\+ (?=I\\()", perl = TRUE)[[1]]
    
    #fp_term <- strsplit(fp_term, " \\+ I", perl = TRUE)[[1]]
  }
  fp_term <- fp_term[!grepl("\\bage_\\w+\\b", fp_term)]
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded spaces
  print(paste0("fp term = ",fp_term))
  age_1_ars = ""
  age_2_ars = ""
  cont_vars <- get_cont_interactions(fp_term,model =model)
  
  
  for(term in fp_term){
    
    if(model == 'qrisk'){
      print("qr is true")
      cont_vars <- c(cont_vars,paste0(term,":age_1"))
      cont_vars <- c(cont_vars,paste0(term,":age_2"))
    }
    else if(model == 'score2'){
      cont_vars <- c(cont_vars,paste0(term,":Age"))
    }
  }
  
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded space
  
  print("cont vars")
  print(cont_vars)
  for (i in colnames(matrix_data)) {
    if(i %in% cont_vars){
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i] - mean(matrix_data[, colnames(matrix_data) == i] )
    }
    else{
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i]
    }
    
  }
  
  # Weight each column
  #print("weigth matrix")
  #print(matrix_weight)
  #print(matrix_cent)
  
  for (i in colnames(matrix_weight)) {
    #print(i)
    #print(matrix_cent[1:5, colnames(matrix_cent) == i])
    #print(mod[mod$term == i,]$estimate)
    #print(mod)
    #print(test_form)
    #print(colnames(matrix_data))
    #print(as.formula(test_form))
    matrix_weight[, colnames(matrix_weight) == i] =  matrix_cent[, colnames(matrix_cent) == i] * mod[mod$term == i,]$estimate  #coef(mod)[i]
    #matrix_weight[, i] <- matrix_cent[, i] * coef(test_ars)[i]
  }
  
  print("dimw eight")
  print(dim(matrix_weight))
  pred <- rowSums(matrix_weight)
  
  return(pred)
  
  
}

correct_optimism <- function(mods,rows_with_na,met = 'c_idx'){
  #get avergae  across each bs sample 
  # Calculate the mean across the rows of 'row_means' cbl
  org_mods <- mods[[1]]
  bs_mods <- mods[[2]]
  male_q_cbl <- sapply(org_mods[[1]], function(df) df[-unique(rows_with_na),met])
  male_q_cbl <-rowMeans(male_q_cbl)
  
  male_ars_cbl <- sapply(org_mods[[2]], function(df) df[-unique(rows_with_na),met])
  male_ars_cbl <-rowMeans(male_ars_cbl)
  
  female_q_cbl <- sapply(org_mods[[3]], function(df) df[-unique(rows_with_na),met])
  female_q_cbl <-rowMeans(female_q_cbl)
  
  female_ars_cbl <- sapply(org_mods[[4]], function(df) df[-unique(rows_with_na),met])
  female_ars_cbl <-rowMeans(female_ars_cbl)
  
  male_q_bs_cbl <- sapply(bs_mods[[1]], function(df) df[-unique(rows_with_na),met])
  male_q_bs_cbl <-rowMeans(male_q_bs_cbl)
  
  male_ars_bs_cbl <- sapply(bs_mods[[2]], function(df) df[-unique(rows_with_na),met])
  male_ars_bs_cbl <-rowMeans(male_ars_bs_cbl)
  
  female_q_bs_cbl <- sapply(bs_mods[[3]], function(df) df[-unique(rows_with_na),met])
  female_q_bs_cbl <-rowMeans(female_q_bs_cbl)
  
  female_ars_bs_cbl <- sapply(bs_mods[[4]], function(df) df[-unique(rows_with_na),met])
  female_ars_bs_cbl <-rowMeans(female_ars_bs_cbl)
  
  male_q_us = 1 - mean(male_q_bs_cbl - male_q_cbl)
  male_ars_us = 1 - mean(male_ars_bs_cbl - male_ars_cbl)
  female_q_us = 1 - mean(female_q_bs_cbl - female_q_cbl)
  female_ars_us = 1 - mean(female_ars_bs_cbl - female_ars_cbl)
  
  
  
  return(list(male_q_us,male_ars_us,female_q_us,female_ars_us))
}

remove_rows <- function(mods, rows_with_na,met = 'cbl') {
  if (length(rows_with_na) > 0) {
    return(sapply(mods, function(df) df[-unique(rows_with_na),met]))
  }
  return(sapply(mods, function(df) df[,met]))
}

performance_shrinkage <-function(mods,rows_with_na,met){
  #get avergae  across each bs sample 
  # Calculate the mean across the rows of 'row_means'
  org_mods <- mods[[1]]
  bs_mods <- mods[[2]]
  
  
  print("org mods")
  #male_q_cbl <- sapply(org_mods[[1]], function(df) df[-unique(rows_with_na),'cbl'])
  print("male q")
  print(dim(org_mods[[1]][[1]]))
  male_q_cbl <- remove_rows(org_mods[[1]],rows_with_na,met = met)
  print(dim(male_q_cbl))
  male_q_cbl <-rowMeans(male_q_cbl)
  
  print("male ars")
  print(dim(org_mods[[2]][[1]]))
  #male_ars_cbl <- sapply(org_mods[[2]], function(df) df[-unique(rows_with_na),'cbl'])
  male_ars_cbl <- remove_rows(org_mods[[2]],rows_with_na,met = met)
  male_ars_cbl <-rowMeans(male_ars_cbl)
  
  print("female q")
  print(dim(org_mods[[3]][[1]]))
  #female_q_cbl <- sapply(org_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_cbl <- remove_rows(org_mods[[3]],rows_with_na,met = met)
  female_q_cbl <-rowMeans(female_q_cbl)
  
  print("female ars")
  print(dim(org_mods[[4]][[1]]))
  #female_ars_cbl <- sapply(org_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  female_ars_cbl <- remove_rows(org_mods[[4]],rows_with_na,met = met)
  female_ars_cbl <-rowMeans(female_ars_cbl)
  
  print('bs mods')
  print("male q")
  print(dim(bs_mods[[1]][[1]]))
  #male_q_bs_cbl <- sapply(bs_mods[[1]], function(df) df[-unique(rows_with_na),'cbl'])
  male_q_bs_cbl <- remove_rows(bs_mods[[1]],rows_with_na,met = met)
  male_q_bs_cbl <-rowMeans(male_q_bs_cbl)
  
  print("male ars")
  print(dim(bs_mods[[2]][[1]]))
  #male_ars_bs_cbl <- sapply(bs_mods[[2]], function(df) df[-unique(rows_with_na),'cbl'])
  male_ars_bs_cbl <- remove_rows(bs_mods[[2]],rows_with_na,met = met)
  male_ars_bs_cbl <-rowMeans(male_ars_bs_cbl)
  
  
  print("female ars")
  print(dim(bs_mods[[3]][[1]]))
  #female_q_bs_cbl <- sapply(bs_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_bs_cbl <- remove_rows(bs_mods[[3]],rows_with_na,met = met)
  female_q_bs_cbl <-rowMeans(female_q_bs_cbl)
  
  #female_ars_bs_cbl <- sapply(bs_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  print("female ars")
  print(dim(bs_mods[[4]][[1]]))
  female_ars_bs_cbl <- remove_rows(bs_mods[[4]],rows_with_na,met = met)
  female_ars_bs_cbl <-rowMeans(female_ars_bs_cbl)
  
  print("shrink lp")
  male_q_us =  mean(male_q_bs_cbl - male_q_cbl)
  male_ars_us =  mean(male_ars_bs_cbl - male_ars_cbl)
  female_q_us =  mean(female_q_bs_cbl - female_q_cbl)
  female_ars_us =  mean(female_ars_bs_cbl - female_ars_cbl)
  
  return(list(male_q_us,male_ars_us,female_q_us,female_ars_us))
}

get_optimism <- function(mods,rows_with_na){
  #get avergae  across each bs sample 
  # Calculate the mean across the rows of 'row_means'
  org_mods <- mods[[1]]
  bs_mods <- mods[[2]]
  
  
  print("org mods")
  #male_q_cbl <- sapply(org_mods[[1]], function(df) df[-unique(rows_with_na),'cbl'])
  print("male q")
  print(dim(org_mods[[1]][[1]]))
  male_q_cbl <- remove_rows(org_mods[[1]],rows_with_na)
  print(dim(male_q_cbl))
  male_q_cbl <-rowMeans(male_q_cbl)
  
  print("male ars")
  print(dim(org_mods[[2]][[1]]))
  #male_ars_cbl <- sapply(org_mods[[2]], function(df) df[-unique(rows_with_na),'cbl'])
  male_ars_cbl <- remove_rows(org_mods[[2]],rows_with_na)
  male_ars_cbl <-rowMeans(male_ars_cbl)
  
  print("female q")
  print(dim(org_mods[[3]][[1]]))
  #female_q_cbl <- sapply(org_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_cbl <- remove_rows(org_mods[[3]],rows_with_na)
  female_q_cbl <-rowMeans(female_q_cbl)
  
  print("female ars")
  print(dim(org_mods[[4]][[1]]))
  #female_ars_cbl <- sapply(org_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  female_ars_cbl <- remove_rows(org_mods[[4]],rows_with_na)
  female_ars_cbl <-rowMeans(female_ars_cbl)
  
  print('bs mods')
  print("male q")
  print(dim(bs_mods[[1]][[1]]))
  #male_q_bs_cbl <- sapply(bs_mods[[1]], function(df) df[-unique(rows_with_na),'cbl'])
  male_q_bs_cbl <- remove_rows(bs_mods[[1]],rows_with_na)
  male_q_bs_cbl <-rowMeans(male_q_bs_cbl)
  
  print("male ars")
  print(dim(bs_mods[[2]][[1]]))
  #male_ars_bs_cbl <- sapply(bs_mods[[2]], function(df) df[-unique(rows_with_na),'cbl'])
  male_ars_bs_cbl <- remove_rows(bs_mods[[2]],rows_with_na)
  male_ars_bs_cbl <-rowMeans(male_ars_bs_cbl)
  
  
  print("female ars")
  print(dim(bs_mods[[3]][[1]]))
  #female_q_bs_cbl <- sapply(bs_mods[[3]], function(df) df[-unique(rows_with_na),'cbl'])
  female_q_bs_cbl <- remove_rows(bs_mods[[3]],rows_with_na)
  female_q_bs_cbl <-rowMeans(female_q_bs_cbl)
  
  #female_ars_bs_cbl <- sapply(bs_mods[[4]], function(df) df[-unique(rows_with_na),'cbl'])
  print("female ars")
  print(dim(bs_mods[[4]][[1]]))
  female_ars_bs_cbl <- remove_rows(bs_mods[[4]],rows_with_na)
  female_ars_bs_cbl <-rowMeans(female_ars_bs_cbl)
  
  print("shrink lp")
  male_q_us = 1 - mean(male_q_bs_cbl - male_q_cbl)
  male_ars_us = 1 - mean(male_ars_bs_cbl - male_ars_cbl)
  female_q_us = 1 - mean(female_q_bs_cbl - female_q_cbl)
  female_ars_us = 1 - mean(female_ars_bs_cbl - female_ars_cbl)
  
  return(list(male_q_us,male_ars_us,female_q_us,female_ars_us))
}

load_modles <-function(path = 'risk_scores/ARS/org_imp/',suffix = ''){
  #load orgianl models 
  q_mod_m <-readRDS(paste0(path,'q_mod_m',suffix , '.rds'))
  ars_mod_m <-readRDS(paste0(path,'ars_mod_m' ,suffix,'.rds'))
  
  q_mod_f <-readRDS(paste0(path,'q_mod_f',suffix,'.rds'))
  ars_mod_f <-readRDS(paste0(path,'ars_mod_f',suffix, '.rds'))
  
  return(list(q_mod_m,ars_mod_m,q_mod_f,ars_mod_f))
}


adjust_lp <- function(imputed_datasets,mods,shrinkage,model = 'qrisk',fp_terms = NULL){
  #get pooled model lp and ajust for optimism
  q_imp_lp_m <- list()
  q_imp_lp_f <- list()
  
  ars_imp_lp_m <- list()
  ars_imp_lp_f <- list()
  
  for(i in 1:5){
    print("q m")
    q_imp_lp_m[[i]] <-shrinkage[[1]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==1,],mods[[1]],ars = FALSE,model = model)
    print("ars m")
    ars_imp_lp_m[[i]] <- shrinkage[[2]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==1,],mods[[2]],ars = TRUE,model = model,fp_terms = fp_terms)
    print("q f")
    q_imp_lp_f[[i]] <- shrinkage[[3]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==0,],mods[[3]],ars = FALSE,model = model)
    #print(paste0("female q cbl =",imputed_datasets[[i]],q_imp_lp_f[[i]] /shrinkage[[3]] ,lab = 'macce.incident'))
    print("ars")
    ars_imp_lp_f[[i]] <- shrinkage[[4]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==0,],mods[[4]],ars = TRUE,model = model,fp_terms = fp_terms)
  }
  
  return(list(q_imp_lp_m,ars_imp_lp_m,q_imp_lp_f,ars_imp_lp_f))
}

adjust_rs <- function(imputed_datasets,mods,lp,base_surv = NULL){
  #get risk scores
  q_score_m <- list()
  q_score_f <- list()
  
  ars_score_m <- list()
  ars_score_f <- list()
  
  
  for (i in 1:5) {
    sex_cond = imputed_datasets[[i]]$Sex ==1
    q_score_m[[i]] <- get_scores(imputed_datasets[[i]][sex_cond,],lp[[1]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[1]])
    ars_score_m[[i]] <- get_scores(imputed_datasets[[i]][sex_cond,],lp[[2]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[3]])
    
    q_score_f[[i]] <- get_scores(imputed_datasets[[i]][!sex_cond,],lp[[3]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[2]])
    ars_score_f[[i]] <- get_scores(imputed_datasets[[i]][!sex_cond,],lp[[4]][[i]],lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = base_surv[[4]])
    
  }
  #pool riks cores together using rubisn rule:https://pubmed.ncbi.nlm.nih.gov/25630926/
  male_q_pool <- Reduce(`+`, q_score_m) / 5
  male_ars_pool <- Reduce(`+`, ars_score_m) / 5
  
  female_q_pool <- Reduce(`+`, q_score_f) / 5
  female_ars_pool <- Reduce(`+`, ars_score_f) / 5
  return(list(male_q_pool,male_ars_pool,female_q_pool,female_ars_pool))
  
}


get_basline_survival <- function(imputed_datasets,risk_models,sex_cond,lp,time = 6){
  print('male pooled S0')
  base_male_q <- lapply(seq(5),function(i) {get_base_surv_mp(imputed_datasets[[i]][sex_cond,],time = time,mod =risk_models[[i]],lp =lp[[1]])})
  base_male_q <-mean(unlist(base_male_q))
  
  base_male_ars <- lapply(seq(5),function(i) {get_base_surv_mp(imputed_datasets[[i]][sex_cond,],time = time,mod =risk_models[[i]],lp =lp[[2]])})
  base_male_ars <-mean(unlist(base_male_ars))
  
  print('female pooled S0')
  base_female_q <- lapply(seq(5),function(i) {get_base_surv_mp(imputed_datasets[[i]][!sex_cond,],time = time,mod =risk_models[[i]],lp =lp[[3]])})
  base_female_q <-mean(unlist(base_female_q))
  
  base_female_ars <- lapply(seq(5),function(i) {get_base_surv_mp(imputed_datasets[[i]][!sex_cond,],time = time,mod =risk_models[[i]],lp =lp[[4]])})
  base_female_ars <-mean(unlist(base_female_ars))
  
  baseline_surv <- list(base_male_q,base_female_q,base_male_ars,base_female_ars)
  
  return(baseline_surv)
}

pool_ci <-function(res,alpha= 0.05){
  # Calculate the (1 - 2*alpha)% percentile confidence interval for res
  lower_percentile <- alpha * 100 / 2
  upper_percentile <- 100 - lower_percentile
  
  # Calculate the lower and upper percentiles of res_b_m values
  lower_percentile_value <- quantile(res, lower_percentile / 100)
  upper_percentile_value <- quantile(res, upper_percentile / 100)
  # Form the confidence interval
  avg <- mean(res)
  confidence_interval <- c(lower_percentile_value,avg, upper_percentile_value)
  return(confidence_interval)
}

pool_bs <- function(res,met,rows_with_na,M= 5, B = 1000){
  # Initialize an empty matrix to store res_b_m estimates
  
  #loop through uimputed data sets and get mean point estimate for each boostrap
  
  #Theta_b_m point estimates for each boot strap
  #male_q_us
  res_m <- remove_rows(res,rows_with_na,met = met) #sapply(res, function(df) df[-unique(rows_with_na),met])
  res_m <-rowMeans(res_m)
  
  #Now average camples within the bootstrap
  res_b_m <- mean(res_m)
  
  ci <- pool_ci(res_m)
  return(c(ci[1],res_b_m,ci[2]))
}

decison_curve <- function(df,title ='nb_curve',lab= 'macce.incident',time = 10.0,
                          scores = c('ukbb_q,ukbb_full')) {
  png(paste0("/gpfs3ecg_scores/results/",title,".png"), type="cairo")  
  
  probs <- c() #risk score in proabilty format
  for(s in scores){
    prob <- paste0(s,'_prob')
    df[prob] <-df[,s]/100
    probs <- c(probs,prob)
  }
  
  pgrs_nb <- stdca(data=df[ ,], outcome= lab, ttoutcome="macce_obs_time", timepoint=time, 
                   predictors=probs, xstop=0.3, loess.span=0.2, smooth=TRUE)
  
  dev.off()
}
pool_diff <- function(res1,res2,met,rows_with_na,M= 5, B = 1000,idx = 1){
  # Initialize an empty matrix to store res_b_m estimates
  
  #loop through uimputed data sets and get mean point estimate for each boostrap
  
  #Theta_b_m point estimates for each boot strap
  #male_q_us
  res1_m <- remove_rows(res1,rows_with_na,met = met) #sapply(res, function(df) df[-unique(rows_with_na),met])
  res1_m <-rowMeans(res1_m)
  
  res2_m <- remove_rows(res2,rows_with_na,met = met)
  
  
  res2_m <-rowMeans(res2_m)
  
  
  #Now average camples within the bootstrap
  res_b_m1 <- mean(res1_m)
  res_b_m2 <- mean(res2_m)
  
  #ci <- pool_ci(shrinkage_con[[idx+1]]*res2_m - shrinkage[[idx]]*res1_m)
  ci <- pool_ci(res2_m - res1_m)
  mean_dif <- mean(res2_m - res1_m)
  
  boot.high <- ifelse(res2_m >res1_m, 1, 0)
  print(c(ci[1],mean_dif,ci[2]))
  
  return(c(ci[1],mean_dif,ci[2]))
}

calc_diff <- function(res1,res2,met,rows_with_na,M= 5, B = 1000,idx = 1){
  # Initialize an empty matrix to store res_b_m estimates
  
  #loop through uimputed data sets and get mean point estimate for each boostrap
  
  #Theta_b_m point estimates for each boot strap
  #male_q_us
  res1_m <- remove_rows(res1,rows_with_na,met = met) #sapply(res, function(df) df[-unique(rows_with_na),met])
  #res1_m <- sapply(res1, function(df) df[-unique(rows_with_na),met])
  res1_m <-rowMeans(res1_m)
  
  res2_m <- remove_rows(res2,rows_with_na,met = met)
  #res2_m <- sapply(res2, function(df) df[-unique(rows_with_na_con),met])
  
  res2_m <-rowMeans(res2_m)
  print("hello")
  #Now average camples within the bootstrap
  res_b_m1 <- mean(res1_m)
  res_b_m2 <- mean(res2_m)
  
  print('shrink')
  print(shrinkage_con[[idx+1]])
  #ci <- pool_ci(shrinkage_con[[idx+1]]*res2_m - shrinkage[[idx]]*res1_m)
  
  return(res2_m - res1_m)
}

get_p_val <- function(obs,diff){
  print("p above zero")
  print(mean(diff < 0))
  
  print("two sided test 0's")
  print(2 * min(mean(diff >= 0),
               mean(diff <= 0)))
  
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