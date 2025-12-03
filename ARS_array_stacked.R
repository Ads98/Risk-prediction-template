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
library(tidyr)


env <- 'score_hc_lin'


source(file.path("risk_scores/array_job/9_year_censoring/centered",'0_conf.R'), echo = T)
args <- commandArgs(TRUE)
task_index <- as.integer(args[1])
model <- args[2]
#################################################
store <- as.character(args[3])
split <-  as.character(args[4])
fit_poly <- as.character(args[5])
model_type <- as.character(args[6])
data_source <- as.character(args[7])
fp_terms <- strsplit(args[8], ",")[[1]]
age_interact <- TRUE
if(split == 'null'){
  split <- ''
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


if(data_source != 'ukb' &data_source != 'gp'){
  stop('Error invalid input data source must be either gp or ukb')
}



#################################################



check_dir <- function(path){
  # Check if the directory exists, if not, create it
  if (!file.exists((path))) {
    dir.create(path, recursive = TRUE)  # 'recursive = TRUE' creates parent directories if needed
  }
}

get_cont_interactions <- function(fp_term,model = 'qrisk'){
  print(paste0("model = ",model))
  if(model =='qrisk'){
    cont_vars <- c(fp_term,"SystolBloodPressur" ,"CholesRatio" ,"age_2",
                   "sbp5","age_1","bmi_1" ,"bmi_2" ,"TownsendIndex",
                   "age_1:Diabetes","age_2:Diabetes","Diabetes:age_1","Diabetes:age_2",
                   "age_1:TownsendIndex" ,"age_1:Chronic.kidney.disease.prevalent",                             
                   "age_2:diabetes.type2.prevalent",  "diabetes.type2.prevalent:age_1",
                   "age_2:TownsendIndex", "Essential..primary..hypertension.prevalent:age_1",
                   "age_2:diabetes.type1.prevalent" ,"diabetes.type1.prevalent:age_1" ,               
                   "age_2:bmi_2","age_1:bmi_2","Essential..primary..hypertension.prevalent:age_2",
                   "age_2:bmi_1","age_1:bmi_1", "SystolBloodPressur:age_1",
                   "SystolBloodPressur:age_2" ,"age_2:TownsendIndex" ,                          
                   "age_1:CVDFamilyHistory"  ,"age_1:SmokingCurrent"   ,     "age_1:SmokingPrevious"   ,                        
                   "age_1:SmokingNever" ,"age_1:Atrial.fibrillation.and.flutter.prevalent",
                   
                   "age_2:SystolBloodPressur"   , "age_2:CVDFamilyHistory"  ,                        
                   "age_2:SmokingCurrent"   ,  "age_2:SmokingPrevious"   ,                        
                   "age_2:SmokingNever" ,"age_2:Chronic.kidney.disease.prevalent",
                   "age_2:Atrial.fibrillation.and.flutter.prevalent",
                   
                   "TownsendIndex:age_1" , "Chronic.kidney.disease.prevalent:age_1",                   
                   "diabetes.type2.prevalent:age_2",  "age_1:diabetes.type2.prevalent",
                   "TownsendIndex:age_2", "age_1:Essential..primary..hypertension.prevalent",
                   "diabetes.type1.prevalent:age_2" ,"age_1:diabetes.type1.prevalent" ,               
                   "bmi_2:age_2","bmi_2:age_1","age_2:Essential..primary..hypertension.prevalent",
                   "bmi_1:age_2","bmi_1:age_1", "age_1:SystolBloodPressur",
                   
                   "CVDFamilyHistory:age_1"  ,  "SmokingCurrent:age_1"   ,                         
                   "SmokingPrevious:age_1"   , "SmokingNever:age_1" ,
                   
                   "CVDFamilyHistory:age_2"  ,"SmokingCurrent:age_2"   ,                         
                   "SmokingPrevious:age_2"   , "SmokingNever:age_2" ,"Chronic.kidney.disease.prevalent:age_2",
                   "age_2:SystolBloodPressur", "age_1:Migraine.prevalent",
                   "age_2:Migraine.prevalent","Migraine.prevalent:age_1","Migraine.prevalent:age_2",
                   "age_1:Corticosteroid.prevalent","age_2:Corticosteroid.prevalent",
                   "Corticosteroid.prevalent:age_1","Corticosteroid.prevalent:age_2",
                   "age_1:AtrialFibrillation_prevalent","age_2:AtrialFibrillation_prevalent",
                   "AtrialFibrillation_prevalent:age_1","AtrialFibrillation_prevalent:age_2")
  }
  else if(model =='score2'){
    
    cont_vars <- c(fp_term,"Age","SystolBloodPressur" ,"Cholesterol" ,'HdlCholesterol',"BodyMassIndex",
                   "Age:Smoking_binary" ,      "Age:Diabetes" ,  "Age:SystolBloodPressur" , 
                   "Age:Cholesterol" , "Age:HdlCholesterol" , "Smoking_binary:Age" ,  
                   "Diabetes:Age" , "Smoking_binary:Age" , "SystolBloodPressur:Age" ,
                   "Cholesterol:Age" , "HdlCholesterol:Age"
    )
  }
  
  else if(model == 'aha_male' || model == 'aha_female'){
    cont_vars <- c(fp_term,"log(Age)","log(Age):log(Age)" ,"log(Cholesterol)" ,'log(HdlCholesterol)',"log(T_sbp)",
                   "log(U_sbp)" ,      "log(Age):log(Cholesterol)" ,  "log(Age):log(HdlCholesterol)" , 
                   "log(age):Smkoing_binary" , "log(age):log(U_sbp)","log(age):log(T_sbp)",
                   
                   "log(Cholesterol):log(Age)" ,"log(HdlCholesterol):log(Age)" ,
                   "Smkoing_binary:log(Age)","log(Age):log(T_sbp)",
                   "log(Age):log(U_sbp)"
    )
  }else if(model == 'asx'){
  cont_vars <- c(fp_term,"Age")
  
  }else if(model == 'acc'){
  cont_vars <- c(fp_term)
  }
  
  return(cont_vars)
  
  
}

lp_test <- function(df,mod,ars = FALSE,model = 'qrisk'){
  
  test_form <- get_formula(rownames(summary(mod)$coef),summary(mod)$estimate)
  #test_form <- get_formula(mod$term,mod$estimate)
  
  
  matrix_data <- model.matrix(as.formula(test_form), data = df)
  print(matrix_data)
  matrix_data <- matrix_data[, -1]
  # Initialize an empty matrix for mean-centered data
  matrix_cent <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
  matrix_weight <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
  print("rename")
  for(i in 1:ncol(matrix_data)){
    
    colnames(matrix_data)[i] <- as.character(names(coef(mod)[i]))
    #colnames(matrix_data)[i] <- gsub(colnames(matrix_data)[i],as.character(mod$term[i]))
  }
  print(colnames(matrix_data))
  print("assign cols")
  colnames(matrix_cent) <- colnames(matrix_data)
  colnames(matrix_weight) <- colnames(matrix_data)
  
  # Mean center each column
  ars_term = ''
  age_1_ars =''
  age_2_ars = ''
  if(ars == TRUE){
    #ars_term <- regmatches(test_form, regexpr("I\\([^ActivityRiskScore]+ActivityRiskScore \\+ [^+]+", test_form))
    #ars_term <- gsub("\\s+$", "", ars_term)#remove padded spaces
    #age_1_ars <- paste0(ars_term,":age_1")
    #age_2_ars <- paste0(ars_term,":age_2")
    
    
  }
  
  fp_term <- regmatches(test_form, gregexpr("~\\s*(\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\)\\s*)", test_form))[[1]]   
  fp_term <- sub("^\\s*~\\s*", "", fp_term) # remove'~ charecter
  #fp_term <- regmatches(test_form, gregexpr("~\\s*(\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\s*)", test_form))[[1]]
  
  print(fp_term)
  #if not 2 degree polynomal test for 1 degree polynomial
  if (identical(fp_term, character(0))) {
    print("here")
    fp_term <- regmatches(test_form, gregexpr("I\\([^ActivityRiskScore]+ActivityRiskScore \\+ [^+]+", test_form))[[1]]
    
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
  
  cont_vars <- c(fp_term,"SystolBloodPressur" ,"CholesRatio" ,"age_2",
                 "sbp5","age_1","bmi_1" ,"bmi_2" ,"TownsendIndex",
                 
                 
                 "age_1:TownsendIndex" ,                             
                 "age_2:diabetes.type2.prevalent",  "diabetes.type2.prevalent:age_1",
                 "age_2:TownsendIndex", "Essential..primary..hypertension.prevalent:age_1",
                 "age_2:diabetes.type1.prevalent" ,"diabetes.type1.prevalent:age_1" ,               
                 "age_2:bmi_2","age_1:bmi_2","Essential..primary..hypertension.prevalent:age_2",
                 "age_2:bmi_1","age_1:bmi_1", "SystolBloodPressur:age_1",
                 "SystolBloodPressur:age_2" ,"age_2:TownsendIndex" ,                          
                 "age_1:CVDFamilyHistory"  ,"age_1:SmokingCurrent"   ,     "age_1:SmokingPrevious"   ,                        
                 "age_1:SmokingNever" ,
                 
                 "age_2:SystolBloodPressur"   , "age_2:CVDFamilyHistory"  ,                        
                 "age_2:SmokingCurrent"   ,  "age_2:SmokingPrevious"   ,                        
                 "age_2:SmokingNever" ,
                 
                 "TownsendIndex:age_1" ,                    
                 "diabetes.type2.prevalent:age_2",  "age_1:diabetes.type2.prevalent",
                 "TownsendIndex:age_2", "age_1:Essential..primary..hypertension.prevalent",
                 "diabetes.type1.prevalent:age_2" ,"age_1:diabetes.type1.prevalent" ,               
                 "bmi_2:age_2","bmi_2:age_1","age_2:Essential..primary..hypertension.prevalent",
                 "bmi_1:age_2","bmi_1:age_1", "age_1:SystolBloodPressur",
                 
                 "CVDFamilyHistory:age_1"  ,  "SmokingCurrent:age_1"   ,                         
                 "SmokingPrevious:age_1"   , "SmokingNever:age_1" ,
                 
                 "CVDFamilyHistory:age_2"  ,"SmokingCurrent:age_2"   ,                         
                 "SmokingPrevious:age_2"   , "SmokingNever:age_2" ,
                 "age_2:SystolBloodPressur")
  #age_1_ars,age_2_ars)
  
  for(term in fp_term){
    #age_1_ars <- paste0(age_1_ars,term,":age_1")
    #age_2_ars <- paste0(age_2_ars,term,":age_2")   fp_term
    #cont_vars <- c(term,cont_vars)
    if(model == 'qrisk'){
      cont_vars <- c(cont_vars,paste0(term,":age_1"))
      cont_vars <- c(cont_vars,paste0(term,":age_2"))
    }
    else if(model == 'score2'){
      cont_vars <- c(cont_vars,paste0(term,":Age"))
    }
  }
  
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded spaces
  
  
  print(cont_vars)
  for (i in colnames(matrix_data)) {
    if(i %in% cont_vars){
      print(i)
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i] - mean(matrix_data[, colnames(matrix_data) == i] )
    }
    else{
      print(paste0("no mean:",i))
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i]
    }
    #matrix_cent[, i] <- matrix_data[, i] - mean(matrix_data[, i])
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
    #matrix_weight[, colnames(matrix_weight) == i] =  matrix_cent[, colnames(matrix_cent) == i] * mod[mod$term == i,]$estimate  #coef(mod)[i]
    print(paste0("col = ",i , "coef = ",coef(mod)[i]))
    matrix_weight[, i] <- matrix_cent[, i] * coef(mod)[i]
  }
  print("wights")
  print(matrix_weight)
  print(dim(matrix_weight))
  pred <- rowSums(matrix_weight)
  
  return(pred)
  
  
}





get_formula <- function(terms,estimates){
  # Create a formula string
  #formula_string <- "Surv(event_time) ~ "
  formula_string <- "Surv(macce_obs_time, macce.incident) ~"
  # Iterate through the terms and estimates
  for (i in seq_along(terms)) {
    if (i > 1) {
      formula_string <- paste(formula_string, " + ", sep = "")
    }
    #print(i)
    #print(terms[i])
    #print(estimates[i])
    #formula_string <- paste(formula_string, terms[i], "*",estimates[i]," ", sep = "")
    formula_string <- paste(formula_string, terms[i], " ", sep = "")
  }
  
  # Print the formula string
  formula_string <- gsub(":", "*", formula_string)
  cat(formula_string, "\n")
  return(formula_string)
  
}


pool_avg <- function(datasets,mod,model = 'qrisk',fp_terms = NULL,sex_cond =NULL){
  #get varibles and interaction terms to cenetr
  model_formula <- get_formula(mod$term,mod$estimate)
  
  #derive interaction terms
  print('derive ints')
  interaction_datasets <- lapply(datasets, function(df) {
    #print(dim(df))
    matrix_data <- model.matrix(as.formula(model_formula), data = df[sex_cond,])
    matrix_data <- matrix_data[, -1]
      for(i in 1:ncol(matrix_data)){
      #print(i)
      
      colnames(matrix_data)[i] <- as.character(mod$term[i])
      #colnames(matrix_data)[i] <- gsub(colnames(matrix_data)[i],as.character(mod$term[i]))
    }
    matrix_data
  })
  
  print('got interactuions')
  #Gather list of interacttion terms that need to be cenetred
  # Note as polynoimails can vary we need to supply a list of the spefic polynomail dervied for this instance
  
  formula_parts <- strsplit(model_formula, "~")[[1]]
  if (length(formula_parts) < 2) {
    return(character(0))  # No terms found after ~
  }
  formula_part <- formula_parts[2]
  
  
  fp_term <- unlist(strsplit(formula_part, "\\s*\\ + \\s*"))
  term_list <- c()
  
  print('getting fps')
  for(term in fp_terms){
    fp <- fp_term[grepl(paste0(term), fp_term) & !grepl("age_", fp_term)] 
    print(fp)
    fp <- gsub("\\s*\\+\\s*", "", fp)
    term_list <- c(term_list,fp)
  }
  fp_term <- term_list
  
  fp_term <- fp_term[!grepl("\\bage_\\w+\\b", fp_term)]
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded spaces
  
  #Load list of terms
  cont_vars <- get_cont_interactions(fp_term,model =model)
  
  #Add new polynomail terms
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
  
  #remove unsused cont_vars
  #cont_vars <- cont_vars[cont_vars %in% colnames(matrix_data)]
  # Step 4: Calculate pooled means for continuous variables and their interactions
  # Initialize an empty matrix to store results
  result_matrix <- matrix(nrow = length(cont_vars), ncol = 1)
  rownames(result_matrix) <- cont_vars
  colnames(result_matrix) <- "Pooled_Mean"
  
  print(colnames(interaction_datasets[[1]]))
  # Calculate means across imputations
  for (var in cont_vars) {
    # Extract the column for each dataset and calculate pooled mean
    var_means <- sapply(interaction_datasets, function(mat) {
      if (var %in% colnames(mat)) {
        mean(mat[, var])
      } else {
        NA  # Handle cases where the variable is not present
      }
    })
    
    
    
    result_matrix[var, "Pooled_Mean"] <- mean(var_means)
  }
  # Pooled mean (exclude NA values)
  result_matrix <- result_matrix[!is.na(result_matrix[, "Pooled_Mean"]), , drop = FALSE]
  return(result_matrix)
  
}

get_lp <- function(df,mod,ars = FALSE,model = 'qrisk',fp_terms = NULL,pooled_means = NULL){
  
  #test_form <- get_formula(rownames(summary(mod)$coef),summary(mod)$estimate)
  test_form <- get_formula(mod$term,mod$estimate)
  #print(paste0("test form : ",test_form))
  
  matrix_data <- model.matrix(as.formula(test_form), data = df)
  #print("dim matrix data")
  #print(dim(matrix_data))
  matrix_data <- matrix_data[, -1]
  #print(dim(matrix_data))
  # Initialize an empty matrix for mean-centered data
  matrix_cent <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
  matrix_weight <- matrix(NA, nrow = nrow(matrix_data), ncol = ncol(matrix_data))
 # print("rename")
  for(i in 1:ncol(matrix_data)){
    print(i)
    
    colnames(matrix_data)[i] <- as.character(mod$term[i])
    #colnames(matrix_data)[i] <- gsub(colnames(matrix_data)[i],as.character(mod$term[i]))
  }
  #print(colnames(matrix_data))
  #print("assign cols")
  colnames(matrix_cent) <- colnames(matrix_data)
  colnames(matrix_weight) <- colnames(matrix_data)
  
  # Mean center each column
  ars_term = ''
  age_1_ars =''
  age_2_ars = ''


  #if not 2 degree polynomal test for 1 degree polynomial
  formula_parts <- strsplit(test_form, "~")[[1]]
  if (length(formula_parts) < 2) {
    return(character(0))  # No terms found after ~
  }
  formula_part <- formula_parts[2]

  
  
  fp_term <- unlist(strsplit(formula_part, "\\s*\\ + \\s*"))
  term_list <- c()

  for(term in fp_terms){
    #print('cur term')
    #print(term)
    #print(paste0(term,'_'))
    fp <- fp_term[grepl(paste0(term), fp_term) & !grepl("age_", fp_term)] 
    print(fp)
    fp <- gsub("\\s*\\+\\s*", "", fp)
    term_list <- c(term_list,fp)
  }
  fp_term <- term_list

  fp_term <- fp_term[!grepl("\\bage_\\w+\\b", fp_term)]
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded spaces
 # print(paste0("fp term = ",fp_term))
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
  
  print('col names')
  print(colnames(matrix_data))
  print('')
  print('cont vars')
  print(cont_vars)
  print('')
  print(pooled_means)
        
  for (i in colnames(matrix_data)) {
    if(i %in% cont_vars){
      print(paste0('i = ',i))
      print(paste0(i, ": ", pooled_means[i ,"Pooled_Mean"] ))
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i] - pooled_means[i ,"Pooled_Mean"] #mean(matrix_data[, colnames(matrix_data) == i] )
    }
    else{
      print(paste0("no mean:",i))
      matrix_cent[, colnames(matrix_cent) == i] =  matrix_data[, colnames(matrix_data) == i]
    }
  }
  

  
  for (i in colnames(matrix_weight)) {
    
  
    matrix_weight[, colnames(matrix_weight) == i] =  matrix_cent[, colnames(matrix_cent) == i] * mod[mod$term == i,]$estimate  #coef(mod)[i]
  }
  

  pred <- rowSums(matrix_weight)
  
  if(ars)
  {
    print(paste0('ars pred = ',mean(pred)))
    print(paste0('len pred = ',length(colnames(matrix_weight))))
  }else{
    print(paste0('q pred = ',mean(pred)))
    print(paste0('len pred = ',length(colnames(matrix_weight))))
  }
  
  return(pred)
  
  
}

reduce_mp <- function(lp, M= 5) {
  # Combine lists into a matrix
  matrix_data <- do.call(cbind, lp)
  
  # Calculate mean across rows (each index)
  mean_values <- rowMeans(matrix_data)
  
  return(mean_values)
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


get_scores <- function(df,lp,lab = 'macce.incident',time = 9,th = 0.1,baseline_surv = NULL){
  res  <- tryCatch({
 
    print('getting risk score')
    print(baseline_surv)
    risk_score <-  100 * (1 - baseline_surv^exp(lp))
    print('got rs')
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

bs_loop <- function(df,q_mod_m,ars_mod_m,q_mod_f,ars_mod_f,lab = 'macce.incident',time = 9,eval_time_col = 'macce_eval_time',eval_lab = 'macce_eval_lab',th = 0.1,model = 'qrisk',baseline_surv = NULL,pooled_means = NULL){
  #make prediction in the orignal dataset
  #dfine empty dfs to 
  print("make empty")
  empty_df <-data.frame(cbl = NA,c_idx = NA,ici = NA,r2 = NA,nb_10 = NA,nb_7.5 = NA,nb_5 = NA,c_under_65_ars = NA,
                        nri_10 = NA,
                        nri_7.5 = NA,
                        nri_5 = NA,
                        nri_cont = NA,
                        lr = NA,
                        info_gain = NA,
                        c_over_65_ars = NA ,
                        c_under_55_ars = NA,
                        c_55_65_ars = NA,
                        c_65_75_ars =NA,
                        c_over_75_ars = NA)
  print('got empty')
  #define null metrics
  male_mets <- list(empty_df,empty_df)
  female_mets <- list(empty_df,empty_df)
  
  sex_cond = df$Sex == 1
  print("get lp's")
  
  q_imp_lp_m <-get_lp(df[sex_cond,],q_mod_m,ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pooled_means[[1]])
  q_imp_lp_f <-get_lp(df[!sex_cond,],q_mod_f,ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pooled_means[[2]] )
  
  print('fp lps')
  ars_imp_lp_m <-get_lp(df[sex_cond,],ars_mod_m,ars = TRUE,model = model,fp_terms = fp_terms,pooled_means = pooled_means[[1]])
  ars_imp_lp_f <-get_lp(df[!sex_cond,],ars_mod_f,ars = TRUE,model = model,fp_terms = fp_terms,pooled_means = pooled_means[[2]])
  
  print(paste0("male lps =",length(ars_imp_lp_m)))
  print(paste0("female lps =",length(ars_imp_lp_f)))
  print(dim(df[sex_cond,]))

  #calculate risk
  print("surv function")
  print(mean(q_imp_lp_m))
  
  
  print(paste0('baseline survival vals =',baseline_surv))
  print(baseline_surv[[1]])
  print(baseline_surv[[1]][[1]])
  qrisk_male <- get_scores(df[sex_cond,],q_imp_lp_m,lab = eval_lab,time = eval_time,th = th,baseline_surv = ,baseline_surv[[1]])
  qrisk_female <- get_scores(df[!sex_cond,],q_imp_lp_f,lab = eval_lab,time = eval_time,th = th,baseline_surv = ,baseline_surv[[2]])
  
  ars_male <- get_scores(df[sex_cond,],ars_imp_lp_m,lab = eval_lab,time = eval_time,th = th,baseline_surv = ,baseline_surv[[3]])
  ars_female <- get_scores(df[!sex_cond,],ars_imp_lp_f,lab = eval_lab,time = eval_time,th = th,baseline_surv = ,baseline_surv[[4]])
  
  male_rs <- list(qrisk_male,ars_male)
  female_rs <- list(qrisk_female,ars_female)
  
  
  male_preds <- list(q_imp_lp_m,ars_imp_lp_m)
  female_preds <- list(q_imp_lp_f,ars_imp_lp_f)
  
  print("get mets")
  male_mets <- get_mets(df[sex_cond,],male_preds,male_rs,lab = lab,eval_time_col = 'macce_eval_time', eval_lab ='macce_eval_lab')
  print("get female mets")
  female_mets <- get_mets(df[!sex_cond,],female_preds,female_rs,lab = lab,eval_time_col = 'macce_eval_time', eval_lab ='macce_eval_lab')
  
  nri_df_male <- data.frame(macce_obs_time = df[sex_cond,'macce_obs_time'],
                            macce.incident = df[sex_cond,eval_lab],
                       qrisk=qrisk_male,
                       ars = ars_male,
                       qrisk_lp = q_imp_lp_m,
                       ars_lp = ars_imp_lp_m)
  
  nri_df_female <- data.frame(macce_obs_time = df[!sex_cond,'macce_obs_time'],
                            macce.incident = df[!sex_cond,eval_lab],
                            qrisk=qrisk_female,
                            ars = ars_female,
                            qrisk_lp = q_imp_lp_f,
                            ars_lp = ars_imp_lp_f)
  
  print('got mets')
  
  print(names(nri_df_female))
  print(nri_df_female[,'qrisk'][1:5])
  print(nri_df_female[,'ars'][1:5])
 
  print('10 %')
  print(length(male_mets))
 
  
  #print(male_mets[[1]][[1]])
  return(list(male_mets,female_mets))
  
}
# Step 1: Load your data and define your survival object
# Assuming 'macce_obs_time' is the time-to-event variable and 'macce_incident' is the event indicator


#save.image(file='risk_scores/ARS/ars_mice_env.RData')


# Step 2: Perform bootstrap resampling for 10 iterations
#n_bootstrap <- 100

imp_vars <- c("ActivityRiskScore",'Age' ,'Sex', 'BodyMassIndex' , 'diabetes.type1.prevalent' , 'diabetes.type2.prevalent'
              , 'Essential..primary..hypertension.prevalent',  'SystolBloodPressur' , 'TownsendIndex' , 'CholesRatio' , 'Corticosteroid.prevalent' , 'Migraine.prevalent' ,
              'Rheumatoid.arthritis.prevalent' , 'Systemic.lupus.erythematosus.prevalent' , 'Severe.mental.illnes.prevalent' ,
              'Atrial.fibrillation.and.flutter.prevalent' ,'sbp5' ,'CVDFamilyHistory', 'Ethnicity' , 'Smoking','macce_obs_time','macce.incident') 

run_bs <- function(index,model,fit_poly = TRUE,fp_terms = c('ActivirtRiskScore'),store = ''){
  #bs_samples <- read.csv(paste0("/gpfs3risk_scores/array_job",store,"/bs_samples.csv"))
  bs_samples <-  read.csv("risk_scores/array_job/ukb_models/cut_off/cv/stacked/all_nri//qrisk_ars_fp/bs_samples.csv")
  set.seed(0)  # Set a seed for reproducibility
  print(paste0("bs iter = ",index))
  # Create an empty data frame template
  empty_df <- data.frame(
    cbl = numeric(0),
    cidx = numeric(0),
    ici = numeric(0),
    r2 = numeric(0),
    nb_10 = numeric(0),
    nb_7.5 = numeric(0),
    nb_5 = numeric(0),
    nri_10 = numeric(0),
    nri_7.5 = numeric(0),
    nri_5 = numeric(0),
    nri_cont <- numeric(0),
    lr = numeric(0),
    info_gain = numeric(0),
    c_under_65_ars = numeric(0),
    c_over_65_ars = numeric(0) ,
    c_under_55_ars = numeric(0),
    c_55_65_ars = numeric(0),
    c_65_75_ars = numeric(0),
    c_over_75_ars = numeric(0)
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
  
  
  male_q_con <- replicate(5, empty_df, simplify = FALSE)
  female_q_con <- replicate(5, empty_df, simplify = FALSE)
  
  male_ars_con <- replicate(5, empty_df, simplify = FALSE)
  female_ars_con <- replicate(5, empty_df, simplify = FALSE)
  
  male_q_bs_con <- replicate(5, empty_df, simplify = FALSE)
  female_q_bs_con <- replicate(5, empty_df, simplify = FALSE)
  
  male_ars_bs_con <- replicate(5, empty_df, simplify = FALSE)
  female_ars_bs_con <-replicate(5, empty_df, simplify = FALSE)
  
  
  idx <- list()
  
  # Step 3: Create a bootstrap sample
  
  
  
  # bootstrap_indices <- sample(nrow(df), replace = TRUE)
  df <- read.csv(paste0("risk_scores/stacked/array_job",store,'/org_data.csv'))
  df$Smoking <- factor(df$Smoking)
  bootstrap_indices <- unlist(bs_samples[index,2:length(bs_samples)])
  df_bs <- df[bootstrap_indices, ]
  #idx<- list(idx,bootstrap_indices)
  
  #get model formual and dataset
  model_vars <- select_formula(model)
  formula <- model_vars[1]
  formula_cond <- model_vars[2]
  imp_vars <- model_vars[3]
  
  imp_vars <- c(fp_terms,unlist(imp_vars),'eid','macce_eval_time','macce_eval_lab')
  print(imp_vars)
  print('match imp vars')
  print(all((unlist(imp_vars) %in% names(df))))
  print(which((unlist(imp_vars) %in% names(df))))
  print(unlist(imp_vars)[which(unlist(imp_vars) %in% names(df))])
  
  
  selected_function <- switch(model,
                              "qrisk" = get_q_feature,
                              "score2" = get_score_features,
                              "aha" = get_aha_feature,
                              default = {
                                stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
                              }
  )
  
  # Step 3a: Impute missing data (assuming you use the 'mice' package)
  # load orignal multiply impute ddata for model validation and optimsim caluclations
  imp_model <- readRDS(paste0("risk_scores/stacked/array_job/",store,"/imp_model.rds"))
  imputed_datasets <- list()
  for (i in 1:5) {
    imputed_datasets[[i]] <- complete(imp_model,i)
    #imputed_datasets[[i]] <- read.csv(paste0("risk_scores/stacked/array_job",store,"/full_models/","org_imp_",i,".csv"))
    imputed_datasets[[i]]['age'] <- imputed_datasets[[i]]$Age #preserve age for later testing
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
    
    print("rename")
    imp_cond <- imputed_datasets[[i]]$Sex == 1
    for(j in 1:length(fp_terms)){
      term <- fp_terms[j]
      new_term <- paste0(term,'_1')
      imputed_datasets[[i]][imp_cond,new_term] <- imputed_datasets[[i]][imp_cond,term] #- mean(imputed_datasets[[i]][imp_cond,term])
      imputed_datasets[[i]][!imp_cond,new_term] <- imputed_datasets[[i]][!imp_cond,term] #- mean(imputed_datasets[[i]][!imp_cond,term])
    }
    
  }

  #impute and fi models in same seed as orignal model to get fair estimate of opisimism 
  #set.seed(0)
  print("getting imp")
  #print(df_bs)
 
  pred_matrix <- make.predictorMatrix(df_bs[,unlist(imp_vars)])
  # Exclude eid and eval labels form being use din the impuation model
  pred_matrix[, c("eid", "macce_eval_time","macce_eval_lab")] <- 0  # Do not use eid/col1 to predict others
  pred_matrix["eid", ] <- 0             # Do not impute eid
  pred_matrix["macce_eval_time", ] <- 0            # Do not impute macce_eval_lab
  pred_matrix["macce_eval_lab", ] <- 0            # Do not impute macce_eval_lab
  
  imp_bs <- mice(df_bs[,unlist(imp_vars)], method = "pmm", predictorMatrix = pred_matrix,m = 5) 
  
  print(imp_bs)
  bs_imp_data <- complete(imp_bs, n = 5)
  print("got imp")
  bs_datasets <- list()
  stacked_bs <- data.frame()
  
  # Store each imputed dataset in the list

  print("imputing")
  print(dim(df))
  print(dim(df_bs))
  for (i in 1:5) {
    bs_datasets[[i]] <- complete(imp_bs, i)
    bs_datasets[[i]]['age'] <- bs_datasets[[i]]$Age #preserve age for later testing
    print("start data")
    print(i)
    if(model == 'qrisk'){
      bs_datasets[[i]] <- selected_function(bs_datasets[[i]],data.frame(bs_datasets[[i]])) 
      print('bind')
      print(imp_vars)
      print(dim( bs_datasets[[i]]))
      print(dim( model.matrix(~ Smoking - 1, data = bs_datasets[[i]])))
      print(sum(is.na(bs_datasets[[i]]$Smoking)))
      bs_datasets[[i]]  <- cbind(bs_datasets[[i]] , model.matrix(~ Smoking - 1, data = bs_datasets[[i]]))
     
      print('Remove duplicates')
      bs_datasets[[i]]  <- remove_duplicate_columns(bs_datasets[[i]] )
    }
    else{
      bs_datasets[[i]] <- selected_function(bs_datasets[[i]],data.frame(bs_datasets[[i]]),imp_vars) 
    }
    if(!fit_poly){
      print("rename")
      imp_cond <- bs_datasets[[i]]$Sex == 1
      
      for(j in 1:length(fp_terms)){
        term <- fp_terms[j]
        new_term <- paste0(term,'_1')
        bs_datasets[[i]][imp_cond,new_term] <- bs_datasets[[i]][imp_cond,term] #- mean(bs_datasets[[i]][imp_cond,term])
        bs_datasets[[i]][!imp_cond,new_term] <- bs_datasets[[i]][!imp_cond,term] #- mean(bs_datasets[[i]][!imp_cond,term])
      }
    }
    
    stacked_bs <- rbind(stacked_bs, bs_datasets[[i]] )  # Stack it onto the combined data frame
    #store data for later optimism adjustmeants 
  }
  
  if(!fit_poly){
    for(j in 1:length(fp_terms)){
      term <- fp_terms[j]
      fp_terms[j] <- paste0(term,'_1')
    }
  }
  print("got data")
  # Step 3b: Fit the model on the bootstrap dataset
  #print("fit male")
  
  #fir fractional polynomail in bs data 
  #print(stacked_bs)
  stack_cond <- stacked_bs$Sex == 1
  print("male mfp")
  #fp_formula_male <- fit_mfp(stacked_bs[stack_cond,],formula = formula,model = model) #formula = formula
  #fp_formula_male <- fit_mfp(stacked_bs[stack_cond,])
  print("female mfp")
  #fp_formula_female <- fit_mfp(stacked_bs[!stack_cond,])
  
  #fp_formula_female <- fit_mfp(stacked_bs[!stack_cond,],formula = formula,model = model)#formula = formula
  
  
  if(fit_poly){
    stack_cond = stacked_bs$Sex == 1
    fp_male <- fit_mfp(stacked_bs[stack_cond,],age_interact,formula = formula,model = model,fp_terms = fp_terms) # add fp_terms formula = formula
    fp_female <- fit_mfp(stacked_bs[!stack_cond,],age_interact,formula = formula,model = model,fp_terms = fp_terms)#
    
    #fp_male <- fit_mfp(stacked_bs[stack_cond,],formula = formula,model = model,fp_terms = fp_terms) #formula = formula
    #fp_female <- fit_mfp(stacked_bs[!stack_cond,],formula = formula,model = model,fp_terms = fp_terms)#formula = formula
    
    fp_formula_male <- fp_male[[1]]
    fp_formula_female <- fp_female[[1]]
    
    fp_term_male <- fp_male[[2]]
    fp_term_female <- fp_female[[2]]
    
    #calulate new for,mula based on the fractional polynomial and age interactions
    n_cols <- dim(bs_datasets)
    mfp_model <- add_mfp(bs_datasets,age_interact,fp_term_male,fp_term_female,formula,fp_terms = fp_terms,center = FALSE)
    bs_datasets <- mfp_model[[1]]
    new_cols <-  unlist(mfp_model[4])
    fp_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model[[2]])
    fp_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model[[3]])
    
    #recaluclate ars in origanl dat using new terms
    #recaluclate ars in origanl dat using new terms

    imputed_datasets <-  lapply(imputed_datasets, function(df) df[, !names(df) %in% new_cols])#lapply(imputed_datasets, function(df) df[, colnames(df[1:28])])
    imputed_datasets <- add_mfp(imputed_datasets,age_interact,fp_term_male,fp_term_female,formula,fp_terms = fp_terms,center = FALSE)[[1]]
    
  }else{
    base <-"Surv(macce_obs_time,macce.incident)~"
    for(t in fp_terms){
      base <- paste0(base,' ', t , ' + ')
    }
    fp_formula_male <- paste0(base,formula)
    fp_formula_female <- paste0(base,formula)
    
    #fp_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula)
    #fp_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula)
    
    if(model == 'qrisk' & age_interact)
    {
      for(t in fp_terms){
        fp_formula_male <- paste0(fp_formula_male,' + age_1 * ', t)
        fp_formula_male <- paste0(fp_formula_male,' + age_2 * ', t)
        
        fp_formula_female <- paste0(fp_formula_female,' + age_1 * ', t)
        fp_formula_female <- paste0(fp_formula_female,' + age_2 * ', t)
      }
      #fp_formula_male <- paste0(fp_formula_male," + age_1 * ARS_1")
      #fp_formula_male <- paste0(fp_formula_male," + age_2 * ARS_1")
      #fp_formula_female <- paste0(fp_formula_female," + age_1 * ARS_1")
      #fp_formula_female <- paste0(fp_formula_female," + age_2 * ARS_1")
    }
    else if(age_interact){
      for(t in fp_terms){
        fp_formula_male <- paste0(fp_formula_male,' + Age * ', t)
        
        fp_formula_female <- paste0(fp_formula_female,' + Age * ', t)
      }
      #fp_formula_male <- paste0(fp_formula_male," + Age * ARS_1")
      #fp_formula_female <- paste0(fp_formula_female," + Age * ARS_1")
    }
  }
  
  
  male_bs_mods <- lapply(bs_datasets, function(imputed_df) {
    #fit_model(imputed_df[imputed_df$Sex == 1,])
    print("male risk scores")
    #fit_stacked(imputed_df[imputed_df$Sex == 1,],fp_formula_male)
    fit_stacked(imputed_df[imputed_df$Sex == 1,],formula,fp_formula_male,model = model)
  })
  
  #print("fit female")
  female_bs_mods <- lapply(bs_datasets, function(imputed_df) {
    #fit_model(imputed_df[imputed_df$Sex == 0,])
    print("female risk scores")
    #fit_stacked(imputed_df[imputed_df$Sex == 0,],fp_formula_female)
    fit_stacked(imputed_df[imputed_df$Sex == 0,],formula,fp_formula_female,model = model)
  })
  
  
  q_bs_mod_m <- list()
  ars_bs_mod_m <- list()
  
  q_bs_mod_f <- list()
  ars_bs_mod_f <- list()
  
  for (i in 1:5) {
    q_bs_mod_m[[i]] <- male_bs_mods[[i]][[1]]
    ars_bs_mod_m[[i]] <- male_bs_mods[[i]][[2]]
    
    q_bs_mod_f[[i]] <- female_bs_mods[[i]][[1]]
    ars_bs_mod_f[[i]] <- female_bs_mods[[i]][[2]]
  }
  q_bs_pool_m <- summary(pool(q_bs_mod_m))
  ars_bs_pool_m  <- summary(pool(ars_bs_mod_m))
  
  q_bs_pool_f <- summary(pool(q_bs_mod_f))
  ars_bs_pool_f  <- summary(pool(ars_bs_mod_f))
  
  #get pooled baselinE survival
  print('male pooled S0')
  sex_cond <- df_bs$Sex == 1
  lapply(q_bs_mod_m,function(df){print(df)})
  base_male_q <- lapply(q_bs_mod_m,function(mod) {get_base_surv_mp(df_bs[sex_cond,],time = eval_time,mod =mod)})
  base_male_q <-mean(unlist(base_male_q))
  base_male_ars <- lapply(ars_bs_mod_m,function(mod) {get_base_surv_mp(df_bs[sex_cond,],time = eval_time,mod =mod)})
  base_male_ars <-mean(unlist(base_male_ars))
  print('female pooled S0')
  
  base_female_q <- lapply(q_bs_mod_f,function(mod) {get_base_surv_mp(df_bs[!sex_cond,],time = eval_time,mod =mod)})
  base_female_q <-mean(unlist(base_female_q))
  
  base_female_ars <- lapply(ars_bs_mod_f,function(mod) {get_base_surv_mp(df_bs[!sex_cond,],time = eval_time,mod =mod)})
  base_female_ars <-mean(unlist(base_female_ars))
  
  baseline_surv <- list(base_male_q,base_female_q,base_male_ars,base_female_ars)
  
  print("female ars")
  print(ars_bs_pool_f)
  #fit condensed models
  
  if(fit_poly){
    
    fp_con_male <- fit_mfp(stacked_bs[stack_cond,],age_interact,formula = formula_cond,model = model,fp_terms = fp_terms) 
    fp_con_female <- fit_mfp(stacked_bs[!stack_cond,],age_interact,formula = formula_cond,model = model,fp_terms = fp_terms) 
    
    #fp_con_male <- fit_mfp(stacked_bs[stack_cond,],formula = formula_cond,model = model,fp_terms = fp_terms) 
    #fp_con_female <- fit_mfp(stacked_bs[!stack_cond,],formula = formula_cond,model = model,fp_terms = fp_terms) 
    
    
    fp_formula_male_con <- fp_con_male[[1]]
    fp_formula_female_con <- fp_con_female[[1]]
    
    fp_term_male_con <- fp_con_male[[2]]
    fp_term_female_con <- fp_con_female[[2]]
    
    #calulate new for,mula based on the fractional polynomial and age interactions
    mfp_model_con <- add_mfp(bs_datasets,age_interact,fp_term_male_con,fp_term_female_con,formula_cond,fp_terms = fp_terms,suffix= '_con',center = FALSE)
    new_cols <- unlist(mfp_model_con[[4]])
    bs_datasets <- mfp_model_con[[1]]
    fp_con_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model_con[[2]])
    fp_con_formula_female <- paste0("Surv(macce_obs_time,macce.incident)~ ",mfp_model_con[[3]])
    
    #recaluclate ars in origanl dat using new terms

    imputed_datasets <-  lapply(imputed_datasets, function(df) df[, !names(df) %in% new_cols])#lapply(imputed_datasets, function(df) df[, colnames(df[1:28])])
    imputed_datasets <- add_mfp(imputed_datasets,age_interact,fp_term_male_con,fp_term_female_con,formula_cond,fp_terms = fp_terms,suffix = '_con',center = FALSE)[[1]] 
    
  }else{
    base <-"Surv(macce_obs_time,macce.incident)~"
    for(t in fp_terms){
      base <- paste0(base,' ', t , ' + ')
    }
    fp_con_formula_male <- paste0(base,formula_cond)
    fp_con_formula_female <- paste0(base,formula_cond)
    
    #fp_con_formula_male <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula_cond)
    #fp_term_female_con <- paste0("Surv(macce_obs_time,macce.incident)~ "," ARS_1 +",formula_cond)
    
    if(model == 'qrisk' & age_interact)
    {
      for(t in fp_terms){
        fp_con_formula_male <- paste0(fp_con_formula_male,' + age_1 * ', t)
        fp_con_formula_male <- paste0(fp_con_formula_male,' + age_2 * ', t)
        
        fp_con_formula_female <- paste0(fp_con_formula_female,' + age_1 * ', t)
        fp_con_formula_female <- paste0(fp_con_formula_female,' + age_2 * ', t)
      }
      #fp_con_formula_male <- paste0(fp_con_formula_male," + age_1 * ARS_1")
      #fp_con_formula_male <- paste0(fp_con_formula_male," + age_2 * ARS_1")
      #fp_term_female_con <- paste0(fp_term_female_con," + age_1 * ARS_1")
      #fp_term_female_con <- paste0(fp_term_female_con," + age_2 * ARS_1")
    }
    else if(age_interact){
      for(t in fp_terms){
        fp_con_formula_male <- paste0(fp_con_formula_male,' + Age * ', t)
        fp_con_formula_female <- paste0(fp_con_formula_female,' + Age * ', t)
        
      }
      
      #fp_con_formula_male <- paste0(fp_con_formula_male," + Age * ARS_1")
     # fp_term_female_con <- paste0(fp_term_female_con," + Age * ARS_1")
    }
  }
  print('con formulas')
  print(paste0('male: ',fp_con_formula_male))
  print(paste0('female: ',fp_con_formula_female))
  print(formula_cond)
  male_bs_con <- lapply(bs_datasets, function(imputed_df) {
    #fit_condensed(imputed_df[imputed_df$Sex == 1,])
    #fit_stacked_con(imputed_df[imputed_df$Sex == 1,],fp_con_male)
    fit_stacked(imputed_df[imputed_df$Sex == 1,],formula_cond,fp_con_formula_male,model = model)
  })
  
  print("fit female")
  female_bs_con <- lapply(bs_datasets, function(imputed_df) {
    #fit_condensed(imputed_df[imputed_df$Sex == 0,])
    #fit_stacked_con(imputed_df[imputed_df$Sex == 0,],fp_con_female)
    fit_stacked(imputed_df[imputed_df$Sex == 0,],formula_cond,fp_con_formula_female,model = model)
  })
  print('model equal')
  print(male_bs_con[[1]][[1]])
  print(male_bs_con[[1]][[2]])
  
  q_bs_con_m <- list()
  ars_bs_con_m <- list()
  
  q_bs_con_f <- list()
  ars_bs_con_f <- list()
  
  for (i in 1:5) {
    q_bs_con_m[[i]] <- male_bs_con[[i]][[1]]
    ars_bs_con_m[[i]] <- male_bs_con[[i]][[2]]
    
    q_bs_con_f[[i]] <- female_bs_con[[i]][[1]]
    ars_bs_con_f[[i]] <- female_bs_con[[i]][[2]]
  }
  
  q_con_bs_pool_m <- summary(pool(q_bs_con_m))
  ars_con_bs_pool_m  <- summary(pool(ars_bs_con_m))
  
  q_con_bs_pool_f <- summary(pool(q_bs_con_f))
  ars_con_bs_pool_f  <- summary(pool(ars_bs_con_f))
  
  #get pooled basline survival
  #split by sex 
  
  base_male_q_con <- lapply(q_bs_con_m,function(mod) {get_base_surv_mp(df_bs[sex_cond,],time = eval_time,mod =mod)})
  base_male_q_con <-mean(unlist(base_male_q_con))
  base_male_ars_con <- lapply(ars_bs_con_m,function(mod) {get_base_surv_mp(df_bs[sex_cond,],time = eval_time,mod =mod)})
  base_male_ars_con <-mean(unlist(base_male_ars_con))
  
  base_female_q_con <- lapply(q_bs_con_f,function(mod) {get_base_surv_mp(df_bs[!sex_cond,],time = eval_time,mod =mod)})
  base_female_q_con <- mean(unlist(base_female_q_con))
  base_female_ars_con <- lapply(ars_bs_con_f,function(mod) {get_base_surv_mp(df_bs[!sex_cond,],time = eval_time,mod =mod)})
  base_female_ars_con <- mean(unlist(base_female_ars_con))
  
  baseline_surv_con <- list(base_male_q_con,base_female_q_con,base_male_ars_con,base_female_ars_con)
  
  # Pool the means from the deravtion data for centering
  pooled_means_m <- pool_avg(bs_datasets,ars_bs_pool_m,model = model,fp_terms = fp_terms,sex_cond = sex_cond)
  pooled_means_f <- pool_avg(bs_datasets,ars_bs_pool_f,model = model,fp_terms = fp_terms,sex_cond = !sex_cond)
  
  pooled_means_cond_m <- pool_avg(bs_datasets,ars_con_bs_pool_m,model = model,fp_terms = fp_terms,sex_cond = sex_cond)
  pooled_means_cond_f <- pool_avg(bs_datasets,ars_con_bs_pool_f,model = model,fp_terms = fp_terms,sex_cond = !sex_cond)
  
  pooled_means <- list(pooled_means_m,pooled_means_f)
  
  pooled_means_cond <- list(pooled_means_cond_m,pooled_means_cond_f)

  
  #Apply full model and adjust for optimism
  #apply models to each bootstraped dataset 
  bs_results <- lapply(bs_datasets, function(df) {
    bs_loop(df,q_bs_pool_m,ars_bs_pool_m,q_bs_pool_f,ars_bs_pool_f, time = 6,model = model,baseline_surv = baseline_surv,pooled_means = pooled_means)})
  
  #apply modles to the original data
  org_results <- lapply(imputed_datasets, function(df) {
    bs_loop(df,q_bs_pool_m,ars_bs_pool_m,q_bs_pool_f,ars_bs_pool_f, time = 6,model = model,baseline_surv = baseline_surv ,pooled_means = pooled_means)})
  
   #Apply condesned models and adjust for optimism
  bs_results_con <- lapply(bs_datasets, function(df) {
    bs_loop(df,q_con_bs_pool_m,ars_con_bs_pool_m,q_con_bs_pool_f,ars_con_bs_pool_f, time = 6,model = model,baseline_surv = baseline_surv_con,pooled_means = pooled_means_cond)})
  
  #apply condesned modles to the origanl data
  org_results_con <- lapply(imputed_datasets, function(df) {
    bs_loop(df,q_con_bs_pool_m,ars_con_bs_pool_m,q_con_bs_pool_f,ars_con_bs_pool_f, time = 6,model = model,baseline_surv = baseline_surv_con ,pooled_means = pooled_means_cond)})
  
  #get metrcis
  print("got results")
  #Get full model results
  male_q_scores_bs <- list()
  male_ars_scores_bs <- list()
  female_q_scores_bs <- list()
  female_ars_scores_bs <- list()
  
  male_q_scores <- list()
  male_ars_scores <- list()
  female_q_scores <- list()
  female_ars_scores <- list()
  
  for (i in 1:5) {
    male_q_scores_bs[[i]] <- bs_results[[i]][[1]][[1]]
    male_ars_scores_bs[[i]] <- bs_results[[i]][[1]][[2]]
    
    female_q_scores_bs[[i]] <- bs_results[[i]][[2]][[1]]
    female_ars_scores_bs[[i]] <- bs_results[[i]][[2]][[2]]
    
    
    male_q_scores[[i]] <- org_results[[i]][[1]][[1]]
    male_ars_scores[[i]] <- org_results[[i]][[1]][[2]]
    
    female_q_scores[[i]] <- org_results[[i]][[2]][[1]]
    female_ars_scores[[i]] <- org_results[[i]][[2]][[2]]
  }
  for (i in 1:5) {
    male_q_bs[[i]] <- rbind(male_q_bs[[i]],male_q_scores_bs[[i]])
    male_ars_bs[[i]] <- rbind(male_ars_bs[[i]],male_ars_scores_bs[[i]])
    female_q_bs[[i]] <- rbind(female_q_bs[[i]],female_q_scores_bs[[i]])
    female_ars_bs[[i]] <- rbind(female_ars_bs[[i]],female_ars_scores_bs[[i]])
    
    male_q[[i]] <- rbind(male_q[[i]],male_q_scores[[i]])
    male_ars[[i]] <- rbind(male_ars[[i]],male_ars_scores[[i]])
    female_q[[i]] <- rbind(female_q[[i]],female_q_scores[[i]])
    female_ars[[i]] <- rbind(female_ars[[i]],female_ars_scores[[i]])
  }
  
  
  
  #get condensed model results
  print("condensed results")
  male_q_scores_con_bs <- list()
  male_ars_scores_con_bs <- list()
  female_q_scores_con_bs <- list()
  female_ars_scores_con_bs <- list()
  
  male_q_scores_con <- list()
  male_ars_scores_con <- list()
  female_q_scores_con <- list()
  female_ars_scores_con <- list()
  
  for (i in 1:5) {
    male_q_scores_con_bs[[i]] <- bs_results_con[[i]][[1]][[1]]
    male_ars_scores_con_bs[[i]] <- bs_results_con[[i]][[1]][[2]]
    
    female_q_scores_con_bs[[i]] <- bs_results_con[[i]][[2]][[1]]
    female_ars_scores_con_bs[[i]] <- bs_results_con[[i]][[2]][[2]]
    
    
    male_q_scores_con[[i]] <- org_results_con[[i]][[1]][[1]]
    male_ars_scores_con[[i]] <- org_results_con[[i]][[1]][[2]]
    
    female_q_scores_con[[i]] <- org_results_con[[i]][[2]][[1]]
    female_ars_scores_con[[i]] <- org_results_con[[i]][[2]][[2]]
  }
  for (i in 1:5) {
    male_q_bs_con[[i]] <- rbind(male_q_bs_con[[i]],male_q_scores_con_bs[[i]])
    male_ars_bs_con[[i]] <- rbind(male_ars_bs_con[[i]],male_ars_scores_con_bs[[i]])
    female_q_bs_con[[i]] <- rbind(female_q_bs_con[[i]],female_q_scores_con_bs[[i]])
    female_ars_bs_con[[i]] <- rbind(female_ars_bs_con[[i]],female_ars_scores_con_bs[[i]])
    
    male_q_con[[i]] <- rbind(male_q_con[[i]],male_q_scores_con[[i]])
    male_ars_con[[i]] <- rbind(male_ars_con[[i]],male_ars_scores_con[[i]])
    female_q_con[[i]] <- rbind(female_q_con[[i]],female_q_scores_con[[i]])
    female_ars_con[[i]] <- rbind(female_ars_con[[i]],female_ars_scores_con[[i]])
  }
  
  
  
  print("writing files")
  write_cols = FALSE
  if(index  == 1){
    write_cols = TRUE
  }
  #write full models
  for (i in 1:5) {
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/male_q/imp_",i))
    write.table(male_q[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/male_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/male_ars/imp_",i))
    write.table(male_ars[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/male_ars/imp_",i ,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/female_q/imp_",i))
    write.table(female_q[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/female_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/female_ars/imp_",i))
    write.table(female_ars[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/org/female_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/male_q/imp_",i))
    write.table(male_q_bs[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/male_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/male_ars/imp_",i))
    write.table(male_ars_bs[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/male_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/female_q/imp_",i))
    write.table(female_q_bs[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/female_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/female_ars/imp_",i))
    write.table(female_ars_bs[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/full",store,"/bs/female_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
  }
  
  #write condensed models
  print("writting conensed ")
  for (i in 1:5) {
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/male_q/imp_",i))
    write.table(male_q_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/male_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/male_ars/imp_",i))
    write.table(male_ars_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/male_ars/imp_",i ,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/female_q/imp_",i))
    write.table(female_q_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/female_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/female_ars/imp_",i))
    write.table(female_ars_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/org/female_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/male_q/imp_",i))
    write.table(male_q_bs_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/male_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/male_ars/imp_",i))
    write.table(male_ars_bs_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/male_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/female_q/imp_",i))
    write.table(female_q_bs_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/female_q/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
    
    check_dir(paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/female_ars/imp_",i))
    write.table(female_ars_bs_con[[i]],paste0("risk_scores/ARS/bs_imp/con/6y/stacked/condensed",store,"/bs/female_ars/imp_",i,"/res_",index,".csv"),row.names = FALSE, col.names = write_cols)
  }
  print("written files")
}



print(paste0("model = ",model))
source(file.path("risk_scores/array_job/9_year_censoring/centered",'ARS_utils.R'), echo = T)
source(file.path("risk_scores/",'reclass_sex.R'), echo = T)

print(store)
print(split)
print(model)
print(store_suffix)
store <- paste0(store,split,'/',model,'_',store_suffix)
print(store)

print('My args')

print(paste0('modle:',model))
print(paste0('store path:',store))
print(paste0('Fit polynomial: ',fit_poly))
print(paste0('model type: ',model_type))
print(paste0('data_source: ',data_source))
print(paste0('FP terms: ',fp_terms))
run_bs(task_index,model,fit_poly = fit_poly,fp_terms = fp_terms,store = store)

#run_bs(task_index,model,fit_poly = TRUE,fp_terms = c("ActivityRiskScore" ),store = store)


 # df_tmp <- bs_datasets[[1]]
 #  sex_cond <- df_tmp$Sex == 1
 #  pool_means <- pool_avg(list(df_tmp),summary(pool(list(ars_bs_mod_m[[1]],ars_bs_mod_m[[1]]))),model = 'qrisk',fp_terms = fp_terms,sex_cond = sex_cond)
 # #  
 # # 
 #  lp_tmp <- get_lp(df_tmp[sex_cond,],summary(pool(list(ars_bs_mod_m[[1]],ars_bs_mod_m[[1]]))),ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pool_means) #ars_bs_pool_m
 #  mean(lp_tmp)
 # lp_cox <- predict(ars_bs_mod_m[[1]], type = 'lp',newdata = df_tmp[sex_cond,])
 # mean(lp_cox)
# 
# 
# 
# 
# 
# 
# # 
#   df_tmp <- imputed_datasets[[1]]
#   sex_cond <- df_tmp$Sex == 1
#   pool_means <- pool_avg(list(df_tmp),summary(pool(list(ars_imp_mod_m[[1]],ars_imp_mod_m[[1]]))),model = 'qrisk',fp_terms = fp_terms,sex_cond = sex_cond)
# #   
# #  
#   lp_tmp <- get_lp(df_tmp[sex_cond,],summary(pool(list(ars_imp_mod_m[[1]],ars_imp_mod_m[[1]]))),ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pool_means) #ars_bs_pool_m
#   mean(lp_tmp)
#   lp_cox <- predict(ars_imp_mod_m[[1]], type = 'lp',newdata = df_tmp[sex_cond,])
#   mean(lp_cox)

 