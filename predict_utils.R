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
  }
  
  return(cont_vars)
  
  
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
get_lp <- function(df,mod,ars = FALSE,model = 'qrisk',fp_terms = NULL){
  
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
    #colnames(matrix_data)[i] <- gsub(colnames(matrix_data)[i],as.character(mod$term[i]))
  }
  #print(colnames(matrix_data))
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
  
  #fp_term <- regmatches(test_form, gregexpr("~\\s*(\\w.*\\(ActivityRiskScore.*?\\)\\)\\s*)", test_form))[[1]]     #regmatches(test_form, gregexpr("~\\s*(\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\)\\s*)", test_form))[[1]]   
  #fp_term <- sub("^\\s*~\\s*", "", fp_term) # remove'~ charecter
  #fp_term <- regmatches(test_form, gregexpr("~\\s*(\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\s*)", test_form))[[1]]
  
  
  #if not 2 degree polynomal test for 1 degree polynomial
  #if (identical(fp_term, character(0))) {
  print("here")
  formula_parts <- strsplit(test_form, "~")[[1]]
  if (length(formula_parts) < 2) {
    return(character(0))  # No terms found after ~
  }
  formula_part <- formula_parts[2]
  print("get terms")
  print(formula_part)
  
  
  fp_term <- unlist(strsplit(formula_part, "\\s*\\ + \\s*"))
  
  term_list <- c()
  print('fp list')
  for(term in fp_terms){
    print(term)
    print(paste0(term,'_'))
    fp <- fp_term[grepl(paste0(term,'_'), fp_term) & !grepl("age_", fp_term)] 
    print(fp)
    fp <- gsub("\\s*\\+\\s*", "", fp)
    term_list <- c(term_list,fp)
  }
  fp_term <- term_list
  
  #fp_term <- fp_term[grepl("ARS_", fp_term) & !grepl("age_", fp_term)]
  #fp_term <- gsub("\\s*\\+\\s*", "", fp_term)
  
  print("terms")
  print(fp_term)
  #fp_term <- regmatches(test_form, gregexpr("~\\s*\\w.*\\(ActivityRiskScore[^)]+\\)|log\\(ActivityRiskScore\\s*)", test_form))[[1]]
  #regmatches(formula_string[3], gregexpr("~\\s*\\w.*\\(ActivityRiskScore(/\\d+)?(\\)?\\^[+-.]?\\d+(\\.\\d+)?)?\\)\\s*\\+?|log\\(ActivityRiskScore\\)", formula_string[3]))[[1]]
  
  #regmatches(test_form, gregexpr("I\\(ActivityRiskScore[^)]+\\)", test_form))[[1]] # regmatches(test_form, gregexpr("I\\([^ActivityRiskScore]+ActivityRiskScore \\+ [^+]+", test_form))[[1]]
  
  #fp_term <- sub("^\\s*~\\s*", "", fp_term) # remove'~ charecter    
  #fp_term <- gsub("\\s*\\+\\s*", " ", fp_term)
  #fp_term<- unlist(strsplit(fp_term, "\\s*\\ \\s*")) 
  #}
  #else{
  # print("split")
  #fp_term<- unlist(strsplit(fp_term, "\\s*\\ \\s*")) #strsplit(fp_term, " \\+ (?=I\\()", perl = TRUE)[[1]]
  
  #fp_term <- strsplit(fp_term, " \\+ I", perl = TRUE)[[1]]
  #}
  fp_term <- fp_term[!grepl("\\bage_\\w+\\b", fp_term)]
  fp_term <- gsub("\\s+$", "", fp_term)#remove padded spaces
  print(paste0("fp term = ",fp_term))
  age_1_ars = ""
  age_2_ars = ""
  cont_vars <- get_cont_interactions(fp_term,model =model)
  # cont_vars <- c(fp_term,"SystolBloodPressur" ,"CholesRatio" ,"age_2",
  #                "sbp5","age_1","bmi_1" ,"bmi_2" ,"TownsendIndex",
  #                
  #                
  #                "age_1:TownsendIndex" ,                             
  #                "age_2:diabetes.type2.prevalent",  "diabetes.type2.prevalent:age_1",
  #                "age_2:TownsendIndex", "Essential..primary..hypertension.prevalent:age_1",
  #                "age_2:diabetes.type1.prevalent" ,"diabetes.type1.prevalent:age_1" ,               
  #                "age_2:bmi_2","age_1:bmi_2","Essential..primary..hypertension.prevalent:age_2",
  #                "age_2:bmi_1","age_1:bmi_1", "SystolBloodPressur:age_1",
  #                "SystolBloodPressur:age_2" ,"age_2:TownsendIndex" ,                          
  #                "age_1:CVDFamilyHistory"  ,"age_1:SmokingCurrent"   ,     "age_1:SmokingPrevious"   ,                        
  #                "age_1:SmokingNever" ,
  #                
  #                "age_2:SystolBloodPressur"   , "age_2:CVDFamilyHistory"  ,                        
  #                "age_2:SmokingCurrent"   ,  "age_2:SmokingPrevious"   ,                        
  #                "age_2:SmokingNever" ,
  #                
  #                "TownsendIndex:age_1" ,                    
  #                "diabetes.type2.prevalent:age_2",  "age_1:diabetes.type2.prevalent",
  #                "TownsendIndex:age_2", "age_1:Essential..primary..hypertension.prevalent",
  #                "diabetes.type1.prevalent:age_2" ,"age_1:diabetes.type1.prevalent" ,               
  #                "bmi_2:age_2","bmi_2:age_1","age_2:Essential..primary..hypertension.prevalent",
  #                "bmi_1:age_2","bmi_1:age_1", "age_1:SystolBloodPressur",
  #                
  #                "CVDFamilyHistory:age_1"  ,  "SmokingCurrent:age_1"   ,                         
  #                "SmokingPrevious:age_1"   , "SmokingNever:age_1" ,
  #                
  #                "CVDFamilyHistory:age_2"  ,"SmokingCurrent:age_2"   ,                         
  #                "SmokingPrevious:age_2"   , "SmokingNever:age_2" ,
  #                "age_2:SystolBloodPressur")
  #age_1_ars,age_2_ars)
  
  for(term in fp_term){
    #age_1_ars <- paste0(age_1_ars,term,":age_1")
    #age_2_ars <- paste0(age_2_ars,term,":age_2")   fp_term
    #cont_vars <- c(term,cont_vars)
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
  print('')
  print(colnames(matrix_data))
  
  
  
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
  print('Hello Adam')
  print("weight matrix")
  print('')
  
  for (i in colnames(matrix_weight)) {
    
    print(i)
    
    #print(matrix_cent[1:5, colnames(matrix_cent) == i])
    #print(mod[mod$term == i,]$estimate)
    #print(mod)
    #print(test_form)
    #print(colnames(matrix_data))
    #print(as.formula(test_form))
    matrix_weight[, colnames(matrix_weight) == i] =  matrix_cent[, colnames(matrix_cent) == i] * mod[mod$term == i,]$estimate  #coef(mod)[i]
    #matrix_weight[, i] <- matrix_cent[, i] * coef(test_ars)[i]
  }
  
  print('')
  print('df names')
  print(colnames(df))
  print("dimw eight")
  print(dim(matrix_weight))
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
    #base <- coxph(Surv(df$macce_obs_time,df[,lab])~offset(lp))
    #base_surv <- summary(survfit(base),time=time)$surv
    
    #base_surv <- summary(survfit(mod),time=time)$surv
    print('getting risk score')
    print(baseline_surv)
    #print(baseline_surv^exp)
    #print('rs baseline_surv = ',baseline_surv)
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