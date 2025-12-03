library(sigmoid)
library(stringr)
#QRISK3 = " age_1 + age_2 + bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + sbp5 + Ethnicity + SmokingPrevious + SmokingCurrent + Chronic.kidney.disease.prevalent + age_1 * Essential..primary..hypertension.prevalent + age_1 * Chronic.kidney.disease.prevalent + age_1 * diabetes.type1.prevalent + age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * CVDFamilyHistory + age_1 * SystolBloodPressur + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * SmokingPrevious + age_1 * SmokingCurrent + age_1 * Corticosteroid.prevalent + age_1 * Migraine.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * Chronic.kidney.disease.prevalent + age_2 * diabetes.type1.prevalent + age_2 * diabetes.type2.prevalent + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * CVDFamilyHistory + age_2 * SystolBloodPressur + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * SmokingPrevious + age_2 * SmokingCurrent + age_2 * Corticosteroid.prevalent + age_2 * Migraine.prevalent"
#           age_1 + age_2 + bmi_1 + bmi_2 + CVDFamilyHistory + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_1 *CVDFamilyHistory + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur  +  age_2 *CVDFamilyHistory ,

#QRISK3 <- " age_1 + age_2 + bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + sbp5 + Ethnicity + SmokingPrevious + SmokingCurrent + Chronic.kidney.disease.prevalent + age_1 * Essential..primary..hypertension.prevalent + age_1 * diabetes.type1.prevalent + age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * CVDFamilyHistory + age_1 * SystolBloodPressur + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * Corticosteroid.prevalent + age_1 * Migraine.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * diabetes.type1.prevalent + age_2 * diabetes.type2.prevalent + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * CVDFamilyHistory + age_2 * SystolBloodPressur + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * Corticosteroid.prevalent + age_2 * Migraine.prevalent"

#small qrisk3 reproducer, exclding vars with small n (<1%)

QRISK3 <- " age_1 + age_2 + bmi_1 + bmi_2 + Diabetes + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent  + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent + age_1 * Diabetes + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * Corticosteroid.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * Diabetes + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * SystolBloodPressur + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * Corticosteroid.prevalent"
ASX <- "Age +Sex"
ASX_start <- " Age + Sex"

#  age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur+ age_2 *CVDFamilyHistory +age_2 *Chronic.kidney.disease.prevalent"

SCORE2 = " Age + SystolBloodPressur + Cholesterol + HdlCholesterol + Diabetes + Smoking_binary + Age * Smoking_binary + Age * Diabetes + Age * SystolBloodPressur + Age * Cholesterol + Age *  HdlCholesterol"

AHA = "log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur) + Essential..primary..hypertension.prevalent + Smkoing_binary + Diabties + log(Age) * log(Cholesterol) + log(Age) * log(HdlCholesterol) + log(Age) * log(SystolBloodPressur) + log(Age) * log(Essential..primary..hypertension.prevalent) +  log(Age) * Smoking_binary"
AHA_m = "log(Age) + log(Age) * log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur) + Essential..primary..hypertension.prevalent + Smkoing_binary + Diabties + log(Age) * log(Cholesterol) + log(Age) * log(HdlCholesterol) + log(Age) * log(SystolBloodPressur) + log(Age) * log(Essential..primary..hypertension.prevalent) +  log(Age) * Smoking_binary"
AHA_f = "log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur) + Essential..primary..hypertension.prevalent + Smkoing_binary + Diabties + log(Age) * log(Cholesterol) + log(Age) * log(HdlCholesterol) + log(Age) * log(SystolBloodPressur) + log(Age) * log(Essential..primary..hypertension.prevalent) +  log(Age) * Smoking_binary"

framingham <- "log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur)  +  Essential..primary..hypertension.prevalent + Smkoing_binary +  log(Age) * log(Cholesterol) + log(Age) * Smkoing_binary"
framingham_m <- "log(Age) + log(Age) * log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur)  +  Essential..primary..hypertension.prevalent + Smkoing_binary +  log(Age) * log(Cholesterol) + log(Age) * Smkoing_binary"
framingha_f <- "log(Age) + log(Cholesterol) + log(HdlCholesterol) +  log(SystolBloodPressur)  +  Essential..primary..hypertension.prevalent + Smkoing_binary +  log(Age) * log(Cholesterol) + log(Age) * Smkoing_binary"

#condensed models
#QRISK3_cond = " age_1 + age_2 + bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent + Essential..primary..hypertension.prevalent + TownsendIndex + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + Ethnicity + SmokingPrevious + SmokingCurrent + Chronic.kidney.disease.prevalent + age_1 * Essential..primary..hypertension.prevalent + age_1 * Chronic.kidney.disease.prevalent + age_1 * diabetes.type1.prevalent + age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * CVDFamilyHistory + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * SmokingPrevious + age_1 * SmokingCurrent + age_1 * Corticosteroid.prevalent + age_1 * Migraine.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * Chronic.kidney.disease.prevalent + age_2 * diabetes.type1.prevalent + age_2 * diabetes.type2.prevalent + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * CVDFamilyHistory + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * SmokingPrevious + age_2 * SmokingCurrent + age_2 * Corticosteroid.prevalent + age_2 * Migraine.prevalent"

#QRISK3_cond = " age_1 + age_2 + bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent + Essential..primary..hypertension.prevalent + TownsendIndex + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + Ethnicity + SmokingPrevious + SmokingCurrent + Chronic.kidney.disease.prevalent + age_1 * Essential..primary..hypertension.prevalent + age_1 * diabetes.type1.prevalent + age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * CVDFamilyHistory + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * Corticosteroid.prevalent + age_1 * Migraine.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * diabetes.type1.prevalent + age_2 * diabetes.type2.prevalent + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * CVDFamilyHistory + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * Corticosteroid.prevalent + age_2 * Migraine.prevalent"

QRISK3_cond = " age_1 + age_2 + bmi_1 + bmi_2 + Diabetes + Essential..primary..hypertension.prevalent + TownsendIndex + Corticosteroid.prevalent + Atrial.fibrillation.and.flutter.prevalent + Ethnicity + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent + age_1 * Diabetes + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * Atrial.fibrillation.and.flutter.prevalent + age_1 * Corticosteroid.prevalent + age_2 * Essential..primary..hypertension.prevalent + age_2 * Diabetes + age_2 * bmi_1 + age_2 * bmi_2 + age_2 * TownsendIndex + age_2 * Atrial.fibrillation.and.flutter.prevalent + age_2 * Corticosteroid.prevalent"


SCORE2_cond = "Age +   Diabetes +Smoking_binary + Age * Smoking_binary + Age * Diabetes "
SCORE2_cond = "Age +   Diabetes +Smoking_binary + Age * Smoking_binary + Age * Diabetes "
AHA_cond = "log(Age) + log(Age) * log(Age) +  Smkoing_binary + diabties +  log(Age)*Smoking_binary"
framingham_cond <- "log(Age) +  Essential..primary..hypertension.prevalent + Smkoing_binary + log(Age) * Smkoing_binary"

#q_vars <- c("ActivityRiskScore",'Age' ,'Sex', 'BodyMassIndex' , 'diabetes.type1.prevalent' , 'diabetes.type2.prevalent'
#            , 'Essential..primary..hypertension.prevalent',  'SystolBloodPressur' , 'TownsendIndex' , 'CholesRatio' , 'Corticosteroid.prevalent' , 'Migraine.prevalent' ,
#            'Rheumatoid.arthritis.prevalent' , 'Systemic.lupus.erythematosus.prevalent' , 'Severe.mental.illnes.prevalent' ,'Chronic.kidney.disease.prevalent',
#            'Atrial.fibrillation.and.flutter.prevalent' ,'sbp5' , 'Ethnicity' , 'Smoking','CVDFamilyHistory','macce_obs_time','macce.incident') 
q_vars <- c('Age' ,'Sex', 'BodyMassIndex' , 'Diabetes' , 'Essential..primary..hypertension.prevalent',  'SystolBloodPressur' ,
            'TownsendIndex' , 'CholesRatio' , 'Corticosteroid.prevalent' ,'Atrial.fibrillation.and.flutter.prevalent' ,
             'sbp5' , 'Ethnicity' , 'Smoking','macce_obs_time','macce.incident') #CVDFamilyHistory

score_vars <- c('Age' ,'Sex', 'Diabetes' , 
                'SystolBloodPressur' ,'Cholesterol' ,'HdlCholesterol', 'Smoking_binary','macce_obs_time','macce.incident') 

aha_vars <- c('Age' ,'Sex', 'Diabetes' , 'SystolBloodPressur','Essential..primary..hypertension.prevalent'
              ,'Cholesterol' ,'HdlCholesterol', 'Smoking_binary','macce_obs_time','macce.incident')

framingham_vars <- c('Age' ,'Sex', 'Essential..primary..hypertension.prevalent','SystolBloodPressur'
              ,'Cholesterol' ,'HdlCholesterol', 'Smoking_binary','macce_obs_time','macce.incident')


select_formula <- function(model){
  if(model == 'qrisk') {
    imp_vars <- q_vars
    formula <- QRISK3
    formula_m <- QRISK3
    formula_f <- QRISK3
    formula_cond <- QRISK3_cond
  }else if (model == 'asx'){
    imp_vars <- c('Age')
    formula <- ASX_start
    formula_m <- ASX_start
    formula_f <- ASX_start
    formula_cond <- SCORE2_cond
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
  }
  else {
    stop("Invalid model name. Supported models are 'qrisk', 'score2', and 'aha'.")
  }
  return(list(formula,formula_cond,imp_vars))
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


# Function to remove duplicate columns
remove_duplicate_columns <- function(df) {
  duplicate_cols <- duplicated(names(df))
  unique_cols <- names(df)[!duplicate_cols]
  return(df %>% select(all_of(unique_cols)))
}

store_imputaion <- function(imp_data,q_mod_m,ars_mod_m,q_mod_f,ars_mod_f,data = TRUE,suffix = '',m = 5,path = 'risk_scores/ARS/org_imp/'){
  if (!file.exists(path)) {
    print('Adding path')
    print(path)
    dir.create(path, recursive = TRUE)  # 'recursive = TRUE' creates parent directories if needed
  }
  if(data){
    print('Add data')
    print(paste0(path,"org_imp_",'i',".csv"))
    for (i in 1:m) {
      write.csv(imp_data[[i]],paste0(path,"org_imp_",i,".csv"))
    }
  }
  
  print('Save models')
  print(paste0(path,"q_mod_m",suffix ,".rds"))
  saveRDS(q_mod_m, file = paste0(path,"q_mod_m",suffix ,".rds"))
  saveRDS(ars_mod_m, file = paste0(path,"ars_mod_m", suffix,".rds"))
  
  saveRDS(q_mod_f, file = paste0(path,"q_mod_f",suffix,".rds"))
  saveRDS(ars_mod_f, file = paste0(path,"ars_mod_f",suffix,".rds"))
}


get_info_gain <- function (df,s1,s2){
  print(s1)
  print(s2)
  p1 <- df[,s1]/100
  p2 <- df[,s2]/100
  var_1 <- var(p1)
  var_2 <- var(p2)
  
  relative_var <- var_1/var_2
  
  info_gain <- 1 - relative_var
  
  return(info_gain)
}

get_idx <- function(bs_df,lp,lab = 'macce.incident',time = 'macce_obs_time'){
  print(paste0('df dim',dim(bs_df)))
  print(paste0('lp len = ',length(lp)))
  print(paste0('ev sum = ',sum(bs_df[,lab])))
  print(paste0('is na =',sum(is.na(lp))))
  
  c_idx <- summary(coxph(Surv(bs_df[,time],bs_df[,lab])~offset(lp)))$concordance[6]
  
  return(c_idx)
}


get_cal <- function(bs_df,rs,lab = 'macce.incident'){
  
 
  #print(length(Y))
  #print(length(P))
  
  # Estimate loess-based smoothed calibration curve
    ICI<- tryCatch({
      Y <- bs_df[,lab]
      P <- rs/100#P <- bs_df[,rs]/100 
      loess.calibrate <- loess(Y ~ P)
      
      P.calibrate<- predict (loess.calibrate, newdata = P)
      ICI <- mean (abs(P.calibrate - P))
      return(ICI)
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
  return(ICI)
}

calculate_nri <- function(df,s1,s2, event_col = 'macce.incident', th = 0.1) {
  p1 <- df[,s1]/100
  p2 <- df[,s2]/100
  print('controls')
  cont_idx <- which(df[,event_col] ==0)
  
  print('cont-chnage')
  control_changes <- move_idx(p1,p2,cont_idx,th)
  
  print('cases')
  case_idx <- which(df[,event_col] ==1)
  print('changes')
  event_changes <- move_idx(p1,p2,case_idx,th)
  
  print('done')
  correct_control <- control_changes[2]
  incorrect_control <- control_changes[3]
  
  correct_cases <- event_changes[3]
  incorrect_cases <- event_changes[2]
  
  correct_control <- (correct_control/100) * length(cont_idx)
  correct_cases <- (correct_cases/100) * length(case_idx)
  
  incorrect_control <- (incorrect_control/100) * length(cont_idx)
  incorrect_cases <- (incorrect_cases/100) * length(case_idx)
  
  
  print('correct moves')
  print(correct_control)
  print(correct_cases)
  
  print('incorrect moves')
  print(incorrect_control)
  print(incorrect_cases)
  
  # Calculate NRI for controls and cases
  nri_control <- (correct_control -incorrect_control ) / length(cont_idx)
  
  nri_case <- ( correct_cases - incorrect_cases) / length(case_idx)
  
  # Calculate overall NRI
  print('hi')
  overall_nri <- nri_control + nri_case
  print(overall_nri)
  
  continuous_nri <-improveProb(x1 = p1, x2 = p2, y = df[, event_col])
  return(c(overall_nri,continuous_nri$nri))
  
}




calc_nb <-function(bs_df,rs,lab  = 'macce.incident',th = 0.1)
{
  sNB <- tryCatch({
    p = (sum(bs_df[,lab])/nrow(bs_df))
    pred  = rs/100#bs_df[rs] /100
    
    
    tp_cond =  (bs_df[,lab] == 1) & ( th < pred)
    fp_cond = (bs_df[,lab] == 0) & ( th < pred)
    
    
    tp = nrow(bs_df[tp_cond,])
    fp = nrow(bs_df[fp_cond,])
    w = (th/(1-th))
    N = nrow(bs_df)
    #print("gathering results")
    NB = tp/N - fp /N * w
    sNB = NB/p
    
    #multi_nb$net.benefit[10,preds]
    #multi_nb$net.benefit[10,preds] / p
    return(sNB)
    
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
  return(sNB)
}

get_cbl <- function(bs_df,preds,lab = 'macce.incident',time_col = 'macce_obs_time'){
  cbl  <- tryCatch(
    coxph(Surv(bs_df[,time_col],bs_df[,lab])~preds)$coef,
    error = function(e) {
      # Handle the error
      cat("An error occurred: ", conditionMessage(e), "\n")
      return(NA)  # Return a default value or do something else
    },
    
    finally = {
      # Cleanup code (optional)
    }
  )
  return(cbl)
}



get_r2 <-function(model_out)
{
  r2 <- tryCatch({
    out_var <- var(model_out)
    error_coef <- (pi^2)/6  # Variance of the error term in an equivalent Weibull mode
    #cat("partial hazard = \n", model_out, "\n")
    #cat("model_out shape = ", dim(model_out), "\n")
    #cat("model_out type = ", class(model_out), "\n")
    return(out_var / (out_var + error_coef))
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
  return(r2)
}


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

mean_cent_vars <- function(dat,dev_data,variables_to_center,imp_model = NULL){
  
  # Create a loop to mean center for both genders
  print("center")
  for (gender in c(0, 1)) {
    sex_cond <- dat$Sex == gender
    deriv_cond <- dev_data$Sex == gender
    for (variable in variables_to_center) {
      print("dim1")
      print(dim(dat))
      print("dim2")
      print(dim(dev_data))
      if(is.null(imp_model)){
        print('null var')
        print(variable)
        center <-  mean(dev_data[deriv_cond, variable],na.rm = TRUE)
      }else{
        imp_data <- list()
        for(i in 1:5){
          imp_data[[i]] <- complete(imp_model,i)
          imp_data[[i]] <- imp_data[[i]][imp_data[[i]]$Sex == gender,]
        }
        print('impute var')
        print(variable)
        center <- remove_rows(imp_data,rows_with_na,met = variable) 
        center <-rowMeans(center)
        center <- mean(center)
        print(paste0('sex = ',gender))
        print(center)
      }
      dat[sex_cond, variable] <- dat[sex_cond, variable] - center
    }
  }
  #print(dat)
  return(dat)


}

get_base_surv_mp <- function(df,time = 6,mod = null,lab = 'macce.incident',lp = NULL,M = 5,shrink = NULL){
  #print('mod = ')
  #print(mod)
  if(is.null(lp)){
    base_surv <- summary(survfit(mod),time=time)$surv
  }else{
    
    #tmp_mod<- mod  # apply unfirom shrinakge to coecfeints
    #tmp_mod$coefficients <- tmp_mod$coefficients *shrink
    #base_surv <- summary(survfit(tmp_mod),time=time)$surv
    shrunk_lp <- reduce_mp(lp,M = M)
    print(dim(shrunk_lp))
    base <- coxph(Surv(df$macce_obs_time,df[,lab])~offset(shrunk_lp))
    base_surv <- summary(survfit(base),time=time)$surv
  }
  return(base_surv)
}

get_score_features <- function(dat,dev_data,vars,imp_model = NULL){
  print("get vars")
  #calculate qrisk 3 interaction terms 
  #print("T1")
  print("hello")
  #print(dat)
  cont_vars <- c('Age','SystolBloodPressur','HdlCholesterol','Cholesterol')
  #dat <- mean_cent_vars(dat,dev_data,cont_vars,imp_model = imp_model)
   dat[,'Age'] <- dat$Age /5
   dat[,'SystolBloodPressur'] <- dat$SystolBloodPressur /20
   dat[,'HdlCholesterol'] <- dat$HdlCholesterol /0.5
  
  if ("ActivityRiskScore" %in% names(dat)) {
    print('sigmoid m')
    dat[dat$Sex == 1,'ActivityRiskScore'] <- sigmoid(dat[dat$Sex == 1,'ActivityRiskScore'])
    print('sigmoid f')
    dat[dat$Sex == 0,'ActivityRiskScore'] <- sigmoid(dat[dat$Sex == 0,'ActivityRiskScore'])
  }
  print(dat)
  print("return")
  return(dat)
  
}
get_q_feature <- function(dat,dev_data){
  print("get vars")
  #calculate qrisk 3 interaction terms 

  dat['dage'] <- dat$Age /10
  print(any(is.na(dat['dage'])))
  print(dim(dat))
  print(dev_data)
  dat['dbmi'] <- dat$BodyMassIndex /10

  
  #print("T2")
  dat['age_1'] <- ifelse(dat$Sex == 1, dat$dage^-1, dat$dage^-2)
  dat['age_2'] <- ifelse(dat$Sex == 1, dat$dage^3, dat$dage)

  #print("T3")
  dat['bmi_1'] <- dat$dbmi^-2
  dat['bmi_2'] <- dat$dbmi^-2 * log(dat$dbmi)

  
  #print("T4")
  dev_data['dage'] <- dev_data$Age /10
  dev_data['dbmi'] <- dev_data$BodyMassIndex /10

  
  #print("T5")
  #print(dev_data$age_1)
  dev_data['age_1'] <- ifelse(dev_data$Sex == 1, dev_data$dage^-1, dev_data$dage^-2)
  dev_data['age_2'] <- ifelse(dev_data$Sex == 1, dev_data$dage^3, dev_data$dage)

  #print("T6")
  dev_data['bmi_1'] <- dev_data$dbmi^-2
  dev_data['bmi_2'] <- dev_data$dbmi^-2 * log(dev_data$dbmi)


   print('center vars')
   gender = 1
   sex_cond = dat$Sex ==  gender
   # dat[sex_cond,'age_1']  = dat[sex_cond,'age_1']  - mean(dev_data[dev_data$Sex == gender,'age_1'])
   # dat[sex_cond,'age_2']  = dat[sex_cond,'age_2'] - mean(dev_data[dev_data$Sex == gender,'age_2'])
   # dat[sex_cond,'bmi_1'] = dat[sex_cond,'bmi_1'] - mean(dev_data[dev_data$Sex == gender,'bmi_1'])
   # dat[sex_cond,'bmi_2'] = dat[sex_cond,'bmi_2'] - mean(dev_data[dev_data$Sex == gender,'bmi_2'])
   # dat[sex_cond,'CholesRatio'] = dat[sex_cond,'CholesRatio'] - mean(dev_data[dev_data$Sex == gender,'CholesRatio'],na.rm=TRUE)
   # dat[sex_cond,'SystolBloodPressur'] = dat[sex_cond,'SystolBloodPressur'] - mean(dev_data[dev_data$Sex == gender,'SystolBloodPressur'])
   # dat[sex_cond,'sbp5'] = dat[sex_cond,'sbp5'] - mean(dev_data[dev_data$Sex == gender,'sbp5'])
   # dat[sex_cond,'TownsendIndex'] = dat[sex_cond,'TownsendIndex'] - mean(dev_data[dev_data$Sex == gender,'TownsendIndex'])
   # 
 
   if ("ActivityRiskScore" %in% names(dat)) {
     dat[sex_cond,'ActivityRiskScore'] = sigmoid(dat[sex_cond,'ActivityRiskScore'])
   }
   
   if ("CVD_PGRS" %in% names(dat)) {
     dat[sex_cond,'CVD_PGRS'] = sigmoid(dat[sex_cond,'CVD_PGRS'])
   }
  # 
   gender = 0
   sex_cond = dat$Sex ==  gender
   # dat[sex_cond,'age_1']  = dat[sex_cond,'age_1']  - mean(dev_data[dev_data$Sex == gender,'age_1'])
   # dat[sex_cond,'age_2']  = dat[sex_cond,'age_2'] - mean(dev_data[dev_data$Sex == gender,'age_2'])
   # dat[sex_cond,'bmi_1'] = dat[sex_cond,'bmi_1'] - mean(dev_data[dev_data$Sex == gender,'bmi_1'])
   # dat[sex_cond,'bmi_2'] = dat[sex_cond,'bmi_2'] - mean(dev_data[dev_data$Sex == gender,'bmi_2'])
   # dat[sex_cond,'CholesRatio'] = dat[sex_cond,'CholesRatio'] - mean(dev_data[dev_data$Sex == gender,'CholesRatio'],na.rm=TRUE)
   # dat[sex_cond,'SystolBloodPressur'] = dat[sex_cond,'SystolBloodPressur'] - mean(dev_data[dev_data$Sex == gender,'SystolBloodPressur'])
   # dat[sex_cond,'sbp5'] = dat[sex_cond,'sbp5'] - mean(dev_data[dev_data$Sex == gender,'sbp5'])
   # dat[sex_cond,'TownsendIndex'] = dat[sex_cond,'TownsendIndex'] - mean(dev_data[dev_data$Sex == gender,'TownsendIndex'])
   
   if ("ActivityRiskScore" %in% names(dat)) {
      dat[sex_cond,'ActivityRiskScore'] = sigmoid(dat[sex_cond,'ActivityRiskScore'])
   }
   
   if ("CVD_PGRS" %in% names(dat)) {
     dat[sex_cond,'CVD_PGRS'] = sigmoid(dat[sex_cond,'CVD_PGRS'])
   }
 
  
   #Tranform activty risk score for mfp model
   
  print('retunr')
  return(dat)
}

get_q_val <- function(dat,dev_data,imp_model = NULL){
  
  dat['dage'] <- dat$Age /10
  print(any(is.na(dat['dage'])))
  dat['dbmi'] <- dat$BodyMassIndex /10
  
  dat['age_1'] <- ifelse(dat$Sex == 1, dat$dage^-1, dat$dage^-2)
  dat['age_2'] <- ifelse(dat$Sex == 1, dat$dage^3, dat$dage)
 
  dat['bmi_1'] <- dat$dbmi^-2
  dat['bmi_2'] <- dat$dbmi^-2 * log(dat$dbmi)
  

  if (is.null(imp_model)){
    dev_data['dage'] <- dev_data$Age /10
    dev_data['dbmi'] <- dev_data$BodyMassIndex /10
    
    dev_data['age_1'] <- ifelse(dev_data$Sex == 1, dev_data$dage^-1, dev_data$dage^-2)
    dev_data['age_2'] <- ifelse(dev_data$Sex == 1, dev_data$dage^3, dev_data$dage)
   
    dev_data['bmi_1'] <- dev_data$dbmi^-2
    dev_data['bmi_2'] <- dev_data$dbmi^-2 * log(dev_data$dbmi)
  
    
    gender = 1
    sex_cond = dat$Sex ==  gender
    dat[sex_cond,'age_1']  = dat[sex_cond,'age_1']  - mean(dev_data[dev_data$Sex == gender,'age_1'])
    dat[sex_cond,'age_2']  = dat[sex_cond,'age_2'] - mean(dev_data[dev_data$Sex == gender,'age_2'])
    dat[sex_cond,'bmi_1'] = dat[sex_cond,'bmi_1'] - mean(dev_data[dev_data$Sex == gender,'bmi_1'])
    dat[sex_cond,'bmi_2'] = dat[sex_cond,'bmi_2'] - mean(dev_data[dev_data$Sex == gender,'bmi_2'])
    dat[sex_cond,'CholesRatio'] = dat[sex_cond,'CholesRatio'] - mean(dev_data[dev_data$Sex == gender,'CholesRatio'],na.rm=TRUE)
    dat[sex_cond,'SystolBloodPressur'] = dat[sex_cond,'SystolBloodPressur'] - mean(dev_data[dev_data$Sex == gender,'SystolBloodPressur'])
    dat[sex_cond,'sbp5'] = dat[sex_cond,'sbp5'] - mean(dev_data[dev_data$Sex == gender,'sbp5'])
    dat[sex_cond,'TownsendIndex'] = dat[sex_cond,'TownsendIndex'] - mean(dev_data[dev_data$Sex == gender,'TownsendIndex'])
    dat[sex_cond,'ActivityRiskScore'] = sigmoid(dat[sex_cond,'ActivityRiskScore'])
    
    gender = 0
    sex_cond = dat$Sex ==  gender
    dat[sex_cond,'age_1']  = dat[sex_cond,'age_1']  - mean(dev_data[dev_data$Sex == gender,'age_1'])
    dat[sex_cond,'age_2']  = dat[sex_cond,'age_2'] - mean(dev_data[dev_data$Sex == gender,'age_2'])
    dat[sex_cond,'bmi_1'] = dat[sex_cond,'bmi_1'] - mean(dev_data[dev_data$Sex == gender,'bmi_1'])
    dat[sex_cond,'bmi_2'] = dat[sex_cond,'bmi_2'] - mean(dev_data[dev_data$Sex == gender,'bmi_2'])
    dat[sex_cond,'CholesRatio'] = dat[sex_cond,'CholesRatio'] - mean(dev_data[dev_data$Sex == gender,'CholesRatio'],na.rm=TRUE)
    dat[sex_cond,'SystolBloodPressur'] = dat[sex_cond,'SystolBloodPressur'] - mean(dev_data[dev_data$Sex == gender,'SystolBloodPressur'])
    dat[sex_cond,'sbp5'] = dat[sex_cond,'sbp5'] - mean(dev_data[dev_data$Sex == gender,'sbp5'])
    dat[sex_cond,'TownsendIndex'] = dat[sex_cond,'TownsendIndex'] - mean(dev_data[dev_data$Sex == gender,'TownsendIndex'])
    
    dat[sex_cond,'ActivityRiskScore'] = sigmoid(dat[sex_cond,'ActivityRiskScore'])
  }
  else{
    imp_dat <- list()
    for(i in 1:5){
      imp_dat[[i]] <- complete(imp_model,i)
      imp_dat[[i]] ['dage'] <- imp_dat[[i]] $Age /10
      print(any(is.na(imp_dat[[i]] ['dage'])))
      imp_dat[[i]] ['dbmi'] <- imp_dat[[i]] $BodyMassIndex /10
      
      imp_dat[[i]] ['age_1'] <- ifelse(imp_dat[[i]] $Sex == 1, imp_dat[[i]] $dage^-1, imp_dat[[i]] $dage^-2)
      imp_dat[[i]] ['age_2'] <- ifelse(imp_dat[[i]] $Sex == 1, imp_dat[[i]] $dage^3, imp_dat[[i]] $dage)
      
      imp_dat[[i]] ['bmi_1'] <- imp_dat[[i]] $dbmi^-2
      imp_dat[[i]] ['bmi_2'] <- imp_dat[[i]] $dbmi^-2 * log(imp_dat[[i]] $dbmi)
    }
    cont_vars <- c('age_1','age_2','bmi_1','bmi_2','CholesRatio','SystolBloodPressur','sbp5','TownsendIndex')
    for (gender in c(0, 1)) {
      sex_cond <- dat$Sex == gender
      dataset <- lapply(imp_dat,function(df){df[df$Sex == gender,]})
      for (variable in cont_vars) {
        print('impute var')
        print(variable)
        center <- remove_rows(dataset,rows_with_na,met = variable) 
        center <-rowMeans(center)
        center <- mean(center)
        print(center)
        dat[sex_cond, variable] <- dat[sex_cond, variable] - center
      }
    }
  }
  
  dat[dat$Sex == 1,'ActivityRiskScore'] <- sigmoid(dat[dat$Sex == 1,'ActivityRiskScore'])
  print('sigmoid f')
  dat[dat$Sex == 0,'ActivityRiskScore'] <- sigmoid(dat[dat$Sex == 0,'ActivityRiskScore'])
  return(dat)
}

get_fp_terms <- function(formula_string,fp_terms = c("ActivityRiskScore")){
  print(paste0("og form = ",formula_string))
  #check for degree 2
  #fp_term <- regmatches(formula_string, gregexpr("\\w.*?\\(ActivityRiskScore[^)]*\\)", formula_string))[[1]]  #regmatches(formula_string[3], gregexpr("\\w.*\\(\\(ActivityRiskScore.*?\\)\\)\\)", formula_string[3]))[[1]]   
  #fp_term <- regmatches(formula_string, gregexpr("\\w.*\\(ActivityRiskScore.*?\\)\\)\\)", formula_string))[[1]] 
  
  ##if not 2 degree polynomal test for 1 degree polynomial
  #if (identical(fp_term, character(0))) {
    # print("here")
  
  
  
  #fp_term <- unlist(strsplit(formula_string, "\\s*\\+\\s*"))
  fp_term <- unlist(str_split(formula_string, "\\s*\\+(?![^()]*\\))", simplify = TRUE))
  print('formula fp terms')
  print(fp_term)
  
  term_list <- c()
   for(term in fp_terms){
        print(fp_term[grepl(term, fp_term)])
        term_list <- c(term_list,fp_term[grepl(term, fp_term)] )
     }
  fp_term <- term_list
  
  #fp_term <- fp_term[grepl("ActivityRiskScore", fp_term)] #& !grepl("\\*", fp_term)]
  print('term list')
  print(fp_term)
    
    #fp_term <- regmatches(formula_string, gregexpr("\\w.*\\(ActivityRiskScore(/\\d+)?(\\)?\\^[+-.]?\\d+(\\.\\d+)?)?\\)\\s*\\+?|log\\(ActivityRiskScore\\)", formula_string))[[1]]
    #print(paste0("fp 1 =",fp_term))
    #fp_term <- gsub("\\s*\\+\\s*", " ", fp_term)  
    #fp_term<- unlist(strsplit(fp_term, "\\s*\\ \\s*"))
    #regmatches(formula_string[3], gregexpr("I\\(ActivityRiskScore[^)]+\\)", formula_string[3]))[[1]] #regmatches(formula_string[3], gregexpr("I\\([^ActivityRiskScore]+ActivityRiskScore \\+ [^+]+", formula_string[3]))[[1]]
  #}
  #else{
  #  print("split")
    #fp_term<- strsplit(fp_term, " \\+ (?=I\\()", perl = TRUE)[[1]]
    
    #fp_term <- strsplit(fp_term, " \\+ I", perl = TRUE)[[1]]
  #}
  return(fp_term)
}


mfp_terms <- function(fp_term,age_interact,su,suffix = '',fp_terms = c('ActivityRiskScore')){
  fp_formula <- ""
  age_1_term <- ''
  age_2_term <- ''
  print("fp term")
  print(fp_term)
  print(length(fp_term))
  for(t in 1:length(fp_term)){
    if(fp_term[t] == 'ActivityRiskScore'){
        term_name = 'ARS'
   }
   else{
     print('finding term')
     #print(fp_terms)
     term_name = find_fp(fp_terms,fp_term[t]) #fp_term[t]
     print(term_name)
   }
   new_term <- paste0(term_name,'_',t,suffix)
    #new_term <- paste0('ARS_',t,suffix)
    fp_formula <- paste0(fp_formula,new_term,' + ')
    #get age interactions
    if(model == 'qrisk' & age_interact){
      print("qr is true")
      print(new_term)
      age_1_term <- paste0(age_1_term,new_term,"*age_1 + ")
      age_2_term <-paste0(age_2_term,new_term,"*age_2 + ")
    }
    else if(model == 'score2' & age_interact){
      age_1_term <- paste0(age_1_term,new_term,":Age + ")
    }
    
    
  }
  age_term <- paste0(age_1_term,age_2_term)
  age_term <- sub("\\+ $", "", age_term)
  print("age")
  print(age_term)
  print("form")
  print(fp_formula)
  return(list(fp_formula,age_term))
}

mean_center_mfp <- function(datasets,new_term,sex_cond){
  print(sum(is.na(datasets[sex_cond,][new_term] )))
  datasets[sex_cond,new_term] <-  datasets[sex_cond,][new_term] - mean(datasets[sex_cond,new_term])
  print(sum(is.na(datasets[sex_cond,][new_term] )))
  return(datasets)

}

find_fp <- function(fp_terms,org_term){
  print('hi')
  for (substring in fp_terms) {
    print('hello')
         print(substring)
    print(paste0('org terms = ',org_term))
         if (grepl(substring, org_term)) {
             term <- substring
             print('got sub')
             print(term)
          }
  }
  return(term)
}
add_mfp <- function(datasets,age_interact,male_fp,female_fp,formula,suffix = '',fp_terms = c('ActivityRiskScore'),center = TRUE){
    male_mfp <- mfp_terms(male_fp,age_interact,suffix = suffix,fp_terms = fp_terms )
    female_mfp <-mfp_terms(female_fp,age_interact,suffix = suffix,fp_terms = fp_terms )
    
    fp_formula_male <- male_mfp[[1]]
    age_terms_male <- male_mfp[[2]]
    
    fp_formula_female <- female_mfp[[1]]
    age_terms_female <- female_mfp[[2]]

    fp_formula_male <- paste0(fp_formula_male,formula)
    fp_formula_female <- paste0(fp_formula_female,formula)
    
    fp_formula_male <- paste0(fp_formula_male,' + ',age_terms_male)
    fp_formula_female <- paste0(fp_formula_female,' + ',age_terms_female)
    
    
    print(fp_formula_female)
    #fp_terms <- unique(c(male_fp,female_fp))
    male_terms <- c()
    female_terms <- c()
    for(i in 1:5){
      sex_cond = datasets[[i]]$Sex == 1  
      #calulate new terms and create a column in the dataframe
      for(t in 1:length(male_fp)){
        print("male mfp")
        
        if(male_fp[t] == 'ActivityRiskScore'){
              term_name = 'ARS'
         }
         else{
           term_name <- find_fp(fp_terms,male_fp[t])
         }
        print('term_name')
        print(term_name)
        
        new_term <- paste0(term_name,'_',t,suffix)
        male_terms <-c(male_terms,new_term)
        print('new term')
        print(new_term)
        
        #new_term <- paste0('ARS_',t,suffix)
        print("mfp form")
        print(male_fp[t])
        df <-  
          datasets[[i]][sex_cond,] %>%
          mutate(new_term = eval(parse(text =  male_fp[t])))
       
        names(df)[names(df) == 'new_term'] <- new_term
        datasets[[i]][sex_cond,new_term] <- df[,new_term]
        print("ARS = ")
        

       print('got new term')
       print(new_term)
       if(center){
          #mean cneter new term
          datasets[[i]] <- mean_center_mfp(datasets[[i]],new_term,sex_cond)
        }
      }
      
      for(t in 1:length(female_fp)){
        print("female mfp")
        
        if(female_fp[t] == 'ActivityRiskScore'){
              term_name = 'ARS'
         }
         else{
           term_name <- find_fp(fp_terms,female_fp[t])
         }
        new_term <- paste0(term_name,'_',t,suffix)
        female_terms <-c(male_terms,new_term)
        
        #new_term <- paste0('ARS_',t,suffix)
        print("got new")
        print(female_fp[t])
        df2<-  datasets[[i]][!sex_cond,] %>%
           mutate(new_term = eval(parse(text = female_fp[t])))
        
        print("reset name")
        #names(df)[names(df) == 'new_term'] <- new_term
        print("female vals")
        #print(df2)
        #print(colnames(df2))
        #print(df2[,'new_term'])
        datasets[[i]][!sex_cond,new_term] <- df2[,'new_term']
        #mean cneter new term
        print("center")
        print("ARS female = ")
        print(mean( df2[,'new_term']))
        print(mean(datasets[[i]][!sex_cond,new_term]))
        print(mean(datasets[[i]][!sex_cond,'StepsDayMedAdjusted']))
        print(mean(datasets[[i]][,new_term]))
        #imputed_datasets[[i]][!sex_cond,][new_term] <-  imputed_datasets[[i]][!sex_cond,][new_term] - mean(imputed_datasets[[i]][!sex_cond,][new_term])
        if(center){
          datasets[[i]] <- mean_center_mfp(datasets[[i]],new_term,!sex_cond)
        }
      }
      
    }
    terms <- unique(c(male_terms,female_terms))
    return(list(datasets,fp_formula_male,fp_formula_female,terms))
  }


interaction_string <-function(formula_string,age_interact,lab = 'macce.incident',model ='qrisk',fp_terms = c('ActivityRiskScore')){
 
  fp_term <- get_fp_terms(formula_string[3],fp_terms = fp_terms)
  print(paste0("fp term = ",fp_term))
  age_1_ars = ""
  age_2_ars = ""
  
  for(term in fp_term){
    if(model =='qrisk' & age_interact){
      age_1_ars <- paste0(age_1_ars," + age_1:",term)
      age_2_ars <- paste0(age_2_ars," + age_2:",term)
    }
    else if(model =='score2' & age_interact){
        age_1_ars <- paste0(age_1_ars," + Age:",term)
    }
  }
  
  print(paste0("pre form = ",formula_string))
  print(paste0("age_1_ars = ",age_1_ars))
  print(paste0("age_2_ars = ",age_2_ars))
  formula_string <- paste0(formula_string,age_1_ars,age_2_ars)
  print("make formuula")
  print(formula_string[[3]])
  form <- as.formula(paste0('Surv(macce_obs_time,',lab,')~',formula_string[3]))
  print(paste0("final form = ",form)) #as.formula(paste0('Surv(macce_obs_time,',lab,')~',formula_string[3])
  return(list(form,fp_term))
  
}

make_pred <-function(df,qrisk_fp,ars_mod){
  pred_ars <-predict(ars_mod,type="lp",newdata = df)
  pred_q <- predict(qrisk_fp,type="lp",newdata = df)
  
  
  #bs_df['ars'] <- ars_score
  #bs_df['qrisk3'] <- qrisk_score
  return(list(pred_q,pred_ars))
}


fit_mfp_con <- function(stacked_df,lab = 'macce.incident'){
  library(mfp)
  #fp_mod <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent  + TownsendIndex +  Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent  + CVDFamilyHistory +Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 *CVDFamilyHistory +  age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 *CVDFamilyHistory , family="cox",verbose=FALSE,select=1,data=stacked_df)
  
  #fp_mod <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent  + TownsendIndex +  Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent  +Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex  +  age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex  , family="cox",verbose=FALSE,select=1,data=stacked_df)
  fp_form <- interaction_string(as.character(fp_mod$formula),lab = lab)
  return(fp_form)
}

fit_mfp <- function(stacked_df,age_interact,lab = 'macce.incident',formula = QRISK3 ,model = 'qrisk' ,fp_terms = c('ActivityRiskScore')){
  library(mfp)
  base <-"Surv(macce_obs_time,macce.incident)~"
  for(t in fp_terms){
      base <- paste0(base,'fp(',t,')',' + ')
  }
   formula <- paste0(base,formula)
  #formula <- paste0("Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + ",formula)
  formula <- as.formula(formula)
  print(formula)
  fp_mod <-  mfp(formula, family="cox",verbose=FALSE,select=1,data=stacked_df)
  
  #fp_mod <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2 + CVDFamilyHistory + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_1 *CVDFamilyHistory + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur  +  age_2 *CVDFamilyHistory , family="cox",verbose=FALSE,select=1,data=stacked_df)
  
  #fp_mod <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2  + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur  + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur , family="cox",verbose=FALSE,select=1,data=stacked_df)
  
  #fp_form <- interaction_string(as.character(fp_mod$formula),lab = lab,model = mode,fp_terms = fp_terms)
  fp_form <- interaction_string(as.character(fp_mod$formula),age_interact,lab = lab,model = model,fp_terms = fp_terms)
  return(fp_form)
  }

fit_stacked_con <- function(bs_df,ars_form,lab = 'macce.incident'){
  library(mfp)
  #fit models for male and females separtley 
  #extract fp terms
  print("fitting qrisk")
  #qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent +  TownsendIndex  + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent  + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex +  age_1 *CVDFamilyHistory +   age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex + age_2 *CVDFamilyHistory ,data=bs_df)
  qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent +  TownsendIndex  + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent  + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent  + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex  +   age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex  ,data=bs_df)
  
  print("fitting ars")
  ars_mod<- coxph(formula = ars_form, data = bs_df)
  print("model fit complete")
  print(paste0("return form = ",ars_mod$formula))
  return(list(qrisk_fp,ars_mod))
}
fit_stacked <- function(bs_df,org_form,ars_form,lab = 'macce.incident',model = 'qrisk'){
  library(mfp)
  #fit models for male and females separtley 
  #extract fp terms
  print("fitting qrisk")
  org_form <- paste0("Surv(macce_obs_time,macce.incident)~",org_form)
  org_form <- as.formula(org_form)
  qrisk_fp <- coxph(org_form,data=bs_df)
  
  #qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 + CVDFamilyHistory + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_1 *CVDFamilyHistory + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur  +  age_2 *CVDFamilyHistory ,data=bs_df)
  
  #qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 +  diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur  + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur   ,data=bs_df)
  print("fitting ars")
  print(ars_form)
  ars_form <- as.formula(ars_form)
  ars_mod<- coxph(formula = ars_form, data = bs_df)
  print("model fit complete")
  print(paste0("return form = ",ars_mod$formula))
  return(list(qrisk_fp,ars_mod))
  
  
}

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                fit_condensed <- function(bs_df,lab = 'macce.incident'){
  library(mfp)
  #fit models for male and females separtley 
  #extract fp terms
  print("fitting qrisk")
  qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent +  TownsendIndex  + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + CVDFamilyHistory + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent  + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex +  age_1 *CVDFamilyHistory +   age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex + age_2 *CVDFamilyHistory ,data=bs_df)
  print("fitting ars")
  ars_mfp <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2 + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent  + TownsendIndex +  Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent  + CVDFamilyHistory +Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 *CVDFamilyHistory +  age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 *CVDFamilyHistory , family="cox",verbose=FALSE,select=1,data=bs_df)
  #mdel interactioon temrs
  print("ars formula")
  ars_from<- interaction_string(as.character(ars_mfp$formula),lab = lab)
  
  #fir surevial models
  print("re fitting ars interactions")
  #print(ars_from)
  print(paste0("ars_form :",ars_from))
  ars_mod<- coxph(formula = ars_from, data = bs_df)
  print("model fit complete")
  print(paste0("return form = ",ars_mod$formula))
  return(list(qrisk_fp,ars_mod))
  
  
}

fit_model <- function(bs_df,lab = 'macce.incident'){
  library(mfp)
  #fit models for male and females separtley 
  #extract fp terms
  print("fitting qrisk")
  qrisk_fp <- coxph(Surv(macce_obs_time,macce.incident)~  age_1 + age_2 +  bmi_1 + bmi_2 + CVDFamilyHistory + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Rheumatoid.arthritis.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent  + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_2 * Essential..primary..hypertension.prevalent +  age_1 *CVDFamilyHistory + age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur + age_2 *CVDFamilyHistory ,data=bs_df)
  print("fitting ars")
  ars_mfp <- mfp(Surv(macce_obs_time,macce.incident)~fp(ActivityRiskScore) + age_1 + age_2 +  bmi_1 + bmi_2 + CVDFamilyHistory + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent  + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +SmokingNever + SmokingPrevious + SmokingCurrent + age_1 * Essential..primary..hypertension.prevalent +age_1 * diabetes.type1.prevalent  +age_1 * diabetes.type2.prevalent + age_1 * bmi_1 + age_1 * bmi_2 + age_1 * TownsendIndex + age_1 * SystolBloodPressur + age_1 *CVDFamilyHistory + age_2 * Essential..primary..hypertension.prevalent +age_2 * diabetes.type1.prevalent +age_2 * diabetes.type2.prevalent + age_2 * bmi_1 +age_2 * bmi_2 + age_2 * TownsendIndex +age_2 * SystolBloodPressur  +  age_2 *CVDFamilyHistory , family="cox",verbose=FALSE,select=1,data=bs_df)
  #mdel interactioon temrs
  print("ars formula")
  ars_from<- interaction_string(as.character(ars_mfp$formula),lab = lab)
  
  #fir surevial models
  print("re fitting ars interactions")
  #print(ars_from)
  print(paste0("ars_form :",ars_from))
  ars_mod<- coxph(formula = ars_from, data = bs_df)
  print("model fit complete")
  print(paste0("return form = ",ars_mod$formula))
  print("model fit complete")
  return(list(qrisk_fp,ars_mod))
  
  
}

#ars_mfp <- mfp(Surv(bs_df$macce_obs_time,bs_df$macce_9y)~fp(ActivityRiskScore) + fp(Age) + BodyMassIndex + diabetes.type1.prevalent + diabetes.type2.prevalent  + Essential..primary..hypertension.prevalent + SystolBloodPressur + TownsendIndex + CholesRatio + Corticosteroid.prevalent + Migraine.prevalent + Rheumatoid.arthritis.prevalent + Systemic.lupus.erythematosus.prevalent + Severe.mental.illnes.prevalent + Atrial.fibrillation.and.flutter.prevalent + sbp5 + Ethnicity +Smoking, family="cox",verbose=TRUE,select=1,data=bs_df)

# get_mets <- function(bs_df,preds,rs,lab = 'macce.incident'){
#   
#   
#   #get model metrics
#   #Harrels c-index
#   q_cidx <- c()
#   ars_cidx <- c()
#   
#   q_cidx  <- tryCatch(
#     summary(coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~offset(preds[[1]])))$concordance[6],
#     error = function(e) {
#       # Handle the error
#       cat("An error occurred: ", conditionMessage(e), "\n")
#       return(NA)  # Return a default value or do something else
#     },
#     
#     finally = {
#       # Cleanup code (optional)
#     }
#   )
#   
#   ars_cidx  <- tryCatch(
#     summary(coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~offset(preds[[2]])))$concordance[6],
#     error = function(e) {
#       # Handle the error
#       cat("An error occurred: ", conditionMessage(e), "\n")
#       return(NA)  # Return a default value or do something else
#     },
#     
#     finally = {
#       # Cleanup code (optional)
#     }
#   )  
#   #q_cidx <- summary(coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~offset(preds[[1]])))$concordance[6]
#   
#   
#   #ars_cidx <- summary(coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~offset(preds[[2]])))$concordance[6]
#   #get c-index per age group
#   bs_df$risk_group <- rep(0,nrow(bs_df))
#   bs_df['over_65'] <- rep(0,nrow(bs_df))
#   print("hello")
#   bs_df[bs_df$age<55,'risk_group'] <- 1
#   bs_df[(bs_df$age>=55 &bs_df$age< 65 ),'risk_group'] <- 2
#   bs_df[(bs_df$age>=65 &bs_df$age< 75 ),'risk_group'] <- 3
#   bs_df[bs_df$age >=75 ,'risk_group'] <- 4
#   
#   bs_df[bs_df$age>65,'over_65'] <- 1
#   print(paste0('maen age = ',mean(bs_df$age)))
#   print('max')
#   print(paste0('max age = ',max(bs_df$age)))
#   #qrisk c-index
#   print('df dim')
#   print(dim(bs_df[bs_df$over_65 == 0,]))
#   print('lp len')
#   print(length(preds[[1]][bs_df$over_65 == 0]))
#   print(sum(bs_df$over_65 == 0))
#   
#   print('under 65s')
#   print(dim(bs_df[bs_df$over_65 == 1,]))
#   c_under_65_q <- get_idx(bs_df[bs_df$over_65 == 0,],preds[[1]][bs_df$over_65 == 0],lab = lab)
#   print(c_under_65_q)
#   print('over 65s')
#   print(dim(bs_df[bs_df$over_65 == 1,]))
#   print(dim(bs_df[bs_df$over_65 == 1,]))
#   print(length(preds[[1]][bs_df$over_65 == 1]))
#   c_over_65_q  <- get_idx(bs_df[bs_df$over_65 == 1,],preds[[1]][bs_df$over_65 == 1],lab = lab)
#   
#   print('55')
#   print(dim(bs_df[bs_df$risk_group == 1,]))
#   print(length(preds[[1]][bs_df$risk_group == 1]))
#   
#   c_under_55_q  <- get_idx(bs_df[bs_df$risk_group == 1,],preds[[1]][bs_df$risk_group == 1],lab = lab)
#   print('55-65')
#   print(dim(bs_df[bs_df$risk_group == 2,]))
#   print(length(preds[[1]][bs_df$risk_group == 2]))
#   
#   c_55_65_q  <- get_idx(bs_df[bs_df$risk_group == 2,],preds[[1]][bs_df$risk_group == 2],lab = lab)
#   print('65-75')
#   print(dim(bs_df[bs_df$risk_group == 3,]))
#   print(length(preds[[1]][bs_df$risk_group == 3]))
#   
#   c_65_75_q  <- get_idx(bs_df[bs_df$risk_group == 3,],preds[[1]][bs_df$risk_group == 3],lab = lab)
#   print('>75')
#   print(dim(bs_df[bs_df$risk_group == 4,]))
#   print(length(preds[[1]][bs_df$risk_group == 4]))
#   
#   c_over_75_q  <- get_idx(bs_df[bs_df$risk_group == 4,],preds[[1]][bs_df$risk_group == 4],lab = lab)
#   
#   print('got q sub groups')
#   print('under 65s')
#   print(dim(bs_df[bs_df$over_65 == 0,]))
#   print(length(preds[[2]][bs_df$over_65 == 0]))
#   print('over 65s')
#   print(dim(bs_df[bs_df$over_65 == 1,]))
#   print(length(preds[[2]][bs_df$over_65 == 1]))
#   
#   q_c <-  data.frame(c_under_65_q = c_under_65_q,c_over_65_q = c_over_65_q,c_under_55_q = c_under_55_q,c_55_65_q = c_55_65_q,c_65_75_q = c_65_75_q,c_over_75_q = c_over_75_q)
#   #ars c-index
#   c_under_65_ars <- get_idx(bs_df[bs_df$over_65 == 0,],preds[[2]][bs_df$over_65 == 0],lab = lab)
#   c_over_65_ars  <- get_idx(bs_df[bs_df$over_65 == 1,],preds[[2]][bs_df$over_65 == 1],lab = lab)
#   
#   c_under_55_ars  <- get_idx(bs_df[bs_df$risk_group == 1,],preds[[2]][bs_df$risk_group == 1],lab = lab)
#   c_55_65_ars  <- get_idx(bs_df[bs_df$risk_group == 2,],preds[[2]][bs_df$risk_group == 2],lab = lab)
#   c_65_75_ars  <- get_idx(bs_df[bs_df$risk_group == 3,],preds[[2]][bs_df$risk_group == 3],lab = lab)
#   c_over_75_ars  <- get_idx(bs_df[bs_df$risk_group == 4,],preds[[2]][bs_df$risk_group == 4],lab = lab)
#   ars_c <-  data.frame(c_under_65_ars = c_under_65_ars,c_over_65_ars = c_over_65_ars,c_under_55_ars = c_under_55_ars,c_55_65_ars = c_55_65_ars,c_65_75_ars = c_65_75_ars,c_over_75_ars = c_over_75_ars)
#   #c_under_65_q , c_over_65_q,c_under_55_q,c_65_75_q,c_over_75_q
#   
#   #Calibartion in the large 
#   print("qrisk c-index")
#   print(q_cidx)
#   
#   print("ars c-index")
#   print(ars_cidx)
#   
#   print("q-cal")
#   q_cbl <- get_cbl(bs_df,preds[[1]],lab = lab) #coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~preds[[1]])$coef
#   
#   print("ars-cal")
#   ars_cbl <- get_cbl(bs_df,preds[[2]],lab = lab)  #coxph(Surv(bs_df[,'macce_obs_time'],bs_df[,lab])~preds[[2]])$coef
#   
#   
#   #get metrics for ci clacls
#   print("gettinbg sum mets")
#   print("ICI")
#   print(paste0("mean qrisk =",mean(rs[[1]])))
#   print(paste0("mean ars =",mean(rs[[2]])))
#   q_ici <- get_cal(bs_df,rs[[1]],lab = 'macce.incident')
#   ars_ici <- get_cal(bs_df,rs[[2]],lab = 'macce.incident')
#   
#   
#   print("nb")
#   q_nb_10 <- calc_nb(bs_df,rs[[1]],lab = 'macce.incident')
#   ars_nb_10 <- calc_nb(bs_df,rs[[2]],lab = 'macce.incident')
#   
#   q_nb_7_5 <- calc_nb(bs_df,rs[[1]],lab = 'macce.incident',th = 0.075)
#   ars_nb_7_5 <- calc_nb(bs_df,rs[[2]],lab = 'macce.incident',th = 0.075)
#   
#   q_nb_5 <- calc_nb(bs_df,rs[[1]],lab = 'macce.incident',th = 0.05)
#   ars_nb_5 <- calc_nb(bs_df,rs[[2]],lab = 'macce.incident',th = 0.05)
#   
#   print("r2")
#   q_r2 <- get_r2(preds[[1]])
#   ars_r2 <- get_r2(preds[[2]])
#   
#   q_mets <-  data.frame(cbl = q_cbl,c_idx = q_cidx,ici = q_ici,r2 = q_r2,nb_10 = q_nb_10,nb_7.5 = q_nb_7_5,nb_5 = q_nb_5)
#   ars_mets <- data.frame(cbl = ars_cbl,c_idx = ars_cidx,ici = ars_ici,r2 = ars_r2,nb_10 = ars_nb_10,nb_7.5 = ars_nb_7_5,nb_5 = ars_nb_5)  
#   
#   q_mets <- cbind(q_mets,q_c)
#   ars_mets <- cbind(ars_mets,ars_c)
#   print('my dataframe')
#   print(ars_mets)
#   print("len q")
#   print(length(q_mets))
#   print(q_mets$cbl)
#   print(q_mets$c_idx)
#   print(q_mets$ici)
#   print(q_mets$r2)
#   print(q_mets$nb_10)
#   print(q_mets$nb_7.5)
#   print(q_mets$nb_5)
#   
#   print("len ars")
#   print(length(ars_mets))
#   print(ars_mets$cbl)
#   print(ars_mets$c_idx)
#   print(ars_mets$ici)
#   print(ars_mets$r2)
#   print(ars_mets$nb_10)
#   print(ars_mets$nb_7.5)
#   print(ars_mets$nb_5)
#   return(list(q_mets,ars_mets))
#   
# }

get_mets <- function(bs_df, preds, rs, q_mod, ars_mod, q_base_mods, ars_base_mods, lab = 'macce.incident', time_col = 'macce_obs_time',eval_time_col = 'macce_eval_time', eval_lab ='macce_eval_lab') {
  
  # Create risk groups based on age
  bs_df$risk_group <- rep(0, nrow(bs_df))
  bs_df[bs_df$age < 55, 'risk_group'] <- 1
  bs_df[bs_df$age >= 55 & bs_df$age < 65, 'risk_group'] <- 2
  bs_df[bs_df$age >= 65 & bs_df$age < 75, 'risk_group'] <- 3
  bs_df[bs_df$age >= 75, 'risk_group'] <- 4
  
  # Define NRI dataframe
  nri_df <- data.frame(
    macce_obs_time = bs_df[, time_col],
    macce_incident = bs_df[, eval_lab],
    qrisk = rs[[1]],
    ars = rs[[2]],
    qrisk_lp = preds[[1]],
    ars_lp = preds[[2]]
  )
  
  # Initialize empty data frames for qrisk and ars metrics
  qrisk_metrics <- data.frame(
    cbl = NA, c_idx = NA, ici = NA, r2 = NA, nb_10 = NA, nb_7.5 = NA, nb_5 = NA,
    c_under_65 = NA, c_over_65 = NA, c_under_55 = NA, c_55_65 = NA, c_65_75 = NA, c_over_75 = NA,
    ici_under_55 = NA, ici_55_65 = NA, ici_65_75 = NA, ici_over_75 = NA,
    nb_10_under_55 = NA, nb_10_55_65 = NA, nb_10_65_75 = NA, nb_10_over_75 = NA,
    nb_7.5_under_55 = NA, nb_7.5_55_65 = NA, nb_7.5_65_75 = NA, nb_7.5_over_75 = NA,
    nb_5_under_55 = NA, nb_5_55_65 = NA, nb_5_65_75 = NA, nb_5_over_75 = NA,
    r2_under_55 = NA, r2_55_65 = NA, r2_65_75 = NA, r2_over_75 = NA,
    nri_10 = NA, nri_7.5 = NA, nri_5 = NA, nri_cont = NA, info_gain = NA,
    nri_10_under_55 = NA, nri_7.5_under_55 = NA, nri_5_under_55 = NA, nri_cont_under_55 = NA, info_gain_under_55 = NA,
    nri_10_55_65 = NA, nri_7.5_55_65 = NA, nri_5_55_65 = NA, nri_cont_55_65 = NA, info_gain_55_65 = NA,
    nri_10_65_75 = NA, nri_7.5_65_75 = NA, nri_5_65_75 = NA, nri_cont_65_75 = NA, info_gain_65_75 = NA,
    nri_10_over_75 = NA, nri_7.5_over_75 = NA, nri_5_over_75 = NA, nri_cont_over_75 = NA, info_gain_over_75 = NA
  )
  
  ars_metrics <- qrisk_metrics  # Start with the same structure for ARS metrics
  
  # Calculate metrics for the whole dataset
  qrisk_metrics$cbl <- get_cbl(bs_df, preds[[1]], lab = lab, time_col = time_col)
  qrisk_metrics$c_idx <- tryCatch(
    summary(coxph(Surv(bs_df[, eval_time_col], bs_df[, eval_lab]) ~ offset(preds[[1]])))$concordance[6],
    error = function(e) { cat("Error in c-index (qrisk):", conditionMessage(e), "\n"); return(NA) }
  )
  qrisk_metrics$ici <- get_cal(bs_df, rs[[1]], lab = eval_lab)
  qrisk_metrics$nb_10 <- calc_nb(bs_df, rs[[1]], lab = eval_lab)
  qrisk_metrics$nb_7.5 <- calc_nb(bs_df, rs[[1]], lab = eval_lab, th = 0.075)
  qrisk_metrics$nb_5 <- calc_nb(bs_df, rs[[1]], lab = eval_lab, th = 0.05)
  qrisk_metrics$r2 <- get_r2(preds[[1]])
  
  ars_metrics$cbl <- get_cbl(bs_df, preds[[2]], lab = lab, time_col = time_col)
  ars_metrics$c_idx <- tryCatch(
    summary(coxph(Surv(bs_df[, eval_time_col], bs_df[, eval_lab]) ~ offset(preds[[2]])))$concordance[6],
    error = function(e) { cat("Error in c-index (ars):", conditionMessage(e), "\n"); return(NA) }
  )
  ars_metrics$ici <- get_cal(bs_df, rs[[2]], lab = eval_lab)
  ars_metrics$nb_10 <- calc_nb(bs_df, rs[[2]], lab = eval_lab)
  ars_metrics$nb_7.5 <- calc_nb(bs_df, rs[[2]], lab = eval_lab, th = 0.075)
  ars_metrics$nb_5 <- calc_nb(bs_df, rs[[2]], lab = eval_lab, th = 0.05)
  ars_metrics$r2 <- get_r2(preds[[2]])
  
  # Add NRI and info gain metrics for the whole dataset
  qrisk_metrics$nri_10 <- calculate_nri(nri_df, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.1)[1]
  qrisk_metrics$nri_7.5 <- calculate_nri(nri_df, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.075)[1]
  qrisk_metrics$nri_5 <- calculate_nri(nri_df, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.05)[1]
  qrisk_metrics$nri_cont <- calculate_nri(nri_df, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.1)[2]
  qrisk_metrics$info_gain <- get_info_gain(nri_df, 'qrisk', 'ars')
  
  ars_metrics$nri_10 <- qrisk_metrics$nri_10
  ars_metrics$nri_7.5 <- qrisk_metrics$nri_7.5
  ars_metrics$nri_5 <- qrisk_metrics$nri_5
  ars_metrics$nri_cont <- qrisk_metrics$nri_cont
  ars_metrics$info_gain <- qrisk_metrics$info_gain
  
  # Calculate metrics for each risk group
  for (group in 1:4) {
    cat("Processing risk group", group, "\n")
    
    # Subset data for the current risk group
    group_data <- bs_df[bs_df$risk_group == group, ]
    nri_group_data <- nri_df[bs_df$risk_group == group, ]
    if (nrow(group_data) == 0) {
      cat("No data for risk group", group, "\n")
      next
    }
    group_label <- switch(group,
                          "1" = "under_55",
                          "2" = "55_65",
                          "3" = "65_75",
                          "4" = "over_75")
    
    # QRISK metrics
    qrisk_metrics[[paste0("c_", group_label)]] <- tryCatch(
      summary(coxph(Surv(group_data[, eval_time_col], group_data[, eval_lab]) ~ offset(preds[[1]][bs_df$risk_group == group])))$concordance[6],
      error = function(e) { cat("Error in c-index (qrisk):", conditionMessage(e), "\n"); return(NA) }
    )
    qrisk_metrics[[paste0("ici_", group_label)]] <- get_cal(group_data, rs[[1]][bs_df$risk_group == group], lab = eval_lab)
    qrisk_metrics[[paste0("nb_10_", group_label)]] <- calc_nb(group_data, rs[[1]][bs_df$risk_group == group], lab = eval_lab)
    qrisk_metrics[[paste0("nb_7.5_", group_label)]] <- calc_nb(group_data, rs[[1]][bs_df$risk_group == group], lab = eval_lab, th = 0.075)
    qrisk_metrics[[paste0("nb_5_", group_label)]] <- calc_nb(group_data, rs[[1]][bs_df$risk_group == group], lab = eval_lab, th = 0.05)
    qrisk_metrics[[paste0("r2_", group_label)]] <- get_r2(preds[[1]][bs_df$risk_group == group])
    qrisk_metrics[[paste0("nri_10_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.1)[1]
    qrisk_metrics[[paste0("nri_7.5_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.075)[1]
    qrisk_metrics[[paste0("nri_5_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.05)[1]
    qrisk_metrics[[paste0("nri_cont_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.1)[2]
    qrisk_metrics[[paste0("info_gain_", group_label)]] <- get_info_gain(nri_group_data, 'qrisk', 'ars')
    
    # ARS metrics
    ars_metrics[[paste0("c_", group_label)]] <- tryCatch(
      summary(coxph(Surv(group_data[, eval_time_col], group_data[, eval_lab]) ~ offset(preds[[2]][bs_df$risk_group == group])))$concordance[6],
      error = function(e) { cat("Error in c-index (ars):", conditionMessage(e), "\n"); return(NA) }
    )
    ars_metrics[[paste0("ici_", group_label)]] <- get_cal(group_data, rs[[2]][bs_df$risk_group == group], lab = eval_lab)
    ars_metrics[[paste0("nb_10_", group_label)]] <- calc_nb(group_data, rs[[2]][bs_df$risk_group == group], lab = eval_lab)
    ars_metrics[[paste0("nb_7.5_", group_label)]] <- calc_nb(group_data, rs[[2]][bs_df$risk_group == group], lab = eval_lab, th = 0.075)
    ars_metrics[[paste0("nb_5_", group_label)]] <- calc_nb(group_data, rs[[2]][bs_df$risk_group == group], lab = eval_lab, th = 0.05)
    ars_metrics[[paste0("r2_", group_label)]] <- get_r2(preds[[2]][bs_df$risk_group == group])
    ars_metrics[[paste0("nri_10_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.1)[1]
    ars_metrics[[paste0("nri_7.5_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.075)[1]
    ars_metrics[[paste0("nri_5_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col = 'macce_incident', th = 0.05)[1]
    ars_metrics[[paste0("nri_cont_", group_label)]] <- calculate_nri(nri_group_data, 'qrisk', 'ars', event_col ='macce_incident', th = 0.1)[2]
    ars_metrics[[paste0("info_gain_", group_label)]] <- get_info_gain(nri_group_data, 'qrisk', 'ars')
  }
  
  return(list(qrisk_metrics,  ars_metrics))
}



# n_bootstrap <- 100
# set.seed(0)  
# 
# idx <- list()
# for (n in 1:n_bootstrap) {
#   # Step 3: Create a bootstrap sample
#   print(paste0('bs iter = ',n))
#   bootstrap_indices <- sample(nrow(df), replace = TRUE)
#   idx <- list(idx,bootstrap_indices)
#   df_bs <- df[bootstrap_indices, ]
#   
#   # Step 3a: Impute missing data (assuming you use the 'mice' package)
#   imp_bs <- mice(df_bs[,imp_vars], method = "pmm", m = 5) 
#   bs_imp_data <- complete(imp_bs, n = 5)
#   
#   bs_datasets <- list()
#   
#   # Store each imputed dataset in the list
#   for (i in 1:5) {
#     bs_datasets[[i]] <- complete(imp_bs, i)
#     bs_datasets[[i]] <- get_q_feature(bs_datasets[[i]],data.frame(bs_datasets[[i]]))
#     bs_datasets[[i]]  <- cbind(bs_datasets[[i]] , model.matrix(~ Smoking - 1, data = bs_datasets[[i]]))
#     bs_datasets[[i]]  <- remove_duplicate_columns(bs_datasets[[i]] )
#     
#   }
#   df_bs <- bs_datasets[[1]]
#   
# sex_cond = df_bs$Sex == 1
#   m_mod <- fit_model(df_bs[df_bs$Sex == 1,])
# #print("fit female")
# 
#   f_mod <- fit_model(df_bs[df_bs$Sex == 0,])
#   
#   
#   q_mod_m <- m_mod[[1]]
#   ars_mod_m <- m_mod[[2]]
#   
#   q_mod_f <- f_mod[[1]]
#   ars_mod_f <- f_mod[[2]]
#   
#   print("get lp")
#   q_imp_lp_m <-lp_test(df_bs[sex_cond,],q_mod_m,ars = FALSE)
#   q_imp_lp_f <-lp_test(df_bs[!sex_cond,],q_mod_f,ars = FALSE)
#   ars_imp_lp_m <-lp_test(df_bs[sex_cond,],ars_mod_m,ars = TRUE)
#   ars_imp_lp_f <-lp_test(df_bs[!sex_cond,],ars_mod_f,ars = TRUE)
#   
#   
#   #male_preds <- make_pred(df[sex_cond,],q_mod_m,ars_mod_m)
#   #print("male preds")
#   #print("female preds")
#   #female_preds <- make_pred(df[!sex_cond,],q_mod_f,ars_mod_f)
#   
#   #calclate 9 yera risk
#   print("surv function")
#   lab = 'macce.incident'
#   q_base_m <- coxph(Surv(df_bs[sex_cond,]$macce_obs_time,df_bs[sex_cond,lab])~offset(q_imp_lp_m))
#   q_base_f <- coxph(Surv(df_bs[!sex_cond,]$macce_obs_time,df_bs[!sex_cond,lab])~offset(q_imp_lp_f))
#   
#   ars_base_m <- coxph(Surv(df_bs[sex_cond,]$macce_obs_time,df_bs[sex_cond,lab])~offset(ars_imp_lp_m))
#   ars_base_f <- coxph(Surv(df_bs[!sex_cond,]$macce_obs_time,df_bs[!sex_cond,lab])~offset(ars_imp_lp_f))
#   
#   q_base_m <- coxph(Surv(df[sex_cond,]$macce_obs_time,df[sex_cond,lab])~offset(q_imp_lp_m))
#   q_base_f <- coxph(Surv(df[!sex_cond,]$macce_obs_time,df[!sex_cond,lab])~offset(q_imp_lp_f))
#   
#   ars_base_m <- coxph(Surv(df[sex_cond,]$macce_obs_time,df[sex_cond,lab])~offset(ars_imp_lp_m))
#   ars_base_f <- coxph(Surv(df[!sex_cond,]$macce_obs_time,df[!sex_cond,lab])~offset(ars_imp_lp_f))
#   
# }

