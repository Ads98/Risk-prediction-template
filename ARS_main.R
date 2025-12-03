source(file.path("/well/doherty/users/rii148/risk_scores/array_job/9_year_censoring/centered/",'0_conf.R'), echo = T)
source(file.path("/well/doherty/users/rii148/risk_scores/array_job/9_year_censoring/centered/",'ARS_deriv.R'), echo = T)


#load('/well/doherty/users/rii148/risk_scores/ARS/tmp_env.RData')
# print('My args')
# print(paste0('modle:',model))
# print(paste0('store path:',store))
# print(paste0('Fit polynomial: ',fit_poly))
# print(paste0('model type: ',model_type))
# print(paste0('data_source: ',data_source))
# print(paste0('FP terms: ',fp_terms))



# print(dim(df))
# print(mean(df$macce_obs_time))
# print(median(df$macce_obs_time))
# print(max(df$macce_obs_time))
# print(100*(sum(df$macc.incident)/nrow(df)))




args <- commandArgs(TRUE)
model <- as.character(args[1])
store <- as.character(args[2])
split <- as.character(args[3])
fit_poly <- as.character(args[4])
model_type <- as.character(args[5])
data_source <- as.character(args[6])
fp_terms <- strsplit(args[7], ",")[[1]]
stack_splits <- as.character(args[8])

# Print the arguments
print('List args')
print(paste("model:", model))
print(paste("store:", store))
print(paste("split:", split))
print(paste("fit_poly:", fit_poly))
print(paste("model_type:", model_type))
print(paste("data_source:", data_source))
print(paste("fp_terms:", fp_terms))
print(paste("stack_splits:", stack_splits))

model <- tolower(model)  # Convert model name to lowercase
model_type <- tolower(model_type)  # Convert model name to lowercase

if(split == 'null'){
  split <- ''
}



fit_poly <- tolower(fit_poly)  # Convert model name to lowercase
if(fit_poly != 't' &fit_poly != 'f'){
  print(fit_poly)
  stop('Error invalid input fit polynomail must be treue or false')
}
if(fit_poly == 't'){
  fit_poly <- TRUE
  store_suffix <- paste0(model_type,'_fp')
}else{
  fit_poly <- FALSE
  store_suffix <- paste0(model_type,'_lin')
}

if(stack_splits == 't'){
  stack_splits <- TRUE
}else{
  stack_splits <- FALSE
}




data_source <- tolower(data_source) 
if(data_source != 'ukb' &data_source != 'gp'){
  stop('Error invalid input data source must be either gp or ukb')
}


store <- paste0(store,split,'/',model,'_',store_suffix)


data_path <- '/well/doherty/users/rii148/risk_scores/stacked/array_job'
if (!file.exists(paste0(data_path,store,'/deriv_set.csv'))){
  print('creating dervation dataset')
  if (!file.exists(paste0(data_path,store))){
    dir.create(paste0(data_path,store), recursive = TRUE)  # 'recursive = TRUE' creates parent directories if needed
  }
  df <-load_data(split,folds,data_source,stack_splits = stack_splits)
  write.csv(df,paste0(data_path,store,'/deriv_set.csv'))
}else{
  print('loading pre-created deriv set')
  df <- read.csv(paste0(data_path,store,'/deriv_set.csv')) 
  df$Smoking <- factor(df$Smoking) #refactoirse smoking
}

#save.image(file='/well/doherty/users/rii148/risk_scores/ARS/tmp_env.RData')

source(file.path("/well/doherty/users/rii148/risk_scores/array_job/9_year_censoring/centered/",'array_setup.R'), echo = T)
df$Smoking
print('study pop size: ')
print(dim(df))
main_model(df,model,store,fit_poly,fp_terms,data_source,model_type,new_bs = FALSE)
df$Smoking