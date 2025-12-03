####
#observed risk scores
#meaure peromance of adjusted risk scores
####
library(PredictABEL)

#C-index

gather_bs_res <- function(bs_res,metric,rows_with_na){
  stat <-pool_bs(bs_res,metric,rows_with_na)
  stat_low <- round(stat[1],3)
  stat_avg <- round(stat[2],3)#format(0.73, nsmall = 3)
  stat_high <-round(stat[3],3)
  stat_CI <- round(c(stat_low,stat_high), 2)
  print(paste0('metric: ',stat_avg ,'(',stat_low,' - ',stat_high,')'))
}

gather_c_idx <- function(df,sex_cond,lp,rows_with_na, bs_res = NULL,M = 5){
  
  male_base_cidx <- get_idx(df[sex_cond,], Reduce(`+`, lp[[1]]) / M)
  male_ars_cidx <- get_idx(df[sex_cond,], Reduce(`+`, lp[[2]]) / M)
  
  female_base_cidx <- get_idx(df[!sex_cond,], Reduce(`+`, lp[[3]]) / M)
  female_ars_cidx <- get_idx(df[!sex_cond,], Reduce(`+`, lp[[4]]) / M)
  
  delta_male <- male_ars_cidx - male_base_cidx
  delta_female <- female_ars_cidx - female_base_cidx
  
  print('Optimisim adjusted peromance')
  print(paste0('Delta male: ',delta_male))
  print(paste0('Delta female: ',delta_female))
  
  if(!(is.null(bs_res))){
    print('Bootstrap peromance')
    print('base male:')
    gather_bs_res(bs_res[[1]],'c_idx',rows_with_na)
    print('ars male:')
    gather_bs_res(bs_res[[2]],'c_idx',rows_with_na)
    print('delta male')
    pool_diff(bs_res[[1]],bs_res[[2]],'c_idx',rows_with_na)
    
    print('base female:')
    gather_bs_res(bs_res[[3]],'c_idx',rows_with_na)
    print('ars female:')
    gather_bs_res(bs_res[[4]],'c_idx',rows_with_na)
    print('female delta')
    pool_diff(bs_res[[3]],bs_res[[4]],'c_idx',rows_with_na)
    
  }
  
  
}



gather_nb <- function(df , sex_cond,th = 0.1,base = 'qrisk3',comp = 'ars',lab = 'macce.incident', bs_res = NULL){
  male_base_nb <- calc_nb(df[sex_cond,],df[sex_cond,base],lab = lab,th = th)
  male_ars_nb <-calc_nb(df[sex_cond,],df[sex_cond,comp],lab = lab,th = th)
  
  female_base_nb <-calc_nb(df[!sex_cond,],df[!sex_cond,base],lab = lab,th = th)
  female_ars_nb <-calc_nb(df[!sex_cond,],df[!sex_cond,comp],lab = lab,th = th)
  
  ovr_base_nb <- calc_nb(df,df[,base],lab = lab,th = th)
  ovr_ars_nb <- calc_nb(df,df[,comp],lab = lab,th = th)
  
  delta_male <- male_ars_nb - male_base_nb
  delta_female <- female_ars_nb - female_base_nb
  delta_ovr <- ovr_ars_nb - ovr_base_nb 
  print('')
  print(paste0('Net-benefit at ',th*100,'% threshold'))
  print(paste0('Delta male: ',delta_male))
  print(paste0('Delta female: ',delta_female))
  print(paste0('Delta overall: ',delta_ovr))
  
  if(!(is.null(bs_res))){
    print('Bootstrap peromance')
    print('base male:')
    nb_suffix = ceiling(th*100)
    nb_metric = paste0('nb_',nb_suffix)
    gather_bs_res(bs_res[[1]],nb_metric,rows_with_na)
    
    print('ars male:')
    gather_bs_res(bs_res[[2]],nb_metric,rows_with_na)
    
    print('delta male')
    pool_diff(bs_res[[1]],bs_res[[2]],nb_metric,rows_with_na)
    
    #print('')
    
    print('base female:')
    gather_bs_res(bs_res[[3]],nb_metric,rows_with_na)
    print('ars female:')
    gather_bs_res(bs_res[[4]],nb_metric,rows_with_na)
    print('female delta')
    pool_diff(bs_res[[3]],bs_res[[4]],nb_metric,rows_with_na)
    
  }
  
}

getaher_nri <- function(df , sex_cond,th = 0.1,base = 'qrisk3',comp = 'ars',lab = 'macce.incident', bs_res = NULL){
  
  
  print(paste0('Male at NRI',th*100,'% threshold'))
  get_nri(df[sex_cond,],base,comp,'macce.incident',cutoff = c(0,th,1))
  print('')
  print(paste0('Female at NRI',th*100,'% threshold'))
  
  get_nri(df[!sex_cond,],base,comp,'macce.incident',cutoff = c(0,th,1))
  print('')
  print(paste0('Overall at NRI',th*100,'% threshold'))
  print('')
  get_nri(df,base,comp,'macce.incident',cutoff = c(0,th,1))
  
  
}

gather_cal <- function(df , sex_cond,base = 'qrisk3',comp = 'ars',lab = 'macce.incident', bs_res = NULL){
  
  male_base_cal <- get_cal(df[sex_cond,],df[sex_cond,base],lab = 'macce.incident')
  male_ars_cal <- get_cal(df[sex_cond,],df[sex_cond,comp],lab = 'macce.incident')
  
  female_base_cal <- get_cal(df[!sex_cond,],df[!sex_cond,base],lab = 'macce.incident')
  female_ars_cal <- get_cal(df[!sex_cond,],df[!sex_cond,comp],lab = 'macce.incident')
  
  ovr_base_cal <-get_cal(df,df[,base],lab = 'macce.incident')
  ovr_ars_cal <-get_cal(df,df[,comp],lab = 'macce.incident')
  
  delta_male <- male_ars_cal - male_base_cal
  delta_female <- female_ars_cal - female_base_cal
  delta_ovr <- ovr_ars_cal - ovr_base_cal
  
  print('Calibration')
  print(paste0('Delta male: ',delta_male))
  print(paste0('Delta female: ',delta_female))
  print(paste0('Delta overall: ',delta_ovr))
  
  if(!(is.null(bs_res))){
    print('Bootstrap peromance')
    print('base male:')
    gather_bs_res(bs_res[[1]],'ici',rows_with_na)
    print('ars male:')
    gather_bs_res(bs_res[[2]],'ici',rows_with_na)
    print('delta male')
    pool_diff(bs_res[[1]],bs_res[[2]],'ici',rows_with_na)
    
    print('base female:')
    gather_bs_res(bs_res[[3]],'ici',rows_with_na)
    print('ars female:')
    gather_bs_res(bs_res[[4]],'ici',rows_with_na)
    print('female delta')
    pool_diff(bs_res[[3]],bs_res[[4]],'ici',rows_with_na)
    
  }
  
}


#Bootstarp results
male_q = full_res[[1]][[1]]
male_ars = full_res[[1]][[2]]
female_q = full_res[[1]][[3]]
female_ars = full_res[[1]][[4]]

male_q_con = cond_res[[1]][[1]]
male_ars_con = cond_res[[1]][[2]]
female_q_con = cond_res[[1]][[3]]
female_ars_con = cond_res[[1]][[4]]

gather_c_idx(df,sex_cond,lp_adj_full,rows_with_na,bs_res = full_res[[1]])
gather_c_idx(df,sex_cond,lp_adj_con,rows_with_na_con,bs_res = cond_res[[1]])

gather_cal(df,sex_cond,base = 'qrisk3',comp = 'ars')
gather_cal(df,sex_cond,base = 'qrisk3_con',comp = 'ars_con')
gather_nb(df,sex_cond,th = th,base = 'qrisk3',comp = 'ars_con')

th = 0.05
gather_nb(df,sex_cond,th = th,base = 'qrisk3',comp = 'ars',bs_res = full_res[[1]])
gather_nb(df,sex_cond,th = th,base = 'qrisk3_con',comp = 'ars_con',bs_res = full_res[[1]])
gather_nb(df,sex_cond,th = th,base = 'qrisk3',comp = 'ars_con',bs_res = full_res[[1]])

getaher_nri(df,sex_cond,th = th,base = 'qrisk3',comp = 'ars')
getaher_nri(df,sex_cond,th = th,base = 'qrisk3_con',comp = 'ars_con')
getaher_nri(df,sex_cond,th = th,base = 'qrisk3',comp = 'ars_con')


#Plot calibartion curves for shrunken predictors 

source(file.path("risk_scores",'cal_plot_imp.R'), echo = T)
#config
xmin = 0
xmax = 20
sex_cond <- df$Sex == 1
male_q = full_res[[1]][[1]]
male_ars = full_res[[1]][[2]]
female_q = full_res[[1]][[3]]
female_ars = full_res[[1]][[4]]

male_q_con = cond_res[[1]][[1]]
male_ars_con = cond_res[[1]][[2]]
female_q_con = cond_res[[1]][[3]]
female_ars_con = cond_res[[1]][[4]]
setwd(paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'))

cal_curve(df[sex_cond,],male_q,rows_with_na,lab = 'macce.incident', pred_event_prob = 'qrisk3',xmin =0,xmax = 20,hist_max = 5000,title = "Male QRISK3 calibration sigmoid",store_path =paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df[sex_cond,],male_ars,rows_with_na,lab = 'macce.incident', pred_event_prob = 'ars',xmin =0,xmax = 20,hist_max = 5000,title = "Male ARS calibration sigmoid",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),offset = 2.5)

cal_curve(df[!sex_cond,],female_q,rows_with_na,lab = 'macce.incident', pred_event_prob = 'qrisk3',xmin =0,xmax = 20,hist_max = 5000,title = "Female QRISK3 calibration sigmoid",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df[!sex_cond,],female_ars,rows_with_na,lab = 'macce.incident', pred_event_prob = 'ars',xmin =0,xmax = 20,hist_max = 5000,title = "Female ARS calibration sigmoid",store_path = paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)


cal_curve(df[sex_cond,],male_q_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'qrisk3_con',xmin =0,xmax = 20,hist_max = 5000,title = "Male QRISK3 Condensed calibration sigmoid",store_path  =paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df[sex_cond,],male_ars_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'ars_con',xmin =0,xmax = 20,hist_max = 5000,title = "Male ARS Condensed calibration",store_path  =paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)

cal_curve(df[!sex_cond,],female_q_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'qrisk3_con',xmin =0,xmax = 20,hist_max = 5000,title = "Female QRISK3 Condensed calibration sigmoid",store_path  =paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)
cal_curve(df[!sex_cond,],female_ars_con,rows_with_na_con,lab = 'macce.incident', pred_event_prob = 'ars_con',xmin =0,xmax = 20,hist_max = 5000,title = "Female ARS Condensed calibrationsigmoid ",store_path  =paste0('risk_scores/ARS/bs_imp/con/6y/stacked/full/',store,'/'),met_path = paste0('/gpfs3ecg_scores/macce_res/minb_set/comb/q_ecg/lin_res/mets/'),offset = 2.5)




