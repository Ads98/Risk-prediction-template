library(tidyr)

stdca <- function(data, outcome, ttoutcome, timepoint, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                  ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                  interventionper=100, smooth=FALSE,loess.span=0.10,cmprsk=FALSE,colour_map = NULL) {
  
  # LOADING REQUIRED LIBRARIES
  require(survival)
  require(stats)
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[c(outcome,ttoutcome,predictors)]),c(outcome,ttoutcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if ((length(data[!(data[outcome]==0 | data[outcome]==1),outcome])>0) & cmprsk==FALSE) {
    stop("outcome must be coded as 0 and 1")
  }
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }  
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A T OR F
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO T OR F, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A COX REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=coxph(Surv(data.matrix(data[ttoutcome]),data.matrix(data[outcome])) ~ data.matrix(data[predictors[m]]))
      surv.data=data.frame(0)
      pred=data.frame(1-c(summary(survfit(model, newdata=surv.data), time=timepoint)$surv))
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with Cox regression. Due to linearity and proportional hazards assumption, miscalibration may occur.",sep=" "))
    }
  }  
  
  #########  CALCULATING NET BENEFIT   #########
  N=dim(data)[1]
  
  # getting the probability of the event for all subjects
  # this is used for the net benefit associated with treating all patients
  if(cmprsk==FALSE) {
    km.cuminc=survfit(Surv(data.matrix(data[ttoutcome]),data.matrix(data[outcome]))~1)
    pd=1 - summary(km.cuminc, times=timepoint)$surv
  } else {
    require(cmprsk)
    cr.cuminc=cuminc(data[[ttoutcome]],data[[outcome]])
    pd=timepoints(cr.cuminc, times=timepoint)$est[1]
  }
  
  #creating dataset that is one line per threshold for the treat all and treat none strategies;
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  error=NULL
  
  # Calculate event prevalence (p)
  N <- nrow(data)
  p <- sum(data[[outcome]]) / N  # prevalence of the event in the dataset
  
  nb["all"]=pd - (1-pd)*nb$threshold/(1-nb$threshold) / p
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    nb[predictors[m]]=NA
    
    for(t in 1:length(nb$threshold)){
      #calculating number of true and false postives;
      px=sum(data[predictors[m]]>nb$threshold[t])/N
      
      if (px==0){
        error=rbind(error,paste(predictors[m],": No observations with risk greater than ",nb$threshold[t]*100,"%",sep=""))
        break
      } else {
        #calculate risk using Kaplan Meier
        if(cmprsk==FALSE) {
          km.cuminc=survfit(Surv(data.matrix(data[data[predictors[m]]>nb$threshold[t],ttoutcome]),data.matrix(data[data[predictors[m]]>nb$threshold[t],outcome]))~1)
          pdgivenx=(1 - summary(km.cuminc, times=timepoint)$surv)
          if(length(pdgivenx)==0){
            error=rbind(error,paste(predictors[m],": No observations with risk greater than ",nb$threshold[t]*100,"% that have followup through the timepoint selected",sep=""))
            break
          }
          #calculate risk using competing risk
        }  else {
          cr.cuminc=cuminc(data[[ttoutcome]][data[[predictors[m]]]>nb$threshold[t]],data[[outcome]][data[[predictors[m]]]>nb$threshold[t]])
          pdgivenx=timepoints(cr.cuminc, times=timepoint)$est[1]
          if(is.na(pdgivenx)){
            error=rbind(error,paste(predictors[m],": No observations with risk greater than ",nb$threshold[t]*100,"% that have followup through the timepoint selected",sep=""))
            break
          }
        }
        #calculating NB based on calculated risk
        nb[t,predictors[m]]=pdgivenx*px - (1-pdgivenx)*px*nb$threshold[t]/(1-nb$threshold[t]) - harm[m]
        nb[t,predictors[m]]=  nb[t,predictors[m]] / p
      }
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  if(length(error)>0){
    print(paste(error,", and therefore net benefit not calculable in this range.",sep="")) 
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)     
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 1)
      }
      
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("Treat None", "Treat All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        
        if (!is_null(colour_map)){
          #current_predictor <- predictors[m]
          current_color <- colour_map[[m]]  # Get the assigned color
        }
        else{
          current_color <- m
        }
        
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=current_color,lty=1,lwd=3) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=current_color,lty=1,lwd=3)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, current_color)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 1)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  return(results)
  
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
    print("Cleaning data frame")
    return(sapply(mods, function(df) df[-unique(rows_with_na),met]))
  }
  print("fulldf")
  return(sapply(mods, function(df) df[,met]))
}

optim_dist <-function(mods,rows_with_na,met,mode = 'mean'){
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
  print(mode)
  
  male_q_us =  (male_q_bs_cbl - male_q_cbl)
  male_ars_us =  (male_ars_bs_cbl - male_ars_cbl)
  female_q_us =  (female_q_bs_cbl - female_q_cbl)
  female_ars_us =  (female_ars_bs_cbl - female_ars_cbl)
  
  
  
  return(list(male_q_us,male_ars_us,female_q_us,female_ars_us))
}

performance_shrinkage <-function(mods,rows_with_na,met,mode = 'mean'){
 
  optims <- optim_dist(mods,rows_with_na,met,mode = 'mean')
  male_q_us <- optims[[1]]
  male_ars_us <- optims[[2]]
  female_q_us <- optims[[3]]
  female_ars_us <- optims[[4]]
  
  print("shrink lp")
  print(mode)
  if(mode == 'mean'){
    male_q_us =  mean(male_q_us)
    male_ars_us =  mean(male_ars_us)
    female_q_us =  mean(female_q_us )
    female_ars_us =  mean(female_ars_us)
  }
  else{
    male_q_us =  sd(male_q_us )
    male_ars_us =  sd(male_ars_us)
    female_q_us =  sd(female_q_us  )
    female_ars_us =  sd(female_ars_us)
  }
  
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
load_modles <-function(path = 'risk_scores/ARS/org_imp/',suffix = c('_m','_f')){
  #load orgianl models 
  q_mod_m <-readRDS(paste0(path,'q_mod',suffix[1] , '.rds'))
  ars_mod_m <-readRDS(paste0(path,'ars_mod' ,suffix[1],'.rds'))
  
  q_mod_f <-readRDS(paste0(path,'q_mod',suffix[2],'.rds'))
  ars_mod_f <-readRDS(paste0(path,'ars_mod',suffix[2], '.rds'))
  
  return(list(q_mod_m,ars_mod_m,q_mod_f,ars_mod_f))
}


adjust_lp <- function(imputed_datasets,mods,shrinkage,model = 'qrisk',fp_terms = NULL){
  #get pooled model lp and ajust for optimism
  q_imp_lp_m <- list()
  q_imp_lp_f <- list()
  
  ars_imp_lp_m <- list()
  ars_imp_lp_f <- list()
  # Pool the means from the deravtion data for centering
  sex_cond <- imputed_datasets[[1]]$Sex ==1
  pooled_means_m <- pool_avg(imputed_datasets,mods[[2]],model = model,fp_terms = fp_terms,sex_cond = sex_cond)
  pooled_means_f <- pool_avg(imputed_datasets,mods[[4]],model = model,fp_terms = fp_terms,sex_cond = !sex_cond)
  
  
  
  
  for(i in 1:5){
    print("q m")
    q_imp_lp_m[[i]] <-shrinkage[[1]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==1,],mods[[1]],ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pooled_means_m)
    print("ars m")
    ars_imp_lp_m[[i]] <- shrinkage[[2]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==1,],mods[[2]],ars = TRUE,model = model,fp_terms = fp_terms,pooled_means = pooled_means_m)
    print("q f")
    q_imp_lp_f[[i]] <- shrinkage[[3]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==0,],mods[[3]],ars = FALSE,model = model,fp_terms = fp_terms,pooled_means = pooled_means_f)
    #print(paste0("female q cbl =",imputed_datasets[[i]],q_imp_lp_f[[i]] /shrinkage[[3]] ,lab = 'macce.incident'))
    print("ars")
    ars_imp_lp_f[[i]] <- shrinkage[[4]] * get_lp(imputed_datasets[[i]][imputed_datasets[[i]]$Sex ==0,],mods[[4]],ars = TRUE,model = model,fp_terms = fp_terms,pooled_means = pooled_means_f)
  }
  
  return(list(q_imp_lp_m,ars_imp_lp_m,q_imp_lp_f,ars_imp_lp_f))
}


adjust_rs <- function(imputed_datasets,mods,lp,base_surv = NULL,cent = FALSE){
  #get risk scores
  q_score_m <- list()
  q_score_f <- list()
  
  ars_score_m <- list()
  ars_score_f <- list()
  
  
  for (i in 1:5) {
    sex_cond = imputed_datasets[[i]]$Sex ==1
    q_score_m[[i]] <- get_scores(
      imputed_datasets[[i]][sex_cond, ],
      if (cent) lp[[1]][[i]] - mean(lp[[1]][[i]]) else lp[[1]][[i]],
      lab = 'macce.incident',
      time = 6,
      th = 0.5,
      baseline_surv = base_surv[[1]]
    )
    
    ars_score_m[[i]] <- get_scores(
      imputed_datasets[[i]][sex_cond, ],
      if (cent) lp[[2]][[i]] - mean(lp[[2]][[i]]) else lp[[2]][[i]],
      lab = 'macce.incident',
      time = 6,
      th = 0.5,
      baseline_surv = base_surv[[3]]
    )
    
    q_score_f[[i]] <- get_scores(
      imputed_datasets[[i]][!sex_cond, ],
      if (cent) lp[[3]][[i]] - mean(lp[[3]][[i]]) else lp[[3]][[i]],
      lab = 'macce.incident',
      time = 6,
      th = 0.5,
      baseline_surv = base_surv[[2]]
    )
    
    ars_score_f[[i]] <- get_scores(
      imputed_datasets[[i]][!sex_cond, ],
      if (cent) lp[[4]][[i]] - mean(lp[[4]][[i]]) else lp[[4]][[i]],
      lab = 'macce.incident',
      time = 6,
      th = 0.5,
      baseline_surv = base_surv[[4]]
    )
    
  }
  #pool riks cores together using rubisn rule:https://pubmed.ncbi.nlm.nih.gov/25630926/
  male_q_pool <- Reduce(`+`, q_score_m) / 5
  male_ars_pool <- Reduce(`+`, ars_score_m) / 5
  
  female_q_pool <- Reduce(`+`, q_score_f) / 5
  female_ars_pool <- Reduce(`+`, ars_score_f) / 5
  return(list(male_q_pool,male_ars_pool,female_q_pool,female_ars_pool))
  
}
pool_ci <-function(res,alpha= 0.05){
  # Calculate the (1 - 2*alpha)% percentile confidence interval for res
  lower_percentile <- alpha * 100 / 2
  upper_percentile <- 100 - lower_percentile
  
  # Calculate the lower and upper percentiles of res_b_m values
  lower_percentile_value <- quantile(res, lower_percentile / 100)
  upper_percentile_value <- quantile(res, upper_percentile / 100)
  # Form the confidence interval
  confidence_interval <- c(lower_percentile_value, upper_percentile_value)
  return(confidence_interval)
}
pool_bs <- function(res,met,rows_with_na,M= 5, B = 1000,shrink = 0){
  # Initialize an empty matrix to store res_b_m estimates
  
  #loop through uimputed data sets and get mean point estimate for each boostrap
  
  #Theta_b_m point estimates for each boot strap
  #male_q_us
  res_m <- remove_rows(res,rows_with_na,met = met) #sapply(res, function(df) df[-unique(rows_with_na),met])
  res_m <-rowMeans(res_m) #collapse to single estaimte
  res_m <- res_m - shrink #adjust for optimism if shirnk >0
  
  #Now average camples within the bootstrap
  res_b_m <- mean(res_m)
  
  ci <- pool_ci(res_m)
  return(c(ci[1],res_b_m,ci[2]))
}

c_idx <- function(res1,res2,met,rows_with_na,M= 5, B = 1000,idx = 1){
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
  ci <- pool_ci(res2_m - res1_m)
  mean_dif <- mean(res2_m - res1_m)
  
  boot.high <- ifelse(res2_m >res1_m, 1, 0)
  
  print(mean(boot.high))
  
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

